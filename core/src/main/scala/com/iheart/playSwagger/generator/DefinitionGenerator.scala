package com.iheart.playSwagger.generator

import scala.collection.JavaConverters
import scala.meta.internal.parsers.ScaladocParser
import scala.meta.internal.{Scaladoc => iScaladoc}
import scala.reflect.runtime.universe.{Try => _, _}
import scala.util.Try

import com.fasterxml.jackson.databind.{BeanDescription, ObjectMapper}
import com.github.takezoe.scaladoc.Scaladoc
import com.iheart.playSwagger.ParametricType
import com.iheart.playSwagger.domain.Definition
import com.iheart.playSwagger.domain.parameter.{GenSwaggerParameter, SwaggerParameter}
import net.steppschuh.markdowngenerator.MarkdownElement
import net.steppschuh.markdowngenerator.link.Link
import net.steppschuh.markdowngenerator.table.Table
import net.steppschuh.markdowngenerator.text.Text
import net.steppschuh.markdowngenerator.text.code.{Code, CodeBlock}
import net.steppschuh.markdowngenerator.text.heading.Heading
import play.routes.compiler.Parameter

final case class DefinitionGenerator(
    mapper: SwaggerParameterMapper,
    swaggerPlayJava: Boolean = false,
    _mapper: ObjectMapper = new ObjectMapper(),
    namingConvention: NamingConvention = NamingConvention.None,
    embedScaladoc: Boolean = false
)(implicit cl: ClassLoader) {

  private val refinedTypePattern = raw"(eu\.timepit\.refined\.api\.Refined(?:\[.+])?)".r

  private def dealiasParams(t: Type): Type = {
    t.toString match {
      case refinedTypePattern(_) => t.typeArgs.headOption.getOrElse(t)
      case _ =>
        appliedType(
          t.dealias.typeConstructor,
          t.typeArgs.map { arg =>
            dealiasParams(arg.dealias)
          }
        )
    }
  }

  private def scalaDocToMarkdown: PartialFunction[iScaladoc.Term, MarkdownElement] = {
    case value: iScaladoc.Text =>
      new Text(value.parts.map {
        case word: iScaladoc.Word => new Text(word.value)
        case link: iScaladoc.Link => new Link(link.anchor.mkString(" "), link.ref)
        case code: iScaladoc.CodeExpr => new Code(code.code)
      }.mkString(" "))
    case code: iScaladoc.CodeBlock => new CodeBlock(code, "scala")
    case code: iScaladoc.MdCodeBlock =>
      new CodeBlock(code.code.mkString("\n"), code.info.mkString(":"))
    case head: iScaladoc.Heading => new Heading(head, 1)
    case table: iScaladoc.Table =>
      val builder = new Table.Builder().withAlignments(Table.ALIGN_RIGHT, Table.ALIGN_LEFT).addRow(
        table.header.cols: _*
      )
      table.rows.foreach(row => builder.addRow(row.cols: _*))
      builder.build()
    // TODO: Support List
    // https://github.com/Steppschuh/Java-Markdown-Generator/pull/13
    case _ => new Text("")
  }

  private def definition(pt: ParametricType) = Definition(
    name = pt.reifiedTypeName,
    properties = definitionProperties(pt),
  )

  private def definitionProperties(parametricType: ParametricType): Seq[SwaggerParameter] = {
    val tpe = parametricType.tpe
    if (swaggerPlayJava) return definitionForPOJO(tpe)

    lazy val paramToDescription = paramDescriptions(tpe).toMap

    tpe
      .decls
      .collectFirst { case m: MethodSymbol if m.isPrimaryConstructor => m }
      .flatMap(_.paramLists.headOption)
      .getOrElse(Nil)
      .map(swaggerParam(parametricType, paramToDescription))
  }

  private def swaggerParam(
      parametricType: ParametricType,
      paramDescriptions: Map[String, String],
  )(
      field: Symbol,
  ) = {
    // TODO: find a better way to get the string representation of typeSignature
    val dealiased = dealiasParams(field.typeSignature).toString
    val rawTypeName = dealiased match {
      case refinedTypePattern(_) =>
        field.info.dealias.typeArgs.headOption.map(_.toString).getOrElse(dealiased)
      case _ => dealiased
    }
    val decodedName = field.name.decodedName.toString
    // passing None for 'fixed' and 'default' here, since we're not dealing with route parameters
    val param = Parameter(
      name = namingConvention(decodedName),
      typeName = parametricType.resolve(rawTypeName),
      fixed = None,
      default = None,
    )
    mapper.mapParam(param, paramDescriptions.get(decodedName))
  }

  private def paramDescriptions(tpe: Type): List[(String, String)] = {
    if (!embedScaladoc) return List.empty[(String, String)]
    for {
      annotation <- tpe.typeSymbol.annotations
      if typeOf[Scaladoc] == annotation.tree.tpe
      value <- Try(annotation.tree.children.tail).getOrElse(List.empty).headOption.toList
      docTree <- Try(value.children.tail).getOrElse(List.empty).headOption.toList
      docString = Try(docTree.toString().tail.init).getOrElse("").replace("\\n", "\n")
      doc <- ScaladocParser.parse(docString).toList
      paragraph <- doc.para
      term <- paragraph.terms
      (key, scalaDoc) <- term match {
        case iScaladoc.Tag(iScaladoc.TagType.Param, Some(iScaladoc.Word(key)), Seq(text)) => Some(key -> text)
        case _ => None
      }
    } yield key -> scalaDocToMarkdown(scalaDoc).toString
  }

  private def definitionForPOJO(tpe: Type): Seq[SwaggerParameter] = {
    val m = runtimeMirror(cl)
    val clazz = m.runtimeClass(tpe.typeSymbol.asClass)
    val `type` = _mapper.constructType(clazz)
    val beanDesc: BeanDescription = _mapper.getSerializationConfig.introspect(`type`)
    val beanProperties = beanDesc.findProperties
    val ignoreProperties = beanDesc.getIgnoredPropertyNames
    val propertySet = JavaConverters.asScalaIteratorConverter(beanProperties.iterator()).asScala.toSeq
    propertySet.filter(bd => !ignoreProperties.contains(bd.getName)).map { entry =>
      val name = entry.getName
      val className = entry.getPrimaryMember.getType.getRawClass.getName
      val generalTypeName = if (entry.getField != null && entry.getField.getType.hasGenericTypes) {
        val generalType = entry.getField.getType.getContentType.getRawClass.getName
        s"$className[$generalType]"
      } else {
        className
      }
      val typeName = if (!entry.isRequired) {
        s"Option[$generalTypeName]"
      } else {
        generalTypeName
      }
      val param = Parameter(name, typeName, None, None)
      mapper.mapParam(param, None)
    }
  }

  def definition[T: TypeTag]: Definition = definition(ParametricType[T])

  def definition(className: String): Definition = definition(ParametricType(className))

  def allDefinitions(typeNames: Seq[String]): List[Definition] = {
    def genSwaggerParameter: PartialFunction[SwaggerParameter, GenSwaggerParameter] = {
      case p: GenSwaggerParameter => p
    }

    def allReferredDefs(defName: String, memo: List[Definition]): List[Definition] = {
      def findRefTypes(p: GenSwaggerParameter): Seq[String] =
        p.referenceType.toSeq ++ {
          p.items.toSeq.collect(genSwaggerParameter).flatMap(findRefTypes)
        }

      memo.find(_.name == defName) match {
        case Some(_) => memo
        case None =>
          val thisDef = definition(defName)
          val refNames: Seq[String] = for {
            p <- thisDef.properties.collect(genSwaggerParameter)
            className <- findRefTypes(p)
            if mapper.isReference(className)
          } yield className

          refNames.foldLeft(thisDef :: memo) { (foundDefs, refName) =>
            allReferredDefs(refName, foundDefs)
          }
      }
    }

    typeNames.foldLeft(List.empty[Definition]) { (memo, typeName) =>
      allReferredDefs(typeName, memo)
    }
  }
}

object DefinitionGenerator {
  def apply(
      mapper: SwaggerParameterMapper,
      swaggerPlayJava: Boolean,
      namingConvention: NamingConvention
  )(implicit cl: ClassLoader): DefinitionGenerator =
    new DefinitionGenerator(
      mapper,
      swaggerPlayJava,
      namingConvention = namingConvention
    )

  def apply(
      mapper: SwaggerParameterMapper,
      namingConvention: NamingConvention,
      embedScaladoc: Boolean
  )(implicit cl: ClassLoader): DefinitionGenerator =
    new DefinitionGenerator(
      mapper = mapper,
      namingConvention = namingConvention,
      embedScaladoc = embedScaladoc
    )
}
