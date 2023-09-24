import sbt._
import scala.sys.process._

import indigoplugin.DataType

object KeyMappingsGen {

  val fileName: String = "key-mappings.md"

  def toKeyFormat(name: String): String = {
    val k = name.split(' ').map(_.toUpperCase()).mkString("_")
    if (k.length == 1) "KEY_" + k else k
  }

  def genLookUpValues(
      moduleName: String
  ): List[String] => String = {
    case command :: mapping1 :: "<unmapped>" :: Nil =>
      val n  = sanitizeName(command)
      val m1 = toKeyFormat(mapping1)
      s"  val ${n}: Key = Key.$m1\n"

    case command :: mapping1 :: mapping2 :: Nil =>
      val n  = sanitizeName(command)
      val m1 = toKeyFormat(mapping1)
      val m2 = toKeyFormat(mapping2)
      s"  val ${n}1: Key = Key.$m1\n" +
        s"  val ${n}2: Key = Key.$m2"

    case command :: mapping1 :: Nil =>
      val n = sanitizeName(command)
      val m = toKeyFormat(mapping1)
      s"""  val $n: Key = Key.$m"""

    case _ =>
      ""
  }

  def toHelpChar: String => String = {
    case "forward slash"              => "/"
    case "period"                     => "."
    case "add"                        => "+"
    case "equal sign"                 => "="
    case "subtract"                   => "-"
    case "dash"                       => "_"
    case "escape"                     => "esc"
    case k if k.split(" ").length > 1 => k.split(" ").head
    case k                            => k
  }

  def genHelpText(moduleName: String): List[String] => String = {
    case command :: mapping1 :: "<unmapped>" :: Nil =>
      val n  = sanitizeName(command)
      val m1 = toHelpChar(mapping1)
      s"""|    ("$command" -> List("$m1")),"""

    case command :: mapping1 :: mapping2 :: Nil =>
      val n  = sanitizeName(command)
      val m1 = toHelpChar(mapping1)
      val m2 = toHelpChar(mapping2)
      s"""|    ("$command" -> List("$m1", "$m2")),"""

    case command :: mapping1 :: Nil =>
      val n  = sanitizeName(command)
      val m1 = toHelpChar(mapping1)
      s"""|    ("$command" -> List("$m1")),"""

    case _ =>
      ""
  }

  def present(moduleName: String): List[List[DataType]] => String = {
    case headers :: rows =>
      val vals: List[String] =
        rows.map(r => genLookUpValues(moduleName)(r.map(_.toStringData.value)))

      val help: List[String] =
        rows.map(r => genHelpText(moduleName)(r.map(_.toStringData.value)))

      template(moduleName, vals, help)

    case _ =>
      throw new Exception("Not enough data, expected headers and rows")
  }

  def template(
      moduleName: String,
      vals: List[String],
      help: List[String]
  ): String =
    s"""import indigo.Key
    |
    |object $moduleName:
    |${vals.mkString("\n")}
    |
    |  val helpText: List[(String, List[String])] = List(
    ${help.mkString("\n")}
    |  )
    |
    |  val longestCommandName: Int = helpText.map(_._1.length).sorted.reverse.headOption.getOrElse(-1)
    |  val longestMappings: Int = helpText.map(_._2.mkString.length).sorted.reverse.headOption.getOrElse(-1)
    |""".stripMargin

  def sanitizeName(name: String): String =
    name.split(' ').map(_.capitalize).mkString
}
