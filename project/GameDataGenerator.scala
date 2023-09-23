import sbt._
import scala.sys.process._

object KeyMappingsGen extends GameDataGenerator {

  val fileName: String = "key-mappings.md"

  def toKeyFormat(name: String): String = {
    val k = name.split(' ').map(_.toUpperCase()).mkString("_")
    if (k.length == 1) "KEY_" + k else k
  }

  def genLookUpValues(
      moduleName: String
  ): PartialFunction[List[String], String] = {
    case command :: mapping1 :: mapping2 :: Nil =>
      val n  = GameDataGenerator.sanitizeName(command)
      val m1 = toKeyFormat(mapping1)
      val m2 = toKeyFormat(mapping2)
      s"  val ${n}1: Key = Key.$m1\n" +
        s"  val ${n}2: Key = Key.$m2"

    case command :: mapping1 :: Nil =>
      val n = GameDataGenerator.sanitizeName(command)
      val m = toKeyFormat(mapping1)
      s"""  val $n: Key = Key.$m"""
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

  def genHelpText(moduleName: String): PartialFunction[List[String], String] = {
    case command :: mapping1 :: mapping2 :: Nil =>
      val n  = GameDataGenerator.sanitizeName(command)
      val m1 = toHelpChar(mapping1)
      val m2 = toHelpChar(mapping2)
      s"""|    ("$command" -> List("$m1", "$m2")),"""

    case command :: mapping1 :: Nil =>
      val n  = GameDataGenerator.sanitizeName(command)
      val m1 = toHelpChar(mapping1)
      s"""|    ("$command" -> List("$m1")),"""
  }

  def mappers(moduleName: String): List[PartialFunction[List[String], String]] =
    List(
      genLookUpValues(moduleName),
      genHelpText(moduleName)
    )

  def template(
      moduleName: String,
      fullyQualifiedPath: String,
      contents: List[String]
  ): String =
    s"""package $fullyQualifiedPath
    |
    |import indigo.Key
    |
    |object $moduleName:
    |${contents(0)}
    |
    |  val helpText: List[(String, List[String])] = List(
    ${contents(1)}
    |  )
    |
    |  val longestCommandName: Int = helpText.map(_._1.length).sorted.reverse.headOption.getOrElse(-1)
    |  val longestMappings: Int = helpText.map(_._2.mkString.length).sorted.reverse.headOption.getOrElse(-1)
    |""".stripMargin

}

trait GameDataGenerator {

  val tripleQuotes: String = "\"\"\""

  val fileName: String

  def mappers(moduleName: String): List[PartialFunction[List[String], String]]

  def template(
      moduleName: String,
      fullyQualifiedPath: String,
      contents: List[String]
  ): String

  def gen(
      moduleName: String,
      fullyQualifiedPath: String,
      files: Set[File],
      sourceManagedDir: File
  ): Set[File] =
    GameDataGenerator.generate(
      moduleName,
      fullyQualifiedPath,
      files,
      sourceManagedDir
    )(
      fileName,
      mappers,
      template
    )
}

object GameDataGenerator {

  val fallback: Int => String => PartialFunction[List[String], String] =
    mappingNum =>
      moduleName => { case fields =>
        val msg1 = s"Non-exhaustive match on mapper in position $mappingNum"
        val msg2 =
          moduleName + s" gen: Unexpected number of data fields, got: " + fields
            .mkString("[", ", ", "]")
        println(msg1 + "\n" + msg2)
        throw new Exception(msg1 + " - " + msg2)
      }

  def generate(
      moduleName: String,
      fullyQualifiedPath: String,
      files: Set[File],
      sourceManagedDir: File
  )(
      fileName: String,
      mappers: String => List[PartialFunction[List[String], String]],
      template: (String, String, List[String]) => String
  ): Set[File] = {
    println(s"Generating $moduleName...")

    val file: File =
      files.filter(f => f.name == fileName).headOption match {
        case Some(s) => s
        case None =>
          val msg = "Missing file: " + fileName
          println(msg)
          throw new Exception(msg)
      }

    val contents: List[String] =
      IO.read(file)
        .split('\n')
        .toList
        .dropWhile(s => !s.contains("---|"))
        .drop(1)

    val out: File =
      sourceManagedDir / (moduleName + ".scala")

    val rows: List[List[String]] = contents
      .map(_.split('|').map(_.trim).toList)

    val newContents = mappers(moduleName).zipWithIndex
      .map { case (mpr, i) =>
        rows
          .map { fields =>
            mpr.orElse(fallback(i)(moduleName))(fields)
          }
          .filterNot(_.trim.isEmpty)
      }
      .map(_.mkString("\n"))

    IO.write(out, template(moduleName, fullyQualifiedPath, newContents))

    println("Written: " + out.getCanonicalPath)

    Set(out)
  }

  def sanitizeName(name: String): String =
    name.split(' ').map(_.capitalize).mkString

}
