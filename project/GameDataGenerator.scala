import sbt._
import scala.sys.process._

object MeleeGen extends GameDataGenerator {

  val fileName: String = "melee.md"

  def mapFields(moduleName: String): PartialFunction[List[String], String] = {
    case name :: power :: Nil =>
      val n = GameDataGenerator.sanitizeName(name)
      s"""  case $n extends $moduleName("$name", $power)"""
  }

  def mappers(moduleName: String): List[PartialFunction[List[String], String]] =
    List(mapFields(moduleName))

  def template(
      moduleName: String,
      fullyQualifiedPath: String,
      contents: List[String]
  ): String =
    s"""package $fullyQualifiedPath
    |
    |import roguelike.model.items.Item
    |
    |enum $moduleName(val name: String, val power: Int) extends Item:
    |${contents.head}
    |""".stripMargin

}

object ArmourGen extends GameDataGenerator {

  val fileName: String = "armour.md"

  def mapFields(moduleName: String): PartialFunction[List[String], String] = {
    case name :: defense :: Nil =>
      val n = GameDataGenerator.sanitizeName(name)
      s"""  case $n extends $moduleName("$name", $defense)"""
  }

  def mappers(moduleName: String): List[PartialFunction[List[String], String]] =
    List(mapFields(moduleName))

  def template(
      moduleName: String,
      fullyQualifiedPath: String,
      contents: List[String]
  ): String =
    s"""package $fullyQualifiedPath
    |
    |import roguelike.model.items.Item
    |
    |enum $moduleName(val name: String, val defenseBonus: Int) extends Item:
    |${contents.head}
    |""".stripMargin

}

object ConsumablesGen extends GameDataGenerator {

  val fileName: String = "consumables.md"

  def mapFields(moduleName: String): PartialFunction[List[String], String] = {
    case name :: amount :: Nil =>
      val n = GameDataGenerator.sanitizeName(name)
      s"""  case $n extends $moduleName("$name", $amount)"""
  }

  def mappers(moduleName: String): List[PartialFunction[List[String], String]] =
    List(mapFields(moduleName))

  def template(
      moduleName: String,
      fullyQualifiedPath: String,
      contents: List[String]
  ): String =
    s"""package $fullyQualifiedPath
    |
    |import roguelike.model.items.Item
    |
    |enum $moduleName(val name: String, val amount: Int) extends Item:
    |${contents.head}
    |""".stripMargin

}

object RangedGen extends GameDataGenerator {

  val fileName: String = "ranged.md"

  def mapFields(moduleName: String): PartialFunction[List[String], String] = {
    case name :: damage :: radius :: turnCount :: maxRange :: Nil =>
      val n = GameDataGenerator.sanitizeName(name)
      s"""  case $n extends $moduleName("$name", $damage, $radius, $turnCount, $maxRange)"""
  }

  def mappers(moduleName: String): List[PartialFunction[List[String], String]] =
    List(mapFields(moduleName))

  def template(
      moduleName: String,
      fullyQualifiedPath: String,
      contents: List[String]
  ): String =
    s"""package $fullyQualifiedPath
    |
    |import roguelike.model.items.Item
    |
    |enum $moduleName(val name: String, val damage: Int, val radius: Int, val turnCount: Int, val maxRange: Int) extends Item:
    |${contents.head}
    |""".stripMargin

}

object HostilesGen extends GameDataGenerator {

  val fileName: String = "hostiles.md"

  def mapFields(moduleName: String): PartialFunction[List[String], String] = {
    case name :: hp :: maxHp :: defense :: power :: xpGiven :: Nil =>
      val n = GameDataGenerator.sanitizeName(name)
      s"""  case $n extends $moduleName("$name", $hp, $maxHp, $defense, $power, $xpGiven)"""
  }

  def mappers(moduleName: String): List[PartialFunction[List[String], String]] =
    List(mapFields(moduleName))

  def template(
      moduleName: String,
      fullyQualifiedPath: String,
      contents: List[String]
  ): String =
    s"""package $fullyQualifiedPath
    |
    |enum $moduleName(val name: String, val hp: Int, val maxHp: Int, val defense: Int, val power: Int, val xpGiven: Int):
    |${contents.head}
    |""".stripMargin

}

object KeyMappingsGen extends GameDataGenerator {

  val fileName: String = "key-mappings.md"

  def toKeyFormat(name: String): String = {
    val k = name.split(' ').map(_.toUpperCase()).mkString("_")
    if (k.length == 1) "KEY_" + k else k
  }

  def genLookUpValues(
      moduleName: String
  ): PartialFunction[List[String], String] = { case command :: mapping :: Nil =>
    val n = GameDataGenerator.sanitizeName(command)
    val m = toKeyFormat(mapping)
    s"""  val $n: Key = Key.$m"""
  }

  def toHelpChar: String => String = {
    case "forward slash"              => "/"
    case "period"                     => "."
    case "add"                        => "+"
    case "subtract"                   => "-"
    case k if k.split(" ").length > 1 => k.split(" ").head
    case k                            => k
  }

  def genHelpText(moduleName: String): PartialFunction[List[String], String] = {
    case command :: mapping :: Nil =>
      val n = GameDataGenerator.sanitizeName(command)
      val m = toHelpChar(mapping)
      s"""|    |$command = $m"""
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
    |  val helpText: String =
    |    $tripleQuotes
    ${contents(1)}$tripleQuotes.stripMargin
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

  val fallback: String => PartialFunction[List[String], String] =
    moduleName => { case _ =>
      val msg = moduleName + " gen: Unexpected number of data fields"
      println(msg)
      throw new Exception(msg)
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

    val newContents = mappers(moduleName)
      .map { mpr =>
        rows.map(fields => mpr.orElse(fallback(moduleName))(fields))
      }
      .map(_.mkString("\n"))

    IO.write(out, template(moduleName, fullyQualifiedPath, newContents))

    println("Written: " + out.getCanonicalPath)

    Set(out)
  }

  def sanitizeName(name: String): String =
    name.split(' ').map(_.capitalize).mkString

}
