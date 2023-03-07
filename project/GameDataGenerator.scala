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

object AssetsGen extends GameDataGenerator {

  val fileName: String = "assets.md"

  def makeName(name: String): String = {
    GameDataGenerator.sanitizeName(name)
  }

  def assetNames: PartialFunction[List[String], String] = {
    case name :: typ :: phase :: path :: Nil =>
      val n = makeName(name)
      s"""  val $n: AssetName = AssetName("$n")"""
  }

  def audioAssets: PartialFunction[List[String], String] = {
    case name :: typ :: phase :: path :: Nil if typ == "audio" =>
      "      " + makeName(name) + ","

    case _ =>
      ""
  }

  def imageAssets: PartialFunction[List[String], String] = {
    case name :: typ :: phase :: path :: Nil if typ == "image" =>
      "      " + makeName(name) + ","

    case _ =>
      ""
  }

  def textAssets: PartialFunction[List[String], String] = {
    case name :: typ :: phase :: path :: Nil if typ == "text" =>
      "      " + makeName(name) + ","

    case _ =>
      ""
  }

  def assetSetLine(name: String, typ: String, path: String): String = {
    val n = makeName(name)
    typ match {
      case "audio" =>
        s"""      AssetType.Audio($n, AssetPath("$path")),"""

      case "image" =>
        s"""      AssetType.Image($n, AssetPath("$path")),"""

      case "text" =>
        s"""      AssetType.Text($n, AssetPath("$path")),"""

      case _ =>
        ""
    }
  }

  def initialAssetSet: PartialFunction[List[String], String] = {
    case name :: typ :: phase :: path :: Nil if phase == "init" =>
      assetSetLine(name, typ, path)

    case _ =>
      ""
  }

  def lazyAssetSet: PartialFunction[List[String], String] = {
    case name :: typ :: phase :: path :: Nil if phase == "lazy" =>
      assetSetLine(name, typ, path)

    case _ =>
      ""
  }

  def mappers(moduleName: String): List[PartialFunction[List[String], String]] =
    List(
      assetNames,
      audioAssets,
      imageAssets,
      textAssets,
      initialAssetSet,
      lazyAssetSet
    )

  def template(
      moduleName: String,
      fullyQualifiedPath: String,
      contents: List[String]
  ): String =
    s"""package $fullyQualifiedPath
    |
    |import indigo.*
    |
    |object $moduleName:
    |
    |${contents.head}
    |
    |  private val audioAssets: Set[AssetName] =
    |    Set(
    |${contents(1)}
    |    )
    |
    |  private val imagesAssets: Set[AssetName] =
    |    Set(
    |${contents(2)}
    |    )
    |
    |  private val textAssets: Set[AssetName] =
    |    Set(
    |${contents(3)}
    |    )
    |
    |  val initialAssets: Set[AssetType] =
    |    Set(
    |${contents(4)}
    |    )
    |
    |  val lazyAssets: Set[AssetType] =
    |    Set(
    |${contents(5)}
    |    )
    |  
    |  def loaded(assetCollection: AssetCollection): Boolean =
    |    audiosLoaded(assetCollection) && imagesLoaded(assetCollection) && textsLoaded(assetCollection)
    |  
    |  private def audiosLoaded(assetCollection: AssetCollection): Boolean =
    |    audioAssets.forall(a => assetCollection.findAudioDataByName(a).isDefined)
    |  
    |  private def imagesLoaded(assetCollection: AssetCollection): Boolean =
    |    imagesAssets.forall(i => assetCollection.findImageDataByName(i).isDefined)
    |  
    |  private def textsLoaded(assetCollection: AssetCollection): Boolean =
    |    textAssets.forall(t => assetCollection.findTextDataByName(t).isDefined)
    |
    |  def lazyAssetsLoaded(assetCollection: AssetCollection): Boolean =
    |    lazyAssets.forall {
    |      case t: AssetType.Text =>
    |        assetCollection.findTextDataByName(t.name).isDefined
    |  
    |      case i: AssetType.Image =>
    |        assetCollection.findImageDataByName(i.name).isDefined
    |  
    |      case a: AssetType.Audio =>
    |        assetCollection.findAudioDataByName(a.name).isDefined
    |  
    |      case f: AssetType.Font =>
    |        assetCollection.findFontDataByName(f.name).isDefined
    |  
    |      case t: AssetType.Tagged =>
    |        t.images.forall(i => assetCollection.findImageDataByName(i.name).isDefined)
    |    }
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
