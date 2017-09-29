package streams.full

/**
 * This component implements a parser to define terrains from a
 * graphical ASCII representation.
 *
 * When mixing in that component, a level can be defined by
 * defining the field `level` in the following form:
 *
 *   val level =
 *     """------
 *       |--ST--
 *       |--oo--
 *       |--oo--
 *       |------""".stripMargin
 *
 * - The `-` character denotes parts which are outside the terrain
 * - `o` denotes fields which are part of the terrain
 * - `S` denotes the start position of the block (which is also considered
     *inside the terrain)
 * - `T` denotes the final position of the block (which is also considered
     *inside the terrain)
 * - `.` denotes a soft switch, which activates if either cube of the block touches it
 * - `x` denotes a heavy switch, which activates if both cubes of the block touch it (the block is standing)
 *
 * In this example, the first and last lines could be omitted, and
 * also the columns that consist of `-` characters only.
  */
trait StringParserTerrain extends GameDef {

  /**
   * A ASCII representation of the terrain. This field should remain
   * abstract here.
   */
  val level: String

  /**
   * This function should return the position of character `c` in the
   * terrain described by `levelVector`. You can assume that the `c`
   * appears exactly once in the terrain.
   *
   * Hint: you can use the functions `indexWhere` and / or `indexOf` of the
   * `Vector` class
   */
  def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = {
    val row = levelVector indexWhere (_ contains c)
    val col = levelVector(row) indexOf c
    Pos(row, col)
  }

  lazy val startCharGrid: Vector[Vector[Char]] =
    Vector(level.split("\n").takeWhile(_.trim.nonEmpty).map(str => Vector(str.trim: _*)): _*)

  lazy val startTerrain: Terrain = Terrain(
    startCharGrid map (_ map {
      case '-' => OutOfBounds
      case 'x' => HeavySwitch
      case '.' => SoftSwitch
      case '@' => Transporter
      case '!' => FragileTerrain
      case _ => NormalTerrain
    }))

  lazy val infos: List[(Pos, String)] =
    level.split("\n").drop(startCharGrid.length + 1) map (str => {
      val (switchPosStr, restStr) = str.span(_ != ' ')
      (parsePos(switchPosStr), restStr.tail)
    }) toList

  lazy val switchInfos: Map[Pos, Set[(Pos, SwitchAction)]] =
    infos filter { case (pos, str) => startTerrain(pos) match {
      case SoftSwitch => true; case HeavySwitch => true; case _ => false
    }} groupBy (_._1) mapValues (_ map { case (pos, str) => {
      val fields = str.split(" ")
      val action = fields(1) match {
        case x if x contains "Tog" => Toggle
        case x if x contains "On" => TurnOn
        case x if x contains "Off" => TurnOff
        case _ => throw new Exception("Switch affect not recognized: " + fields(1))
      }
      (parsePos(fields(0)), action)
    } } toSet)

  lazy val transportInfos: Map[Pos, (Pos, Pos)] =
    infos filter { case (pos, str) => startTerrain(pos) match {
      case Transporter => true; case _ => false
    }} map { case (pos, str) => {
      val fields = str.split(" ")
      (pos, (parsePos(fields(0)), parsePos(fields(1))))
    }} toMap

  lazy val startPos: Pos = findChar('S', startCharGrid)
  lazy val goal: Pos = findChar('T', startCharGrid)

  def makeMoves(moves: List[Action]): BloxorzState =
    moves.foldLeft(startState) { case (state, move) =>
      require(state.isLegal) // The solution must always lead to legal blocks
      state.transition(move)
    }
}
