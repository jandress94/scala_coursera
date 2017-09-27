package streams.full

/**
 * This trait represents the layout and building blocks of the game
 */
trait GameDef {

  /**
   * The case class `Pos` encodes positions in the terrain.
   *
   * IMPORTANT NOTE
   *  - The `row` coordinate denotes the position on the vertical axis
   *  - The `col` coordinate is used for the horizontal axis
   *  - The coordinates increase when moving down and right
   *
   * Illustration:
   *
   *     0 1 2 3   <- col axis
   *   0 o o o o
   *   1 o o o o
   *   2 o # o o    # is at position Pos(2, 1)
   *   3 o o o o
   *
   *   ^
   *   |
   *
   *   row axis
   */
  case class Pos(row: Int, col: Int) {
    /** The position obtained by changing the `row` coordinate by `d` */
    def deltaRow(d: Int): Pos = copy(row = row + d)

    /** The position obtained by changing the `col` coordinate by `d` */
    def deltaCol(d: Int): Pos = copy(col = col + d)
  }

  /**
   * The position where the block is located initially.
   *
   * This value is left abstract, it will be defined in concrete
   * instances of the game.
   */
  val startPos: Pos

  /**
   * The target position where the block has to go.
   * This value is left abstract.
   */
  val goal: Pos

  /**
   * The terrain is represented as a function from positions to
   * booleans. The function returns `true` for every position that
   * is inside the terrain.
   *
   * As explained in the documentation of class `Pos`, the `row` axis
   * is the vertical one and increases from top to bottom.
   */

  sealed abstract class TerrainType
  case object OutOfBounds   extends TerrainType
  case object NormalTerrain extends TerrainType
  case object SoftSwitch    extends TerrainType
  case object HeavySwitch   extends TerrainType

  case class Terrain(types: Vector[Vector[TerrainType]]) {
    def canSupportBlock(pos: Pos): Boolean =
      if (pos.row < 0 || pos.col < 0 || pos.row >= types.length || pos.col >= types(pos.row).length) false
      else types(pos.row)(pos.col) match {
        case OutOfBounds => false
        case _ => true
      }

    def respondToBlock(block: Block): Terrain = {
      this
    }
  }

  val startTerrain: Terrain

  /**
   * In Bloxorz, we can move left, right, Up or down.
   * These moves are encoded as case objects.
   */
  sealed abstract class Action
  case object Left  extends Action
  case object Right extends Action
  case object Up    extends Action
  case object Down  extends Action

  /**
   * A block is represented by the position of the two cubes that
   * it consists of. We make sure that `b1` is lexicographically
   * smaller than `b2`.
   */
  case class Block(b1: Pos, b2: Pos) {

    // checks the requirement mentioned above
    require(b1.row <= b2.row && b1.col <= b2.col, "Invalid block position: b1=" + b1 + ", b2=" + b2)

    /**
     * Returns a block where the `row` coordinates of `b1` and `b2` are
     * changed by `d1` and `d2`, respectively.
     */
    def deltaRow(d1: Int, d2: Int) = Block(b1.deltaRow(d1), b2.deltaRow(d2))

    /**
     * Returns a block where the `col` coordinates of `b1` and `b2` are
     * changed by `d1` and `d2`, respectively.
     */
    def deltaCol(d1: Int, d2: Int) = Block(b1.deltaCol(d1), b2.deltaCol(d2))


    /** The block obtained by moving left */
    def applyAction(action: Action): Block = action match {
      case Up =>    if (isStanding) deltaRow(-2, -1) else if (b1.row == b2.row) deltaRow(-1, -1) else deltaRow(-1, -2)
      case Down =>  if (isStanding) deltaRow(1, 2)   else if (b1.row == b2.row) deltaRow(1, 1)   else deltaRow(2, 1)
      case Left =>  if (isStanding) deltaCol(-2, -1) else if (b1.row == b2.row) deltaCol(-1, -2) else deltaCol(-1, -1)
      case Right => if (isStanding) deltaCol(1, 2)   else if (b1.row == b2.row) deltaCol(2, 1)   else deltaCol(1, 1)
    }

    /**
     * Returns `true` if the block is standing.
     */
    def isStanding: Boolean = b1 == b2

//      this match {
//      case Block(Pos(r1, c1), Pos(r2, c2)) => r1 == r2 && c1 == c2
//    }

    /**
     * Returns `true` if the block is entirely inside the terrain.
     */
    def isLegal(terrain: Terrain): Boolean = terrain.canSupportBlock(b1) && terrain.canSupportBlock(b2)
  }

  def startState: BloxorzState = BloxorzState(startTerrain, Block(startPos, startPos))

  case class BloxorzState(terrain: Terrain, block: Block) {
    def isLegal: Boolean = block.isLegal(terrain)

    def neighbors: List[(BloxorzState, Action)] =
      List(Up, Down, Left, Right) map (action => {
        val nextBlock = block.applyAction(action)
        val nextTerrain = terrain.respondToBlock(nextBlock)
        (BloxorzState(nextTerrain, nextBlock), action)
      })

    def legalNeighbors: List[(BloxorzState, Action)] = neighbors filter { case (state, act) => state.isLegal }
  }
}
