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

    def distTo(that: Pos): Int = math.abs(this.row - that.row) + math.abs(this.col - that.col)
  }

  def parsePos(str: String): Pos = {
    // Takes a string of the form "(x,y)"
    val fields = str.split("[(,)]") filter (_.trim.nonEmpty)
    Pos(fields(0).toInt, fields(1).toInt)
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

  sealed abstract class TerrainType(val cubeSupportMax: Int)
  trait SwitchTerrainType {
    val requiresStanding: Boolean
  }
  case object OutOfBounds    extends TerrainType(0)
  case object NormalTerrain  extends TerrainType(2)
  case object FragileTerrain extends TerrainType(1)
  case object SoftSwitch     extends TerrainType(2) with SwitchTerrainType { val requiresStanding: Boolean = false }
  case object HeavySwitch    extends TerrainType(2) with SwitchTerrainType { val requiresStanding: Boolean = true }
  case object Transporter    extends TerrainType(2) with SwitchTerrainType { val requiresStanding: Boolean = true }

  case class Terrain(types: Vector[Vector[TerrainType]]) {
    def apply(pos: Pos): TerrainType = {
      if (pos.row < 0 || pos.col < 0 || pos.row >= types.length || pos.col >= types(pos.row).length) OutOfBounds
      else types(pos.row)(pos.col)
    }

    def isLegal(block: Block): Boolean = block.getPositions groupBy (pos => pos) mapValues (_.length) forall { case (pos, count) => this(pos).cubeSupportMax >= count }

    def canSupportBlock(pos: Pos): Boolean = this(pos) match {
      case OutOfBounds => false
      case _ => true
    }

    def updated(pos: Pos, terrainType: TerrainType): Terrain =
      Terrain(types.updated(pos.row, types(pos.row).updated(pos.col, terrainType)))

    def respondToBlock(block: Block): (Block, Terrain) = {
      def getPressedLocationsOfType(SwitchType: SwitchTerrainType): Set[Pos] =
        if (SwitchType.requiresStanding && !block.isStanding) Set()
        else block.getPositions filter (pos => this(pos) match { case SwitchType => true; case _ => false }) toSet

      val transportersPressed = getPressedLocationsOfType(Transporter)
      val newBlockAfterTransporter = if (transportersPressed.isEmpty) block else {
        val (newPos1, newPos2) = transportInfos(transportersPressed.head)
        SmallBlockPair(newPos1, newPos2)
      }

      val softSwitchesPressed = getPressedLocationsOfType(SoftSwitch)
      val heavySwitchesPressed = getPressedLocationsOfType(HeavySwitch)
      val switchChanges = (softSwitchesPressed ++ heavySwitchesPressed) flatMap switchInfos.apply

      val newTerrAfterSwitches = switchChanges.foldLeft(this) {case (terr, (affectedPos, action)) =>
        terr.updated(affectedPos, action match {
          case TurnOn => NormalTerrain
          case TurnOff => OutOfBounds
          case Toggle => terr(affectedPos) match {
            case NormalTerrain => OutOfBounds
            case OutOfBounds => NormalTerrain
            case _ => throw new Error("toggling something that isn't normal or outOfBounds")
          }
        })
      }
      (newBlockAfterTransporter, newTerrAfterSwitches)
    }

    override def toString: String =
      types map (_ map {
        case OutOfBounds => '-'
        case HeavySwitch => 'x'
        case SoftSwitch => '.'
        case NormalTerrain => 'o'
        case Transporter => '@'
        case FragileTerrain => '!'
      } mkString) mkString "\n"
  }

  val switchInfos: Map[Pos, Set[(Pos, SwitchAction)]]
  sealed trait SwitchAction
  case object TurnOn  extends SwitchAction
  case object TurnOff extends SwitchAction
  case object Toggle  extends SwitchAction

  val transportInfos: Map[Pos, (Pos, Pos)]


  val startTerrain: Terrain

  /**
   * In Bloxorz, we can move left, right, up or down.
   * These moves are encoded as case objects.
   *
   * You can also press Space
   */
  sealed abstract class Action
  case object Left  extends Action
  case object Right extends Action
  case object Up    extends Action
  case object Down  extends Action
  case object Space extends Action

  sealed abstract class Block {
    def applyAction(action: Action): Block
    def getPositions: List[Pos]
    def isStanding: Boolean
  }

  /**
   * A block is represented by the position of the two cubes that
   * it consists of. We make sure that `b1` is lexicographically
   * smaller than `b2`.
   */
  case class BigBlock(b1: Pos, b2: Pos) extends Block {

    // checks the requirement mentioned above
    require(b1.row <= b2.row && b1.col <= b2.col, "Invalid block position: b1=" + b1 + ", b2=" + b2)

    /**
     * Returns a block where the `row` coordinates of `b1` and `b2` are
     * changed by `d1` and `d2`, respectively.
     */
    def deltaRow(d1: Int, d2: Int) = BigBlock(b1.deltaRow(d1), b2.deltaRow(d2))

    /**
     * Returns a block where the `col` coordinates of `b1` and `b2` are
     * changed by `d1` and `d2`, respectively.
     */
    def deltaCol(d1: Int, d2: Int) = BigBlock(b1.deltaCol(d1), b2.deltaCol(d2))


    /** The block obtained by moving left */
    override def applyAction(action: Action): BigBlock = action match {
      case Up =>    if (isStanding) deltaRow(-2, -1) else if (b1.row == b2.row) deltaRow(-1, -1) else deltaRow(-1, -2)
      case Down =>  if (isStanding) deltaRow(1, 2)   else if (b1.row == b2.row) deltaRow(1, 1)   else deltaRow(2, 1)
      case Left =>  if (isStanding) deltaCol(-2, -1) else if (b1.row == b2.row) deltaCol(-1, -2) else deltaCol(-1, -1)
      case Right => if (isStanding) deltaCol(1, 2)   else if (b1.row == b2.row) deltaCol(2, 1)   else deltaCol(1, 1)
      case Space => this
    }

    override def getPositions: List[Pos] = List(b1, b2)

    /**
     * Returns `true` if the block is standing.
     */
    override def isStanding: Boolean = b1 == b2
  }

  case class SmallBlockPair(b1: Pos, b2: Pos) extends Block {
    private case class SmallBlock(b: Pos) extends Block {
      override def applyAction(action: Action): SmallBlock = action match {
        case Up =>    SmallBlock(b.deltaRow(-1))
        case Down =>  SmallBlock(b.deltaRow(1))
        case Left =>  SmallBlock(b.deltaCol(-1))
        case Right => SmallBlock(b.deltaCol(1))
        case Space => this
      }

      override def getPositions: List[Pos] = List(b)

      override def isStanding: Boolean = false
    }

    private val sb1: SmallBlock = SmallBlock(b1)
    private val sb2: SmallBlock = SmallBlock(b2)

    override def applyAction(action: Action): Block = action match {
      case Space => SmallBlockPair(b2, b1)
      case _ => {
        val movedBlockPos = sb1.applyAction(action).getPositions.head
        if ((movedBlockPos distTo b2) == 1) {
          if (movedBlockPos.row <= b2.row && movedBlockPos.col <= b2.col) BigBlock(movedBlockPos, b2) else BigBlock(b2, movedBlockPos)
        }
        else SmallBlockPair(movedBlockPos, b2)
      }
    }

    override def getPositions: List[Pos] = sb1.getPositions ++ sb2.getPositions

    override def isStanding: Boolean = false
  }

  def startState: BloxorzState = BloxorzState(startTerrain, BigBlock(startPos, startPos))

  case class BloxorzState(terrain: Terrain, block: Block) {
    def isLegal: Boolean = terrain.isLegal(block)

    def transition(action: Action): BloxorzState = {
      val movedBlock = block.applyAction(action)
      val (nextBlock, nextTerrain) = terrain.respondToBlock(movedBlock)
      BloxorzState(nextTerrain, nextBlock)
    }

    def neighbors: List[(BloxorzState, Action)] = {
      val directionActions = List(Up, Down, Left, Right)
      (block match {
        case bb: BigBlock => directionActions
        case sbp: SmallBlockPair => Space :: directionActions
      }) map (action => (this.transition(action), action))
    }
//      List(Up, Down, Left, Right, Space) map (action => (this.transition(action), action))

    def legalNeighbors: List[(BloxorzState, Action)] = neighbors filter { case (state, act) => state.isLegal }
  }
}
