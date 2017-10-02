package streams.full

object GameLevels extends App {

  class BloxorzLevel(title: String, passCode: String, val level: String) extends GameDef with Solver with StringParserTerrain {
    def solveAndPrint(): Unit = {
      println()
      println(title + ":")
      println(solution mkString "\n")
    }

    def printPaths(limit: Int = -1): Unit = {
      val streamToUse = if (limit < 0) pathsFromStart else pathsFromStart take (limit)

      println()
      println(title + ":")
      for {
        (state, actionList) <- streamToUse takeWhile{ case (state, actionList) => !done(state)}
      } println(actionList)
    }

    def printStartStateSwitches(): Unit =
      for {
        row <- startTerrain.types.indices
        col <- startTerrain.types(row).indices
        if {startTerrain.types(row)(col) match { case s: SwitchTerrainType => true; case _ => false}}
      } println(startTerrain.types(row)(col) + " at (" + row + "," + col + ")")
  }

  // http://www.coolmath-games.com/0-bloxorz


  new BloxorzLevel("Level 1", "780464",
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 2", "290299",
    """------oooo--ooo
      |oooo--ooxo--oTo
      |oo.o--oooo--ooo
      |oooo--oooo--ooo
      |oSoo--oooo--ooo
      |oooo--oooo-----
      |
      |(2,2) (4,4) Tog
      |(2,2) (4,5) Tog
      |(1,8) (4,10) Tog
      |(1,8) (4,11) Tog""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 3", "918660",
    """------ooooooo--
      |oooo--ooo--oo--
      |ooooooooo--oooo
      |oSoo-------ooTo
      |oooo-------oooo
      |------------ooo""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 4", "520967",
    """---!!!!!!!----
      |---!!!!!!!----
      |oooo-----ooo--
      |ooo-------oo--
      |ooo-------oo--
      |oSo--oooo!!!!!
      |ooo--oooo!!!!!
      |-----oTo--!!o!
      |-----ooo--!!!!""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 5", "028431",
    """-----------oooo
      |-ooooooo.ooooSo
      |-oooo-------ooo
      |-oo.o----------
      |-oooo----------
      |---ooo.oooooo--
      |----------oooo.
      |ooo-------ooooo
      |oTooooooooooo--
      |oooo-----------
      |
      |(1,8) (1,6) Tog
      |(1,8) (1,5) Tog
      |(3,3) (8,5) On
      |(3,3) (8,6) On
      |(5,6) (8,5) Off
      |(5,6) (8,6) Off
      |(6,14) (8,5) Tog
      |(6,14) (8,6) Tog""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 6", "524383",
    """-----oooooo----
      |-----o--ooo----
      |-----o--ooooo--
      |Sooooo-----oooo
      |----ooo----ooTo
      |----ooo-----ooo
      |------o--oo----
      |------ooooo----
      |------ooooo----
      |-------ooo-----""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 7", "189493",
    """--------oooo---
      |--------oooo---
      |ooo-----o--oooo
      |oSooooooo---oTo
      |ooo----oox--ooo
      |ooo----ooo--ooo
      |-oo----o-------
      |--oooooo-------
      |
      |(4,9) (6,3) Tog""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 8", "499707",
    """---------ooo---
      |---------ooo---
      |---------ooo---
      |oooooo---oooooo
      |oSoo@o---ooooTo
      |oooooo---oooooo
      |---------ooo---
      |---------ooo---
      |---------ooo---
      |
      |(4,4) (1,10) (7,10)""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 9", "074355",
    """oooo---o---oooo
      |oSoo---o---oo@o
      |ooooooooooooooo
      |------oTo------
      |------ooo------
      |
      |(1,13) (1,12) (1,2)""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 10", "300590",
    """ooo-----oooooo
      |oTo--o--oSoo@o
      |ooo-----oooo--
      |---------ooo--
      |-----------oo-
      |------------o-
      |------------o-
      |-----------oo-
      |----ooooo--oo-
      |----o.--oooxo-
      |
      |(1,12) (1,12) (1,9)
      |(9,5) (1,3) Tog
      |(9,5) (1,4) Tog
      |(9,11) (1,6) Tog
      |(9,11) (1,7) Tog
      |(9,11) (2,12) Tog
      |(9,11) (3,12) Tog""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 11", "291709",
    """-oooo-------
      |-oToo-------
      |-ooo--------
      |-o---oooooo-
      |-o---oo--oo-
      |Soooooo--ooo
      |-----o.----o
      |-----oooo--o
      |-----ooooooo
      |--------ooo-
      |
      |(6,6) (0,4) Off
      |(6,6) (1,4) Off""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 12", "958640",
    """------------x
      |-----ooo--ooo
      |-----oxooooo-
      |---ooooo--oo-
      |---oTo----oo-
      |-ooooo---oooo
      |ooSo-----oooo
      |oooo--ooooo--
      |-----ooo-----
      |-----ooo-----
      |
      |(2,6) (2,12) Tog
      |(0,12) (4,6) Tog""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 13", "448106",
    """ooo!oooo!oooo-
      |oo--------ooo-
      |oo---------ooo
      |ooo---ooo--oSo
      |ooo!!!oTo--ooo
      |ooo--!ooo--o--
      |--o--!!!!!oo--
      |--ooo!!o!!!---
      |---oo!!!!!!---
      |---ooo--oo----""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 14", "210362",
    """--------ooo---
      |---ooo--ooo---
      |o--oSooooooooo
      |o--ooo------xo
      |o-----------oo
      |o-----------oo
      |o-------oooooo
      |ooooo---ooo---
      |-ooTo---ooo---
      |--ooo---ooooox
      |
      |(9,13) (3,1) Tog
      |(9,13) (3,2) Tog
      |(3,12) (2,1) Tog
      |(3,12) (2,2) Tog""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 15", "098598",
    """-------ooo--ooo
      |----oooooo--xoo
      |oo--o--ooo--ooo
      |ooooo---.------
      |oo-------------
      |-o-----@-------
      |-o-----o-------
      |ooo---ooo--.oo-
      |oSooooooooooTo-
      |ooo---ooo--.oo-
      |
      |(5,7) (1,13) (8,1)
      |(7,11) (8,10) Off
      |(7,11) (8,9) Off
      |(9,11) (8,10) Off
      |(9,11) (8,9) Off
      |(3,8) (1,5) Tog
      |(3,8) (1,6) Tog
      |(3,8) (1,10) Tog
      |(3,8) (1,11) Tog
      |(1,12) (1,5) Tog
      |(1,12) (1,6) Tog
      |(1,12) (2,2) Tog
      |(1,12) (2,3) Tog""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 16", "000241",
    """-@--------ooo
      |@o@--xxo--oTo
      |-@--------ooo
      |-------------
      |--ooo---ooo--
      |--oSooooo@o--
      |--ooo---ooo--
      |
      |(5,9) (0,1) (1,0)
      |(1,2) (1,0) (1,2)
      |(1,0) (1,2) (0,1)
      |(2,1) (2,1) (1,0)
      |(0,1) (1,7) (1,5)
      |(1,5) (1,4) On
      |(1,5) (1,3) On
      |(1,6) (1,8) On
      |(1,6) (1,9) On""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 17", "683596",
    """ooo------------
      |oSooooooo---ooo
      |ooo-----oooooTo
      |ooo---------xxo
      |ooo------------
      |ooo------------
      |ooo----ooooox--
      |oooooooo---oo--
      |o.o--------oo--
      |ooo--------ox--
      |
      |(8,1) (7,8) Tog
      |(6,12) (2,7) On
      |(9,12) (7,8) Off
      |(9,12) (1,9) On
      |(3,12) (6,6) Off
      |(3,13) (6,6) On""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 18", "284933",
    """-------.-------
      |oo.o---o-------
      |ooooo--o-------
      |o.Sooooo--oo--o
      |ooooo---o---o--
      |oo.o----o---o--
      |o-------.--ooo-
      |o---------ooTo-
      |o--x------oooo-
      |
      |(1,2) (8,1) Off
      |(1,2) (8,2) Off
      |(1,2) (3,12) Off
      |(1,2) (3,13) Off
      |(3,1) (3,8) Off
      |(3,1) (3,9) Off
      |(5,2) (8,1) Off
      |(5,2) (8,2) Off
      |(5,2) (3,12) Off
      |(5,2) (3,13) Off
      |(0,7) (3,8) On
      |(0,7) (3,9) On
      |(6,8) (8,1) On
      |(6,8) (8,2) On
      |(6,8) (3,12) On
      |(6,8) (3,13) On
      |(8,3) (4,5) Tog""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 19", "119785",
    """-Soooooooo.oooo
      |-----oo------oo
      |-----oo------oo
      |-------------oo
      |-------------oo
      |ooo--oo--o.oooo
      |oTo--oo--------
      |ooo--oo--------
      |-oo--oo--------
      |-ooooooooo.ooo-
      |
      |(0,10) (5,7) Tog
      |(0,10) (5,8) Tog
      |(5,10) (9,2) Off
      |(5,10) (9,3) Off
      |(9,10) (9,2) On
      |(9,10) (9,3) On""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 20", "543019",
    """------------ooo
      |--oooooooo--ooo
      |--ooo--.So--ooo
      |--ooo--ooo-----
      |--o.o--@o.-----
      |--ooo--ooo-----
      |oooo---ooo--.oo
      |o.----------ooo
      |------------oTo
      |------------ooo
      |
      |(4,9) (1,5) Off
      |(4,9) (1,6) Off
      |(2,7) (1,5) Off
      |(2,7) (1,6) Off
      |(4,7) (1,13) (7,13)
      |(6,12) (6,11) Tog
      |(6,12) (6,10) Tog
      |(4,3) (1,5) Off
      |(4,3) (1,6) Off
      |(7,1) (1,11) Tog
      |(7,1) (1,10) Tog""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 21", "728724",
    """--------oo-----
      |-------ooo-----
      |oo--oooooo-----
      |oSoooo--o------
      |oooo----o---ooo
      |-oo-----xooooTo
      |--o-----xo--ooo
      |--ooo---oo-----
      |---ooo--oo-----
      |----oooooo-----
      |
      |(5,8) (9,3) Tog
      |(6,8) (7,5) Tog""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 22", "987319",
    """-----oo----ooo
      |---oooooo--oTo
      |oooooo.ooooooo
      |oSoo.--ooooo--
      |ooo------ooo--
      |-o--------o---
      |-o--------o---
      |-o-------oo---
      |-oo------oo---
      |--x------x----
      |
      |(3,4) (7,2) Off
      |(3,4) (3,12) Off
      |(2,6) (7,2) Off
      |(2,6) (3,12) Off
      |(9,9) (7,2) Tog
      |(9,2) (3,12) Tog""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 23", "293486",
    """-ooo--------ooo
      |-oxo--------o.o
      |-ooo---oooooooo
      |-ooo---oTo--oo.
      |o---o--ooo----o
      |.---o--!!!----o
      |o--ooo!!!!!oooo
      |---oSo!!!!!o@o-
      |---ooo!!!!!ooo-
      |---ooooo-------
      |
      |(3,14) (2,11) Off
      |(3,14) (2,10) Off
      |(3,14) (6,14) Off
      |(1,13) (6,1) On
      |(1,13) (6,2) On
      |(1,13) (9,8) Tog
      |(7,12) (7,12) (2,2)
      |(5,0) (3,0) On
      |(5,0) (6,1) Off
      |(5,0) (6,2) Off
      |(1,2) (3,4) On""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 24", "088198",
    """----------oooo
      |----oooooooxo@
      |-S--oxo---oooo
      |xo--oo------o-
      |oo--o-------o-
      |ooooo-----ooo-
      |ooo--ooo--oTo-
      |-----xo---ooo-
      |
      |(1,11) (2,2) On
      |(1,11) (2,3) On
      |(1,13) (6,5) (6,7)
      |(3,0) (1,3) On
      |(2,5) (7,7) On
      |(7,5) (6,8) On
      |(7,5) (6,9) On""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 25", "250453",
    """--oo----------
      |--ooo---------
      |--oo.-----ooo-
      |---oooo---oTo-
      |------oo--ooo-
      |-oo---oo------
      |ooxooooo------
      |oSo---oo---ooo
      |ooo---oo.ooooo
      |-----------ooo
      |
      |(6,2) (4,8) On
      |(6,2) (4,9) On
      |(8,8) (3,7) On
      |(8,8) (6,4) Off
      |(8,8) (6,5) Off
      |(2,4) (4,8) Tog
      |(2,4) (4,9) Tog
      |(2,4) (2,13) Tog
      |(2,4) (3,13) Tog""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 26", "426329",
    """-----oooo----@
      |-----oo.ooo--o
      |----ooooooo--o
      |oooooooo--oooo
      |ooo---o---oo--
      |ooo---o---S---
      |-o----ooo-----
      |-x----oTo-----
      |------ooo-----
      |
      |(1,7) (3,2) Off
      |(1,7) (3,3) Off
      |(0,13) (3,12) (5,10)
      |(7,1) (4,3) On
      |(7,1) (7,9) On""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 27", "660141",
    """ooo----oooooooo
      |oSooooooooo--oo
      |ooo----oo----oo
      |------------oxo
      |------------oo-
      |ooo--!!!!o--..-
      |oTo!!!!!!!--ooo
      |ooo!!!!!!!!!ooo
      |-----!!!!!!!ooo
      |------oooo-----
      |
      |(3,13) (9,6) Off
      |(3,13) (9,9) Off
      |(5,13) (9,9) Off
      |(5,12) (9,6) Off""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 28", "769721",
    """-oooooo--------
      |-oo--ooo-------
      |!!S--oooo------
      |!!-----ooo-----
      |!!------ooo----
      |!ooo-----oo@---
      |-oTo------ooooo
      |-oooooo---o.ooo
      |--o--oo---ooo--
      |--o--oooooooo--
      |
      |(7,11) (0,3) Off
      |(7,11) (0,4) Off
      |(7,11) (9,8) Off
      |(7,11) (9,9) Off
      |(5,11) (6,14) (9,12)""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 29", "691859",
    """--.ooo---o--x--
      |-----o---o-----
      |-----ooooo-----
      |x--ooooSoooo--x
      |-----ooooo-----
      |------o--o-----
      |------o--ooo.--
      |ooo--oo--o-----
      |oTo--o---o-----
      |ooo------ooo.--
      |
      |(0,2) (0,10) On
      |(0,2) (0,11) On
      |(0,2) (6,10) Off
      |(0,2) (6,11) Off
      |(0,12) (6,5) On
      |(0,12) (7,5) On
      |(9,12) (0,10) Off
      |(9,12) (0,11) Off
      |(9,12) (0,3) Off
      |(9,12) (0,4) Off
      |(9,12) (6,10) Off
      |(9,12) (6,11) Off
      |(9,12) (3,12) On
      |(9,12) (3,13) On
      |(3,14) (9,3) On
      |(6,12) (3,1) On
      |(6,12) (3,2) On
      |(3,0) (8,3) On
      |(3,0) (8,4) On
      |(3,0) (9,10) Off
      |(3,0) (9,11) Off""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 30", "280351",
    """---ooooo!!oooo-
      |---oToo-----!o-
      |---ooo------!ox
      |-------!ooooooo
      |--S----!!-----o
      |-xo!---!!-----o
      |!!!!---oo----oo
      |!!!o!o!!o!--xo-
      |o!!!!!!!!!!!o--
      |-!o!!!--!!!!o--
      |
      |(5,1) (3,10) On
      |(5,1) (3,11) On
      |(7,12) (7,14) Tog
      |(2,14) (3,10) Off
      |(2,14) (3,11) Off
      |(2,14) (6,12) On
      |(2,14) (6,9) On""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 31", "138620",
    """-----------ooo-
      |-ooo----x--oTo-
      |-oooooooo--ooo-
      |-ooo--ooo---o--
      |-!!!--.oo---!--
      |--!---ooo--!!!-
      |--o---ooo--ooo-
      |oooo--o.ooooSo-
      |ooxo--x----ooo-
      |oooo-----------
      |
      |(7,7) (2,4) Off
      |(7,7) (2,5) Off
      |(7,7) (2,9) Off
      |(7,7) (2,10) Off
      |(7,7) (7,4) Off
      |(7,7) (7,5) Off
      |(7,7) (7,9) Off
      |(7,7) (7,10) Off
      |(4,6) (2,4) Off
      |(4,6) (2,5) Off
      |(4,6) (2,9) Off
      |(4,6) (2,10) Off
      |(4,6) (7,4) Off
      |(4,6) (7,5) Off
      |(4,6) (7,9) Off
      |(4,6) (7,10) Off
      |(8,6) (7,4) Tog
      |(8,6) (7,5) Tog
      |(1,8) (2,9) Tog
      |(1,8) (2,10) Tog
      |(8,2) (2,4) Off
      |(8,2) (2,5) Off
      |(8,2) (0,14) On
      |(8,2) (1,14) On
      |(8,2) (2,14) On""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 32", "879021",
    """------------ox
      |--oooooo---ooo
      |-ooo--oo--oxoo
      |-oTo---ooooo--
      |-ooo----ooo---
      |---------oo---
      |----ooo--oS---
      |oo--oxo--oo---
      |oo--ooooooo---
      |
      |(2,11) (8,2) Tog
      |(2,11) (8,3) Tog
      |(0,13) (1,4) Tog
      |(0,13) (1,5) Tog
      |(0,13) (7,2) Tog
      |(0,13) (7,3) Tog
      |(7,5) (2,4) Tog
      |(7,5) (2,5) Tog""".stripMargin).solveAndPrint()

  new BloxorzLevel("Level 33", "614955",
    """-----oo.ooo----
      |-----oooooo----
      |ooo--.oo.ooooo-
      |oSooooooo..oo.-
      |-----oo.oo.ooo-
      |-----oooooo.oo-
      |ooo--oooooo.ooo
      |oToooo.o--ooo.x
      |ooo--ooo---oooo
      |ooo---------ooo
      |
      |(7,14) (1,11) On
      |(0,7) (7,3) Off
      |(0,7) (7,4) Off
      |(2,5) (7,3) Off
      |(2,5) (7,4) Off
      |(2,8) (7,3) Off
      |(2,8) (7,4) Off
      |(3,9) (7,3) Off
      |(3,9) (7,4) Off
      |(3,10) (7,3) Off
      |(3,10) (7,4) Off
      |(3,13) (7,3) Off
      |(3,13) (7,4) Off
      |(4,7) (7,3) Off
      |(4,7) (7,4) Off
      |(4,10) (7,3) Off
      |(4,10) (7,4) Off
      |(5,11) (7,3) Off
      |(5,11) (7,4) Off
      |(6,11) (7,3) Off
      |(6,11) (7,4) Off
      |(7,6) (7,3) Off
      |(7,6) (7,4) Off
      |(7,13) (7,3) Off
      |(7,13) (7,4) Off""".stripMargin).solveAndPrint()
}

