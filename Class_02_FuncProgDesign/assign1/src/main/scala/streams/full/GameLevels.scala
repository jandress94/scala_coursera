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
  }

  // http://www.coolmath-games.com/0-bloxorz

  /*
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
  /*

//  new BloxorzLevel("Level 23", "293486", """""")
}

