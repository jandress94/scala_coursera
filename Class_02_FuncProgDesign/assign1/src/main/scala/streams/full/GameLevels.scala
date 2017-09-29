package streams.full

object GameLevels extends App {

  class BloxorzLevel(title: String, passCode: String, val level: String) extends GameDef with Solver with StringParserTerrain {
    def solveAndPrint(): Unit = {
      println()
      println(title + ":")
      println(solution mkString "\n")
    }
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

//  new BloxorzLevel("Level 10", "300590",
//    """ooo-----oooooo
//      |oTo--o--oSoo@o
//      |ooo-----oooo--
//      |---------ooo--
//      |-----------oo-
//      |------------o-
//      |------------o-
//      |-----------oo-
//      |----ooooo--oo-
//      |----o.--oooxo-
//      |
//      |(1,12) (1,12) (1,9)
//      |(9,5) (1,3) Tog
//      |(9,5) (1,4) Tog
//      |(9,11) (1,6) Tog
//      |(9,11) (1,7) Tog
//      |(9,11) (2,12) Tog
//      |(9,11) (3,12) Tog""".stripMargin).solveAndPrint()

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
      |ooooooooooooTo-
      |ooo---ooo--.oo-
      |
      |(5,7) (1,13) (8,1)
    """.stripMargin)
}

