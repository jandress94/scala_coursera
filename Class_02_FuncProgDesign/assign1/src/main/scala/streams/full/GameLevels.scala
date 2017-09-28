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
      |---------ooo---""".stripMargin).solveAndPrint()
}

