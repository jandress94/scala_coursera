package streams.full

object GameLevels extends App {

  class BloxorzLevel(title: String, val level: String) extends GameDef with Solver with StringParserTerrain {
    def solveAndPrint(): Unit = {
      println()
      println(title + ":")
      println(solution mkString "\n")
    }
  }

  new BloxorzLevel("Level 1",
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-
    """.stripMargin).solveAndPrint()

  new BloxorzLevel("Level 2",
    """------oooo--ooo
      |oooo--ooxo--oTo
      |oo.o--oooo--ooo
      |oooo--oooo--ooo
      |oSoo--oooo--ooo
      |oooo--oooo-----
    """.stripMargin).solveAndPrint()

  new BloxorzLevel("Level 3",
    """------ooooooo--
      |oooo--ooo--oo--
      |ooooooooo--oooo
      |oSoo-------ooTo
      |oooo-------oooo
      |------------ooo
    """.stripMargin).solveAndPrint()

  new BloxorzLevel("Level 4",
    """---!!!!!!!----
      |---!!!!!!!----
      |oooo-----ooo--
      |ooo-------oo--
      |ooo-------oo--
      |oSo--oooo!!!!!
      |ooo--oooo!!!!!
      |-----oTo--!!o!
      |-----ooo--!!!!
    """.stripMargin).solveAndPrint()
}
