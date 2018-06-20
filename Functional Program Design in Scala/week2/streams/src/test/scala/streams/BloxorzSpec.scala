package streams

import org.specs2.mutable.Specification

class BloxorzSpec extends Specification {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
      * This method applies a list of moves `ls` to the block at position
      * `startPos`. This can be used to verify if a certain list of moves
      * is a valid solution, i.e. leads to the goal.
      */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
      }
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  "terrain function level 1" >> {
    new Level1 {
      terrain(Pos(0, 0)) must_== true
      terrain(Pos(1, 1)) must_== true
      terrain(Pos(4, 7)) must_== true
      terrain(Pos(5, 8)) must_== true
      terrain(Pos(5, 9)) must_== false
      terrain(Pos(4, 9)) must_== true
      terrain(Pos(6, 8)) must_== false
      terrain(Pos(4, 11)) must_== false
      terrain(Pos(-1, 0)) must_== false
      terrain(Pos(0, -1)) must_== false
    } must not(throwA[Exception])
  }

  "startPos must work" >> {
    new Level1 {
      startPos must_== Pos(1, 1)
    } must not(throwA[Exception])
  }

  "solution must be a path to the goal" >> {
    new Level1{
      solve(solution) must_== Block(goal, goal)
    } must not(throwA[Exception])
  }

  "soluton must be the shortest path" >> {
    new Level1 {
      solution.length must_== optsolution.length
    } must not(throwA[Exception])
  }

}
