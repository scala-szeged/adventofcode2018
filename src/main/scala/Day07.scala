

object Day07 {

  private def getInstructions(lines: List[String]) = {
    lines.map(line => (line(5), line(36)))
  }

  /*
    -->A--->B--
   /    \      \
  C      -->D----->E
   \           /
    ---->F-----
     */
  def task1(lines: List[String]): String = {
    val graf = getInstructions(lines).sortBy(_._1)
    val count = graf.map(_._1).distinct.size

    //noinspection ScalaUnnecessaryParentheses
    def loop(remaining: List[(Char, Char)], result: List[Char]): List[Char] = {
      val notResultYet: ((Char, Char)) => Boolean = {
        case (x: Char, _: Char) => !result.contains(x)
      }
      val ys = graf.filter(notResultYet).map(_._2)
      remaining.filter(notResultYet) match {
        case Nil =>
          if (result.size == count)
            result
          else
            loop(graf, result)

        case (x, y) :: tail =>
          if (ys.contains(x))
            loop(tail, result)
          else
            loop(graf, x :: result)
      }
    }

    val r = loop(graf, List.empty[Char])
    r.reverse.mkString + graf.filter{ case (_: Char, y: Char) => !r.contains(y) }.head._2
  }

  /*
  C  A
  C  F
  A  B
  A  D
  B  E
  D  E
  F  E
   */

  def task2(lines: List[String], workerCount: Int, extraTime: Int): Int = {
    0
  }
}
