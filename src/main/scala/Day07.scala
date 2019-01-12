

object Day07 {

  private def getInstructions(lines: List[String]) = {
    lines.map(line => (line(5), line(36)))
  }

  /*
  Sample input:

  C  A
  C  F
  A  B
  A  D
  B  E
  D  E
  F  E

  The graph given by the above input:

    -->A--->B--
   /    \      \
  C      -->D----->E
   \           /
    ---->F-----
     */
  def task1(lines: List[String]): String = {
    val graph = getInstructions(lines).sortBy(_._1)
    val count = graph.map(_._1).distinct.size

    //noinspection ScalaUnnecessaryParentheses
    def loop(remaining: List[(Char, Char)], result: List[Char]): List[Char] = {
      val xNotResultYet: ((Char, Char)) => Boolean = {
        case (x: Char, _: Char) => !result.contains(x)
      }
      val blocked = graph.filter(xNotResultYet).map(_._2)
      remaining.filter(xNotResultYet) match {
        case Nil =>
          if (result.size == count)
            result
          else
            loop(graph, result)

        case (x, y) :: tail =>
          if (blocked.contains(x))
            loop(tail, result)
          else
            loop(graph, x :: result)
      }
    }

    val r = loop(graph, List.empty[Char])
    r.reverse.mkString + graph.filter{ case (_: Char, y: Char) => !r.contains(y) }.head._2
  }

  def task2(lines: List[String], workerCount: Int, extraTime: Int): Int = {
    0
  }
}
