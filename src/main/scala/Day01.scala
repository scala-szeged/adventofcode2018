import scala.annotation.tailrec


object Day01 {

  def task1(changes: List[Int]): Int = changes.sum

  def task2(changes: List[Int]): Int = {

    @tailrec
    def reachedTwice(remaining: List[Int], frequencies: Set[Int] = Set(0), lastFrequency: Int = 0): Int = remaining match {
      case head :: tail =>
        val f = lastFrequency + head
        if (frequencies.contains(f))
          f
        else
          reachedTwice(tail, frequencies + f, f)

      case Nil =>
        reachedTwice(changes, frequencies, lastFrequency)
    }

    reachedTwice(changes)
  }
}