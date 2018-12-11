/*
2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2
A----------------------------------
    B----------- C-----------
                     D-----

In this example, each node of the tree is also marked with an underline starting with a letter for easier identification. In it, there are four nodes:

A, which has 2 child nodes (B, C) and 3 metadata entries (1, 1, 2).
B, which has 0 child nodes and 3 metadata entries (10, 11, 12).
C, which has 1 child node (D) and 1 metadata entry (2).
D, which has 0 child nodes and 1 metadata entry (99).
*/

object Day08 extends App {

  val sampleInputText = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
  val sampleInput = sampleInputText.split(" ").map(_.toInt)
  sampleInput.zipWithIndex.mkString("\n")

  val input = scala.io.Source.fromFile("day08-0-input.txt")
  val nodes: List[Int] = input.mkString.split(" ").map(_.toInt).toList


  println
  println("BadSolution1")
  println
  BadSolution1.solve

  println
  println
  println
  println("BadSolution2")
  println
  BadSolution2.solve


  object BadSolution1 {

    type Collect = (Int, Int, Array[Int]) // (childCount, metaCount, array)

    //noinspection VariablePatternShadow
    val collectMetaNodesBadSolution1: (Collect, Int) => Collect = {
      case ((childCount, 0, array), node) => (node, -1, array)
      case ((childCount, -1, array), node) => (childCount, node, array)
      case ((0, metaCount, array), node) => (0, metaCount - 1, array :+ node)
      case ((childCount, metaCount, array), node) => (childCount - 1, metaCount, array)
    }

    def solve = {
      val (childCount9, metaCount9, array9) =
        sampleInput.take(9).foldLeft((10, 0, Array.empty[Int]))(collectMetaNodesBadSolution1)

      val (childCount10, metaCount10, array10) =
        sampleInput.take(10).foldLeft((10, 0, Array.empty[Int]))(collectMetaNodesBadSolution1)

      val (childCount11, metaCount11, array11) =
        sampleInput.take(11).foldLeft((10, 0, Array.empty[Int]))(collectMetaNodesBadSolution1)

      println("childCount9", "metaCount9", "array9")
      println(childCount9, metaCount9, array9.toList)

      println("childCount10", "metaCount10", "array10")
      println(childCount10, metaCount10, array10.toList)

      println("childCount11", "metaCount11", "array11")
      println(childCount11, metaCount11, array11.toList)
    }
  }


  object BadSolution2 {

    //noinspection VariablePatternShadow
    def collectMetaNodesBadSolution2(tail: List[Int], metaNodes: List[Int]): List[Int] = tail match {

      case 0 :: metaCount :: newTail =>
        collectMetaNodesBadSolution2(newTail.drop(metaCount), metaNodes ::: newTail.take(metaCount))

      case childCount :: metaCount :: newTail =>
        collectMetaNodesBadSolution2(newTail, metaNodes)
    }

    def solve {
      collectMetaNodesBadSolution2(sampleInput.toList, List.empty[Int])
    }
  }

}