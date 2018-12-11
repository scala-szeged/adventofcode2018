

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

object Day08 {

  def main(args: Array[String]): Unit = {
    val parent = MyParser.parse(inputText)
    println("task1", sumThem(0, parent))
  }

  val sampleInputText = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

  val inputText: String = scala.io.Source.fromFile("day08-0-input.txt").mkString


  val sumThem: (Int, ChildNode) => Int = {
    case (sum, LeafNode(_, _, data)) => sum + data.sum
    case (sum, ParentNode(_, _, children, data)) => children.foldLeft(sum + data.sum)(sumThem) // Error:(27, 53) could not optimize @tailrec annotated method sumThem: it contains a recursive call not in tail position
  }


  sealed trait ChildNode

  case class ParentNode(childCount: Int, metaCount: Int, children: List[ChildNode], data: List[Int]) extends ChildNode

  /**
    * childCount is always 0, but makes it easier to read the log.
    */
  case class LeafNode(childCount: Int, metaCount: Int, data: List[Int]) extends ChildNode


  import scala.util.parsing.combinator.JavaTokenParsers

  //noinspection TypeAnnotation
  object MyParser extends JavaTokenParsers {

    def parent: Parser[ChildNode] = wholeNumber ~ wholeNumber into {
      case childCount ~ metaCount => repN(childCount.toInt, child) ~ repN(metaCount.toInt, wholeNumber) ^^ {
        case children ~ data => ParentNode(childCount.toInt, metaCount.toInt, children, data.map(_.toInt))
      }
    }

    def child = log(leaf)("leaf") | no_log(parent)("parent")

    def leaf = "0" ~> wholeNumber into {
      metaCount =>
        repN(metaCount.toInt, wholeNumber) ^^ {
          data => LeafNode(0, metaCount.toInt, data.map(_.toInt))
        }
    }

    def no_log[T](p: Parser[T])(notUsed: String) = p

    def parse(s: String) = parseAll(MyParser.child, s) match {
      case Success(startDslData, _) =>
        startDslData

      case NoSuccess(msg, next) =>
        println(msg)
        println(next)
        null
    }
  }

}