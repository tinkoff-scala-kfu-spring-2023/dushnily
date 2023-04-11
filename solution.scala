import scala.collection.immutable.List

// https://www.geeksforgeeks.org/sliding-window-maximum-maximum-of-all-subarrays-of-size-k/

case class Node(data: Int, maximum: Int)
object Solution extends App {
  def insert(s2: List[Node], value: Int): List[Node] = {
    val other = Node(value, if (s2.isEmpty) value else math.max(value, s2.head.maximum))
    other :: s2
  }

  def delete(s1: List[Node], s2: List[Node]): (List[Node], List[Node]) = {
    if (s1.nonEmpty) (s1.tail, s2)
    else {
      val (s1New, s2New) = s2.foldLeft((List.empty[Node], s2)) {
        case ((s1Acc, s2Acc), node) => (insert(s1Acc, node.data), s2Acc.tail)
      }
      (s1New.tail, s2New)
    }
  }

  def getMax(s1: List[Node], s2: List[Node]): Int = {
    val s1Max = s1.headOption.map(_.maximum).getOrElse(-1)
    val s2Max = s2.headOption.map(_.maximum).getOrElse(-1)
    math.max(s1Max, s2Max)
  }

  def slidingMaximum(arr: Array[Int], k: Int): List[Int] = {
    val (s1, s2) = arr.slice(0, k - 1).foldLeft((List.empty[Node], List.empty[Node])) {
      case ((s1Acc, s2Acc), value) => (s1Acc, insert(s2Acc, value))
    }

    (0 to arr.length - k).foldLeft((s1, s2, List.empty[Int])) {
      case ((s1Acc, s2Acc, result), i) =>
        val (s1New, s2New) = if (i > 0) delete(s1Acc, s2Acc) else (s1Acc, s2Acc)
        val s2Updated = insert(s2New, arr(i + k - 1))
        val max = getMax(s1New, s2Updated)
        (s1New, s2Updated, result :+ max)
    }._3
  }

  val arr = Array(8, 5, 10, 7, 9, 4, 15, 12, 90, 13)
  val k = 4

  val ans = slidingMaximum(arr, k)
  println(ans.mkString(" "))
}
