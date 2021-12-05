import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

object Main {
    sealed trait BT[A]
  case class Empty[A]() extends BT[A]
  case class Node[A](elem: A, left: BT[A], right: BT[A]) extends BT[A]


    def toHex(number: Int): List[Int]=
      @tailrec
      def toHexTailrec(number: Int, list: List[Int]): List[Int]=
        if number != 0 then
          toHexTailrec(number/16, List(number%16) ::: list)
        else list
      toHexTailrec(number, Nil)

  //zdecydowałem się na dwa argumenty zamiast listy dla przejrzystości użycia tej funkcji
    def changeBase(number: Int, base: Int): List[Int]=
      @tailrec
      def changeBaseTailrec(number: Int, list: List[Int]): List[Int]=
        if number != 0 then
          changeBaseTailrec(number/base, List(number%base) ::: list)
        else list
      changeBaseTailrec(number, Nil)

    def createNLevelTree(n: Int):Node[Double] =
      if n==1 then Node(Random.nextDouble(),Empty[Double](),Empty[Double]())
      else Node(Random.nextDouble(),createNLevelTree(n-1),createNLevelTree(n-1))

    def treeProduct(tree: BT[Double]): Double =
      tree match
        case Empty() => 1
        case Node(elem, left, right) => elem*treeProduct(left)*treeProduct(right)

    def contains[A](list: List[A], elem: A): Boolean =
      list match
        case Nil => false
        case h::t => if h == elem then true else contains(t, elem)

    def depthFirstSearch[A](node: BT[A], list: List[A], parent: BT[A]): Unit= //todo rozbicie na przejscie oraz sprawdzenie powtorzen
      node match
        case Empty() =>
        case Node(elem, left, right) =>
          depthFirstSearch(left, list, node)
          if contains(list, elem) then println() //node jest niepotrzebny i parentowi przypisujemy jakieś dziecko, childmovem
          else
            depthFirstSearch(right, elem :: list, node)
/*
    def childMove[A](current: Node[A], parent: Node[A]): Unit=
      current match
        case Node(_, left: Node[A], _) =>if parent.left==current then parent.left=left else parent.right=left; childMove(left, current)
        case Node(_, Empty(), right: Node[A])=> println("pass"); childMove(right, current)
        case Node(_, Empty(), Empty()) => //done*/

    def printTree[A](node: Node[A]): Unit=
      node match
        case Node(elem, left: Node[A], right: Node[A])=> println(elem); printTree(left); printTree(right);
        case Node(elem, left: Node[A], Empty())=> println(elem); printTree(left);
        case Node(elem, Empty(), right: Node[A])=> println(elem); printTree(right);
        case Node(elem, _, _) => println(elem)

    def nodeEnqueue[A](list: List[BT[A]], elem: BT[A]): List[BT[A]] =
      list match
        case h::t => h :: nodeEnqueue(t, elem)
        case Nil => List(elem)

    def nodeDequeue[A](list: List[BT[A]]): BT[A] =
      list match
        case h::t => h
        case Nil => throw new Exception("Empty queue exception")

    def breadthFirstSearch[A](tree: BT[A], queue: List[BT[A]]): List[A] =
      tree match
        case Empty() => Nil
        case Node(elem, left, right) =>

    def main(args: Array[String]): Unit =
      println(toHex(31))
      println(changeBase(128, 2))
      //val tree = createNLevelTree(2)
      val tree = Node(9, Node(4, Node(2, Empty(), Empty()), Node(1, Empty(), Empty())), Node(8, Node(6, Empty(), Empty()), Node(5, Empty(), Empty())))
      printTree(tree)
      //println(treeProduct(tree))
  }