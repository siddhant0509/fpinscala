package main.ch03

/**
  * Created by siddhants on 7/30/17.
  */
trait Tree[+A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

case class Leaf[A](value: A) extends Tree[A]

object Tree {
  //ex 3.25

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }


  //ex 3.26
  def max[A](tree: Tree[A])(com: (A,A) => Int): A = tree match {
    case Leaf(v) => v
    case Branch(l, r) =>
      val maxL = max(l)(com)
      val maxR = max(r)(com)
      if(com(maxL, maxR) >= 0)
        maxL
      else
        maxR
  }

  //ex 3.29
  def fold[A, B](tree: Tree[A])(f: (A) => B)(g: (B,B) => B): B = tree match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
  //ex 3.29
  def sizeViaFold[A](tree: Tree[A]) = fold(tree)(_ => 1)(_ + _)

  //ex 3.29
  def maximumViaFold[A](tree: Tree[A])(com : (A,A) => Int) : A =
    fold(tree)(x => x)((a,b) => {
      if(com(a,b) > 0)
        a
      else
        b
    })

  //ex 3.29
  def depthViaFold[A](tree: Tree[A]): Int = fold(tree)(_ => 1)((x, y) => if(x > y) x + 1 else y + 1)

  //ex 3.29
  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(x => Leaf(f(x)): Tree[B])((l,r) => Branch(l, r))

  //ex 3.28
  def map[A,B](tree: Tree[A])(f:A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  //ex 3.27
  def depth[A](tree: Tree[A]): Int = tree match {
    case Branch(l, r) => 1 + depth(l) max depth(r)
    case Leaf(value) => 1
  }


}


object Main{

  import main.ch03.Tree._

  def main(args: Array[String]): Unit = {
    val tree = Branch(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)), Leaf(4))
    println(depth(tree))
    println(depthViaFold(tree))
    println(map(tree)(x => x * 10))
    println(maximumViaFold(tree)((x,y) => if(x >= y) 1 else -1))
  }
}

