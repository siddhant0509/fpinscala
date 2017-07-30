package main.ch03

import java.util.concurrent.CompletableFuture

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * Created by siddhants on 7/29/17.
  */
object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(a, tail) => a + sum(tail)
  }

  def product(ds: List[Double]) : Double = ds match {
    case Nil => 1.0
    case Cons(0.00, _) => 0.00
    case Cons(v, tail)  => v * product(tail)
  }

  def apply[A](as: A*): List[A] =
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  //ex 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }


  //ex 3,3
  def setHead[A](l: List[A], head: A): List[A] = l match {
    case Nil => Cons(head, Nil)
    case Cons(_, xs) => Cons(head, xs)
  }


  //ex 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    @tailrec
    def go(n: Int, li: List[A]): List[A] = {
      if(n == 0) li
      else li match {
        case Nil => Nil
        case Cons(_, xs) => go(n - 1, xs)
      }
    }

    go(n, l)
  }

//ex 3.5
  @tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) =>
        if(f(x))
          dropWhile(xs)(f)
        else
          l
    }
  }

  def foldRight[A, B](li: List[A], b: B)(f: (A,B) => B) : B = li match {
    case Nil => b
    case Cons(x, xs) => f(x, foldRight(xs, b)(f))
  }

  //ex 3.10
  def foldLeft[A,B](li: List[A], b: B)(f: (A,B) => B):B = li match {
    case Nil => b
    case Cons(x, xs) => foldLeft(xs, f(x, b))(f)
  }

// ex 3.6
  def init[A](li: List[A]): List[A] = li match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }


  def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(x, xs) => Cons(x, append(xs, l2))
  }

  //ex 3.14
  def appendUsingFoldRight[A](l1: List[A], l2: List[A]): List[A] = foldRight(l1, l2)((x, xs) => Cons(x, xs))


  // ex 3.16
  def add1(li: List[Int]) : List[Int] = li match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, add1(xs))
  }

  def add1UsingFoldL(li: List[Int]): List[Int] = foldRight(li, Nil: List[Int])((x, xs) => Cons(x + 1, xs))

  // ex 3.17
  def toStringD(li: List[Double]): List[String] = li match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, toStringD(xs))
  }

  def toStringDUsingFoldR(li: List[Double]): List[String]  = foldRight(li, Nil: List[String])((x, xs) => Cons(x.toString, xs))


  //ex 3.18
  def map[A,B](li: List[A])(f: A => B): List[B] = li match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  def mapUsingFoldR[A,B](li: List[A])(f: A => B) : List[B] = foldRight(li, Nil: List[B])((x, xs) => Cons(f(x), xs))


  def mapRec[A,B](li: List[A])(f: A => B): List[B] = {

    @tailrec
    def go(li: List[A], r: List[B]): List[B] = li match {
      case Nil => r
      case Cons(x, xs) => go(xs, Cons(f(x), r))
    }
    go(li, Nil)
  }

  def map_2[A,B](li: List[A])(f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]

    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(x, xs) => buf += f(x); go(xs)
    }

    go(li)

    List(buf.toList: _*)
  }


  def filter[A](li: List[A])(f: A => Boolean): List[A] = foldRight(li, Nil: List[A])((x,xs) => f(x) match{
    case true => Cons(x, xs)
    case false => xs
  })


  def filter_2[A](li: List[A])(f: A => Boolean): List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(x, xs) => if(f(x)) buf += x; go(xs)
    }
    go(li)
    List(buf.toList: _*)
  }


  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A,B) => C): List[C] = (l1, l2) match{
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a, as), Cons(b, bs)) => Cons(f(a,b), zipWith(as, bs)(f))
  }

  def flatmap[A, B](li: List[A])(f: A => List[B]): List[B] = foldRight(li, Nil: List[B])((x, xs) => append(f(x), xs))


  def concat[A](li: List[List[A]]): List[A] = foldRight(li, Nil: List[A])(append)


  def filterUsingFMap[A](li: List[A])(f: A => Boolean): List[A] = flatmap(li)(x => if(f(x)) List(x) else Nil)

//  def foldLInRight[A,B](li: List[A], b: B)(f: (A,B) => B) =
//    foldRight(li, b)((a,b) => )

  def main(args: Array[String]): Unit = {
    List(1,2,3)
    println(drop(List(1,2,3,4), 8))
    println(dropWhile[Int](List(2,4,3,4))(x => x % 2 == 0))
    println(append(List(1,2,3), List(4,5,6)))
    println(init(List(1,2,3,4)))
    println(foldRight(List(1,2,3,4), 0)(_ + _))
    println(foldLeft(List(1,2,3,4), 0)(_ + _))
    println(foldLeft(List(1,2,3), Nil: List[Int])((x,xs) => Cons(x + 1, xs)))

    //ex 3.9
    println(foldRight(List(1,2,3,4), 0)((_, v) => v + 1))

    //ex 3.11
    println(foldLeft(List(1,2,3,4), 0)(_ + _))
    println(foldLeft(List(1,2,3,4), 1.0)(_ * _))

    println(append(List(1,2,3), Nil))

    // ex 3.12
    println(foldRight(List(1,2,3,4), Nil: List[Int])((x: Int, xs: List[Int]) => append(xs, Cons(x, Nil))))

    println(appendUsingFoldRight(List(1,2,3), List(4,5,6)))

    println(add1(List(1,2,3)))
    println(toStringD(List(1.0,2.0,3.1)))
    println(map(List(1,2,3))(x => x + 1))
    println(mapRec(List(1,2,3))(x => x + 1))

    println(filter(List(1,2,3,4))(x => x%2 == 0))
    println(filter_2(List(1,2,3,4))(x => x%2 == 0))

    println(filterUsingFMap(List(1,2,3,4))(x => x%2 == 0))


    println(concat(List(List(1,2), List(3,4), List(5,6))))


  }

}



sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]