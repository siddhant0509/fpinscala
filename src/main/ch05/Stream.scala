package main.ch05

import scala.annotation.tailrec

/**
  * Created by siddhants on 8/20/17.
  */
sealed trait Stream[+A]{
  import Stream._

  def toList(): List[A] = this match {
    case Cons(h, t) => h()::t().toList()
    case Empty => List()
  }


  def toListR(): List[A] = {
    @tailrec
    def go(s: Stream[A], l: List[A]): List[A] = {
      s match {
        case Cons(h, tail) => go(tail(), h()::l)
        case _ => l
      }
    }

    go(this, List()).reverse
  }


  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 0 => cons(h(), empty)
      case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, tail) if n > 1 => tail().drop(n-1)
    case Cons(_, tail) if n == 1 => tail()
    case _ => Empty
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, tail) =>
      if(f(h()))
        cons(h(), tail().takeWhile(f))
      else
        this
    case _ => this
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream{
  def cons[A](hd: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if(as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  def head[A](s: Stream[A]): Option[A] = s match {
    case Empty => None
    case Cons(x, _) => Some(x())
  }


}