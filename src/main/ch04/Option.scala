package main.ch04

/**
  * Created by siddhants on 7/30/17.
  */


trait Option[+A]{
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def flatmap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(v) => f(v)
  }

  def flatmap_1[B](f: A =>  Option[B]): Option[B] =
    this.map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(_) => this
  }

  def orElse_1[B >: A](ob: => Option[B]): Option[B] = this.map(x => Some(x)) getOrElse ob

//  def filter(p: A => Boolean): Option[A] = {
//    case Some(v:A) => if(p(v)) this
//    case _ => None
//  }

  def filter_1(p: A => Boolean): Option[A] =
    this.flatmap((x: A) => if(p(x)) Some(x) else None)


}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = a.flatmap(v => b.map(bV => f(v, bV)))

  def Try[A](a: => A): Option[A] = {
    try{
      Some(a)
    }catch {
      case e: Exception => None
    }
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
      def go(a: List[Option[A]], o: Some[List[A]]): Option[List[A]] = {
        a match {
          case Some(x)::l => o match {
            case Some(li) => go(l, Some(x::li))
          }
          case None::l => None
        }
      }
    go(a, Some(Nil))
  }


  def mean(xs: Seq[Double]): Option[Double] =
    if(xs.isEmpty) None
    else Some(xs.sum/xs.size)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).map(v => xs.map(x1 => math.pow(x1 - v, 2))).flatmap(seq => mean(seq))
}
