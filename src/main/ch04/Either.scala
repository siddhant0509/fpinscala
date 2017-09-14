package main.ch04

/**
  * Created by siddhants on 8/20/17.
  */
sealed trait Either [+E, +A]{
  def map[B](f: A => B): Either[E,B] = this match {
    case Left(e) => Left(e)
    case Right(v) => Right(f(v))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(v) => f(v)
  }

  def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] = this match {
    case Left(_) => b
    case _ => this
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C) : Either[EE, C] = {
   this.flatMap((v: A) => b.map((v2: B) => f(v, v2)))
  }

  def map2C[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C) : Either[EE, C] = {
    for{
      v1 <- this
      v2 <- b
    } yield f(v1, v2)
  }





}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]


object Either{
  def Try[E,A](a: => A)= {
    try {
      Right(a)
    }catch {
      case e: Exception => Left(e)
    }
  }

  def lift[A,B, EE](f: A => B): Either[EE, A] => Either[EE, B] = eA => eA.map(f)

  def lift2[A,B, C,EE](f: (A,B) => C): (Either[EE, A], Either[EE, B]) => Either[EE, C] = {
    (eA, eB) => for{
      v1 <- eA
      v2 <- eB
    } yield f(v1, v2)
  }


}
