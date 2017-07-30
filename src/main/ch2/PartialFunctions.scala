package main.ch2

/**
  * Created by siddhants on 7/29/17.
  */
object PartialFunctions {

  def partial1[A,B,C](a: A, f: (A,B) => C) : B => C = (b: B) => f(a,b)


  //ex 2.3
  def curry[A,B,C](f: (A,B) => C) : A => B => C = (a: A) => (b: B) => f(a,b)

  // ex 2.4
  def uncurry[A,B,C](f: A => B => C): (A,B) => C = (a,b) => f(a)(b)

  //ex 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

}
