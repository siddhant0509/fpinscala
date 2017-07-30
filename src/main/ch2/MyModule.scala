package main.ch2

import scala.annotation.tailrec

/**
  * Created by siddhants on 7/29/17.
  */
object MyModule {

  def abs(n: Int) : Int =
    if(n > 0) n
    else -n

  def formatAbs(n: Int): String = "Abs value of %d is %d".format(n, abs(n))



  def format(name:String, n: Int, f: Int => Int): String =
    "The %s of %d is %d".format(name, n , f(n))

  def main(args: Array[String]): Unit = {
    println(formatAbs(10))
    println(format("Factorial",1,factorial))
    println(format("Factorial",2,factorial))
    println(format("Factorial",5,factorial))

    println(format("Fibonacci",1,fib))
    println(format("Fibonacci",2,fib))
    println(format("Fibonacci",3,fib))
    println(format("Fibonacci",4,fib))
    println(format("Fibonacci",7,fib))

  }


  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int =
      if(n == 0) acc
      else go(n-1, acc * n)
    go(n, 1)
  }


  def fib(n: Int): Int = {
    def go(a: Int, b: Int, n: Int): Int = {
      if(n == 1) return a
      else go(b, a + b, n - 1)
    }
    go(0,1,n)
  }

}
