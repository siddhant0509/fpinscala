package main.ch2

import scala.annotation.tailrec

/**
  * Created by siddhants on 7/29/17.
  */
object ArrayUtils {

  def findFirstString(arr: Array[String], key: String): Int = {

    @tailrec
    def go(n: Int): Int = {
      if(n >= arr.length)  -1
      else if(arr(n) == key) n
      else go(n + 1)
    }
    go(0)
  }


  def findFirst[A](arr: Array[A], p: A => Boolean): Int = {
    @tailrec
    def go(n: Int): Int = {
      if(n >= arr.length)  -1
      else if(p(arr(n))) n
      else go(n + 1)
    }
    go(0)
  }

  //ex 2.2
  // [0,1,2,3]
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean) = {
    @tailrec
    def go(f: Int, s: Int): Boolean = {
      if(f >= as.length || s >= as.length) true
      else if (!ordered(as(f), as(s))) false
      else go(s, s + 1)
    }

    go(0,1)
  }


  def main(arr: Array[String]) : Unit = {
    print(isSorted[Int](Array(0,4,5,2), (a, b) => a < b))
  }

}
