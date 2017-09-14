package main.monoid

/**
  * Created by siddhants on 8/27/17.
  */

// Associative (a+b)+c = a+(b+c)
// Identity (a + zero) = (zero + a) = a
trait Monoid[A] {

  def op(a: A, b: A): A

  def zero: A

}

object Monoid{

  val stringMoniod = new Monoid[String] {
    override def op(a: String, b: String): String = a.concat(b)

    override def zero: String = ""
  }

  // Ex 10.1
  val integerMonoid = new Monoid[Int] {
    override def op(a: Int, b: Int): Int = a + b
    override def zero: Int = 0
  }

  val integerMulMonoid = new Monoid[Int] {
    override def op(a: Int, b: Int): Int = a * b
    override def zero: Int = 1
  }

  val booleanOr = new Monoid[Boolean] {
    override def op(a: Boolean, b: Boolean): Boolean = a || b
    override def zero: Boolean = false
  }

  val booleanAnd = new Monoid[Boolean]{
    override def op(a: Boolean, b: Boolean) : Boolean = a && b
    override def zero = true
  }


  def optionMonoid[A] = new Monoid[Option[A]] {
    override def op(a: Option[A], b: Option[A]): Option[A] = a orElse b
    override def zero: Option[A] = None
  }

  def endoMonoid[A] = new Monoid[A => A] {
    override def op(a: (A) => A, b: (A) => A): (A) => A = a andThen b
    override def zero: (A) => A = i => i
  }

  def listMonoid[A] = new Monoid[List[A]] {
    override def op(a: List[A], b: List[A]): List[A] = a ++ b
    override def zero: List[A] = Nil
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def op(a: A, b: A): A = m.op(b, a)
    override def zero: A = m.zero
  }


  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = as.map(f).foldRight(m.zero)(m.op)

  //todo 10.6

  //todo 10.8
  //todo 10.9

  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if(v.isEmpty) m.zero
    else if(v.length == 1)f(v(0))
      val (_1, _2) = v.splitAt(v.length / 2)
      m.op(foldMapV(_1, m)(f), foldMapV(_2, m)(f))
  }

  // (a + b) + c = a + (b + c)
  val wcMonoid = new Monoid[WC] {
    override def op(a: WC, b: WC): WC = (a, b) match {
      case (Stub(a), Stub(b)) => Stub( a + b )
      case (Stub(a), Part(aL, c, aR)) => Part(a + aL, c, aR)
      case (Part(aL, c, bR), Stub(b)) => Part(aL, c, bR + b)
      case (Part(al, c, ar), Part(bl, c1, br)) =>
        if(ar.isEmpty && bl.isEmpty)
           Part(al, c + c1, br)
        else
          Part(al, c + c1 + 1, br)
    }

    override def zero: WC = Stub("")


  }


  object fizbuzz{

   def stringSemiGroupMonoid = new Monoid[Option[String]] {
     override def op(a: Option[String], b: Option[String]): Option[String] = (a,b) match {
       case (None, _) => b
       case (_, None) => a
       case (Some(m1), Some(m2)) =>  Some(m1 + m2)
     }

     override def zero: Option[String] = None
   }


    def buildFizBuzz() = {
      val li = List(1,2,3,4) //
      li.map(v =>
        List(divBy(v, 3)("fizz"), divBy(v, 5)("buzz"))
          .reduceOption(stringSemiGroupMonoid.op).getOrElse(v.toString)
      )

    }

    def divBy(dividend: Int, divisor: Int)(v: String) = dividend % divisor match {
      case 0 => Some(v)
      case _ => None
    }


  }

  // todo 10.11

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      override def op(a: (A, B), b: (A, B)): (A, B) = (A.op(a._1, b._1), B.op(a._2, b._2))

      override def zero: (A, B) = (A.zero, B.zero)
    }


}

sealed trait WC

case class Stub(chars: String) extends WC

case class Part(lStub: String, words: Int, rStub: String) extends WC
