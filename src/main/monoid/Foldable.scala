package main.monoid

import main.ch03.{Branch, Leaf, Tree}

/**
  * Created by siddhants on 8/27/17.
  */
object Foldable {


  def optionFoldable = new Foldable[Option] {

    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
      case None => z
      case Some(x) => f(z, x)
    }

    override def foldMap[A, B](as: Option[A])(m: Monoid[B])(f: (A) => B): B = as match {
      case None => m.zero
      case Some(x) => f(x)
    }

    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
      case None => z
      case Some(x) => f(x, z)
    }
  }



  def listFoldable = new Foldable[List] {

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    override def foldMap[A, B](as: List[A])(m: Monoid[B])(f: (A) => B): B = as.foldLeft(m.zero)((b,a) => m.op(b, f(a)))

    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
  }



  val treeFoldable = new Foldable[Tree] {

    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
      case Leaf(v) => f(z, v)
      case Branch(l, r) =>  foldLeft(l)(foldLeft(r)(z)(f))(f)
    }

    override def foldMap[A, B](as: Tree[A])(m: Monoid[B])(f: (A) => B): B = as match {
      case Leaf(v) => f(v)
      case Branch(l, r) => m.op(foldMap(l)(m)(f), foldMap(r)(m)(f))
    }

    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
      case Leaf(v) => f(v, z)
      case Branch(l, r) => foldRight(r)(foldRight(l)(z)(f))(f)
    }
  }


  val optionFunctor = new Functor[Option] {
    override def map[A, B](v: Option[A])(f: (A) => B): Option[B] = v.map(f)
  }
}

trait Foldable[F[_]]{

  def foldLeft[A,B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldRight[A,B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldMap[A,B](as: F[A])(m: Monoid[B])(f: A => B): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)

  def toList[A](fa: F[A]): List[A] = foldLeft(fa)(List[A]())((l: List[A], a: A) => a::l)

}


trait Functor[F[_]]{
  def map[A,B](v: F[A])(f: A => B): F[B]
}

