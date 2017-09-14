package main

/**
  * Created by siddhants on 9/8/17.
  */
object TestSome {

  def main(args: Array[String]): Unit = {
    val a = new Animal {}
    val m = new Mammal {}
    val d = new Dog {}

    println(getN(a))
    println(getN(m))
    println(getN(d))


    val f: Animal => Dog = a => new Dog {}
    hf(f)
  }

  def getN(a: Animal) = a.name;
  def getN(a: Mammal) = a.name;

  def hf(f: Dog => Dog) = "Ok"

}

trait Animal{
  val name = "Animal"
}

trait Mammal extends Animal{
  override val name = "Mammal"
}

trait Dog extends Animal{
  override val name = "Dog"
}
