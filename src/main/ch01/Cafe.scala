package main.ch01

/**
  * Created by siddhants on 7/29/17.
  */
object CafeWithSideEffects {

}


class Cafe{

  // Impure Function, doing side effect
  def buyCoffee(cc: CreditCard): Coffee = {
    val cup = Coffee()
    cc.charge(cup.price)
    cup
  }

  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
    val purchases = List.fill(n)(buyCoffeePure(cc))
    val (coffees, charges) = purchases.unzip
    (coffees, charges.reduce(_.combine(_)))
  }


  def buyCoffee(cc: CreditCard, payment: Payment): Coffee = {
    val cup = Coffee()
    payment.charge(cup.price())
    cup
  }


  def buyCoffeePure(cc: CreditCard) : (Coffee, Charge) = {
    val cup = Coffee()
    val charge = Charge(cup.price(), cc)
    (cup, charge)
  }
}



case class Charge(price: Double, cc: CreditCard){
    def combine(other: Charge) = {
      if(other.cc.equals(this.cc))
        Charge(other.price + this.price, this.cc)
      else
        throw new Exception("Diff Credit Cards")
    }
}


object Charge{
  def coalesce(charges: List[Charge]): List[Charge] =
    charges.groupBy(_.cc).values.map(charges => charges.reduce(_.combine(_))).toList
}

case class CreditCard(){

  // Not a good separation of concern
  def charge(price: Double): Unit = {}
}

case class Coffee(){
  def price(): Double = 1.2
}

case class Payment(){
  def charge(price: Double): Unit = {}
}
