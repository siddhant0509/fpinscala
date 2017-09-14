package main.ch05

/**
  * Created by siddhants on 8/20/17.
  */
object Laziness {

  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A) = if(cond) onTrue else onFalse



}
