import scala.annotation.tailrec


object CoffeeShop {

  private val menu = Map("Cola" -> 0.5, "Coffee" -> 1.0, "Cheese Sandwich" -> 2.0, "Steak Sandwich" -> 4.5) withDefaultValue 0.0

  def standardBill(items: List[String]): Double = {

    @tailrec
    def menuAcc(subItems: List[String], acc: Double): Double = subItems match {
      case List() => acc
      case x :: xs => menuAcc(xs, menu(x) + acc)
    }

    menuAcc(items, 0)
  }

}
