import scala.annotation.tailrec


object CoffeeShop {

  /*
   * Use Decimal to ensure rounding
   */
  case class MenuItem(name: String, cold: Boolean, price: BigDecimal, drink: Boolean)

  val menu: List[MenuItem] = List(
    MenuItem(name = "Cola", cold = true, price = 0.5, drink = true),
    MenuItem(name = "Coffee", cold = false, price = 1.0, drink = true),
    MenuItem(name = "Cheese Sandwich", cold = true, price = 2.0, drink = false),
    MenuItem(name = "Steak Sandwich", cold = false, price = 4.5, drink = false)
  )

  private def itemsToMenuItems(items: List[String]) =
    items.flatMap(item => menu.filter(_.name == item))

  private val maxCharge: BigDecimal = 20.0

  @tailrec
  private def menuAcc(subItems: List[MenuItem], acc: BigDecimal): BigDecimal = subItems match {
    case List() => acc
    case x :: xs => menuAcc(xs, x.price + acc)
  }

  def standardBill(items: List[String]): BigDecimal = {
    menuAcc(itemsToMenuItems(items), 0)
  }

  def serviceChargeBill(bindings: String*): BigDecimal = serviceChargeBill(bindings.toList)

  def serviceChargeBill(items: List[String]): BigDecimal = {
    val menuList = itemsToMenuItems(items)
    if (menuList.forall(item => item.drink)) menuAcc(menuList, 0)
    else {
      val containsHotFood = menuList.exists(x => !x.cold && !x.drink)

      if (containsHotFood) maxCharge.min(menuAcc(menuList, 0) * 1.2)
      else menuAcc(menuList, 0) * 1.1 // if we get here we've got food, but not hot food
    }
  }
}
