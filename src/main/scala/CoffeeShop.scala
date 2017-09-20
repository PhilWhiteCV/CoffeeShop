import scala.annotation.tailrec

/**
  * CoffeeShop object that exposes the
  * standardBill and
  * serviceChargeBill
  * methods
  */
object CoffeeShop {

  /** Use Decimal to ensure rounding */
  case class MenuItem(name: String, cold: Boolean, price: BigDecimal, drink: Boolean)

  private val menu: List[MenuItem] = List(
    MenuItem(name = "Cola", cold = true, price = 0.5, drink = true),
    MenuItem(name = "Coffee", cold = false, price = 1.0, drink = true),
    MenuItem(name = "Cheese Sandwich", cold = true, price = 2.0, drink = false),
    MenuItem(name = "Steak Sandwich", cold = false, price = 4.5, drink = false)
  )

  /** Convert the list of items into a list of MenuItems */
  private def itemsToMenuItems(items: List[String]) =
    items flatMap (item => menu filter (_.name == item))

  /** Maximum Charge if applying the higher service charge */
  private val maxCharge: BigDecimal = 20.0

  @tailrec
  private def menuAcc(subItems: List[MenuItem], acc: BigDecimal, mult: BigDecimal = 1.0): BigDecimal =
    subItems match {
      case List() => acc * mult
      case x :: xs => menuAcc(xs, x.price + acc, mult)
  }

  /**
    * The original standardBill method modified to
    * utilise the MenuItems list
    *
    * @param items List of items ordered from the menu
    * @return sum of items ordered (to 2dp)
    */
  def standardBill(items: List[String]): BigDecimal = {
    menuAcc(itemsToMenuItems(items), 0)
  }

  /**
    * Convenience function to allow 'serviceChargeBill' to be
    * called without the parameters being wrapped in a list
    * (this method actually does the wrapping)
    *
    * @param bindings variable list of menu items
    * @return the sum of items ordered (to 2dp)
    */
  def serviceChargeBill(bindings: String*): BigDecimal = serviceChargeBill(bindings.toList)

  /**
    * Calculates...
    * If only drinks are ordered the sum of all items ordered
    * If food is ordered then the sum of all items + 10%
    * If hot food is ordered then the sum of all items + 20% (to a max of £20)
    *
    * @param items List of items ordered from the menu
    * @return the sum of items as documented above
    */
  def serviceChargeBill(items: List[String]): BigDecimal = {
    val menuList = itemsToMenuItems(items)
    if (menuList.forall(_.drink)) menuAcc(menuList, 0)
    else {
      val containsHotFood = menuList.exists(x => !x.cold && !x.drink)

      if (containsHotFood) maxCharge.min(menuAcc(menuList, 0, 1.2))
      else menuAcc(menuList, 0, 1.1) // if we get here we've got food, but not hot food
    }
  }
}
