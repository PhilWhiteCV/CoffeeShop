import org.scalatest.FunSuite
import CoffeeShop._

class CoffeeShopSuite extends FunSuite {

  test("Standard Bill Empty List") {
    assert(standardBill(List()) == 0.0)
  }

  test("Standard Bill One item") {
    assert(standardBill(List("Cola")) == 0.5)
  }

  test("Standard Bill List as given") {
    assert(standardBill(List("Cola", "Coffee", "Cheese Sandwich")) == 3.5)
  }

  test("Standard Bill All items") {
    assert(standardBill(List("Cola", "Coffee", "Cheese Sandwich", "Steak Sandwich")) == 8)
  }

  test("Standard Bill Duplicates") {
    assert(standardBill(List("Cola", "Coffee", "Cheese Sandwich", "Steak Sandwich", "Cola")) == 8.5)
  }

  test("Standard Bill Unknown Items") {
    assert(standardBill(List("Cola", "Coffee", "Cheese Sandwich", "Banana")) == 3.5)
  }
}
