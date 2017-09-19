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

  test("Service Charge Bill Empty List") {
    assert(serviceChargeBill(List()) == 0.0)
  }

  test("Service Charge Bill All Drinks") {
    assert(serviceChargeBill("Cola", "Coffee", "Cola") == 2.0)
  }

  test("Service Charge Bill Some Food") {
    assert(serviceChargeBill("Cola", "Coffee", "Cheese Sandwich") == 3.85)
  }

  test("Service Charge Bill No Drinks") {
    assert(serviceChargeBill("Cheese Sandwich") == 2.2)
  }

  test("Service Charge Bill Hot Food") {
    assert(serviceChargeBill("Cola", "Coffee", "Cheese Sandwich", "Steak Sandwich") == 9.60)
  }

  test("Service Charge Bill Hot Food Cannot Exceed £20") {
    assert(serviceChargeBill("Cola", "Coffee", "Cheese Sandwich", "Steak Sandwich", "Steak Sandwich", "Steak Sandwich") == 20.0)
  }

  test("Service Charge Bill Duplicates") {
    assert(serviceChargeBill("Cola", "Coffee", "Cheese Sandwich", "Steak Sandwich", "Cola") == 10.2)
  }

  test("Service Charge Bill Unknown Items") {
    assert(serviceChargeBill("Cola", "Coffee", "Cheese Sandwich", "Banana") == 3.85)
  }
}
