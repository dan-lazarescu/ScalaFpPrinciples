package recfun

class RecFunSuite extends munit.FunSuite:
  import RecFun.*


  // ------ balance tests -----------------------------------------------------

  test("balance: '(if (zero? x) max (/ 1 x))' is balanced") {
    assert(balance("(if (zero? x) max (/ 1 x))".toList))
  }

  test("balance: 'I told him ...' is balanced") {
    assert(balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList))
  }

  test("balance: ':-)' is unbalanced") {
    assert(!balance(":-)".toList))
  }

  test("balance: counting is not enough 0") {
  }

  test("balance: counting is not enough 1") {
    assert(!balance("())(".toList))
  }

  test("balance: counting is not enough 2") {
    assert(!balance(")".toList))
  }
  // ------ countChange tests -------------------------------------------------

  test("countChange: example given in instructions") {
    assertEquals(countChange(4,List(1,2)), 3)
  }

  test("countChange: 0 money") {
    assertEquals(countChange(0,List(1,2)), 0)
  }

  test("countChange: no coins") {
    assertEquals(countChange(4,List()), 0)
  }

  test("countChange: negative test - amt negative") {
    assertEquals(countChange(-4,List(1,2)), 0)
  }

  test("countChange: no available change") {
    assertEquals(countChange(4,List(3,5,7)), 0)
  }

  test("countChange: sorted CHF") {
    assertEquals(countChange(300,List(5,10,20,50,100,200,500)), 1022)
  }

  test("countChange: no pennies") {
    assertEquals(countChange(301,List(5,10,20,50,100,200,500)), 0)
  }

  test("countChange: unsorted CHF") {
    assertEquals(countChange(300,List(500,5,50,100,20,200,10)), 1022)
  }

  // ------ pascal tests ------------------------------------------------------

  test("pascal: col=0,row=2") {
    assertEquals(pascal(0, 2), 1)
  }

  test("pascal: col=1,row=2") {
    assertEquals(pascal(1, 2), 2)
  }

  test("pascal: col=1,row=3") {
    assertEquals(pascal(1, 3), 3)
  }

  test("pascal: col=2,row=6") {
    assertEquals(pascal(2, 6), 15)
  }

  test("pascal: col=-1,row=6") {
    assertEquals(pascal(-1, 6), 0)
  }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
