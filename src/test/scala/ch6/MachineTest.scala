package ch6

import org.scalatest.FlatSpec

class MachineTest extends FlatSpec {

  "Non-empty machine" should "dispense one piece of candy" in {
    val machine = Machine(5, 0)
    val result = Machine.simulate { List(Coin, Turn) } run machine
    assert { result._1 == (4,1) }
    assert { result._2 == Machine(4, 1) }
  }

  it should "dispenses two pieces of candy" in {
    val machine = Machine(5, 0)
    val result = Machine.simulate { List(Coin, Turn, Coin, Turn) } run machine
    assert { result._1 == (3, 2) }
    assert { result._2 == Machine(3, 2) }
  }

  it should "doesn't dispense candy without coins" in {
    val machine = Machine(5, 0)
    // Only first turn works
    val result = Machine.simulate { List(Coin, Turn, Turn, Turn) } run machine
    assert { result._1 == (4,1) }
    assert { result._2 == Machine(4, 1) }
  }

  it should "not take extra coins" in {
    val machine = Machine(5, 0)
    // Only first coin works
    val result = Machine.simulate { List(Coin, Coin, Coin, Turn) } run machine
    assert { result._1 == (4,1) }
    assert { result._2 == Machine(4, 1) }
  }

  "Empty machine" should "dispense nothing" in {
    val machine = Machine(0, 3)
    val result = Machine.simulate { List(Coin, Turn) } run machine
    assert { result._1 == (0, 3) }
    assert { result._2 == Machine(0, 3) }
  }

}
