package ch6

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(candies: Int, coins: Int, locked:Boolean = true) {

  def insertCoin(): Machine = {
    if (locked && candies > 0) Machine(candies, coins + 1, locked = false)
    else this
  }

  def turnKnob(): Machine = {
    if (!locked && candies > 0) Machine(candies - 1, coins)
    else this
  }

}

object Machine {

  type Counts = (Int, Int)

  private def state(m:Machine):(Counts, Machine) = ((m.candies, m.coins), m)

  def insertCoin():State[Machine, Counts] = State { m => state(m.insertCoin()) }

  def turnKnob():State[Machine, Counts] = State { m => state(m.turnKnob()) }

  def perform(inputs: List[Input]): State[Machine, List[(Int, Int)]] =
    State.sequence(
      inputs.map {
        case Coin => Machine.insertCoin()
        case Turn => Machine.turnKnob()
      }
    )

  def simulate(inputs: List[Input]): State[Machine, (Int, Int)] =
    State(
      machine => {
        val state = perform(inputs) run machine
        (state._1.last, state._2)
      }
    )
}
