package ad

object Test extends App {
  val f = DifferentiableFunction(x => norm(x(0 to 1)))

  println(f(3, 4, 5, 6))
  println(f(2, 3, 4, 5))
  println(f(1, 2, 3, 4))
}

