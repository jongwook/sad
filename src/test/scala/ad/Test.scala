package ad

object Test extends App {
  val f = DifferentiableFunction(x => x(0) * x(1) + sin(x(0)))

  println(f(3, 4))
  println(f(2, 3))
  println(f(1, 3))
}

