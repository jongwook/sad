package ad

case class DifferentiableFunction(f: Variables => Variable) {

  def apply(x: Double*): (Double, Seq[Double]) = {
    val v = new Variables(x.map(Variable.apply))
    val y = f(v)
    (y.value, y.diff(v: _*))
  }

}
