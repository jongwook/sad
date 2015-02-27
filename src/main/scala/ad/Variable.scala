package ad

abstract class Variable(children: Variable*) { self =>

  val value: Double
  
  private var diff: Double = 0

  final private[ad] def reset(): Unit = {
    diff = 0
    children.foreach(_.reset())
  }

  final private[ad] def propagate(w: Double): Unit = {
    diff += w
    differentiate(w)
  }

  protected def differentiate(w: Double): Unit

  final def diff(variables: Variable*): Seq[Double] = {
    reset()
    propagate(1.0)
    variables.map(_.diff)
  }

  def +(other: Variable): Variable = new Variable(self, other) {
    override val value: Double = self.value + other.value

    override protected def differentiate(w: Double): Unit = {
      self.propagate(1.0)
      other.propagate(1.0)
    }
  }

  def -(other: Variable): Variable = new Variable(self, other) {
    override val value: Double = self.value - other.value

    override protected def differentiate(w: Double): Unit = {
      self.propagate(1.0)
      other.propagate(-1.0)
    }
  }

  def *(other: Variable): Variable = new Variable(self, other) {
    override val value: Double = self.value * other.value
    override protected def differentiate(w: Double): Unit = {
      self.propagate(other.value)
      other.propagate(self.value)
    }
  }

  def /(other: Variable): Variable = new Variable(self, other) {
    override val value: Double = self.value / other.value
    override protected def differentiate(w: Double): Unit = {
      self.propagate(1.0 / other.value)
      other.propagate(-self.value / other.value / other.value)
    }
  }

  def ^(exponent: Double): Variable = new Variable(self) {
    override val value: Double = math.pow(self.value, exponent)
    override protected def differentiate(w: Double): Unit = {
      self.propagate(exponent * math.pow(self.value, exponent - 1))
    }
  }

}

object Variable {
  def apply(value: Double): Variable = {
    val v = value
    new Variable {
      val value: Double = v
      override protected def differentiate(w: Double): Unit = {}
    }
  }
}
