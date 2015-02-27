package object ad {

  def exp(v: Variable): Variable = new Variable {
    override val value: Double = math.exp(v.value)
    override protected def differentiate(w: Double): Unit = {
      v.propagate(value)
    }
  }

  def sigmoid(v: Variable): Variable = new Variable {
    override val value: Double = 1 / ( 1 + math.exp(-v.value) )
    override protected def differentiate(w: Double): Unit = {
      v.propagate(value * (1 - value))
    }
  }

  def log(v: Variable): Variable = new Variable {
    override val value: Double = math.log(v.value)
    override protected def differentiate(w: Double): Unit = {
      v.propagate(1 / math.abs(v.value))
    }
  }

  def sin(v: Variable): Variable = new Variable {
    override val value: Double = math.sin(v.value)
    override protected def differentiate(w: Double): Unit = {
      v.propagate(math.cos(v.value))
    }
  }

  def cos(v: Variable): Variable = new Variable {
    override val value: Double = math.cos(v.value)
    override protected def differentiate(w: Double): Unit = {
      v.propagate(-math.sin(v.value))
    }
  }

  def tan(v: Variable): Variable = new Variable {
    override val value: Double = math.tan(v.value)
    override protected def differentiate(w: Double): Unit = {
      val sec = 1 / math.cos(v.value)
      v.propagate(1 / ( sec * sec ))
    }
  }

  def sum(v: Variable*): Variable = new Variable {
    override val value: Double = v.map(_.value).sum
    override protected def differentiate(w: Double): Unit = {
      v.map(_.propagate(w))
    }
  }

  def sum(v: Variables): Variable = sum(v.seq: _*)

  def norm(v: Variables, p: Double = 2): Variable = sum(v.map(_ ^ p): _*)

}
