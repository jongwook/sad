package ad

class Variables(variables: Seq[Variable]) extends Seq[Variable] {

  override def length: Int = variables.length

  override def apply(idx: Int): Variable = variables(idx)

  def apply(range: Seq[Int]): Variables = new Variables(range.map(variables))

  def sum(): Variable = ad.sum(variables: _*)

  def norm(p: Double = 2): Variable = ad.norm(this, p)

  override def iterator: Iterator[Variable] = variables.iterator

}
