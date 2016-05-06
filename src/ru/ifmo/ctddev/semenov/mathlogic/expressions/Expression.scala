package ru.ifmo.ctddev.semenov.mathlogic.expressions

/**
  * Currently Expression stands for Propositional Expression,
  * but it'll be extended in future
  *
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
sealed trait Expression {
  def wrap(expression: Expression) = expression match {
    case !(_) => expression.toString
    case Term(_) => expression.toString
    case _ => '(' + expression.toString + ')'
  }
  def ->(other: Expression) = new ->(this, other)
  def &(other: Expression) = new &(this, other)
  def |(other: Expression) = new |(this, other)
}

// antecedent->consequent
case class ->(lhs: Expression, rhs: Expression) extends Expression {
  override def toString = wrap(lhs) + "->" + wrap(rhs)
}

case class &(lhs: Expression, rhs: Expression) extends Expression {
  override def toString = wrap(lhs) + "&" + wrap(rhs)
}

case class |(lhs: Expression, rhs: Expression) extends Expression {
  override def toString = wrap(lhs) + "|" + wrap(rhs)
}

case class !(expression: Expression) extends Expression {
  override def toString = "!" + wrap(expression)
}

case class Term(name: String) extends Expression {
  override def toString = name
}