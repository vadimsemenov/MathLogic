package ru.ifmo.ctddev.semenov.mathlogic.expressions

/**
  * Currently Expression stands for Propositional Expression,
  * but it'll be extended in future
  *
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
sealed trait Expression {
  def wrap(expression: Expression) = expression match {
    case !(_) | Variable(_) => expression.toString
    case _                  => '(' + expression.toString + ')'
  }

  def ->(other: Expression): ->  = new ->(this, other)
  def ->:(other: Expression): -> = new ->(this, other) // right-associative
  def &(other: Expression): &    = new &(this, other)
  def V(other: Expression): V    = new V(this, other)
  def unary_!(): !               = new !(this)
}

// antecedent->consequent
case class ->(lhs: Expression, rhs: Expression) extends Expression {
  override def toString = wrap(lhs) + "->" + wrap(rhs)
}

case class &(lhs: Expression, rhs: Expression) extends Expression {
  override def toString = wrap(lhs) + "&" + wrap(rhs)
}

case class V(lhs: Expression, rhs: Expression) extends Expression {
  override def toString = wrap(lhs) + "|" + wrap(rhs)
}

case class !(arg: Expression) extends Expression {
  override def toString = "!" + wrap(arg)
}

case class Variable(name: String) extends Expression {
  override def toString = name
}