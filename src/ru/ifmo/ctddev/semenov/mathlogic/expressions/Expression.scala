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
  // TODO: make it right-associative
  def ->(other: Expression) = new ->(this, other)
  def &(other: Expression) = new &(this, other)
  def |(other: Expression) = new |(this, other)
  def unary_!() = new !(this)
}

sealed trait Binary extends Expression {
  def lhs: Expression
  def rhs: Expression
}

sealed trait Unary extends Expression {
  def arg: Expression
}

// antecedent->consequent
case class ->(lhs: Expression, rhs: Expression) extends Binary {
  override def toString = wrap(lhs) + "->" + wrap(rhs)
}

case class &(lhs: Expression, rhs: Expression) extends Binary {
  override def toString = wrap(lhs) + "&" + wrap(rhs)
}

case class |(lhs: Expression, rhs: Expression) extends Binary {
  override def toString = wrap(lhs) + "|" + wrap(rhs)
}

case class !(arg: Expression) extends Unary {
  override def toString = "!" + wrap(arg)
}

case class Variable(name: String) extends Expression {
  override def toString = name
}