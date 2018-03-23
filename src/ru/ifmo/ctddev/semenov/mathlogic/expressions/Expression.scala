package ru.ifmo.ctddev.semenov.mathlogic.expressions

/**
  * Currently Expression stands for Propositional Expression,
  * but it'll be extended in future
  *
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
sealed trait Expression {
  def ->(other: Expression): ->  = new ->(this, other)
  def ->:(other: Expression): -> = new ->(this, other) // right-associative
  def &(other: Expression): &    = new &(this, other)
  def V(other: Expression): V    = new V(this, other)
  def unary_!(): !               = new !(this)
}

abstract case class BinaryExpression(lhs: Expression, rhs: Expression, delim: String) extends Expression {
  override def toString: String = wrap(lhs) + delim + wrap(rhs)
}

abstract case class Quantifier(variable: Variable, expression: Expression, symbol: String) extends Expression {
  override def toString: String = symbol + variable + wrap(expression)
}

trait ArithmeticExpression extends Expression {
  def +(that: ArithmeticExpression): Function  = new +(this, that)
  def *(that: ArithmeticExpression): Function  = new *(this, that)
  def ===(that: ArithmeticExpression): === = new ===(this, that)
  def succ: Succ                           = Succ(this)
}


// antecedent->consequent
class ->(override val lhs: Expression, override val rhs: Expression) extends BinaryExpression(lhs, rhs, "->")

class &(override val lhs: Expression, override val rhs: Expression) extends BinaryExpression(lhs, rhs, "&")

class V(override val lhs: Expression, override val rhs: Expression) extends BinaryExpression(lhs, rhs, "|")

case class !(arg: Expression) extends Expression {
  override def toString: String = "!" + wrap(arg)
}


class @@(override val variable: Variable, override val expression: Expression) extends Quantifier(variable, expression, "@")

class ??(override val variable: Variable, override val expression: Expression) extends Quantifier(variable, expression, "?")

case class Predicate(name: String, args: Expression*) extends Expression {
  override def toString: String = name + (if (args.isEmpty) "" else args.mkString("(", ",", ")"))
}

class ===(lhs: ArithmeticExpression, rhs: ArithmeticExpression) extends Predicate("=", lhs, rhs) {
  override def toString: String = lhs + "=" + rhs
}


// Arithmetic expressions
case class Variable(name: String) extends ArithmeticExpression {
  override def toString: String = name
}

case class Succ(expression: ArithmeticExpression) extends ArithmeticExpression {
  override def toString: String = wrap(expression) + "'"
}

// +, * -- also functions
case class Function(name: String, args: ArithmeticExpression*) extends ArithmeticExpression {
  require(args.nonEmpty)
  override def toString: String = name + args.mkString("(", ",", ")")
}

class +(lhs: ArithmeticExpression, rhs: ArithmeticExpression) extends Function("+", lhs, rhs) {
  override def toString: String = wrap(lhs) + "+" + wrap(rhs)
}

class *(lhs: ArithmeticExpression, rhs: ArithmeticExpression) extends Function("*", lhs, rhs) {
  override def toString: String = wrap(lhs) + "*" + wrap(rhs)
}

case class Zero() extends ArithmeticExpression {
  override def toString: String = "0"
}

// for axiom schemas
case class Gap(name: String) extends Expression {
  override def toString: String = "_" + name
}

// make case classes
object & {
  def apply(lhs: Expression, rhs: Expression): & = lhs & rhs
  def unapply(arg: &): Some[(Expression, Expression)] = Some(arg.lhs, arg.rhs)
}

object V {
  def apply(lhs: Expression, rhs: Expression): V = lhs V rhs
  def unapply(arg: V): Some[(Expression, Expression)] = Some(arg.lhs, arg.rhs)
}

object -> {
  def apply(lhs: Expression, rhs: Expression): -> = lhs -> rhs
  def unapply(arg: ->): Some[(Expression, Expression)] = Some(arg.lhs, arg.rhs)
}

object @@ {
  def apply(variable: Variable, expression: Expression): @@ = new @@(variable, expression)
  def unapply(arg: @@): Some[(Variable, Expression)] = Some(arg.variable, arg.expression)
}

object ?? {
  def apply(variable: Variable, expression: Expression): ?? = new ??(variable, expression)
  def unapply(arg: ??): Some[(Variable, Expression)] = Some(arg.variable, arg.expression)
}