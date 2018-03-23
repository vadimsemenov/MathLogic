package ru.ifmo.ctddev.semenov.mathlogic.expressions

import scala.collection.mutable

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
class Matcher(pattern: Expression) {
  val map = new mutable.HashMap[String, Expression]()

  def apply(expression: Expression): Boolean = {
    val result = InternalMatcher.internalMatch(pattern, expression, map)
    map.clear()
    result
  }
}

class ReverseMatcher(expression: Expression) extends Matcher(expression) {
  override def apply(pattern: Expression): Boolean = {
    val result = InternalMatcher.internalMatch(pattern, expression, map)
    map.clear()
    result
  }
}

class ExactMatcher(expression: Expression) extends Matcher(expression) {
  override def apply(expression: Expression): Boolean = {
    val result = InternalMatcher.internalExactMatch(this.expression, expression)
    map.clear()
    result
  }
}

private object InternalMatcher {
  def matchAll(fst: Seq[Expression], snd: Seq[Expression], onGap: (String, Expression) => Boolean): Boolean =
    fst.size == snd.size && (fst zip snd forall (p => internalMatch(p._1, p._2, onGap)))

  def internalMatch(fst: Expression, snd: Expression, onGap: (String, Expression) => Boolean): Boolean = (fst, snd) match {
    case (Gap(name), _)                             => onGap(name, snd)
    case (f: (!), s: (!))                           => internalMatch(f.arg, s.arg, onGap)
    case (f: Variable, s: Variable)                 => f.name == s.name
    case (f: Succ, s: Succ)                         => internalMatch(f.expression, s.expression, onGap)
    case (_: Zero, _: Zero)                         => true
    case (f: Predicate, s: Predicate)               => f.name == s.name && matchAll(f.args, s.args, onGap)
    case (f: Function, s: Function)                 => f.name == s.name && matchAll(f.args, s.args, onGap)
    case (f: BinaryExpression, s: BinaryExpression) =>
      internalMatch(f.lhs, s.lhs, onGap) && internalMatch(f.rhs, s.rhs, onGap)
    case (f: Quantifier, s: Quantifier)             =>
      f.symbol == s.symbol && internalMatch(f.expression, s.expression, (name: String, expression: Expression) => {
        if (name == f.variable.name) expression match {
          case Variable(thatName) => s.variable.name == thatName
          case _                  => false
        } else onGap(name, expression)
      })
    case _                                          => false
  }

  def internalExactMatch(fst: Expression, snd: Expression): Boolean =
    InternalMatcher.internalMatch(fst, snd, (_, _: Expression) => false) // shouldn't have gaps

  def internalMatch(pattern: Expression, expression: Expression, substitution: mutable.Map[String, Expression]): Boolean = {
    InternalMatcher.internalMatch(pattern, expression, (name: String, expression: Expression) => {
      if (substitution.contains(name)) {
        internalExactMatch(substitution(name), expression)
      } else {
        substitution.put(name, expression)
        true
      }
    })
  }
}