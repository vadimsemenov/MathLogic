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

private object InternalMatcher {
  def internalMatch(fst: Expression, snd: Expression, onVariable: (String, Expression) => Boolean): Boolean = fst match {
    case lhs -> rhs     =>
      if (!snd.isInstanceOf[->]) false
      else {
        val casted = snd.asInstanceOf[->]
        internalMatch(lhs, casted.lhs, onVariable) && internalMatch(rhs, casted.rhs, onVariable)
      }
    case lhs & rhs      =>
      if (!snd.isInstanceOf[&]) false
      else {
        val casted = snd.asInstanceOf[&]
        internalMatch(lhs, casted.lhs, onVariable) && internalMatch(rhs, casted.rhs, onVariable)
      }
    case lhs V rhs      =>
      if (!snd.isInstanceOf[V]) false
      else {
        val casted = snd.asInstanceOf[V]
        internalMatch(lhs, casted.lhs, onVariable) && internalMatch(rhs, casted.rhs, onVariable)
      }
    case !(arg)         => if (!snd.isInstanceOf[!]) false else internalMatch(arg, snd.asInstanceOf[!].arg, onVariable)
    case Variable(name) => onVariable(name, snd)
  }

  def internalExactMatch(fst: Expression, snd: Expression): Boolean = {
    InternalMatcher.internalMatch(fst, snd, (name: String, exp: Expression) => exp match {
      case Variable(otherName) => name == otherName
      case _                   => false
    })
  }

  def internalMatch(pattern: Expression, expression: Expression, map: mutable.Map[String, Expression]): Boolean = {
    InternalMatcher.internalMatch(pattern, expression, (name: String, expression: Expression) => {
      if (map.contains(name)) {
        internalExactMatch(map(name), expression)
      } else {
        map.put(name, expression)
        true
      }
    })
  }
}