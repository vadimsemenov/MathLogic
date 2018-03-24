package ru.ifmo.ctddev.semenov.mathlogic.expressions

import java.util.Objects

import ru.ifmo.ctddev.semenov.mathlogic.expressions.Matcher.SubstitutionMap

import scala.collection.mutable

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
class Matcher(pattern: Expression) {
  val map = new mutable.HashMap[String, Expression]()
  var lastHash: Int = 0
  var lastMap: Option[SubstitutionMap] = None

  def smartUpdate(hash: Int, update: () => Option[SubstitutionMap]) = {
    if (hash != lastHash || lastMap.isEmpty) {
      lastHash = hash
      lastMap = update()
    }
    lastMap
  }

  def apply(expression: Expression): Boolean = {
    val result = InternalMatcher.internalMatchWithSubstitutionMap(pattern, expression, map)
    map.clear()
    result
  }
}

object Matcher {
  def reverseMatch(expression: Expression)(pattern: Expression): Boolean = new ReverseMatcher(expression)(pattern)

  def exactMatch(expression: Expression)(that: Expression): Boolean = new ExactMatcher(expression)(that)

  def matchWithOneSubstitution(pattern: Expression, substituted: Variable)(expression: Expression): Option[SubstitutionMap] =
    new MatcherWithSubstitution(pattern, substituted).check(expression)

  def matchAfterSubstitution(pattern: Expression, substituted: Variable, substitution: Expression)(expression: Expression): Boolean =
    new ExactMatcherAfterSubstitution(pattern, substituted, substitution)(expression)

  type SubstitutionMap = Map[String, Expression]
}

private class ReverseMatcher(expression: Expression) extends Matcher(expression) {
  override def apply(pattern: Expression): Boolean = {
    val result = InternalMatcher.internalMatchWithSubstitutionMap(pattern, expression, map)
    map.clear()
    result
  }
}

private class ExactMatcher(expression: Expression) extends Matcher(expression) {
  override def apply(expression: Expression): Boolean = {
    val result = InternalMatcher.internalExactMatch(this.expression, expression)
    map.clear()
    result
  }
}

private class MatcherWithSubstitution(pattern: Expression, substituted: Variable) extends Matcher(pattern) {
  override def apply(expression: Expression): Boolean = check(expression).isDefined

  def check(expression: Expression): Option[SubstitutionMap] =
    smartUpdate(Objects.hash(pattern, substituted, expression), { () =>
      import InternalMatcher._
      val result = internalMatch(pattern, expression, partialEq = {
        case (v@Variable(name), other) if v == substituted => internalExactMatch(other, map.getOrElseUpdate(name, other))
      })
      assert(map.size <= 1)
      val mapCopy = Map(map.toArray: _*)
      map.clear()
//      result
      if (result) Some(mapCopy) else None
    })
}

private class ExactMatcherAfterSubstitution(pattern: Expression, substituted: Variable, substitution: Expression) extends Matcher(pattern) {
  override def apply(expression: Expression): Boolean = InternalMatcher.internalMatch(pattern, expression, partialEq = {
    case (Variable(name), other) if substituted.name == name => InternalMatcher.internalExactMatch(substitution, other)
  })
}

private object InternalMatcher {
  def matchAll(fst: Seq[Expression], snd: Seq[Expression], callback: PartialPredicate[(Expression, Expression)]): Boolean =
    fst.size == snd.size && (fst zip snd forall (p => internalMatch(p._1, p._2, callback)))

  def internalMatch(fst: Expression, snd: Expression, partialEq: PartialPredicate[(Expression, Expression)]): Boolean = (fst, snd) match {
    case arg if partialEq.isDefinedAt(arg)          => partialEq(arg)
    case (f: (!), s: (!))                           => internalMatch(f.arg, s.arg, partialEq)
    case (f: Succ, s: Succ)                         => internalMatch(f.arg, s.arg, partialEq)
    case (f: Variable, s: Variable)                 => f.name == s.name
    case (_: Zero, _: Zero)                         => true
    case (f: Predicate, s: Predicate)               => f.name == s.name && matchAll(f.args, s.args, partialEq)
    case (f: Function, s: Function)                 => f.name == s.name && matchAll(f.args, s.args, partialEq)
    case (f: BinaryExpression, s: BinaryExpression) =>
      internalMatch(f.lhs, s.lhs, partialEq) && internalMatch(f.rhs, s.rhs, partialEq)
    case (f: Quantifier, s: Quantifier)             =>
      lazy val filter: PartialPredicate[(Expression, Expression)] = {
        case (v@Variable(thisName), Variable(thatName)) if v == f.variable => thisName == thatName
      }
      f.symbol == s.symbol && internalMatch(f.expression, s.expression, filter orElse partialEq)
    case _                                          => false
  }

  def internalExactMatch(fst: Expression, snd: Expression): Boolean =
    (fst eq snd) || InternalMatcher.internalMatch(fst, snd, PartialFunction.empty) // shouldn't have gaps

  def internalMatchWithSubstitutionMap(pattern: Expression, expression: Expression, substitution: mutable.Map[String, Expression]): Boolean =
    InternalMatcher.internalMatch(pattern, expression, partialEq = {
      case (Gap(name), right: Expression) => internalExactMatch(right, substitution.getOrElseUpdate(name, right))
    })
}