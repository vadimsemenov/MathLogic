package ru.ifmo.ctddev.semenov.mathlogic

import scala.collection.mutable

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
package object expressions {
  def wrap(expression: Expression): String = expression match {
    case !(_) | Variable(_) | Predicate(_, _) | Succ(_)
         | @@(_, _) | ??(_, _) => expression.toString
    case _                     => '(' + expression.toString + ')'
  }

  def freeVars(expression: Expression): Set[String] = expression match {
    case e: BinaryExpression => freeVars(e.lhs) ++ freeVars(e.rhs)
    case p: Predicate        => p.args.foldRight(Set.empty[String])((e, fv) => fv ++ freeVars(e))
    case f: Function         => f.args.foldRight(Set.empty[String])((e, fv) => fv ++ freeVars(e))
    case q: Quantifier       => freeVars(q.expression) - q.variable.name
    case !(e)                => freeVars(e)
    case Succ(e)             => freeVars(e)
    case Variable(name)      => Set(name)
    case Zero()              => Set.empty
  }


  def freeForSubstitution(expression: Expression, substituted: String, substitution: Expression): Boolean =
    checkForSubstitution(expression, substituted, substitution, Set.empty[String], freeVars(substitution))

  def checkForSubstitution(expression: Expression, substituted: String,
                           substitution: Expression, bounded: Set[String], free: Set[String]): Boolean = expression match {
    case e: BinaryExpression => checkForSubstitution(e.lhs, substituted, substitution, bounded, free) &&
      checkForSubstitution(e.rhs, substituted, substitution, bounded, free)
    case p: Predicate => p.args.forall(checkForSubstitution(_, substituted, substitution, bounded, free))
    case f: Function => f.args.forall(checkForSubstitution(_, substituted, substitution, bounded, free))
    case !(e) => checkForSubstitution(e, substituted, substitution, bounded, free)
    case Succ(e) => checkForSubstitution(e, substituted, substitution, bounded, free)
    case Zero() => true
    case Variable(name) => (bounded contains name) || name != substituted || (name == substituted && (bounded & free).isEmpty)
    case q: Quantifier => checkForSubstitution(q.expression, substituted, substitution, bounded + q.variable.name, free)
  }

  type PartialPredicate[T] = PartialFunction[T, Boolean]
}
