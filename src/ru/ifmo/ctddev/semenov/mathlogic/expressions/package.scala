package ru.ifmo.ctddev.semenov.mathlogic

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
package object expressions {
  def wrap(expression: Expression): String = expression match {
    case !(_) | Variable(_) | Predicate(_, _) | Succ(_)
         | @@(_, _) | ??(_, _) => expression.toString
    case _                     => '(' + expression.toString + ')'
  }
}
