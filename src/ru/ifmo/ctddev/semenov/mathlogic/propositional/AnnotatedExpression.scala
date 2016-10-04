package ru.ifmo.ctddev.semenov.mathlogic.propositional

import ru.ifmo.ctddev.semenov.mathlogic.expressions.Expression

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
case class AnnotatedExpression(lineNum: Int, expression: Expression, annotation: Annotation) {
  override def toString: String = s"($lineNum) $expression ($annotation)"
}
