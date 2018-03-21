package ru.ifmo.ctddev.semenov.mathlogic.ordinals

import org.parboiled2._

import scala.language.implicitConversions

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
class OrdinalParser(val input: ParserInput) extends Parser {

  implicit private def wrpStr(s: String): Rule0 = rule {
    zeroOrMore(' ') ~ str(s) ~ zeroOrMore(' ')
  }

  private def leftAssoc[A](a: => Rule1[A], b: (A, A) => A, divider: String): Rule1[A] = rule {
    a ~ zeroOrMore(wrpStr(divider) ~ a ~> b)
  }

  def equalityOrdinals: Rule1[(Ordinal, Ordinal)] = rule {
    oneOrMore(ordExpr).separatedBy("=") ~> ((sq: Seq[Ordinal]) => (sq(0), sq(1)))
  }

  def ordExpr: Rule1[Ordinal] = leftAssoc(summable, Ordinal.sum, "+")

  private def summable: Rule1[Ordinal] = leftAssoc(mullable, Ordinal.mul, "*")

  private def mullable: Rule1[Ordinal] = rule {
    oneOrMore(unary).separatedBy("^") ~> ((_: Seq[Ordinal]).reduceRight(Ordinal.pow))
  }

  private def unary: Rule1[Ordinal] = rule {
    omega | number | ("(" ~ ordExpr ~ ")")
  }

  private def omega: Rule1[Ordinal] = rule {
    ch('w') ~> (() => ω)
  }

  private def number: Rule1[Ordinal] = rule {
    capture(oneOrMore(anyOf("0123456789"))) ~> ((s: String) => Atom(Integer.parseInt(s)))
  }
}
