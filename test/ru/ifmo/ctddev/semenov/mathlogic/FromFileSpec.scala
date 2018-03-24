package ru.ifmo.ctddev.semenov.mathlogic

import org.scalatest.{FlatSpec, Matchers}
import ru.ifmo.ctddev.semenov.mathlogic.expressions.Expression
import ru.ifmo.ctddev.semenov.mathlogic.parsing.{FormalParser, PropositionalParser}
import ru.ifmo.ctddev.semenov.mathlogic.propositional.{Context, Derivation, Proof}

import scala.io.Source

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
class FromFileSpec extends FlatSpec with Matchers {
  protected def parseExpression(fileName: String, propositional: Boolean = false): Expression = {
    val parser = if (propositional) new PropositionalParser() else new FormalParser()
    parser parse lineIterator(fileName).mkString
  }

  protected def parseDerivation(fileName: String, withHeader: Boolean = true, propositional: Boolean = false): Derivation = {
    val parser = if (propositional) new PropositionalParser() else new FormalParser()
    val lines = lineIterator(fileName)
    val assumptions = if (withHeader) (parser parseHeader lines.next())._1 else Context()
    val proof = Proof(lines.map(parser.parse).toArray: _*)
    Derivation(assumptions, proof)
  }

  private def lineIterator(fileName: String): Iterator[String] = Source.fromFile(fileName, "UTF-8").getLines()
}
