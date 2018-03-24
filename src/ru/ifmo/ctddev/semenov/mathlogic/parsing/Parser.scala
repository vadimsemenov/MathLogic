package ru.ifmo.ctddev.semenov.mathlogic.parsing

import ru.ifmo.ctddev.semenov.mathlogic.expressions._
import ru.ifmo.ctddev.semenov.mathlogic.propositional._

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
trait Parser {
  def parseHeader(string: String): (Context, Expression)
  def parse(string: String): Expression
  def createLexer(string: String): Lexer
}

/**
  * Simple top-down Propositional Parser
  *
  * NB: not thread-safe
  */
class PropositionalParser(axiomSchema: Boolean = false) extends Parser {
  protected var lexer: Lexer = _

  override def createLexer(string: String): Lexer = new PropositionalLexer(string)

  override def parseHeader(string: String): (Context, Expression) = {
    lexer = createLexer(string)
    var context = Context()
    var first = true
    while (lexer.current() != TURNSTILE) {
      if (first) first = false
      else consume(COMMA)
      context += parseExpression()
    }
    consume(TURNSTILE)
    val expression = parseExpression()
    consume(EOL)
    (context, expression)
  }

  override def parse(string: String): Expression = {
    lexer = createLexer(string)
    val expression = parseExpression()
    consume(EOL)
    expression
  }

  protected def parseExpression(): Expression = {
    var expression = parseDisjunction()
    if (lexer.current() == ARROW) {
      consume(ARROW)
      expression = expression -> parseExpression() // TODO: tailrec
    }
    expression
  }

  protected def parseDisjunction(): Expression = {
    var disjunction = parseConjunction()
    while (lexer.current() == STICK) {
      consume(STICK)
      disjunction = disjunction V parseConjunction()
    }
    disjunction
  }

  protected def parseConjunction(): Expression = {
    var conjunction = parseConjunct()
    while (lexer.current() == AMPERSAND) {
      consume(AMPERSAND)
      conjunction = conjunction & parseConjunct()
    }
    conjunction
  }

  protected def parseConjunct(): Expression = lexer.current() match {
    case BANG         => parseNegation()
    case OPENED       => parseBetweenParenthesis(parseExpression)()
    case PREDICATE(p) => parsePredicate(p)
    case _            => throw new IllegalStateException("not expected: " + lexer.current())
  }

  protected def parseNegation(): Expression = {
    consume(BANG)
    !parseConjunct()
  }

  protected def parseBetweenParenthesis[T](parseAction: () => T)(): T = {
    consume(OPENED)
    val expression = parseAction()
    consume(CLOSED)
    expression
  }

  protected def parsePredicate(name: String): Expression = {
    consume(PREDICATE(name))
    if (axiomSchema) Gap(name) else Variable(name)
  }

  protected def consume(token: Lexeme): Unit = {
    if (lexer.current().getClass != token.getClass) {
      throw new IllegalStateException("expected: " + token + ", but found: " + lexer.current() + ", " + lexer.toString)
    }
    if (lexer.hasNext) {
      lexer.next()
    }
  }
}

class FormalParser extends PropositionalParser {
  override def createLexer(string: String): Lexer = new FormalLexer(string)

  override protected def parseConjunct(): Expression = lexer.current() match {
    case BANG            => parseNegation()
    case q: LQuantifier  => parseQuantifier(q)
    case PREDICATE(name) => parsePredicate(name)
    case OPENED          => if (lexer.lookAheadForPredicate) parseEquality() else parseBetweenParenthesis(parseExpression)()
    case _               => parseEquality()
  }

  override protected def parsePredicate(name: String): Predicate = {
    consume(PREDICATE(name))
    if (lexer.current() == OPENED) Predicate(name, parseArgs(): _*)
    else Predicate(name)
  }

  protected def parseEquality(): === = {
    val lhs = parseTerm()
    consume(EQUALS)
    val rhs = parseTerm()
    lhs === rhs
  }

  protected def parseQuantifier(quantifier: LQuantifier): Quantifier = {
    consume(quantifier)
    lexer.current() match {
      case v: VAR =>
        consume(v)
        val expression = parseConjunct()
        quantifier match {
          case FORALL => @@(Variable(v.name), expression)
          case EXISTS => ??(Variable(v.name), expression)
        }
      case _      => throw new IllegalStateException(s"${lexer.current()} under quantifier $quantifier")
    }
  }


  protected def parseTerm(): ArithmeticExpression = {
    var result = parseSummand()
    while (lexer.current() == PLUS) {
      consume(PLUS)
      result = result + parseSummand()
    }
    result
  }


  protected def parseSummand(): ArithmeticExpression = {
    var result = parseMultiplier()
    while (lexer.current() == TIMES) {
      consume(TIMES)
      result = result * parseMultiplier()
    }
    result
  }

  protected def parseMultiplier(): ArithmeticExpression = {
    var result = lexer.current() match {
      case OPENED => parseBetweenParenthesis(parseTerm)()
      case ZERO   => parseZero()
      case v: VAR => parseVarOrFunc(v)
      case _      => throw new IllegalStateException(s"Unexpected lexeme: ${lexer.current()}, lexer=$lexer")
    }
    while (lexer.current() == APOSTROPHE) {
      consume(APOSTROPHE)
      result = result.succ
    }
    result
  }

  protected def parseZero(): Zero = {
    consume(ZERO)
    Zero()
  }

  protected def parseVarOrFunc(variable: VAR): ArithmeticExpression = {
    consume(variable)
    if (lexer.current() == OPENED) Function(variable.name, parseArgs(): _*)
    else Variable(variable.name)
  }

  protected def parseArgs(): Array[ArithmeticExpression] = parseBetweenParenthesis(parseArgsInside)

  protected def parseArgsInside(): Array[ArithmeticExpression] = {
    val args  = Array.newBuilder[ArithmeticExpression]
    var first = true
    while (lexer.current() != CLOSED) {
      if (first) first = false
      else consume(COMMA)
      args += parseTerm()
    }
    args.result()
  }
}
