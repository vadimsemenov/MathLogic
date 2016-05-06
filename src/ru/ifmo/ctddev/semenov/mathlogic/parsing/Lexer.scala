package ru.ifmo.ctddev.semenov.mathlogic.parsing

/**
  * Simple Lexer interface // TODO: upgrade scala-doc
  *
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
trait Lexer {
  def hasNext: Boolean
  def current(): Lexeme
  def next(): Lexeme
}

/**
  * Lexer for Propositional Logic
  *
  * @param string - String to split by lexemes
  */
class PropositionalLexer(string: String) extends Lexer {
  private val nonVar = List(OPENED, CLOSED, ARROW, STICK, AMPERSAND, BANG)

  private var ptr: Int = 0
  private var currentToken: Lexeme = null

  // initialize currentToken
  move()

  override def hasNext = {
    skipWhitespaces()
    ptr < string.length
  }

  override def current() = currentToken

  override def next() = {
    if (!hasNext) throw new IllegalStateException("!hasNext")
    move()
    currentToken
  }

  private def move(): Unit = {
    skipWhitespaces()
    // check ranges
    if (ptr >= string.length) {
      currentToken = EOL
      return
    }
    // check logical connectives
    // NB: every two connectives differ in first character
    for (nv <- nonVar) {
      val str = nv.toString
      if (string.startsWith(str, ptr)) {
        currentToken = nv
        ptr += str.length
        return
      }
    }
    // parse propositional variable
    if (Character.isUpperCase(string.charAt(ptr))) {
      var end = ptr + 1
      while (end < string.length && Utils.isDigit(string.charAt(end))) {
        end += 1
      }
      currentToken = VAR(string.substring(ptr, end))
      ptr = end
      return
    }
    throw new IllegalStateException("unknown token at " + ptr + " position")
  }

  private def skipWhitespaces() = {
    while (ptr < string.length && Utils.isWhitespace(string.charAt(ptr))) {
      ptr += 1
    }
  }
}
