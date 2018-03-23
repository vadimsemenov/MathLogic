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
  protected def nonVar: List[Lexeme]
}

/**
  * Lexer for Propositional Logic
  *
  * @param string - String to split by lexemes
  */
class PropositionalLexer(string: String) extends Lexer {
  override def nonVar: List[Lexeme] = List(OPENED, CLOSED, ARROW, STICK, AMPERSAND, BANG, COMMA, TURNSTILE)

  protected var ptr: Int = 0
  protected var currentToken: Lexeme = _

  // initialize currentToken
  move()

  override def current(): Lexeme = currentToken

  override def next(): Lexeme = {
    if (!hasNext) throw new IllegalStateException("!hasNext")
    move()
    currentToken
  }

  override def hasNext: Boolean = {
    skipWhitespaces()
    currentToken = if (ptr < string.length) UNDEFINED else EOL
    ptr < string.length
  }

  private def skipWhitespaces(): Unit = {
    while (ptr < string.length && Utils.isWhitespace(string.charAt(ptr))) {
      ptr += 1
    }
  }

  private def move(): Unit = {
    skipWhitespaces()
    // check ranges
    if (ptr >= string.length) {
      currentToken = EOL
      return
    }
    if (parseTurnstile()) return
    if (parseNonVar()) return
    if (parseText()) return
    throw new IllegalStateException(s"unknown token at $ptr position")
  }

  protected def parseTurnstile(): Boolean = {
    if (string.startsWith(TURNSTILE.toString, ptr)) {
      currentToken = TURNSTILE
      ptr += TURNSTILE.toString.length
      return true
    }
    false
  }

  protected def parseNonVar(): Boolean = {
    // check logical connectives
    // NB: every two connectives differ in first character
    // NB: be careful with turnstile
    for (nv <- nonVar) {
      val str = nv.toString
      if (string.startsWith(str, ptr)) {
        currentToken = nv
        ptr += str.length
        return true
      }
    }
    false
  }

  protected def parseText(): Boolean = {
    // parse propositional variable
    if (Utils.isUpperCase(string.charAt(ptr))) {
      var end = ptr + 1
      while (end < string.length && Utils.isDigit(string.charAt(end))) {
        end += 1
      }
      currentToken = PREDICATE(string.substring(ptr, end))
      ptr = end
      return true
    }
    false
  }
}

class FormalLexer(string: String) extends PropositionalLexer(string) {
  override def nonVar: List[Lexeme] = super.nonVar ::: List(EQUALS, ZERO, APOSTROPHE, PLUS, TIMES, FORALL, EXISTS)

  override protected def parseText(): Boolean = {
    if (Utils.isAlphabetic(string.charAt(ptr))) {
      var end = ptr + 1
      while (end < string.length && Utils.isDigit(string.charAt(end))) {
        end += 1
      }
      val ident = string.substring(ptr, end)
      currentToken = if (Utils.isLowerCase(string.charAt(ptr))) VAR(ident) else PREDICATE(ident)
      ptr = end
      return true
    }
    false
  }
}
