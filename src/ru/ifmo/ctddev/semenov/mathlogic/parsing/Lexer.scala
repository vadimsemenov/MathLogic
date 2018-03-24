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
  def lookAheadForPredicate: Boolean
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

  override def lookAheadForPredicate: Boolean = currentToken.isInstanceOf[PREDICATE]

  override def toString: String = s"Lexer(ptr=$ptr,str=[$string])"

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
  private var nextEq = -1 // dirty hack to improve time complexity while looking ahead

  override def lookAheadForPredicate: Boolean = {
    if (super.lookAheadForPredicate) return true
    if (nextEq < ptr) {
      nextEq = string.indexOf('=', ptr)
    }
    if (nextEq < 0) false else {
      var balance = 0
      for (i <- math.max(0, ptr - 1) until nextEq) {
        val ch = string.charAt(i)
        if (ch == '(') {
          balance += 1
        } else if (ch == ')') {
          balance -= 1
          if (balance < 0) return false
        }
      }
      balance == 0
    }
  }

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
