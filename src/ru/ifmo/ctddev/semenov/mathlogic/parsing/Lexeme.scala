package ru.ifmo.ctddev.semenov.mathlogic.parsing

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
trait Lexeme {
}

// Propositional Tokens
case object OPENED            extends Lexeme { override def toString = "(" }
case object CLOSED            extends Lexeme { override def toString = ")" }
case object ARROW             extends Lexeme { override def toString = "->" }
case object STICK             extends Lexeme { override def toString = "|" }
case object AMPERSAND         extends Lexeme { override def toString = "&" }
case object BANG              extends Lexeme { override def toString = "!" }
case class  VAR(name: String) extends Lexeme { override def toString = name }
case object FOREACH           extends Lexeme { override def toString = "@" }
case object EXISTS            extends Lexeme { override def toString = "?" }

// Auxiliary Tokens
case object EPS               extends Lexeme { override def toString = "ε" }
case object EOL               extends Lexeme { override def toString = "$" }
case object BB                extends Lexeme { override def toString = "_" }
