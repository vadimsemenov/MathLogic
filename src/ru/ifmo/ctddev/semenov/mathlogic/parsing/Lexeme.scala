package ru.ifmo.ctddev.semenov.mathlogic.parsing

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
trait Lexeme

trait LQuantifier extends Lexeme

// Propositional Tokens
case object OPENED            extends Lexeme { override def toString = "(" }
case object CLOSED            extends Lexeme { override def toString = ")" }
case object COMMA             extends Lexeme { override def toString = "," }
case object ARROW             extends Lexeme { override def toString = "->" }
case object STICK             extends Lexeme { override def toString = "|" }
case object AMPERSAND         extends Lexeme { override def toString = "&" }
case object BANG              extends Lexeme { override def toString = "!" }
case object PLUS              extends Lexeme { override def toString = "+" }
case object TIMES             extends Lexeme { override def toString = "*" }
case object EQUALS            extends Lexeme { override def toString = "=" }
case object ZERO              extends Lexeme { override def toString = "0" }
case object APOSTROPHE        extends Lexeme { override def toString = "'" }
case object TURNSTILE         extends Lexeme { override def toString = "|-" }
case object FORALL            extends LQuantifier { override def toString = "@" }
case object EXISTS            extends LQuantifier { override def toString = "?" }

case class VAR(name: String)       extends Lexeme { override def toString: String = name }
case class PREDICATE(name: String) extends Lexeme { override def toString: String = name }

//object PREDICATE {
//  def apply(name: String): PREDICATE = new PREDICATE(name)
//  def unapply(arg: PREDICATE): Some[String] = Some(arg.name)
//}

// Auxiliary Tokens
case object EPS               extends Lexeme { override def toString = "ε" }
case object EOL               extends Lexeme { override def toString = "$" }
case object BB                extends Lexeme { override def toString = "_" }
case object UNDEFINED         extends Lexeme { override def toString = " "}
