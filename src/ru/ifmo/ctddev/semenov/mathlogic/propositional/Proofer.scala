package ru.ifmo.ctddev.semenov.mathlogic.propositional

import ru.ifmo.ctddev.semenov.mathlogic.expressions._

import scala.collection.immutable

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
object Proofer {
  def prove(expression: Expression): Verdict = go(expression, getVarSet(expression).toList) match {
    case Left(interpretation)    => NonTautology(interpretation)
    case Right((context, proof)) => assert(context.isEmpty, () => "context should be empty"); Tautology(proof)
  }

  private def eval(expression: Expression, map: immutable.Map[String, Boolean]): Boolean = {
    def eval(expression: Expression): Boolean = expression match {
      case lhs -> rhs     => if (eval(lhs)) eval(rhs) else true
      case lhs & rhs      => eval(lhs) && eval(rhs)
      case lhs V rhs      => eval(lhs) || eval(rhs)
      case !(arg)         => !eval(arg)
      case Variable(name) => map(name)
      case _              => throw new AssertionError("cannot eval " + expression)
    }

    eval(expression)
  }

  private def getVarSet(expression: Expression): Set[String] = expression match {
    case lhs -> rhs     => getVarSet(lhs) ++ getVarSet(rhs)
    case lhs & rhs      => getVarSet(lhs) ++ getVarSet(rhs)
    case lhs V rhs      => getVarSet(lhs) ++ getVarSet(rhs)
    case !(arg)         => getVarSet(arg)
    case Variable(name) => Set(name)
  }

  private def prove(assignment: immutable.ListMap[String, Boolean],
                    expression: Expression
                   ): Either[Proof, Proof] = {
    expression match {
      case lhs -> rhs     => prove(assignment, lhs) match {
        case Left(proof)  => Right(proof ++ proveImplicationF_(lhs, rhs))
        case Right(proof) => prove(assignment, rhs) match {
          case Left(anotherProof)  => Left(proof ++ anotherProof ++ proveImplicationTF(lhs, rhs))
          case Right(anotherProof) => Right(proof ++ anotherProof ++ proveImplication_T(lhs, rhs))
        }
      }
      case lhs & rhs      => prove(assignment, lhs) match {
        case Left(proof)  => Left(proof ++ proveConjunctionF_(lhs, rhs))
        case Right(proof) => prove(assignment, rhs) match {
          case Left(anotherProof)  => Left(anotherProof ++ proveConjunction_F(lhs, rhs))
          case Right(anotherProof) => Right(proof ++ anotherProof ++ proveConjunctionTT(lhs, rhs))
        }
      }
      case lhs V rhs      => prove(assignment, lhs) match {
        case Left(proof)  => prove(assignment, rhs) match {
          case Left(anotherProof)  => Left(proof ++ anotherProof ++ proveDisjunctionFF(lhs, rhs))
          case Right(anotherProof) => Right(anotherProof ++ proveDisjunction_T(lhs, rhs))
        }
        case Right(proof) => Right(proof ++ proveDisjunctionT_(lhs, rhs))
      }
      case !(exp)         => prove(assignment, exp) match {
        case Left(proof)  => Right(proof)
        case Right(proof) => Left(proof ++ doubleNegationIntroduction(exp))
      }
      case Variable(name) => if (assignment(name)) Right(Proof(expression)) else Left(Proof(!expression))
    }
  }

  /**
    * Rule of excluded middle.
    * <p>
    * Context for `fst` and `snd` should be same except last expression.
    * In context of `fst` last expression should be propositional variable (say `A`),
    * and last expression in context of `snd` should be `!A`
    * <p>
    * Proofs of `fst` and `snd` should end with same expression — `α`.
    * <p>
    * The result will be derivation of `α` from context without both `A` and `!A`.
    */
  private def excludeMiddle(fst: Derivation, snd: Derivation): Derivation = {
    val (alpha, beta) = {
      val f = fst._1.last
      val s = snd._1.last
      assert(f.isInstanceOf[Variable])
      assert((!f) == s, s"negation of $f not equal to $s [$fst, $snd]")
      assert(fst._1.init == snd._1.init)
      val ff = fst._2.last
      val ss = snd._2.last
      assert(ff == ss, s"$ff != $ss [$fst, $snd]")
      (f, ff)
    }
    Derivation(fst._1.init, deduction(fst)._2 ++ deduction(snd)._2 ++ lawOfExcludedMiddle(alpha, beta))
  }

  private def go(expression: Expression,
                 vars: List[String],
                 assignment: immutable.ListMap[String, Boolean] = immutable.ListMap.empty
                ): Either[List[(String, Boolean)], Derivation] = vars match {
    case name :: vs => go(expression, vs, assignment + ((name, true))) match {
      case Right(fst) => go(expression, vs, assignment + ((name, false))) match {
        case Right(snd) => Right(excludeMiddle(fst, snd))
        case r@Left(_)  => r
      }
      case r@Left(_)  => r
    }
    case Nil        =>
      val context = Context(assignment.toList map (v => if (v._2) Variable(v._1) else !Variable(v._1)): _*)
      prove(assignment, expression) match {
        case Left(_)      => Left(assignment.toList)
        case Right(proof) => Right(Derivation(context, proof))
      }
  }

  sealed trait Verdict {
    def isTautology: Boolean
    def getProof: Proof = throw new IllegalStateException("Expression is not tautology")
    def getInterpretation: List[(String, Boolean)] = throw new IllegalStateException("Expression is tautology")
  }

  case class Tautology(proof: Proof) extends Verdict {
    override def isTautology = true
    override def getProof = proof
  }

  case class NonTautology(interpretation: List[(String, Boolean)]) extends Verdict {
    override def isTautology = false
    override def getInterpretation = interpretation
  }
}
