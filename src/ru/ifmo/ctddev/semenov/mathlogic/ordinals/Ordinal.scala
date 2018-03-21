package ru.ifmo.ctddev.semenov.mathlogic.ordinals

import scala.annotation.tailrec
import scala.math.BigInt

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
sealed trait Ordinal extends Ordered[Ordinal] {
  def +(that: Ordinal): Ordinal = Ordinal.sum(this, that)
  def *(that: Ordinal): Ordinal = Ordinal.mul(this, that)
  def ^(that: Ordinal): Ordinal = Ordinal.pow(this, that)
}

object Ordinal {
  def sum(lhs: Ordinal, rhs: Ordinal): Ordinal = (lhs, rhs) match {
    case (Atom(lval), Atom(rval))                         => Atom(lval + rval)
    case (Atom(_), _)                                     => rhs
    case (CNF(lpow, lcoef, lrem), Atom(_))                => CNF(lpow, lcoef, sum(lrem, rhs))
    case (CNF(lpow, lcoef, lrem), CNF(rpow, rcoef, rrem)) => lpow compare rpow match {
      case c if c < 0 => rhs
      case c if c > 0 => CNF(lpow, lcoef, sum(lrem, rhs))
      case _          => CNF(lpow, lcoef + rcoef, rrem)
    }
  }

  // TODO: speedup
  def mul(lhs: Ordinal, rhs: Ordinal): Ordinal = (lhs, rhs) match {
    case (ZERO, _)                                 => ZERO
    case (_, ZERO)                                 => ZERO
    case (Atom(lval), Atom(rval))                  => Atom(lval * rval)
    case (Atom(_), _)                              => rhs
    case (CNF(lpow, lcoef, lrem), Atom(rval))      => CNF(lpow, lcoef * rval, lrem)
    case (CNF(lpow, _, _), CNF(rpow, rcoef, rrem)) => CNF(sum(lpow, rpow), rcoef, mul(lhs, rrem))
  }

  def sub(lhs: Ordinal, rhs: Ordinal): Ordinal = (lhs, rhs) match {
    case (Atom(lval), Atom(rval)) if lval <= rval         => ZERO
    case (Atom(lval), Atom(rval))                         => Atom(lval - rval)
    case (Atom(_), _)                                     => ZERO
    case (_, Atom(_))                                     => lhs
    case (CNF(lpow, lcoef, lrem), CNF(rpow, rcoef, rrem)) => lpow compare rpow match {
      case c if c < 0         => ZERO
      case c if c > 0         => lhs
      case _ if lcoef < rcoef => ZERO
      case _ if lcoef > rcoef => CNF(lpow, lcoef - rcoef, lrem)
      case _                  => sub(lrem, rrem)
    }
  }

  def pow(lhs: Ordinal, rhs: Ordinal): Ordinal = lhs match {
    case _ if rhs == ZERO => ONE
    case Atom(n)          => raiseNaturalToInfOrdinal(n, rhs)
    case CNF(lpow, _, _)  => rhs match {
      case Atom(m) => raiseInfOrdinalToNatural(lhs, m)
      case _: CNF  => mul(CNF(mul(lpow, limitPart(rhs)), 1, ZERO),
        raiseInfOrdinalToNatural(lhs, naturalPart(rhs)))
    }
  }

  private def raiseLimitOrdinalToNatural(base: Ordinal, power: BigInt): Ordinal =
    if (power == 0) ONE
    else if (power == 1) base
    else base match {
      case Atom(n)          => Atom(powBigInt(n, power))
      case CNF(alpha, _, _) => mul(CNF(mul(alpha, Atom(power - 1)), 1, ZERO), base)
    }

  private def raiseInfOrdinalToNatural(base: Ordinal, power: BigInt): Ordinal = {
    if (power == 0) ONE
    else if (power == 1) base
    else base match {
      case Atom(n)      => Atom(powBigInt(n, power))
      case CNF(_, _, _) =>
        if (isLimitOrdinal(base)) raiseLimitOrdinalToNatural(base, power)
        else mul(raiseInfOrdinalToNatural(base, power - 1), base)
    }
  }

  private def raiseNaturalToInfOrdinal(base: BigInt, ordinal: Ordinal): Ordinal =
    if (base == 0 || base == 1) Atom(base)
    else ordinal match {
      case Atom(m)                            => Atom(powBigInt(base, m))
      case CNF(power, coefficient, remainder) =>
        if (power == ONE) {
          require(remainder.isInstanceOf[Atom])
          CNF(Atom(coefficient), powBigInt(base, remainder.asInstanceOf[Atom].value), ZERO)
        } else remainder match {
          case Atom(v) =>
            CNF(CNF(sub(power, ONE), coefficient, ZERO), powBigInt(base, v), ZERO)
          case _       =>
            val (ce, cc): (Ordinal, BigInt) = raiseNaturalToInfOrdinal(base, remainder) match {
              case Atom(n)      => (ZERO, n)
              case CNF(e, c, _) => (e, c)
            }
            CNF(CNF(sub(power, ONE), 1, ce), cc, ZERO)
        }
    }

  private def powBigInt(base: BigInt, power: BigInt): BigInt =
    if (power.isValidInt) base pow power.intValue()
    else throw new ArithmeticException(s"too big power: $power")

  @tailrec private def naturalPart(ordinal: Ordinal): BigInt = ordinal match {
    case Atom(x)              => x
    case CNF(_, _, remainder) => naturalPart(remainder)
  }

  private def limitPart(ordinal: Ordinal): Ordinal = ordinal match {
    case Atom(_)                            => ZERO
    case CNF(power, coefficient, remainder) => CNF(power, coefficient, limitPart(remainder))
  }

  @tailrec private def isLimitOrdinal(ordinal: Ordinal): Boolean = ordinal match {
    case Atom(x)              => x == 0
    case CNF(_, _, remainder) => isLimitOrdinal(remainder)
  }
}

case class Atom(value: BigInt) extends Ordinal {
  require(value >= 0)

  override def compare(that: Ordinal): Int = that match {
    case Atom(thatValue) => value compare thatValue
    case _               => -1
  }
}

case class CNF(power: Ordinal, coefficient: BigInt, remainder: Ordinal) extends Ordinal {
  override def compare(that: Ordinal): Int = that match {
    case _: Atom => 1
    case other: CNF  => // cnfComparator.compare(this, c)
      this.power.compare(other.power) match {
        case 0 => this.coefficient.compare(other.coefficient) match {
          case 0 => this.remainder.compare(other.remainder)
          case c => c
        }
        case c => c
      }
  }
}