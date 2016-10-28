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
    case (_, Atom(_))                                     => lhs
    case (CNF(lpow, lcoef, lrem), CNF(rpow, rcoef, rrem)) =>
      if (lpow < rpow) rhs
      else if (lpow == rpow) CNF(lpow, lcoef + rcoef, rrem)
      else CNF(lpow, lcoef, sum(lrem, rhs))
  }

  // TODO: speedup
  def mul(lhs: Ordinal, rhs: Ordinal): Ordinal = (lhs, rhs) match {
    case (Atom(lval), Atom(rval))                         => Atom(lval * rval)
    case (Atom(lval), _)                                  => if (lval == 0) Atom(0) else rhs
    case (CNF(lpow, lcoef, lrem), Atom(rval))             => CNF(lpow, lcoef * rval, lrem)
    case (CNF(lpow, lcoef, lrem), CNF(rpow, rcoef, rrem)) => CNF(sum(lpow, rpow), rcoef, mul(lhs, rrem))
  }

  def sub(lhs: Ordinal, rhs: Ordinal): Ordinal = (lhs, rhs) match {
    case (Atom(lval), Atom(rval))                         => Atom(BigInt(0) max (lval - rval))
    case (Atom(_), _)                                     => Atom(0)
    case (_, Atom(_))                                     => lhs
    case (CNF(lpow, lcoef, lrem), CNF(rpow, rcoef, rrem)) =>
      if (lpow < rpow) Atom(0)
      else if (lpow > rpow) lhs
      else if (lcoef < rcoef) Atom(0)
      else if (lcoef > rcoef) CNF(lpow, lcoef - rcoef, lrem)
      else sub(lrem, rrem)
  }

  def pow(lhs: Ordinal, rhs: Ordinal): Ordinal = lhs match {
    case Atom(n)                => if (n == 0 || n == 1) lhs else powNaturalToInfOrdinal(n, rhs)
    case CNF(lpow, lcoef, lrem) => rhs match {
      case Atom(m)                => powInfOrdinalToNatural(lhs, m)
      case CNF(rpow, rcoef, rrem) => mul(CNF(mul(lpow, limitPart(rhs)), 1, Atom(0)),
        powInfOrdinalToNatural(lhs, getNaturalPart(rhs)))
    }
  }

  private def powLimitOrdinalToNatural(base: Ordinal, power: BigInt): Ordinal =
    if (power == 0) Atom(1)
    else if (power == 1) base
    else base match {
      case Atom(n)                            => Atom(powBigInt(n, power))
      case CNF(alpha, coefficient, remainder) => mul(CNF(mul(alpha, Atom(power - 1)), 1, Atom(0)), base)
    }

  private def powInfOrdinalToNatural(base: Ordinal, power: BigInt): Ordinal =
    if (power.isValidInt && power.intValue() == 0) Atom(1)
    else if (power.isValidInt && power.intValue() == 1) base
    else base match {
      case Atom(n)                => Atom(powBigInt(n, power))
      case CNF(bpow, bcoef, brem) =>
        if (isLimitOrdinal(base)) powLimitOrdinalToNatural(base, power)
        else mul(powInfOrdinalToNatural(base, power - 1), base)
    }

  private def powNaturalToInfOrdinal(base: BigInt, rhs: Ordinal): Ordinal =
    if (base == 0 || base == 1) Atom(base)
    else rhs match {
      case Atom(m)                            => Atom(powBigInt(base, m))
      case CNF(power, coefficient, remainder) =>
        if (power == Atom(1)) {
          require(remainder.isInstanceOf[Atom])
          CNF(Atom(coefficient), powBigInt(base, remainder.asInstanceOf[Atom].value), Atom(0))
        } else remainder match {
          case atom: Atom =>
            CNF(CNF(sub(power, Atom(1)), coefficient, Atom(0)), powBigInt(base, atom.value), Atom(0))
          case _          =>
            val (ce, cc): (Ordinal, BigInt) = powNaturalToInfOrdinal(base, remainder) match {
              case Atom(n)      => (Atom(0), n)
              case CNF(e, c, _) => (e, c)
            }
            CNF(CNF(sub(power, Atom(1)), 1, ce), cc, Atom(0))
        }
    }

  private def powBigInt(base: BigInt, power: BigInt): BigInt =
    if (power.isValidInt) base pow power.intValue()
    else throw new ArithmeticException(s"too big power: $power")

  @tailrec private def getNaturalPart(ordinal: Ordinal): BigInt = ordinal match {
    case Atom(x)              => x
    case CNF(_, _, remainder) => getNaturalPart(remainder)
  }

  private def limitPart(ordinal: Ordinal): Ordinal = ordinal match {
    case Atom(_)                            => Atom(0)
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
    case Atom(_)                               => 1
    case CNF(_power, _coefficient, _remainder) => power compare _power match {
      case 0   => coefficient compare _coefficient match {
        case 0   => remainder compare _remainder
        case cmp => cmp
      }
      case cmp => cmp
    }
  }
}