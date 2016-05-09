package ru.ifmo.ctddev.semenov.mathlogic.utils

/**
  * @author Vadim Semenov (semenov@rain.ifmo.ru)
  */
class FenwickTree(size: Int) {
// TODO: generalize with type T (use Numeric?..)
  private val data = new Array[Int](size)

  def update(at: Int, delta: Int): Unit = {
    var i = at
    while (i < size) {
      data(i) += delta
      i |= i + 1
    }
  }

  def sum(at: Int): Int =  {
    var result = 0
    var i = at
    while (i >= 0) {
      result += data(i)
      i = (i & i + 1) - 1
    }
    result
  }
}
