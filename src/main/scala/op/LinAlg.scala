/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package op

object LinAlg {

  def scalarMultiply(scalar: Double, v: List[Double]): List[Double] = {
    v.map(_ * scalar)
  }

  def dot(v1: List[Double], v2: List[Double]): Double = {
    assert(v1.size == v2.size)
    v1.zip(v2).map { case (x1, x2) => x1 * x2 }.sum
  }
}
