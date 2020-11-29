/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package data.normalize

class UnitNormalizer(override val xs: List[Double],
                     val minValue: Double = 0.0,
                     val maxValue: Double = 1.0) extends Normalizer(xs) {

  val euclideanNorm: Double = math.sqrt(xs.map(x => math.pow(x, 2)).sum)

  def normalize(xs: List[Double]): List[Double] = xs.map(_ / euclideanNorm)

}
