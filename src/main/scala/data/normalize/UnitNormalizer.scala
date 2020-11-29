/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package data.normalize

class UnitNormalizer(val minValue: Double = 0.0, val maxValue: Double = 1.0) extends Normalizer {
  def normalize(xs: List[Double]): List[Double] = {
    val euclideanNorm = math.sqrt(xs.map(x => math.pow(x, 2)).sum)
    xs.map(x => x / euclideanNorm)
  }
}
