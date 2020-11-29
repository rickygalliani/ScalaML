/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package data.normalize

class MinMaxNormalizer(val minValue: Double = 0.0, val maxValue: Double = 1.0) extends Normalizer {
  def normalize(xs: List[Double]): List[Double] = {
    val minX = xs.min
    val maxX = xs.max
    xs.map(x => ((x - minX) * (maxValue - minValue)) / (maxX - minX))
  }
}
