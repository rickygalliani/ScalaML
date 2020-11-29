/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package data.normalize

class MeanNormalizer() extends Normalizer {
  def normalize(xs: List[Double]): List[Double] = {
    val meanX = 1.0 * xs.sum / xs.size
    val minX = xs.min
    val maxX = xs.max
    xs.map(x => (x - meanX) / (maxX - minX))
  }
}
