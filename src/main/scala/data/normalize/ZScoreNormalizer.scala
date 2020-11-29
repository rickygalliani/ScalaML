/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package data.normalize

class ZScoreNormalizer() extends Normalizer {
  def normalize(xs: List[Double]): List[Double] = {
    val meanX = 1.0 * xs.sum / xs.size
    val stdDevX = math.sqrt(xs.map(x => math.pow(x - meanX, 2)).sum / xs.size)
    xs.map(x => (x - meanX) / stdDevX)
  }
}
