/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package data.normalize

class ZScoreNormalizer(override val xs: List[Double]) extends Normalizer(xs) {

  val meanX: Double = 1.0 * xs.sum / xs.size
  val stdDevX: Double = math.sqrt(xs.map(x => math.pow(x - meanX, 2)).sum / xs.size)

  def normalize(xs: List[Double]): List[Double] = xs.map(x => (x - meanX) / stdDevX)

}
