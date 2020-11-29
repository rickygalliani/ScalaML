/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package data.normalize

class MeanNormalizer(override val xs: List[Double]) extends Normalizer(xs) {

  val minX: Double = xs.min
  val maxX: Double = xs.max
  val meanX: Double = 1.0 * xs.sum / xs.size

  def normalize(newXs: List[Double]): List[Double] = xs.map(x => (x - meanX) / (maxX - minX))

}
