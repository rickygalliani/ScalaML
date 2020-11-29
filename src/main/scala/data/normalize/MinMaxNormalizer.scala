/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package data.normalize

class MinMaxNormalizer(override val xs: List[Double],
                       val minValue: Double = 0.0,
                       val maxValue: Double = 1.0) extends Normalizer(xs) {

  val minX: Double = xs.min
  val maxX: Double = xs.max

  def normalize(newXs: List[Double]): List[Double] = xs.map(x => ((x - minX) * (maxValue - minValue)) / (maxX - minX))

}
