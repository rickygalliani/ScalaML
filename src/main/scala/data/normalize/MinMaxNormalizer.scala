/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package data.normalize

class MinMaxNormalizer(val minValue: Double = 0.0, val maxValue: Double = 1.0) extends Normalizer {

  // Store first seen min and max as baseline parameters to normalize future vectors
  var minSeen: Option[Double] = None
  var maxSeen: Option[Double] = None

  def normalize(xs: List[Double]): List[Double] = {
    val curMin = xs.min
    val curMax = xs.max
    minSeen = Some(this.minSeen.getOrElse(curMin))
    maxSeen = Some(this.maxSeen.getOrElse(curMax))
    xs.map(x => ((x - minSeen.get) * (maxValue - minValue)) / (maxSeen.get - minSeen.get))
  }

}
