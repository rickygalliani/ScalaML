/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package data.normalize

class MinMaxNormalizer(val minValue: Double = 0.0, val maxValue: Double = 1.0) extends Normalizer {

  // Store first seen min and max as baseline parameters to normalize future vectors
  var minSeen: Map[Int, Double] = Map[Int, Double]()
  var maxSeen: Map[Int, Double] = Map[Int, Double]()

  def normalize(index: Int, xs: List[Double]): List[Double] = {
    val curMin = xs.min
    val curMax = xs.max
    if (!minSeen.contains(index)) { minSeen += (index -> curMin) }
    if (!maxSeen.contains(index)) { maxSeen += (index -> curMax) }
    xs.map(x => ((x - minSeen(index)) * (maxValue - minValue)) / (maxSeen(index) - minSeen(index)))
  }

}
