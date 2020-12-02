/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package data.normalize

class MeanNormalizer extends Normalizer {

  // Store first seen min, max, and mean as baseline parameters to normalize future vectors
  var minX: Map[Int, Double] = Map[Int, Double]()
  var maxX: Map[Int, Double] = Map[Int, Double]()
  var meanX: Map[Int, Double] = Map[Int, Double]()

  def normalize(index: Int, xs: List[Double]): List[Double] = {
    val curMin = xs.min
    val curMax = xs.max
    val curMean = 1.0 * xs.sum / xs.size
    if (!minX.contains(index)) minX += (index -> curMin)
    if (!maxX.contains(index)) maxX += (index -> curMax)
    if (!meanX.contains(index)) meanX += (index -> curMean)
    xs.map(x => (x - meanX(index)) / (maxX(index) - minX(index)))
  }

}
