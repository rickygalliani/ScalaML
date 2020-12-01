/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package data.normalize

class MeanNormalizer extends Normalizer {

  // Store first seen min, max, and mean as baseline parameters to normalize future vectors
  var minX: Option[Double] = None
  var maxX: Option[Double] = None
  var meanX: Option[Double] = None

  def normalize(xs: List[Double]): List[Double] = {
    val curMin = xs.min
    val curMax = xs.max
    val curMean = 1.0 * xs.sum / xs.size
    minX = Some(this.minX.getOrElse(curMin))
    maxX = Some(this.maxX.getOrElse(curMax))
    meanX = Some(this.meanX.getOrElse(curMean))
    xs.map(x => (x - meanX.get) / (this.maxX.get - this.minX.get))
  }

}
