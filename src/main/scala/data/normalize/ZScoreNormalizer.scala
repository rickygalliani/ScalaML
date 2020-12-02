/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package data.normalize

class ZScoreNormalizer extends Normalizer {

  // Store first computed mean and standard deviation as baseline parameters to normalize future vectors
  var mean: Map[Int, Double] = Map[Int, Double]()
  var stdDev: Map[Int, Double] = Map[Int, Double]()

  def normalize(index: Int, xs: List[Double]): List[Double] = {
    val curMean = 1.0 * xs.sum / xs.size
    val curStdDev = math.sqrt(xs.map(x => math.pow(x - curMean, 2)).sum / xs.size)
    if (!mean.contains(index)) mean += (index -> curMean)
    if (!stdDev.contains(index)) stdDev += (index -> curStdDev)
    xs.map(x => (x - mean(index)) / stdDev(index))
  }

}
