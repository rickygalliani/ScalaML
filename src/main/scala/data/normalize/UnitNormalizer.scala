/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package data.normalize

class UnitNormalizer(val minValue: Double = 0.0, val maxValue: Double = 1.0) extends Normalizer {

  // Store first computed euclidean norm as baseline parameters to normalize future vectors
  var euclideanNorm: Map[Int, Double] = Map[Int, Double]()

  def normalize(index: Int, xs: List[Double]): List[Double] = {
    val curEuclideanNorm = math.sqrt(xs.map(x => math.pow(x, 2)).sum)
    if (!euclideanNorm.contains(index)) euclideanNorm += (index -> curEuclideanNorm)
    xs.map(_ / euclideanNorm(index))
  }

}
