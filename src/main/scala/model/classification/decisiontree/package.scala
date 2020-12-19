/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model.classification

package object decisiontree {
  val LogLevelSeed: Int = 51
  val TrainSeed: Int = 71
  val EqualityDelta: Double = 10e-6
  val MaxDepth: Int = 1
  val NumThresholds = 20
  val Thresholds: List[Double] = (1 to NumThresholds).map(x => 1.0 * x / (NumThresholds + 1)).toList
}
