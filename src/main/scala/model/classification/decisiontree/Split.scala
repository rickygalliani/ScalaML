/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model.classification.decisiontree

import example.BinaryClassificationExample

import scala.collection.mutable.ListBuffer

case class Split(featureIndex: Int,
                 threshold: Double,
                 leftExamples: List[BinaryClassificationExample],
                 rightExamples: List[BinaryClassificationExample],
                 leftPositives: Int,
                 rightPositives: Int,
                 leftSize: Int,
                 rightSize: Int) {

  val numPositives: Int = leftPositives + rightPositives
  val size: Int = leftSize + rightSize
  val leftYHat: Double = if (leftSize < EqualityDelta) 0.0 else (1.0 * leftPositives) / leftSize
  val rightYHat: Double = if (rightSize < EqualityDelta) 0.0 else (1.0 * rightPositives) / rightSize
  val yHat: Double = if (size < EqualityDelta) 0.0 else (1.0 * numPositives) / size
  val leftEntropy: Double = Split.entropy(leftYHat)
  val rightEntropy: Double = Split.entropy(rightYHat)
  val entropy: Double = Split.weightedEntropy(leftSize, rightSize, leftEntropy, rightEntropy)

}

object Split {

  def apply(examples: List[BinaryClassificationExample], featureIndex: Int, threshold: Double): Split = {
    var leftExamples = new ListBuffer[BinaryClassificationExample]
    var rightExamples = new ListBuffer[BinaryClassificationExample]
    var (leftPositives, leftSize, rightPositives, rightSize) = (0, 0, 0, 0)
    examples.foreach { example =>
      if (example.X(featureIndex) >= threshold) {
        leftExamples += example
        rightPositives += example.y.toInt
        rightSize += 1
      } else {
        rightExamples += example
        leftPositives += example.y.toInt
        leftSize += 1
      }
    }
    Split(featureIndex, threshold, leftExamples.toList, rightExamples.toList, leftPositives, rightPositives, leftSize, rightSize)
  }

  def entropy(yHat: Double): Double = {
    if (yHat <= EqualityDelta || math.abs(yHat - 1.0) <= EqualityDelta) { 0.0 }
    else { -1.0 * yHat * math.log(yHat) - (1 - yHat) * math.log(1 - yHat) }
  }

  def weightedEntropy(leftSize: Int, rightSize: Int, leftEntropy: Double, rightEntropy: Double): Double = {
    val size = leftSize + rightSize
    (1.0 * leftSize) / size * leftEntropy + (1.0 * rightSize) / size * rightEntropy
  }

}