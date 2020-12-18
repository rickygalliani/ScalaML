/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model.classification.decisiontree

import example.BinaryClassificationExample

case class Split(featureIndex: Int,
                 threshold: Double,
                 leftExamples: List[BinaryClassificationExample],
                 rightExamples: List[BinaryClassificationExample],
                 leftPos: Int,
                 rightPos: Int,
                 leftSize: Int,
                 rightSize: Int) {

  val numPos: Int = leftPos + rightPos
  val size: Int = leftSize + rightSize
  val leftYHat: Double = if (leftSize < EqualityDelta) 0.0 else (1.0 * leftPos) / leftSize
  val rightYHat: Double = if (rightSize < EqualityDelta) 0.0 else (1.0 * rightPos) / rightSize
  val yHat: Double = if (size < EqualityDelta) 0.0 else (1.0 * numPos) / size
  val leftEntropy: Double = DecisionTreeClassifier.entropy(leftYHat)
  val rightEntropy: Double = DecisionTreeClassifier.entropy(rightYHat)
  val entropy: Double = DecisionTreeClassifier.weightedEntropy(leftSize, rightSize, leftEntropy, rightEntropy)
}