/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model.classification.decisiontree

import model.classification.decisiontree.DecisionTreeClassifier.{entropy, weightedEntropy}

case class Split(featureIndex: Int, threshold: Double, leftPos: Int, rightPos: Int, leftNum: Int, rightNum: Int) {

  val numPos: Int = leftPos + rightPos
  val num: Double = leftNum + rightNum
  val leftYHat: Double = 1.0 * leftPos / leftNum
  val rightYHat: Double = 1.0 * rightPos / rightNum
  val yHat: Double = 1.0 * numPos / num
  private val leftEntropy: Double = entropy(leftYHat)
  private val rightEntropy: Double = entropy(rightYHat)
  private val splitEntropy: Double = weightedEntropy(leftPos, rightPos, leftEntropy, rightEntropy)

  def getLeftEntropy: Double = {
    if (leftPos == leftNum) { 0.0 }  // perfect split -> no entropy
    else { leftEntropy }
  }

  def getRightEntropy: Double = {
    if (rightPos == rightNum) { 0.0 }  // perfect split -> no entropy
    else { rightEntropy }
  }

  def getEntropy: Double = {
    if (leftPos == leftNum || rightPos == rightNum) { 0.0 }  // perfect split -> no entropy
    else { splitEntropy }
  }
}