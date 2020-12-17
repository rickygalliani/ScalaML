/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model.classification.decisiontree

import scala.collection.mutable.ListBuffer

abstract class Node(val depth: Int,
                    val parent: Option[NonLeafNode],
                    var leftChild: Option[Node],
                    var rightChild: Option[Node]) {
  def isLeaf: Boolean
  def evaluate(X: List[Double]): Double  // >= / < threshold (1.0 / 0.0) or terminal value

  /**
   * Used to determine which features this Node can split on
   * @param featureIndices features from which to choose
   * @return features not used by "parents" of this Node
   */
  def unusedFeatures(featureIndices: List[Int]): List[Int] = {
    val parentFeatures = new ListBuffer[Int]()
    var p = parent
    while (p.nonEmpty) {
      parentFeatures += p.get.split.get.featureIndex
      p = p.get.parent
    }
    featureIndices.toSet.diff(parentFeatures.toSet).toList.sorted
  }
}

class NonLeafNode(depth: Int,
                  parent: Option[NonLeafNode],
                  leftChild: Option[Node] = None,
                  rightChild: Option[Node] = None,
                  var split: Option[Split] = None) extends Node(depth, parent, leftChild, rightChild) {

  def isLeaf: Boolean = false

  def evaluate(X: List[Double]): Double = {
    assert(X.indices.contains(split.get.featureIndex))
    if (X(split.get.featureIndex) >= split.get.threshold) 1.0 else 0.0
  }

}

class LeafNode(depth: Int, parent: Option[NonLeafNode], terminalValue: Option[Double] = None)
  extends Node(depth, parent, leftChild = None, rightChild = None) {

  def isLeaf: Boolean = true

  def evaluate(X: List[Double]): Double = terminalValue.getOrElse(-1.0)

}
