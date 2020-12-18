/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model.classification.decisiontree

abstract class Node(val depth: Int,
                    val parent: Option[NonLeafNode],
                    var leftChild: Option[Node],
                    var rightChild: Option[Node]) {
  def isLeaf: Boolean
  def evaluate(X: List[Double]): Double  // >= / < threshold (1.0 / 0.0) or terminal value
  def isLeftChild: Boolean = parent.exists(p => p.leftChild.get == this)
  def isRightChild: Boolean = parent.exists(p => p.rightChild.get == this)
  def updateLeftChild(leftChild: Node): Unit = this.leftChild = Option(leftChild)
  def updateRightChild(rightChild: Node): Unit = this.rightChild = Option(rightChild)

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

  def evaluate(X: List[Double]): Double = {
    if (terminalValue.isEmpty) { throw new Exception("Undefined terminal value for leaf node at inference time") }
    else { terminalValue.get }
  }

}
