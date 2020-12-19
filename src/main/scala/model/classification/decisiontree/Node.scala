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
  def isLeftChild: Boolean = {
    parent match {
      case Some(p) => p.leftChild match {
        case Some(lc) => lc == this
        case None => false
      }
      case None => false
    }
  }
  def isRightChild: Boolean = {
    parent match {
      case Some(p) => p.rightChild match {
        case Some(rc) => rc == this
        case None => false
      }
      case None => false
    }
  }
  def updateLeftChild(leftChild: Node): Unit = this.leftChild = Option(leftChild)
  def updateRightChild(rightChild: Node): Unit = this.rightChild = Option(rightChild)

}

class NonLeafNode(override val depth: Int,
                  override val parent: Option[NonLeafNode],
                  leftChild: Option[Node] = None,
                  rightChild: Option[Node] = None,
                  var split: Option[Split] = None) extends Node(depth, parent, leftChild, rightChild) {

  def isLeaf: Boolean = false

  def evaluate(X: List[Double]): Double = {
    val featureIndex = split.get.featureIndex
    if (!X.indices.contains(featureIndex)) throw new Exception(s"Unrecognized feature index $featureIndex")
    if (X(featureIndex) >= split.get.threshold) 1.0 else 0.0
  }

  def getChildren(leftYHat: Double, rightYHat: Double): (LeafNode, LeafNode) = {
    // Create new children nodes for current node
    val leftChild = new LeafNode(depth + 1, Option(this), Option(leftYHat))
    val rightChild = new LeafNode(depth + 1, Option(this), Option(rightYHat))
    // Point this Node to new children
    this.updateLeftChild(leftChild)
    this.updateRightChild(rightChild)
    (leftChild, rightChild)
  }

}

class RootNode extends NonLeafNode(depth = 1, parent = None, leftChild = None, rightChild = None)

class LeafNode(override val depth: Int,
               override val parent: Option[NonLeafNode],
               val terminalValue: Option[Double] = None)
  extends Node(depth, parent, leftChild = None, rightChild = None) {

  // Make sure parent points to this Node
  if (this.isLeftChild) { parent.foreach(p => p.updateLeftChild(this)) }
  else if (this.isRightChild) { parent.foreach(p => p.updateRightChild(this)) }

  def isLeaf: Boolean = true

  def evaluate(X: List[Double]): Double = {
    if (terminalValue.isEmpty) { throw new Exception("Undefined terminal value for leaf node at inference time") }
    else { terminalValue.get }
  }

}
