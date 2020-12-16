/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model.decisiontree

abstract class Node(val depth: Int, var leftChild: Option[Node], var rightChild: Option[Node]) {
  def isLeaf: Boolean
  def evaluate(X: List[Double]): Double
}


class NonLeafNode(override val depth: Int,
                  override var leftChild: Option[Node] = None,
                  override var rightChild: Option[Node] = None,
                  var featureIndex: Option[Int] = None,
                  var threshold: Option[Double] = None) extends Node(depth) {

  def isLeaf: Boolean = false

  def evaluate(X: List[Double]): Double = {
    assert(X.indices.contains(featureIndex.get))
    if (X(featureIndex.get) >= threshold.get) 1.0 else 0.0
  }

}

class LeafNode(override val depth: Int, terminalValue: Double) extends Node(depth) {

  override var leftChild: Option[Node] = None
  override var rightChild: Option[Node] = None

  def isLeaf: Boolean = true

  def evaluate(X: List[Double]): Double = terminalValue

}
