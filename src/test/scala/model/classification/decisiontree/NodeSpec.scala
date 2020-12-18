/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model.classification.decisiontree

import org.scalatest.funsuite.AnyFunSuite

class NodeSpec extends AnyFunSuite {

  test("updateLeftChild: case 1") {
    val n = new NonLeafNode(1, None, None, None)
    val lc = new NonLeafNode(2, Option(n), None, None)
    n.updateLeftChild(lc)
    assert(n.leftChild.nonEmpty)
    assert(n.leftChild.get.depth == 2)
  }

  test("updateRightChild: case 1") {
    val n = new NonLeafNode(depth = 3, None, None, None)
    val depth = 4
    val rc = new NonLeafNode(depth = depth, Option(n), None, None)
    n.updateRightChild(rc)
    assert(n.rightChild.nonEmpty)
    assert(n.rightChild.get.depth == 4)
  }

  test("isLeftChild()/isRightChild(): case 1") {
    val root = new NonLeafNode(1, parent = None, leftChild = None, rightChild = None, None)
    val lc = new LeafNode(depth = 2, parent = Option(root), terminalValue = Option(0.5))
    val rc = new LeafNode(depth = 2, parent = Option(root), terminalValue = Option(0.5))
    root.leftChild = Option(lc)
    root.rightChild = Option(rc)
    assert(lc.isLeftChild)
    assert(!lc.isRightChild)
    assert(!rc.isLeftChild)
    assert(rc.isRightChild)
  }

}
