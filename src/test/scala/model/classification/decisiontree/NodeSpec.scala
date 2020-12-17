/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model.classification.decisiontree

import org.scalatest.funsuite.AnyFunSuite

class NodeSpec extends AnyFunSuite {

  test("leftChild update") {
    val n = new NonLeafNode(1, None, None, None)
    val lc = new NonLeafNode(2, Option(n), None, None)
    n.leftChild = Option(lc)
    assert(n.leftChild.nonEmpty)
    assert(n.leftChild.get.depth == 2)
  }

}
