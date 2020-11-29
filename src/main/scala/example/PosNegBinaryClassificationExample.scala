/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package example

case class PosNegBinaryClassificationExample(override val X: List[Double], override val y: Int)
  extends BinaryClassificationExample(X, y, positiveValue = 1, negativeValue = -1)

object PosNegBinaryClassificationExample {

  def apply(b: BinaryClassificationExample): PosNegBinaryClassificationExample = {
    new PosNegBinaryClassificationExample(b.X, binaryToPosNeg(b.y))
  }

}