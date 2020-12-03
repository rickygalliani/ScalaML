/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package example

case class UnitBinaryClassificationExample(override val X: List[Double], override val y: Double)
  extends BinaryClassificationExample(X, y, positiveValue = 1, negativeValue = -1)

object UnitBinaryClassificationExample {

  def apply(e: Example): UnitBinaryClassificationExample = {
    new UnitBinaryClassificationExample(e.X, e.y)
  }

}