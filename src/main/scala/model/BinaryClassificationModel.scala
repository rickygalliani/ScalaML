/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model

import data.normalize.{Normalizer, MinMaxNormalizer}
import example.{BinaryClassificationExample, Example, UnitBinaryClassificationExample}

abstract class BinaryClassificationModel(override val verbose: Boolean = false)
  extends Model[BinaryClassificationExample] {

  override val normalizer: Option[Normalizer] = None

  def cast(examples: List[Example]): List[BinaryClassificationExample] = {
    examples.map(e => BinaryClassificationExample(e))
  }

  override protected def learn(examples: List[BinaryClassificationExample]): Unit

}

abstract class UnitBinaryClassificationModel extends Model[UnitBinaryClassificationExample] {

  override val normalizer: Option[Normalizer] = None

  override def cast(examples: List[Example]): List[UnitBinaryClassificationExample] = {
    examples.map(e => UnitBinaryClassificationExample(e))
  }

  override protected def learn(examples: List[UnitBinaryClassificationExample]): Unit

}

