/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package data.normalize

abstract class Normalizer {
  def normalize(xs: List[Double]): List[Double]
}
