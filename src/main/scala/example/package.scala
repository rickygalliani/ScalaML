/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package object example {
  // Converts {0, 1} binary label to {-1, 1} binary label
  def binaryToPosNeg(y: Int): Int = {
    if (y == 0) { -1 }
    else if (y == 1) { 1 }
    else { throw new Exception(s"Unrecognized label y in binaryToPosNeg(): $y") }
  }

  // Converts {-1, 1} binary label to {0, 1} binary label
  def posNegToBinary(y: Int): Int = {
    if (y == -1) { 0 }
    else if (y == 1) { 1 }
    else { throw new Exception(s"Unrecognized label y in posNegToBinary(): $y") }
  }

}
