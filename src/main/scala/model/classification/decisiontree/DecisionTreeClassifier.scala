/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model.classification.decisiontree

import data.normalize.{MinMaxNormalizer, Normalizer}
import example.BinaryClassificationExample
import model.BinaryClassificationModel
import model.classification.decisiontree.DecisionTreeClassifier.getSplit
import org.apache.logging.log4j.Level

import scala.collection.mutable
import scala.util.Random

class DecisionTreeClassifier(val maxDepth: Int = MaxDepth,
                             override val verbose: Boolean = false) extends BinaryClassificationModel {

  private val random = new Random
  val dtc: Level = Level.forName("decisiontreeclassifier", LogLevelSeed)

  var root: Node = new LeafNode(1, None)

  override val normalizer: Option[Normalizer] = Some(new MinMaxNormalizer())

  def shouldSplitAgain(node: Node, bestSplit: Split, features: List[Int]): Boolean = {
    val someIncorrect = bestSplit.entropy > EqualityDelta
    val numFeaturesLeft = features.toSet.diff(Set(bestSplit.featureIndex)).size
    val featuresLeft = numFeaturesLeft > 0
    val entropyDrop = node.parent match {
      case Some(p) => p.split.get.entropy - bestSplit.entropy
      case None => EntropyEpsilon + 0.1
    }
    val entropyDropStillBig = true // entropyDrop >= EntropyEpsilon
    val aboveMaxDepth = node.depth < MaxDepth
    val shouldSplitAgain = Seq(someIncorrect, featuresLeft, entropyDropStillBig, aboveMaxDepth).forall(identity)
    if (!shouldSplitAgain && verbose) {
      logger(dtc,
        s"""\n\nStopping condition checks:
          |\t1) All examples are correct: ${!someIncorrect}
          |\t2) No features left to split on: ${!featuresLeft} (features left: $numFeaturesLeft)
          |\t3) Entropy drop below epsilon ($EntropyEpsilon): ${!entropyDropStillBig} ($entropyDrop)
          |\t4) Below max depth ($MaxDepth): ${!aboveMaxDepth} (node depth: ${node.depth})\n""".stripMargin)
    }
    shouldSplitAgain
  }

  // Implements the ID3 learning algorithm
  override def learn(examples: List[BinaryClassificationExample]): Unit = {
    random.setSeed(TrainSeed)
    random.shuffle(examples)
    val featureIndices: List[Int] = examples.head.X.indices.toList
    val nodes: mutable.Queue[Node] = mutable.Queue[Node](root)
    while(nodes.nonEmpty) {
      val node = nodes.dequeue
      val features = node.unusedFeatures(featureIndices) // Only use features not used in parent nodes
      // Consider each feature/threshold combination
      val featureThresholds = for (f <- features; t <- Thresholds) yield (f, t)
      var bestSplit = getSplit(examples, featureThresholds.head._1, featureThresholds.head._2)
      featureThresholds.tail.foreach { case (feature, threshold) =>
        // TODO(ricky): need to filter for only examples that get to this node!
        val split = getSplit(examples, feature, threshold)
        if (split.entropy < bestSplit.entropy) bestSplit = split
      }
      if (verbose) {
        logger(dtc,
          s"""\n\nAdded node to decision tree:
          |\t- depth: ${node.depth}
          |\t- feature: ${bestSplit.featureIndex}
          |\t- threshold: ${bestSplit.threshold}
          |\t- leftPos: ${bestSplit.leftPos}
          |\t- leftSize: ${bestSplit.leftSize}
          |\t- rightPos: ${bestSplit.rightPos}
          |\t- rightSize: ${bestSplit.rightSize}
          |\t- entropy: ${bestSplit.entropy}\n""".stripMargin)
      }
      if (shouldSplitAgain(node, bestSplit, features)) {
        // Change current node from leaf node to non-leaf node
        val nonLeafNode = new NonLeafNode(node.depth, node.parent, None, None, Option(bestSplit))
        // Update parent to point to the new non-leaf
        if (node.isLeftChild) { node.parent.get.updateLeftChild(nonLeafNode) }
        else if (node.isRightChild) { node.parent.get.updateRightChild(nonLeafNode) }
        // Create new children nodes for current node
        val leftChild = new LeafNode(nonLeafNode.depth + 1, Option(nonLeafNode), Option(bestSplit.leftYHat))
        val rightChild = new LeafNode(nonLeafNode.depth + 1, Option(nonLeafNode), Option(bestSplit.rightYHat))
        nonLeafNode.updateLeftChild(leftChild)
        nonLeafNode.updateRightChild(rightChild)
        nodes.addAll(Seq(leftChild, rightChild))
        if (node == root) { root = nonLeafNode }
      }
    }
  }

  override def inference(X: List[Double]): Double = {
    var cur: Node = root
    while (!cur.isLeaf) {
      cur = cur match {
        case c: NonLeafNode => if (cur.evaluate(X) < EqualityDelta) c.leftChild.get else c.rightChild.get
        case l: LeafNode => l
      }
    }
    cur.evaluate(X)
  }

}

object DecisionTreeClassifier {

  def getSplit(examples: List[BinaryClassificationExample], featureIndex: Int, threshold: Double): Split = {
    var (leftPos, leftSize, rightPos, rightSize) = (0, 0, 0, 0)
    examples.foreach { example =>
      if (example.X(featureIndex) >= threshold) {
        rightPos += example.y.toInt
        rightSize += 1
      } else {
        leftPos += example.y.toInt
        leftSize += 1
      }
    }
    Split(featureIndex, threshold, leftPos, rightPos, leftSize, rightSize)
  }

  def entropy(yHat: Double): Double = {
    if (yHat <= EqualityDelta || math.abs(yHat - 1.0) <= EqualityDelta) { 0.0 }
    else { -1.0 * yHat * math.log(yHat) - (1 - yHat) * math.log(1 - yHat) }
  }

  def weightedEntropy(leftSize: Int, rightSize: Int, leftEntropy: Double, rightEntropy: Double): Double = {
    val size = leftSize + rightSize
    (1.0 * leftSize) / size * leftEntropy + (1.0 * rightSize) / size * rightEntropy
  }

}
