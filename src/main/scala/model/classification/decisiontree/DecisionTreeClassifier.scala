/**
 * Copyright (C) 2020-2021. Ricky Galliani. All Rights Reserved.
 * Email: pjgalliani@gmail.com
 */

package model.classification.decisiontree

import data.normalize.{MinMaxNormalizer, Normalizer}
import example.BinaryClassificationExample
import model.BinaryClassificationModel
import org.apache.logging.log4j.Level

import scala.collection.mutable
import scala.util.Random

class DecisionTreeClassifier(val maxDepth: Int = MaxDepth,
                             val entropyDropEpsilon: Double = EntropyDropEpsilon,
                             override val verbose: Boolean = false) extends BinaryClassificationModel {

  private val random = new Random
  val dtc: Level = Level.forName("decisiontreeclassifier", LogLevelSeed)

  var root: Node = new LeafNode(1, None)

  override val normalizer: Option[Normalizer] = Some(new MinMaxNormalizer())

  def shouldSplitAgain(node: Node, bestSplit: Split, featureIndices: List[Int]): Boolean = {
    val someIncorrect = bestSplit.entropy > EqualityDelta
    val numFeaturesLeft = featureIndices.size
    val featuresLeft = numFeaturesLeft > 1  // will remove current feature we split again
    val entropyDrop = node.parent match {
      case Some(p) => p.split.get.entropy - bestSplit.entropy
      case None => entropyDropEpsilon + 0.1
    }
    val entropyDropStillBig = true // entropyDrop >= EntropyEpsilon
    val aboveMaxDepth = node.depth < MaxDepth
    val shouldSplitAgain = Seq(someIncorrect, featuresLeft, entropyDropStillBig, aboveMaxDepth).forall(identity)
    if (!shouldSplitAgain && verbose) {
      logSplitDecision(
        someIncorrect,
        numFeaturesLeft,
        featuresLeft,
        entropyDrop,
        entropyDropStillBig,
        node.depth,
        aboveMaxDepth
      )
    }
    shouldSplitAgain
  }

  // Implements the ID3 learning algorithm
  override def learn(examples: List[BinaryClassificationExample]): Unit = {
    random.setSeed(TrainSeed)
    random.shuffle(examples)
    val featureIndices: List[Int] = examples.head.X.indices.toList
    val trainNodes = mutable.Queue(TrainNode(root, examples, featureIndices))
    while(trainNodes.nonEmpty) {
      val trainNode = trainNodes.dequeue
      // Consider each feature/threshold combination
      val featureThresholds = for (f <- trainNode.featureIndices; t <- Thresholds) yield (f, t)
      var bestSplit = Split(trainNode.examples, featureThresholds.head._1, featureThresholds.head._2)
      featureThresholds.tail.foreach { case (feature, threshold) =>
        val split = Split(trainNode.examples, feature, threshold)
        if (split.entropy < bestSplit.entropy) bestSplit = split
      }
      if (verbose) { logNode(trainNode, bestSplit) }
      if (shouldSplitAgain(trainNode.node, bestSplit, trainNode.featureIndices)) {
        // Change current node from leaf node to non-leaf node
        val nonLeafNode = new NonLeafNode(trainNode.node.depth, trainNode.node.parent, None, None, Option(bestSplit))
        // Update parent to point to the new non-leaf version of this node
        if (trainNode.node.isLeftChild) { trainNode.node.parent.get.updateLeftChild(nonLeafNode) }
        else if (trainNode.node.isRightChild) { trainNode.node.parent.get.updateRightChild(nonLeafNode) }
        // Create new children nodes for current node
        val leftChild = new LeafNode(nonLeafNode.depth + 1, Option(nonLeafNode), Option(bestSplit.leftYHat))
        val rightChild = new LeafNode(nonLeafNode.depth + 1, Option(nonLeafNode), Option(bestSplit.rightYHat))
        // Point this node to new children
        nonLeafNode.updateLeftChild(leftChild)
        nonLeafNode.updateRightChild(rightChild)
        // Update the root node of the tree, if necessary
        if (trainNode.node == root) { root = nonLeafNode }
        // Eliminate current feature from set of features for downstream nodes
        val downstreamFeatures = trainNode.featureIndices.toSet.diff(Set(bestSplit.featureIndex)).toList
        val leftTrainNode = TrainNode(leftChild, bestSplit.leftExamples, downstreamFeatures)
        val rightTrainNode = TrainNode(rightChild, bestSplit.rightExamples, downstreamFeatures)
        trainNodes.addAll(Seq(leftTrainNode, rightTrainNode))
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

  def logNode(trainNode: TrainNode, split: Split): Unit = {
    logger(dtc,
      s"""\n\nAdded node to decision tree:
         |\t- depth: ${trainNode.node.depth}
         |\t- features left: ${trainNode.featureIndices.size}
         |\t- feature: ${split.featureIndex}
         |\t- threshold: ${split.threshold}
         |\t- leftPositives: ${split.leftPositives}
         |\t- leftSize: ${split.leftSize}
         |\t- rightPositives: ${split.rightPositives}
         |\t- rightSize: ${split.rightSize}
         |\t- entropy: ${split.entropy}\n""".stripMargin)
  }

  def logSplitDecision(someIncorrect: Boolean,
                       numFeaturesLeft: Int,
                       featuresLeft: Boolean,
                       entropyDrop: Double,
                       entropyDropStillBig: Boolean,
                       depth: Int,
                       aboveMaxDepth: Boolean): Unit = {
    logger(dtc,
      s"""\n\nStopping condition checks:
         |\t1) All examples are correct: ${!someIncorrect}
         |\t2) No features left to split on: ${!featuresLeft} (features left: $numFeaturesLeft)
         |\t3) Entropy drop below epsilon ($entropyDropEpsilon): ${!entropyDropStillBig} ($entropyDrop)
         |\t4) Below max depth ($MaxDepth): ${!aboveMaxDepth} (node depth: $depth)\n""".stripMargin)
  }

}

case class TrainNode(node: Node, examples: List[BinaryClassificationExample], featureIndices: List[Int])
