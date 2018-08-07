
import org.scalacheck._
import Gen._
import Arbitrary.arbitrary


object scalaCheckGen {

  val smallInteger = Gen.choose(0,100)

  val propSmallInteger = Prop.forAll(smallInteger) { n =>
    n >= 0 && n <= 100
  }

  propSmallInteger.check

  sealed abstract class Tree
  case class Node(left: Tree, right: Tree, v: Int) extends Tree
  case object Leaf extends Tree


  val genLeaf = const(Leaf)

  val genNode = for {
    v <- arbitrary[Int]
    left <- genTree
    right <- genTree
  } yield Node(left, right, v)

  def genTree: Gen[Tree] = oneOf(genLeaf, genNode)

  val t1 = genTree.sample match {
    case Some(x) => x
  }
}

