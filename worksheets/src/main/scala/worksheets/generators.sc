

object generators {

  trait Generator[+T] {
    self =>       // an alias for ”this”.

    def generate: T

    def map[S](f: T => S): Generator[S] = new Generator[S] {
      def generate = f(self.generate)
    }

    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
      def generate = f(self.generate).generate
    }
  }

  val integers = new Generator[Int] {
    def generate = scala.util.Random.nextInt()
  }

  val i1 = integers.generate
  val i2 = integers.generate

  val booleans = integers.map(_ >= 0)
  val b1 = booleans.generate

  trait Tree
  case class Inner(left: Tree, right: Tree) extends Tree
  case class Leaf(x: Int) extends Tree

  def leafs : Generator[Leaf] = integers.map(Leaf)
  val l1 = leafs.generate
  val l2 = leafs.generate

  def inners: Generator[Inner] = for {
    l <- trees
    r <- trees
  } yield Inner(l, r)

  def trees: Generator[Tree] = booleans.generate match {
    case true => integers.map(Leaf)
    case false => inners
  }
  val t1 = trees.generate
}