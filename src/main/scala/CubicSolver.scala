import scala.collection.mutable
import scala.language.implicitConversions

class CubicSolver {
  private case class Node(sol: mutable.Set[String] = mutable.Set(),
                          succ: mutable.Set[String] = mutable.Set(),
                          cond: mutable.Map[String, mutable.Set[(String, String)]] = mutable.Map().withDefaultValue(mutable.Set()))
  private val nodes: mutable.Map[String, Node] = mutable.Map()
  private val W: mutable.Queue[(String, Node)] = mutable.Queue()

  private implicit def stringToNode(x: String): Node = {
    nodes.getOrElseUpdate(x, Node())
  }

  private def addToken(t: String, x: String): Unit = {
    if (x.sol.add(t))
      W += ((t, x))
  }

  private def addEdge(x: String, y: String): Unit = {
    if (x != y && x.succ.add(y))
      x.sol.foreach(t => addToken(t, y))
  }

  private def propagate(): Unit = {
    while (W.nonEmpty) {
      val (t, x) = W.dequeue()
      for ((y, z) <- x.cond(t))
        addEdge(y, z)
      for (y <- x.succ)
        addToken(t, y)
    }
  }

  def addConstantConstraint(t: String, x: String): Unit = {
    addToken(t, x)
    propagate()
  }

  def addConditionalConstraint(t: String, x: String, y: String, z: String): Unit = {
    if (x.sol.contains(t)) {
      addEdge(y, z)
      propagate()
    } else {
      x.cond(t).add(y, z)
    }
  }

  def printNodes(): Unit = {
    println("-----------------------------------------")
    nodes.foreachEntry((k, v) => {
      println(s"$k.sol = ${v.sol}")
      println(s"$k.succ = ${v.succ}")
      println(s"$k.cond = ${v.cond}")
      println()
    })
    println("-----------------------------------------")
  }

  def printSolution(): Unit = {
    nodes.foreachEntry((v, n) => println(s"$v = ${n.sol}"))
  }
}
