import scala.collection.mutable

object Main {
  def main(args: Array[String]): Unit = example1()

  private def example1(): Unit = {
    val cs = new CubicSolver

    cs.addConstantConstraint("s", "s z sz")
    cs.addConstantConstraint("z", "z sz")
    cs.addConstantConstraint("n", "n t e e")
    cs.addConstantConstraint("t", "t e e")
    cs.addConstantConstraint("e", "e e")
    cs.addConstantConstraint("r", "r p r")
    cs.addConstantConstraint("p", "p r")

    cs.addConditionalConstraint("s", "s z sz", "n t e e", "s")
    cs.addConditionalConstraint("s", "s z sz", "z sz", "(s z sz)(n t e e)")
    cs.addConditionalConstraint("z", "s z sz", "n t e e", "z")
    cs.addConditionalConstraint("z", "s z sz", "sz", "(s z sz)(n t e e)")
    cs.addConditionalConstraint("n", "s z sz", "n t e e", "n")
    cs.addConditionalConstraint("n", "s z sz", "t e e", "(s z sz)(n t e e)")
    cs.addConditionalConstraint("t", "s z sz", "n t e e", "t")
    cs.addConditionalConstraint("t", "s z sz", "e e", "(s z sz)(n t e e)")
    cs.addConditionalConstraint("e", "s z sz", "n t e e", "e")
    cs.addConditionalConstraint("e", "s z sz", "e", "(s z sz)(n t e e")
    cs.addConditionalConstraint("r", "s z sz", "n t e e", "r")
    cs.addConditionalConstraint("r", "s z sz", "p r", "(s z sz)(n t e e)")
    cs.addConditionalConstraint("p", "s z sz", "n t e e", "p")
    cs.addConditionalConstraint("p", "s z sz", "r", "(s z sz)(n t e e)")
    cs.addConditionalConstraint("s", "s", "z", "s")
    cs.addConditionalConstraint("s", "s", "z sz", "sz")
    cs.addConditionalConstraint("z", "s", "z", "z")
    cs.addConditionalConstraint("z", "s", "sz", "sz")
    cs.addConditionalConstraint("n", "s", "z", "n")
    cs.addConditionalConstraint("n", "s", "t e e", "s z")
    cs.addConditionalConstraint("t", "s", "z", "t")
    cs.addConditionalConstraint("t", "s", "e e", "sz")
    cs.addConditionalConstraint("e", "s", "z", "e")
    cs.addConditionalConstraint("e", "s", "e", "sz")
    cs.addConditionalConstraint("r", "s", "z", "r")
    cs.addConditionalConstraint("r", "s", "p r", "sz")
    cs.addConditionalConstraint("p", "s", "z", "p")
    cs.addConditionalConstraint("p", "s", "r", "sz")
    cs.addConditionalConstraint("s", "(s z sz)(n t e e)", "r p r", "s")
    cs.addConditionalConstraint("s", "(s z sz)(n t e e)", "z sz", "(s z sz)(n t e e)(r p r)")
    cs.addConditionalConstraint("z", "(s z sz)(n t e e)", "r p r", "z")
    cs.addConditionalConstraint("z", "(s z sz)(n t e e)", "sz", "(s z sz)(n t e e)(r p r)")
    cs.addConditionalConstraint("n", "(s z sz)(n t e e)", "r p r", "n")
    cs.addConditionalConstraint("n", "(s z sz)(n t e e)", "t e e", "(s z sz)(n t e e)(r p r)")
    cs.addConditionalConstraint("t", "(s z sz)(n t e e)", "r p r", "t")
    cs.addConditionalConstraint("t", "(s z sz)(n t e e)", "e e", "(s z sz)(n t e e)(r p r)")
    cs.addConditionalConstraint("e", "(s z sz)(n t e e)", "r p r", "e")
    cs.addConditionalConstraint("e", "(s z sz)(n t e e)", "e", "(s z sz)(n t e e)(r p r)")
    cs.addConditionalConstraint("r", "(s z sz)(n t e e)", "r p r", "r")
    cs.addConditionalConstraint("r", "(s z sz)(n t e e)", "p r", "(s z sz)(n t e e)(r p r)")
    cs.addConditionalConstraint("p", "(s z sz)(n t e e)", "r p r", "p")
    cs.addConditionalConstraint("p", "(s z sz)(n t e e)", "r", "(s z sz)(n t e e)(r p r)")

    cs.printSolution()
  }
}
