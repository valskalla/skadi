package io.skadi

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{Checkers, ScalaCheckDrivenPropertyChecks}
import org.typelevel.discipline.Laws

class SkadiSpec extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with Instances with Checkers with Matchers {

  def checkAll(name: String, ruleSet: Laws#RuleSet): Unit =
    ruleSet.props.foreach {
      case (law, property) =>
        test(s"${ruleSet.name}[$name]: $law") {
          check(property)
        }
    }

}
