package com.gilt.handlebars.scala.binding

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.scalajs.js

class DynamicJsObjectBindingSpec extends AnyFunSpec with Matchers {
  describe("traverse") {
    it("traverses methods in classes") {
      class Person(val name: String, val age: Int) extends js.Object {
        def theAge: Int = age
        def theAgeParens(): Int = age
        def ageTimes(mult: Int) = age * mult
      }
      val b = DynamicBinding(new Person("Bob", 42))
      b.traverse("name") should equal(DynamicBinding("Bob"))
      b.traverse("age") should equal(DynamicBinding(42))
      b.traverse("theAge") should equal(DynamicBinding(42))
      b.traverse("theAgeParens") should equal(DynamicBinding(42))
      b.traverse("ageTimes", Seq(DynamicBinding(2))) should equal(DynamicBinding(84))
    }

  }
}
