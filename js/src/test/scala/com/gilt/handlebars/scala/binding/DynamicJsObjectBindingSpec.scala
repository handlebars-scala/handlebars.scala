package com.gilt.handlebars.scala.binding

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.scalajs.js

class DynamicJsObjectBindingSpec extends AnyFunSpec with Matchers {
  describe("traverse") {
    it("traverses methods in classes") {
      class Person(val name: String, val age: Int) extends js.Object
      val b = DynamicBinding(new Person("Bob", 42))
      b.traverse("name") should equal(DynamicBinding("Bob"))
      b.traverse("age") should equal(DynamicBinding(42))    }

  }
}
