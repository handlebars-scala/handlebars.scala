package com.gilt.handlebars.scala.binding

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import scala.scalajs.js

class DynamicJsObjectBindingSpec extends AnyFunSpec with Matchers {
  describe("traverse") {
    it("traverses methods in classes") {
      class Person(val name: String) extends js.Object
      DynamicBinding(new Person("Bob")).traverse("name") should equal (DynamicBinding("Bob"))
    }

  }
}
