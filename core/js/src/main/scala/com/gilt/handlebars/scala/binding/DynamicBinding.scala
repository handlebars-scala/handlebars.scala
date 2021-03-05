package com.gilt.handlebars.scala.binding

import com.gilt.handlebars.scala.logging.Loggable

import scala.scalajs.js
import scala.scalajs.js.Object.getOwnPropertyNames
import scala.util.Try

class DynamicBinding(val data: Any) extends FullBinding[Any] with Loggable {
  override protected def factory = DynamicBinding

  def render = if (isTruthy) data.toString else ""

  def isTruthy = data match {
    case /* UndefinedValue |*/ None | () | Nil | null | false => false
    case _: scala.runtime.BoxedUnit => false
    case _ => true
  }

  override def toString = s"DynamicBinding($data)"

  override def isDefined = data match {
    case /* UndefinedValue |*/ None | () | null => false
    case _ => true
  }

  object JsObjectWithProperty {
    def unapply(a: (Any, String, Seq[js.Any])): Option[js.Dynamic with js.Object] = {
      a._1 match {
        case o: js.Object if js.Object.hasProperty(o, a._2) => Some(o.asInstanceOf[js.Dynamic with js.Object])
        case _ => None
      }
    }
  }

  object ScalaJsObjectWithProperty {
    def unapply(a: (Any, String, Seq[js.Any])): Option[Any] = {
      if (!a._1.isInstanceOf[js.Object]) None
      else {
        val data = a._1.asInstanceOf[js.Object with js.Dynamic]
        if (!js.Object.hasProperty(data, "$classData")) None
        else {
          val objName = data.$classData.arrayEncodedName
            .asInstanceOf[String]
            .replaceAll("\\.", "_")
            .replace("$", "\\$")
            .replaceAll(";$", "")
          val fieldMatcher = s"^${objName}__(.)_${a._2}$$".r
          val methodMatcher = s"^${a._2}__(.)(__.)?$$".r
          val entries =
            js.Object.getOwnPropertyNames(data) ++ js.Object.getOwnPropertyNames(js.Object.getPrototypeOf(data))
          entries.collect {
            case k if fieldMatcher.matches(k) => data.selectDynamic(k)
            case k if methodMatcher.matches(k) => data.applyDynamic(k)(a._3: _*)
          }.headOption
        }
      }
    }
  }

  @scala.annotation.tailrec
  final def traverse(key: String, args: Seq[Binding[Any]] = Nil): Binding[Any] = {
    val argValues = args.map(_.get.asInstanceOf[js.Any])
    (data, key, argValues) match {
      case (Some(m), key, _) => new DynamicBinding(m).traverse(key, args)
      case (_: Map[_, _], key, _) =>
        data
          .asInstanceOf[Map[String, _]]
          .get(key)
          .map(new DynamicBinding(_))
          .getOrElse(VoidBinding)
      case ScalaJsObjectWithProperty(data) => new DynamicBinding(data)

      case JsObjectWithProperty(o) =>
        try if (args.isEmpty) new DynamicBinding(Try(o.applyDynamic(key)()).getOrElse(o.selectDynamic(key)))
        else new DynamicBinding(o.applyDynamic(key)(argValues: _*))
        catch {
          case e: Exception =>
            throw new RuntimeException(
              s"method $key with args $args invoked on $o failed with cause: ${e.getStackTrace.mkString("\n")}"
            )
        }
      case _ => VoidBinding
    }
  }

  def isDictionary = data.isInstanceOf[Map[_, _]]

  def isCollection = data.isInstanceOf[Iterable[_]] && !isDictionary

  protected def collectionToIterable = data.asInstanceOf[Iterable[Any]]

  protected def dictionaryToIterable = data.asInstanceOf[Map[Any, Any]].toIterable map { case (k, v) =>
    (k.toString, v)
  }

  protected def isPrimitiveType(obj: Any) =
    obj.isInstanceOf[Int] || obj.isInstanceOf[Long] || obj.isInstanceOf[Float] ||
      obj.isInstanceOf[BigDecimal] || obj.isInstanceOf[Double] || obj.isInstanceOf[String]
}

object DynamicBinding extends BindingFactory[Any] {
  def apply(model: Any): Binding[Any] =
    new DynamicBinding(model)

  def bindPrimitive(model: String): Binding[Any] = apply(model)

  def bindPrimitive(model: Boolean): Binding[Any] = apply(model)

  def bindPrimitive(model: Int): Binding[Any] = apply(model)
}
