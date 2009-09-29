package com.eltimn.scamongo

/*
 * Copyright 2009 Tim Nelson
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */

import scala.collection.jcl.Conversions._

import java.util.Date

import net.liftweb.json.Formats
import net.liftweb.json.JsonAST._
//import net.liftweb.json.Meta.Reflection._

import com.mongodb.{BasicDBObject, BasicDBList, DBObject}

object JObjectParser {

	/*
	* Parse a JObject into a DBObject
	*/
	def parse(jo: JObject): DBObject = {
		Parser.parse(jo)
  }

	/*
	* Serialize a DBObject into a JObject
	*/
	def serialize(a: Any)(implicit formats: Formats): JValue = serialize(a, formats)

  private def serialize(a: Any, formats: Formats): JValue = {
  	import Meta.Reflection._
		a.asInstanceOf[AnyRef] match {
			case x if primitive_?(x.getClass) => primitive2jvalue(x)
			case x if datetype_?(x.getClass) => datetype2jvalue(x)(formats)
			case x: java.util.ArrayList[_] => JArray(x.toList.map( x => serialize(x, formats)))
			case x: Option[_] => serialize(x getOrElse JNothing, formats)
			case x: DBObject =>
				x.keySet.toArray.toList.map { f =>
					JField(f.toString, serialize(x.get(f.toString), formats))
				}
				match {
			    case Nil => JNothing
			    case fields => JObject(fields)
			  }
			case x => {
				println("match error (serialize): "+x.getClass)
				JNothing
			}
		}
  }

	object Parser {

		def parse(jo: JObject): DBObject = {
			parseObject(jo.obj)
		}

		private def parseArray(arr: List[JValue]): BasicDBList = {
			val dbl = new BasicDBList
			trimArr(arr).foreach { a =>
				a match {
					case JArray(arr) => dbl.add(parseArray(arr))
					case JObject(jo) => dbl.add(parseObject(jo))
					case jv: JValue => dbl.add(renderValue(jv))
				}
			}
			dbl
		}

		private def parseObject(obj: List[JField]): BasicDBObject = {
			val dbo = new BasicDBObject
			trimObj(obj).foreach { jf =>
				jf.value match {
					case JArray(arr) => dbo.put(jf.name, parseArray(arr))
					case JObject(jo) => dbo.put(jf.name, parseObject(jo))
					case jv: JValue => dbo.put(jf.name, renderValue(jv))
				}
			}
			dbo
		}

		private def renderValue(jv: JValue): Object = jv match {
		  case JBool(b) => java.lang.Boolean.valueOf(b)
		  case JInt(n) => renderInteger(n)
		  case JDouble(n) => new java.lang.Double(n)
		  case JNull => null
		  case JNothing => error("can't render 'nothing'")
		  case JString(null) => "null"
		  case JString(s) => s
		  case _ => "match error (renderValue): "+jv.getClass
		}

		private def renderInteger(i: BigInt): Object = {
			if (i <= java.lang.Integer.MAX_VALUE && i >= java.lang.Integer.MIN_VALUE) {
				new java.lang.Integer(i.intValue)
			}
			else if (i <= java.lang.Long.MAX_VALUE && i >= java.lang.Long.MIN_VALUE) {
				new java.lang.Long(i.longValue)
			}
			else {
				i.toString
			}
		}

		private def trimArr(xs: List[JValue]) = xs.filter(_ != JNothing)
		private def trimObj(xs: List[JField]) = xs.filter(_.value != JNothing)
  }


}