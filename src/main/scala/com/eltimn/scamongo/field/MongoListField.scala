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

import java.util.Date

import scala.collection.mutable.ListBuffer

import net.liftweb.json.JsonAST.JObject
import net.liftweb.http.js.JE.Str
import net.liftweb.record.{Field, Record}
import net.liftweb.util.{Box, Empty, Failure, Full}

import com.mongodb._
import com.mongodb.util.JSON

/**
 * List field. This version uses a Scala List
*/
class MongoListField[OwnerType <: MongoRecord[OwnerType], ListType](rec: OwnerType)
	extends Field[List[ListType], OwnerType] with MongoFieldFlavor[List[ListType]] {

	import Meta.Reflection._

	def asJs = Str(toString)

	def asXHtml = <div></div>

	def defaultValue = List[ListType]()

	def setFromAny(in: Any): Box[List[ListType]] = {
    in match {
    	// add a List[JObject]
    	case list: List[ListType] => Full(set(list))
    	case Some(list: List[ListType]) => Full(set(list))
      case Full(list: List[ListType]) => Full(set(list))
      case dbo: DBObject => setFromDBObject(dbo)
    	case seq: Seq[ListType] if !seq.isEmpty => Full(set(seq.toList))
      case null => Full(set(null))
      case s: String => setFromString(s)
      case None | Empty | Failure(_, _, _) => Full(set(null))
      case o => {
      	println("setFromStrring: "+o.toString)
      	setFromString(o.toString)
      }
    }
  }

	def toForm = <div></div>

	def owner = rec

	/*
	* Convert this field's value into a DBObject so it can be stored in Mongo.
	* Compatible with most object types. Including Pattern, ObjectId, DBRef, JObject,
	* and JsonObject case classes
	* Override this method for custom logic.
	*/
  def asDBObject(): DBObject = {
  	val dbl = new BasicDBList

  	implicit val formats = owner.meta.formats

		value.foreach {
			case jo: JsonObject[Any] => dbl.add(JObjectParser.parse(jo.asJObject)) // A case class that extends JsonObject
			//case jo: JObject => dbl.add(JObjectParser.parse(jo)) // Any JObject
			case m: Map[String, Any] => dbl.add(MapParser.parse(m))
			case f =>	f.asInstanceOf[AnyRef] match {
				case x if primitive_?(x.getClass) => dbl.add(x)
				case x if datetype_?(x.getClass) => dbl.add(datetype2dbovalue(x))
				case x if mongotype_?(x.getClass) => dbl.add(mongotype2dbovalue(x))
				case o => dbl.add(o.toString)
			}
		}
		dbl
  }

	// set this field's value using a DBObject returned from Mongo.
	def setFromDBObject(dbo: DBObject): Box[List[ListType]] = {
		val ret = new ListBuffer[ListType]
		for (k <- dbo.keySet.toArray) {
			dbo.get(k.toString) match {
				case x if primitive_?(x.getClass) => ret += x.asInstanceOf[ListType]
				//case x if mongotype_?(x.getClass) => ret += dbovalue2mongotype(x))
				/* How do I find out what type ListType is?
				case dbo: BasicDBObject => ListType match {
					case DBRef => (dbo.containsField("ref"), dbo.containsField("id")) match {
						case (true, true) => Full(set(DBRef(dbo.get("ref").toString, dbo.get("id").toString)))
						case _ => Empty //Full(set(null))
					}
				}
				*/
				case dbo: BasicDBObject if (dbo.containsField("ref") && dbo.containsField("id") && dbo.keySet.size == 2) =>
					ret += DBRef(dbo.get("ref").toString, dbo.get("id").toString).asInstanceOf[ListType]
				case o => {
					ret += o.asInstanceOf[ListType]
				}
			}
		}
		Full(set(ret.toList))
		/*
		for {
			k <- dbo.keySet.toArray
			dbo.get(k.toString) match {

			}
		} yield
		*/
	}
}

/*
* List of Dates
*/
class MongoDateListField[OwnerType <: MongoRecord[OwnerType]](rec: OwnerType)
	extends MongoListField[OwnerType, Date](rec: OwnerType) {

	override def setFromDBObject(dbo: DBObject): Box[List[Date]] = {
		val ret = new ListBuffer[Date]
		for (k <- dbo.keySet.toArray) {
			dbo.get(k.toString) match {
				case s: String=> owner.meta.formats.dateFormat.parse(s) match {
					case Some(d: Date) => ret += d
					case _ =>
				}
				case _ =>
			}
		}
		Full(set(ret.toList))
	}
}

/*
* List of JObjects
*/
class MongoJObjectListField[OwnerType <: MongoRecord[OwnerType]](rec: OwnerType)
	extends MongoListField[OwnerType, JObject](rec: OwnerType) {
	
	override def setFromDBObject(dbo: DBObject): Box[List[JObject]] = {
		implicit val formats = owner.meta.formats
		val ret = new ListBuffer[JObject]
		for (k <- dbo.keySet.toArray) {
			ret += JObjectParser.serialize(dbo.get(k.toString)).asInstanceOf[JObject]
		}
		Full(set(ret.toList))
	}
}

/*
* List of JsonObject case classes

class MongoJsonObjectListField[OwnerType <: MongoRecord[OwnerType], CaseType <: JsonObjectMeta[CaseType]](rec: OwnerType, cas: CaseType)
	extends MongoListField[OwnerType, CaseType](rec: OwnerType) {
	
	override def setFromDBObject(dbo: DBObject): Box[List[CaseType]] = {
		implicit val formats = owner.meta.formats
		val ret = new ListBuffer[CaseType]
		for (k <- dbo.keySet.toArray) {
			ret += cas.create(JObjectParser.serialize(dbo.get(k.toString)).asInstanceOf[JObject])
		}
		Full(set(ret.toList))
	}
}
*/