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

import net.liftweb.json.JsonAST.JObject

import com.mongodb._

/*
* extend case class with this trait
*/
trait MongoDocument[BaseDocument] extends JsonObject[BaseDocument] {
	self: BaseDocument =>

  var errors = List()

	def _id: Any

	def meta: MongoDocumentMeta[BaseDocument]

  def validate : Boolean
	def save = meta.save(this)

	def delete {
		meta.delete("_id", _id)
	}


	def getRef: MongoRef = MongoRef(meta.collectionName, _id.toString)
}

/*
* extend case class companion objects with this trait
*/
trait MongoDocumentMeta[BaseDocument] extends JsonObjectMeta[BaseDocument] with MongoMeta[BaseDocument] {

	def create(dbo: DBObject): BaseDocument = {
		create(JObjectParser.serialize(dbo).asInstanceOf[JObject])
	}

	/**
	* Find a single document by _id object.
	def find(a: Any): Option[BaseDocument] = a match {
		case oid: ObjectId => find(new BasicDBObject("_id", oid))
		case s: String => find(new BasicDBObject("_id", s))
	}
	*/

	/**
	* Find a single row by a qry, using a DBObject.
	*/
	private def find(qry: DBObject): Option[BaseDocument] = {
		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll =>
			coll.findOne(qry) match {
				case null => None
				case dbo => {
					Some(create(dbo))
				}
			}
		)
	}

	/**
	* Find a single document by _id.
	*/
	def find(s: String): Option[BaseDocument] = ObjectId.isValid(s) match {
		case true => find(new BasicDBObject("_id", new ObjectId(s)))
		case false => find(new BasicDBObject("_id", s))
	}

	/**
	* Find a single document by a qry using String, Any inputs
	*/
	def find(k: String, v: Any): Option[BaseDocument] = find(new BasicDBObject(k, v match {
		case s: String if (ObjectId.isValid(s)) => new ObjectId(s)
		case _ => v
	}))

	/**
	* Find a single document by a qry using a json value
	*/
	def find(json: JObject): Option[BaseDocument] = find(JObjectParser.parse(json))

	/**
	* Find all documents in this collection
	*/
	def findAll: List[BaseDocument] = {
		import scala.collection.jcl.Conversions._

		/*
		* The call to toArray retrieves all documents and puts them in memory.
		*/
		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll => {
			coll.find.toArray.map(dbo => create(dbo)).toList
		})
	}

	/**
	* Find all documents using a DBObject query.
	*/
	private def findAll(qry: DBObject, sort: Option[DBObject], opts: FindOption*): List[BaseDocument] = {
		import scala.collection.jcl.Conversions._

		val findOpts = opts.toList

		MongoDB.useCollection(mongoIdentifier, collectionName) ( coll => {
			val cur = coll.find(qry).limit(
				findOpts.find(_.isInstanceOf[Limit]).map(x => x.value).getOrElse(0)
			).skip(
				findOpts.find(_.isInstanceOf[Skip]).map(x => x.value).getOrElse(0)
			)
			sort.foreach( s => cur.sort(s))
			// The call to toArray retrieves all documents and puts them in memory.
			cur.toArray.map(dbo => create(dbo)).toList
		})
	}

	/**
	* Find all documents using a DBObject query. These are for passing in regex queries.
	*/
	def findAll(qry: DBObject, opts: FindOption*): List[BaseDocument] =
		findAll(qry, None, opts :_*)

	/**
	* Find all documents using a DBObject query with sort
	*/
	def findAll(qry: DBObject, sort: DBObject, opts: FindOption*): List[BaseDocument] =
		findAll(qry, Some(sort), opts :_*)

	/**
	* Find all documents using a JObject query
	*/
	def findAll(qry: JObject, opts: FindOption*): List[BaseDocument] =
		findAll(JObjectParser.parse(qry), None, opts :_*)

	/**
	* Find all documents using a JObject query with sort
	*/
	def findAll(qry: JObject, sort: JObject, opts: FindOption*): List[BaseDocument] =
		findAll(JObjectParser.parse(qry), Some(JObjectParser.parse(sort)), opts :_*)

	/**
	* Find all documents using a Map query
	*/
	def findAll(qry: Map[String, Any], opts: FindOption*): List[BaseDocument] =
		findAll(MapParser.parse(qry), None, opts :_*)

	/**
	* Find all documents using a Map query with sort
	*/
	def findAll(qry: Map[String, Any], sort: Map[String, Any], opts: FindOption*): List[BaseDocument] =
		findAll(MapParser.parse(qry), Some(MapParser.parse(sort)), opts :_*)

	/**
	* Find all documents using a k, v query
	*/
	def findAll(k: String, o: Any, opts: FindOption*): List[BaseDocument] =
		findAll(new BasicDBObject(k, o), None, opts :_*)

	/**
	* Find all documents using a k, v query with JObject sort
	*/
	def findAll(k: String, o: Any, sort: JObject, opts: FindOption*): List[BaseDocument] =
		findAll(new BasicDBObject(k, o), Some(JObjectParser.parse(sort)), opts :_*)

	/**
	* Find all documents using a k, v query with Map sort
	*/
	def findAll(k: String, o: Any, sort: Map[String, Any], opts: FindOption*): List[BaseDocument] =
		findAll(new BasicDBObject(k, o), Some(MapParser.parse(sort)), opts :_*)

	/*
	* Save a document to the db
	*/
	def save(in: BaseDocument) : Boolean = {
    MongoDB.use(mongoIdentifier) (db => {
      save(in, db)
    })
	}

	/*
	* Save a document to the db using the given Mongo instance
	*/
	def save(in: BaseDocument, db: DB) : Boolean = {
    in match {
      case md:MongoDocument[_] => 
        md.validate match {
          case true =>
            saveWithoutValidation(in, db)
            true
          case _ => false
        }
      case _ =>
        saveWithoutValidation(in, db)
        true
    }
	}

  /*
  * Save a document to the db using the given Mongo instance, without validations.
  */
  def saveWithoutValidation(in: BaseDocument, db: DB) = {
    db.getCollection(collectionName).save(JObjectParser.parse(toJObject(in)))
  }

	/*
	* Update document with a JObject query using the given Mongo instance
	*/
	def update(qry: JObject, newbd: BaseDocument, db: DB, opts: UpdateOption*) {
		update(qry, toJObject(newbd), db, opts :_*)
	}

	/*
	* Update document with a JObject query
	*/
	def update(qry: JObject, newbd: BaseDocument, opts: UpdateOption*) {
    newbd match {
      case md:MongoDocument[_] => 
        md.validate match {
          case true =>
            updateWithoutValidation(qry, newbd, opts :_*)
            true
          case _ => false
        }
      case _ =>
        updateWithoutValidation(qry, newbd, opts :_*)
        true
    }
	}

  /*
  * Update document with a JObject query, without validations.
  */
  def updateWithoutValidation(qry: JObject, newbd: BaseDocument, opts: UpdateOption*) {
		MongoDB.use(mongoIdentifier) ( db => {
			update(qry, newbd, db, opts :_*)
		})
  }

}
