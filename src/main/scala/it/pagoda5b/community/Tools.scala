package it.pagoda5b.community

package tools {

	package serialization {

		package gson {
			import com.google.gson._
			import _root_.java._
			import lang.reflect.Type

			/**
			 * Collects operations for the gson library to ease up it's use for the Community App
			 */
			object GSon {
				implicit def stringToJson(s: String): JsonElement = new JsonPrimitive(s)

				//Define some de/serializer specific to the domain
				private class GenderSerializer extends JsonSerializer[Gender] {
					override def serialize(gender: Gender, srcType: Type, context: JsonSerializationContext): JsonElement = gender.toString
				}
				private class GenderDeserializer extends JsonDeserializer[Gender] {
					override def deserialize(json: JsonElement, srcType: Type, context: JsonDeserializationContext): Gender = json.getAsJsonPrimitive.getAsString match {
						case "Man" ⇒ Man
						case "Woman" ⇒ Woman
					}
				}

				private class SpouseSerializer[Member] extends JsonSerializer[Option[Member]] {
					override def serialize(option: Option[Member], srcType: Type, context: JsonSerializationContext): JsonElement = (option.map(_.toString) getOrElse ("None")).asInstanceOf[String]
				}
				private class SpouseDeserializer[Member] extends JsonDeserializer[Option[Member]] {
					override def deserialize(json: JsonElement, srcType: Type, context: JsonDeserializationContext): Option[Member] = json.getAsJsonPrimitive.getAsString match {
						case "None" ⇒ None
						// Still undefined case: should look up the correct  object from a lookup table or something, using its toString definition
						//					case "Some" ⇒ Some
					}
				}

				//The private instance, which uses default configuration and is exposed through a public method
				private[this] val builder = new GsonBuilder()
					.registerTypeAdapter(classOf[Gender], new GenderSerializer)
					.registerTypeAdapter(classOf[Gender], new GenderDeserializer)
					.registerTypeAdapter(classOf[Option[Member]], new SpouseSerializer)
					.registerTypeAdapter(classOf[Option[Member]], new SpouseDeserializer)
					.setPrettyPrinting

				def json = builder.create
			}

		}

		package java {
			/**
			 * Defines utility methods and patterns to easily de/serialize domain objects based on standard java serialization
			 */
			object CommunitySerializer {

				//loan pattern
				private def loan[A, R <: { def close(): Unit }](res: R)(thunk: R ⇒ A) = {
					try {
						thunk(res)
					} finally {
						res.close()
					}
				}

				/**
				 * Defines a sort of serializable type class
				 */
				trait StandardSerializable[A] {
					def serialize(obj: A): Array[Byte]
					def deserialize(bytes: Array[Byte]): A
				}

				/**
				 * Generic serialization method for any implicitly available type class
				 */
				def serialize[S: StandardSerializable](from: S) = {
					val serializer = implicitly[StandardSerializable[S]]
					serializer.serialize(from)
				}

				/**
				 * Generic deserialization method for any implicitly available type class
				 */
				def deserialize[S: StandardSerializable](bytes: Array[Byte]): S = {
					val serializer = implicitly[StandardSerializable[S]]
					serializer.deserialize(bytes)
				}

				implicit def pimp[A](a: A)(implicit s: StandardSerializable[A]) = new {
					def serialize: Array[Byte] = s.serialize(a)
					def deserialize(bytes: Array[Byte]): A = s.deserialize(bytes)

				}

				/**
				 * implicit definition of serialization type class for the Member class
				 */
				implicit val memberSerializable = new StandardSerializable[Member] {
					import _root_.java.io._

					override def serialize(member: Member): Array[Byte] = {
						loan(new ByteArrayOutputStream) { bytes ⇒
							val outstream = new ObjectOutputStream(bytes)
							outstream.writeObject(member)
							bytes.toByteArray
						}
					}

					override def deserialize(bytes: Array[Byte]): Member = {
						loan(new ByteArrayInputStream(bytes)) { in ⇒
							new ObjectInputStream(in).readObject().asInstanceOf[Member]
						}
					}
				}

			}
		}

	}

}