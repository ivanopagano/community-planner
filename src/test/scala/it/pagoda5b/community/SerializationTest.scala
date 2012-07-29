package it.pagoda5b.community

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.WordSpec
import com.google.gson.Gson

class SerializationTest extends WordSpec with ShouldMatchers {

	"A Member object" should {
		lazy val theBride: Member = new Member(name = "Jane", surname = "Dear", address = "1, Hit Street", gender = Woman, theSpouse = Some(theGroom))
		lazy val theGroom: Member = new Member(name = "John", surname = "Doe", address = "1, Test Drive", gender = Man, theSpouse = Some(theBride), residence = true)

		"recoursively define his/her spouse" in {

			theBride.spouse should be(Some(theGroom))
			theGroom.spouse foreach (_.spouse should be(Some(theGroom)))

		}
		"serialize and deserialize using java serialization" when {
			import tools.serialization.java.CommunitySerializer._

			"he/she's not married" in {

				val member = new Member("John", "Doe", "1, Test Drive", Man)
				val bytes = member.serialize
				deserialize(bytes) should be(member)

			}
			"he/she's married" in {
				val brideArray = theBride.serialize
				val freshBride = deserialize(brideArray)
				freshBride should be(theBride)
				freshBride.spouse should be('defined)
				freshBride.spouse foreach (_ should be(theGroom))

			}

		}
		/*
		 * json serialization carries some problem for recursive definition of the spouse reference
		 * 
		 * STILL UNSOLVED
		 */
		/*		"serialize to a json file" when {
			import tools.serialization.gson.GSon._
			"he/she's not married" in {

				val member = new Member("John", "Doe", "1, Test Drive", Man)

				json.toJson(member) should be(
					"""|{
						|  "name": "John",
						|  "surname": "Doe",
						|  "address": "1, Test Drive",
						|  "gender": "Man",
						|  "car": false,
						|  "residence": false,
						|  "available": true,
						|  "spouse": "None"
					  |}""".stripMargin)

			}
			"he/she's married" in {

				json.toJson(theGroom) should be(
					"""|{
										|  "name": "John",
										|  "surname": "Doe",
										|  "address": "1, Test Drive",
										|  "gender": "Man",
										|  "car": false,
										|  "residence": true,
										|  "available": true,
										|  "spouse": "Jane Dear"
									  |}""".stripMargin)

				json.toJson(theBride) should be(
					"""|{
										|  "name": "Jane",
										|  "surname": "Dear",
										|  "address": "1, Hit Street",
										|  "gender": "Woman",
										|  "car": false,
										|  "residence": false,
										|  "available": true,
										|  "spouse": "John Doe"
										|}""".stripMargin)

			}

		}
		*/
	}

}