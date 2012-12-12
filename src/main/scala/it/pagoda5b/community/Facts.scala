/**
 *
 */
package it.pagoda5b.community

import org.drools.planner.api.domain.entity.PlanningEntity
import org.drools.planner.api.domain.variable._

//A person's gender
sealed trait Gender extends Serializable
case object Man extends Gender
case object Woman extends Gender

/**
 * @author Ivano Pagano
 *
 * This is a community member, be it man or woman alike
 * It's a problem's fact
 */
class Member(
	val name: String,
	val surname: String,
	val address: String,
	val gender: Gender,
	@transient theSpouse: ⇒ Option[Member] = None,
	val car: Boolean = false,
	val residence: Boolean = false,
	val available: Boolean = true) extends Serializable {

	lazy val spouse = theSpouse

	override def toString: String = name + " " + surname

	override def equals(other: Any): Boolean =
		other match {
			case that: Member ⇒ toString == that.toString
			case _ ⇒ false
		}

	override def hashCode: Int = 41 * toString.hashCode
}

/**
 * Describes the community as a whole, giving extra information about member's relationship
 */
class Community(val members: Seq[Member] = Nil) {

	//An easy access to the couples in the community
	val couples: Map[Member, MarriedCouple] = getMarriedCouples

	//Calculates the married couples scanning the members list
	private def getMarriedCouples = {
		members.foldLeft(Map[Member, MarriedCouple]()) {
			case (couples, member) ⇒ (member.gender, member.spouse) match {
				case (Man, Some(wife)) ⇒
					couples + (member -> MarriedCouple(member, wife))
				case (Woman, Some(husband)) ⇒
					couples + (member -> MarriedCouple(husband, member))
				case _ ⇒ couples
			}
		}
	}

}

/**
 * This is a general class describing a liturgy preparation made by community members
 * The preferred number of people to prepare is defined in subclasses
 * It's a planning fact
 */
@PlanningEntity
abstract class LiturgyPreparation(val peoplePreferred: Int, val ordinal: Int) {

	//people preparing the liturgy
	private var assigned: Seq[Member] = Nil

	def setAssigned(newAssignment: Seq[Member]): Unit = {
		assigned = newAssignment
	}

	@PlanningVariable //	@ValueRange(type=ValueRangeType.FROM_SOLUTION_PROPERTY, )
	def getAssigned: Seq[Member] = assigned

	/**
	 * creates a clone of this preparation
	 */
	def duplicate: LiturgyPreparation

	/*Instead of calculating other attributes to know if the liturgy is full 
	 *or if there's need for a car or a house to prepare in, create a violated constraint with a score
  */
}

/**The holy mass*/
case class Eucharist(val ord: Int) extends LiturgyPreparation(peoplePreferred = 3, ord) {
	def duplicate: LiturgyPreparation = copy(ord)
}
/**The reading of the holy bible*/
case class Word(val ord: Int) extends LiturgyPreparation(peoplePreferred = 4, ord) {
	def duplicate: LiturgyPreparation = copy(ord)
}

//There follows some of the problem constraints used to build the score

case class MarriedCouple(val husband: Member, val wife: Member)

