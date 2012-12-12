package it.pagoda5b.community

import org.drools.planner.core.solution.Solution
import org.drools.planner.core.score.buildin.hardandsoft.HardAndSoftScore
import scala.reflect.BeanProperty
import org.drools.planner.api.domain.solution.PlanningEntityCollectionProperty
import scala.annotation.tailrec

package planning {

	import java.util.{ List ⇒ JList, Collection }
	import scala.collection.JavaConversions._

	/**
	 * This class represents the planning solution to be generated from the engine.<br>
	 * It needs parameters to specify the desired numbers of preparations of each type (mass or word liturgy), and creates
	 * the preparation sequence with people assigned to each
	 */
	class PreparationsPlan(wordRequested: Int = 0, eucharistRequested: Int = 0)(implicit val community: Community) extends Solution[HardAndSoftScore] {

		@BeanProperty
		var score: HardAndSoftScore = _

		//underlying collection of the planning entities
		var preparations: Seq[LiturgyPreparation] = initPreparations(Nil, wordRequested, eucharistRequested)

		/**
		 * recursively creates empty preparations, depending on the requested number of each.
		 * Each preparation is interspersed based on the ordinal attribute wrt the preparation type, and then sorted in the final result
		 */
		@tailrec
		private def initPreparations(populated: Seq[LiturgyPreparation], word: Int, eucharist: Int): Seq[LiturgyPreparation] = (word, eucharist) match {
			case (0, 0) ⇒ populated.sortBy(_.ordinal)
			case (0, e) ⇒ initPreparations(Eucharist(populated.size + 1) +: populated, 0, e - 1)
			case (w, 0) ⇒ initPreparations(Word(populated.size + 1) +: populated, w - 1, 0)
			case (w, e) ⇒ initPreparations(Word(populated.size + 1) +: Eucharist(populated.size + 2) +: populated, w - 1, e - 1)
		}

		//use a java collection, compatible with the planning engine
		@PlanningEntityCollectionProperty
		def getPreparationList: JList[LiturgyPreparation] = preparations

		def setPreparationList(preps: JList[LiturgyPreparation]) {
			preparations = preps
		}

		/**
		 * Facts needed by the rule engine to apply planning rules
		 */
		override def getProblemFacts: Collection[AnyRef] = community.members

		/**
		 * Creates the possible initial values for the planning variables.
		 * To be used with the @ValueRangeType.FROM_SOLUTION_PROPERTY.
		 * It just wraps all possible members in a single element sequence
		 */
		def getPlanningRange: Collection[Seq[Member]] = community.members.map(Seq(_))

		/**
		 * The planning engine use this to try different evolutions of the best fit found at each step
		 */
		def cloneSolution: PreparationsPlan = {
			val cloned = new PreparationsPlan
			cloned.score = score
			cloned.preparations = preparations.map(_.duplicate)
			cloned
		}

	}
}