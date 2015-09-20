package edu.uga.pickle
import java.io.File
import org.apache.commons.io.FileUtils
import scala.xml._
import scala.util.Random

/** A set of utilities to help with File IO and config xml parsing.
 *
 * @author: Terrance Medina, The University of Georgia
 * @contact: medinat_at_uga.edu
 * @date_created: 05 JAN 2015
 * @lastupdated: 31 MAY 2015
 *
 *	Parse an input specification file.
 */

object FileHelper
{

	def exists(path: String): Boolean = ( new File(path)).exists

	def mkdir(path: String, onError: () => Unit): Boolean = 
	{
		try{
			(new File(path)).mkdir
		} catch {
			case _: Throwable =>
			{
				onError()
				false
			}
		}
	}

	def loadXML(path: String): NodeSeq =
	{
		try {
				XML.loadFile(path)
			} catch {
			case _: Throwable => {
				println("ERROR: Could not open xml file at "+path)
				System.exit(-1)
				<Empty/>
			}
		}
		
	}

	def getFromXML(
		xml: NodeSeq, 
		path: List[String]
//		onError: (Throwable) => Unit = () => {
//			println("ERROR: Failed to get Element "+path+": ");
//			println(err);
//			System.exit(-1);
//		}
	) :String =
	{
		def worker(xml: NodeSeq, path: List[String]): NodeSeq =
		{
			path match
			{
					case Nil => xml
					case x::xs => worker(xml\x, xs)
			}
		}

		try {
			worker(xml, path).text
		} catch {
			case e: Throwable => { 
//				onError(e) 
				println("ERROR: Failed to get Element "+path+": ");
				println(e);
				System.exit(-1);
				""
			}
		}
	}

	/** Parse an Evolve.xml configuration file and store the values in a Map.
		*/
	def parseEvolveXMLFromPath(path: String): Map[String, String] =
	{
		val params = loadXML(path)
		parseEvolveXML(params)

	}

	def parseEvolveXML(params: NodeSeq): Map[String, String] =
	{
		val agent_type = getFromXML( params, List("ControllerParameters", "AgentToEvolve") )
		println("AgentToEvolve = "+agent_type)

		val applicationXML = getFromXML( params, List("ControllerParameters", "ApplicationXML") )
		val population_size = getFromXML( params, List("EvolutionParameters", "PopulationSize") )
		println("Population Size = "+population_size)
	
		val mutation_prob = getFromXML(params, List("EvolutionParameters","MutationProbability"))
		println("Mutation Probability = "+mutation_prob)

		val crossover_prob = getFromXML(params, List("EvolutionParameters","CrossoverProbability"))
		println("Crossover Probability = "+crossover_prob)

		val survivor_selection = getFromXML(params, List("EvolutionParameters","SurvivorSelection"))
		println("Survivor Selection = "+survivor_selection)

		val generations = getFromXML(params, List("EvolutionParameters","Generations"))
		println("Generations = "+generations)

		val randomSeed = if( getFromXML(params, List("RandomSeed")) == "" ) 
		{
			val r = new Random()
			r.nextInt(1000000000).toString
		} else 
		{
			getFromXML(params, List("RandomSeed"))
		}
		println("RandomSeed = "+randomSeed)

		val simulationSteps = getFromXML(params, List("SimulationSteps"))
		println("SimulationSteps = "+simulationSteps)

		Map("AgentToEvolve" -> agent_type, 
				"PopulationSize" -> population_size, 
				"MutationProbability" -> mutation_prob,
				"CrossoverProbability" -> crossover_prob,
				"SurvivorSelection" -> survivor_selection,
				"ApplicationXML" -> applicationXML,
				"RandomSeed" -> randomSeed,
				"SimulationSteps" -> simulationSteps,
				"Generations" -> generations
		)
		
	}

	def copyFile(src: String, dst: String)
	{
		try {
			FileUtils.copyFile(new File(src), new File(dst))
		} catch {
			case _: Throwable =>
			{
				println("ERROR: failed to copy "+src+" to "+dst+".")
				System.exit(-1)
			}
		}
	}


	/** Open the Evolve.xml at the supplied path and read the "Destination"
	  *		Element, which tells us where to write the evolution results.
		*/
	def getEvolveDirPath(evolveParamsPath: String): String =
	{
		val params = try {
				XML.loadFile(evolveParamsPath)
			} catch {
			case _: Throwable => {
				println("ERROR: Could not open evolve params at "+evolveParamsPath)
				System.exit(-1)
				<Empty/>
			}
		}

		val path = try {
			(params\"Destination").text
		} catch {
			case _: Throwable => {
				println("ERROR: No Destination Found in evolve.xml");
				System.exit(-1)
				""
			}
		}

		path
	}

}
