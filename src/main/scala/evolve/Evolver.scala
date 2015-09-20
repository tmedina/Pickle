package edu.uga.pickle.evolve

import edu.uga.pickle.MainApplication
import edu.uga.pickle.Simulator
import edu.uga.pickle.FileHelper

import scala.xml.{XML, NodeSeq, Node}
import scala.annotation.tailrec

import java.io.File
import java.io.FileWriter
import java.util.UUID

import org.apache.commons.io.FileUtils
import ec.util.MersenneTwisterFast;

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent._
import scala.util.Random
import scala.concurrent.ExecutionContext.Implicits.global
import scala.math.sqrt

import java.util.Calendar

/** Provides the main entry point for a Pickle evolution run.
 *
 * @author: Terrance Medina, The University of Georgia
 * @contact: medinat_at_uga.edu
 * @date_created: 05 JAN 2015
 * @lastupdated: 31 MAY 2015
 *
 *	Parse an input specification file.
 */

object Evolver
{

	val random = new MersenneTwisterFast(0)

	def main(args: Array[String])
	{
		val mode = args.length match
		{
			case 0 => ""
			case _ => args(0).toUpperCase
		}

		if( mode == "INIT" )
		{
			if(args.length < 3)
			{
				println
				println("Usage: evolve path/to/evolve.xml path/to/outputDir")
				println
			}
			else
			{
				println("Starting new evolution run")
				val paramsPath = args(1) //where to find the Evolve.xml
				val destPath = args(2) //where to put the results
	
				//assert dest does not already exist (don't accidentally clobber previoius runs)
				FileHelper.exists(destPath) && { println("ERROR: "+destPath +" already exists."); System.exit(-1); false }
				FileHelper.mkdir(destPath, () => System.exit(-1))
	
				//copy Evolve.xml and App.xml to dest
				/////////////////////
				FileHelper.copyFile(paramsPath, destPath+"/Evolve.xml")
	
				//Use copy of Evolve.xml as our reference copy
				val evParamsXML = FileHelper.loadXML(destPath+"/Evolve.xml")
				val evolveParams = FileHelper.parseEvolveXML(evParamsXML)
	
				//copy app to dest
				FileHelper.copyFile(evolveParams("ApplicationXML"), destPath+"/Application.xml")
				val appParamsXML = FileHelper.loadXML(destPath+"/Application.xml")
	
				//start evolution
				////////////////////
				Evolver.init(evParamsXML, appParamsXML, destPath)
			}

		}
//		else if(mode == "RESUME" )
//		{
//
//			val destPath = args(1)
//			//assert dest exists and contains Evolve.xml and Application.xml
//			FileHelper.exists(destPath) || { println("ERROR: no such evolution set found"); System.exit(-1); false }
//			FileHelper.exists(destPath+"/Evolve.xml") || { println("ERROR: evolution is missing Evolution.xml"); System.exit(-1); false }
//			FileHelper.exists(destPath+"/Application.xml") || { println("ERROR: evolution is missing Application.xml"); System.exit(-1); false }
//
//			//Find highest numbered generation and ensure it is complete
////			val lastGen: List[Node] = getLastGeneration(destPath);
////
////			//Reset initial population to generation n 
////
////			println("resuming from previous run")
////			Evolver.resume(destPath, lastGen)
//		}
		else if(mode == "REPLAY")
		{
			// Get Controller from Result.xml file
			// Parse path from supplied argument
			// Use App.xml from root of evolution directory
			if(args.length < 1)
			{
				println
				println("Usage: replay path/to/evolution/result.xml")
				println
			}
			else
			{
				val resultXML = try {
					XML.loadFile(args(1))
				} catch {
					case _: Throwable => {
						println
						println("ERROR: unable to open result xml file "+args(1));
						println
						null
					}
				}

				if(resultXML != null)
				{
					val controller = resultXML\"controller"
		
						val pathParts = args(1).split("/").toList
						val dir = (for( i <- 0 until (pathParts.size-2)) yield pathParts(i)).toList reduceLeft ( _ +"/"+ _ )
						println(dir)
					val appXML = try {
						XML.loadFile(dir+"/Application.xml")
					} catch {
						case _: Throwable =>
						{
							println
							println("ERROR: Unable to open application xml")
							println
							null
						}
					}

					if(appXML != null)
					{
						val evolveXML = XML.loadFile(dir+"/Evolve.xml")
						val agentToEvolve = (evolveXML\"ControllerParameters"\"AgentToEvolve").text
						val controllerMap = Map[String, NodeSeq](agentToEvolve -> controller)
						val seed = (resultXML\"RandomSeed").text
						val simParams = Map[String,String]("AppFile" -> (dir+"/Application.xml"), "RandomSeed" -> seed)
			
						val sim = new Simulator
						sim.runSimulation(simParams,controllerMap)
					}
				}
			}
		}
	}//end main


	/** Run a new evolution from the start
		*/
	def init(evParamsXML: NodeSeq, appParamsXML: NodeSeq, dest: String)
	{

		val app = appParamsXML
		val evolveParams = FileHelper.parseEvolveXML(evParamsXML)

		val population_size = evolveParams("PopulationSize").toInt
		val agent_type = evolveParams("AgentToEvolve")
		val mutation_prob = evolveParams("MutationProbability").toInt
		val crossover_prob = evolveParams("CrossoverProbability").toInt
		val survivor_selection = evolveParams("SurvivorSelection")
		val generations = evolveParams("Generations").toInt

		val seed = (new Random(Calendar.getInstance.getTime.getTime)).nextInt


		val agent_spec  = for { a <- (app\"Agents"\"Agent"); if ((a\"@type").toString == agent_type ) } yield a
		val rc = new RandomController(agent_spec(0), evParamsXML)

		//Initialize a Random Population: Ramped half-and-half method
		//or resume from a given population
		val initialpop: List[NodeSeq] = (for( i <- 0 until population_size ) yield rc.gen(seed) ).toList

		// main worker to recursively run the evolutions
		@tailrec
		def run(population: List[NodeSeq], generation: Int): List[NodeSeq] =
		{
			//TODO: alternatively, match on best fitness result (i.e. we've found an ideal candidate)

			generation match
			{
				case i: Int =>
				{
					if ( i == generations ) population
					else
					{
						val nextPopulation = makeNextGeneration(
											population, 
											dest+"/generation."+(generation+1), 
											agent_type, 
											evolveParams, 
											app,
											rc,
											generation)
						run(nextPopulation, generation + 1)
					}
				}
			}
		}
		run(initialpop, 0)
	}

	def uuid() = UUID.randomUUID


	/** Evaluate a population, create the next generation based on the fitness results.
	 *	@param population: the population to be evaluated.
	 *	@param filesLocation: The directory to write the results into
	 *	@param agent_type: The agent type being evolved
	 *	@param evolveParams: a map that contains the evolution parameters from evolve.xml
	 *	@param params: The XML from Application XML
	 *	@returns the next population as a list of XML controllers
	 */
	def makeNextGeneration(
		population: List[NodeSeq], //the last generation
		filesLocation: String, //where to save our results
		agent_type: String, //which agent we are evolving
		evolveParams: Map[String,String], //parameters to define the evolution
		params: NodeSeq, //parameters from the Application xml
		rc: RandomController, //a factory for making new random controllers
		genNumber: Int
		): List[NodeSeq] = 
	{
			val time: Long = Calendar.getInstance.getTime.getTime
			val sRand = new Random(time)
			val newSeed: Long = sRand.nextLong()
			val agent_spec  = for { a <- (params\"Agents"\"Agent"); if ((a\"@type").toString == agent_type ) } yield a
			val evutils = new EvolveUtils(agent_spec(0), (XML.loadFile("conf/mutate.xml"))(0))
			val genDir = new File(filesLocation)
			if(!genDir.exists()) 
				try{
					genDir.mkdir()
				} catch {
					case _: Throwable =>
					{
						println("ERROR: failed to create evolve dir.")
						System.exit(-1)
					}
				}
			//Run simulation on each member of population
			def runSimulation(p: NodeSeq): NodeSeq =
			{
//				val printer = new scala.xml.PrettyPrinter(120,2)
//				println(printer.format(p(0)))
				val simulator = new Simulator
				val controllerMap = Map(agent_type -> p)
				val simParams = scala.collection.mutable.Map(
						"AppFile" -> evolveParams("ApplicationXML"),
						"RandomSeed" -> newSeed.toString,//evolveParams("RandomSeed"),
						"GUI" -> "None",
						"SimulationSteps" -> evolveParams("SimulationSteps")
				)

				var results: List[Double] = List()
				var stderr:Double = 100.0
				var numSurvivors:Double  = 0.0
				var iteration = 0
				var goAgain = true
					do {
						try {
	
							val thisResult = simulator.runForEvolution(simParams.toMap, controllerMap)
							
							results = thisResult :: results
							val sum:Double = results.foldLeft (0.0)( _ + _ )
							val samples: Int = results.size
							val adjustedSamples = if( sqrt(samples-1)  == 0 ) 1 else sqrt(samples-1)
							val mean: Double = sum/samples
							val variance: Double = (results.foldLeft (0.0) {
								(acc: Double, next: Double) =>
								{
									acc + ((next - mean) * (next - mean))
								}
							}) / adjustedSamples
							val stddev = sqrt(variance)
							stderr = stddev/adjustedSamples
							numSurvivors = mean

							println("Sum: "+sum)
							println("Samples: "+samples)
							println("AdjSamples: "+adjustedSamples)
							println("Mean: "+mean)
							println("Variance: "+variance)
							println("Stddev: "+stddev)
							println("StdErr: "+stderr)

							goAgain = if( iteration < 3 ) true
								else if(stderr > 1.5 && iteration < 9) true
								else false
							if( goAgain )
							{
								iteration = iteration + 1
								println("Re-running simulation "+iteration+" ...")
								simParams("RandomSeed") = (new Random(Calendar.getInstance.getTime.getTime)).nextInt.toString
								println("New seed = "+simParams("RandomSeed"))
							}

	
					} catch {
						case e: Throwable =>
						{
							//log the failure and rerun
							println("ERROR: [RunSimulation] -  "+e);
							println("ERROR: [RunSimulation] -  "+e.printStackTrace());
							goAgain = true
						}
					}
				} while( goAgain )

				println("Id: "+(p\\"controller"\"@id").text);
				println("Survivors: "+numSurvivors +"  ")
				println("Generation: "+genNumber)
				<Results>
				<Generation>{genNumber}</Generation>
				<RandomSeed>{simParams("RandomSeed")}</RandomSeed>
				<SurvivalRate>{numSurvivors}</SurvivalRate>
				{p}
				</Results>
			}

			//Single-threaded version
			val results = for( p <- population ) yield { runSimulation(p) }


////		Uncomment for Parallel version		
//			val simThreads = for( p <- population ) yield 
//			{
//				Future[NodeSeq] {
//					runSimulation(p)
//				}
//			}
//
//			var results = List[NodeSeq]()
//
//			val simResults = for( s <- simThreads ) yield
//			{
//				s.map
//				{
//					result: NodeSeq => 
//					{
//						results = result :: results
//					}
////					e => println("ERROR: "+e)
//				}
//			}
//
//			for( s <- simResults ) Await.result(s, 1.hour)
//			println("\nDone Waiting!")
//			println("Size = "+results.size)




			println()
			println("Initial Results")
			println("========================")
			var i = 0
			for( r <- results ) 
			{
				print(i +"->"+(r\"SurvivalRate").text.toDouble + "  ")
				i = i + 1
			}
			println

			//Order results by fitness, make the rank part of the filename
			val ordered = getOrderedResults("SurvivalRate", results)
			println("Sorted by rank ============================")
			for( s <- ordered ) print( (s\"Rank").text +"->"+ (s\"SurvivalRate").text + "  ")
			println

			//Perform crossover on best 6 -> 3 offspring
			val offspring = makeOffspring(ordered, evutils)
//			println("OFFSPRING SIZE == "+offspring.size)

			//mutate next best 3
			val mutated = mutate(ordered, evutils)
//			println("MUTATED ==========================================")
//			for( m <- mutated) println( m )

			//replace worst 3 by the 3 new offspring
			val seed = (new Random(Calendar.getInstance.getTime.getTime)).nextInt
			val randoms = (for( n <- 0 until ordered.size/4) yield rc.gen(seed)).toList
			println("MADE "+randoms.size+" RANDOMS !!!!!!!!!!!!!!!!!!");

			val nextGen = replace(ordered, mutated, offspring, randoms)
			println("MADE "+nextGen.size+" NEXT GEN !!!!!!!!!!!!!!!!!!");
//			println("Next Generation ===================")
//			for( n <- nextGen ) println(n + "\n\n")
//			println("NEXTGEN SIZE == "+nextGen.size)
	
			//Log old population
			var k = 0
			val printer = new scala.xml.PrettyPrinter(120,2)
			for( r <- ordered )
			{
				val runStats = new FileWriter(filesLocation+"/result_"+k+".xml")
				runStats.write(printer.format(r(0)))
				runStats.close()
				k += 1
			}

			nextGen

	}

	def replace(ordered: List[NodeSeq], mutated: List[Node], offspring: List[Node], newrandom: List[Node]): List[Node] =
	{
			val top = ordered.take(ordered.size/4)
			println("OFFSPRING SIZE = "+offspring.size)
			println("MUTATED SIZE = "+mutated.size)
			val lastGen = for( t <- top ) yield 
			{
						(t\"controller")(0)
			}

			lastGen ::: mutated ::: offspring ::: newrandom
//			mutated ::: offspring ::: newrandom
	}

	def makeOffspring(ordered: List[NodeSeq], evutils: EvolveUtils): List[Node] =
	{
		val numparents = ordered.size/2
		val parents = Random.shuffle(ordered.take(numparents/2) ::: ordered.takeRight(numparents/2))
		println("SHUFFLED TOP "+numparents+" =======================")
		for( p <- parents ) print( (p\"Rank").text + "->" + (p\"SurvivalRate").text +"  ")
		println
		var offspring = List[Node]()
		var i = 0;
		while( i < numparents)
		{
			println("Pairing "+ (parents(i)\"Rank").text + " with " + (parents(i+1)\"Rank").text )
			val off = (evutils.crossoverXML( 
				(parents(i)\"controller")(0), 
				(parents(i+1)\"controller")(0),
				List( (parents(i)\"controller"\"@id").text, (parents(i+1)\"controller"\"@id").text)
				)
			) 
			offspring = off :: offspring
			i = i+2
//			println("Ofspring size = "+offspring.size)
		}
		offspring
	}

	def mutate(ordered: List[NodeSeq], evutils: EvolveUtils): List[Node] =
	{
		val startpoint = ordered.size/2
		val endpoint = startpoint + startpoint/2
		val mutationCandidates = ordered.slice(startpoint,endpoint)
		println("Mutating ...")
		for( c <- mutationCandidates) print( (c\"Rank").text +"  " )
		println
//		for( c <- mutationCandidates) println( c\"controller" )
		(for( m <- mutationCandidates ) yield 
		 	evutils.mutateXML((m\"controller")(0), (m\"controller"\"@id").text)
		)


	}

	def crossover(p1: Node, p2: Node): NodeSeq =
	{
		val navigator1 = (p1.child(1)\"Navigation")(0)
		val navigator2 = (p2.child(1)\"Navigation")(0)

		val childController = navigator1

		val child = 
			<controller>
				<AgentSchema init="true" name="agentschema_1">
					{childController}
				</AgentSchema>
			</controller>

		child
		
		
	}
	
	def getOrderedResults(criteria: String, results: List[NodeSeq]): List[NodeSeq] =
	{
		val sorted = results.sortWith( 
			(a: NodeSeq, b:NodeSeq) => (a\criteria).text.toDouble > (b\criteria).text.toDouble 
		)
		var rank = -1
		val sortedPlusRank = for( s <- sorted ) yield 
		{
			rank = rank + 1
			<Results>{(s\\"Results")(0).child ++ <Rank>{rank}</Rank>}</Results>
		}
		sortedPlusRank
	}

}
