package edu.uga.pickle;

import org.apache.logging.log4j.{Logger, LogManager};

import edu.uga.pickle.application._
import edu.uga.pickle.factories._
import edu.uga.pickle.drivers.mason._
import edu.uga.pickle.body._
import edu.uga.pickle.actuators._
import edu.uga.pickle.controller.Controller

import scala.xml.{XML, NodeSeq}
import scala.math.{max}

import scala.language.postfixOps
import scala.annotation.tailrec

import java.util.NoSuchElementException
import scala.util.Random


/** Provides the main entry point for a Pickle simulation.
 *
 * @author: Terrance Medina, The University of Georgia
 * @contact: medinat_at_uga.edu
 * @date_created: 05 JAN 2015
 * @lastupdated: 15 MAR 2015
 *
 *	Parse an input specification file.
 */

object MainApplication
{
	def usage
	{
		println("Usage:")
		println("	pickle appName.xml (seed value)")
	}

	def main(args: Array[String])
	{
		val random = new Random
		val sim = new Simulator

		//get the input application XML or fail
		val appFile: String = args.length match
		{
			case 0 => 
			{
				println("ERROR: you must specify an application XML to run.");
				usage
				""
			}
			case _ => args(0)	
		}

		// get the input random generator seed, or make one
		val seed: String = args.length match
		{
			case 0 => "0"
			case 1 => random.nextInt(Int.MaxValue).toString
			case _ => args(1)
		}

		if(appFile != "")
			sim.runSimulation(Map("AppFile" -> appFile, "RandomSeed" -> seed))
	}
}

class Simulator
{



	def runForEvolution(simParams: Map[String,String], controllerMap: Map[String,NodeSeq]): Int = 
	{
//		val simParams = scala.collection.mutable.Map[String, String]()
//		simParams.put("AppFile", appFile)
//		simParams.put("RandomSeed", "0")
		runSimulation(simParams, controllerMap)

	}


	def runSimulation(
		args: Map[String, String], 
		controllerMap: Map[String, NodeSeq] = Map[String, NodeSeq]()
	) : Int =
	{

		val logger = LogManager.getLogger("Pickle_Main");
		logger.debug("Loading "+args("AppFile"))

		//Load application specification
		val spec = try {
				XML.loadFile(args("AppFile"));
			} catch {
				case e : Throwable => {
					println("Failed to load XML Application file"+ e );
					System.exit(-1)
					<empty/>
				}
			}
		logger.debug("Loaded XML spec")

		val displayWidth = try {
				(spec\"Display"\"width").text.toInt 
			} catch {
				case _:Throwable => {
					println("WARNING: using default display width of 640")
					640
				}
			}
		logger.debug("Display width = "+displayWidth)

		val displayHeight = try {
				(spec\"Display"\"height").text.toInt 
			} catch {
				case _:Throwable => {
					println("WARNING: using default display width of 480")
					480
				}
			}
		logger.debug("Display height = "+displayHeight)

		val simWidth = try {
				(spec\"World"\"width").text.toInt 
			} catch {
				case _:Throwable => {
					println("WARNING: using default field width of 640")
					640
				}
			}
		logger.debug("Sim width = "+simWidth)

		val simHeight = try {
				(spec\"World"\"height").text.toInt 
			} catch {
				case _:Throwable => {
					println("WARNING: using default field width of 480")
					480
				}
			}
		logger.debug("Sim height = "+simHeight)

		val simFriction = try {
				(spec\"World"\"friction").text.toDouble
			} catch {
				case _:Throwable => {
					println("WARNING: using default friction of 1")
					1
				}
			}
		logger.debug("Sim friction = "+simFriction)

		val simCD = (spec\"World"\"collision_detection").text match 
		{
			case "off" => false
			case _ => true
		}
		logger.debug("Sim Collision Detection = "+simCD)


		// Save the biggest of all Phenomena sizes
		def getMaxSize: Int =
		{
			var maxSize: Int = 0
			for ( a <- spec\"Agents"\"Agent" )
			{
				val s:Int = try { (a\"Size").text.toInt } 
				catch {
					case _ : Throwable => { 
						try { (a\"Size"\"@max").text.toInt }
						catch { 
							case _ : Throwable => { 0} }
					}
				}
//				println("Agent size = "+s)
	
				maxSize = max(maxSize, s)
//				println("Max size = "+maxSize)
			}
			for ( r <- spec\"Resources"\"Resource" )
			{
				val s:Int = try { (r\"Size").text.toInt } 
				catch {
					case _ : Throwable => { 
						try { (r\"Size"\"@max").text.toInt }
						catch { 
							case _ : Throwable => { 0} }
					}
				}
//				println("Resource size = "+s)
	
				maxSize = max(maxSize, s)
//				println("Max size = "+maxSize)
			}
			for ( o <- spec\"Obstacles"\"Obstacle" )
			{
				val s:Int = try { (o\"Size").text.toInt } 
				catch {
					case _ : Throwable => { 
						try { (o\"Size"\"@max").text.toInt }
						catch { 
							case _ : Throwable => { 0} }
					}
				}
//				println("Obstacle size = "+s)
	
				maxSize = max(maxSize, s)
//				println("Max size = "+maxSize)
			}
			maxSize
		}
		val maxSize = getMaxSize
		logger.debug("Max of all Phenomena sizes = "+maxSize)

		
		//Create simulation driver
		val sim = (spec\"Driver").text.toUpperCase match
		{
			//TODO: Refactor. We should just give the XML Node to the Driver and let the Driver parse it
			case "MASON" => 
			{
				
				if (args("RandomSeed").toLong != -1) 
				{
					println("!!!!!!!!!!!!!!!!!!!!!RandomSeed = "+args("RandomSeed"))
					new MasonDriver(simWidth, simHeight, simFriction, simCD, displayWidth, displayHeight, maxSize, args("RandomSeed").toLong)
				}
				else
					new MasonDriver(simWidth, simHeight, simFriction, simCD, displayWidth, displayHeight, maxSize)
			}
			case _	=> {
				logger.fatal("Failed to load simulation driver."); 
				System.exit(-1); 
				new MasonDriver
			}
		}
		sim.init




		//AGENTS
		//	Get all agent descriptions from the XML, create
		//	Sensor and actuator factories and create the Agent
		/////////////////////////////////////////////////
		val agents = spec\"Agents"\"Agent"
		for(agent <- agents)
		{
			val count = (agent\"Count").text.toInt
			for(i <- 0 until count)
			{
				// Load Controller XML Spec
				val controllerXML: scala.xml.NodeSeq = 
//					if( (agent\"@type").text == specifiedAgent )
					if( controllerMap.contains( (agent\"@type").text ) )
					{
						controllerMap( (agent\"@type").text ) 
					}
					else
					{
						try{
							(XML.loadFile((agent\"Controller"\"@file").text))\\"controller"
						}
						catch
						{
							case e : Throwable => 
							{
								println("Failed to load XML controller: "+e);
								System.exit(-1)
								<Empty/>
							}
						}
					}
//				println("Controller:")
//				println("===========")
//				println(controllerXML)

				val typeOfAgent = if ( (agent\"@type").text != "" )
				{
					(agent\"@type").text
				} else {
					println("ERROR: No agent type specified.")
					System.exit(-1)
					""
				}
				logger.debug("Creating "+typeOfAgent)

//				val printer = new scala.xml.PrettyPrinter(120,2)
//				println("typeOfAgent CONTROLLER XML =========================================")
//				println(printer.format(controllerXML(0)))

				val sensorFactory = new SensorFactory(agent\"Sensors")
				val actuatorFactory = new ActuatorFactory(agent\"Actuators")
				val bodyParams = new BodyParameters(agent, sim)

				val a = new Agent(typeOfAgent, bodyParams, sensorFactory, actuatorFactory, controllerXML, sim)

				sim.addAgents(a)
			}
		}

		//OBSTACLES
		//	Create obstacles from the XML spec
		/////////////////////////////////////////////////////
		val obstacles = spec\"Obstacles"\"Obstacle"
		for(obj <- spec\"Obstacles"\"Obstacle")
		{
			val count = (obj\"Count").text.toInt
			for( i <- 0 until count)
			{
				val bodyParams = new BodyParameters(obj, sim)
				val name = (obj\"@type").text
				val o = new Obstacle(name, bodyParams)
				sim.addObstacles(o)
			}

		}


		//RESOURCES
		val resources = spec\"Resources"\"Resource"
		for(resource <- resources)
		{
			val name = (resource\"@type").text
			val count = (resource\"Count").text.toInt
			for( i <- 0 until count)
			{
				val bodyParams = new BodyParameters(resource, sim)
				val res = new Resource(name, bodyParams)
				sim.addResources(res)
			}

		}



		// World Edges
		//	Add Obstacles around the edge of the simulated world.
		//////////////////////////////////////////////////
//		println("World bounded = "+ (spec\"World"\"bounded").text)
		if ( (spec\"World"\"bounded").text != "false" )
		{
			for ( x <- 0 until sim.width+1; y <- 0 until sim.height+1; if (x == 0 || y == 0 || x == sim.width || y == sim.height) )
			{
				val edgeBodyParamsSpec = <Agent type="Minnow">
					<Controller file="apps/MinnowController.xml" />
					<Count>3</Count>
					<Size>1</Size>
					<MaxSpeed>3</MaxSpeed>
					<TurningRadius>3</TurningRadius>
					<StartingPosition>
						<X>{x}</X>
						<Y>{y}</Y>
						<R>0</R>
					</StartingPosition>
	
					<Attributes>
						<Attribute type="string" key="Color">Black</Attribute>
					</Attributes>
					</Agent>

				val edgeBodyParams = new BodyParameters(edgeBodyParamsSpec, sim)

				val edge = new Obstacle("Edge", edgeBodyParams)
				sim.addObstacles(edge)
			}
		}


		val bgColor = if ((spec\"Display"\"BackgroundColor").text == "" ) "WHITE" else (spec\"Display"\"BackgroundColor").text
		val showIDs = if ( (spec\"Display"\"ShowAgentIDs").text == "true" ) true else false

		
		val gui = try {
			if(args("GUI") == "None") "HEADLESS" else (spec\"GUI").text.toUpperCase 
		} catch {
			case ex: NoSuchElementException =>
			{
				try {
					(spec\"GUI").text.toUpperCase
				} catch {
					case _: Throwable =>
					{
						println("WARN: No GUI specification found. Running without GUI.")
						"HEADLESS"
					}
				}
			}
		}
//		println("Gui = "+gui)

		gui match
		{
			//TODO: Refactor. We should just give the XML Node to the Driver and let the Driver parse it
			case "MASON" => 
			{
				sim runWithGUI(bgColor, showIDs)
				0
			}
			case "HEADLESS"	=> {
				val steps = try{
					args("SimulationSteps").toInt
					} catch
					{
						case _: Throwable =>
						{
							try {
								(spec\"Steps").text.toInt
							} catch
							{
								case _: Throwable => {
									println("Error: GUI-less mode found, but no number of steps specified in application.xml.")
									System.exit(-1)
									0
								}
							}
						}
				}
//				println("Running simulation with "+steps+" steps.")
				sim run steps
			}
		}
	}
}
