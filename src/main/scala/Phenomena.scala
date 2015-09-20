package edu.uga.pickle.application;

import org.apache.logging.log4j.{Logger, LogManager};

import java.util.HashMap

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.xml.{XML, Elem, Node, NodeSeq}
import scala.math.{ceil, abs, atan, tan, sqrt}

import edu.uga.pickle.body._
import edu.uga.pickle.factories._
import edu.uga.pickle.controller._
import edu.uga.pickle.vector._
import edu.uga.pickle.actuators._
import edu.uga.pickle.drivers.SimulationDriver


/** Provides the main agent and physical world abstractions for a Pickle
 *	simulation.
 *
 * @author: Terrance Medina, The University of Georgia
 * @contact: medinat_at_uga.edu
 * @date_created: 05 JAN 2015
 * @lastupdated: 17 MAR 2015
 *
 */

object Phenomenon
{
	var id : Int = 0
	def makeId =
	{
		id = id+1
		id
	}
}

/** Provides the super class of all Pickle entities that may be
 *	perceived by an Agent through its sensors.
 *	Attributes: a Map of String to Any value to represent query-able
 *	information (color, health, etc) about the Phenomena.
 */
abstract class Phenomenon(pType: String)
{
	val id = Phenomenon.makeId
//	println("Building Phenomenon "+id)
	val _type = pType
	var bumped = scala.collection.mutable.Map[String, (Boolean,Phenomenon)]()

	val body: Body
	// state information on the current grid position
	var currentLocation : Point
	var x: Double 
	var y: Double 
	var z: Double 

	var rotation: Double 
	var vector: Vector 

	var attributes: scala.collection.mutable.Map[String, Any] 
	var size: Int 

	//return the distance between the outer edges of two phenomena
	def distance(that: Phenomenon): Double =
	{
		val dx = this.currentLocation.x - that.currentLocation.x
		val dy = this.currentLocation.y - that.currentLocation.y
		// The distance between the outer edges of the bodies
		sqrt(dx * dx + dy * dy) - ((this.size + that.size).toDouble/2)
	}

	// return a deep copy of this Phenomenon
	def copy: Phenomenon

	def getAttribute(s:String) : Option[Any] = attributes.get(s)

}

/** Provides a Pickle agent -- an autonomous entity based around a
 *	sense-think-act cycle.
 */
case class Agent( 
	val agentType: String, 
	bodyParams: BodyParameters, 
	sensorParams: SensorFactory, 
	actuatorParams: ActuatorFactory, 
	controllerXML: NodeSeq, 
	sim: SimulationDriver
)
extends Phenomenon(agentType)
{
	val logger = LogManager.getLogger("Agent");

	logger.debug("Creating agent "+this.id)


	override def copy: Agent = new Agent(agentType, bodyParams, sensorParams, actuatorParams, controllerXML, sim)

	override val body = new AgentBody2D(bodyParams, sensorParams, actuatorParams, this)
	var currentLocation : Point = body.currentLocation
	var x: Double  = body.currentLocation.x
	var y: Double  = body.currentLocation.y
	var z: Double = 0 //TODO: implement for 3D space

	var rotation: Double = body.rotation
	var vector: Vector  = new PolarVector(bodyParams.rotation, 0)//new CartesianVector(0,0) //body.vector //NOT the same as roatation!

	var attributes: scala.collection.mutable.Map[String, Any]  = body.attributes
	var size: Int = body.size
	var speed = body.currentSpeed
	logger.debug("speed "+speed)

	val controller = new Controller(controllerXML, this)

	/** Return a List of Tuples that map Sensor names to the List of Phenomena and 
	 *	Point in Agent Space that	those sensors return.
	 */
	def sense : List[(String, List[(Point, Phenomenon)])]=
	{
		//limit number of queries on simulation layer by querying once with
		// the maximum of all sensor ranges, then filter based on that result
		val roughResults = sim.getRough(this) 

		// Pass a copy of 'this' Agent to each value function and return the 
		// result of each value function as a tuple mapped with its key.
		for(s <- body.sensors; (name, function) = s) yield (name, function(this, roughResults))
	}

	/** Return a Tuple of Vector and AgentAction based on the output of the Agent's controller. 
	 */
	def think( sensorData: List[(String, List[(Point,Phenomenon)])]) : List[Action] =
	{
//		println
//		println
//		println
//		println(sensorData)
//		println
//		println
//		println
		controller.run(sensorData)
	}

	/** Process the output of Controller.
	 *  Each actuator produces a function (SimulationDriver, Agent) => Unit, that applies changes to
	 *	the given simulation driver on behalf of the given agent.
	 */
	def act(actionInputs: List[Action])
	{
			for {
				actuator <- body.actuators
				input <- actionInputs
				if ( input.actuator == actuator.name )
			} actuator.act(input)
	}
	
}


/** A Cludge for passing sensor data where there is no actual Phenomenon e.g. Random, or Centroid
	* TODO: need to find a better way, keep one phantom and share it?
	*/
case class Phantom(phantomType: String) extends Phenomenon("Phantom")
{
	override def copy: Phenomenon = new Phantom(phantomType)
	val body = new Body2D()
	var currentLocation : Point = body.currentLocation
	var x: Double  = body.currentLocation.x
	var y: Double  = body.currentLocation.y
	var z: Double = 0 //TODO: implement for 3D space

	var rotation: Double = body.rotation
	var vector: Vector  = new CartesianVector(0,0) //body.vector //NOT the same as roatation!

	var attributes: scala.collection.mutable.Map[String, Any]  = body.attributes
	var size: Int = body.size

}

/** An object in the simulation that can be consumed, e.g. food pellets.
 */
case class Resource(resourceType: String, bodyParams: BodyParameters) extends Phenomenon(resourceType)
{
	override def copy: Phenomenon = new Resource(resourceType, bodyParams)
	override val body = new Body2D(bodyParams)
	var currentLocation : Point = body.currentLocation
	var x: Double  = body.currentLocation.x
	var y: Double  = body.currentLocation.y
	var z: Double = 0 //TODO: implement for 3D space

	var rotation: Double = body.rotation
	var vector: Vector  = new CartesianVector(0,0) //body.vector //NOT the same as roatation!

	var attributes: scala.collection.mutable.Map[String, Any]  = body.attributes
	var size: Int = body.size
}

/** A static, non-changing object in the simulated environment.
	*/
case class Obstacle( resourceType: String, bodyParams: BodyParameters) extends Phenomenon(resourceType)
{
	override def copy: Phenomenon = new Obstacle(resourceType, bodyParams)
	override val body = new Body2D(bodyParams)
	var currentLocation : Point = body.currentLocation
	var x: Double  = body.currentLocation.x
	var y: Double  = body.currentLocation.y
	var z: Double = 0 //TODO: implement for 3D space

	var rotation: Double = body.rotation
	var vector: Vector  = new CartesianVector(0,0) //body.vector //NOT the same as roatation!

	var attributes: scala.collection.mutable.Map[String, Any]  = body.attributes
	var size: Int = body.size
}


