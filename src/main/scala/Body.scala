package edu.uga.pickle

import org.apache.logging.log4j.{Logger, LogManager};

import edu.uga.pickle.factories._
import edu.uga.pickle.actuators._
import edu.uga.pickle.application._

import scala.math.{abs}

/** Provides a body abstraction for Pickle Phenomena.
 *
 * @author: Terrance Medina, The University of Georgia
 * @contact: medinat_at_uga.edu
 * @date_created: 29 JAN 2015
 * @lastupdated: 15 MAR 2015
 *
 */
package body
{
/** Provides a 2D point for Pickle Simulations. 
 */
case class Point(var x: Double, var y: Double)






abstract class Body
{
	var currentLocation: Point
	var size: Int
	var attributes:scala.collection.mutable.Map[String, Any]
	//TODO: there needs to be some kind of NULL vector
	var currentSpeed: Double
	val maxSpeed: Double
	var rotation: Double

}









/** A Body encapsulates a set of parameters for a physical object in the simulated world. 
 */
class Body2D(
	x: Double, y: Double,
	var size: Int, val trailingLength: Int, 
	var currentSpeed: Double, val maxSpeed: Double,
	var rotation: Double,
	var attributes: scala.collection.mutable.Map[String, Any]
)
extends Body
{
	// initialize using a BodyParameters object
	def this(bodyParams: BodyParameters) = this (
		bodyParams.x, bodyParams.y, 
		bodyParams.size, bodyParams.trailingLength, 
		bodyParams.currentSpeed, bodyParams.maxSpeed,
		bodyParams.rotation,
		bodyParams.attributes
	)

	// default, null constructor
	def this() = this(0,0,0,0,0,0,0,scala.collection.mutable.Map[String, Any]())

	var currentLocation = new Point(x, y)
}













// adds sensors and actuators to a Body
class AgentBody2D(
	bodyParams: BodyParameters,
	sensorFactory: SensorFactory,
	actuatorFactory: ActuatorFactory,
	owner: Agent
)
extends Body2D(bodyParams)
{
	import edu.uga.pickle.application.Agent
	val maxSensorRange = sensorFactory.maxSensorRange
	var sensors: List[(String, (Agent, List[Phenomenon]) => List[(Point,Phenomenon)])] = sensorFactory.build(owner)
	var actuators: List[Actuator] = actuatorFactory.build(owner)
	var turningRadius = bodyParams.turningRadius
}

}//end package body

