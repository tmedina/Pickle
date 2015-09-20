package edu.uga.pickle.actuators

import org.apache.logging.log4j.{Logger, LogManager};

import edu.uga.pickle.body.Point
import edu.uga.pickle.vector._
import edu.uga.pickle.body._
import edu.uga.pickle.application._
import edu.uga.pickle.controller.{Action, AgentAction, NavigationAction}
import edu.uga.pickle.drivers.SimulationDriver

/** Implementation of agent actuators
 *
 * @author: Terrance Medina, The University of Georgia
 * @contact: medinat_at_uga.edu
 * @date_created: 05 JAN 2015
 * @lastupdated: 15 MAR 2015
 *
 */


abstract class Actuator(val name: String, val owner: Agent, val sim: SimulationDriver)
{
	val logger = LogManager.getLogger("Body")
	def subscribesTo(s: String)(s2:String): Boolean =
	{
		s == s2	
	}
//	def getAction(v: Vector): (SimulationDriver, Agent) => Unit
		def act(a: Action)
}

//TODO: just make this a curried function?
class LazyNavigation(var turningRadius: Double, val speed_threshold: Double, val vector_threshold: Int, override val name: String, override val owner: Agent, override val sim: SimulationDriver) 
extends Actuator(name, owner, sim)
{
	def act(inputact: Action)
	{
		logger.debug("Calling ACT: "+inputact)
		val input = inputact.asInstanceOf[NavigationAction]
		val nextVector = input.vector
		val p = owner
		if (nextVector.toCartesian.x != 0 || nextVector.toCartesian.y != 0 )
		{
				val turningRadius = p.body.turningRadius
				logger.debug(p.id+": rotation = "+p.rotation)
				logger.debug(p.id+": nextVector = "+nextVector)
				val speed = p.body.maxSpeed// * nextVector.toPolar.magnitude
				val currentVector = p.vector
	
				if ( (currentVector innerAngle nextVector) < turningRadius )
				{
					logger.debug(p.id+": normal turn")
					if ( (nextVector.toPolar innerAngle p.vector.toPolar ) > vector_threshold )
					{
						logger.debug("error = "+(nextVector.toPolar innerAngle p.vector.toPolar ))
						p.rotation = nextVector.toPolar.azimuth.degrees
						p.vector = nextVector.toCartesian
						p.speed = p.body.maxSpeed// * nextVector.toPolar.magnitude
					}
					if (p.speed < speed_threshold && nextVector.toPolar.magnitude != 0) 
					{
						p.speed = p.body.maxSpeed// * nextVector.toPolar.magnitude
						p.vector = nextVector.toCartesian
					}
				}
				else 
				{
					logger.debug(p.id+": compromised turn")
					val bestAvailableVector = if ( nextVector isLeftOf currentVector )
						new PolarVector(((p.rotation - turningRadius) + 360 ) % 360, speed)
					else
						new PolarVector((p.rotation + turningRadius) % 360, speed)
		
					if ( (bestAvailableVector.toPolar innerAngle p.vector.toPolar ) > vector_threshold )
					{
						logger.debug("error = "+(bestAvailableVector.toPolar innerAngle p.vector.toPolar ))
						p.rotation = bestAvailableVector.toPolar.azimuth.degrees
						p.vector = bestAvailableVector.toCartesian
						p.speed = p.body.maxSpeed
					}
					if (p.speed < speed_threshold && bestAvailableVector.toPolar.magnitude != 0) 
					{
						p.speed = p.body.maxSpeed
						p.vector = bestAvailableVector.toCartesian
					}
				}
		}
	}
}

class Chomp(override val name: String, override val owner: Agent, override val sim: SimulationDriver) extends Actuator(name, owner, sim)
{
	def act(agentinput: Action)
	{
//		println("CHOMP!!!!!!!!!!")
		val input = agentinput.asInstanceOf[AgentAction]

		val p = input.target
//		println("Calling chomp on "+input.target.id)
		try {
			sim.removePhenomenon(input.target)
		} catch {
			case e: Throwable => { println("ERROR: Failed to removePhenomenon: "+e) }
		}
	}

}

//TODO: implement me!
//class Grab(sim: SimulationDriver) extends Actuator
//{
//
//}
//
//class Release(sim: SimulationDriver) extends Actuator
//{
//
//}
