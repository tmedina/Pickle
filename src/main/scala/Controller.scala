package edu.uga.pickle.controller

import org.apache.logging.log4j.{Logger, LogManager};

import scala.annotation.tailrec
import scala.math.{sqrt, pow, log, ceil}
import scala.xml.{Elem, Node, NodeSeq}
import scala.language.postfixOps

import edu.uga.pickle.vector._
import edu.uga.pickle.body._
import edu.uga.pickle.application._
import edu.uga.pickle.drivers.SimulationDriver

/** Represents an Agent's decision-making process as an abstract-syntax tree.
 *
 * 	@author Terrance Medina, The University of Georgia
 * 	@contact: medinat_at_uga.edu
 * 	@date_created: 05 JAN 2015
 * 	@lastupdated: 17 MAR 2015
 *
 */

/** Interface for a node in the Controller abstract syntax tree. */
abstract class ASTNode
{
	//TODO: Coordinators don't need this, so we should purge it from the ASTNode
	val perceptualSchema: String
	def eval(ps: Map[String, List[(Point,Phenomenon)]]): Vector
}

/** Represents a MotorSchema as a leaf in the abstract syntax tree. 
 *	perceptualSchema: binds the MotorSchema by name to the Body Sensor that activates it.
 *	direction: either attraction or repulsion
 *		TODO: can we implement a "flanking" direction that will try to move around the phenomenon?
 */
case class MotorSchema(ps: String, direction: String, curveType: String, weight: Double, priority: Int) extends ASTNode
{
		val logger = LogManager.getLogger("MotorSchema")
		override val perceptualSchema = ps

		val curve: (Double) => Double = curveType.toLowerCase match
		{
			//Note, we always start with x = c^2, so take the sqrt lazily
			case "linear" => (x_2: Double) => sqrt(x_2)
			case "quadratic" => (x_2: Double) => x_2
			case "exponential" => (x_2: Double) => pow(2, sqrt(x_2))
			case "logarithmic" => (x_2: Double) => log(sqrt(x_2))
		}

		/** Return a Vector based on the given the data from a perceptualSchema. */
		override def eval(psMap: Map[String, List[(Point,Phenomenon)]] ): Vector =
		{
			for(t<-psMap) logger.debug(t)

			if( direction == "mimic" )
			{
				try {
					val sensorData = psMap(ps)
					val ((head,ph)::tail) = sensorData
	
					def sumOfVectors(input: List[Vector]): Vector =
					{
						@tailrec
						def sovWorker(aggregate: Vector, next: List[Vector]): Vector =
						{
							if( next.isEmpty ) aggregate
							else 
							{
								val (head::tail) = next
								sovWorker(aggregate + head, tail)
							}
						}
						
						sovWorker(new CartesianVector(0,0), input)
					}
	
					
					val neighborvectors = for { 
						sd <- sensorData 
						(point, phenom) = sd
					} yield phenom.vector
	
					sumOfVectors(neighborvectors)
				}
				catch
				{
					case _ : Throwable => 
					{
						logger.debug(perceptualSchema+" found nothing")
						val result = new CartesianVector(0,0,0)
						result
					}
				}

			}
			else if( direction == "attraction" || direction == "repulsion")
			{
				try{
					val sensorData = psMap(ps)
					val ((head,ph)::tail) = sensorData
					val distance: Double = head.x*head.x + head.y*head.y
	
					val r_x = try { 
						head.x/curve(distance) 
						} catch { 
							case _: Throwable => {
	//							println("Caught divide by 0")
								0
							}
					}
	
					val r_y = try { 
						head.y/curve(distance) 
						} catch { 
							case _: Throwable => {
	//							println("Caught divide by 0")
								0
							}
					}
					val result = new CartesianVector(r_x, r_y, priority)
					if ( direction == "attraction") result else result.invert.scale(weight)
				} 
				catch
				{
					case _ : Throwable => 
					{
						logger.debug(perceptualSchema+" found nothing")
						val result = new CartesianVector(0,0,0)
						result
					}
				}
			}
			else new CartesianVector(0,0,0)
		}
}



/** Represents a CoordinationOperator as an interior node in the abstract syntax tree. */
case class Coordinator( op: (Vector, Vector) => Vector, weight: Integer, priority: Integer ) extends ASTNode
{
	override val perceptualSchema = ""
	var children: List[ASTNode] = List()
	def addChild( child: ASTNode)
	{
		children = child :: children
	}

	override def eval(ps: Map[String, List[(Point,Phenomenon)]]): Vector =
	{
		@tailrec
		def doEval(remaining: List[ASTNode], result: Vector):Vector = remaining match
		{
			case Nil => result
			case x::xs => doEval(xs, op(result, x.eval(ps)))
		}

		doEval(children, new CartesianVector(0,0))
	}

}

/** Represents the abstract syntax tree for the Agent Controller. */
class AgentSchema(spec: Node)
{
	val logger = LogManager.getLogger("Controller")

	val name = (spec\"@name").text
	val isInit = if ((spec\"@init").text == "true") true else false

	var navigator = new Coordinator((a,b) => {a + b}, 1, 0)

	private val maxWeight = 
	try{
		(spec\\"MotorSchema"\\"@weight").toList.map(w => w.text.toDouble).max
	} catch {
		case _:Throwable => 0
	}


	/** Build the abstract syntax tree of the Navigator recursively.
		*/
	def buildNavigator(childNodes: List[Node], root: Coordinator): Unit = childNodes match
	{
		case Nil => 
		case x::xs =>
		{
			for(child <- x::xs)
			{
				child match
				{
					case Elem(_, "MotorSchema",_, _,  _*) => 
					{
						val ps = (child\"@PerceptualSchema").text
						val ms_type = (child\"@type").text
						val curve = (child\"@curve").text
						val weight = try {
							(child\"@weight").text.toDouble / maxWeight
						} catch { case _: Throwable => 1 }
						val priority = try {
							(child\"@priority").text.toInt
						} catch { case _: Throwable => 0 }

						var node = new MotorSchema(ps, ms_type, curve, weight, priority)
						root.addChild(node)
					}
					case Elem(_, "CoordinationOperator", _, _, _*) =>
					{
						val weight = try {
							(child\"@weight").text.toInt
						} catch { case _: Throwable => 0 }

						val priority = try {
							(child\"@priority").text.toInt
						} catch { case _: Throwable => 0 }

						val op: (Vector, Vector) => Vector = (child\"@type").text match
						{
							case "sum" => (a, b) => {a + b}
							case "priority" => (a, b) => {a ## b} 
						}
						var node = new Coordinator(op, weight, priority)

						buildNavigator((child\"_").toList, node)
						root.addChild(node)
					}
				}
			}
		}
	}

	if( (spec\"Navigation"\"_").toList.size != 0)
		buildNavigator((spec\"Navigation"\"_").toList, navigator)

	val stateChangeSchemas = for( sc <- (spec\"StateChange"\"_").toList ) yield new StateChangeSchema(sc)
	val actionSchemas = for( as <- (spec\"Action").toList ) yield new ActionSchema(as)



}

/** Binds a Body Sensor to a Body Actuator. The ActionSchema is fired by input from
 *	a body's Sensor (e.g. a bump sensor) and in turn, passes the associated SensorData
 *	(Point, Phenomenon) to an Actuator for processing. This is useful for activating
 *	the Chomp Actuator whenever a Bump Sensor is activated by a Prey.
 */
class ActionSchema(spec: NodeSeq)
{
	val name = (spec\"_"\"@name").text
	val subscribesTo = (spec\"_"\"@PerceptualSchema").text
	val actuator = (spec\"@actuator").text

	def eval(perceptualSchemas: Map[String, List[(Point,Phenomenon)]]): AgentAction =
	{
		val sensorData = perceptualSchemas(subscribesTo)
		//TODO: This only returns the first of any Phenomena, we need a 
		// more scalable decision here ...
		new AgentAction( actuator, sensorData(0)._2 )
	}
}

/** Change the active AgentSchema based on Senspr Input. For example, on seeing
 *	a predator, we may enter a new mode of behavior.
 */
class StateChangeSchema(spec: Node)
{
	val name = (spec\"@name").text
	val nextState = (spec\"@nextState").text
	val subscribesTo = (spec\"@PerceptualSchema").text
}


/** Represents the 'think' process for an agent. A Controller has a List of AgentSchemas,
	* one of which is currently active at any given time. It also has a Map of PerceptualSchema
	*	data which is refreshed by the Body's sensors at each time step.
	*/
class Controller(spec: NodeSeq, owner: Agent)
{

	val logger = LogManager.getLogger("Controller")

	var agentSchemas: List[AgentSchema] = for ( as <- (spec\"AgentSchema").toList )
		yield new AgentSchema(as)

	var activeAgentSchema: AgentSchema = agentSchemas(0)
	for ( as <- agentSchemas; if(as.isInit) )
		activeAgentSchema = as

	var perceptualSchemas : Map[String, List[(Point,Phenomenon)]] = Map()

	def toXML
	{
		//TODO: implement me
	}

	def toJSON
	{
		//TODO: implement me
	}

	/** Print out the AgentSchema abstract syntax tree to a debug log file. */
	def printSchema: Unit =
	{

		def doPrint(e:ASTNode): Unit = e match
		{
			case Coordinator(op, w, p) => 
			{
				val ee = e.asInstanceOf[Coordinator]
				for(c <- ee.children) doPrint(c)
				logger.debug("Coordinator: sum"+ee.perceptualSchema)
			}
			case MotorSchema(s,d,c,w,p) => 
			{
				val ee = e.asInstanceOf[MotorSchema]
				logger.debug("MotorSchema: "+s+" "+c+" "+d+" "+w+" "+p)
			}
		}

		doPrint(activeAgentSchema.navigator)
	}

	/** Check if any sensorData for stateChanging Schemas exist, and if 
	 *	so, change state accordingly.
	 */
	def checkStateChange(perceptualSchemas: Map[String, List[(Point,Phenomenon)]]): AgentSchema =
	{
		var result: AgentSchema = activeAgentSchema
		for ( stateChanger <- activeAgentSchema.stateChangeSchemas )
		{
			val shouldChangeState = try {
				perceptualSchemas(stateChanger.subscribesTo) != List()
			} catch {
				case _: Throwable => { false }
			}
			if ( shouldChangeState )
			{
				for( as <- agentSchemas; if(as.name == stateChanger.nextState ) )
				{
					result = as
				}
			}
		}
		result
	}

	/** Return a List of Actions, as the result of evaluating the 
		*	active AgentSchema's Navigator and polling All ActionSchemas.
	 	*/
	def run( sensorData: List[(String, List[(Point,Phenomenon)])] ): List[Action] = 
	{
			// convert a List to a Map
			perceptualSchemas = Map()
			for( t <- sensorData ) perceptualSchemas += t

			// Change active AgentSchema if necessary
			activeAgentSchema = checkStateChange(perceptualSchemas)

			// Run the navigator to get next desired vector
			val nextVector = activeAgentSchema.navigator.eval(perceptualSchemas)

			//Poll all ActionSchemas
			val actions : List[Action] = for 
			{ 
				as <- activeAgentSchema.actionSchemas; 
				if ( perceptualSchemas.contains(as.subscribesTo) )
				if ( perceptualSchemas(as.subscribesTo) != List() )
			} yield as.eval(perceptualSchemas)

			//Prepend the Navigation Action onto the List of AgentActions
			(new NavigationAction("Navigator", nextVector) :: actions)
	}
}

// Agent Actions
/** Abstract class to define information passed from a Schema to an Actuator.
	*/
abstract class Action(val actuator: String)

/** Passes information from a Navigator to an Actuator given by the String actuator parameter.
	*/
class NavigationAction(override val actuator: String, val vector: Vector)
extends Action(actuator)

/** Passes information from an ActionSchema to an Actuator named by the String actuator parameter.
	*/
class AgentAction(override val actuator: String, val target: Phenomenon)
extends Action(actuator)


