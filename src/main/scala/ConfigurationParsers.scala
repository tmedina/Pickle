package edu.uga.pickle.factories

import org.apache.logging.log4j.{Logger, LogManager};

import edu.uga.pickle.drivers.SimulationDriver
import edu.uga.pickle.application._
import edu.uga.pickle.body._
import edu.uga.pickle.actuators._

import scala.xml.{XML, NodeSeq, Node}

import scala.math.max

/** Parse the provided NodeSeq XML specification and store
	* all necessary parameters for Body creation.
	* TODO: make this a full fledged factory, that returns
	*	a Body based om the spec.
	*/
class BodyParameters(spec: NodeSeq, sim: SimulationDriver)
{
	val logger = LogManager.getLogger("Parsers")
	
	//Position
	val pos = spec\"StartingPosition"
	val x : Int = (pos\"X").text match
	{
		case "Random" => (sim.randomDouble * sim.width).toInt
		case "" => (sim.randomDouble * sim.width).toInt
		case x  => x.toInt
	}
	val y : Int = (pos\"Y").text match
	{
		case "Random" => (sim.randomDouble * sim.height).toInt
		case "" => (sim.randomDouble * sim.height).toInt
		case y => y.toInt
	}
	val rotation : Double = (pos\"R").text match
	{
		case "Random" => sim.randomDouble * 360
		case "" => sim.randomDouble * 360
		case r => r.toDouble
	}
	val size : Int = try {
			(spec\"Size").text.toInt
	} catch { case _ : Throwable => 0 }

	val turningRadius : Int = try {
			(spec\"TurningRadius").text.toInt
	} catch { case _ : Throwable => 0 }

	val maxSpeed : Double = try {
			(spec\"MaxSpeed").text.toDouble
	} catch { case _ : Throwable => 0 }

	val trailingLength : Int = try {
			(spec\"TrailingLength").text.toInt
	} catch { case _ : Throwable => 0 }

	var currentSpeed: Double = 0

	// Attributes
	var attributes = scala.collection.mutable.Map[String, Any]()
	val attrs = spec\"Attributes"\"Attribute"
	for (attr <- attrs) 
	{
		if( (attr\"@type").text.toLowerCase == "string" )
		{
			attributes put ((attr\"@key").text, attr.text)
		}
		else if( (attr\"@type").text.toLowerCase == "double" )
		{
			attributes put ((attr\"@key").text, attr.text.toDouble)
		}
		else if( (attr\"@type").text.toLowerCase == "int" )
			attributes put ((attr\"@key").text, attr.text.toInt)
	}
	//ensure defaults for colors
	if ( ! attributes.contains("Color") ) attributes.put("Color", "Black")
	if ( ! attributes.contains("DefaultColor") ) attributes.put("DefaultColor", attributes("Color"))
	if ( ! attributes.contains("BumpColor") ) attributes.put("BumpColor", attributes("Color"))
	attributes("Color") = attributes("DefaultColor")

}

/** Make and return a List of Sensors based on the provided XML spec.
	*/
class SensorFactory(spec: NodeSeq) 
{
	val logger = LogManager.getLogger("Parsers")
	private def getMaxSensorRange: Int =
	{
		var maxRange = 0
		for ( sensor <- spec\"_" )
		{
				val radius = try { (sensor\"@range").text.toInt }
					catch { case _: Throwable => { 0 } }
	
				maxRange = max(maxRange, radius)
		}
		maxRange
	}
	val maxSensorRange = getMaxSensorRange

	def build(owner: Agent): List[(String, (Agent, List[Phenomenon]) => List[(Point,Phenomenon)])] =
	{
		val sim: SimulationDriver = owner.sim
		var sensorList:List[(String, (Agent, List[Phenomenon]) => List[(Point,Phenomenon)])] = List()
		for (sensor <- spec\"_")
		{
			val name = (sensor\"@name").text
			val radius = try { (sensor\"@range").text.toInt }
				catch { case _: Throwable => { 0 } }
			val center = try { (sensor\"@center").text.toInt }
				catch { case _: Throwable => { 0 } }
			val offLeft = try { (sensor\"@left_offset").text.toInt }
				catch { case _: Throwable => { 0 } }
			val offRight = try { (sensor\"@right_offset").text.toInt }
				catch { case _: Throwable => { 0 } }

			def filters(filterList: NodeSeq): List[ (Phenomenon) => Boolean ] =
			{
				val generatedFilterList = (for( f <- filterList; if (! f.isInstanceOf[scala.xml.Text]) ) yield
				{
					f match
					{
						case filter @ <Filter/> => 
						{
							val lhs = (filter\"@lhs").text
							val rhs = (filter\"@rhs").text
							lhs match
							{
								case "type" =>
								{
									//println("Creating a type filter for == "+rhs)
									(p: Phenomenon) => {  p._type == rhs }
								}

								case _ =>
								{
									val criteriaType = (filter\"@type").text
									criteriaType match
									{
										case "String" => 
										{
											val op = (filter\"@op").text match
											{
												case "equals" => (a: String, b: String) => a == b
												case "notEqual" => (a: String, b: String) => a != b
											}
											(p:Phenomenon) => op(p.getAttribute(lhs).getOrElse("").toString, rhs.toString)
										}
										case "Integer" => 
										{
											val op = (filter\"@op").text match
											{
												case "equals" => (a: Int, b: Int) => a == b
												case "notEqual" => (a: Int, b: Int) => a != b
												case "greaterThan" => (a: Int, b: Int) => a > b
												case "lessThan" => (a: Int, b: Int) => a < b
											}
											(p:Phenomenon) => op(p.getAttribute(lhs).getOrElse("").asInstanceOf[Int], rhs.toInt)
										}
										case "Double" => 
										{
											val op = (filter\"@op").text match
											{
												case "equals" => (a: Double, b: Double) => a == b
												case "notEqual" => (a: Double, b: Double) => a != b
												case "greaterThan" => (a: Double, b: Double) => a > b
												case "lessThan" => (a: Double, b: Double) => a < b
											}
											(p:Phenomenon) => op(p.getAttribute(lhs).getOrElse("").asInstanceOf[Int], rhs.toDouble)
										}
									}
								}
							}
						}
						case other => 
						{
							(p: Phenomenon) => true
						}
					}
				}).toList
				generatedFilterList
			}

			val query : (Agent, List[Phenomenon]) => List[ (Point,Phenomenon)]  = (sensor\"_").head match
			{
				case <Nearest>{contents @ _*}</Nearest> =>
				{
					sim.getNearest(filters(contents), radius, center, offLeft, offRight)(_: Agent, _: List[Phenomenon])
				}
				case <All>{contents @ _*}</All> =>
				{
					sim.get(filters(contents), radius, center, offLeft, offRight)(_: Agent, _: List[Phenomenon])
				}
				case <Bump>{contents @ _*}</Bump> =>
				{
					sim.bumpSensor(contents.text)(_: Agent, _: List[Phenomenon])
				}
				case <Random>{contents @ _*}</Random> =>
				{
					val freq = try {
						contents.text.toDouble
					} catch {
						case _:Throwable => 
						{
							println("ERROR: Could not parse RandomSensor frequency as a Double: "+contents.text)
							System.exit(-1)
							0.0
						}
					}

					val active = try {
						((sensor.head)\"@active").text.toDouble	
						} catch {
							case _: Throwable =>
							{
								logger.error("ERROR: Could not parse RandomSensor active as a Double: "+((sensor.head)\"@active").text)
								System.exit(-1)
								0.0
							}
					}

//					val center = try {
//						((sensor.head)\"@center").text.toDouble	
//						} catch {
//							case _: Throwable =>
//							{
//								logger.error("ERROR: Could not parse RandomSensor center as a Double: "+((sensor.head)\"@center").text)
//								println("Value was: " +((sensor.head)\"@center").text )
//								System.exit(-1)
//								0.0
//							}
//					}


					sim.getRandom(freq, active, center)(_: Agent, _: List[Phenomenon])
				}
			}
			sensorList = (name, query) :: sensorList
		}
		sensorList
	}

}

/** Make and return a List of Actuators based on the provided XML spec.
	*/
class ActuatorFactory(spec: NodeSeq)
{
	val logger = LogManager.getLogger("Parsers")

	var actuators: List[Actuator] = List()

	def build(owner: Agent) : List[Actuator] = 
	{
		val sim: SimulationDriver = owner.sim
		for (actuator <- spec\"_")
		{
//			println(actuator)
			(actuator\"@type").text match
			{
				case "LazyNavigation" => {
					val speed_threshold = try { (actuator\"@speed_threshold").text.toDouble }
						catch {
							case _:Throwable => { println("WARNING: using default speed_threshold"); 0.1 }
						}
					val vector_threshold = try { (actuator\"@vector_threshold").text.toInt }
						catch {
							case _:Throwable => { println("WARNING: using default vector_threshold"); 20 }
						}
					val name = if ( (actuator\"@name").text == "" )
					{
						println("ERROR: Cannot build actuator for agent "+owner.id+" of type "+owner._type+". No name provided.")
						System.exit(-1)
						""
					} else (actuator\"@name").text

					actuators = (new LazyNavigation(/*owner.body.turningRadius*/10, speed_threshold, vector_threshold, name, owner, sim )) :: actuators
	
				}
				case "Chomp" => {
					val name = if ( (actuator\"@name").text == "" )
					{
						println("ERROR: Cannot build actuator for agent "+owner.id+" of type "+owner._type+". No name provided.")
						System.exit(-1)
						""
					} else (actuator\"@name").text

					actuators = (new Chomp(name, owner, sim)) :: actuators
				}
			}
		}

		actuators
	}
}

class ControllerParameters(controllerSpec: Node)
{
	val logger = LogManager.getLogger("Parsers")

}
