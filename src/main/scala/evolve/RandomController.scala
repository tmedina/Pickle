package edu.uga.pickle.evolve;

import edu.uga.pickle.drivers.mason._
import scala.xml.Elem
import scala.xml.NodeSeq
import java.util.UUID


class RandomController(spec: NodeSeq, params: NodeSeq)
{
	val sim = new MasonDriver

	val sensors = for{ 
		s <- spec\"Sensors"\\"Sensor"
	} yield (s\"@name").text

	println("Sensors: "+sensors)

	val actuators = for{
		a <- spec\"Actuators"\\"Actuator"
		if ( (a\"@name").text != "Navigator" )
	} yield (a\"@name").text

	println("Actuators: "+actuators)

	val evolutionParams = params\"ControllerParameters"

	val maxAgentSchemas = ((evolutionParams\"AgentSchemas"\"Max").text).toInt
	println("maxAgentSchemas = "+maxAgentSchemas)

	val maxMS = ((evolutionParams\"NavigationController"\"MaxMSChildren").text).toInt
	println("maxMS = "+maxMS)

	val maxCO = ((evolutionParams\"NavigationController"\"MaxCOChildren").text).toInt
	println("maxCO = "+maxCO)

	val maxWeight = ((evolutionParams\"NavigationController"\"MaxWeight").text).toInt
	println("maxWeight = "+maxWeight)

	val maxNumberOfActions = ((evolutionParams\"Actions"\"Max").text).toInt
	println("maxActions = "+maxNumberOfActions)

	def navTree =
	{
		def makeCoordinationOperator(parentType: String = "", weightOrPriority:Int = 0): Elem =
		{
			val coType = if ( sim.randomDouble < 0.5 ) "priority" else "sum"
			
			def makeMotorSchema(parentType:String, weightOrPriority: Int) =
			{
				def makeMSType =
				{
					val flip = sim.randomDouble
					if( flip < 0.333 ) "attraction"
					else if( flip < 0.666 ) "repulsion"
					else "mimic"
				}
	
				def makeResponseCurve =
				{
					val flip = sim.randomDouble
					if ( flip < 0.333 ) "linear"
					else if ( flip < 0.666 ) "exponential"
					else "logarithmic"
				}

				def choosePS =
				{
					val choose = sim.randomInt(sensors.size)
					sensors(choose)
				}

				if( coType == "priority" )
					<MotorSchema type={makeMSType} curve={makeResponseCurve} priority={weightOrPriority.toString} PerceptualSchema={choosePS} />
				else if( coType == "sum" )
					<MotorSchema type={makeMSType} curve={makeResponseCurve} weight={weightOrPriority.toString} PerceptualSchema={choosePS} />
				else
					<MotorSchema type={makeMSType} curve={makeResponseCurve} PerceptualSchema={choosePS} />
			}
			parentType match
			{
				case "sum" =>
					<CoordinationOperator type={coType} weight={weightOrPriority.toString} >
						{for ( i <- 1 to sim.randomInt(maxMS)+1) yield makeMotorSchema(coType, sim.randomInt(maxWeight-1)+1) }
						{for ( i <- 1 to sim.randomInt(maxCO)) yield makeCoordinationOperator(coType, sim.randomInt(maxWeight-1)+1) }
					</CoordinationOperator>
				case "priority" =>
					val numMS = sim.randomInt(maxMS)+1
					<CoordinationOperator type={coType} priority={weightOrPriority.toString}>
						{for ( i <- 1 to numMS) yield makeMotorSchema(coType, i) }
						{for ( i <- 1 to sim.randomInt(2)) yield makeCoordinationOperator(coType, i+numMS) }
					</CoordinationOperator>
				case "" => //base case for root of navigation tree
					<CoordinationOperator type={coType} >
						{for ( i <- 1 to sim.randomInt(3)+1) yield makeMotorSchema(coType, i) }
						{for ( i <- 1 to sim.randomInt(2)) yield makeCoordinationOperator(coType, i) }
					</CoordinationOperator>
			}
		}

		makeCoordinationOperator()
	}

	def makeActions(action_limit: Int = 0) =
	{
		val n = if ( action_limit <= 0 ) 0 else sim.randomInt(action_limit)
		def actionSchema(num: Int) =
		{
			def chooseActuator =
			{
				val choose = sim.randomInt(actuators.size)
				actuators(choose)
			}

			def choosePerceptualSchema =
			{
				val choose = sim.randomInt(sensors.size)
				sensors(choose)
			}

			<Action actuator={chooseActuator}>
				<ActionSchema PerceptualSchema={choosePerceptualSchema}/>
			</Action>
		}
		for( i <- 1 to n ) yield actionSchema(i)
	}

	def agentSchemas(limit: Int = 0) =
	{
		var numAgentSchemas = 0
		val n = if ( limit <= 0 ) 1 else sim.randomInt(limit)+1
		val init = if ( limit <= 0 ) 1 else sim.randomInt(n)+1
		val action_limit = maxNumberOfActions

		def agentSchema(asNumber: Int) =
		{
			def makeASName: String = 
			{
				"agentschema_"+ asNumber
			}
		
			def isInit: String = 
			{
				if( asNumber == init ) "true" else "false"
			}

			def makeStateChanges =
			{
				<StateChange />
			}

			<AgentSchema name={makeASName} init={isInit}>
				<Navigation actuator="LazyNavigation">	
					{navTree}
				</Navigation>	

				{if(actuators.size > 0) makeActions(action_limit)}

				{makeStateChanges}

			</AgentSchema>
		}

		for( i <- 1 to n ) yield agentSchema(i)
	}

	def gen(seed: Double) =
	{

		var i = 0
		val as_limit = maxAgentSchemas
		val controller = 
			<controller id={UUID.randomUUID.toString}>
				{ agentSchemas(as_limit) }
			</controller>

//		val printer = new scala.xml.PrettyPrinter(80,2)
//		println(printer.format(controller))
		controller
	}
}
