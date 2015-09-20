package edu.uga.pickle;

import edu.uga.pickle.drivers.SimulationDriver
import edu.uga.pickle.application._
import edu.uga.pickle.body._
import edu.uga.pickle.vector._

import org.apache.logging.log4j.{Logger, LogManager};

import ec.util.MersenneTwisterFast;

import sim.engine.{Steppable, Stoppable, SimState};
import sim.util.{MutableDouble2D, Double2D, Bag};
import sim.field.continuous.Continuous2D;

import java.awt.Color;
import java.util.Calendar
import java.io.{PrintWriter, FileWriter, BufferedWriter, File}

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.math.{sqrt, log10, log1p, log, max}


/** Provides a middle layer for interfacing the Pickle ABM with the MASON
 *  simulation package from George Mason University.
 *
 * @author: Terrance Medina, The University of Georgia
 * @contact: medinat_at_uga.edu
 * @date_created: 05 JAN 2015
 * @lastupdated: 11 MAR 2015
 *
 */
package drivers.mason
{

/** A utility handle to MASON's SimState.
 */
class MasonSimulation(seed: Long, val w: Int, val h: Int, val friction: Double, discretization: Int, val dW: Int, val dH: Int) extends SimState(seed)
{
	val logger = LogManager.getLogger("SimulationDriver")
	val field: Continuous2D = new Continuous2D(discretization, w, h)
	override def start
	{
	}
	override def finish
	{
	}
}

/** A utility wrapper for interfacing Pickle agents with MASON.
 */
class MasonAgent(a: Agent) extends Steppable
{
	val agent = a
	def step(s: SimState)
	{
		agent act(agent think( agent sense ))
	}
}

class MasonUpdater(sim: MasonDriver) extends Steppable
{
		
	def step(s: SimState)
	{
		//Check all Agents for move updates
		for( agent <- sim.agents )
		{
			val nextX = agent.vector.enlarge(agent.speed).toCartesian.x.asInstanceOf[Double]
			val nextY = agent.vector.enlarge(agent.speed).toCartesian.y.asInstanceOf[Double]
			val decel = (agent.speed * sim.sim.friction)
//			println("speed = "+agent.speed+"        decel = "+decel)
//			println("("+agent.currentLocation.x+","+agent.currentLocation.y+")")
			agent.speed = agent.speed - decel
			sim.put(agent, new Point(agent.currentLocation.x + nextX, agent.currentLocation.y + nextY))
		}
		//Calculate position updates based on current position, speed and direction
		//Update all agents in the field

	}
}

/** A utility wrapper for interfacing Pickle resources with MASON.
 */
private class MasonResource(r: Resource) extends Steppable
{
	def step(s:SimState)
	{
		
	}
}

/** A utility wrapper for interfacing Pickle obstacles with MASON.
 */
private class MasonObstacle(o: Obstacle) extends Steppable
{
	def step(s:SimState)
	{

	}
}


/** provides an interface with the MASON sim package.
 *  TODO: lazy instantiation, so I can pass in w,h and seed later
 */
class MasonDriver(val width: Int = 640, val height: Int = 480, friction: Double = 1.0, collisionDetection: Boolean = true, dW: Int = 640, dH: Int = 480, val maxSize: Int = 0, seed: Long = Calendar.getInstance.getTime.getTime) extends SimulationDriver
{
	println("RealSeed = "+seed)
	val logger = LogManager.getLogger("SimulationDriver")
//	val survivalLogger = new PrintWriter(new BufferedWriter(new FileWriter(new File("logs/survivors.log"), true)))
	val survivalLogger = new PrintWriter(new FileWriter(new File("logs/survivors.log"), true))

//	println("Seed = "+seed)
	val random = new MersenneTwisterFast(seed)
	val sim = new MasonSimulation(seed, width, height, friction, 1, dW, dH)

	var nextMoveCandidates = scala.collection.mutable.Map[Phenomenon, Point]()
	var survivingAgents = 0;

	var agentCheckList = scala.collection.mutable.Map[Agent,Boolean]()

	var agents = List[Agent]()
	var agentStoppers = scala.collection.mutable.Map[Agent, Stoppable]()

	def randomDouble: Double =
	{
		random.nextDouble
	}

	def randomInt(lim: Int): Int =
	{
		random.nextInt(lim)
	}

	def init {
		sim.start()
		sim.schedule.scheduleRepeating(new MasonUpdater(this))
	}

	def markReady(a: Agent)
	{
		try{
			agentCheckList(a) = true
		} catch {
			case e: Throwable =>
			{
				println("ERROR: markReady called on a non-existing Agent. "+e);
			}

		}
	}

	def addAgents(agentsToAdd : Agent*)
	{
		for(agent <- agentsToAdd) 
		{
			val masonAgent = new MasonAgent(agent)
			val start = agent.currentLocation
			sim.field.setObjectLocation(agent, new Double2D(start x, start y))
			agentStoppers(agent) = sim.schedule.scheduleRepeating(masonAgent)
			logger.debug("Adding agent "+ agent.id+" at ("+start.x+","+start.y+")")
			survivingAgents += 1
//			println("Adding agent. Total = "+survivingAgents)
			agents = agent :: agents
		}
	}

//	def addListOfAgents(agents : List[Agent])
//	{
//		for(agent <- agents) 
//		{
//			val masonAgent = new MasonAgent(agent)
//			val start = agent.currentLocation
//			sim.field.setObjectLocation(agent, new Double2D(start x, start y))
//			sim.schedule.scheduleRepeating(masonAgent)
//			logger.debug("Adding agent "+ agent.id+" at ("+start.x+","+start.y+")")
//			survivingAgents += 1
//		}
//	}

	def addListOfResources(resources : List[Resource])
	{
		for(resource <- resources) 
		{
			val masonResource = new MasonResource(resource)
			val start = resource.currentLocation
			sim.field.setObjectLocation(resource, new Double2D(start x, start y))
			sim.schedule.scheduleRepeating(masonResource)
			logger.debug("Adding resource "+ resource.id+" at ("+start.x+","+start.y+")")
		}
	}

	def addResources(resources : Resource*)
	{
		for(resource <- resources) 
		{
			val masonResource = new MasonResource(resource)
			val start = resource.currentLocation
			sim.field.setObjectLocation(resource, new Double2D(start x, start y))
			sim.schedule.scheduleRepeating(masonResource)
			logger.debug("Adding resource "+ resource.id+" at ("+start.x+","+start.y+")")
		}
	}


	def addListOfObstacles(obstacles : List[Obstacle])
	{
		for(obstacle <- obstacles) 
		{
			val masonObstacle = new MasonObstacle(obstacle)
			val start = obstacle.currentLocation
			sim.field.setObjectLocation(obstacle, new Double2D(start x, start y))
			sim.schedule.scheduleRepeating(masonObstacle)
			logger.debug("Adding obstacle "+ obstacle.id+" at ("+start.x+","+start.y+")")
		}
	}

	def addObstacles(obstacles : Obstacle*)
	{
		for(obstacle <- obstacles) 
		{
			val masonObstacle = new MasonObstacle(obstacle)
			val start = obstacle.currentLocation
			sim.field.setObjectLocation(obstacle, new Double2D(start x, start y))
			sim.schedule.scheduleRepeating(masonObstacle)
			logger.debug("Adding obstacle "+ obstacle.id+" at ("+start.x+","+start.y+")")
		}
	}










	def run (stepsLimit: Int): Int = 
	{
		var steps: Long = 0;
//		var oldSA = survivingAgents
		do
    {
      if (!sim.schedule.step(sim)) return -1
      steps = sim.schedule.getSteps()
//      if (steps % 1000 == 0)
//        logger.debug("Steps: " + steps + " Time: " + sim.schedule.getTime());
//				println("Surviving Agents: "+survivingAgents)
//			if(survivingAgents != oldSA)
//			{
//				println("Surviving Agents: "+survivingAgents)
//				oldSA = survivingAgents;
//			}
    }
    while(steps < stepsLimit);
		
		println("SurvivingAgents: "+survivingAgents)
		survivalLogger.println(seed+"\t"+survivingAgents);
		survivalLogger.flush();
		survivalLogger.close();

		survivingAgents

	}

	def runWithGUI(bgColor: String, showIDs: Boolean)
	{
		val gui = new MasonGUI(sim, bgColor, showIDs)
		gui.createController()
	}



/********************************************************************************
						COLLISION DETECTION
********************************************************************************/
	/** Return a tuple (Boolean, Point) where Boolean tells us if p will collide with
		*	an inanimate object at newLocation, and Point tells us the best available
		*	point along p's intended path without colliding wih the inanimate object.
		*/
	def collidesWithInanimate( p: Phenomenon, newLocation: Double2D): (Boolean,Point) = 
	{
		def toPoint(d: Double2D) = new Point(newLocation.x.toInt, newLocation.y.toInt)

		//TODO can I somehow reuse the rough sensor input here?
		val searchRadius = p.size+maxSize
		val neighbors = sim.field.getObjectsWithinDistance(newLocation, p.size+maxSize)
//		println("Collision search radius: "+p.size+" + "+maxSize+" = "+searchRadius)


		var possibleMoves: List[(Double, Point)] = Nil
		for ( thing <- neighbors.objs )
		{
			if (thing != p)
			if (thing.isInstanceOf[Obstacle] || thing.isInstanceOf[Resource])
			{
				val inanimate = thing.asInstanceOf[Phenomenon]
				val newLoc2P = toPoint(newLocation)
				val theyCollided = collides(inanimate, inanimate.currentLocation,  p, newLoc2P)
				// if there is a collision, find the best available new position for the agent
				if (theyCollided)
				{
					try{
						p.bumped(inanimate.asInstanceOf[Phenomenon]._type) = (true,inanimate.asInstanceOf[Phenomenon])
					}
					catch {
						case _: Throwable => 
						{
							p.bumped.put(inanimate.asInstanceOf[Phenomenon]._type, (true, inanimate.asInstanceOf[Phenomenon]) )
						}
					}
//					val adjustedDistance = p.distance(inanimate) - ((p.size + inanimate.size)/2.0)
					val adjustedDistance = p.distance(inanimate)
					//if the agent is already as close as possible to the inanimate, keep it there
					if (adjustedDistance < 4 )
					{
//						println("Last Agent Vector: "+p.vector)
//						println("Distance to inanimate: "+p.distance(inanimate))
//						println("Adjusted Distance: "+adjustedDistance)
						possibleMoves = (adjustedDistance, new Point(p.currentLocation.x, p.currentLocation.y)) :: possibleMoves
					}
					else
					{
						//calculate the same vector it was heading in, but shrink it to the size of adjustedDistance
//						val adjustedMove = new CartesianVector(newLoc2P.x - p.currentLocation.x , newLoc2P.y - p.currentLocation.y).adjust(adjustedDistance)
						val adjustedMove = p.vector.toCartesian.adjust(adjustedDistance)
						val adjustedCoords = new Point(p.currentLocation.x + adjustedMove.x.toInt, p.currentLocation.y+adjustedMove.y.toInt) 
//						println("Last Agent Vector: "+p.vector)
//						println("Adjusted Distance: "+adjustedDistance)
//						println("Adjusted Move: "+adjustedMove)
//						println("Adjusted Coords: "+adjustedCoords)
						possibleMoves = (adjustedDistance, adjustedCoords) :: possibleMoves
					}
				}

			}
		}

		//if there are no possibleMoves then there is no collision, so return the desired newLocation
		//otherwise choose the the possibleMove that brings the agent as close as possible to 
		//the inanimate it is colliding with
		if (possibleMoves.size != 0) 
		{
			//
			val shortest = possibleMoves.reduceLeft((a,b) => ( a._1 min b._1, if (a._1 < b._1) a._2 else b._2 ))._1
			val move = possibleMoves.reduceLeft((a,b) => ( a._1 min b._1, if (a._1 < b._1) a._2 else b._2 ))._2
//			println("Collision: true, Move: "+move)
//			println("Last Agent Vector: "+p.vector)
			(true, move)
		}
		else 
		{
//			println("Collision: false, Move: "+toPoint(newLocation))
//			println("Last Agent Vector: "+p.vector)
			(false, toPoint(newLocation))
		}
	}


	/*
	 *	Return true if p at newLocation would collide with another Agent in the field.
	 */
	def collidesWithAgent( p: Phenomenon, newLocation: Double2D): Boolean = 
	{
		val result = try
		{
			val neighbors = sim.field.getObjectsWithinDistance(newLocation, maxSize)
			val filtered = 
				for {
					thing <- neighbors.objs
					if ( thing.isInstanceOf[Phenomenon])
					thingAsP = thing.asInstanceOf[Phenomenon]
					if (thingAsP != p)
					if ( thing.isInstanceOf[Agent] )
					if ( collides(p, nextMoveCandidates(p), thingAsP, nextMoveCandidates(thingAsP)) )
				} yield thing
	
			// set bump state on agent
			for(thing <- filtered)
			{
				try {
					p.bumped(thing.asInstanceOf[Phenomenon]._type) = (true,thing.asInstanceOf[Phenomenon])
				}
				catch {
					case _: Throwable => 
					{
						p.bumped.put(thing.asInstanceOf[Phenomenon]._type, (true, thing.asInstanceOf[Phenomenon]) )
					}
				}
			}
	
			filtered.size != 0
		} catch {
			case e: Throwable => { 
				println("Threw error on collidesWithAgent: "+e.printStackTrace())
				false;
			}
		}
		result
	}

	/*
	 * Return true if agent a at aLoc collides with agent b at bLoc.
	 */
	def collides(a: Phenomenon, aLoc: Point, b: Phenomenon, bLoc: Point): Boolean =
	{
		val dx = aLoc.x - bLoc.x 
		val dy = aLoc.y - bLoc.y 
		val dist = sqrt( (dx * dx) + (dy * dy) )
//		println("Collision Check: a.size = "+a.size)
//		println("Collision Check: b.size = "+b.size)
//		println("Collision Check: dist = "+dist)

		if (dist < (a.size + b.size)/2.0 ) true 
		else false

	}

//	def resolveCollisions
//	{
//		// Produce an unordered cross-product of all agents, discarding duplicate pairings
//		val checklist: List[List[Phenomenon]] = (((
//			for {
//				agent1 <- nextMoveCandidates.keys
//				agent2 <- nextMoveCandidates.keys
//				if (agent1 != agent2)
//			}
//			yield Set(agent1, agent2) 
//		).toSet).map((s: Set[Phenomenon]) => s.toList)).toList
//
////		println("checklist = "+checklist)
//
//		for {
//			agentPair <- checklist
//			if ( collidesWith( 
//												 agentPair(0).body.at(nextMoveCandidates(agentPair(0))), 
//												 agentPair(1).body.at(nextMoveCandidates(agentPair(1))) 
//											 ) 
//				 ) 
//		}
//		{
//			//TODO: randomly choose a loser
//			nextMoveCandidates(agentPair(0)) = agentPair(0).currentLocation
//		}
//
//		//  a List of edges in the rollback graph
////		val rollbacksRough = (for {
////			agent <- checklist
////			if (agent(0).currentLocation.couldCollideWith(nextMoveCandidates(agent(1))))
////		}
////		yield 
////		{
//////			println("Rollback check: "+agent(0).id+" X "+agent(1).id);
////			(agent(1).id, agent(0).id)
////		}).toList
//
////		val collisionsRough = (for {
////			agent <- checklist
////			if (nextMoveCandidates(agent(0)).couldCollideWith(nextMoveCandidates(agent(1))))
////		}
////		yield (agent(0).id,agent(1).id)).toList
////
////		val rollbacksActual = List()
//		val collisionsActual = List()
//
////		println("rollbacksRough = "+rollbacksRough)
////		println("collisionsRough = "+collisionsRough)
//	}

	/** updates the Phenomenon's position in the MASON field.
	 * 
	 *	1. cache the update in a Map: agent.id -> Point
	 *	2. if all agents have submitted an update:
	 *		2a. check each update against collision with an inanimate object
	 *		2b. if they collide, scrap the update
	 *		2c. check each update against collision with an agent update (N x N operations)
	 *		2d. if they collide, randomly choose a loser and scrap it
	 *	3. commit all remaining updates
	 */
	def put(p: Phenomenon, newLocation: Point)
	{
		try {
		
		if ( ! collisionDetection )
		{
				//WITHOUT COLLISION DETECTION
				val agent = p.asInstanceOf[Agent]
				val newLocationAsD2D = new Double2D(newLocation.x, newLocation.y)
				sim.field.setObjectLocation(agent, newLocationAsD2D)
				agent.currentLocation = newLocation
		}
		//TODO: Requesting Agent needs to be synchronized with the Driver
		//	i.e agent cannot begin another STA cycle until the update completes
		//	Do this with Actors (for multithreading only)
		//DONE: cache the update and apply all updates at once within a given window
		//TODO: collisionDetect and validate the newLocation. If successful, 
		// update the Phenomenon's position, then update the field

		else
		{
			//WITH COLLISION DETECTION
			nextMoveCandidates += (p -> newLocation)
	
			if (nextMoveCandidates.keys.size == survivingAgents)
			{
				// initialize bump sensor on all agents
				for (agent <- nextMoveCandidates.keys) 
				{
					for((key,value) <- agent.bumped)
						agent.bumped(key) = (false, new Phantom("bumpPhantom"))
				}

				// check for and adjust inanimate collisions
				for (agent <- nextMoveCandidates.keys)
				{
					val newLocationAsD2D = new Double2D((nextMoveCandidates(agent)).x, (nextMoveCandidates(agent)).y)
					val (theyCollided, adjustedResult) = collidesWithInanimate(agent, newLocationAsD2D)
					if (theyCollided)
					{
//						println("Collision detected for agent "+agent.id)
						//TODO: activate bumpSensor on Agent
//						agent.bumped = true
						nextMoveCandidates(agent) = adjustedResult
					}
				}
	
				// resolveCollisions
				for (agent <- nextMoveCandidates.keys)
				{
					val newLocationAsD2D = new Double2D((nextMoveCandidates(agent)).x, (nextMoveCandidates(agent)).y)
					if ( ! collidesWithAgent(agent, newLocationAsD2D))
					{
						sim.field.setObjectLocation(agent, newLocationAsD2D)
						agent.currentLocation = nextMoveCandidates(agent)
	
					}
					else
					{
						//TODO: activate bumpSensor on both Agents
						//TODO: discard a random loser
//						println("Refusing to move agent "+agent.id+" because of collision.");
//						agent.bumped = true
						nextMoveCandidates(agent) = agent.currentLocation
					}
//					println(agent.id+": current = "+agent.currentLocation)
				}
	
				//Reset
				nextMoveCandidates = scala.collection.mutable.Map[Phenomenon, Point]()
			}
		}//end if collision detection
		} catch {
			case e: Throwable => {
				println("ERROR: MasonDriver failed in put operation: "+e)
			}
		}

	}

	override def bumpSensor(target: String)(caller: Agent, empty: List[Phenomenon]): List[(Point,Phenomenon)] =
	{
		try{
			val (wasBumped, bumper) = caller.bumped(target)
			if(wasBumped)
			{
				caller.attributes("Color") = caller.attributes("BumpColor")
//				println(caller.id +" Bumped! returning "+bumper.id)
				caller.bumped.put(target, (false, new Phantom("bump")))
				List( (new Point(bumper.currentLocation.x, bumper.currentLocation.y), bumper) )
			}
			else
			{
				caller.attributes("Color") = caller.attributes("DefaultColor")
				List()
			}
		} catch {
			case e: Throwable => {
//				println("Err: "+e)
				caller.bumped.put(target, (false, null))
				List()
			}
		}
	}

	def removePhenomenon(p: Phenomenon)
	{
		val agentToRemove = try {
			p.asInstanceOf[Agent]
		} catch {
			case _: Throwable =>
			{
				return
				null
			}
		}

		try
		{
			agents = agents.filter( _ != p.asInstanceOf[Agent])
			agentStoppers(p.asInstanceOf[Agent]).stop
			survivingAgents -= 1
//			println("Removing agent. Total = "+survivingAgents)
			sim.field.remove(p)
		} catch
		{
			case _: Throwable =>
			{
				println("WARNING: Foiled attempt to double free an agent!")
			}
		}
	}

	def getRough(caller: Agent): List[Phenomenon] =
	{
		try {
	//		println("Agent "+caller.id+" called getRough with maxSensorRange = "+caller.body.maxSensorRange)
			val from = new Double2D(caller.currentLocation.x, caller.currentLocation.y);
			val maxRange = caller.body.maxSensorRange+((caller.size+maxSize)/2)
			val result = sim.field.getObjectsWithinDistance(from, maxRange).objs.toList
	
			for { 
				r <- result 
				if ( r != null ) 
				p = r.asInstanceOf[Phenomenon]
				if ( p.distance(caller) <= caller.body.maxSensorRange)
			} yield r.asInstanceOf[Phenomenon]
		} catch {
			case e: Throwable =>
			{
				println("ERROR: getRough threw exception: "+e)
				List()
			}
		}
	}

	/** return a list of Points for all Phenomena within sensor range
	 *	of the calling Agent. Filter the results by applying each
	 *	of the supplied filter functions.
	 *
	 *	TODO: cache the initial bag and map it to caller id.
	 *		If the same agent calls 'get' within the same move-cycle
	 *		i.e., is querying from multiple sensors, just lookup
	 *		and reuse the cached bag
	 */
	def get(filters: List[(Phenomenon) => Boolean], range: Int, center: Int, offsetL: Int, offsetR: Int )(caller: Agent, rough: List[Phenomenon]) : List[(Point,Phenomenon)] =
	{
//		println(caller.id+" calling get "+filters)
//		val from = new Double2D(caller.currentLocation.x, caller.currentLocation.y);

		val bag = getWithFilters(filters)(caller, rough)
		val callerX = caller.currentLocation.x
		val callerY = caller.currentLocation.y
		val from = new Double2D(caller.currentLocation.x, caller.currentLocation.y);

		//MASON tells us absolute locations on the field
		//we need to convert these to caller's POV before passing to the sensors
//		println("range = "+range)
//		println("center = "+center)
//		println("offsetL = "+offsetL)
//		println("offsetR = "+offsetR)
//				val leftV = new PolarVector( (center-offsetL+360)%360, 1)
//				val rightV = new PolarVector( (center+offsetR+360)%360, 1)
//				println("leftVector: "+(center-offsetL+360)%360)
//				println("rightVector: "+(center-offsetR+360)%360)

		val result = for{
			b<-bag
//			dist = caller.distance(b)
			// return a point relative to the caller's location
//			x = b.currentLocation.x - caller.currentLocation.x
//			y = b.currentLocation.y - caller.currentLocation.y
//			r = dist + caller.size/2.0
//			v = (new CartesianVector(x, y)).adjust(r)
//			d = v.adjust(r)
//			// filter out phenomena outside of sensor range
//				if ( r <= range )
//				leftV = new PolarVector( (center-offsetL+360)%360, 1)
//				rightV = new PolarVector( (center+offsetR+360)%360, 1)
//				if ( (offsetL + offsetR >= 360) || ((v isLeftOf rightV) && (v isRightOf leftV)) )
				bX = b.currentLocation.x
				bY = b.currentLocation.y
				// filter out phenomena outside of sensor range
				if ( from.distance(new Double2D(bX, bY)) <= range ) 
				rotation = caller.body.rotation
				vector = new CartesianVector(bX - callerX, bY - callerY)
				povVector = new PolarVector(vector.toPolar.azimuth.degrees - rotation, vector.toPolar.magnitude)
				leftV = new PolarVector( (center-offsetL+360)%360, 1)
				rightV = new PolarVector( (center+offsetR+360)%360, 1)
				if ( (offsetL + offsetR >= 360) || ((povVector isLeftOf rightV) && (povVector isRightOf leftV)) )

		} yield (new Point(bX, bY), b)


		result


	}

	def getRandom(freq: Double, active: Double, center: Double)(caller: Agent, rough: List[Phenomenon]) : List[(Point,Phenomenon)] =
	{
		if (this.randomDouble > freq ) List()//List(caller.vector.toPoint(caller))
		else
		{
			val _active = if ( active == 0 ) 1 else active
			val d = if (this.randomDouble > 0.5) 1 else -1
			val randDir = caller.vector.toPolar.azimuth.degrees + (((this.randomDouble *100) % _active)* d)
			val v = (new PolarVector(randDir, caller.body.maxSpeed)).toCartesian
			val r = new Point(v.x, v.y)
			List((r, new Phantom("random")))
		}
	}

	def getWithFilters(filters: List[(Phenomenon) => Boolean])(caller: Agent, bag: List[Object]) : List[Phenomenon] =
	{
//		println(caller.id+" called getWithFilters "+filters)
//		val from = new Double2D(caller.currentLocation.x, caller.currentLocation.y);

		//query the MASON field as a List[Object]
//		val bag = sim.field.getObjectsWithinDistance(from, caller.body.sensorRange).objs.toList
//		val bag = sim.field.getObjectsWithinDistance(from, caller.body.maxSensorRange).objs.toList

		logger.debug("Initial bag size = "+bag.size)

		//filter out the caller, and cast all to Phenomenon
		val bagMinusSelf = for {
			thing <- bag
			if thing.isInstanceOf[Phenomenon]
			if thing != caller
		} yield thing.asInstanceOf[Phenomenon]

//		println("bagMinusSelf = "+bagMinusSelf.size)

		//apply all of the filters
		@tailrec
		def where(filters: List[(Phenomenon) => Boolean], bag: List[Phenomenon]): List[Phenomenon] = filters match
		{
			case Nil => 
			{
				logger.debug("Reached the end of filtering.")
				bag
			}
			case f::fs => 
			{
				for(thing <- bag) 
				{
					logger.debug("Checking "+thing)
					logger.debug("Result: "+f(thing))
				}
				val newBag = for {
					thing <- bag
					if f(thing)
				}
				yield thing

				where(fs, newBag)
			}
		}

		val filteredBag = where(filters, bagMinusSelf)

//		for(f<-filteredBag) yield new Point(f.currentLocation.x, f.currentLocation.y)

//		if ( caller._type == "Shark" ) println ("Shark filteredInput = "+filteredBag)

		filteredBag

	}

	/** return the nearest Phenomena within sensor range
	 *	of the calling Agent. Filter the results by applying each
	 *	of the supplied filter functions.
	 */
	def getNearest(filters: List[(Phenomenon) => Boolean], range: Int, center: Int, offsetL: Int, offsetR: Int )(caller: Agent, rough: List[Phenomenon]) : List[(Point,Phenomenon)] =
	{
//		if ( caller.id == 61 )
//		{
//			println("Agent "+caller.id+" calling getNearest")
//			println("Rough = "+rough.size)
//			println("-----")
//			for ( p <- rough; pp = p.asInstanceOf[Phenomenon] ) println(pp._type+" ("+pp.x+","+pp.y+")")
//		}

		val bagOfPhenomena = getWithFilters(filters)(caller, rough)

		logger.debug(caller.id+" called getNearest")
		val callerX = caller.currentLocation.x
		val callerY = caller.currentLocation.y
		val from = new Double2D(caller.currentLocation.x, caller.currentLocation.y);

//		println("Applied Filters")
//		println("----------------")
//		for ( p <- bagOfPhenomena ) 
//		{
//			val pX = p.currentLocation.x
//			val pY = p.currentLocation.y
//			val rotation = caller.body.rotation
//			println("("+pX+","+pY+")")
//			println( from.distance(new Double2D(pX, pY))+" <=? "+range ) 
//			val vector = new CartesianVector(pX - callerX, pY - callerY)
//			println(" agent location: "+caller.currentLocation)
//			println(" agent location: "+callerX+","+callerY)
//			println(" vector = "+vector)
//			println(" vector = "+vector.toPolar)
//			val povVector = new PolarVector(vector.toPolar.azimuth.degrees - rotation, vector.toPolar.magnitude)
//			println(" POV vector = "+povVector)
//			println(" rotation = "+rotation)
//			val leftV_POV = new PolarVector( (center-offsetL+360-rotation)%360, 1)
//			val rightV_POV = new PolarVector( (center+offsetR-rotation+360)%360, 1)
//			val leftV = new PolarVector( (center-offsetL+360)%360, 1)
//			val rightV = new PolarVector( (center+offsetR+360)%360, 1)
//			println(" leftV = "+leftV)
//			println(" rightV = "+rightV)
//			println(" povVector isLeftOf rightV? "+ (povVector isLeftOf rightV))
//			println(" povVector isRightOf leftV? "+ (povVector isRightOf leftV))
//
//		}

		// Filter out any Phenomena not in the sensor region
//		for ( b <- bagOfPhenomena )
//		{
//				val bX = b.currentLocation.x
//				val bY = b.currentLocation.y
//				println(from.distance(new Double2D(bX, bY))+" "+range)
//				val v = new CartesianVector(bX - callerX, bY - callerY)
//				val leftV = new PolarVector( (center-offsetL+360)%360, 1)
//				val rightV = new PolarVector( (center+offsetR)%360, 1)
//				println("Offset sum = "+(offsetL.toInt + offsetR.toInt))
//		}
		val bag = for 
			{ 
				b <- bagOfPhenomena
				bX = b.currentLocation.x
				bY = b.currentLocation.y
				// filter out phenomena outside of sensor range
				if ( from.distance(new Double2D(bX, bY)) <= range ) 
				rotation = caller.body.rotation
				vector = new CartesianVector(bX - callerX, bY - callerY)
				povVector = new PolarVector(vector.toPolar.azimuth.degrees - rotation, vector.toPolar.magnitude)
				leftV = new PolarVector( (center-offsetL+360)%360, 1)
				rightV = new PolarVector( (center+offsetR+360)%360, 1)
				if ( (offsetL + offsetR >= 360) || ((povVector isLeftOf rightV) && (povVector isRightOf leftV)) )
			} 
			yield (new Point(b.currentLocation.x, b.currentLocation.y), b)

//	Debugging code - TODO delete when done
//		if ( caller._type == "Shark" )
//		{
//			println("Applied Sensor Region")
//			println("----------------")
//			for ( (p,ph) <- bag) 
//			{
//				println(ph._type+" ("+p.x+","+p.y+")")
//			}
//		}

		@tailrec 
		def findClosest(remaining: List[(Point,Phenomenon)], closest: (Point, Phenomenon), lastDistance: Double): Option[(Point,Phenomenon)] =
		{

				remaining match
				{
					case Nil => 
						if (lastDistance >= 0) 
							Option(closest)
							//Option(new Point(closest.currentLocation.x, closest.currentLocation.y)) 
						else None
					case (p,ph)::ps =>
					{
						val distance = from.distance(new Double2D(p.x, p.y))
//						println(caller.id+" distance = "+distance)
						findClosest(
								ps, 
								if (lastDistance < 0 || distance < lastDistance ) (p,ph) else closest, 
								if (lastDistance < 0 || distance < lastDistance ) distance else lastDistance
						)
					}
					case _ => findClosest(remaining.tail, closest, lastDistance)
				}
		}

		val filteredResult = findClosest(bag, (new Point(-1, -1), null), -1) match
		{
			case Some(point) => List(point)
			case None => List()
		}

//		if ( caller._type == "Shark" )
//		{
//			println("Applied FindClosest")
//			println("----------------")
//			for ( (p,ph) <- filteredResult) 
//			{
//				println(ph._type+" ("+p.x+","+p.y+")")
//			}
//		}

		//MASON tells us absolute locations on the field
		//we need to convert these to caller's POV before passing to the sensors
//		println("Result")
//		println("----------------")
		val result = for((f,p)<-filteredResult) yield (new Point(f.x - caller.currentLocation.x, f.y - caller.currentLocation.y), p)

//		for ( rr <- result ) 
//		{
//			println("("+rr.x+","+rr.y+")")
//		}

		result

	}
}
}
