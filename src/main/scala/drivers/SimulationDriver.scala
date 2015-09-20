package edu.uga.pickle.drivers;
import edu.uga.pickle.body.Point
import edu.uga.pickle.application.{Phenomenon, Agent}

abstract class SimulationDriver
{
	val width: Int
	val height: Int
	def put(p: Phenomenon, l: Point)
	def markReady(p: Agent)
	def randomDouble: Double
	def getRough(caller: Agent): List[Phenomenon]
	def get(filters: List[(Phenomenon) => Boolean], range: Int, center: Int, offsetL: Int, offsetR: Int )(caller: Agent, rough: List[Phenomenon]) : List[(Point,Phenomenon)]
	def getRandom(freq: Double, active: Double, center: Double)(caller: Agent, rough: List[Phenomenon]) : List[(Point,Phenomenon)]
	def getWithFilters(filters: List[(Phenomenon) => Boolean])(caller: Agent, bag: List[Object]) : List[Phenomenon]
	def getNearest(filters: List[(Phenomenon) => Boolean], range: Int, center: Int, offsetL: Int, offsetR: Int )(caller: Agent, rough: List[Phenomenon]) : List[(Point,Phenomenon)]
	def runWithGUI(bgColor: String, showIDs: Boolean)
	def removePhenomenon(p: Phenomenon)
	def bumpSensor(target: String)(caller: Agent, empty: List[Phenomenon]): List[(Point,Phenomenon)]

}
