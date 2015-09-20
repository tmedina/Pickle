package edu.uga.pickle.evolve

import scala.xml._
import scala.util.Random
import ec.util.MersenneTwisterFast;
import java.util.UUID;

class EvolveUtils(val spec: Node, val mutateSpec: Node)
{
			def modifyAttributes(orig: MetaData, newType: String, newValue: Int): MetaData =
			{
				val filtered = orig.filter( (x: MetaData) => {
					val stringValue = x.value.toString
					if(stringValue == "priority") false
					else if(stringValue == "sum") false
					else true
				})

				filtered
			}

		def fixAttributes(src:Node) : Node =
		{

			def filterAttributes(orig: MetaData): MetaData =
			{
				val r = orig.filter( (x: MetaData) => {
					x.key != "priority" && x.key != "weight"
				})
				r
			}

			def doFixAttributes(src:Node, coType: String, value: Int) : Node =
			{
//				val value = if( ! values.isEmpty) values.head else 0
//				val rest = if ( ! values.isEmpty) values.tail else List()
				src match
				{
					case Elem(ns, "MotorSchema", attrs, scope, children @ _*) =>
					{
//							println("Element: "+"MotorSchema")
//							println("Attributes: "+attrs)

							val newType = if(coType == "priority") "priority"
								else if(coType == "sum") "weight"
								else ""

							(new Elem(ns, "MotorSchema", filterAttributes(attrs), scope, false, children: _*) % 
								new UnprefixedAttribute(newType, value.toString, Null))(0)	
					}

					case Elem(ns, "CoordinationOperator", attrs, scope, child @ _*) =>
					{
//							println("Element: "+"CoordinationOperator")
//							println("Attributes: "+attrs)

							val thisCoType: String = attrs.get("type") match
							{
								case Some(value) => value.text
								case None => ""
							}
//							println("coType: "+coType)

							val newType = if(coType == "priority") "priority"
								else if(coType == "sum") "weight"
								else ""

							val children = src(0).child.toSeq

							var newValues: List[Int] =
								if( thisCoType == "priority" )
									Random.shuffle((for( i <- 1 until children.size+1 ) yield i).toList)
								else if( thisCoType == "sum" )
									Random.shuffle((for( i <- 1 until children.size+1 ) yield i).toList)
								else List()

//							println("NEW VALS: ")
//							println(newValues)

							val childrenCopy: Seq[Node] = for( c <- children ) yield 
							{
								val h::t = newValues
								newValues = t
								doFixAttributes(c, thisCoType, h)
							}

							(new Elem(ns, "CoordinationOperator", filterAttributes(attrs), scope, false, childrenCopy:_*) %
								new UnprefixedAttribute(newType, value.toString, Null))(0)	
					}

					case Elem(ns, label, attrs, scope, child @ _*) =>
					{
//							println("Element: "+label)
//							println("Attributes: "+attrs)
							(new Elem(ns, label, attrs, scope, false, child:  _*))
					}

					case a => { a }

				}

			}

			val firstCO = (src\\"controller"\"AgentSchema"\"Navigation"\"CoordinationOperator")(0)
			val r = (
				src\\"controller" ++ 
				src\\"controller"\"AgentSchema" ++ 
				src\\"controller"\"AgentSchema"\"Navigation" ++ 
				doFixAttributes(firstCO, (firstCO\"@type").text, 1))
			r(0)
			
		}

		def mutateXML(src: Node, srcId: String) : Node =
		{
			val controllerGenerator = new RandomController(spec, mutateSpec)
			val rand = new Random()
			fixAttributes(crossoverXML(src, controllerGenerator.gen(rand.nextDouble),List(srcId+"_M")))
		}

		def mutateXMLOLD(src: Node) : Node =
		{
			val controllerGenerator = new RandomController(spec, mutateSpec)
			val rand = new Random()
			val totalNodes = countNodes((src\\"controller"\"AgentSchema"\"Navigation"\"CoordinationOperator")(0))
			val choice = rand.nextInt(totalNodes - 1 ) + 2//Don't crossover at root

			var count = 0;
			println("Mutating at node "+ choice)
			def doMutation(src: Node) : Node =
			{
				src match
				{
					case Elem(ns, "MotorSchema", attrs, scope, children @ _*) =>
					{
						count = count + 1;
						if( count == choice )
						{
							//mutate
							controllerGenerator.navTree
							//make a new subtree?
							//or modify attrs?
						}
						else
						{
							(new Elem(ns, "MotorSchema", attrs, scope, false, children: _*))(0)	
						}
					}
					case Elem(ns, "CoordinationOperator", attrs, scope, child @ _*) =>
					{
						count = count + 1;
						if( count == choice )
						{
							controllerGenerator.navTree
						}
						else
						{
							val children = src(0).child.toSeq
							val childrenCopy: Seq[Node] = for( c <- children ) yield doMutation(c)
							(new Elem(ns, "CoordinationOperator", attrs, scope, false, childrenCopy:_*))(0)	
							
						}
					}
					case Elem(ns, label, attrs, scope, child @ _*) =>
					{
						val children = src(0).child.toSeq
						val childrenCopy: Seq[Node] = for( c <- children ) yield doMutation(c)
						(new Elem(ns, label, attrs, scope, false, childrenCopy:_*))(0)	
					}
					case a => { a }
				}
			}

			doMutation((src\\"controller")(0))

		}


		def crossoverXML(src: Node, donor: Node, parentIds: List[String]) : Node =
		{
			val rand = new Random()
			val totalSourceNodes = countNodes(src)
			val totalDonorNodes = countNodes(donor)
			val choiceSrc = if( totalSourceNodes > 0 )
			{
				rand.nextInt(totalSourceNodes - 1 ) + 2//Don't crossover at root
			} else {
				println("ERROR: crossing over a controller with 0 nodes!");
				println(src);
				0
			}
			val choiceDonor = if( totalDonorNodes > 0 )
			{
				rand.nextInt(totalDonorNodes - 1 ) + 2//Don't crossover at root
			} 
			else 
			{
				println("ERROR: crossing over a controller with 0 nodes!");
				println(donor);
				0
			}

			def getNthNode(root: Node, n: Int): Node =
			{
				var result: Node = (<Foo/>)(0)
				var count = 0;
				def worker(root: Node)
				{
					root match
					{
						case Elem(ns, "MotorSchema", attrs, scope, children @ _*) =>
						{
							count = count +1
							if(count == n) 
								result =  new Elem(ns, "MotorSchema", attrs, scope, false, children: _*)
							else for( c <- children ) worker(c)
						}
						case Elem(ns, "CoordinationOperator", attrs, scope, children @ _*) =>
						{
							count = count +1
							if(count == n) 
								result = new Elem(ns, "CoordinationOperator", attrs, scope, false, children: _*)
							else for( c <- children ) worker(c)
						}
						case a => for( c <- a.child ) worker(c)

					}
				}
				worker(root)
				result(0)
			}
			val donorSample = getNthNode(donor, choiceDonor)


			var count = 0;
			println("CrossingOver at node "+ choiceSrc)
			def doCrossover(src: Node) : Node =
			{
				src match
				{
					case Elem(ns, "MotorSchema", attrs, scope, children @ _*) =>
					{
						count = count + 1;
						if( count == choiceSrc )
						{
							donorSample	
						}
						else
						{
							(new Elem(ns, "MotorSchema", attrs, scope, false, children: _*))(0)	
						}
					}
					case Elem(ns, "CoordinationOperator", attrs, scope, child @ _*) =>
					{
						count = count + 1;
						if( count == choiceSrc )
						{
							donorSample	
						}
						else
						{
							val children = src(0).child.toSeq
							val childrenCopy: Seq[Node] = for( c <- children ) yield doCrossover(c)
							(new Elem(ns, "CoordinationOperator", attrs, scope, false, childrenCopy:_*))(0)	
							
						}
					}
					case Elem(ns, "controller", attrs, scope, child @ _*) =>
					{
						val parents = (<Parents>
						{for( p <- parentIds ) yield <Parent>{p}</Parent>}
						</Parents>)(0)
						val children = src(0).child.toSeq
						val childrenCopy: Seq[Node] = for( c <- children ) yield doCrossover(c)
						(new Elem(ns, "controller", new UnprefixedAttribute("id", UUID.randomUUID.toString, Null), scope, false, (childrenCopy ++ parents):_*))(0)	
					}
					//TODO: generate new Attribute UUID
					case Elem(ns, label, attrs, scope, child @ _*) =>
					{
						val children = src(0).child.toSeq
						val childrenCopy: Seq[Node] = for( c <- children ) yield doCrossover(c)
						(new Elem(ns, label, attrs, scope, false, childrenCopy:_*))(0)	
					}
					case a => { a }
				}
			}

			fixAttributes(doCrossover(src))
		}

		def countNodes(src: Node): Int =
		{
			src match
			{
				case Elem(ns, "MotorSchema", attrs, scope, child @ _*) =>
				{
					1
				}
				case Elem(ns, "CoordinationOperator", attrs, scope, child @ _*) =>
				{
					val children = src(0).child.toSeq
					var childSum = for( c <- children ) yield countNodes(c)
					1 + childSum.foldLeft  (0) ( (a,b) => a + b  )
				}
				case a => { 
					val children = src(0).child.toSeq
					var childSum = for( c <- children ) yield countNodes(c)
					childSum.foldLeft  (0) ( (a,b) => a + b  )
				}
			}

		}

		def copyXML(src: Node): Node =
		{
			println("===================")
			println(src)
			println("===================")
			src match
			{
				case Elem(ns, "MotorSchema", attrs, scope, child) =>
				{
					println("MotorSchema: "+attrs)
					(new Elem(ns, "MotorSchema", attrs, scope, false, child))(0)	
				}
				case Elem(ns, "CoordinationOperator", attrs, scope, child @ _*) =>
				{
					println("CoordinationOperator: "+attrs)
					val children = src(0).child.toSeq
					val childrenCopy: Seq[Node] = for( c <- children ) yield copyXML(c)
//					(new Elem(ns, "CoordinationOperator", attrs, scope, false, (NodeSeq.fromSeq(childrenCopy))(0)))(0)	
					(new Elem(ns, "CoordinationOperator", attrs, scope, false, childrenCopy:_*))(0)	
				}
				case a => { a }
			}
		}

//		def main(args: Array[String])
//		{
//			val xml = XML.loadFile("test3/generation.0/result_3.xml")
//			val controller = xml\"controller"\"AgentSchema"\"Navigation"\"CoordinationOperator"
////			println("Orig ============================")
////			println(controller(0))
////			println("Count = "+countNodes(controller(0)))
////
////			val copy = copyXML(controller(0))
////			println("Copy ============================")
////			println(copy)
////
//			val crossover = crossoverXML(controller(0), <DONOR/>(0))
//			println("Crossover ============================")
//			println(crossover)
//
//
//		}

}
