package edu.uga.pickle.drivers.mason

import edu.uga.pickle.application._
import edu.uga.pickle.vector._

import sim.engine.SimState;
import sim.display.GUIState;
import sim.display.Display2D;
import sim.display.{Controller => MasonGUIController}
//import sim.portrayal.Portrayal;
import sim.portrayal.DrawInfo2D;
import sim.portrayal.continuous.ContinuousPortrayal2D;
import sim.portrayal.simple.OvalPortrayal2D;
//import sim.portrayal.simple.OrientedPortrayal2D;
//import sim.portrayal.simple.LabelledPortrayal2D;
//import sim.portrayal.SimplePortrayal2D;
//
//import javax.swing.JFrame;
import java.awt.Color;
//import java.awt.Font;
import java.awt.Graphics2D;

import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.LogManager;


object MasonGUI
{
	def getColor(c: String) = c.toUpperCase match
	{
		case "BLACK" => Color.black
		case "RED" => Color.red
		case "BLUE" => Color.blue
		case "GREEN" => Color.green
		case "CYAN" => Color.cyan
		case "DARKGRAY" => Color.darkGray
		case "LIGHTGRAY" => Color.lightGray
		case "GRAY" => Color.gray
		case "ORANGE" => Color.orange
		case "MAGENTA" => Color.magenta
		case "PINK" => Color.pink
		case "WHITE" => Color.white
		case "YELLOW" => Color.yellow
		case _ => Color.black
	}
}

class MasonGUI(sim: MasonSimulation, bgColor: String, showIDs: Boolean) extends GUIState(sim)
{
	val logger = LogManager.getLogger("SimulationDriver")
	val field2DPortrayal = new ContinuousPortrayal2D()
	field2DPortrayal.setField(sim.field)
	val allObjs = sim.field.allObjects.objs.toList
	for( o <- allObjs)
	{
		field2DPortrayal.setPortrayalForObject(o, new MasonBodyPortrayal(sim, o.asInstanceOf[Phenomenon], showIDs));
	}

	def this() = this(new MasonSimulation(0,0,0,0,0,0,0), "", false)

	override def init(c: MasonGUIController)
	{
		val display = new Display2D(sim.dW, sim.dH, this)
		display.setClipping(false);     
    val displayFrame = display.createFrame();
    displayFrame.setTitle("Pickle");
    c.registerFrame(displayFrame);  
    displayFrame.setVisible(true);  
    display.attach(field2DPortrayal,"Pickle");
    display.setBackdrop(MasonGUI.getColor(bgColor));
    //reschedule the displayer      
    display.reset();
    display.setBackdrop(MasonGUI.getColor(bgColor));
    //redraw the display     
    display.repaint();      
	}

}

class MasonBodyPortrayal(sim: MasonSimulation, obj: Phenomenon, showIDs: Boolean) 
extends OvalPortrayal2D(
	//color
	try { 
		MasonGUI.getColor(obj.attributes.get("Color").getOrElse("BLACK").asInstanceOf[String])
	} catch { case r : Throwable => { /*sim.logger.error("Exception getting Color Attribute: "+r);*/ Color.red } },
	//size
	try { 
		obj.size
	} catch { case r : Throwable => { /*sim.logger.error("Exception getting Size Attribute"+r);*/ 10 } },
	//filled
	false)
{

	override def draw(obj: Object , graphics: Graphics2D , info: DrawInfo2D )
	{
		val p = obj.asInstanceOf[Phenomenon]
		
		paint = try { 
			MasonGUI.getColor(p.attributes.get("Color").getOrElse("BLACK").asInstanceOf[String])
		} catch { case r : Throwable => { sim.logger.error("Exception getting Color Attribute: "+r); Color.red } }

		super.draw(obj, graphics, info)

		if(p.isInstanceOf[Agent] && showIDs)
			graphics.drawString(p.id.toString, info.draw.x.asInstanceOf[Int],info.draw.y.asInstanceOf[Int])
		if(p.isInstanceOf[Agent])
		{
    	//graphics.setColor(Color.red);
			val pVector = p.vector.enlarge(p.size).toCartesian
//			println("Draw vector: "+p.vector)
//			println("Draw vector: "+p.vector.toPolar)
//			println("Draw body vector: "+p.body.vector)
//			println("Draw body vector: "+p.body.vector.toPolar)
			graphics.drawLine(
				info.draw.x.asInstanceOf[Int], 
				info.draw.y.asInstanceOf[Int], 
				(info.draw.x.asInstanceOf[Int] - pVector.x.toInt),
				(info.draw.y.asInstanceOf[Int] - pVector.y.toInt)
			)
		}
	}
}

