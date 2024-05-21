/**
 * 
 */
package simpleSIR;

import static org.junit.Assert.*;

import java.util.List;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import repast.simphony.context.Context;
import repast.simphony.engine.environment.RunState;
import repast.simphony.util.collections.IndexedIterable;

/**
 * @author me597
 *
 */
public class SurveillanceTests {

	/**
	 * @throws java.lang.Exception
	 */
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
	}

	/**
	 * @throws java.lang.Exception
	 */
	@AfterClass
	public static void tearDownAfterClass() throws Exception {
	}

	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
		TestDrive.setUp(Main.setParameters(1000, 1, "none", "GISP", 10, 4000, 4.5, 0.5, 0.5, 3, 0.5, 2, 0.02, 500, 25,100, 100,1,1.5, 2,3,4, 5, 6));

	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void test1() {
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true), 1,0,0);
		context.add(observer);
		SurveillanceProgram surveillance = new SurveillanceProgram();
		context.add(surveillance);
		
		double bigTestPopSize = 100.0;
		
		//make the new indivs
		for (int i = 0; i < bigTestPopSize; i++) {
			context.add(new Indiv());
		}	
		
		double infections = 50.0;
		
		IndexedIterable<Object> indivs = context.getObjects(Indiv.class);
		
		for (int index = 0; index < infections; index++) {
			Indiv indiv = (Indiv) indivs.get(index);
			indiv.infect("none");
		}
		
		Screener screener = new Screener();
		IndexedIterable<Object> indivList = context.getObjects(Indiv.class);
		for (Object i : indivList) { 
			Indiv indiv = (Indiv) i;
			screener.screen(indiv, observer);
			
		}
		
		

		surveillance.conductSurveillance();
		//System.out.print(surveillance.getThisMonthRateA());
		assertTrue("Surveillance1", surveillance.getThisMonthRateA()==0.08);
		
	}

	@Test
	public void test2() {
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true), 1,0,0);
		context.add(observer);
		SurveillanceProgram surveillance = new SurveillanceProgram();
		context.add(surveillance);
		
		double bigTestPopSize = 100.0;
		
		//make the new indivs
		for (int i = 0; i < bigTestPopSize; i++) {
			context.add(new Indiv());
		}	
		
		double infections = 50.0;
	
		
		IndexedIterable<Object> indivs = context.getObjects(Indiv.class);
		
		for (int index = 0; index < infections;) {
			//System.out.println(index);
			Indiv indiv = (Indiv) indivs.get(index);
			String strain = "none";
			if (index % 2 == 0) {
				strain = "A";
			} 
			//System.out.println(strain);

			indiv.infect(strain);
			
			index++;
			
		}
		
		Screener screener = new Screener();
		IndexedIterable<Object> indivList = context.getObjects(Indiv.class);
		for (Object i : indivList) { 
			Indiv indiv = (Indiv) i;
			screener.screen(indiv, observer);
			
		}
		
		

		surveillance.conductSurveillance();
		
		//System.out.println(observer.calcResistAInc(100, context));
		//System.out.println(observer.calcInc(bigTestPopSize, context));
		//System.out.print(surveillance.getThisMonthRateA());

		assertTrue("Surveillance 1.5", observer.calcResistAInc(bigTestPopSize, context) == 25000);
		assertTrue("Surveillance2", surveillance.getThisMonthRateA()>0.4&&surveillance.getThisMonthRateA()<0.6);
		
	}
	
	@Test
	public void calcDetectedStrainTest() {
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true), 1,0,0);
		context.add(observer);
		SurveillanceProgram surveillance = new SurveillanceProgram();
		context.add(surveillance);
		
		double bigTestPopSize = 100.0;
		
		//make the new indivs
		for (int i = 0; i < bigTestPopSize; i++) {
			context.add(new Indiv());
		}	
		
		double infections = 50.0;
	
		
		IndexedIterable<Object> indivs = context.getObjects(Indiv.class);
		
		for (int index = 0; index < infections; index++) {
			Indiv indiv = (Indiv) indivs.get(index);
			
			if (index % 2 == 0) {
				indiv.infect("A");
			} else {
				indiv.infect("none");
			}
			
		}
		
		Screener screener = new Screener();
		IndexedIterable<Object> indivList = context.getObjects(Indiv.class);
		for (Object i : indivList) { 
			Indiv indiv = (Indiv) i;
			screener.screen(indiv, observer);
			
		}
		
		
		//do some rounds of surveillance, infecting 
		


		surveillance.conductSurveillance();
		//System.out.println(surveillance.getAnnualDetected());
		double estProp = surveillance.calcDetectedResistantA();
		
		//System.out.print(estProp);
		
		assertTrue("Surveillance2", estProp>0.4&&estProp<0.6);
		
		
		for (int index = 0; index < infections; index++) {
			Indiv indiv = (Indiv) indivs.get(index);
				indiv.infect("A");
		}
		IndexedIterable<Object> indivList2 = context.getObjects(Indiv.class);
		for (Object i : indivList2) { 
			Indiv indiv = (Indiv) i;
			screener.screen(indiv, observer);
			
		}
		surveillance.conductSurveillance();
		double estProp2 = surveillance.calcDetectedResistantA();
		//System.out.println(estProp2);
		assertTrue("Surveillance4", estProp2 == 0.92);
		
	}
}
