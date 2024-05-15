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

public class ScreenerTests {

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
	}

	@Before
	public void setUp() throws Exception {
		TestDrive.setUp(Main.setParameters(1000, 1, "none", "none",10, 4000, 4.5, 0.5, 0, 3, 0.5, 2, 50, 500, 25,95,97,1, 1.5,2,3,4,5,6));
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void testScreeningSchedule() {
		Screener screener = new Screener();
		List schej = screener.makeScreenSchedule();
		//System.out.println(schej.size());
		
		//System.out.println(schej.get(0));
		//System.out.println(schej.get(1));
		//System.out.println(schej.get(2));
		//System.out.println(schej.get(10));

	}

	@Test
	public void testScreen() {
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
		
		double infections = bigTestPopSize/2;
		
		IndexedIterable<Object> indivs = context.getObjects(Indiv.class);
		
		for (int index = 0; index < infections; index++) {
			Indiv indiv = (Indiv) indivs.get(index);
			indiv.infect("none");
		}
		double prev = observer.calcPrev(bigTestPopSize, context);
		assertTrue("Screen1", prev == 50.0);
		
		Screener screener = new Screener();
		IndexedIterable<Object> indivList = context.getObjects(Indiv.class);
		for (Object i : indivList) { 
			Indiv indiv = (Indiv) i;
			screener.screen(indiv, observer);
			
		}
		
		double prev2 = observer.calcPrev(bigTestPopSize, context);
		assertTrue("Screen2", prev2 == 0.0);
	}
	
	
}
