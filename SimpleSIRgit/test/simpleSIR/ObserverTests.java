package simpleSIR;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import repast.simphony.context.Context;
import repast.simphony.engine.environment.RunState;
import repast.simphony.util.collections.IndexedIterable;

public class ObserverTests {

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
	}

	@Before
	public void setUp() throws Exception {
		TestDrive.setUp(Main.setParameters(1000, 1, "none", "none", 10, 4000, 4.5, 0.5, 0.5, 3, 0.5, 2, 50, 500, 25,
				95, 97, 1, 1.5,2, 3, 4, 5,6));
	}

	@After
	public void tearDown() throws Exception {
	}

	
	
	@Test
	public void testCalcInc() {
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true),1,0,0);
		context.add(observer);
		
		//here, add two Indivs to make a known incidence
		Indiv indiv1 = new Indiv();
		context.add(indiv1); 
		Indiv indiv2 = new Indiv();
		context.add(indiv2);
		indiv2.infect("none");
		//observer.recordNewCase(indiv2);
		
		double inc = observer.calcInc(2, context);
		assertTrue("CalcInc1", inc == 50000);
	}
	
	@Test
	public void testCalcPrev() {
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true),1,0,0);
		context.add(observer);

		//here, add two Indivs to make a known prevalence (e.g., 50%)
		Indiv indiv1 = new Indiv();
		context.add(indiv1);
		indiv1.infect("none");
		context.add(new Indiv());
	
		double prev = observer.calcPrev(2, context);
		assertTrue("CalcPrev1", prev == 50);
		
		context.add(new Indiv());
		context.add(new Indiv());

		double prev2 = observer.calcPrev(4, context);
		assertTrue("CalcPrev2", prev2 == 25);
	}
	
	@Test
	public void testCalcObserver() {
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true),1,0,0);
		context.add(observer);
		
		SurveillanceProgram surveillance = new SurveillanceProgram();
		context.add(surveillance);
		
		Indiv oldcase = new Indiv();
		context.add(oldcase);
		oldcase.infect("none");
		
		Indiv newcase = new Indiv();
		context.add(newcase);
		newcase.infect("none");
		
		context.add(new Indiv());
		context.add(new Indiv());
		
		//prev should be 50
		//inc should be 
		
		observer.calcObserver();
		
		assertTrue("CalcObserver1", observer.getPrevalence() == 50);
		assertTrue("CalcObserver2", observer.getIncidence() == 50000);
		assertTrue("CalcObserver3", observer.getNewCases()==0);
		
	}
	
	@Test
	public void testBigCalcObserver() {
		//even with the other tests, there still seems to be something 
		//wrong with incidence and newCases. 
		//so i'm writing a bigger test that generates a population of 100,000
		
		//first, setup the observer
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true), 1,0,0);
		context.add(observer);
		
		SurveillanceProgram surveillance = new SurveillanceProgram();
		context.add(surveillance);
		
		double bigTestPopSize = 1000000.0;
		
		//make the new indivs
		for (int i = 0; i < bigTestPopSize; i++) {
			context.add(new Indiv());
		}
		
		//infect a set number of indivs
		double infections = 1000.0;
		
		IndexedIterable<Object> indivs = context.getObjects(Indiv.class);
		
		for (int index = 0; index < infections; index++) {
			Indiv indiv = (Indiv) indivs.get(index);
			indiv.infect("none");
		}

		//assert that newCases matches that set number
		assertTrue("BigCalcObserver1", infections == observer.getNewCases());
		
		//run the calcObserver
		observer.calcObserver();
		
		//assert inc matches infections
		assertTrue("BigCalcObserver2", (infections / bigTestPopSize) * 100000 == observer.getIncidence());
		//System.out.println(observer.getPrevalence());
		//System.out.println((infections / bigTestPopSize) * 100);
		assertTrue("BigCalcObserver3", observer.getPrevalence() == (infections / bigTestPopSize) * 100);
		assertTrue("BigCalcObserver4", observer.getNewCases() == 0);
		
		observer.calcObserver();
		assertTrue("BigCalcObserver5", observer.getPrevalence() == 100 * infections / bigTestPopSize);
		assertTrue("BigCalcObserver6", observer.getIncidence()==0);

		
	}
	

}
