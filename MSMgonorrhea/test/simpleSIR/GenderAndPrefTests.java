package simpleSIR;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import repast.simphony.context.Context;
import repast.simphony.engine.environment.RunState;

public class GenderAndPrefTests {

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
	}

	@Before
	public void setUp() throws Exception {
		TestDrive.setUp(Main.setParameters(1000, 1, "none", "none", 10, 4000, 1.0, 0.5, 3, 0.5, 2, 50, 500, 25,95,97,1, 1,1.5, 2, 3, 4,5,6));

	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void test() {
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true),1,0,0);
		context.add(observer);
		
		double bigTestPopSize = 100000.0;
		
		//make the new indivs
		for (int i = 0; i < bigTestPopSize; i++) {
			context.add(new Indiv());
		}	
		
//		System.out.println(SubGrouping.msmCount(context)/bigTestPopSize);
//		System.out.println(SubGrouping.msmwCount(context)/bigTestPopSize);
//		System.out.println(SubGrouping.mswCount(context)/bigTestPopSize);
//		System.out.println(SubGrouping.wCount(context)/bigTestPopSize);
//		System.out.println(SubGrouping.nbCount(context)/bigTestPopSize);

		assertTrue("subgrouping", SubGrouping.msmCount(context) + SubGrouping.msmwCount(context) + SubGrouping.mswCount(context) + SubGrouping.wCount(context) + SubGrouping.nbCount(context) == bigTestPopSize);
	
	}
	
	@Test
	public void subGroupTest() {
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true),1,0,0);
		context.add(observer);
		
		double bigTestPopSize = 100000.0;
		
		//make the new indivs
		for (int i = 0; i < bigTestPopSize; i++) {
			context.add(new Indiv());
		}	
		
		SubGrouping.w(context).forEach(f -> ((Indiv) f).infect("none"));
		
		
		assertTrue("WAllInfected1", observer.calcPrevW(context) == 1.0);
		assertTrue("WAllInfected2", observer.calcPrevMSM(context) == 0.0);
		
		SubGrouping.w(context).forEach(f -> ((Indiv)f).infectiousActions());
		
		assertTrue("WAllInfected3", observer.calcPrevW(context) == 1.0);
		assertTrue("WAllInfected4", observer.calcPrevMSM(context) == 0.0);
		assertTrue("WAllInfected5", observer.calcPrevMSW(context) > 0);
		
		//System.out.println(observer.calcPrevMSW(context));

	}
	
	
	
	

}
