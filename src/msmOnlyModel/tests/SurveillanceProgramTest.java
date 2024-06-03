package msmOnlyModel.tests;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import msmOnlyModel.BatchRun;
import msmOnlyModel.SingleRun;
import repast.simphony.parameter.Parameters;

public class SurveillanceProgramTest {

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
	}

	@Before
	public void setUp() throws Exception {
	}

	@After
	public void tearDown() throws Exception {
	}

	public SingleRun setUpSurvTest() {
		BatchRun testBatch = new BatchRun("GISP", "combo", 10);
		Parameters params = testBatch.setParameters(0,52, 1, "combo", "GISP", 10, 5000, 1.0, 1.0, 0.5, 1.0, 0.0, 0.0, 1.0, 53, 1.0, 0.95, 0.95, 1, 2, 3, 4, 5, 6, 7);
		SingleRun testRun = new SingleRun("/Users/me597/Documents/MSMoutput/tests", params);
		testRun.setUp(52);
		
		return testRun;
	}
	
	@Test
	public void test() {
		
		SingleRun testRun = setUpSurvTest();

		for (int i=0; i < 3; i++) {
			testRun.schedule().execute();
		}
		//System.out.println("tick " + testRun.schedule().getTickCount());


		//System.out.println("prev: " + testRun.observer().calcPrev());
		//System.out.println("detected: " + testRun.observer().detected());


		
		testRun.resistanceInserter().convertToResistantA();
		System.out.println("actual resistance to A: " + testRun.observer().calcResistAPrev());

		for (int i=0; i < 300; i++) {
			testRun.schedule().execute();
		}
		
		testRun.surveillanceProgram().conductSurveillance();
		double result = testRun.surveillanceProgram().calcDetectedResistantA();

		//System.out.println("surveillance: " + result);
		
		//System.out.println("switch? " + testRun.surveillanceProgram().checkForSwitch(result));

		
	}
	

}
