package msmOnlyModel.tests;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import msmOnlyModel.BatchRun;
import msmOnlyModel.SingleRun;
import msmOnlyModel.SurveillanceProgram;
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
		BatchRun testBatch = new BatchRun("GISP", "combo");
		Parameters params = testBatch.setParameters(1,//runNumber
				1000,//endtime
				1, //seed
				"combo", //resistance
				"GISP", //counterfactual
				10, //yearX
				1000, //initial infected
				0.1, //propHighRisk
				10, //transmission
				1 , //recoveryLambda
				0.5, //probSymptomatic
				2, //screenInterval
				1, //delaytoseekcare
				2, //delaytoretreatment
				1.0,//assortativity
				0.1,//riskGroupTransferProp
				0.5,//riskGroupTransmissionRatio
				25, //amount resistant A
				10, //being importing B
				10,//importing B interval
				95, //sensitivity
				97,//specificity
				1, //care cost
				2,//testcost
				3,//straintestcost
				4, //treatmentAcost
				5, //treatmentBcost
				6, //treatmentXcost
				7,//treatmentEcost);
				0,
				0);		SingleRun testRun = new SingleRun("/Users/me597/Documents/MSMoutput/tests", params);
		testRun.setUp(52);
		
		return testRun;
	}
	
	@Test
	public void test() {
		
		SingleRun testRun = setUpSurvTest();

		for (int i=0; i < 300; i++) {
			testRun.schedule().execute();
		}
		System.out.println("tick " + testRun.schedule().getTickCount());


		System.out.println("prev: " + testRun.observer().calcPrev());
		System.out.println("detected: " + testRun.observer().getDetectedList().size());

		testRun.observer().clearDetectedList();
		
		testRun.resistanceInserter().convertToResistantA();
		System.out.println("actual resistance to A: " + testRun.observer().calcResistAPrev());

		for (int i=0; i < 300; i++) {
			testRun.schedule().execute();
		}
		
		System.out.println("detected: " + testRun.observer().getDetectedList().size());
		System.out.println("actual resistance to A: " + testRun.observer().calcResistAPrev());

		
		testRun.surveillanceProgram().conductSurveillance();
		double result = testRun.surveillanceProgram().calcDetectedResistantA();

		System.out.println("surveillance: " + result);
		
		System.out.println("switch? " + testRun.surveillanceProgram().checkForSwitch(result));

		
	}
	
	@Test
	public void test2() {
		
		SingleRun testRun = setUpSurvTest();

		for (int i=0; i < 300; i++) {
			testRun.schedule().execute();
		}
		//System.out.println("tick " + testRun.schedule().getTickCount());


		System.out.println("prev: " + testRun.observer().calcPrev());
		System.out.println("detected: " + testRun.observer().getDetectedList().size());

		testRun.observer().clearDetectedList();
		
		testRun.population().allIndivs().limit(5000).forEach(indiv -> indiv.infect("Both", "test"));
		System.out.println("actual resistance to Both " + testRun.observer().calcResistBothPrev());

		for (int i=0; i < 300; i++) {
			testRun.schedule().execute();
		}
		
		System.out.println("detected: " + testRun.observer().getDetectedList().size());
		System.out.println("actual resistance to Both: " + testRun.observer().calcResistBothPrev());

		
		testRun.surveillanceProgram().conductSurveillance();
		double result = testRun.surveillanceProgram().calcDetectedResistantBoth();

		System.out.println("surveillance: " + result);
		
		System.out.println("switch? " + testRun.surveillanceProgram().checkForSwitch(result));
		
		SurveillanceProgram surveillance = testRun.surveillanceProgram();
		double surveillanceResultA = surveillance.calcDetectedResistantA();
		double surveillanceResultB = surveillance.calcDetectedResistantB();
		double surveillanceResultBoth = surveillance.calcDetectedResistantBoth();

		if (!surveillance.getRemovedA() && surveillance.checkForSwitch(surveillanceResultA)) {
			surveillance.removeDrugA();
		}
			//if we haven;t already removed B and resistance to B is above 5%
		if (!surveillance.getRemovedB() && surveillance.checkForSwitch(surveillanceResultB)) {
			surveillance.removeDrugB();
		}	
		
		//if we havent' already removed combinatino therapy and multiresistance is above 5%
		if (!surveillance.getRemovedAandB() && surveillance.checkForSwitch(surveillanceResultBoth)) {
			surveillance.removeDrugsABandAandB();
		}
		
		

		
		}
	

}
