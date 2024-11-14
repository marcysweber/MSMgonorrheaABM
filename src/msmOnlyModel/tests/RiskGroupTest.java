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

public class RiskGroupTest {

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

	public SingleRun setUpRiskGroupTest() {
		BatchRun testBatch = new BatchRun("GISP", "combo");
		Parameters params = testBatch.setParameters(1,//runNumber
				1000,//endtime
				1, //seed
				"combo", //resistance
				"none", //counterfactual
				10, //yearX
				10, //initial infected
				10, //transmission
				1 , //recoveryLambda
				0.5, //probSymptomatic
				2, //screenInterval
				1, //delaytoseekcare
				2, //delaytoretreatment
				0.6,//riskGroupTransferProp
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
				7);//treatmentEcost);
		SingleRun testRun = new SingleRun("/Users/me597/Documents/MSMoutput/tests", params);
		testRun.setUp(52);
		
		return testRun;
	}
	
	
	
	@Test
	public void test() {
		SingleRun testRun = setUpRiskGroupTest();
		
		//System.out.println("high: " + testRun.population().highRiskCount());
		//System.out.println("low: " + testRun.population().lowRiskCount());
		
		testRun.riskGroupChanger().changeRiskGroups();
		
		//System.out.println("high: " + testRun.population().highRiskCount());
		//System.out.println("low: " + testRun.population().lowRiskCount());

		testRun.riskGroupChanger().changeRiskGroups();
		
		//System.out.println("high: " + testRun.population().highRiskCount());
		//System.out.println("low: " + testRun.population().lowRiskCount());

		
		assertTrue("riskGroup1", testRun.population().highRiskCount()+ testRun.population().lowRiskCount() == 100000);		
		
	}

}
