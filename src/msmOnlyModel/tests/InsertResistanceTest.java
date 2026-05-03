package msmOnlyModel.tests;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import msmOnlyModel.BatchRun;
import msmOnlyModel.Indiv;
import msmOnlyModel.SingleRun;
import msmOnlyModel.Treatment;
import repast.simphony.parameter.Parameters;

public class InsertResistanceTest {

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

	public SingleRun setUpInsertResistanceTest() {
		BatchRun testBatch = new BatchRun("GISP", "combo");
		Parameters params = testBatch.setParameters(1,//runNumber
				1000,//endtime
				1, //seed
				"combo", //resistance
				"test", //counterfactual
				10, //yearX
				10, //initial infected
				0.1, //propHighRisk
				10, //transmission
				1 , //recoveryLambda
				0.5, //probSymptomatic
				2, //screenIntervalmean
				2, //screenIntervalVar
				1, //delaytoseekcare
				2, //delaytoretreatment
				1.0,//assortativity
				0.1,//riskGroupTransferProp
				0.5,//riskGroupTransmissionRatio
				25, //amount resistant A
				10, //being importing B
				10,//importing B interval
				-0.1,//probDevelopResistanceAExponent
				-0.1,//probDevelopResistanceBExponent

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
				0);
		SingleRun testRun = new SingleRun("output/tests", params);
		testRun.setUp(52);
		
		return testRun;
	}
	
	@Test
	public void testDevelopResistance() {
		SingleRun testRun = setUpInsertResistanceTest();
		
		testRun.testSetUp(1000);
		
		testRun.createIndivs(100);
		testRun.observer().setPopulation(testRun.population());
		
		for (int i = 0; i < 10; i++) {
			Indiv indiv = testRun.population().msm().get(i);
			System.out.println("The selected indiv: " + indiv.hashCode());
			indiv.infect("none");
		
			indiv.recoverOrDevelopResistance("A");
		
			System.out.println(indiv.infectious());
		
			//System.out.println(indiv.myInfection().resistantToA());
		}
		
	}

}
