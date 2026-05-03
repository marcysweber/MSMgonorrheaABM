package msmOnlyModel.tests;

import static org.junit.Assert.*;

import java.util.Optional;
import java.util.stream.Collectors;

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

public class TreatementTest {

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

	public SingleRun setUpTreatmentTest(int seed) {
		BatchRun testBatch = new BatchRun("GISP", "combo");
		Parameters params = testBatch.setParameters(
				1,//runNumber
				1000,//endtime
				seed, //seed
				"combo", //resistance
				"none", //counterfactual
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
				7);//treatmentEcost);
		SingleRun testRun = new SingleRun("output/tests", params);
		testRun.setUp(1000);
		
		return testRun;
	}
	
	public void treatX(SingleRun testRun, Indiv indiv) {
		//Treatment treatment = new Treatment(indiv, testRun.observer());
		//treatment.tryDrugXorE();
	}
	
	@Test
	public void testX() {

		for (int i = 0; i < 1; i++) {

			SingleRun testRun = setUpTreatmentTest(i);

			for (int j = 0; j < 10; j++) {
				testRun.schedule().execute();
			}

			testRun.go();

			if (testRun.observer().attemptsX() != testRun.observer().sucessesX()) {

				System.out.println(testRun.observer().attemptsX());
				System.out.println(testRun.observer().sucessesX());
			}

		}
	}

}
