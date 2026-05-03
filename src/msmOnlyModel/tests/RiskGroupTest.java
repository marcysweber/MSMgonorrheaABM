package msmOnlyModel.tests;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import msmOnlyModel.BatchRun;
import msmOnlyModel.CustomParameterSweep;
import msmOnlyModel.Indiv;
import msmOnlyModel.SingleRun;
import msmOnlyModel.ThreadSafeSchedule;
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
		testRun.setUp(52);
		
		return testRun;
	}
	
	
	
	@Test
	public void test() {
		SingleRun testRun = setUpRiskGroupTest();
		
		System.out.println("high: " + testRun.population().highRiskCount());
		System.out.println("low: " + testRun.population().lowRiskCount());
		
		testRun.riskGroupChanger().changeActivityGroups();
		
		System.out.println("high: " + testRun.population().highRiskCount());
		System.out.println("low: " + testRun.population().lowRiskCount());

		testRun.riskGroupChanger().changeActivityGroups();
		
		System.out.println("high: " + testRun.population().highRiskCount());
		System.out.println("low: " + testRun.population().lowRiskCount());

		testRun.riskGroupChanger().changeActivityGroups();
		
		System.out.println("high: " + testRun.population().highRiskCount());
		System.out.println("low: " + testRun.population().lowRiskCount());

		testRun.riskGroupChanger().changeActivityGroups();
		
		System.out.println("high: " + testRun.population().highRiskCount());
		System.out.println("low: " + testRun.population().lowRiskCount());

		
		assertTrue("riskGroup1", testRun.population().highRiskCount()+ testRun.population().lowRiskCount() == 100000);		
		
	}
	
	
	
	@Test
	public void testSweepPropHighRisk() {
		BatchRun testBatch = new BatchRun("GISP", "combo");
		
		for (int i = 0; i < 100; i++) {

			CustomParameterSweep sweeper = new CustomParameterSweep();
			Parameters params = testBatch.setParameters(1,//runNumber
					1000,//endtime
					1, //seed
					"combo", //resistance
					"none", //counterfactual
					10, //yearX
					10, //initial infected
					sweeper.getPropHighActivityValues(1).get(0), //propHighRisk
					100, //transmission
					1 , //recoveryLambda
					0.5, //probSymptomatic
					2, //screenInterval
					1, //delaytoseekcare
					2, //delaytoretreatment
					1.0,//assortativity
					0.1,//riskGroupTransferProp
					0.1,//riskGroupTransmissionRatio
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

			testRun.testSetUp(52);

			testRun.createIndivs(100);
			testRun.observer().setPopulation(testRun.population());

			System.out.println("high: " + testRun.population().highRiskCount());
			System.out.println("low: " + testRun.population().lowRiskCount());
		}
		
		
		
	}
	

	@Test
	public void testSweepRiskGroupTransfer() {
		BatchRun testBatch = new BatchRun("GISP", "combo");
		
		for (int i = 0; i < 10; i++) {

			CustomParameterSweep sweeper = new CustomParameterSweep();
			Parameters params = testBatch.setParameters(1,//runNumber
					1000,//endtime
					1, //seed
					"combo", //resistance
					"none", //counterfactual
					10, //yearX
					10, //initial infected
					0.2, //propHighRisk
					100, //transmission
					1 , //recoveryLambda
					0.5, //probSymptomatic
					2, //screenInterval
					1, //delaytoseekcare
					2, //delaytoretreatment
					1.0,//assortativity
					sweeper.getActivityGroupTransferPropValues(1).get(0),//riskGroupTransferProp
					0.1,//riskGroupTransmissionRatio
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

			testRun.testSetUp(52);

			testRun.createIndivs(100);
			testRun.observer().setPopulation(testRun.population());
			testRun.riskGroupChanger().setPopulation(testRun.population());

			System.out.println("RiskGroupTransferProp: " + params.getDouble("risk_group_transfer_prop"));
			System.out.println("high: " + testRun.population().highRiskCount());
			System.out.println("low: " + testRun.population().lowRiskCount());
			
			testRun.riskGroupChanger().changeActivityGroups();

			System.out.println("high: " + testRun.population().highRiskCount());
			System.out.println("low: " + testRun.population().lowRiskCount());
			
			
			System.out.println("\n");
		}
		
		
		
	}
	
	
	@Test 
	public void testLow() {
		BatchRun testBatch = new BatchRun("GISP", "combo");
		Parameters params = testBatch.setParameters(1,//runNumber
				1000,//endtime
				1, //seed
				"combo", //resistance
				"none", //counterfactual
				10, //yearX
				10, //initial infected
				0.1, //propHighRisk
				100, //transmission
				1 , //recoveryLambda
				0.5, //probSymptomatic
				2, //screenInterval
				1, //delaytoseekcare
				2, //delaytoretreatment
				1.0,//assortativity
				0.1,//riskGroupTransferProp
				0.1,//riskGroupTransmissionRatio
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

		testRun.testSetUp(52);
		
		testRun.createIndivs(100);
		testRun.observer().setPopulation(testRun.population());
				
		System.out.println("high: " + testRun.population().highRiskCount());
		System.out.println("low: " + testRun.population().lowRiskCount());
				
		testRun.population().lowActivityGroupStream().limit(1).forEach(indiv -> indiv.infectInit());
		
		System.out.println("high infected: " + testRun.population().highRiskInfected().count());
		System.out.println("low infected: " + testRun.population().lowRiskInfected().count());
		
		for (int i=0; i<10; i++) {
			testRun.schedule().execute();
		}
		
		System.out.println("high infected: " + testRun.population().highRiskInfected().count());
		System.out.println("low infected: " + testRun.population().lowRiskInfected().count());
		
		
		
	}

	
	@Test 
	public void testHigh() {
		BatchRun testBatch = new BatchRun("GISP", "combo");
		Parameters params = testBatch.setParameters(1,//runNumber
				1000,//endtime
				1, //seed
				"combo", //resistance
				"none", //counterfactual
				10, //yearX
				10, //initial infected
				0.1, //propHihgeRisk
				100, //transmission
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

		testRun.testSetUp(52);
		
		testRun.createIndivs(100);
		testRun.observer().setPopulation(testRun.population());
				
		System.out.println("high: " + testRun.population().highRiskCount());
		System.out.println("low: " + testRun.population().lowRiskCount());
				
		testRun.population().highRiskGroupStream().limit(1).forEach(indiv -> indiv.infectInit());
		
		System.out.println("high infected: " + testRun.population().highRiskInfected().count());
		System.out.println("low infected: " + testRun.population().lowRiskInfected().count());
		
		for (int i=0; i<10; i++) {
			testRun.schedule().execute();
		}
		
		System.out.println("high infected: " + testRun.population().highRiskInfected().count());
		System.out.println("low infected: " + testRun.population().lowRiskInfected().count());
		
		
		
	}
	
	
	
	
	
	@Test
	public void assortTest() {
		BatchRun testBatch = new BatchRun("GISP", "combo");
		Parameters params = testBatch.setParameters(1,//runNumber
				1000,//endtime
				1, //seed
				"combo", //resistance
				"none", //counterfactual
				10, //yearX
				10, //initial infected
				0.5, //propHihgeRisk
				100, //transmission
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

		testRun.testSetUp(52);
		
		testRun.createIndivs(100);
		testRun.observer().setPopulation(testRun.population());
		
		Indiv indiv = testRun.population().highActivityGroup.get(0);
		
		for (int i = 0; i < 10; i++) {
			assertTrue("assort 1.0 test", indiv.partnerSelect().getRiskGroup().equals("high"));
		}
		
		Indiv indiv2 = testRun.population().lowActivityGroup.get(0);

		for (int i = 0; i < 10; i++) {
			assertTrue("assort 1.0 test", indiv2.partnerSelect().getRiskGroup().equals("low"));
		}
		
		
		
		
		
		
		
		
		
		
		//dissassortavity
		testBatch = new BatchRun("GISP", "combo");
		Parameters params2 = testBatch.setParameters(1,//runNumber
				1000,//endtime
				1, //seed
				"combo", //resistance
				"none", //counterfactual
				10, //yearX
				10, //initial infected
				0.5, //propHihgeRisk
				100, //transmission
				1 , //recoveryLambda
				0.5, //probSymptomatic
				2, //screenInterval
				1, //delaytoseekcare
				2, //delaytoretreatment
				0.0,//assortativity
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
		SingleRun testRun2 = new SingleRun("output/tests", params2);

		testRun2.testSetUp(52);
		
		testRun2.createIndivs(100);
		testRun2.observer().setPopulation(testRun2.population());
		
		Indiv indiv3 = testRun2.population().highActivityGroup.get(0);
		
		for (int i = 0; i < 10; i++) {
			assertTrue("assort 1.0 test", indiv3.partnerSelect().getRiskGroup().equals("low"));
		}
		
		Indiv indiv4 = testRun2.population().lowActivityGroup.get(0);

		for (int i = 0; i < 10; i++) {
			assertTrue("assort 1.0 test", indiv4.partnerSelect().getRiskGroup().equals("high"));
		}
		
		
		
		
		
		
		
		
		
		
		
		
		testBatch = new BatchRun("GISP", "combo");
		Parameters params3 = testBatch.setParameters(1,//runNumber
				1000,//endtime
				1, //seed
				"combo", //resistance
				"none", //counterfactual
				10, //yearX
				10, //initial infected
				0.5, //propHihgeRisk
				100, //transmission
				1 , //recoveryLambda
				0.5, //probSymptomatic
				2, //screenInterval
				1, //delaytoseekcare
				2, //delaytoretreatment
				0.5,//assortativity
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
		SingleRun testRun3 = new SingleRun("output/tests", params3);

		testRun3.testSetUp(52);
		
		testRun3.createIndivs(100);
		testRun3.observer().setPopulation(testRun3.population());
		
		Indiv indiv5 = testRun3.population().highActivityGroup.get(0);
		
		System.out.println( "\n" + indiv5.getRiskGroup());

		for (int i = 0; i < 10; i++) {
			System.out.println(indiv5.partnerSelect().getRiskGroup());
		}
		
		Indiv indiv6 = testRun3.population().lowActivityGroup.get(0);
		System.out.println( "\n" + indiv6.getRiskGroup());

		for (int i = 0; i < 10; i++) {
			System.out.println(indiv6.partnerSelect().getRiskGroup());
		}
		
		
		
		
		
	}

}
