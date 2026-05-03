package msmOnlyModel.tests;

import static org.junit.Assert.*;

import java.util.stream.Collectors;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import msmOnlyModel.BatchRun;
import msmOnlyModel.CareSeeking;
import msmOnlyModel.CustomFileOutput;
import msmOnlyModel.Indiv;
import msmOnlyModel.Observer;
import msmOnlyModel.SingleRun;
import msmOnlyModel.ThreadSafeRandomHelper;
import msmOnlyModel.ThreadSafeSchedule;
import msmOnlyModel.Treatment;
import repast.simphony.parameter.Parameters;

public class SequelaeTest {

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

	public SingleRun setUpSequelaeTest() {
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
				7,//treatmentEcost);
				0,
				0);
		SingleRun testRun = new SingleRun("output/tests", params);
		testRun.setUp(52);
		
		return testRun;
	}
	
	@Test
	public void NatRecovTest() {
		//sequelae checks:
			//ineffective treatment
			//recover without treatment
		BatchRun testBatch = new BatchRun("GISP", "combo");
		Parameters params = testBatch.setParameters(
				0, //run number
				52, //end time
				1, //seed
				"combo",  //resistance
				"GISP", //counterfactual
				10, //yearX
				100, //initialinfected
				0.1, //propHighRisk

				1.0, //transmission
				1.0, //recoveryLambda
				1.0, //probSymptomatic
				1.0, //screen interval
				1.0, //delay to seek care
				1.0, //delay to retreatment
				1.0,//assortativity

				0.5,//riskGroupTransferProp
				0.5,//riskGroupTransmissionRatio
				0.05, //percent resistant A
				53, //begin importing B
				1.0, //importing B interval
				0.95, //DST sensitivity
				0.95, //DST specificity
				1, //care cost
				2, //diagnostic test cost
				3, //strain test cost
				4, //treatment A cost
				5, //treatment B cost
				6, //treatment X cost
				7,//treatmentEcost);
				0,
				0);
		SingleRun testRun = new SingleRun("output/tests", params);
		ThreadSafeSchedule schedule = new ThreadSafeSchedule();
		testRun.assignSchedule(schedule);
		ThreadSafeRandomHelper randomHelper = testRun.registerDistributions();
		Observer observer = testRun.createObserver(0, null, null, 0);
		testRun.createSurveillance("none");
		testRun.createIndivs(0);
		observer.setPopulation(testRun.population());
		
		double bigTestPopSize = 1000.0;

		for (int i = 0; i < bigTestPopSize; i++) {
			testRun.population().add(new Indiv(params, "msm", "high", randomHelper, observer, schedule));
		}	
	
		testRun.population().allIndivs().forEach(indiv -> indiv.infect("A"));
			
		testRun.population().allIndivs().forEach(indiv -> indiv.recoverNaturally());
		
		
//		System.out.println(observer.getChecksForSequelae());
//		System.out.println(observer.getCasesEpi());
//		System.out.println(observer.getCasesDGI());
//		System.out.println(observer.getCasesBothSequelae());

		assertTrue("NatRecov", observer.getChecksForSequelae() == 1000);
		
	}
	

	@Test
	public void TreatFailTest() {
		//sequelae checks:
			//ineffective treatment
			//recover without treatment
		BatchRun testBatch = new BatchRun("GISP", "combo");
		Parameters params = testBatch.setParameters(
				0, //run number
				52, //end time
				1, //seed
				"combo",  //resistance
				"GISP", //counterfactual
				10, //yearX
				100, //initialinfected
				0.1, //propHighRisk

				1.0, //transmission
				1.0, //recoveryLambda
				1.0, //probSymptomatic
				1.0, //screen interval
				1.0, //delay to seek care
				1.0, //delay to retreatment
				1.0,//assortativity

				0.5,//riskGroupTransferProp
				0.5,//riskGroupTransmissionRatio
				0.05, //percent resistant A
				53, //begin importing B
				1.0, //importing B interval
				0.95, //DST sensitivity
				0.95, //DST specificity
				1, //care cost
				2, //diagnostic test cost
				3, //strain test cost
				4, //treatment A cost
				5, //treatment B cost
				6, //treatment X cost
				7,//treatmentEcost);
				0,
				0);
		SingleRun testRun = new SingleRun("output/tests", params);
		ThreadSafeSchedule schedule = new ThreadSafeSchedule();
		testRun.assignSchedule(schedule);
		ThreadSafeRandomHelper randomHelper = testRun.registerDistributions();
		Observer observer = testRun.createObserver(0, null, null, 0);
		testRun.createSurveillance("none");
		testRun.createIndivs(0);
		observer.setPopulation(testRun.population());
		
		double bigTestPopSize = 1000.0;

		for (int i = 0; i < bigTestPopSize; i++) {
			testRun.population().add(new Indiv(params, "msm", "high", randomHelper, observer, schedule));
		}	
	
		testRun.population().allIndivs().forEach(indiv -> indiv.infect("Both"));
			
		for (Indiv indiv : testRun.population().allInfectious().collect(Collectors.toList())) {
			Treatment treatment = new Treatment(indiv.myInfection(), observer);
			treatment.prescribeDrugA();
			treatment.prescribeDrugB();
			indiv.recoverNaturally();
		}
//		System.out.println(observer.getChecksForSequelae());
//		System.out.println(observer.getCasesEpi());
//		System.out.println(observer.getCasesDGI());
//		System.out.println(observer.getCasesBothSequelae());

		
		assertTrue("FailTreat", observer.getChecksForSequelae() == 3000);
		
	}
	
	@Test
	public void GISPTest() {
		//sequelae checks:
			//ineffective treatment
			//recover without treatment
		BatchRun testBatch = new BatchRun("GISP", "combo");
		Parameters params = testBatch.setParameters(
				0, //run number
				52, //end time
				1, //seed
				"combo",  //resistance
				"GISP", //counterfactual
				10, //yearX
				100, //initialinfected
				0.1, //propHighRisk

				1.0, //transmission
				1.0, //recoveryLambda
				1.0, //probSymptomatic
				1.0, //screen interval
				1.0, //delay to seek care
				1.0, //delay to retreatment
				1.0,//assortativity

				0.5,//riskGroupTransferProp
				0.5,//riskGroupTransmissionRatio
				0.05, //percent resistant A
				53, //begin importing B
				1.0, //importing B interval
				0.95, //DST sensitivity
				0.95, //DST specificity
				1, //care cost
				2, //diagnostic test cost
				3, //strain test cost
				4, //treatment A cost
				5, //treatment B cost
				6, //treatment X cost
				7,//treatmentEcost);
				0,
				0);
		SingleRun testRun = new SingleRun("output/tests", params);
		ThreadSafeSchedule schedule = new ThreadSafeSchedule();
		testRun.assignSchedule(schedule);
		ThreadSafeRandomHelper randomHelper = testRun.registerDistributions();
		CustomFileOutput outputter = new CustomFileOutput(true);
		testRun.assignOutputter(outputter);
		Observer observer = testRun.createObserver(0, null, null, 0);
		testRun.createSurveillance("none");
		testRun.createIndivs(0);
		observer.setPopulation(testRun.population());
		
		double bigTestPopSize = 10000.0;

		for (int i = 0; i < bigTestPopSize; i++) {
			Indiv indiv = new Indiv(params, "msm", "high", randomHelper, observer, schedule);
			testRun.population().add(indiv);
			indiv.setPop(testRun.population());
		}	
		
		testRun.population().updateActivityGroups();
	
		testRun.population().allIndivs().limit(500).forEach(indiv -> indiv.infect("Both"));
			
		for (Indiv indiv : testRun.population().allInfectious().collect(Collectors.toList())) {
			Treatment treatment = new Treatment(indiv.myInfection(), observer);
			try {
				treatment.treat();
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			treatment.retreat("B");
			treatment.retreat("E");
		}
		
		
		
		System.out.println(observer.getChecksForSequelae());
		System.out.println(observer.getCasesEpi());
		System.out.println(observer.getCasesDGI());
		System.out.println(observer.getCasesBothSequelae());

		
		assertTrue("FailTreat", observer.getChecksForSequelae() == 300);
		
	}
	
	@Test
	public void RandomTest() {
		//sequelae checks:
			//ineffective treatment
			//recover without treatment
		BatchRun testBatch = new BatchRun("random", "combo");
		Parameters params = testBatch.setParameters(
				0, //run number
				52, //end time
				1, //seed
				"combo",  //resistance
				"random", //counterfactual
				10, //yearX
				100, //initialinfected
				0.1, //propHighRisk

				1.0, //transmission
				1.0, //recoveryLambda
				1.0, //probSymptomatic
				1.0, //screen interval
				1.0, //delay to seek care
				1.0, //delay to retreatment
				1.0,//assortativity

				0.5,//riskGroupTransferProp
				0.5,//riskGroupTransmissionRatio
				0.05, //percent resistant A
				53, //begin importing B
				1.0, //importing B interval
				0.95, //DST sensitivity
				0.95, //DST specificity
				1, //care cost
				2, //diagnostic test cost
				3, //strain test cost
				4, //treatment A cost
				5, //treatment B cost
				6, //treatment X cost
				7,//treatmentEcost);
				0,
				0);
		SingleRun testRun = new SingleRun("output/tests", params);
		ThreadSafeSchedule schedule = new ThreadSafeSchedule();
		testRun.assignSchedule(schedule);
		ThreadSafeRandomHelper randomHelper = testRun.registerDistributions();
		CustomFileOutput outputter = new CustomFileOutput(true);
		testRun.assignOutputter(outputter);
		Observer observer = testRun.createObserver(0, null, null, 0);
		testRun.createSurveillance("none");
		testRun.createIndivs(0);
		observer.setPopulation(testRun.population());
		
		double bigTestPopSize = 10000.0;

		for (int i = 0; i < bigTestPopSize; i++) {
			Indiv indiv = new Indiv(params, "msm", "high", randomHelper, observer, schedule);
			testRun.population().add(indiv);
			indiv.setPop(testRun.population());
		}	
		
		testRun.population().updateActivityGroups();
	
		testRun.population().allIndivs().limit(500).forEach(indiv -> indiv.infect("Both"));
			
		for (Indiv indiv : testRun.population().allInfectious().collect(Collectors.toList())) {
			Treatment treatment = new Treatment(indiv.myInfection(), observer);
			try {
				treatment.treat();
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			indiv.recoverNaturally();
		}
		
		
		
		System.out.println(observer.getChecksForSequelae());
		System.out.println(observer.getCasesEpi());
		System.out.println(observer.getCasesDGI());
		System.out.println(observer.getCasesBothSequelae());

		
		assertTrue("FailTreat", observer.getChecksForSequelae() == 300);
		
	}

}
