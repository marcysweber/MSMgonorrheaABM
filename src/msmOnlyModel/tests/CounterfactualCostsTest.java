package msmOnlyModel.tests;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import msmOnlyModel.BatchRun;
import msmOnlyModel.Indiv;
import msmOnlyModel.Observer;
import msmOnlyModel.SingleRun;
import msmOnlyModel.ThreadSafeRandomHelper;
import msmOnlyModel.ThreadSafeSchedule;
import repast.simphony.parameter.Parameters;

public class CounterfactualCostsTest {
	
	//tests here should recreate a full treatment course for resistant infections
	//under different counterfactuals and availabilities of drug X
	
	//a single indiv is created and infected
	//they are confirmed to be either asymptomatic or symptomatic
	//symptomatic indivs have their care scheduled 
	//then the schedule is executed until there are no more actions in the schedule
	//the indiv should no longer be infectious. 
	//cost will depend on counterfactual and avail of X, and resistance.
	//correct treatment protocol can also be confirmed by treatment attempts

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

	@Test
	public void test() {
		fail("Not yet implemented");
	}
	
	@Test
	public void RandomTest() {
		BatchRun testBatch = new BatchRun("random", "combo");
		Parameters params = testBatch.setParameters(
				0, //run number
				52, //end time
				1, //seed
				"combo",  //resistance
				"random", //counterfactual
				10, //yearX
				100, //initialinfected
				1.0, //transmission
				1.0, //recoveryLambda
				0.5, //probSymptomatic
				1.0, //screen interval
				1.0, //delay to seek care
				1.0, //delay to retreatment
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
				7); //treatment E cost
		SingleRun testRun = new SingleRun("/Users/me597/Documents/MSMoutput/tests", params);
		ThreadSafeSchedule schedule = new ThreadSafeSchedule();
		testRun.assignSchedule(schedule);
		ThreadSafeRandomHelper randomHelper = testRun.registerDistributions();
		Observer observer = testRun.createObserver(0, null, null, 0);
		testRun.createSurveillance("none");
		testRun.createIndivs(0);
		observer.setPopulation(testRun.population());
		
		
		
	}
	
	@Test
	public void TOCsymptSusAllTest() {
		BatchRun testBatch = new BatchRun("test-of-cure", "combo");
		Parameters params = testBatch.setParameters(
				0, //run number
				52, //end time
				1, //seed
				"combo",  //resistance
				"test-of-cure_100", //counterfactual
				10, //yearX
				100, //initialinfected
				1.0, //transmission
				1.0, //recoveryLambda
				0.5, //probSymptomatic
				1.0, //screen interval
				1.0, //delay to seek care
				1.0, //delay to retreatment
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
				7); //treatment E cost
		SingleRun testRun = new SingleRun("/Users/me597/Documents/MSMoutput/tests", params);
		ThreadSafeSchedule schedule = new ThreadSafeSchedule();
		testRun.assignSchedule(schedule);
		ThreadSafeRandomHelper randomHelper = testRun.registerDistributions();
		Observer observer = testRun.createObserver(0, null, null, 0);
		testRun.createSurveillance("none");
		testRun.createIndivs(0);
		observer.setPopulation(testRun.population());
		
		Indiv indiv1 = new Indiv(params, "msm", randomHelper, observer, schedule);
		testRun.population().add(indiv1);
		
	}
	
	@Test
	public void TOCasymptSusAllTest() {
		BatchRun testBatch = new BatchRun("test-of-cure", "combo");
		Parameters params = testBatch.setParameters(
				0, //run number
				52, //end time
				1, //seed
				"combo",  //resistance
				"test-of-cure_100", //counterfactual
				10, //yearX
				100, //initialinfected
				1.0, //transmission
				1.0, //recoveryLambda
				0.5, //probSymptomatic
				1.0, //screen interval
				1.0, //delay to seek care
				1.0, //delay to retreatment
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
				7); //treatment E cost
		SingleRun testRun = new SingleRun("/Users/me597/Documents/MSMoutput/tests", params);
		ThreadSafeSchedule schedule = new ThreadSafeSchedule();
		testRun.assignSchedule(schedule);
		ThreadSafeRandomHelper randomHelper = testRun.registerDistributions();
		Observer observer = testRun.createObserver(0, null, null, 0);
		testRun.createSurveillance("none");
		testRun.createIndivs(0);
		observer.setPopulation(testRun.population());
		
		Indiv indiv1 = new Indiv(params, "msm", randomHelper, observer, schedule);
		testRun.population().add(indiv1);
		
	}
	
	@Test
	public void TOCasymptResistASusBTest() {
		BatchRun testBatch = new BatchRun("test-of-cure", "combo");
		Parameters params = testBatch.setParameters(
				0, //run number
				52, //end time
				1, //seed
				"combo",  //resistance
				"test-of-cure_100", //counterfactual
				10, //yearX
				100, //initialinfected
				1.0, //transmission
				1.0, //recoveryLambda
				0.5, //probSymptomatic
				1.0, //screen interval
				1.0, //delay to seek care
				1.0, //delay to retreatment
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
				7); //treatment E cost
		SingleRun testRun = new SingleRun("/Users/me597/Documents/MSMoutput/tests", params);
		ThreadSafeSchedule schedule = new ThreadSafeSchedule();
		testRun.assignSchedule(schedule);
		ThreadSafeRandomHelper randomHelper = testRun.registerDistributions();
		Observer observer = testRun.createObserver(0, null, null, 0);
		testRun.createSurveillance("none");
		testRun.createIndivs(0);
		observer.setPopulation(testRun.population());
		
		Indiv indiv1 = new Indiv(params, "msm", randomHelper, observer, schedule);
		testRun.population().add(indiv1);
		
	}
	
	@Test
	public void TOCasymptResistABTest() {
		BatchRun testBatch = new BatchRun("test-of-cure_100", "combo");
		Parameters params = testBatch.setParameters(
				0, //run number
				52, //end time
				1, //seed
				"combo",  //resistance
				"test-of-cure_100", //counterfactual
				10, //yearX
				100, //initialinfected
				1.0, //transmission
				1.0, //recoveryLambda
				0.5, //probSymptomatic
				1.0, //screen interval
				1.0, //delay to seek care
				1.0, //delay to retreatment
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
				7); //treatment E cost
		SingleRun testRun = new SingleRun("/Users/me597/Documents/MSMoutput/tests", params);
		ThreadSafeSchedule schedule = new ThreadSafeSchedule();
		testRun.assignSchedule(schedule);
		ThreadSafeRandomHelper randomHelper = testRun.registerDistributions();
		Observer observer = testRun.createObserver(0, null, null, 0);
		testRun.createSurveillance("none");
		testRun.createIndivs(0);
		observer.setPopulation(testRun.population());
		
		Indiv indiv1 = new Indiv(params, "msm", randomHelper, observer, schedule);
		testRun.population().add(indiv1);
		
	}
	
	
	@Test
	public void DSTtest() {
		BatchRun testBatch = new BatchRun("DST_100", "combo");
		Parameters params = testBatch.setParameters(
				0, //run number
				52, //end time
				1, //seed
				"combo",  //resistance
				"DST_100", //counterfactual
				10, //yearX
				100, //initialinfected
				1.0, //transmission
				1.0, //recoveryLambda
				0.5, //probSymptomatic
				1.0, //screen interval
				1.0, //delay to seek care
				1.0, //delay to retreatment
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
				7); //treatment E cost
		SingleRun testRun = new SingleRun("/Users/me597/Documents/MSMoutput/tests", params);
		ThreadSafeSchedule schedule = new ThreadSafeSchedule();
		testRun.assignSchedule(schedule);
		ThreadSafeRandomHelper randomHelper = testRun.registerDistributions();
		Observer observer = testRun.createObserver(0, null, null, 0);
		testRun.createSurveillance("none");
		testRun.createIndivs(0);
		observer.setPopulation(testRun.population());
	}
	
	@Test
	public void GISPtest() {
		BatchRun testBatch = new BatchRun("GISP", "combo");
		Parameters params = testBatch.setParameters(
				0, //run number
				52, //end time
				1, //seed
				"combo",  //resistance
				"GISP", //counterfactual
				10, //yearX
				100, //initialinfected
				1.0, //transmission
				1.0, //recoveryLambda
				0.5, //probSymptomatic
				1.0, //screen interval
				1.0, //delay to seek care
				1.0, //delay to retreatment
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
				7); //treatment E cost
		SingleRun testRun = new SingleRun("/Users/me597/Documents/MSMoutput/tests", params);
		ThreadSafeSchedule schedule = new ThreadSafeSchedule();
		testRun.assignSchedule(schedule);
		ThreadSafeRandomHelper randomHelper = testRun.registerDistributions();
		Observer observer = testRun.createObserver(0, null, null, 0);
		testRun.createSurveillance("none");
		testRun.createIndivs(0);
		observer.setPopulation(testRun.population());
	}

}
