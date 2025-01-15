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
	}
	
	@Test
	public void RandomTest() {
		BatchRun testBatch = new BatchRun("random", "combo");
		Parameters params = testBatch.setParameters(
				0, //run number
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
		SingleRun testRun = new SingleRun("/Users/me597/Documents/MSMoutput/tests", params);
		ThreadSafeSchedule schedule = new ThreadSafeSchedule();
		testRun.assignSchedule(schedule);
		ThreadSafeRandomHelper randomHelper = testRun.registerDistributions();
		Observer observer = testRun.createObserver(0, null, null, 0);
		testRun.createSurveillance("none");
		testRun.createIndivs(0);
		observer.setPopulation(testRun.population());
		
		Indiv indiv1 = new Indiv(params, "msm", "high", randomHelper, observer, schedule);
		testRun.population().add(indiv1);
		
	}
	
	@Test
	public void TOCasymptSusAllTest() {
		BatchRun testBatch = new BatchRun("test-of-cure", "combo");
		Parameters params = testBatch.setParameters(
				0, //run number
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
		SingleRun testRun = new SingleRun("/Users/me597/Documents/MSMoutput/tests", params);
		ThreadSafeSchedule schedule = new ThreadSafeSchedule();
		testRun.assignSchedule(schedule);
		ThreadSafeRandomHelper randomHelper = testRun.registerDistributions();
		Observer observer = testRun.createObserver(0, null, null, 0);
		testRun.createSurveillance("none");
		testRun.createIndivs(0);
		observer.setPopulation(testRun.population());
		
		Indiv indiv1 = new Indiv(params, "msm", "high", randomHelper, observer, schedule);
		testRun.population().add(indiv1);
		
	}
	
	@Test
	public void TOCasymptResistASusBTest() {
		BatchRun testBatch = new BatchRun("test-of-cure", "combo");
		Parameters params = testBatch.setParameters(
				0, //run number
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
		SingleRun testRun = new SingleRun("/Users/me597/Documents/MSMoutput/tests", params);
		ThreadSafeSchedule schedule = new ThreadSafeSchedule();
		testRun.assignSchedule(schedule);
		ThreadSafeRandomHelper randomHelper = testRun.registerDistributions();
		Observer observer = testRun.createObserver(0, null, null, 0);
		testRun.createSurveillance("none");
		testRun.createIndivs(0);
		observer.setPopulation(testRun.population());
		
		Indiv indiv1 = new Indiv(params, "msm", "high", randomHelper, observer, schedule);
		testRun.population().add(indiv1);
		
	}
	
	@Test
	public void TOCasymptResistABTest() {
		BatchRun testBatch = new BatchRun("test-of-cure_100", "combo");
		Parameters params = testBatch.setParameters(
				0, //run number
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
		SingleRun testRun = new SingleRun("/Users/me597/Documents/MSMoutput/tests", params);
		ThreadSafeSchedule schedule = new ThreadSafeSchedule();
		testRun.assignSchedule(schedule);
		ThreadSafeRandomHelper randomHelper = testRun.registerDistributions();
		Observer observer = testRun.createObserver(0, null, null, 0);
		testRun.createSurveillance("none");
		testRun.createIndivs(0);
		observer.setPopulation(testRun.population());
		
		Indiv indiv1 = new Indiv(params, "msm", "high", randomHelper, observer, schedule);
		testRun.population().add(indiv1);
		
	}
	
	
	@Test
	public void DSTtest() {
		BatchRun testBatch = new BatchRun("DST_100", "combo");
		Parameters params = testBatch.setParameters(
				0, //run number
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
