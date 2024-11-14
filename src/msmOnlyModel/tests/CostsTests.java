package msmOnlyModel.tests;

import static org.junit.Assert.*;

import java.util.List;
import java.util.stream.Collectors;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import msmOnlyModel.*;
import repast.simphony.parameter.Parameters;

public class CostsTests {

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

	public SingleRun setUpCostsTest() {
		BatchRun testBatch = new BatchRun("GISP", "combo");
		Parameters params = testBatch.setParameters(1,//runNumber
				1000,//endtime
				1, //seed
				"none", //resistance
				"none", //counterfactual
				10, //yearX
				10, //initial infected
				10, //transmission
				1 , //recoveryLambda
				0.5, //probSymptomatic
				2, //screenInterval
				1, //delaytoseekcare
				2, //delaytoretreatment
				0.5,//riskGroupTransferProp
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
	public void CareCostTest() {
		//accurate cost of a single treatment of symptomatic infection
		
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

		Indiv indiv1 = new Indiv(params, "msm", randomHelper, observer, schedule);
		testRun.population().add(indiv1);
		
		indiv1.infect("none");
		while (!indiv1.symptoms()) { //keep trying until you get a symptomatic infection
			indiv1.infect("none");
		}
		

		CareSeeking care = new CareSeeking(indiv1, observer, schedule);
		care.seekCare();
		//diagnostic test + care + treatment A
		//2 + 1 + 4 = 7
		
		CostCalc costCalc = testRun.observer().getCostCalc();
		//System.out.println(costCalc.getMonetaryCost());
		assertTrue("CareCost1", costCalc.getMonetaryCost() == 7);

		
	}
	
	@Test
	public void MultiCareCostTest() {
		//accurate cost for several treatments of symptomatic infections
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
		
		double bigTestPopSize = 100.0;

		for (int i = 0; i < bigTestPopSize; i++) {
			testRun.population().add(new Indiv(params, "msm", randomHelper, observer, schedule));
		}	
	
		//infect half with symptomatic infections
		double infections = bigTestPopSize/2;

		for (int index = 0; index < infections; index++) {
			Indiv indiv = testRun.population().allIndivs().collect(Collectors.toList()).get(index);
			indiv.infect("none");
			
			while (!indiv.symptoms()) { //keep going until the infection is symptomatic
				indiv.infect("none");
			}
		}
		
		List<Indiv> popList = testRun.population().allIndivs().collect(Collectors.toList());
		
		for (Indiv indiv : popList) {
			CareSeeking care = new CareSeeking(indiv, observer, schedule);
			care.seekCare();
		}
		
		//everybody got a diagnostic test: 100 * 2
		//everybody got care: 50 * 1
		//50 got treatment A: 50 * 4
		
		CostCalc costCalc = testRun.observer().getCostCalc();
		//System.out.println(costCalc.getMonetaryCost());
		assertTrue("MultiCareCost1", costCalc.getMonetaryCost() == 500);
	}
	
	@Test
	public void StrainTestCostTest() {
		//accurate cost for one round of GISP surveillance sampling
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

		Indiv indiv1 = new Indiv(params, "msm", randomHelper, observer, schedule);
		testRun.population().add(indiv1);
		indiv1.infect("none");
		
		Screener screener = new Screener();
		screener.screen(indiv1, observer);
		
		testRun.surveillanceProgram().collectSamples(observer.getDetectedList());
		
		CostCalc costCalc = testRun.observer().getCostCalc();
		assertTrue("StrainTestCost", costCalc.getMonetaryCost()==7);
		
		
	}

	@Test
	public void ScreenCostTest() {
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
		CostCalc costCalc = testRun.observer().getCostCalc();
		
		
		//no cost for screening of single indiv who was not infected
		Indiv indiv1 = new Indiv(params, "msm", randomHelper, observer, schedule);
		testRun.population().add(indiv1);
		Screener screener = new Screener();
		screener.screen(indiv1, observer);
		assertTrue("ScreeningCost", costCalc.getMonetaryCost()==0);

		
		//accurate cost for screening of a single individual who was infected
		Indiv indiv2 = new Indiv(params, "msm", randomHelper, observer, schedule);
		testRun.population().add(indiv2);
		indiv2.infect("none");
		Screener screener2 = new Screener();
		screener2.screen(indiv2, observer);
		assertTrue("ScreeningCost2", costCalc.getMonetaryCost()==4);

		
	}
	
	@Test
	public void QALYLostSymptSusTest() {
		//accurate loss of QALYs for symptomatic infection without resistance
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
		CostCalc costCalc = testRun.observer().getCostCalc();
		
		Indiv indiv1 = new Indiv(params, "msm", randomHelper, observer, schedule);
		testRun.population().add(indiv1);
		indiv1.infect("none");
		while (!indiv1.symptoms()) { //keep trying until you get a symptomatic infection
			indiv1.infect("none");
		}
		
		CareSeeking care = new CareSeeking(indiv1, observer, schedule);
		care.scheduleSeekCare();
		
		assertTrue("QALYLostSympSusTest1", indiv1.infectious());
		Treatment treatment = new Treatment(indiv1, observer);
		try {
			treatment.treat();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		assertTrue("QALYLostSympSusTest2", !indiv1.infectious());
		//System.out.println("sus " + costCalc.getQALYsLost());
		assertTrue("QALYLostSympSusTest3", costCalc.getQALYsLost()>0.0001);

		
	}
	
	@Test
	public void QALYLostSymptResistTest(){
		//accurate loss of QALYs for a symptomatic infection resistant to drug A
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
		CostCalc costCalc = testRun.observer().getCostCalc();
		
		Indiv indiv1 = new Indiv(params, "msm", randomHelper, observer, schedule);
		testRun.population().add(indiv1);
		indiv1.infect("A");
		while (!indiv1.symptoms()) { //keep trying until you get a symptomatic infection
			indiv1.infect("A");
		}
		
		CareSeeking care = new CareSeeking(indiv1, observer, schedule);
		care.scheduleSeekCare();
		
		assertTrue("QALYLostSympSusTest1", indiv1.infectious());
		Treatment treatment = new Treatment(indiv1, observer);
		try {
			treatment.treat();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		assertTrue("QALYLostSympSusTest2", indiv1.infectious());

		treatment.retreat("B");
		
		assertTrue("QALYLostSympSusTest2", !indiv1.infectious());
		//System.out.println("resist " + costCalc.getQALYsLost());
		assertTrue("QALYLostSympSusTest3", costCalc.getQALYsLost()>0.0005);
	}
	
	@Test
	public void QALYLostAsymptResistTest() {
		//accurate loss of QALYs (none) for asymptomatic infection
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
		CostCalc costCalc = testRun.observer().getCostCalc();
		
		Indiv indiv1 = new Indiv(params, "msm", randomHelper, observer, schedule);
		testRun.population().add(indiv1);
		indiv1.infect("A");
		while (indiv1.symptoms()) { //keep trying until you get an asymptomatic infection
			indiv1.infect("A");
		}
		
		CareSeeking care = new CareSeeking(indiv1, observer, schedule);
		care.scheduleSeekCare();
		
		assertTrue("QALYLostSympSusTest1", indiv1.infectious());
		Treatment treatment = new Treatment(indiv1, observer);
		try {
			treatment.treat();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		assertTrue("QALYLostSympSusTest2", indiv1.infectious());

		treatment.retreat("B");
		
		assertTrue("QALYLostSympSusTest2", !indiv1.infectious());
		//System.out.println("resist " + costCalc.getQALYsLost());
		assertTrue("QALYLostSympSusTest3", costCalc.getQALYsLost()==0.0);
	}
		
	
}
