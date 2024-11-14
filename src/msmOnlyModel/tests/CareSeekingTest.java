package msmOnlyModel.tests;

import static org.junit.Assert.*;

import java.util.List;
import java.util.stream.Collectors;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import msmOnlyModel.BatchRun;
import msmOnlyModel.SingleRun;
import msmOnlyModel.*;
import msmOnlyModel.ThreadSafeSchedule;
import repast.simphony.parameter.Parameters;


public class CareSeekingTest {

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

	public SingleRun setUpCareSeekingTest() {
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
	public void testSeekCareOne() {
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
		
		double prev = observer.calcPrev();
		//System.out.print(prev);
		assertTrue("SeekCareOne1", prev==100.0);
		
		CareSeeking care = new CareSeeking(indiv1, observer, schedule);
		care.seekCare();
		
		double prev2 = observer.calcPrev();
		//System.out.print(prev2);
		assertTrue("SeekCareOne2", prev2==0.0);
	}
	@Test
	public void testSeekCare() {
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
		
		double prev = observer.calcPrev();
		assertTrue("Care1", prev == 50.0);
	
		
		//treat everyone and verify that prev is now 0
		List<Indiv> popList = testRun.population().allIndivs().collect(Collectors.toList());
		
		for (Indiv indiv : popList) {
			CareSeeking care = new CareSeeking(indiv, observer, schedule);
			care.seekCare();
		}
		
		double soughtCare = observer.getSoughtCare();
		double treatments = observer.getTreatments();
		assertTrue("Care3", treatments == 50);
		double prev2 = observer.calcPrev();
		assertTrue("Care2", prev2 == 0.0);

		
		
	}
	

}
