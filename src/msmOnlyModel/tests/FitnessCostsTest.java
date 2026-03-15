package msmOnlyModel.tests;

import static org.junit.Assert.*;

import java.util.stream.Collectors;

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

public class FitnessCostsTest {

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
	public void test1() {
		BatchRun testBatch = new BatchRun("GISP", "combo");
		Parameters params = testBatch.setParameters(1,//runNumber
				1000,//endtime
				1, //seed
				"combo", //resistance
				"none", //counterfactual
				10, //yearX
				10, //initial infected
				1.0, //propHighRisk
				300, //transmission - this makes the probability of transmission almost 100%
				1 , //recoveryLambda
				0.5, //probSymptomatic
				2, //screenInterval
				1, //delaytoseekcare
				2, //delaytoretreatment
				1.0,//assortativity
				0.0,//riskGroupTransferProp
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
		
		int popsize = 1000;
		
		SingleRun testRun = new SingleRun("/Users/me597/Documents/MSMoutput/tests", params);
		ThreadSafeSchedule schedule = new ThreadSafeSchedule();
		testRun.assignSchedule(schedule);
		ThreadSafeRandomHelper randomHelper = testRun.registerDistributions();
		testRun.assignRandomHelper(randomHelper);
		Observer observer = testRun.createObserver(0, null, null, 0);
		testRun.createSurveillance("none");
		testRun.createIndivs(popsize);
		observer.setPopulation(testRun.population());
		
		
		int infections = popsize/10; //10% seeded infections

		for (int index = 0; index < infections; index++) {
			Indiv indiv = testRun.population().allIndivs().collect(Collectors.toList()).get(index);
			indiv.infect("none");
			
		}
		
		assertTrue(testRun.population().msmInfected().count()==100);
		
		for (int index = 0; index < infections; index++) {
			Indiv indiv = testRun.population().allIndivs().collect(Collectors.toList()).get(index);
			
			indiv.infectiousActions(); // the 10% should infect another 10%
		}
		assertTrue(observer.getReinfectedCount()==10);
		assertTrue(testRun.population().msmInfected().count()==190);
		
		//Baseline transmission - with these parameters, we are able to get 
		//90 new infections from 100 seeded infections for a total of 190 infections
		
	}

	@Test
	public void test2() {
		BatchRun testBatch = new BatchRun("GISP", "combo");
		Parameters params = testBatch.setParameters(1,//runNumber
				1000,//endtime
				5, //seed
				"combo", //resistance
				"none", //counterfactual
				10, //yearX
				10, //initial infected
				1.0, //propHighRisk
				300, //transmission - this makes the probability of transmission almost 100%
				1 , //recoveryLambda
				0.5, //probSymptomatic
				2, //screenInterval
				1, //delaytoseekcare
				2, //delaytoretreatment
				1.0,//assortativity
				0.0,//riskGroupTransferProp
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
				0.5,
				0);
		
		int popsize = 1000;
		
		SingleRun testRun = new SingleRun("/Users/me597/Documents/MSMoutput/tests", params);
		ThreadSafeSchedule schedule = new ThreadSafeSchedule();
		testRun.assignSchedule(schedule);
		ThreadSafeRandomHelper randomHelper = testRun.registerDistributions();
		testRun.assignRandomHelper(randomHelper);
		Observer observer = testRun.createObserver(0, null, null, 0);
		testRun.createSurveillance("none");
		testRun.createIndivs(popsize);
		observer.setPopulation(testRun.population());
		
		
		
		
		
		
		int infections = popsize/10; //10% seeded infections

		for (int index = 0; index < infections; index++) {
			Indiv indiv = testRun.population().allIndivs().collect(Collectors.toList()).get(index);
			indiv.infect("A");
			
		}
		
		assertTrue(testRun.population().msmInfected().count()==100);

		
		for (int index = 0; index < infections; index++) {
			Indiv indiv = testRun.population().allIndivs().collect(Collectors.toList()).get(index);
			
			indiv.infectiousActions(); // the 10% should infect another 10%
		}
		//assertTrue(observer.getReinfectedCount()==10);

		assertTrue(130 < testRun.population().msmInfected().count());
		assertTrue(150 > testRun.population().msmInfected().count());
		
		//if we keep everything the same as test 1 except
		//change the strain to A and reduce the fitness of strain A,
		//then we only get around 40 new infections, for a total of around 140
		
	}
	
	@Test
	public void test3() {
		BatchRun testBatch = new BatchRun("GISP", "combo");
		Parameters params = testBatch.setParameters(1,//runNumber
				1000,//endtime
				10, //seed
				"combo", //resistance
				"none", //counterfactual
				10, //yearX
				10, //initial infected
				1.0, //propHighRisk
				300, //transmission - this makes the probability of transmission almost 100%
				1 , //recoveryLambda
				0.5, //probSymptomatic
				2, //screenInterval
				1, //delaytoseekcare
				2, //delaytoretreatment
				1.0,//assortativity
				0.0,//riskGroupTransferProp
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
				0.5,
				0.5);
		
		int popsize = 1000;
		
		SingleRun testRun = new SingleRun("/Users/me597/Documents/MSMoutput/tests", params);
		ThreadSafeSchedule schedule = new ThreadSafeSchedule();
		testRun.assignSchedule(schedule);
		ThreadSafeRandomHelper randomHelper = testRun.registerDistributions();
		testRun.assignRandomHelper(randomHelper);
		Observer observer = testRun.createObserver(0, null, null, 0);
		testRun.createSurveillance("none");
		testRun.createIndivs(popsize);
		observer.setPopulation(testRun.population());
		
		
		
		
		
		
		int infections = popsize/10; //10% seeded infections

		for (int index = 0; index < infections; index++) {
			Indiv indiv = testRun.population().allIndivs().collect(Collectors.toList()).get(index);
			indiv.infect("B");
			
		}
		
		assertTrue(testRun.population().msmInfected().count()==100);

		
		for (int index = 0; index < infections; index++) {
			Indiv indiv = testRun.population().allIndivs().collect(Collectors.toList()).get(index);
			
			indiv.infectiousActions(); // the 10% should infect another 10%
		}
		//assertTrue(observer.getReinfectedCount()==10);

		assertTrue(130 < testRun.population().msmInfected().count());
		assertTrue(150 > testRun.population().msmInfected().count());
		
		//same as test 2, except we use strain B. again, 
		//we only get around 40 new infections, for a total of around 140.
		//the exact numbers change as you adjust the random seed (third argument of setParameters)
		
		//this seems to indicate that the fitness costs are working as expected!
				
			
		
	}
	
	
}
