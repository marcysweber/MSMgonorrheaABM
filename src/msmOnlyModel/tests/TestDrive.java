package msmOnlyModel.tests;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import cern.jet.random.Beta;
import cern.jet.random.Exponential;
import cern.jet.random.Normal;
import cern.jet.random.Uniform;
import cern.jet.random.engine.RandomEngine;
import msmOnlyModel.BatchRun;
import repast.simphony.context.Context;
import repast.simphony.context.DefaultContext;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.environment.RunState;
import repast.simphony.engine.schedule.Schedule;
import repast.simphony.parameter.Parameters;
import repast.simphony.random.RandomHelper;

public class TestDrive {

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
	}


	public static void setUp(Parameters parameters) throws Exception {
		//make a dummy context
		//make a dummy schedule

		Schedule schedule = new Schedule();
		RunEnvironment.init(schedule, null, null, true);
		Context context = new DefaultContext();
		RunState.init().setMasterContext(context);
		Parameters params = parameters;
		RunEnvironment.getInstance().setParameters(params);

	
		RandomEngine eng = RandomHelper.registerGenerator("myStream", 6);
		
		Exponential recoveryExp = new Exponential(1/params.getDouble("recovery_lambda"), eng);
		RandomHelper.registerDistribution("recoveryExp", recoveryExp);
		
		//dist for contact
		Uniform zeroOneUniform = new Uniform(0.0, 1.0, eng);
		RandomHelper.registerDistribution("zeroOneUniform", zeroOneUniform);
		
		Uniform genderUniform = new Uniform(0.0, 1.0, eng);
		RandomHelper.registerDistribution("genderUniform", genderUniform);
		
		Beta genderPrefBeta = new Beta(0.5, 0.05, eng);
		RandomHelper.registerDistribution("genderPrefBeta", genderPrefBeta);
		
		Uniform partnerGenderUniform = new Uniform(0.0, 1.0, eng);
		RandomHelper.registerDistribution("partnerGenderUniform", partnerGenderUniform);
		
		Uniform symptomaticUniform = new Uniform(0.0,1.0, eng);
		RandomHelper.registerDistribution("symptomaticUniform", symptomaticUniform);
		
		Normal screenIntervalNormal = new Normal(params.getDouble("screen_interval") * 52, params.getDouble("screen_interval") * 52/10, eng);
		RandomHelper.registerDistribution("screenIntervalNormal", screenIntervalNormal);
		Uniform screenFirstValueUniform = new Uniform(0, params.getDouble("screen_interval") * 52, eng);
		RandomHelper.registerDistribution("screenFirstValueUniform", screenFirstValueUniform);
		
		Uniform adherenceUniform = new Uniform(0.0, 1.0, eng);
		RandomHelper.registerDistribution("adherenceUniform", adherenceUniform);
		
		Uniform randomDrugUniform = new Uniform(0.0, 1.0, eng);
		RandomHelper.registerDistribution("randomDrugUniform", randomDrugUniform);
		
		Uniform partnerSelectUniform = new Uniform(0, params.getInteger("population_size") - 1, eng);
		RandomHelper.registerDistribution("partnerSelectUniform", partnerSelectUniform);
		
		Uniform importResistantUniform = new Uniform(0, 20 - 1, eng);
		RandomHelper.registerDistribution("importResistantUniform", importResistantUniform);
		
		Uniform developResistanceUniform = new Uniform(0.0,1.0, eng);
		RandomHelper.registerDistribution("developResistanceUniform", developResistanceUniform);
		
		Uniform testsUniform = new Uniform(0.0,1.0, eng);
		RandomHelper.registerDistribution("testsUniform", testsUniform);
		
		Exponential delayToSeekCareExp = new Exponential(1/params.getDouble("delay_to_seek_care") * 52, eng);
		RandomHelper.registerDistribution("delayToSeekCareExp", delayToSeekCareExp);
		
		Exponential delayToRetreatmentExp = new Exponential(1/params.getDouble("delay_to_retreatment") * 52, eng);
		RandomHelper.registerDistribution("delayToRetreatmentExp", delayToRetreatmentExp);
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void test() {
		fail("Not yet implemented");
	}

	@Test
	public void testBatch() {
		BatchRun testBatch = new BatchRun("test", "none");
		testBatch.setParameters(
				1,//runNumber
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
				7);//treatmentEcost
		
	}
	
}
