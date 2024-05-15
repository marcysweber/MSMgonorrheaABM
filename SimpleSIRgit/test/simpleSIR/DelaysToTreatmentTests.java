package simpleSIR;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import cern.jet.random.Uniform;
import cern.jet.random.engine.RandomEngine;
import repast.simphony.context.Context;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.environment.RunState;
import repast.simphony.engine.schedule.ISchedule;
import repast.simphony.random.RandomHelper;
import repast.simphony.util.collections.IndexedIterable;

public class DelaysToTreatmentTests {

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
	}

	@Before
	public void setUp() throws Exception {
		TestDrive.setUp(Main.setParameters(1000, 1, "none", "none", 10, 4000, 4.5, 0.5, 1, 3, 10, 10, 50, 500, 25,
				95, 97, 1, 1.5,2, 3, 4, 5 ,6));

	}

	@After
	public void tearDown() throws Exception {
	}

	//things to test:
	
	//symptomatic cases can now infect others, esp if delay to care is long and transmission is high
	
	@Test
	public void SymptomaticTransmissionTest() {
		//setup stuff
		ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true), 1,0,0);
		context.add(observer);
		SurveillanceProgram surveillance = new SurveillanceProgram();
		context.add(surveillance);
		
		//make a very small pop
		double testPopSize = 10.0;
		
		//make the new indivs
		for (int i = 0; i < testPopSize; i++) {
			context.add(new Indiv());
		}
		
		//infect 1 with a symptomatic case (probsymptomatic = 1)
		IndexedIterable<Object> indivs = context.getObjects(Indiv.class);
		Indiv indiv = (Indiv) indivs.get(0);
		indiv.infect("none");
		
		assertTrue("SymptomaticTransmissionTest1", 0.0==observer.calcAllStrainPrev(testPopSize, context));
		assertTrue("SymptomaticTransmissionTest2", 10.0==observer.calcPrev(testPopSize, context));

		for (int i = 0; i < 10; i++) {
			schedule.execute();
		}
		
		assertTrue("SymptomaticTransmissionTest2", 10.0>observer.calcPrev(testPopSize, context));
		
	}
	
	
	//resistant cases and retreatment
	@Test
	public void RetreatmentTest() {
		ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true), 1,0,0);
		context.add(observer);
		SurveillanceProgram surveillance = new SurveillanceProgram();
		context.add(surveillance);
		
		//resistant and symptomatic cases being treated results in actions in the schedule
		Indiv resistantIndiv = new Indiv();
		context.add(resistantIndiv);
		resistantIndiv.infect("A");
		
		Treatment treatment = new Treatment(resistantIndiv, observer);
		treatment.treat();
		
		assertTrue("RetreatmentTest1", schedule.getActionCount()==3);
		
		//susceptible cases being treated should result in no ADDITIONAL actions in schedule
		//(infectiousActions and natural recovery will still be in the schedule)
		Indiv susIndiv = new Indiv();
		context.add(susIndiv);
		susIndiv.infect("none");
		
		Treatment treatment2 = new Treatment(susIndiv, observer);
		treatment2.treat();
		
		assertTrue("RetreatmentTest2", schedule.getActionCount()==5);
	}
	
	//abstinence
	@Test
	public void AbstinenceTest() {
		ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true), 1,0,0);
		context.add(observer);
		SurveillanceProgram surveillance = new SurveillanceProgram();
		context.add(surveillance);
		
		
		
		//a symptomatic case between treatment and retreatment cannot transmit
		//partnerSelect should never be called, so even if there are no other agents, no error when calling infectiousActions
		Indiv symptIndiv = new Indiv();
		context.add(symptIndiv);
		symptIndiv.infect("A");
		
		Treatment treatment = new Treatment(symptIndiv, observer);
		treatment.treat();
		
		assertTrue("AbstinenceTest1", symptIndiv.infectious());
		
		symptIndiv.infectiousActions();
		
		
		
		//an asymptomatic resistant case should be able to receive treatment and then continue to transmit
		context.remove(symptIndiv);
		double testPopSize = 9.0;
		
		//make the new indivs
		for (int i = 0; i < testPopSize; i++) {
			context.add(new Indiv());
		}
		RandomEngine eng = RandomHelper.getGenerator();
		Uniform partnerSelectUniform = new Uniform(0, 9, eng);
		RandomHelper.registerDistribution("partnerSelectUniform", partnerSelectUniform);
		
		
		Indiv asymptIndiv = new Indiv();
		context.add(asymptIndiv);
		asymptIndiv.infectInit("A");
		
		while (asymptIndiv.symptoms()) {
			asymptIndiv.infectInit("A");
		}
		
		assertTrue("AbstinenceTest2", observer.calcPrev(10, context)==10);
		
		Treatment treatment2 = new Treatment(asymptIndiv, observer);
		treatment2.treat();
		
		assertTrue("AbstinenceTest3", asymptIndiv.infectious());

		for (int i = 0; i < 10; i++) {
			asymptIndiv.infectiousActions();
		}

		assertTrue("AbstinenceTest4", (observer.calcPrev(10, context) >= 10.0));
		
	}

}
