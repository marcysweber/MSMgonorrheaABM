package simpleSIR;

import static org.junit.Assert.*;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import cern.jet.random.Exponential;
import cern.jet.random.Uniform;
import cern.jet.random.engine.RandomEngine;
import repast.simphony.context.Context;
import repast.simphony.context.DefaultContext;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.environment.RunState;
import repast.simphony.engine.schedule.ISchedule;
import repast.simphony.engine.schedule.Schedule;
import repast.simphony.engine.schedule.ScheduleParameters;
import repast.simphony.parameter.Parameters;
import repast.simphony.random.RandomHelper;

public class IndivTests {

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
	}

	@Before
	public void setUp() throws Exception {
		TestDrive.setUp(Main.setParameters(1000, 1, "none", "none", 10, 4000, 1000, 0.5, 3, 0.5, 2, 50, 500, 25,95,97,1, 1,1.5, 2, 3, 4,5,6));
	}

	@After
	public void tearDown() throws Exception {
	}




	@Test
	public void testRecover() {
		
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true),1,0,0);
		context.add(observer);
		
		RandomEngine eng = RandomHelper.getGenerator();
		Uniform partnerSelectUniform = new Uniform(0, 3, eng);
		RandomHelper.registerDistribution("partnerSelectUniform", partnerSelectUniform);
		
		SurveillanceProgram surveillance = new SurveillanceProgram();
		context.add(surveillance);
		
		//should change state of Indiv from 1 to 2
		//should leave alone if Indiv.state is 0 or 2
		
		//make an infectious indiv
		Indiv infectiousIndiv = new Indiv();
		context.add(infectiousIndiv);
		infectiousIndiv.infect("none");
		//call recover on them
		infectiousIndiv.recoverOrDevelopResistance("X");
		//assert that indiv.state == 2
		assertTrue("Recovery1", infectiousIndiv.getState() == 0);
		
		
		//make an already recovered indiv
		Indiv recoveredIndiv = new Indiv();
		context.add(recoveredIndiv);
		recoveredIndiv.infect("none");
		//call recover on them
		recoveredIndiv.recoverOrDevelopResistance("X");
		//assert that indiv.state == 2
		assertTrue("Recovery2", recoveredIndiv.getState() == 0);
		
		
		//make a sus indiv
		Indiv susceptibleIndiv = new Indiv();
		context.add(susceptibleIndiv);
		//call recover on them
		susceptibleIndiv.recoverOrDevelopResistance("X");
		//assert that indiv.state == 0
		assertTrue("Recovery3", susceptibleIndiv.getState() == 0);
		
		//Schedule schedule = new Schedule();
		//RunEnvironment.init(schedule, null, null, true);
		ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
		Indiv scheduledIndiv = new Indiv();
		context.add(scheduledIndiv);
		scheduledIndiv.infect("none");
		ScheduleParameters schparams = ScheduleParameters.createOneTime(0);
		schedule.schedule(schparams, scheduledIndiv, "actuallyRecover");
		assertTrue("Recovery4", scheduledIndiv.getState() == 1);
		while (schedule.getModelActionCount()>0) {
			schedule.execute();
		}
		assertTrue("Recovery5", scheduledIndiv.getState() == 0);
		
	}
	 @Test
	public void testGender() {
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true),1,0,0);
		context.add(observer);
				
		double bigTestPopSize = 100000.0;
		
		//make the new indivs
		for (int i = 0; i < bigTestPopSize; i++) {
			context.add(new Indiv());
		}	
		

		Stream<Object> indivs = context.getObjectsAsStream(Indiv.class); //grabs all objects of class Indiv
		Stream<Object> indivs2 = context.getObjectsAsStream(Indiv.class); //grabs all objects of class Indiv
		Stream<Object> indivs3 = context.getObjectsAsStream(Indiv.class); //grabs all objects of class Indiv

		List<Object> males = indivs
				.filter(indiv -> ((Indiv) indiv).getGender().equals("m"))
				.collect(Collectors.toList());
		
		int maleCount =  males.size();
		
		//System.out.println(maleCount);
		
		List<Object> females = indivs2
				.filter(indiv -> ((Indiv) indiv).getGender().equals("f"))
				.collect(Collectors.toList());
		
		int femaleCount =  females.size();
		
		//System.out.println(femaleCount);
		
		List<Object> nbs = indivs3
				.filter(indiv -> ((Indiv) indiv).getGender().equals("nb"))
				.collect(Collectors.toList());
		
		int nbCount =  nbs.size();
		
		//System.out.println(nbCount);
		
		assertTrue("gender 1", nbCount + maleCount + femaleCount == bigTestPopSize);
		
	}
	 
	 @Test
	 public void testGenderPrefAssign() {
			Context context = RunState.getInstance().getMasterContext();
			Observer observer = new Observer(new CustomFileOutput(true),1,0,0);
			context.add(observer);
					
			double bigTestPopSize = 10.0;
			
			//make the new indivs
			for (int i = 0; i < bigTestPopSize; i++) {
				Indiv indiv = new Indiv();
				context.add(indiv);
				//System.out.println(indiv.getGenderPref());
			}
	 }
	 
	 
	 @Test
	 public void testGenderSeeking() {
			RandomEngine eng = RandomHelper.getGenerator();
			Uniform partnerSelectUniform = new Uniform(0, 1, eng);
			RandomHelper.registerDistribution("partnerSelectUniform", partnerSelectUniform);
			
			//two individuals with incompatibly genders/preferences (i.e., prefs of 0 or 1) should never be partnered and should never lead to transmisison of infection
			Context context = RunState.getInstance().getMasterContext();
			Observer observer = new Observer(new CustomFileOutput(true),1,0,0);
			context.add(observer);
			
			Indiv indiv1 = new Indiv("m", 0.0);
			context.add(indiv1);
			
			assertTrue("genderSeekingTest1", indiv1.selectGenderSeeking().equals("m"));
			
			Indiv indiv2 = new Indiv("m", 1.0);
			context.add(indiv2);
			
			assertTrue("genderSeekingTest2", indiv2.selectGenderSeeking().equals("f"));

		 
		 
	 }
	
	
	@Test
	public void testPartnerSelectIncomp() {
		//System.out.println("Incompatible");
		/////NEW GENDER AND GENDER PREF PARTNER SELECT
		//test - searching for partners when n=2
		RandomEngine eng = RandomHelper.getGenerator();
		Uniform partnerSelectUniform = new Uniform(0, 1, eng);
		RandomHelper.registerDistribution("partnerSelectUniform", partnerSelectUniform);
		
		//two individuals with incompatibly genders/preferences (i.e., prefs of 0 or 1) should never be partnered and should never lead to transmisison of infection
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true),1,0,0);
		context.add(observer);
		
		Indiv indiv1 = new Indiv("m", 1.0);
		context.add(indiv1);
				
		Indiv indiv2 = new Indiv("m", 1.0);
		context.add(indiv2);
		
		assertTrue("PartnerSelectTest1", !indiv1.verifyCompatibility(indiv2));
		
		
		indiv1.infect("none");
		
		for (int i = 0; i < 10; i++) {
			indiv1.infectiousActions();
			//System.out.println(indiv2.getState());
		}
		
		assertTrue("imcompPartnerstest1", indiv2.getState()==0);
		
		
		//two individuals with POTENTIALLY compatible genders and preferences should SOMETIMES be partnered and SOMETIMES lead to transmission
	}
	
	@Test
	public void testVerifyCompatibility() {
		RandomEngine eng = RandomHelper.getGenerator();
		Uniform partnerSelectUniform = new Uniform(0, 1, eng);
		RandomHelper.registerDistribution("partnerSelectUniform", partnerSelectUniform);
		
		//two individuals with incompatibly genders/preferences (i.e., prefs of 0 or 1) should never be partnered and should never lead to transmisison of infection
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true),1,0,0);
		context.add(observer);
		
		Indiv indiv1 = new Indiv("m", 1.0);
		context.add(indiv1);
		
		//System.out.println(indiv1.getGenderPref());
				
		Indiv indiv2 = new Indiv("m", 1.0);
		context.add(indiv2);
		//System.out.println(indiv2.getGenderPref());

		
		assertTrue("VerifyCompTest1", !indiv1.verifyCompatibility(indiv2));
		
		
		Indiv indiv3 = new Indiv("m", 0.0);
		context.add(indiv3);
		
		//System.out.println(indiv1.getGenderPref());
				
		Indiv indiv4 = new Indiv("m", 0.0);
		context.add(indiv4);
		//System.out.println(indiv2.getGenderPref());

		
		assertTrue("VerifyCompTest1", indiv3.verifyCompatibility(indiv4));
		
		
		
	}
	
	@Test
	public void testPartnerSelectComp() {
		//System.out.println("Compatible");
		//two individuals with POTENTIALLY compatible genders and preferences should SOMETIMES be partnered and SOMETIMES lead to transmission
		RandomEngine eng = RandomHelper.getGenerator();
		Uniform partnerSelectUniform = new Uniform(0, 1, eng);
		RandomHelper.registerDistribution("partnerSelectUniform", partnerSelectUniform);
		
		//two individuals with incompatibly genders/preferences (i.e., prefs of 0 or 1) should never be partnered and should never lead to transmisison of infection
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true),1,0,0);
		context.add(observer);
		
		Indiv indiv1 = new Indiv("m", 0.0);
		context.add(indiv1);
		indiv1.infect("whatever");
				
		Indiv indiv2 = new Indiv("m", 0.0);
		context.add(indiv2);
		
		assertTrue("PartnerSelectTest1", indiv1.verifyCompatibility(indiv2));
		

		for (int i = 0; i < 10; i++) {
			indiv1.infectiousActions();
			//System.out.println(indiv2.getState());
			}
		
		assertTrue("compPartnerstest1", indiv2.getState()==1);
		
		
	}
	
	
	
	
	@Test
	public void testInfect() {
		//should change state of Indiv
		//should add events to schedule
		//Schedule schedule = new Schedule();
		//RunEnvironment.init(schedule, null, null, true);
		Context context = RunState.getInstance().getMasterContext();
		ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();

		context.add(new Observer(new CustomFileOutput(true), 1,0,0));

		assertTrue("Infect0", schedule.getActionCount() == 0);

		//Context context = new DefaultContext();
		//RunState.init().setMasterContext(context);

		//make a sus indiv to infect
		Indiv indiv = new Indiv();
		context.add(indiv);
		//context.add(indiv);
		ScheduleParameters schparams = ScheduleParameters.createOneTime(0);
		schedule.schedule(schparams, indiv, "infect", "none");

		//call infect on that indiv
		indiv.infect("none");
		//assert that indiv.state == 1
		assertTrue("Infect1", indiv.getState()==1);
		//assert that schedule now contains actions
		//System.out.println(schedule.getActionCount());
		assertTrue("Infect2", schedule.getActionCount() > 0);
		
	}


	
	@Test
	public void testInfectiousActions() {
		Context context = RunState.getInstance().getMasterContext();
		ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
		context.add(new Observer(new CustomFileOutput(true),1,0,0));
		
		RandomEngine eng = RandomHelper.getGenerator();
		Uniform partnerSelectUniform = new Uniform(0, 1, eng);
		RandomHelper.registerDistribution("partnerSelectUniform", partnerSelectUniform);
		
		
		Indiv susIndiv1 = new Indiv("m", 0.0);
		context.add(susIndiv1);
		
		//make an infectious indiv with a 100% chance of seeking contact
		//check that they successfully infect the other indiv
		Indiv infectious100Indiv = new Indiv("m", 0.0);
		context.add(infectious100Indiv);
		infectious100Indiv.infect("none");
		
		infectious100Indiv.infectiousActions();
		
		assertTrue("Contact1", susIndiv1.getState()==1);
		assertTrue("Contact2", schedule.getActionCount() > 0);
		
		context.remove(susIndiv1);
		context.remove(infectious100Indiv);
		
		//make an infectious indiv with a 0% chance of seeking contact
		//check that they do not infect the other indiv
		Indiv susIndiv2 = new Indiv();
		context.add(susIndiv2);
		Indiv infectious0Indiv = new Indiv();
		context.add(infectious0Indiv);
		
		infectious0Indiv.infectiousActions();
		
		assertTrue("Contact3", susIndiv2.getState()==0);
		
	}
	

	
	@Test
	public void testNewCase() {
		//add an observer and an indiv to context
		ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();

		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true),1,0,0);
		context.add(observer);
		
		Indiv indiv = new Indiv();
		context.add(indiv);
		indiv.createInfection("A", false, indiv.getRecoveryTime());
		indiv.changeStateTo(1);
		observer.recordNewCase(indiv);
		assertTrue("NewCase1", observer.getNewCases()==1);

		
		indiv.changeStateTo(0);
		observer.recordNewCase(indiv);
		
		//check newCases should now be 1
		assertTrue("NewCase2", observer.getNewCases()==1);
		
		Indiv indiv2 = new Indiv();
		context.add(indiv2);
		
		indiv2.infect("none");
		//schedule.execute();
		//System.out.print(observer.getNewCases());
		
		
	}
	
	
	
	@Test
	public void testStrainInheritance() {
		ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();

		RandomEngine eng = RandomHelper.getGenerator();
		Uniform partnerSelectUniform = new Uniform(0, 1, eng);
		RandomHelper.registerDistribution("partnerSelectUniform", partnerSelectUniform);
		
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true),1,0,0);
		context.add(observer);
		
		Indiv infectiousIndiv0 = new Indiv("m", 0.0);
		context.add(infectiousIndiv0);
		infectiousIndiv0.infect("whatever");
		
		
		Indiv susIndiv = new Indiv("m", 0.0);
		context.add(susIndiv);
		
		for (int i = 0; i < 3; i++) {
			infectiousIndiv0.infectiousActions();
		}
		
		
		assertTrue("StrainInherit", susIndiv.myInfection().getStrain().equals("whatever"));
		
		
		
		
		
		
	}
}
