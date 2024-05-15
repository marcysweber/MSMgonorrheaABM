package simpleSIR;

import static org.junit.Assert.*;

import java.util.Optional;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import repast.simphony.context.Context;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.environment.RunState;
import repast.simphony.engine.schedule.ISchedule;
import repast.simphony.util.collections.IndexedIterable;

public class TreatmentCounterfactualTests {

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

	
	public void setUpTreatmentTest(String resistance) {
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true), 1,0,0);
		context.add(observer);
		SurveillanceProgram surveillance = new SurveillanceProgram();
		context.add(surveillance);
		
		double bigTestPopSize = 100.0;
		
		//make the new indivs
		for (int i = 0; i < bigTestPopSize; i++) {
			Indiv newIndiv = new Indiv();
			context.add(newIndiv);
			newIndiv.infect(resistance);
			
			Treatment treatment = new Treatment(newIndiv, observer);
			treatment.treat();
		}	
	}

	@Test
	public void treatGISPSymptomaticTest() throws Exception {		
		TestDrive.setUp(Main.setParameters(1000, 1, "none", "GISP", 10, 4000, 4.5, 0.5, 1, 3, 0.5, 2, 50, 500, 25,95, 97,1,1.5, 2,3,4,5,6));
		//if 100 symptomatic infections with resistance to A are put in treatment,
		// 100 will be scheduled for retreatment
		
		setUpTreatmentTest("A");
		
		Context context = RunState.getInstance().getMasterContext();
		ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
		@SuppressWarnings("unchecked")
		Optional<Object> observerOpt = context.getObjectsAsStream(Observer.class).findAny();
		Object observerObj = observerOpt.get();
		Observer observer = (Observer) observerObj;
		
		assertTrue("TreatGISPSymptomaticTest1", schedule.getActionCount()==300);
		assertTrue("TreatGISPSymptomaticTest2", observer.calcPrev(100, context)==100);
		
	}
	

	@Test
	public void treatGISPAsymptomaticTest() throws Exception {		
		TestDrive.setUp(Main.setParameters(1000, 1, "none", "GISP",10, 4000, 4.5, 0.5, 0.0, 3, 0.5, 2, 50, 500, 25,95,97,1,1.5, 2,3,4,5,6));
		//if 100 asymptomatic infections with resistance to A are put in treatment,
		// 0 will be scheduled for retreatment
		
		setUpTreatmentTest("A");
		
		Context context = RunState.getInstance().getMasterContext();
		ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
		@SuppressWarnings("unchecked")
		Optional<Object> observerOpt = context.getObjectsAsStream(Observer.class).findAny();
		Object observerObj = observerOpt.get();
		Observer observer = (Observer) observerObj;
		

		assertTrue("TreatGISPAsymptomaticTest1", schedule.getActionCount()==200);
		assertTrue("TreatGISPAsymptomaticTest2", observer.calcPrev(100, context)==100);
		
	}
	
	
	@Test
	public void treatDrugSusTesting100Test() throws Exception {		
		TestDrive.setUp(Main.setParameters(1000, 1, "none", "drug_sus_testing_100", 10, 4000, 4.5, 0.5, 1, 3, 0.5, 2, 50, 500, 25,100,100,1,1.5, 2,3,4,5,6));

		//if 100 infectious are put in treatment (regardless of resistance), 100 should recover immediately, 0 remaining infectious
		setUpTreatmentTest("Both");
		
		Context context = RunState.getInstance().getMasterContext();
		ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
		@SuppressWarnings("unchecked")
		Optional<Object> observerOpt = context.getObjectsAsStream(Observer.class).findAny();
		Object observerObj = observerOpt.get();
		Observer observer = (Observer) observerObj;
		

		assertTrue("TreatDrugSus100Test1", schedule.getActionCount()==200);
		assertTrue("TreatDrugSus100Test2", observer.calcPrev(100, context)==0);
		
	}
	
	@Test
	public void treatDrugSusTesting100Testsenspec() throws Exception {		
		TestDrive.setUp(Main.setParameters(1000, 1, "none", "drug_sus_testing_100", 10, 4000, 4.5, 0.5, 1, 3, 0.5, 2, 50, 500, 25,90,90,1,1.5, 2,3,4,5,6));

		//if 100 infectious are put in treatment (regardless of resistance), 100 should recover immediately, 0 remaining infectious
		setUpTreatmentTest("Both");
		
		Context context = RunState.getInstance().getMasterContext();
		ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
		@SuppressWarnings("unchecked")
		Optional<Object> observerOpt = context.getObjectsAsStream(Observer.class).findAny();
		Object observerObj = observerOpt.get();
		Observer observer = (Observer) observerObj;
		

		assertTrue("TreatDrugSus100senspecTest1", schedule.getActionCount()>=200);
		assertTrue("TreatDrugSus100sensspecTest2", observer.calcPrev(100, context)==10);
		
	}
	
	
	
	
	
	
	

	@Test
	public void treatDrugSusTesting80Test() throws Exception {		
		TestDrive.setUp(Main.setParameters(1000, 1, "none", "drug_sus_testing_80", 10, 4000, 4.5, 0.5, 1, 3, 0.5, 2, 50, 500, 25,95,97,1,1.5, 2,3,4,5,6));

		//if 100 infections are put in treatment, approx 80 will recover immediately; 
		// if resistant to A, approx 80 will recover immediately, approx 20 will be scheduled for retreatment
		
		setUpTreatmentTest("Both");
		
		Context context = RunState.getInstance().getMasterContext();
		ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
		@SuppressWarnings("unchecked")
		Optional<Object> observerOpt = context.getObjectsAsStream(Observer.class).findAny();
		Object observerObj = observerOpt.get();
		Observer observer = (Observer) observerObj;
		
		//System.out.println(schedule.getActionCount());
		//System.out.println(observer.calcPrev(100, context));
		
		
		assertTrue("TreatDrugSus80Test1", schedule.getActionCount()>200);
		assertTrue("TreatDrugSus80Test2", observer.calcPrev(100, context)>0);	}

	@Test
	public void treatRandomTest() throws Exception {
		TestDrive.setUp(Main.setParameters(1000, 1, "none", "random", 10,4000, 4.5, 0.5, 1, 3, 0.5, 2, 50, 500, 25,95,97,1,1.5, 2,3,4,5,6));

		//if 100 infections resistant to A are put in treatment, approx 50 will recover immediately and 
		// 50 will be scheduled for retreatment.
		
		setUpTreatmentTest("A");
		
		Context context = RunState.getInstance().getMasterContext();
		ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
		@SuppressWarnings("unchecked")
		Optional<Object> observerOpt = context.getObjectsAsStream(Observer.class).findAny();
		Object observerObj = observerOpt.get();
		Observer observer = (Observer) observerObj;
		
		assertTrue("TreatRandom1", schedule.getActionCount()>225);
		assertTrue("TreatRandom2", observer.calcPrev(100, context)>10);

	}
	
	@Test
	public void treatTestOfCure100Test() throws Exception {
		TestDrive.setUp(Main.setParameters(1000, 1, "none", "test-of-cure_100", 10,4000, 4.5, 0.5, 0.0, 3, 0.5, 2, 50, 500, 25,95,97,1,1.5, 2,3,4,5,6));

		//if 100 asymptomatic infections with resistance to A are put in treatment,
		// 100 will be scheduled for retreatment
		
		setUpTreatmentTest("A");
		
		Context context = RunState.getInstance().getMasterContext();
		ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
		@SuppressWarnings("unchecked")
		Optional<Object> observerOpt = context.getObjectsAsStream(Observer.class).findAny();
		Object observerObj = observerOpt.get();
		Observer observer = (Observer) observerObj;	
		
		assertTrue("TreatTestOfCure1001", schedule.getActionCount()==300);
		assertTrue("TreatTestOfCure1002", observer.calcPrev(100, context)==100);
		
	}
	
	
	@Test
	public void treatTestOfCure80Test() throws Exception {
		TestDrive.setUp(Main.setParameters(1000, 1, "none", "test-of-cure_80", 10,4000, 4.5, 0.5, 0.0, 3, 0.5, 2, 50, 500, 25,95,97,1,1.5, 2,3,4,5,6));

		//if 100 asymptomatic infections with resistance to A are put in treatment,
		// approx 80 will be scheduled for retreatment
		
		setUpTreatmentTest("A");
		
		Context context = RunState.getInstance().getMasterContext();
		ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
		@SuppressWarnings("unchecked")
		Optional<Object> observerOpt = context.getObjectsAsStream(Observer.class).findAny();
		Object observerObj = observerOpt.get();
		Observer observer = (Observer) observerObj;	
		
		assertTrue("TreatTestOfCure801", schedule.getActionCount()>75);
		assertTrue("TreatTestOfCure802", observer.calcPrev(100, context)==100);
		
	
	}
	
	
}
