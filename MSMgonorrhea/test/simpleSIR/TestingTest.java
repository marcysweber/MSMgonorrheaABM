package simpleSIR;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import repast.simphony.context.Context;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.environment.RunState;
import repast.simphony.engine.schedule.ISchedule;

public class TestingTest {

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
	public void test() throws Exception {
		TestDrive.setUp(Main.setParameters(1000, 1, "none", "GISP", 10, 4000, 4.5, 0.5, 0.5, 3, 0.5, 2, 50, 500, 25,100, 100,1,1.5, 2,3,4, 5, 6));
		//100 sens and spec
		
		ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();

		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true),1,0,0);
		context.add(observer);
		
		Indiv indiv = new Indiv();
		context.add(indiv);
		indiv.infect("none");
		
		Testing testing = new Testing(indiv);
		String DSTresult = testing.drugSusceptibilityTest();
		
		assertTrue("Testingtest1.1", DSTresult.equals("AB"));
		
		
		indiv.infect("A");
		
		Testing testing2 = new Testing(indiv);
		String DSTresult2 = testing2.drugSusceptibilityTest();
		
		assertTrue("Testingtest1.2", DSTresult2.equals("B"));
		indiv.actuallyRecover();
		
		indiv.infect("B");
		
		Testing testing3 = new Testing(indiv);
		String DSTresult3 = testing3.drugSusceptibilityTest();
		assertTrue("Testingtest1.3", DSTresult3.equals("A"));
		
		indiv.actuallyRecover();
		indiv.infect("Both");
		
		Testing testing4 = new Testing(indiv);
		String DSTresult4 = testing4.drugSusceptibilityTest();
		
		assertTrue("Testingtest1.4", DSTresult4.equals("XE"));
		
	}
	
	
	@Test
	public void test2() throws Exception {
		TestDrive.setUp(Main.setParameters(1000, 1, "none", "GISP", 10, 4000, 4.5, 0.5, 0.5, 3, 0.5, 2, 50, 500, 25,100, 0,1,1.5, 2,3,4, 5, 6));
		//100sens, 0 spec
		ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();

		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true),1,0,0);
		context.add(observer);
		
		Indiv indiv = new Indiv();
		context.add(indiv);
		indiv.infect("none");
		
		Testing testing = new Testing(indiv);
		String DSTresult = testing.drugSusceptibilityTest();
		
		assertTrue("Testingtest2.1", DSTresult.equals("XE"));
		
		
		indiv.infect("A");
		
		Testing testing2 = new Testing(indiv);
		String DSTresult2 = testing2.drugSusceptibilityTest();
		
		assertTrue("Testingtest2.2", DSTresult2.equals("XE"));
		indiv.actuallyRecover();
		
		indiv.infect("B");
		
		Testing testing3 = new Testing(indiv);
		String DSTresult3 = testing3.drugSusceptibilityTest();
		assertTrue("Testingtest2.3", DSTresult3.equals("XE"));
		
		indiv.actuallyRecover();
		indiv.infect("Both");
		
		Testing testing4 = new Testing(indiv);
		String DSTresult4 = testing4.drugSusceptibilityTest();
		
		assertTrue("Testingtest2.4", DSTresult4.equals("XE"));
		
		
	}
	@Test
	public void test3() throws Exception {
		TestDrive.setUp(Main.setParameters(1000, 1, "none", "GISP", 10, 4000, 4.5, 0.5, 0.5, 3, 0.5, 2, 50, 500, 25,0, 100,1,1.5, 2,3,4, 5, 6));
		//0 sens, 100 spec
		ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();

		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true),1,0,0);
		context.add(observer);
		
		Indiv indiv = new Indiv();
		context.add(indiv);
		indiv.infect("none");
		
		Testing testing = new Testing(indiv);
		String DSTresult = testing.drugSusceptibilityTest();
		
		assertTrue("Testingtest3.1", DSTresult.equals("AB"));
		
		
		indiv.infect("A");
		
		Testing testing2 = new Testing(indiv);
		String DSTresult2 = testing2.drugSusceptibilityTest();
		
		assertTrue("Testingtest3.2", DSTresult2.equals("AB"));
		indiv.actuallyRecover();
		
		indiv.infect("B");
		
		Testing testing3 = new Testing(indiv);
		String DSTresult3 = testing3.drugSusceptibilityTest();
		assertTrue("Testingtest3.3", DSTresult3.equals("AB"));
		
		indiv.actuallyRecover();
		indiv.infect("Both");
		
		Testing testing4 = new Testing(indiv);
		String DSTresult4 = testing4.drugSusceptibilityTest();
		
		assertTrue("Testingtest3.4", DSTresult4.equals("AB"));
		
		
		
	}
	@Test
	public void test4() throws Exception {
		TestDrive.setUp(Main.setParameters(1000, 1, "none", "GISP", 10, 4000, 4.5, 0.5, 0.5, 3, 0.5, 2, 50, 500, 25,0, 0,1,1.5, 2,3,4, 5, 6));
		//0 sens and spec
		ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();

		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true),1,0,0);
		context.add(observer);
		
		Indiv indiv = new Indiv();
		context.add(indiv);
		indiv.infect("none");
		
		Testing testing = new Testing(indiv);
		String DSTresult = testing.drugSusceptibilityTest();
		
		assertTrue("Testingtest4.1", DSTresult.equals("XE"));
		
		
		indiv.infect("A");
		
		Testing testing2 = new Testing(indiv);
		String DSTresult2 = testing2.drugSusceptibilityTest();
		
		assertTrue("Testingtest4.2", DSTresult2.equals("A"));
		indiv.actuallyRecover();
		
		indiv.infect("B");
		
		Testing testing3 = new Testing(indiv);
		String DSTresult3 = testing3.drugSusceptibilityTest();
		assertTrue("Testingtest4.3", DSTresult3.equals("B"));
		
		indiv.actuallyRecover();
		indiv.infect("Both");
		
		Testing testing4 = new Testing(indiv);
		String DSTresult4 = testing4.drugSusceptibilityTest();
		
		assertTrue("Testingtest4.4", DSTresult4.equals("AB"));
		
		
		
	}
}
