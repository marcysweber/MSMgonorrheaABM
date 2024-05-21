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
import repast.simphony.random.RandomHelper;
import repast.simphony.util.collections.IndexedIterable;

public class InsertResistanceTests {

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
	}

	@Before
	public void setUp() throws Exception {
		TestDrive.setUp(Main.setParameters(1000, 1, "none", "none", 10,4000, 4.5, 0.5, 0.5, 3, 0.5, 2, 50, 500, 25,95,97,1, 1.5,2, 3, 4,5,6));

	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void ConvertToResistantTest() {
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true), 1,0,0);
		context.add(observer);
		
		//pop of 200
		double testPopSize = 200.0;
		
		//make the new indivs
		for (int i = 0; i < testPopSize; i++) {
			context.add(new Indiv());
		}
		
		//infect 100
		double infections = 100.0;
		
		IndexedIterable<Object> indivs = context.getObjects(Indiv.class);
		
		for (int index = 0; index < infections; index++) {
			Indiv indiv = (Indiv) indivs.get(index);
			indiv.infect("none");
		}
		
		assertTrue("ConvertToResistantTest1", 0.0==observer.calcAllStrainPrev(testPopSize, context));
		
		//convert to resistant
		InsertResistance resistanceInserter = new InsertResistance("convertToResistant");
		resistanceInserter.convertToResistant(context);
		
		//50% prev, 25% resist A, 25% resist B
		
		assertTrue("ConvertToResistantTest2", 50 == observer.calcPrev(testPopSize, context));
		assertTrue("ConvertToResistantTest3", 25 == observer.calcResistAPrev(testPopSize, context));
		assertTrue("ConvertToResistantTest4", 25 == observer.calcResistBPrev(testPopSize, context));

		//fail("Not yet implemented");
	}
	
	@Test
	public void DropInResistantTest() {
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true), 1,0,0);
		context.add(observer);
				
		//pop of 100, no infections
		double testPopSize = 100.0;
		for (int i = 0; i < testPopSize; i++) {
			context.add(new Indiv());
		}
		assertTrue("DropInResistantTest1", 0.0==observer.calcPrev(testPopSize, context));
		assertTrue("DropInResistantTest2", 0.0==observer.calcAllStrainPrev(testPopSize, context));
		
		//drop in
		InsertResistance resistanceInserter = new InsertResistance("dropInResistant");
		resistanceInserter.dropInResistant(context);
		
		//prev now 100%, 50% resist A, 50% resist B
		assertTrue("DropInResistantTest3", 100.0==observer.calcPrev(testPopSize, context));

		assertTrue("DropInResistantTest3", 50 == observer.calcResistAPrev(testPopSize, context));
		assertTrue("DropInResistantTest4", 50 == observer.calcResistBPrev(testPopSize, context));

		//fail("Not yet implemented");
	}
	
	@Test
	public void ConstantImportResistantTest() {
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true), 1,0,0);
		context.add(observer);
		
		//pop of 20
		double testPopSize = 20.0;
		for (int i = 0; i < testPopSize; i++) {
			context.add(new Indiv());
		}
		
		//10 no-strain infections
		double infections = 10.0;
		
		IndexedIterable<Object> indivs = context.getObjects(Indiv.class);
		
		for (int index = 0; index < infections; index++) {
			Indiv indiv = (Indiv) indivs.get(index);
			indiv.infect("none");
		}
	
		assertTrue("ConstantImportResistantTest1", 50==observer.calcPrev(testPopSize, context));
		assertTrue("ConstantImportResistantTest2", 0.0==observer.calcAllStrainPrev(testPopSize, context));
		
		//single call to constantImportResistant
		InsertResistance resistanceInserter = new InsertResistance("constantImport");
		resistanceInserter.constantImportResistant(context);
		
		//prev now >50%, 25% resist A, 25% resist B

		assertTrue("ConstantImportResistantTest2", 50 < observer.calcPrev(testPopSize, context));
		assertTrue("ConstantImportResistantTest3", 25 == observer.calcResistAPrev(testPopSize, context));
		assertTrue("ConstantImportResistantTest4", 25 == observer.calcResistBPrev(testPopSize, context));

		//fail("Not yet implemented");
	}
	
	
	@Test
	public void DevelopWithTreatmentTest() {
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true), 1,0,0);
		context.add(observer);
		
		//pop of 100000
		double testPopSize = 100000.0;
		for (int i = 0; i < testPopSize; i++) {
			context.add(new Indiv());
		}
		
		//infect all with null strain
		double infections = testPopSize;
		
		IndexedIterable<Object> indivs = context.getObjects(Indiv.class);
		
		for (int index = 0; index < infections; index++) {
			Indiv indiv = (Indiv) indivs.get(index);
			indiv.infect("none");
		}
		
		//continue calling checkForDevelopResistance until the prev of resistance to A is not 0?
		InsertResistance resistanceInserter = new InsertResistance("developWithTreatment");
		while (observer.calcAllStrainPrev(testPopSize, context)==0) {
			int randomIndiv = RandomHelper.nextIntFromTo(0, (int) (testPopSize-1));
			Indiv indivToAttempt = (Indiv) indivs.get(randomIndiv);
			resistanceInserter.checkForDevelopResistance(indivToAttempt, "A");
		}
		
		assertTrue("developWithTreatmentTest1", observer.calcAllStrainPrev(testPopSize, context)!=0);
		assertTrue("developWithTreatmentTest2", observer.calcAllStrainInc(testPopSize, context)==1);

		//fail("Not yet implemented");
	}
	
	@Test
	public void ComboTest() {
		ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true), 1,0,0);
		context.add(observer);
				
		//pop of 100, no infections
		double testPopSize = 100.0;
		for (int i = 0; i < testPopSize; i++) {
			context.add(new Indiv());
		}
		assertTrue("ComboTest1", 0.0==observer.calcPrev(testPopSize, context));
		assertTrue("ComboTest2", 0.0==observer.calcAllStrainPrev(testPopSize, context));

		double infections = 50.0;
		
		IndexedIterable<Object> indivs = context.getObjects(Indiv.class);
		
		for (int index = 0; index < infections; index++) {
			Indiv indiv = (Indiv) indivs.get(index);
			indiv.infect("none");
		}
		assertTrue("ComboTest3", 50.0==observer.calcPrev(testPopSize, context));
		assertTrue("ComboTest4", 0.0==observer.calcAllStrainPrev(testPopSize, context));
		
		InsertResistance resistanceInserter = new InsertResistance("combo");
		
		//convertToResistantA should convert existing infections from null to A resistant
		resistanceInserter.convertToResistantA(context);
		assertTrue("ComboTest5", 50.0==observer.calcPrev(testPopSize, context));
		assertTrue("ComboTest6", 50.0==observer.calcAllStrainPrev(testPopSize, context));
		
		//makeImportingBSchedule(interval) should result in a list of doubles with a mean around "interval"
		
		
		//beginImportingB(contxt) should result in 1 case resistant to B added AND event in schedule
		//System.out.print(schedule.getActionCount());
		assertTrue("ComboTest6.5", 200==schedule.getActionCount());
		resistanceInserter.beginImportingB(context);
		assertTrue("ComboTest7", 50.0<=observer.calcPrev(testPopSize, context));
		assertTrue("ComboTest8", 1.0==observer.calcResistBPrev(testPopSize, context));
		assertTrue("ComboTest9", 200<schedule.getActionCount());

		
		//stochasticImportB(context) should result in 1 more case resistant to B
		
		
	}
	
}
