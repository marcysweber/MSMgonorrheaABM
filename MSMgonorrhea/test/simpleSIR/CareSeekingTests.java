package simpleSIR;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import repast.simphony.context.Context;
import repast.simphony.engine.environment.RunState;
import repast.simphony.util.collections.IndexedIterable;

public class CareSeekingTests {

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
	}

	@Before
	public void setUp() throws Exception {
		TestDrive.setUp(Main.setParameters(1000, 
				1, 
				"none", 
				"none", 
				10,
				4000, 
				4.5, 
				0.5, 
				1, 
				3, 
				0.5, 
				2, 
				50, 
				500, 
				25,
				95,
				97,
				1,
				1.5, 
				2,
				3,
				4,
				5, 6));

	}

	@After
	public void tearDown() throws Exception {
	}

	
	@Test
	public void testSeekCareOne() {
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true), 1,0,0);
		context.add(observer);

		SurveillanceProgram surveillance = new SurveillanceProgram();
		context.add(surveillance);
		
		Indiv indiv1 = new Indiv();
		context.add(indiv1);
		
		indiv1.infect("none");
		while (!indiv1.symptoms()) { //keep trying until you get a symptomatic infection
			indiv1.infect("none");
		}
		//System.out.print(indiv1.symptoms());
		//THESE ARE NOT WORKING BECAUSE STARTING INFECTIONS ARE 90% ASYMPTOMATIC!
		
		double prev = observer.calcPrev(1, context);
		//System.out.print(prev);
		assertTrue("SeekCareOne1", prev==100.0);
		
		CareSeeking care = new CareSeeking(indiv1, observer);
		care.seekCare();
		
		double prev2 = observer.calcPrev(1, context);
		//System.out.print(prev2);
		assertTrue("SeekCareOne2", prev2==0.0);
	}
	
	
	
	
	
	@Test
	public void testSeekCare() {
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true), 1,0,0);
		context.add(observer);

		SurveillanceProgram surveillance = new SurveillanceProgram();
		context.add(surveillance);
		
		double bigTestPopSize = 100.0;
		
		//make the new indivs
		for (int i = 0; i < bigTestPopSize; i++) {
			context.add(new Indiv());
		}	
		
		double infections = bigTestPopSize/2;
		
		IndexedIterable<Object> indivs = context.getObjects(Indiv.class);
		
		for (int index = 0; index < infections; index++) {
			Indiv indiv = (Indiv) indivs.get(index);
			indiv.infect("none");
			
			while (!indiv.symptoms()) { //keep going until the infection is symptomatic
				indiv.infect("none");
			}
		}
		
		double prev = observer.calcPrev(bigTestPopSize, context);
		assertTrue("Care1", prev == 50.0);
		
		//CareSeeking care = new CareSeeking();
		IndexedIterable<Object> indivList = context.getObjects(Indiv.class);
		for (int index2 = 0; index2 < bigTestPopSize; index2++) {
			Indiv indiv = (Indiv) indivList.get(index2);
			CareSeeking care = new CareSeeking(indiv, observer);
			care.seekCare();
		}
				
		double soughtCare = observer.getSoughtCare();
		//System.out.print(soughtCare);
		
		double treatments = observer.getTreatments();
		//System.out.print(treatments);
		assertTrue("Care3", treatments == 50);
		
		double prev2 = observer.calcPrev(bigTestPopSize, context);
		//System.out.print(prev2);
		assertTrue("Care2", prev2 == 0.0);
		

	
	}

}
