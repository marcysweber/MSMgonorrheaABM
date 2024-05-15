/**
 * 
 */
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

/**
 * @author me597
 *
 */
public class CostTests {

	/**
	 * @throws java.lang.Exception
	 */
	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
	}

	/**
	 * @throws java.lang.Exception
	 */
	@AfterClass
	public static void tearDownAfterClass() throws Exception {
	}

	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
		TestDrive.setUp(Main.setParameters(1000, 1, "none", "none", 10, 4000, 100, 0.5, 0.5, 3, 0.5, 2, 50, 500, 25,95, 97,1,1.5, 2, 3, 4,5, 6));
	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void CareCostTest() {
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true), 1,0,0);
		context.add(observer);

		SurveillanceProgram surveillance = new SurveillanceProgram();
		context.add(surveillance);
		
		CostCalc costCalc = observer.getCostCalc();
		Screener screener = new Screener();

		Indiv indiv1 = new Indiv();
		context.add(indiv1);
		
		indiv1.infect("none");
		while (!indiv1.symptoms()) { //keep trying until you get a symptomatic infection
			indiv1.infect("none");
		}
	
		CareSeeking care = new CareSeeking(indiv1, observer);
		care.seekCare();
		
//		1 for careseeking + 1.5 test + 3 for drug A
		//System.out.println("care cost: " + costCalc.getMonetaryCost());
		assertTrue("CareCost1", costCalc.getMonetaryCost() == 5.5);
	
	}
	
	
	@Test
	public void MultiCareCostTest() {
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true), 1,0,0);
		context.add(observer);

		SurveillanceProgram surveillance = new SurveillanceProgram();
		context.add(surveillance);
		
		CostCalc costCalc = observer.getCostCalc();
		Screener screener = new Screener();
		
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
		
		IndexedIterable<Object> indivList = context.getObjects(Indiv.class);
		for (int index2 = 0; index2 < bigTestPopSize; index2++) {
			Indiv indiv = (Indiv) indivList.get(index2);
			CareSeeking care = new CareSeeking(indiv, observer);
			care.seekCare();
		}

		//125 for care and testing of non-infectious; 275 for actually infectious
		//System.out.println("multicare cost: " + costCalc.getMonetaryCost());
		assertTrue("MultiCareCostTest1", costCalc.getMonetaryCost()==400);
		
		
	}

	@Test
	public void StrainTestCostTest() {
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true), 1,0,0);
		context.add(observer);
		SurveillanceProgram surveillance = new SurveillanceProgram();
		context.add(surveillance);
		CostCalc costCalc = observer.getCostCalc();
		Screener screener = new Screener();

		Indiv indiv1 = new Indiv();
		context.add(indiv1);
		indiv1.infect("none");
		
		screener.screen(indiv1, observer);
		//screen costs = 0 
		// drug A treatment = 3
		
		surveillance.collectSamples(observer.getDetectedList());
		// strain test = 2 
		
		//System.out.println("DST cost: " + costCalc.getMonetaryCost());

		assertTrue("CareCost2",costCalc.getMonetaryCost()==5);
		
	}
	
	@Test
	public void ScreenCostTest() {
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true), 1,0,0);
		context.add(observer);
		SurveillanceProgram surveillance = new SurveillanceProgram();
		context.add(surveillance);
		CostCalc costCalc = observer.getCostCalc();
		Screener screener = new Screener();

		Indiv indiv1 = new Indiv();
		context.add(indiv1);
		indiv1.infect("none");
		
		screener.screen(indiv1, observer);
		//System.out.println("screen cost: " + costCalc.getMonetaryCost());

		assertTrue("CareCost2",costCalc.getMonetaryCost()==3);
	}
	
	@Test
	public void QALYLostSymptSusTest() {

		ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true), 1,0,0);
		context.add(observer);
		SurveillanceProgram surveillance = new SurveillanceProgram();
		context.add(surveillance);
		CostCalc costCalc = observer.getCostCalc();
		
		//symptomatic susceptible cases result in low QALY loss
		Indiv symptsusIndiv = new Indiv();
		context.add(symptsusIndiv);
		symptsusIndiv.infect("none");
		
		CareSeeking care = new CareSeeking(symptsusIndiv, observer);
		care.scheduleSeekCare();
		
		assertTrue("QALYLostSympSusTest1", symptsusIndiv.infectious());
		
		Treatment treatment = new Treatment(symptsusIndiv, observer);
		treatment.treat();
		
		assertTrue("QALYLostSympSusTest2", !symptsusIndiv.infectious());
		
		assertTrue("QALYLostSympSusTest3", costCalc.getQALYsLost()<0.0001);

	}
	
	@Test
	public void QALYLostSymptResistTest(){

	//resistant and symptomatic cases result in high QALY loss
		ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true), 1,0,0);
		context.add(observer);
		SurveillanceProgram surveillance = new SurveillanceProgram();
		context.add(surveillance);
		CostCalc costCalc = observer.getCostCalc();
		
		Indiv symptresistIndiv = new Indiv();
		context.add(symptresistIndiv);
		symptresistIndiv.infect("A");
		
		while (!symptresistIndiv.symptoms()) {
			symptresistIndiv.infect("A");
		}
			
		CareSeeking care = new CareSeeking(symptresistIndiv, observer);
		care.scheduleSeekCare();
		
		Treatment treatment = new Treatment(symptresistIndiv, observer);
		treatment.treat();
		
		assertTrue("QALYLostSymptResistTest1", symptresistIndiv.infectious());
		
		treatment.retreat("B");
		
		assertTrue("QALYLostSymptResistTest2", !symptresistIndiv.infectious());
		
		assertTrue("QALYLostSymptResistTest3", costCalc.getQALYsLost()>0.0001);

	}
			
	@Test
	public void QALYLostAsymptResistTest() {

		ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
		Context context = RunState.getInstance().getMasterContext();
		Observer observer = new Observer(new CustomFileOutput(true), 1,0,0);
		context.add(observer);
		SurveillanceProgram surveillance = new SurveillanceProgram();
		context.add(surveillance);
		CostCalc costCalc = observer.getCostCalc();
		
			//asymptomatic cases result in no (very very low) QALY loss
		Indiv asymptresistIndiv = new Indiv();
		context.add(asymptresistIndiv);
		asymptresistIndiv.infect("A");
			
		while (asymptresistIndiv.symptoms()) {
			asymptresistIndiv.infect("A");
		}
			
		Treatment treatment = new Treatment(asymptresistIndiv, observer);
		treatment.treat();
		
		assertTrue("QALYLostAsymptResistTest1", asymptresistIndiv.infectious());
		
		assertTrue("QALYLostAsymptResistTest2", costCalc.getQALYsLost()==0);

			
	}
}
