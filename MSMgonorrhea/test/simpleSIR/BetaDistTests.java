package simpleSIR;

import static org.junit.Assert.*;

import java.util.ArrayList;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import cern.jet.random.Beta;
import cern.jet.random.engine.RandomEngine;
import repast.simphony.random.RandomHelper;

public class BetaDistTests {

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
	public void test() {
		RandomHelper.setSeed(1);
		RandomEngine eng = RandomHelper.registerGenerator("myStream", 1);
		
		Beta betaDist = new Beta(1.0, 0.5, eng);
		
		System.out.println(betaDist.nextDouble());
		
	}
	
	@Test
	public void betaSweepTest() {
		CustomParameterSweep sweeper = new CustomParameterSweep();
		
		ArrayList<Double> testValues = (ArrayList<Double>) sweeper.getBetaSweepValues(1, 1, 0.5, 0.1);
		System.out.print(testValues);
		
	}

}
