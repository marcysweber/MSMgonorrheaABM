package msmOnlyModel.tests;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import msmOnlyModel.BatchRun;
import msmOnlyModel.SingleRun;
import repast.simphony.parameter.Parameters;

public class ScreenerTest {

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

	public SingleRun setUpScreenerTest() {
		BatchRun testBatch = new BatchRun("GISP", "combo", 10);
		Parameters params = testBatch.setParameters(0,52, 1, "combo", "GISP", 10, 100, 1.0, 1.0, 0.5, 1.0, 1.0, 1.0, 0.05, 53, 1.0, 0.95, 0.95, 1, 2, 3, 4, 5, 6, 7);
		SingleRun testRun = new SingleRun("/Users/me597/Documents/MSMoutput/tests", params);
		testRun.setUp(52);
		
		return testRun;
	}
	
	@Test
	public void test() {
		fail("Not yet implemented");
	}

}
