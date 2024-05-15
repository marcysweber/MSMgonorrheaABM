package simpleSIR;

import static org.junit.Assert.*;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class RunnerTests {

	@BeforeClass
	public static void setUpBeforeClass() throws Exception {
	}

	@AfterClass
	public static void tearDownAfterClass() throws Exception {
	}

	@Before
	public void setUp() throws Exception {
		TestDrive.setUp(Main.setParameters(1000, 1, "none", "none", 10,4000, 4.5, 0.5, 0.5, 3, 0.5, 2, 50, 500, 25,95,97,1,1.5, 2,3,4,5,6));
	}

	@After
	public void tearDown() throws Exception {
	}

	@Test
	public void RunnerTest() throws Exception {
		
		
	}

}
