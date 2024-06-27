/**
 *
 */
package msmOnlyModel;

/**
 * @author me597
 *this should be a class to run a single simulation run
 */
import java.io.File;
import java.io.IOException;
import java.util.Scanner;
import repast.simphony.random.RandomHelper;

public class Main {

	public static void main(String[] args) {
		int reps = 0;

		Scanner scanner = new Scanner(System.in);

		System.out.println("If sweeping, enter true. For calibrated, enter false.");

		// the following booleans control what type of runs these will be:
		boolean sweeping = scanner.nextBoolean();
		scanner.nextLine();
		boolean with_calibrated;
		String resistance;
		if (sweeping) { // sweeping overrides other scenario settings
			with_calibrated = false;
			resistance = "none";
			System.out.println("How big should the sweep be?");
			reps = scanner.nextInt();
		} else {
			with_calibrated = true;
			System.out.println("Include resistance? (String)");
			resistance = scanner.nextLine();
			scanner.nextLine();

			if (!resistance.equals("all")) {
				System.out.print("Include counterfactuals? (boolean)");
			} else {
			}
		}
		
		scanner.close();

		File scenariofile = new File("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/SimpleSIR.rs"); // the
																												// scenario
																												// dir

		RandomHelper.setSeed(1);
		
		if (sweeping) {
			
			
			BatchRun batchRunner = new BatchRun("sweep", resistance);
			
			int batches = 20;
			
			for (int i = 0; i < batches; i++) {
				batchRunner.executeSweep(scenariofile, reps, resistance);

				
				
			}
			
			
			try {
				batchRunner.combineBatchFiles();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
		}

		if (with_calibrated) {

			// to run everything:
			//executeCalibratedNoResistanceBatch(scenariofile);

			//executeCounterfactualScenarios(scenariofile);

			executeSensitivityAnalysisBatch(scenariofile);			
			
			
			//executeCompareResistanceInserters(scenariofile);
//			
//			
		}
	}


	public static void executeCounterfactualScenarios(File scenariofile) {
		BatchRun batchRunner = new BatchRun("all", "all");
		batchRunner.executeCalibratedBatch(scenariofile, "GISP_05");
		
		batchRunner.executeCalibratedBatch(scenariofile, "test-of-cure_80");
		
		batchRunner.executeCalibratedBatch(scenariofile, "random");
		
		batchRunner.executeCalibratedBatch(scenariofile, "drug_sus_testing_80");

		System.out.println("completed all counterfactual scenarios!");

	}

	public static void executeCalibratedwResistanceBatch(File scenario, String counterfactual) {
		BatchRun batchRunner = new BatchRun("all", "all");

		batchRunner.executeCalibratedBatch(scenario, counterfactual);
	}

	public static void executeCalibratedNoResistanceBatch(File scenario) {
		BatchRun batchRunner = new BatchRun("none", "none");

		batchRunner.executeCalibratedBatch(scenario, "none");

	}
	
	public static void executeSensitivityAnalysisBatch(File scenariofile) {
		//executeAvailDrugXBatch(scenariofile);
		
		//executeSwitchThresholdBatch(scenariofile);
		//executeAvailrDSTBatch(scenariofile);
		executeAdhereTOCsymptomaticBatch(scenariofile);
		executeAdhereTOCasymptomaticBatch(scenariofile);

	}
	
	public static void executeAvailDrugXBatch(File scenario) {
		BatchRun batchRunner = new BatchRun("all", "combo");

		int switchThres = 5;
		int availrDST = 80;
		int adhereTOCsympt = 80;
		int adhereTOCasympt = 80;

		batchRunner.executeCalibratedBatch(scenario, "GISP_05", "combo", 10, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt);
		batchRunner.executeCalibratedBatch(scenario, "GISP_05", "combo", 15, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt);
		batchRunner.executeCalibratedBatch(scenario, "GISP_05", "combo", 20, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt);
		batchRunner.executeCalibratedBatch(scenario, "GISP_05", "combo", 31, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt);
		
		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_80", "combo", 10, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt);
		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_80", "combo", 15, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt);
		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_80", "combo", 20, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt);
		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_80", "combo", 31, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt);
		
		batchRunner.executeCalibratedBatch(scenario, "random", "combo", 10, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt);
		batchRunner.executeCalibratedBatch(scenario, "random", "combo", 15, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt);
		batchRunner.executeCalibratedBatch(scenario, "random", "combo", 20, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt);
		batchRunner.executeCalibratedBatch(scenario, "random", "combo", 31, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt);
		
		batchRunner.executeCalibratedBatch(scenario, "drug_sus_testing_80", "combo", 10, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt);
		batchRunner.executeCalibratedBatch(scenario, "drug_sus_testing_80", "combo", 15, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt);
		batchRunner.executeCalibratedBatch(scenario, "drug_sus_testing_80", "combo", 20, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt);
		batchRunner.executeCalibratedBatch(scenario, "drug_sus_testing_80", "combo", 31, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt);
	
	}
	

	public static void executeSwitchThresholdBatch(File scenario) {
		BatchRun batchRunner = new BatchRun("switch", "combo");
		
		String resistInsert = "combo";
		int yearX = 25;
		//int switchThres = 5;
		int availrDST = 80;
		int adhereTOCsympt = 80;
		int adhereTOCasympt = 80;

		batchRunner.executeCalibratedBatch(scenario, "GISP_4", resistInsert, yearX, 4, availrDST, adhereTOCsympt, adhereTOCasympt);
		batchRunner.executeCalibratedBatch(scenario, "GISP_45", resistInsert, yearX, 4.5,  availrDST, adhereTOCsympt, adhereTOCasympt);
		batchRunner.executeCalibratedBatch(scenario, "GISP_5", resistInsert, yearX, 5,  availrDST, adhereTOCsympt, adhereTOCasympt);
		batchRunner.executeCalibratedBatch(scenario, "GISP_55", resistInsert, yearX, 5.5, availrDST,  adhereTOCsympt, adhereTOCasympt);
		batchRunner.executeCalibratedBatch(scenario, "GISP_6", resistInsert, yearX, 6, availrDST,  adhereTOCsympt, adhereTOCasympt);
	}
	
	public static void executeAvailrDSTBatch(File scenario) {
		BatchRun batchRunner = new BatchRun("availrDST", "combo");
		String resistInsert = "combo";
		int yearX = 25;
		int switchThres = 5;
		int adhereTOCsympt = 80;
		int adhereTOCasympt = 80;
				
		
		batchRunner.executeCalibratedBatch(scenario, "drug_sus_testing_50", resistInsert, yearX, switchThres, 50, adhereTOCsympt, adhereTOCasympt);
		batchRunner.executeCalibratedBatch(scenario, "drug_sus_testing_60", resistInsert, yearX, switchThres, 60, adhereTOCsympt, adhereTOCasympt);
		batchRunner.executeCalibratedBatch(scenario, "drug_sus_testing_70", resistInsert, yearX, switchThres, 70, adhereTOCsympt, adhereTOCasympt);
		batchRunner.executeCalibratedBatch(scenario, "drug_sus_testing_80", resistInsert, yearX, switchThres, 80, adhereTOCsympt, adhereTOCasympt);
		batchRunner.executeCalibratedBatch(scenario, "drug_sus_testing_90", resistInsert, yearX, switchThres, 90, adhereTOCsympt, adhereTOCasympt);
		batchRunner.executeCalibratedBatch(scenario, "drug_sus_testing_100", resistInsert, yearX, switchThres, 100, adhereTOCsympt, adhereTOCasympt);

	}
	
	public static void executeAdhereTOCsymptomaticBatch(File scenario) {
		BatchRun batchRunner = new BatchRun("adhereTOCsympt", "combo");
		String resistInsert = "combo";

		int yearX = 25;
		int switchThres = 5;
		int availrDST = 80;

		//int adhereTOCsympt = 80;
		int adhereTOCasympt = 80;
		
//		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_sympt_20", resistInsert, yearX, switchThres, availrDST, 20, adhereTOCasympt);
//		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_sympt_40", resistInsert, yearX, switchThres, availrDST, 40, adhereTOCasympt);
//		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_sympt_60", resistInsert, yearX, switchThres, availrDST, 60, adhereTOCasympt);
//		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_sympt_80", resistInsert, yearX, switchThres, availrDST, 80, adhereTOCasympt);
//		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_sympt_100", resistInsert, yearX, switchThres, availrDST, 100, adhereTOCasympt);
//		
		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_sympt_70", resistInsert, yearX, switchThres, availrDST, 70, adhereTOCasympt);
		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_sympt_90", resistInsert, yearX, switchThres, availrDST, 90, adhereTOCasympt);


	}
	
	public static void executeAdhereTOCasymptomaticBatch(File scenario) {
		BatchRun batchRunner = new BatchRun("adhereTOCasympt", "combo");
		String resistInsert = "combo";

		int yearX = 25;
		int switchThres = 5;
		int availrDST = 80;

		int adhereTOCsympt = 80;
		//int adhereTOCasympt = 80;
		
//		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_asympt_20", resistInsert, yearX, switchThres, availrDST, adhereTOCsympt, 20);
//		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_asympt_40", resistInsert, yearX, switchThres, availrDST, adhereTOCsympt, 40);
//		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_asympt_60", resistInsert, yearX, switchThres, availrDST, adhereTOCsympt, 60);
//		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_asympt_80", resistInsert, yearX, switchThres, availrDST, adhereTOCsympt, 80);
//		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_asympt_100", resistInsert, yearX, switchThres, availrDST, adhereTOCsympt, 100);

		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_asympt_70", resistInsert, yearX, switchThres, availrDST, adhereTOCsympt, 70);
		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_asympt_90", resistInsert, yearX, switchThres, availrDST, adhereTOCsympt, 90);

	}
	
	
	
	
	
	public static void executeCompareResistanceInserters(File scenariofile) {
		BatchRun batchRunner = new BatchRun("GISP", "all");

		int yearX = 25;
		int switchThres = 5;
		int availrDST = 80;
		int adhereTOCsympt = 80;
		int adhereTOCasympt = 80;
		
		batchRunner.executeCalibratedBatch(scenariofile, "GISP", "constantImport", yearX, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt);
		batchRunner.executeCalibratedBatch(scenariofile, "GISP", "dropInOnce",yearX, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt);
		batchRunner.executeCalibratedBatch(scenariofile, "GISP", "convertOnce",yearX, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt);
		batchRunner.executeCalibratedBatch(scenariofile, "GISP", "developWithTreatment",yearX, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt);
		batchRunner.executeCalibratedBatch(scenariofile, "GISP", "combo",yearX, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt);
	}
	




//	public static void setUpOne(MyRunner runner, ParamConfig paramConfig, int endTime) {
//
//		// Run the sim a few times to check for cleanup and init issues.
//
//		System.out.print("setting up...");
//
//		// make parameter object, manually add the parameter values
//		Parameters params = setParameters(paramConfig.batchNumber(), endTime, paramConfig.getSeed(), paramConfig.getResistance(),
//				paramConfig.getCounterfactual(), paramConfig.getYearX(), paramConfig.getInitialInfected(), paramConfig.getTransmissionM(), paramConfig.getTransmissionF(),
//				paramConfig.getRecoveryLambda(), paramConfig.getProbSymptomaticM(),paramConfig.getProbSymptomaticF(), paramConfig.getScreenIntervalMSM(),paramConfig.getScreenIntervalMSW(),paramConfig.getScreenIntervalW(),
//				paramConfig.getDelayToSeekCare(), paramConfig.getDelayToRetreatment(),
//				paramConfig.getPercentResistantA(), paramConfig.getBeginImportingB(),
//				paramConfig.getImportingBInterval(), paramConfig.getDSTsensitivity(), paramConfig.getDSTspecificity(),
//				paramConfig.getcareCost(), paramConfig.getTestCost(),
//				paramConfig.getstrainTestCost(), paramConfig.getTreatmentACost(), paramConfig.getTreatmentBCost(), paramConfig.getTreatmentXCost(), paramConfig.getTreatmentECost());
//
//		//RunEnvironment.getInstance().setParameters(params);
//		// System.out.println(params.getSchema().parameterNames());
//
//		runner.runInitialize(params, endTime); // initialize the run
//
//		// RunEnvironment.getInstance().endAt(endTime);
//
//		// System.out.print(" run # " +
//		// RunState.getInstance().getRunInfo().getRunNumber());
//
//	}
//
//	public static void runOne(MyRunner runner) {
//		System.out.print("running...");
//
//		while (runner.getEndTime() > runner.getCurrentTick() && !runner.isFinishing()) { // loop until last action is
//																							// left
//			if (runner.getModelActionCount() == 0) {
//				runner.setFinishing(true);
//			}
//			runner.step(); // execute all scheduled actions at next tick
//		}
//
//		runner.stop(); // execute any actions scheduled at run end
//		runner.cleanUpRun();
//		System.out.println("finished!");
//	}


}