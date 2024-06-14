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
			
			
			BatchRun batchRunner = new BatchRun("sweep", resistance, 0);
			
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
			executeCalibratedNoResistanceBatch(scenariofile);

			//executeCounterfactualScenarios(scenariofile);

			executeDrugDevBatch(scenariofile);

			
			
			
			//executeCompareResistanceInserters(scenariofile);
//			
//			if (resistance.equals("all")) {
//				executeCompareResistanceInserters(scenariofile);
//			} else if (!resistance.equals("none")) {
//				if (with_counterfactuals) {
//					executeCounterfactualScenarios(scenariofile);
//				} else {
//					//running a calibrated run with resistance dropped in
//					executeCalibratedwResistanceBatch(scenariofile, "none");
//				}
//			} else {
//				//rerunning the calibrated trajectories for full 25 years
//				executeCalibratedNoResistanceBatch(scenariofile);
//			}
		}
	}


	public static void executeCounterfactualScenarios(File scenariofile) {
		BatchRun batchRunner = new BatchRun("all", "all", 25);
		batchRunner.executeCalibratedBatch(scenariofile, "GISP_05", "combo", 25);
		
		batchRunner.executeCalibratedBatch(scenariofile, "test-of-cure_80", "combo", 25);
		
		batchRunner.executeCalibratedBatch(scenariofile, "random", "combo", 25);
		
		batchRunner.executeCalibratedBatch(scenariofile, "drug_sus_testing_80", "combo", 25);

		System.out.println("completed all counterfactual scenarios!");

	}

	public static void executeCalibratedwResistanceBatch(File scenario, String counterfactual) {
		BatchRun batchRunner = new BatchRun("all", "all", 10);

		batchRunner.executeCalibratedBatch(scenario, counterfactual, "combo", 10);
	}

	public static void executeCalibratedNoResistanceBatch(File scenario) {
		BatchRun batchRunner = new BatchRun("none", "none", 10);

		batchRunner.executeCalibratedBatch(scenario, "none", "none", 10);

	}
	
	
	public static void executeDrugDevBatch(File scenario) {
		BatchRun batchRunner = new BatchRun("all", "combo", 10);

		//doing yearX = 25 first so that i can visualize those results while the others are still running
		batchRunner.executeCalibratedBatch(scenario, "GISP_05", "combo", 25);
		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_80", "combo", 25);
		batchRunner.executeCalibratedBatch(scenario, "random", "combo", 25);
		batchRunner.executeCalibratedBatch(scenario, "drug_sus_testing_80", "combo", 25);

		batchRunner.executeCalibratedBatch(scenario, "GISP_05", "combo", 10);
		batchRunner.executeCalibratedBatch(scenario, "GISP_05", "combo", 15);
		batchRunner.executeCalibratedBatch(scenario, "GISP_05", "combo", 20);
		batchRunner.executeCalibratedBatch(scenario, "GISP_05", "combo", 31);
		
		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_80", "combo", 10);
		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_80", "combo", 15);
		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_80", "combo", 20);
		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_80", "combo", 31);
		
		batchRunner.executeCalibratedBatch(scenario, "random", "combo", 10);
		batchRunner.executeCalibratedBatch(scenario, "random", "combo", 15);
		batchRunner.executeCalibratedBatch(scenario, "random", "combo", 20);
		batchRunner.executeCalibratedBatch(scenario, "random", "combo", 31);
		
		batchRunner.executeCalibratedBatch(scenario, "drug_sus_testing_80", "combo", 10);
		batchRunner.executeCalibratedBatch(scenario, "drug_sus_testing_80", "combo", 15);
		batchRunner.executeCalibratedBatch(scenario, "drug_sus_testing_80", "combo", 20);
		batchRunner.executeCalibratedBatch(scenario, "drug_sus_testing_80", "combo", 31);
		
		
		
		
	}
	

	public static void executeCompareResistanceInserters(File scenariofile) {
		BatchRun batchRunner = new BatchRun("GISP", "all", 10);

		
		batchRunner.executeCalibratedBatch(scenariofile, "GISP", "constantImport", 10);
		batchRunner.executeCalibratedBatch(scenariofile, "GISP", "dropInOnce",10);
		batchRunner.executeCalibratedBatch(scenariofile, "GISP", "convertOnce",10);
		batchRunner.executeCalibratedBatch(scenariofile, "GISP", "developWithTreatment",10);
		batchRunner.executeCalibratedBatch(scenariofile, "GISP", "combo",10);
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