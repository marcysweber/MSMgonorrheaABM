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
		boolean parcalibrated = true;

		Scanner scanner = new Scanner(System.in);

		System.out.println("If sweeping, enter true. For parcalibrated or calibrated, enter false.");

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
			resistance = "combo";
			System.out.println("For parcalibrated, enter true. For final calibrated, enter false.");
			parcalibrated = scanner.nextBoolean();
		}
		
		scanner.close();

		File scenariofile = new File("/Users/me597/Documents/GitHub/gonorrheaABM/SimpleSIRgit/SimpleSIR.rs"); // the
																												// scenario
																												// dir

		RandomHelper.setSeed(1);
		
		if (sweeping) {
			BatchRun batchRunner = new BatchRun("sweep", resistance);
			int batches = 30;
			
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
			if (parcalibrated) {
				BatchRun batchRunner = new BatchRun("GISPparcal", resistance);
				batchRunner.executeCalibratedBatch(scenariofile, "GISPrand_05");

				
			} else {
				// to run everything:
				executeCalibratedNoResistanceBatch(scenariofile);
				
				//executeCounterfactualScenarios(scenariofile);

				//executeSensitivityAnalysisBatch(scenariofile);			
				
				//executeSensitivityAnalysisBatchnoDST(scenariofile);			

				
				//executeCompareResistanceInserters(scenariofile);
//				
				executeFitnessCostSA(scenariofile);
			}
			
			

			
		}
	}


	public static void executeCounterfactualScenarios(File scenariofile) {
		BatchRun batchRunner = new BatchRun("all", "all");
		//batchRunner.executeCalibratedBatch(scenariofile, "GISPemp_05");
		batchRunner.executeCalibratedBatch(scenariofile, "GISPrand_05");
		batchRunner.executeCalibratedBatch(scenariofile, "random");
		batchRunner.executeCalibratedBatch(scenariofile, "test-of-cure_80");
		batchRunner.executeCalibratedBatch(scenariofile, "realistic_combo_50_50_00");
		
		batchRunner.executeCalibratedBatch(scenariofile, "drug_sus_testing_80");

		System.out.println("completed all counterfactual scenarios!");

	}

	public static void executeCalibratedwResistanceBatch(File scenario, String counterfactual) {
		BatchRun batchRunner = new BatchRun("all", "all");

		batchRunner.executeCalibratedBatch(scenario, counterfactual);
	}

	public static void executeCalibratedNoResistanceBatch(File scenario) {
		BatchRun batchRunner = new BatchRun("none", "none");

		batchRunner.executeCalibratedBatch(scenario, "none", "none", 25, 5, 80, 80, 50, 0.5, 0.5, 0.0, 0.0, 0.0);
	}
	
	public static void executeSensitivityAnalysisBatch(File scenariofile) {
		
		//executeAvailDrugXBatchRC(scenariofile);	
		
		executeAvailDrugXBatch(scenariofile);
    	executeSwitchThresholdBatch(scenariofile);
		executeAvailrDSTBatch(scenariofile);
	    executeAdhereTOCsymptomaticBatch(scenariofile);
	    executeAdhereTOCasymptomaticBatch(scenariofile);
		executeRealisticComboBatch(scenariofile);
		
	}
	
	public static void executeSensitivityAnalysisBatchnoDST(File scenario) {
		BatchRun batchRunner = new BatchRun("all", "combo");

		int switchThres = 5;
		int availrDST = 80;
		int adhereTOCsympt = 80;
		int adhereTOCasympt = 50;
		double realisticRandom = 0.5;
		double realisticTOC = 0.5;
		double realisticDST = 0;
		
		batchRunner.executeCalibratedBatch(scenario, "realistic_combo", "combo", 15, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
		batchRunner.executeCalibratedBatch(scenario, "realistic_combo", "combo", 20, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
		batchRunner.executeCalibratedBatch(scenario, "realistic_combo", "combo", 31, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);

		
		
	}
	
	public static void executeAvailDrugXBatch(File scenario) {
		BatchRun batchRunner = new BatchRun("all", "combo");

		int switchThres = 5;
		int availrDST = 80;
		int adhereTOCsympt = 80;
		int adhereTOCasympt = 50;
		double realisticRandom = 0.5;
		double realisticTOC = 0.5;
		double realisticDST = 0.0;

		
		  
//		  batchRunner.executeCalibratedBatch(scenario, "GISPrand_05", "combo", 15,
//		  switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom,
//		  realisticTOC, realisticDST, 0.0, 0.0); 
//		  
//		  batchRunner.executeCalibratedBatch(scenario,
//		  "GISPrand_05", "combo", 20, switchThres, availrDST, adhereTOCsympt,
//		  adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
//		 
//		  batchRunner.executeCalibratedBatch(scenario, "GISPrand_05", "combo", 31,
//		  switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom,
//		  realisticTOC, realisticDST, 0.0, 0.0);
		  
		  batchRunner.executeCalibratedBatch(scenario, "GISPrand_05", "combo", 15,
				  switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom,
				  realisticTOC, realisticDST, 0.0, 0.0); 
				  
				  batchRunner.executeCalibratedBatch(scenario,
				  "GISPrand_05", "combo", 20, switchThres, availrDST, adhereTOCsympt,
				  adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
				 
				  batchRunner.executeCalibratedBatch(scenario, "GISPrand_05", "combo", 31,
				  switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom,
				  realisticTOC, realisticDST, 0.0, 0.0);
				  
		  
		  
		 
		  batchRunner.executeCalibratedBatch(scenario, "test-of-cure_80", "combo", 15,
		  switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom,
		  realisticTOC, realisticDST, 0.0, 0.0); 
		  
		  batchRunner.executeCalibratedBatch(scenario,
		  "test-of-cure_80", "combo", 20, switchThres, availrDST, adhereTOCsympt,
		  adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
		 
		  batchRunner.executeCalibratedBatch(scenario, "test-of-cure_80", "combo", 31,
		  switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom,
		  realisticTOC, realisticDST, 0.0, 0.0);
		  
		  
		  
		  
		  batchRunner.executeCalibratedBatch(scenario, "random", "combo", 15,
		  switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom,
		  realisticTOC, realisticDST, 0.0, 0.0); 
		  
		  batchRunner.executeCalibratedBatch(scenario,
		  "random", "combo", 20, switchThres, availrDST, adhereTOCsympt,
		  adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
		  
		  batchRunner.executeCalibratedBatch(scenario, "random", "combo", 31,
		  switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom,
		  realisticTOC, realisticDST, 0.0, 0.0);
		  
		  
		  
		 
		  batchRunner.executeCalibratedBatch(scenario, "drug_sus_testing_80", "combo",
		  15, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom,
		  realisticTOC, realisticDST, 0.0, 0.0); 
		  
		  batchRunner.executeCalibratedBatch(scenario,
		  "drug_sus_testing_80", "combo", 20, switchThres, availrDST, adhereTOCsympt,
		  adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
		  
		  
		  batchRunner.executeCalibratedBatch(scenario, "drug_sus_testing_80", "combo",
		  31, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom,
		  realisticTOC, realisticDST, 0.0, 0.0);
		 
		  
		  
		batchRunner.executeCalibratedBatch(scenario, "realistic_combo", "combo", 15, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
		batchRunner.executeCalibratedBatch(scenario, "realistic_combo", "combo", 20, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
		batchRunner.executeCalibratedBatch(scenario, "realistic_combo", "combo", 31, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);

	}
	
	public static void executeAvailDrugXBatchRC(File scenario) {
		BatchRun batchRunner = new BatchRun("all", "combo");
		String resistInsert = "combo";

		int yearX = 25;
		int switchThres = 5;
		int availrDST = 80;
		int adhereTOCsympt = 80;
		int adhereTOCasympt = 80;
		double realisticRandom = 0.33;
		double realisticTOC = 0.33;
		double realisticDST = 0.34;

		  
		batchRunner.executeCalibratedBatch(scenario, "realistic_combo", "combo", 15, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
		batchRunner.executeCalibratedBatch(scenario, "realistic_combo", "combo", 20, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
		batchRunner.executeCalibratedBatch(scenario, "realistic_combo", "combo", 31, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);

		batchRunner.executeCalibratedBatch(scenario, "realistic_combo_60_40_00", resistInsert, yearX, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, 0.6, 0.4, 0.0, 0.0, 0.0);
		batchRunner.executeCalibratedBatch(scenario, "realistic_combo_40_60_00", resistInsert, yearX, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, 0.4, 0.6, 0.0, 0.0, 0.0);

		
	}
	

	public static void executeSwitchThresholdBatch(File scenario) {
		BatchRun batchRunner = new BatchRun("switch", "combo");
		
		String resistInsert = "combo";
		int yearX = 25;
		//int switchThres = 5;
		int availrDST = 80;
		int adhereTOCsympt = 80;
		int adhereTOCasympt = 50;
		
		double realisticRandom = 0.5;
		double realisticTOC = 0.5;
		double realisticDST = 0.0;

		//batchRunner.executeCalibratedBatch(scenario, "GISPemp_3", resistInsert, yearX, 3, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST);
		//batchRunner.executeCalibratedBatch(scenario, "GISPemp_7", resistInsert, yearX, 7, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST);
		
	
		 // batchRunner.executeCalibratedBatch(scenario, "GISPemp_4", resistInsert, yearX,
		 // 4, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST); //
		
		 // batchRunner.executeCalibratedBatch(scenario, "GISPemp_6", resistInsert, yearX,
		 // 6, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST);
		 
		  
		  batchRunner.executeCalibratedBatch(scenario, "GISPrand_3", resistInsert, yearX, 3, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
			batchRunner.executeCalibratedBatch(scenario, "GISPrand_7", resistInsert, yearX, 7, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
			
		
			  batchRunner.executeCalibratedBatch(scenario, "GISPrand_4", resistInsert, yearX,
			  4, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0); //
			
			  batchRunner.executeCalibratedBatch(scenario, "GISPrand_6", resistInsert, yearX,
			  6, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
			 
	}
	
	public static void executeAvailrDSTBatch(File scenario) {
		BatchRun batchRunner = new BatchRun("availrDST", "combo");
		String resistInsert = "combo";
		int yearX = 25;
		int switchThres = 5;
		int adhereTOCsympt = 80;
		int adhereTOCasympt = 80;
		
		double realisticRandom = 0.5;
		double realisticTOC = 0.5;
		double realisticDST = 0.0;
				
		
		batchRunner.executeCalibratedBatch(scenario, "drug_sus_testing_50", resistInsert, yearX, switchThres, 50, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
		batchRunner.executeCalibratedBatch(scenario, "drug_sus_testing_60", resistInsert, yearX, switchThres, 60, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
		batchRunner.executeCalibratedBatch(scenario, "drug_sus_testing_70", resistInsert, yearX, switchThres, 70, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
		//batchRunner.executeCalibratedBatch(scenario, "drug_sus_testing_80", resistInsert, yearX, switchThres, 80, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST);
		batchRunner.executeCalibratedBatch(scenario, "drug_sus_testing_90", resistInsert, yearX, switchThres, 90, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
		batchRunner.executeCalibratedBatch(scenario, "drug_sus_testing_100", resistInsert, yearX, switchThres, 100, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);

	}
	
	public static void executeAdhereTOCsymptomaticBatch(File scenario) {
		BatchRun batchRunner = new BatchRun("adhereTOCsympt", "combo");
		String resistInsert = "combo";

		int yearX = 25;
		int switchThres = 5;
		int availrDST = 80;

		//int adhereTOCsympt = 80;
		int adhereTOCasympt = 50;
		
		double realisticRandom = 0.5;
		double realisticTOC = 0.5;
		double realisticDST = 0.0;
		
		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_sympt_20", resistInsert, yearX, switchThres, availrDST, 20, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
		
		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_sympt_30", resistInsert, yearX, switchThres, availrDST, 30, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
		
		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_sympt_40", resistInsert, yearX, switchThres, availrDST, 40, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
		
		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_sympt_50", resistInsert, yearX, switchThres, availrDST, 50, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
		
		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_sympt_60", resistInsert, yearX, switchThres, availrDST, 60, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
		
		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_sympt_70", resistInsert, yearX, switchThres, availrDST, 70, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);

		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_sympt_80", resistInsert, yearX, switchThres, availrDST, 80, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
		
		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_sympt_90", resistInsert, yearX, switchThres, availrDST, 90, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);

		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_sympt_100", resistInsert, yearX, switchThres, availrDST, 100, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
		


	}
	
	public static void executeAdhereTOCasymptomaticBatch(File scenario) {
		BatchRun batchRunner = new BatchRun("adhereTOCasympt", "combo");
		String resistInsert = "combo";

		int yearX = 25;
		int switchThres = 5;
		int availrDST = 80;

		int adhereTOCsympt = 80;
		//int adhereTOCasympt = 80;
		
		double realisticRandom = 0.5;
		double realisticTOC = 0.5;
		double realisticDST = 0.0;
		
		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_asympt_20", resistInsert, yearX, switchThres, availrDST, adhereTOCsympt, 20, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
		
		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_asympt_30", resistInsert, yearX, switchThres, availrDST, adhereTOCsympt, 30, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);

		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_asympt_40", resistInsert, yearX, switchThres, availrDST, adhereTOCsympt, 40, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
		
		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_asympt_50", resistInsert, yearX, switchThres, availrDST, adhereTOCsympt, 50, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);

		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_asympt_60", resistInsert, yearX, switchThres, availrDST, adhereTOCsympt, 60, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
		
		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_asympt_70", resistInsert, yearX, switchThres, availrDST, adhereTOCsympt, 70, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);

		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_asympt_80", resistInsert, yearX, switchThres, availrDST, adhereTOCsympt, 80, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
		
		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_asympt_90", resistInsert, yearX, switchThres, availrDST, adhereTOCsympt, 90, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);

		batchRunner.executeCalibratedBatch(scenario, "test-of-cure_asympt_100", resistInsert, yearX, switchThres, availrDST, adhereTOCsympt, 100, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);


	}
	
	
	public static void executeRealisticComboBatch(File scenario) {
		BatchRun batchRunner = new BatchRun("realistic_combo", "combo");
		String resistInsert = "combo";

		int yearX = 25;
		int switchThres = 5;
		int availrDST = 80;
		int adhereTOCsympt = 80;
		int adhereTOCasympt = 50;

		batchRunner.executeCalibratedBatch(scenario, "realistic_combo_50_50_00", resistInsert, yearX, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, 0.5, 0.5, 0.0, 0.0, 0.0);
		batchRunner.executeCalibratedBatch(scenario, "realistic_combo_75_25_00", resistInsert, yearX, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, 0.75, 0.25, 0.0, 0.0, 0.0);
		batchRunner.executeCalibratedBatch(scenario, "realistic_combo_25_75_00", resistInsert, yearX, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, 0.25, 0.75, 0.0, 0.0, 0.0);

		batchRunner.executeCalibratedBatch(scenario, "realistic_combo_45_45_10", resistInsert, yearX, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, 0.45, 0.45, 0.10, 0.0, 0.0);
		batchRunner.executeCalibratedBatch(scenario, "realistic_combo_65_25_10", resistInsert, yearX, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, 0.65, 0.25, 0.10, 0.0, 0.0);
		batchRunner.executeCalibratedBatch(scenario, "realistic_combo_25_65_10", resistInsert, yearX, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, 0.25, 0.65, 0.10, 0.0, 0.0);

		batchRunner.executeCalibratedBatch(scenario, "realistic_combo_40_40_20", resistInsert, yearX, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, 0.4, 0.4, 0.2, 0.0, 0.0);
		batchRunner.executeCalibratedBatch(scenario, "realistic_combo_20_60_20", resistInsert, yearX, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, 0.2, 0.6, 0.2, 0.0, 0.0);
		batchRunner.executeCalibratedBatch(scenario, "realistic_combo_60_20_20", resistInsert, yearX, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, 0.6, 0.2, 0.2, 0.0, 0.0);
		
		batchRunner.executeCalibratedBatch(scenario, "realistic_combo_35_35_30", resistInsert, yearX, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, 0.35, 0.35, 0.3, 0.0, 0.0);
		batchRunner.executeCalibratedBatch(scenario, "realistic_combo_55_15_30", resistInsert, yearX, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, 0.55, 0.15, 0.3, 0.0, 0.0);
		batchRunner.executeCalibratedBatch(scenario, "realistic_combo_15_55_30", resistInsert, yearX, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, 0.15, 0.55, 0.3, 0.0, 0.0);

		
	}
	
	
	
	public static void executeCompareResistanceInserters(File scenariofile) {
		BatchRun batchRunner = new BatchRun("GISP", "all");

		int yearX = 25;
		int switchThres = 5;
		int availrDST = 80;
		int adhereTOCsympt = 80;
		int adhereTOCasympt = 50;
		
		double realisticRandom = 0.45;
		double realisticTOC = 0.45;
		double realisticDST = 0.10;
		
		batchRunner.executeCalibratedBatch(scenariofile, "GISP", "constantImport", yearX, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
		batchRunner.executeCalibratedBatch(scenariofile, "GISP", "dropInOnce",yearX, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
		batchRunner.executeCalibratedBatch(scenariofile, "GISP", "convertOnce",yearX, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
		batchRunner.executeCalibratedBatch(scenariofile, "GISP", "developWithTreatment",yearX, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
		batchRunner.executeCalibratedBatch(scenariofile, "GISP", "combo",yearX, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 0.0, 0.0);
	}
	

public static void executeFitnessCostSA(File scenariofile) {
	
//	//fitness costs of A and B are equal
//	//no fitness costs
//	fitnessCostCounterfactuals(scenariofile, 0.0, 0.0);
//	
//	//extremely high fitness costs
//	fitnessCostCounterfactuals(scenariofile, 0.5, 0.5);
//
//	//
//	fitnessCostCounterfactuals(scenariofile, 0.25, 0.25);
//	
//	//
	fitnessCostCounterfactuals(scenariofile, 0.10, 0.10);
//
//
//	//fitness costs of A are higher
//	fitnessCostCounterfactuals(scenariofile, 0.5, 0.25);
//
//	//
//	fitnessCostCounterfactuals(scenariofile, 0.25, 0.1);
//	
//	
	fitnessCostCounterfactuals(scenariofile, 0.10, 0.0);
//	
//	
//	//fitness costs of B are higher
//	fitnessCostCounterfactuals(scenariofile, 0.25, 0.5);
//
//	//
//	fitnessCostCounterfactuals(scenariofile, 0.1, 0.25);
//	
//	//
	fitnessCostCounterfactuals(scenariofile, 0.0, 0.10);
//	
	
	//varying between 0 and 10%
	
	fitnessCostCounterfactuals(scenariofile, 0.05, 0.05);
	fitnessCostCounterfactuals(scenariofile, 0.025, 0.025);
	
	fitnessCostCounterfactuals(scenariofile, 0.01, 0.01);

	
	
}

public static void fitnessCostCounterfactuals(File scenariofile, double costA, double costB) {
	BatchRun batchRunner = new BatchRun("all", "combo", costA, costB);

	int yearX = 25;
	int switchThres = 5;
	int availrDST = 80;
	int adhereTOCsympt = 80;
	int adhereTOCasympt = 50;
	
	double realisticRandom = 0.45;
	double realisticTOC = 0.45;
	double realisticDST = 0.10;
	
	batchRunner.executeCalibratedBatch(scenariofile, "GISPrand_05", "combo",yearX, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 
			costA, costB);
	
	batchRunner.executeCalibratedBatch(scenariofile, "random", "combo",yearX, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 
			costA, costB);
	
	batchRunner.executeCalibratedBatch(scenariofile, "test-of-cure_80", "combo", yearX, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 
			costA, costB);
	
	batchRunner.executeCalibratedBatch(scenariofile, "realistic_combo_50_50_00", "combo",yearX, switchThres, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, 
			costA, costB);
	
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