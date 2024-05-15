package simpleSIR;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import au.com.bytecode.opencsv.CSVWriter;

import java.time.LocalDate;
import java.time.Month;
import java.time.MonthDay;

import repast.simphony.parameter.DefaultParameters;
import repast.simphony.parameter.Parameters;

public class BatchRun {
	
	public BatchRun() {
		
	}
	

	public void executeSweep(File scenariofile, int reps, String resistance) {
		
		String batchDirPath = makeBatchDir("sweep", resistance, 0);


		CustomParameterSweep sweeper = new CustomParameterSweep();
		List<Double> seedValuesList = sweeper.getSeedValues(reps);

		List<Integer> initialInfectedValuesList = sweeper.getInitialInfectedValues(reps);
		
		List<Double> transmissionMSMValuesList = sweeper.getTransmissionMSMValues(reps);
		List<Double> transmissionMSWValuesList = sweeper.getTransmissionMSWValues(reps);
		List<Double> transmissionFValuesList = sweeper.getTransmissionFValues(reps);
		
		List<Double> recoveryLambdaValuesList = sweeper.getRecoveryLambdaValues(reps);
		
		List<Double> probSymptomaticMSMValuesList = sweeper.getProbSymptomaticMSMValues(reps);
		List<Double> probSymptomaticMSWValuesList = sweeper.getProbSymptomaticMSWValues(reps);
		List<Double> probSymptomaticFValuesList = sweeper.getProbSymptomaticFValues(reps);
		
		List<Double> screenIntervalMSMValuesList = sweeper.getScreenIntervalMSMValues(reps);
		List<Double> screenIntervalMSWValuesList = sweeper.getScreenIntervalMSWValues(reps);
		List<Double> screenIntervalWValuesList = sweeper.getScreenIntervalWValues(reps);	
		
		List<Double> delayToSeekCareMSMValuesList = sweeper.getDelayToSeekCareMSMValues(reps);
		List<Double> delayToSeekCareMSWValuesList = sweeper.getDelayToSeekCareMSWValues(reps);
		List<Double> delayToSeekCareFValuesList = sweeper.getDelayToSeekCareFValues(reps);

		List<Double> delayToRetreatmentMSMValuesList = sweeper.getDelayToRetreatmentMSMValues(reps);
		List<Double> delayToRetreatmentMSWValuesList = sweeper.getDelayToRetreatmentMSWValues(reps);
		List<Double> delayToRetreatmentFValuesList = sweeper.getDelayToRetreatmentFValues(reps);

		List<Double> percentResistantAValuesList = sweeper.getPercentResistantA(reps);
		List<Integer> beginImportingBValuesList = sweeper.getBeginImportingB(reps);
		List<Double> importingBIntervalValuesList = sweeper.getImportingBInterval(reps);
		List<Double> DSTsensitivityValuesList = sweeper.getDSTsensitivity(reps);
		List<Double> DSTspecificityValuesList = sweeper.getDSTspecificity(reps);

		List<Double> careCostValuesList = sweeper.getCareCostValues(reps);
		List<Double> testCostValuesList = sweeper.getTestCostValues(reps);
		List<Double> strainTestCostValuesList = sweeper.getStrainTestCostValues(reps);
		List<Double> treatmentACostValuesList = sweeper.getTreatmentACostValues(reps);
		List<Double> treatmentBCostValuesList = sweeper.getTreatmentBCostValues(reps);
		List<Double> treatmentXCostValuesList = sweeper.getTreatmentXCostValues(reps);
		List<Double> treatmentECostValuesList = sweeper.getTreatmentECostValues(reps);
		
		final Integer confirmed_reps = Integer.valueOf(reps);

		// puts together sets of parameters
		List<ParamConfig> lst = new ArrayList<ParamConfig>();
		Stream<ParamConfig> comboStream = lst.stream();
		for (int i = 0; i < confirmed_reps; i++) {
			comboStream = Stream.concat(comboStream,
					Stream.of(new ParamConfig(i + 1, seedValuesList.get(i), resistance, "none", 31,
							initialInfectedValuesList.get(i), 
							transmissionMSMValuesList.get(i),transmissionMSWValuesList.get(i),transmissionFValuesList.get(i),
							recoveryLambdaValuesList.get(i), 
							probSymptomaticMSMValuesList.get(i),probSymptomaticMSWValuesList.get(i),probSymptomaticFValuesList.get(i),
							screenIntervalMSMValuesList.get(i),screenIntervalMSWValuesList.get(i),screenIntervalWValuesList.get(i), 
							delayToSeekCareMSMValuesList.get(i),delayToSeekCareMSWValuesList.get(i),delayToSeekCareFValuesList.get(i),
							delayToRetreatmentMSMValuesList.get(i), delayToRetreatmentMSWValuesList.get(i),delayToRetreatmentFValuesList.get(i),
							percentResistantAValuesList.get(i),
							beginImportingBValuesList.get(i), importingBIntervalValuesList.get(i),
							DSTsensitivityValuesList.get(i), DSTspecificityValuesList.get(i),
							careCostValuesList.get(i), testCostValuesList.get(i), strainTestCostValuesList.get(i),
							treatmentACostValuesList.get(i), treatmentBCostValuesList.get(i),
							treatmentXCostValuesList.get(i), treatmentECostValuesList.get(i))));
		}

		comboStream.
		parallel().
		forEach(parameterConfiguration -> eachRun(batchDirPath, confirmed_reps, parameterConfiguration, 520));
		
		System.out.println("completed " + reps + " runs! ");

		System.out.println("completed sweep!");
		
		combineCSVs(batchDirPath, "sweep", resistance, 0);

	}
	
	public void executeCalibratedBatch(File scenariofile, String counterfactual, String resistance, int yearX) {
		int reps = 0;

		String batchDirPath = makeBatchDir(counterfactual, resistance, yearX);

		List<Double> paramValuesList = null;
		List<Integer> initialInfectedValuesList = new ArrayList<Integer>();
		
		List<Double> transmissionMSMValuesList = new ArrayList<Double>();
		List<Double> transmissionMSWValuesList = new ArrayList<Double>();
		List<Double> transmissionFValuesList = new ArrayList<Double>();
		
		List<Double> recoveryLambdaValuesList = new ArrayList<Double>();
		
		List<Double> probSymptomaticMSMValuesList = new ArrayList<Double>();
		List<Double> probSymptomaticMSWValuesList = new ArrayList<Double>();
		List<Double> probSymptomaticFValuesList = new ArrayList<Double>();
		
		List<Double> screenIntervalMSMValuesList = new ArrayList<Double>();
		List<Double> screenIntervalMSWValuesList = new ArrayList<Double>();
		List<Double> screenIntervalWValuesList = new ArrayList<Double>();

		List<Double> delayToSeekCareMSMValuesList = new ArrayList<Double>();
		List<Double> delayToSeekCareMSWValuesList = new ArrayList<Double>();
		List<Double> delayToSeekCareFValuesList = new ArrayList<Double>();

		
		List<Double> delayToRetreatmentMSMValuesList = new ArrayList<Double>();
		List<Double> delayToRetreatmentMSWValuesList = new ArrayList<Double>();
		List<Double> delayToRetreatmentFValuesList = new ArrayList<Double>();

		
		List<Double> percentResistantAValuesList = new ArrayList<Double>();
		List<Integer> beginImportingBValuesList = new ArrayList<Integer>();
		List<Double> importingBIntervalValuesList = new ArrayList<Double>();
		List<Double> DSTsensitivityValuesList = new ArrayList<Double>();
		List<Double> DSTspecificityValuesList = new ArrayList<Double>();

		List<Double> seedValuesList = new ArrayList<Double>();
		List<Double> careCostValuesList = new ArrayList<Double>();
		List<Double> testCostValuesList = new ArrayList<Double>();
		List<Double> strainTestCostValuesList = new ArrayList<Double>();
		List<Double> treatmentACostValuesList = new ArrayList<Double>();
		List<Double> treatmentBCostValuesList = new ArrayList<Double>();
		List<Double> treatmentXCostValuesList = new ArrayList<Double>();
		List<Double> treatmentECostValuesList = new ArrayList<Double>();
		CalibratedParameters calibrated = new CalibratedParameters();

		try {
			initialInfectedValuesList = calibrated.getInitialInfectedValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		//transmission parameters
		try {
			transmissionMSMValuesList = calibrated.getTransmissionMSMValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		try {
			transmissionMSWValuesList = calibrated.getTransmissionMSWValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		try {
			transmissionFValuesList = calibrated.getTransmissionFValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
		//recovery parameter
		try {
			recoveryLambdaValuesList = calibrated.getRecoveryLambdaValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
		//prob symptomatic parameters
		try {
			probSymptomaticMSMValuesList = calibrated.getProbSymptomaticMSMValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		try {
			probSymptomaticMSWValuesList = calibrated.getProbSymptomaticMSWValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		try {
			probSymptomaticFValuesList = calibrated.getProbSymptomaticFValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
		
		
		///screen interval parameters
		try {
			screenIntervalMSMValuesList = calibrated.getScreenIntervalMSMValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		try {
			screenIntervalMSWValuesList = calibrated.getScreenIntervalMSWValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
			try {
				screenIntervalWValuesList = calibrated.getScreenIntervalWValues();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
			
			
		//delay to seek care parameters
		try {
			delayToSeekCareMSMValuesList = calibrated.getDelayToSeekCareMSMValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		try {
			delayToSeekCareMSWValuesList = calibrated.getDelayToSeekCareMSWValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		try {
			delayToSeekCareFValuesList = calibrated.getDelayToSeekCareFValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		//delay to retreatment parameters
		try {
			delayToRetreatmentMSMValuesList = calibrated.getDelayToRetreatmentMSMValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		try {
			delayToRetreatmentMSWValuesList = calibrated.getDelayToRetreatmentMSWValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		try {
			delayToRetreatmentFValuesList = calibrated.getDelayToRetreatmentFValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
		
		try {
			percentResistantAValuesList = calibrated.getPercentResistantAValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		try {
			beginImportingBValuesList = calibrated.getBeginImportingBValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		try {
			importingBIntervalValuesList = calibrated.getImportingBIntervalValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}


		try {
			DSTsensitivityValuesList = calibrated.getDSTsensitivityValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		try {
			DSTspecificityValuesList = calibrated.getDSTspecificityValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		try {
			seedValuesList = calibrated.getSeedValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		try {
			careCostValuesList = calibrated.getCareCostValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		try {
			testCostValuesList = calibrated.getTestCostValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		try {
			strainTestCostValuesList = calibrated.getStrainTestCostValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		try {
			treatmentACostValuesList = calibrated.getTreatmentACostValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		try {
			treatmentBCostValuesList = calibrated.getTreatmentBCostValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		try {
			treatmentXCostValuesList = calibrated.getTreatmentXCostValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		try {
			treatmentECostValuesList = calibrated.getTreatmentECostValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		reps = transmissionMSMValuesList.size();

		final Integer confirmed_reps = Integer.valueOf(reps);

		// puts together sets of parameters
		List<ParamConfig> lst = new ArrayList<ParamConfig>();
		Stream<ParamConfig> comboStream = lst.stream();
		for (int i = 0; i < confirmed_reps; i++) {
			comboStream = Stream.concat(comboStream,
					Stream.of(new ParamConfig(i + 1, seedValuesList.get(i), resistance, counterfactual, yearX,
							initialInfectedValuesList.get(i), 
							transmissionMSMValuesList.get(i),transmissionMSWValuesList.get(i),transmissionFValuesList.get(i),
							recoveryLambdaValuesList.get(i), 
							probSymptomaticMSMValuesList.get(i),probSymptomaticMSWValuesList.get(i),probSymptomaticFValuesList.get(i),
							screenIntervalMSMValuesList.get(i),screenIntervalMSWValuesList.get(i),screenIntervalWValuesList.get(i), 
							delayToSeekCareMSMValuesList.get(i),delayToSeekCareMSWValuesList.get(i),delayToSeekCareFValuesList.get(i),
							delayToRetreatmentMSMValuesList.get(i), delayToRetreatmentMSWValuesList.get(i),delayToRetreatmentFValuesList.get(i),
							percentResistantAValuesList.get(i),
							beginImportingBValuesList.get(i), importingBIntervalValuesList.get(i),
							DSTsensitivityValuesList.get(i), DSTspecificityValuesList.get(i),
							careCostValuesList.get(i), testCostValuesList.get(i), strainTestCostValuesList.get(i),
							treatmentACostValuesList.get(i), treatmentBCostValuesList.get(i),
							treatmentXCostValuesList.get(i), treatmentECostValuesList.get(i))));
		}

		comboStream.
		parallel().
		forEach(parameterConfiguration -> eachRun(batchDirPath, confirmed_reps, parameterConfiguration, 1560));
		System.out.println("completed " + reps + " runs!");
		
		combineCSVs(batchDirPath, counterfactual, resistance, yearX);

	}

	public void eachRun(String batchDirPath, int reps, ParamConfig paramConfig, int endTime) {
//		try {
//			runner.load(scenariofile); // load the repast scenario
//		} catch (Exception e) {
//			e.printStackTrace();
//		}

		System.out.println("starting run " + paramConfig.batchNumber() + " of " + reps + "...");

		
		SingleRun thisRun = new SingleRun(batchDirPath, setParameters(paramConfig.batchNumber(), endTime, paramConfig.getSeed(), paramConfig.getResistance(),
				paramConfig.getCounterfactual(), paramConfig.getYearX(), paramConfig.getInitialInfected(), 
				paramConfig.getTransmissionMSM(),paramConfig.getTransmissionMSW(), paramConfig.getTransmissionF(),
				paramConfig.getRecoveryLambda(), 
				paramConfig.getProbSymptomaticMSM(),paramConfig.getProbSymptomaticMSW(),paramConfig.getProbSymptomaticF(), 
				paramConfig.getScreenIntervalMSM(),paramConfig.getScreenIntervalMSW(),paramConfig.getScreenIntervalW(),
				paramConfig.getDelayToSeekCareMSM(), paramConfig.getDelayToSeekCareMSW(),paramConfig.getDelayToSeekCareF(),
				paramConfig.getDelayToRetreatmentMSM(),paramConfig.getDelayToRetreatmentMSW(),paramConfig.getDelayToRetreatmentF(),
				paramConfig.getPercentResistantA(), paramConfig.getBeginImportingB(),
				paramConfig.getImportingBInterval(), paramConfig.getDSTsensitivity(), paramConfig.getDSTspecificity(),
				paramConfig.getcareCost(), paramConfig.getTestCost(),
				paramConfig.getstrainTestCost(), paramConfig.getTreatmentACost(), paramConfig.getTreatmentBCost(), paramConfig.getTreatmentXCost(), paramConfig.getTreatmentECost()));
		
		thisRun.setUp(endTime);
		
		
		thisRun.go();
		
		//setUpOne(runner, paramConfig, endTime);
		//runOne(runner);
		//runner.cleanUpRun();
		//runner.cleanUpBatch();

	}
	

	public Parameters setParameters(int runNumber, int endTime, int seed, String resistance, String counterfactual, int yearX,
			int initialInfected, 
			double transmissionMSM, double transmissionMSW, double transmissionF, 
			double recoveryLambda, 
			double probSymptomaticMSM, double probSymptomaticMSW, double probSymptomaticF,
			double screenIntervalMSM, double screenIntervalMSW, double screenIntervalW, 
			double delayToSeekCareMSM, double delayToSeekCareMSW,double delayToSeekCareF,
			double delayToRetreatmentMSM, double delayToRetreatmentMSW,double delayToRetreatmentF,
			double percentResistantA,
			int beginImportingB, double importingBInterval, double DSTsensitivity, double DSTspecificity, double careCost, double testCost, double strainTestCost,
			double treatmentACost, double treatmentBCost, double treatmentXCost, double treatmentECost) {
		DefaultParameters params = new DefaultParameters();
		params.addParameter("runNumber", "runNumber", int.class, runNumber, false);
		params.addParameter("randomSeed", "random seed", int.class, 1, false);
		params.addParameter("transmissionMSM", "TransmissionMSM", double.class, transmissionMSM, false);
		params.addParameter("transmissionMSW", "TransmissionMSW", double.class, transmissionMSW, false);
		params.addParameter("transmissionF", "TransmissionF", double.class, transmissionF, false);
		params.addParameter("recovery_lambda", "RecoveryLambda", double.class, recoveryLambda, false);
		params.addParameter("prob_symptomatic_msm", "ProbSymptomaticMSM", double.class, probSymptomaticMSM, false);
		params.addParameter("prob_symptomatic_msw", "ProbSymptomaticMSW", double.class, probSymptomaticMSW, false);
		params.addParameter("prob_symptomatic_f", "ProbSymptomaticF", double.class, probSymptomaticF, false);
		
		params.addParameter("screen_interval_MSM", "ScreenIntervalMSM", double.class, screenIntervalMSM, false);
		params.addParameter("screen_interval_MSW", "ScreenIntervalMSW", double.class, screenIntervalMSW, false);
		params.addParameter("screen_interval_W", "ScreenIntervalW", double.class, screenIntervalW, false);
		params.addParameter("delay_to_seek_care_msm", "DelayToSeekCareMSM", double.class, delayToSeekCareMSM, false);
		params.addParameter("delay_to_seek_care_msw", "DelayToSeekCareMSW", double.class, delayToSeekCareMSW, false);
		params.addParameter("delay_to_seek_care_f", "DelayToSeekCareF", double.class, delayToSeekCareF, false);

		params.addParameter("delay_to_retreatment_msm", "DelayToRetreatmentMSM", double.class, delayToRetreatmentMSM, false);
		params.addParameter("delay_to_retreatment_msw", "DelayToRetreatmentMSW", double.class, delayToRetreatmentMSW, false);
		params.addParameter("delay_to_retreatment_f", "DelayToRetreatmentF", double.class, delayToRetreatmentF, false);

		params.addParameter("percent_resistant_A", "percent_resistant_A", double.class, percentResistantA, false);
		params.addParameter("begin_importing_B", "begin_importing_B", int.class, beginImportingB, false);
		params.addParameter("importing_B_interval", "importing_B_interval", double.class, importingBInterval, false);
		params.addParameter("DSTsensitivity", "DSTsensitivity,", double.class, DSTsensitivity, false);
		params.addParameter("DSTspecificity", "DSTspecificity,", double.class, DSTspecificity, false);
		
		params.addParameter("seed", "seed", int.class, seed, false);
		params.addParameter("resistance", "Resistance", String.class, resistance, false);

		params.addParameter("population_size", "Pop Size", int.class, 100000, false);
		params.addParameter("infected_count_init", "Initial Infected", int.class, initialInfected, false);
		params.addParameter("end_time", "EndTime", int.class, endTime, false);

		params.addParameter("counterfactual", "counterfactual", String.class, counterfactual, false);
		params.addParameter("yearX", "yearX", int.class, yearX, false);
		params.addParameter("care_cost", "care_cost", double.class, careCost, false);
		params.addParameter("test_cost", "test_cost", double.class, testCost, false);
		params.addParameter("strain_test_cost", "strain_test_cost", double.class, strainTestCost, false);
		params.addParameter("treatment_A_cost", "treatment_A_cost", double.class, treatmentACost, false);
		params.addParameter("treatment_B_cost", "treatment_B_cost", double.class, treatmentBCost, false);
		params.addParameter("treatment_X_cost", "treatment_X_cost", double.class, treatmentXCost, false);
		params.addParameter("treatment_E_cost", "treatment_E_cost", double.class, treatmentECost, false);

		// System.out.println(params.getSchema().parameterNames());

		return (Parameters) params;
	}
	
	public String makeBatchDir(String counterfactual, String resistance, int yearX) {
		LocalDate date = LocalDate.now();
		
		Month month = date.getMonth();
		int day = date.getDayOfMonth();
		int year = date.getYear();
		
		String fullDate = month +"_"+ day +"_"+ year;

		String dirname = "/Users/me597/Documents/output/output_" + fullDate +"_6_";
		//String filename = "SimpleSIR_custom_output_" + fullDate +"_debug2_";

		//String filename = "SimpleSIR_custom_output_MAY_13_2024_overnight_";
		
		dirname += counterfactual;
		
		dirname += "_";
		
		dirname += resistance;
		
		dirname += "_";
		
		dirname += String.valueOf(yearX);		
		
		new File(dirname).mkdir();
		
		return dirname + "/";
		
	}
	
	public void combineCSVs(String dirpath, String counterfactual, String resistance, int yearX) {
        // Directory containing CSV files
        File directory = new File(dirpath);

        // Combined CSV file
        File combinedFile = new File(dirpath + counterfactual + resistance + yearX + "combined.csv");

        try {
            // Create a writer for the combined file
            FileWriter writer = new FileWriter(combinedFile);
            CSVWriter CSVwriter = new CSVWriter(writer);
            String[] header = { 
	        		"RunNumber", 
	        		"seed",
	        		"counterfactual",
	        		"InitialInfected",
	        		
	        		"TransmissionMSM",
	        		"TransmissionMSW",
	        		"TransmissionF",
	        		
	        		"RecoveryLambda",
	        		
	        		"ProbSymptomaticMSM",
	        		"ProbSymptomaticMSW",
	        		"ProbSymptomaticF",
	        		
	        		"ScreenIntervalMSM",
	        		"ScreenIntervalMSW",
	        		"ScreenIntervalW",

	        		
	        		"DelayToSeekCareMSM",
	        		"DelayToSeekCareMSW",
	        		"DelayToSeekCareF",

	        		"DelayToRetreatmentMSM",
	        		"DelayToRetreatmentMSW",
	        		"DelayToRetreatmentF",

	        		"PercentResistantA",
	        		"BeginImportingB",
	        		"ImportingBInterval",
	        		"DSTsensitivity", 
	        		"DSTspecificity",
	        		"CareCost",
	        		"TestCost",
	        		"StrainTestCost",
	        		"DrugATreatmentCost",
	        		"DrugBTreatmentCost",
	        		"DrugXTreatmentCost",
	        		"DrugETreatmentCost",
	        		"tick", 
	        		"Prevalence",
	        		"Incidence",
	        		"ResistAIncidence",
	        		"ResistBIncidence",
	        		"ResistBothIncidence",
	        		"SymptomProportion",
	        		"Treatments",
	        		"FailedTreatments",
	        		"Detected",
	        		"DetectedAndSymptoms",
	        		"DetectedThruScreen",
//	        		"KnownFailedTreatments",
//	        		"KnownFailedTreatmentsA",
//	        		"KnownFailedTreatmentsB",
//	        		"KnownFailedTreatmentsBoth",
	        		"SuccessTreatmentsA",
	        		"SuccessTreatmentsB",
	        		"SuccessTreatmentsX",
	        		"AttemptTreatmentsA",
	        		"AttemptTreatmentsB",
	        		"AttemptTreatmentsX",
	        		"UsageofErtapenem",
	        		"SurveillanceEstPropResistA",
	        		"SurveillanceEstPropResistB",
	        		"SurveillanceEstPropResistBoth",
	        		"SwitchedToB",
	        		"SwitchedToX",
	        		"AnnualMonetaryCost",
	        		"AnnualQALYsLost",
	        		
	        		//MSM output
	        		"MSMPopSize",
	    			 "prevMSM",
	    			 "incMSM",
	    			 "detectedIncMSM",
	    			 "detectedAndSymptomsMSM",
	    			 "resistAIncidenceMSM",
	    			 "resistBIncidenceMSM",
	    			 "resistBothIncidenceMSM",
//	    			 "knownFailedTreatmentsMSM",
//	    			 "knownFailedTreatmentsBMSM",
//	    			 "knownFailedTreatmentsBothMSM",
	    			 "successTreatmentsAMSM",
	    			 "successTreatmentsBMSM", 
	    			 "successTreatmentsXMSM",
	    			 "attemptTreatmentsAMSM",
	    			 "attemptTreatmentsBMSM",
	    			 "attemptTreatmentsXMSM",
	    			 "usageEMSM",
	    			 "monetaryCostMSM",
	    			 "QALYcostMSM",
	    			
	    			"MSMWPopSize",
	    			 "prevMSMW",
	    			"incMSMW",
	    			"detectedIncMSMW",
	    			 "detectedAndSymptomsMSMW",
	    			"resistAIncidenceMSMW",
	    			"resistBIncidenceMSMW",
	    			"resistBothIncidenceMSMW",
//	    			"knownFailedTreatmentsMSMW",
//	    			"knownFailedTreatmentsAMSMW",
//	    			"knownFailedTreatmentsBMSMW",
//	    			"knownFailedTreatmentsBothMSMW",
	    			"successTreatmentsAMSMW",
	    			"successTreatmentsBMSMW", 
	    			"successTreatmentsXMSMW",
	    			"attemptTreatmentsAMSMW",
	    			"attemptTreatmentsBMSMW",
	    			"attemptTreatmentsXMSMW",
	    			"usageEMSMW",
	    			"monetaryCostMSMW",
	    			"QALYcostMSMW",
	    			
	    			//MSW output
	    			"MSWPopSize",
	    			"prevMSW",
	    			"incMSW",
	    			"detectedIncMSW",
	    			 "detectedAndSymptomsMSW",
	    			"resistAIncidenceMSW",
	    			"resistBIncidenceMSW",
	    			"resistBothIncidenceMSW",
//	    			"knownFailedTreatmentsMSW",
//	    			"knownFailedTreatmentsAMSW",
//	    			"knownFailedTreatmentsBMSW",
//	    			"knownFailedTreatmentsBothMSW",
	    			"successTreatmentsAMSW",
	    			"successTreatmentsBMSW", 
	    			"successTreatmentsXMSW",
	    			"attemptTreatmentsAMSW",
	    			"attemptTreatmentsBMSW",
	    			"attemptTreatmentsXMSW",
	    			"usageEMSW",
	    			"monetaryCostMSW",
	    			"QALYcostMSW",
	    			
	    			//W output
	    			"WPopSize",
	    			"prevW",
	    			"incW",
	    			"detectedIncW",
	    			 "detectedAndSymptomsW",
	    			"resistAIncidenceW",
	    			"resistBIncidenceW",
	    			"resistBothIncidenceW",
//	    			"knownFailedTreatmentsW",
//	    			"knownFailedTreatmentsAW",
//	    			"knownFailedTreatmentsBW",
//	    			"knownFailedTreatmentsBothW",
	    			"successTreatmentsAW",
	    			"successTreatmentsBW", 
	    			"successTreatmentsXW",
	    			"attemptTreatmentsAW",
	    			"attemptTreatmentsBW",
	    			"attemptTreatmentsXW",
	    			"usageEW",
	    			"monetaryCostW",
	    			"QALYcostW",
	    			
	    			//NB output
	    			"NBPopSize",
	    			"prevNB",
	    			"incNB",
	    			"detectedIncNB",
	    			 "detectedAndSymptomsNB",
	    			"resistAIncidenceNB",
	    			"resistBIncidenceNB",
	    			"resistBothIncidenceNB",
//	    			"knownFailedTreatmentsNB",
//	    			"knownFailedTreatmentsANB",
//	    			"knownFailedTreatmentsBNB",
//	    			"knownFailedTreatmentsBothNB",
	    			"successTreatmentsANB",
	    			"successTreatmentsBNB", 
	    			"successTreatmentsXNB",
	    			"attemptTreatmentsANB",
	    			"attemptTreatmentsBNB",
	    			"attemptTreatmentsXNB",
	    			"usageENB",
	    			"monetaryCostNB",
	    			"QALYcostNB"
	        }; 
	        
	        CSVwriter.writeNext(header); 

            // Iterate through CSV files in the directory
            Files.list(directory.toPath())
                .filter(path -> path.toString().endsWith(".csv"))
                .forEach(csvFile -> {
                    try {
                        // Read content of each CSV file and write it to the combined file
                        Files.lines(csvFile).skip(1).forEach(line -> {
                            try {
                                writer.write(line + "\n");
                            } catch (IOException e) {
                                e.printStackTrace();
                            }
                        });
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                });

            // Close the writer
            writer.close();
            
            System.out.println("All CSV files have been combined into " + combinedFile.getAbsolutePath());
        } catch (IOException e) {
            e.printStackTrace();
        }
	}
        
  
        
    }



