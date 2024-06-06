package msmOnlyModel;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ForkJoinPool;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import java.time.LocalDate;
import java.time.Month;
import repast.simphony.parameter.DefaultParameters;
import repast.simphony.parameter.Parameters;

public class BatchRun {
	private int batchNumber;
	private String expdir;
	
	private String counterfactual;
	private String resistance;
	private int yearX;
	
	public BatchRun(String counterfactual, String resistance, int yearX) {
		this.counterfactual = counterfactual;
		this.resistance = resistance;
		this.yearX = yearX;
		
		batchNumber = 1;
		expdir = makeExperimentDir();
		
	}
	

	public void executeSweep(File scenariofile, int reps, String resistance) {
		
		String batchDirPath = makeBatchDir();


		CustomParameterSweep sweeper = new CustomParameterSweep();
		List<Double> seedValuesList = sweeper.getSeedValues(reps);

		List<Integer> initialInfectedValuesList = sweeper.getInitialInfectedValues(reps);
		
		List<Double> transmissionMSMValuesList = sweeper.getTransmissionMSMValues(reps);
		
		List<Double> recoveryLambdaValuesList = sweeper.getRecoveryLambdaValues(reps);
		
		List<Double> probSymptomaticMSMValuesList = sweeper.getProbSymptomaticMSMValues(reps);
		
		List<Double> screenIntervalMSMValuesList = sweeper.getScreenIntervalMSMValues(reps);
		
		List<Double> delayToSeekCareMSMValuesList = sweeper.getDelayToSeekCareMSMValues(reps);

		List<Double> delayToRetreatmentMSMValuesList = sweeper.getDelayToRetreatmentMSMValues(reps);

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
					Stream.of(new ParamConfig(i + 1, seedValuesList.get(i), resistance, counterfactual, 31,
							initialInfectedValuesList.get(i), 
							transmissionMSMValuesList.get(i),
							recoveryLambdaValuesList.get(i), 
							probSymptomaticMSMValuesList.get(i),
							screenIntervalMSMValuesList.get(i), 
							delayToSeekCareMSMValuesList.get(i),
							delayToRetreatmentMSMValuesList.get(i), 
							percentResistantAValuesList.get(i),
							beginImportingBValuesList.get(i), importingBIntervalValuesList.get(i),
							DSTsensitivityValuesList.get(i), DSTspecificityValuesList.get(i),
							careCostValuesList.get(i), testCostValuesList.get(i), strainTestCostValuesList.get(i),
							treatmentACostValuesList.get(i), treatmentBCostValuesList.get(i),
							treatmentXCostValuesList.get(i), treatmentECostValuesList.get(i))));
		}
		
//		comboStream.
//		parallel().
//		forEach(parameterConfiguration -> eachRun(batchDirPath, confirmed_reps, parameterConfiguration, 520));
//		System.out.println("completed " + reps + " runs!");
		
		
		
		final Stream<ParamConfig> parallelizableComboStream = comboStream;

		ForkJoinPool customThreadPool = new ForkJoinPool(7);
		try {
			customThreadPool.submit(
			() -> 
			parallelizableComboStream.
			parallel().
			forEach(parameterConfiguration -> eachRun(batchDirPath, confirmed_reps, parameterConfiguration, 520))).get();
		} catch (InterruptedException | ExecutionException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}

		
		System.out.println("completed " + reps + " runs! ");

		System.out.println("completed sweep!");
		
		try {
			combineCSVs(batchDirPath);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		batchNumber += 1;

	}
	
	public void executeCalibratedBatch(File scenariofile, String counterfactual, String resistance, int yearX) {
		int reps = 0;
		
		this.counterfactual=counterfactual;
		this.resistance=resistance;
		this.yearX=yearX;

		String batchDirPath = makeBatchDir();
		
		
		List<Integer> initialInfectedValuesList = new ArrayList<Integer>();
		
		List<Double> transmissionMSMValuesList = new ArrayList<Double>();
		
		List<Double> recoveryLambdaValuesList = new ArrayList<Double>();
		
		List<Double> probSymptomaticMSMValuesList = new ArrayList<Double>();
		
		List<Double> screenIntervalMSMValuesList = new ArrayList<Double>();

		List<Double> delayToSeekCareMSMValuesList = new ArrayList<Double>();

		
		List<Double> delayToRetreatmentMSMValuesList = new ArrayList<Double>();

		
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
		
		
		
		
		///screen interval parameters
		try {
			screenIntervalMSMValuesList = calibrated.getScreenIntervalMSMValues();
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
		
		
		//delay to retreatment parameters
		try {
			delayToRetreatmentMSMValuesList = calibrated.getDelayToRetreatmentMSMValues();
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
							transmissionMSMValuesList.get(i),
							recoveryLambdaValuesList.get(i), 
							probSymptomaticMSMValuesList.get(i),
							screenIntervalMSMValuesList.get(i), 
							delayToSeekCareMSMValuesList.get(i),
							delayToRetreatmentMSMValuesList.get(i), 
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
		
		
		try {
			combineCSVs(batchDirPath);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

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
				paramConfig.getTransmissionMSM(),
				paramConfig.getRecoveryLambda(), 
				paramConfig.getProbSymptomaticMSM(), 
				paramConfig.getScreenIntervalMSM(),
				paramConfig.getDelayToSeekCareMSM(), 
				paramConfig.getDelayToRetreatmentMSM(),
				paramConfig.getPercentResistantA(), paramConfig.getBeginImportingB(),
				paramConfig.getImportingBInterval(), paramConfig.getDSTsensitivity(), paramConfig.getDSTspecificity(),
				paramConfig.getcareCost(), paramConfig.getTestCost(),
				paramConfig.getstrainTestCost(), paramConfig.getTreatmentACost(), paramConfig.getTreatmentBCost(), paramConfig.getTreatmentXCost(), paramConfig.getTreatmentECost()));
		
		thisRun.setUp(endTime);
		
		
		thisRun.go();
		
		thisRun.end();
		
		// Hint to the Garbage Collector that it might want to collect the garbs
		System.gc();
		//setUpOne(runner, paramConfig, endTime);
		//runOne(runner);
		//runner.cleanUpRun();
		//runner.cleanUpBatch();

	}
	

	public Parameters setParameters(int runNumber, int endTime, int seed, String resistance, String counterfactual, int yearX,
			int initialInfected, 
			double transmissionMSM,  
			double recoveryLambda, 
			double probSymptomaticMSM, 
			double screenIntervalMSM,  
			double delayToSeekCareMSM, 
			double delayToRetreatmentMSM, 
			double percentResistantA,
			int beginImportingB, double importingBInterval, double DSTsensitivity, double DSTspecificity, double careCost, double testCost, double strainTestCost,
			double treatmentACost, double treatmentBCost, double treatmentXCost, double treatmentECost) {
		DefaultParameters params = new DefaultParameters();
		params.addParameter("runNumber", "runNumber", int.class, runNumber, false);
		params.addParameter("randomSeed", "random seed", int.class, 1, false);
		params.addParameter("transmissionMSM", "TransmissionMSM", double.class, transmissionMSM, false);
		params.addParameter("recovery_lambda", "RecoveryLambda", double.class, recoveryLambda, false);
		params.addParameter("prob_symptomatic_msm", "ProbSymptomaticMSM", double.class, probSymptomaticMSM, false);
		params.addParameter("screen_interval_MSM", "ScreenIntervalMSM", double.class, screenIntervalMSM, false);
		params.addParameter("delay_to_seek_care_msm", "DelayToSeekCareMSM", double.class, delayToSeekCareMSM, false);
		params.addParameter("delay_to_retreatment_msm", "DelayToRetreatmentMSM", double.class, delayToRetreatmentMSM, false);

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
	
	public String makeExperimentDir() {
		LocalDate date = LocalDate.now();
		
		Month month = date.getMonth();
		int day = date.getDayOfMonth();
		int year = date.getYear();
		
		String fullDate = month +"_"+ day +"_"+ year;

		//String dirname = "/Users/me597/Documents/MSMoutput/output_" + fullDate +"_1_";
		
		String dirname = "/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_overnight_";
		
		dirname += counterfactual;
		
		dirname += "_";
		
		dirname += resistance;
		
		dirname += "_";
		
		dirname += String.valueOf(yearX);
		
		new File(dirname).mkdir();
		
		return dirname + "/";
	}
	
	public String makeBatchDir() {


		String dirname = expdir;
		
		dirname += "Batch_";
		
		dirname += String.valueOf(batchNumber);
		
		dirname += "_";
		
		dirname += counterfactual;
	
		dirname += "_";
		
		dirname += yearX;

		
		//for debugging large number of files: 
		//dirname = "/Users/me597/Documents/output/output_MAY_15_2024_6_sweep_none_0";

		
		new File(dirname).mkdir();
		
		return dirname + "/";
		
	}
	
	public void combineCSVs(String dirpath) throws IOException {
        // Directory containing CSV files
        File directory = new File(dirpath);
        
        if (!directory.exists() || !directory.isDirectory()) {
            throw new IllegalArgumentException("The specified path is not a valid directory: " + dirpath);
        }

        // Combined CSV file
        String combinedFile = (expdir + counterfactual + resistance + yearX + batchNumber + "combined.csv");
       
        try (BufferedWriter writer = Files.newBufferedWriter(Paths.get(combinedFile))){
        	boolean headerWritten = false;

        	List<Path> csvfiles = Files.list(Paths.get(dirpath)).
        			filter(p -> p.getFileName().toString().contains(counterfactual)).
        			collect(Collectors.toList());
        	
        	if (csvfiles.isEmpty()) {
        		throw new FileNotFoundException("No CSV files found in directory: " + dirpath);
        	}

        	for (Path csvFile : csvfiles) {
        		//System.out.println("Reading from " + csvFile.getFileName());
        		try (BufferedReader reader = Files.newBufferedReader(csvFile)){
        			String line;
        			boolean isFirstLine = true;

        			while ((line = reader.readLine()) != null) {
        				if (isFirstLine) {
        					if (!headerWritten) {
        						writer.write(line);
        						writer.newLine();
        						headerWritten = true;
        						System.out.println("Header written from" + csvFile);
        					}
        				} else {
        					writer.write(line);
        					writer.newLine();
        					//System.out.println("Line written to " + csvFile);
        				}
        				isFirstLine = false;
        			}

        		} catch (IOException e){
        			System.err.println("Error reading file: " + csvFile.toString());
        			e.printStackTrace();
        		}
        	}
        } catch (IOException e) {
        		System.err.println("Error writing file: " + combinedFile.toString());
    			e.printStackTrace();
        	}

            
	        
			

            
        
	}
	
	public void combineBatchFiles() throws IOException {
		 File directory = new File(expdir);
	        
	        if (!directory.exists() || !directory.isDirectory()) {
	            throw new IllegalArgumentException("The specified path is not a valid directory: " + expdir);
	        }

	        // Combined CSV file
	        String combinedFile = (expdir + counterfactual + resistance + yearX + "supercombined.csv");
	       
	        try (BufferedWriter writer = Files.newBufferedWriter(Paths.get(combinedFile))){
	        	boolean headerWritten = false;
	        	
	        	List<Path> csvfiles = Files.list(Paths.get(expdir)).filter(p -> !p.getFileName().toString().contains("super")).filter(p -> p.getFileName().toString().contains(".csv")).collect(Collectors.toList());
	        	
	        	if (csvfiles.isEmpty()) {
	        		throw new FileNotFoundException("No CSV files found in directory: " + expdir);
	        	}

	        	for (Path csvFile : csvfiles) {
	        		//System.out.println("Reading from " + csvFile.getFileName());
	        		try (BufferedReader reader = Files.newBufferedReader(csvFile)){
	        			String line;
	        			boolean isFirstLine = true;

	        			while ((line = reader.readLine()) != null) {
	        				if (isFirstLine) {
	        					if (!headerWritten) {
	        						writer.write(line);
	        						writer.newLine();
	        						headerWritten = true;
	        						//System.out.println("Header written to" + csvFile);
	        					}
	        				} else {
	        					writer.write(line);
	        					writer.newLine();
	        					//System.out.println("Line written to " + csvFile);
	        				}
	        				isFirstLine = false;
	        			}

	        		} catch (IOException e){
	        			System.err.println("Error reading file: " + csvFile.toString());
	        			e.printStackTrace();
	        		}
	        	}
	        } catch (IOException e) {
	        		System.err.println("Error writing file: " + combinedFile.toString());
	    			e.printStackTrace();
	        	}

	        	
	        	
	
	
    System.out.println("All CSV files have been combined into " + combinedFile);

  
        
    }
}


