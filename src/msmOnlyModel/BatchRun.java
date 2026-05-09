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
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.math3.stat.descriptive.rank.Percentile;

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
	private double fitnessA;
	private double fitnessB;
	
	private AtomicInteger currentRun = new AtomicInteger(0);

	public BatchRun(String counterfactual, String resistance, int yearX, double fitnessA, double fitnessB) {
		this.counterfactual = counterfactual;
		this.resistance = resistance;
		this.yearX = yearX;
		this.fitnessA = fitnessA;
		this.fitnessB = fitnessB;

		batchNumber = 1;
		expdir = makeExperimentDir();
		
	}
	
	public BatchRun(String counterfactual, String resistance) {
		this.counterfactual = counterfactual;
		this.resistance = resistance;
		this.yearX = 25;
		this.fitnessA = 0;
		this.fitnessB = 0;

		batchNumber = 1;
		expdir = makeExperimentDir();
		
	}
	

	public void executeSweep(File scenariofile, int reps, String resistance) {
		
		String batchDirPath = makeBatchDir();
		currentRun.set(0);

		CustomParameterSweep sweeper = new CustomParameterSweep();
		List<Double> seedValuesList = sweeper.getSeedValues(reps);

		List<Integer> initialInfectedValuesList = sweeper.getInitialInfectedValues(reps);
		
		List<Double> propHighActivityValuesList = sweeper.getPropHighActivityValues(reps);
		
		List<Double> transmissionMSMValuesList = sweeper.getTransmissionMSMValues(reps);
		
		List<Double> recoveryTimeValuesList = sweeper.getNaturalRecoveryTimeValues(reps);
		
		List<Double> probSymptomaticMSMValuesList = sweeper.getProbSymptomaticMSMValues(reps);
		
		List<Double> screenIntervalMeanMSMValuesList = sweeper.getScreenIntervalMeanMSMValues(reps);
		List<Double> screenIntervalVarMSMValuesList = sweeper.getScreenIntervalVarMSMValues(reps);
		
		List<Double> delayToSeekCareMSMValuesList = sweeper.getDelayToSeekCareMSMValues(reps);

		List<Double> delayToRetreatmentMSMValuesList = sweeper.getDelayToRetreatmentMSMValues(reps);
		
		List<Double> assortativityValuesList = sweeper.getAssortativityValues(reps);
		
		List<Double> activityGroupTransferPropValuesList = sweeper.getActivityGroupTransferPropValues(reps);
		
		List<Double> activityGroupTransmissionRatioValuesList = sweeper.getActivityGroupTransmissionRatioValues(reps);

		List<Double> percentResistantAValuesList = sweeper.getPercentResistantA(reps);
		List<Integer> beginImportingBValuesList = sweeper.getBeginImportingB(reps);
		List<Double> importingBIntervalValuesList = sweeper.getImportingBInterval(reps);
		List<Double> probDevelopResistanceAExponentValuesList = sweeper.getProbDevelopResistanceAExponent(reps);
		List<Double> probDevelopResistanceBExponentValuesList = sweeper.getProbDevelopResistanceBExponent(reps);

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
					Stream.of(new ParamConfig(i + 1, 
							seedValuesList.get(i), 
							resistance, 
							counterfactual, 
							31,
							initialInfectedValuesList.get(i), 
							propHighActivityValuesList.get(i),
							transmissionMSMValuesList.get(i),
							recoveryTimeValuesList.get(i), 
							probSymptomaticMSMValuesList.get(i),
							screenIntervalMeanMSMValuesList.get(i), 
							screenIntervalVarMSMValuesList.get(i), 
							delayToSeekCareMSMValuesList.get(i),
							delayToRetreatmentMSMValuesList.get(i), 
							assortativityValuesList.get(i),
							activityGroupTransferPropValuesList.get(i),
							activityGroupTransmissionRatioValuesList.get(i),
							percentResistantAValuesList.get(i),
							beginImportingBValuesList.get(i), importingBIntervalValuesList.get(i),
							probDevelopResistanceAExponentValuesList.get(i),
							probDevelopResistanceBExponentValuesList.get(i),
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

		ForkJoinPool customThreadPool = new ForkJoinPool(12);
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

		System.out.println("completed batch " + batchNumber + "!");
		
		try {
			combineCSVs(batchDirPath);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		batchNumber += 1;

	}
	
	
	
	
	public void executeCalibratedBatch(File scenariofile, String counterfactual) {
		//contains constants for default runs
		executeCalibratedBatch(scenariofile, counterfactual, "combo", 25, 5, 80, 80, 50, 0.5, 0.5, 0.0, 0.0, 0.0);
	}
		
		
	public void executeCalibratedBatch(File scenariofile, String counterfactual, String resistance, int yearX, double switchThreshold, int availrDST, int adhereTOCsympt, int adhereTOCasympt, double realisticRandom, double realisticTOC, double realisticDST, double fitnessCostA, double fitnessCostB) {
		
		currentRun.set(0);
		
		int reps = 0;
		
		this.counterfactual=counterfactual;
		this.resistance=resistance;
		this.fitnessA = fitnessCostA;
		this.fitnessB = fitnessCostB;

		String batchDirPath = makeBatchDir();
		
		

		
		List<Integer> initialInfectedValuesList = new ArrayList<Integer>();
		List <Double> propHighActivityValuesList = new ArrayList<Double>();
		
		List<Double> transmissionMSMValuesList = new ArrayList<Double>();
		
		List<Double> recoveryTimeValuesList = new ArrayList<Double>();
		
		List<Double> probSymptomaticMSMValuesList = new ArrayList<Double>();
		
		List<Double> screenIntervalMeanMSMValuesList = new ArrayList<Double>();
		List<Double> screenIntervalVarMSMValuesList = new ArrayList<Double>();

		List<Double> delayToSeekCareMSMValuesList = new ArrayList<Double>();

		
		List<Double> delayToRetreatmentMSMValuesList = new ArrayList<Double>();
		List <Double> assortativityValuesList = new ArrayList<Double>();
		List<Double> activityGroupTransferPropValuesList = new ArrayList<Double>();
		List<Double> activityGroupTransmissionRatioValuesList = new ArrayList<Double>();


		
		List<Double> percentResistantAValuesList = new ArrayList<Double>();
		List<Integer> beginImportingBValuesList = new ArrayList<Integer>();
		List<Double> importingBIntervalValuesList = new ArrayList<Double>();
		List<Double> probDevelopResistanceAExponentValuesList = new ArrayList<Double>();
		List<Double> probDevelopResistanceBExponentValuesList = new ArrayList<Double>();
		
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
		CalibratedParameters calibrated = new CalibratedParameters(expdir);

		try {
			initialInfectedValuesList = calibrated.getInitialInfectedValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		

		try {
			propHighActivityValuesList = calibrated.getPropActivityRiskValues();
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
			recoveryTimeValuesList = calibrated.getRecoveryTimeValues();
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
			screenIntervalMeanMSMValuesList = calibrated.getScreenIntervalMeanMSMValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
		///screen interval parameters
		try {
			screenIntervalVarMSMValuesList = calibrated.getScreenIntervalVarMSMValues();
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
			assortativityValuesList = calibrated.getAssortativityValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		
		
		try {
			activityGroupTransferPropValuesList = calibrated.getActivityGroupTransferPropValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		try {
			activityGroupTransmissionRatioValuesList = calibrated.getActivityGroupTransmissionRatioValues();
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
			probDevelopResistanceAExponentValuesList = calibrated.getProbDevelopResistanceAExponentValues();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		try {
			probDevelopResistanceBExponentValuesList = calibrated.getProbDevelopResistanceBExponentValues();
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
		for (int i = 0; i < confirmed_reps; i++) {
		    lst.add(new ParamConfig(i + 1, seedValuesList.get(i), resistance, counterfactual, yearX,
		            initialInfectedValuesList.get(i),
		            propHighActivityValuesList.get(i),
		            transmissionMSMValuesList.get(i),
		            recoveryTimeValuesList.get(i),
		            probSymptomaticMSMValuesList.get(i),
		            screenIntervalMeanMSMValuesList.get(i),
		            screenIntervalVarMSMValuesList.get(i),
		            delayToSeekCareMSMValuesList.get(i),
		            delayToRetreatmentMSMValuesList.get(i),
		            assortativityValuesList.get(i),
		            activityGroupTransferPropValuesList.get(i),
		            activityGroupTransmissionRatioValuesList.get(i),
		            percentResistantAValuesList.get(i),
		            beginImportingBValuesList.get(i), importingBIntervalValuesList.get(i),
		            probDevelopResistanceAExponentValuesList.get(i),
					probDevelopResistanceBExponentValuesList.get(i),
		            DSTsensitivityValuesList.get(i), DSTspecificityValuesList.get(i),
		            careCostValuesList.get(i), testCostValuesList.get(i), strainTestCostValuesList.get(i),
		            treatmentACostValuesList.get(i), treatmentBCostValuesList.get(i),
		            treatmentXCostValuesList.get(i), treatmentECostValuesList.get(i)));
		}

		System.out.println("Starting batch " + counterfactual + " of length " + reps);
		
		int numThreads = 20; // Set to desired number of threads
		ForkJoinPool customThreadPool = new ForkJoinPool(numThreads);
		try {
		    customThreadPool.submit(() ->
		        lst.stream().parallel().forEach(parameterConfiguration -> 
		            eachRun(batchDirPath, confirmed_reps, parameterConfiguration, 1560, switchThreshold, 
		                    availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, 
		                    realisticDST, fitnessCostA, fitnessCostB))
		    ).get();
		} catch (InterruptedException | ExecutionException e) {
		    e.printStackTrace();
		} finally {
		    customThreadPool.shutdown();
		}
		System.out.println("completed " + reps + " runs!");
		
		try {
			combineCSVs(batchDirPath);
			//analyzeAndCombineTPI(batchDirPath);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}
	
	public void eachRun(String batchDirPath, int reps, ParamConfig paramConfig, int endTime) {
		eachRun(batchDirPath, reps, paramConfig, endTime, 5.0, 80, 80, 50, 0.5, 0.5, 0.0, 0.0, 0.0);
	}

	public void eachRun(String batchDirPath, 
			int reps, 
			ParamConfig paramConfig, 
			int endTime, 
			double switchThreshold, 
			int availrDST, 
			int adhereTOCsympt, 
			int adhereTOCasympt, 
			double realisticRandom,
			double realisticTOC,
			double realisticDST, double fitnessCostA, double fitnessCostB) {
//		try {
//			runner.load(scenariofile); // load the repast scenario
//		} catch (Exception e) {
//			e.printStackTrace();
//		}

		//System.out.println("starting run " + paramConfig.batchNumber() + " of " + reps + "...");

		
		SingleRun thisRun = new SingleRun(batchDirPath, setParameters(paramConfig.batchNumber(), endTime, paramConfig.getSeed(), paramConfig.getResistance(),
				paramConfig.getCounterfactual(), paramConfig.getYearX(), switchThreshold, availrDST, adhereTOCsympt, adhereTOCasympt, realisticRandom, realisticTOC, realisticDST, paramConfig.getInitialInfected(), paramConfig.getPropHighActivity(),
				paramConfig.getTransmissionMSM(),
				paramConfig.getRecoveryTime(), 
				paramConfig.getProbSymptomaticMSM(), 
				paramConfig.getScreenIntervalMeanMSM(),
				paramConfig.getScreenIntervalVarMSM(),
				paramConfig.getDelayToSeekCareMSM(), 
				paramConfig.getDelayToRetreatmentMSM(),
				paramConfig.getAssortativity(), paramConfig.getActivityGroupTransferProp(), paramConfig.getActivityGroupTransmissionRatio(),
				paramConfig.getPercentResistantA(), paramConfig.getBeginImportingB(),
				paramConfig.getImportingBInterval(), paramConfig.getProbDevelopResistanceA(), paramConfig.getProbDevelopResistanceB(),
				paramConfig.getDSTsensitivity(), paramConfig.getDSTspecificity(),
				paramConfig.getcareCost(), paramConfig.getTestCost(),
				paramConfig.getstrainTestCost(), paramConfig.getTreatmentACost(), paramConfig.getTreatmentBCost(), paramConfig.getTreatmentXCost(), paramConfig.getTreatmentECost(),
				fitnessCostA, fitnessCostB));
		
		thisRun.setUp(endTime);
		
		
		thisRun.go();
		
		thisRun.end();
		
		int completed = currentRun.incrementAndGet();
		
		if (paramConfig.getCounterfactual().contains("sweep")) {
			if (completed == 1 || completed % 1000 == 0 || completed == reps) {
				printProgress(completed, reps);
		}} else {
			if (completed == 1 || completed % 10 == 0 || completed == reps) {
				printProgress(completed, reps);
		}}
		
		// Hint to the Garbage Collector that it might want to collect the garbs
		System.gc();
		//setUpOne(runner, paramConfig, endTime);
		//runOne(runner);
		//runner.cleanUpRun();
		//runner.cleanUpBatch();

	}
	
	public void printProgress(int completed, int total) {
		int barWidth = 40;
	      double pct = (double) completed / total;
	      int filled = (int) (pct * barWidth);

	      StringBuilder bar = new StringBuilder("[");
	      for (int i = 0; i < barWidth; i++) {
	          if (i < filled) bar.append("||");
	          else bar.append("  ");
	      }
	      bar.append("] ");
	      bar.append(completed).append("/").append(total);
	      bar.append(String.format(" (%.0f%%)", pct * 100));
	      bar.append(" in Batch ");
	      bar.append(this.batchNumber);
	      
	      System.out.println(bar.toString());
		
	}
	
	public Parameters setParameters(int runNumber, int endTime, int seed, 
			String resistance, String counterfactual, int yearX, 
			int initialInfected, 
			double propHighActivity,
			double transmissionMSM,  
			double recoveryTime, 
			double probSymptomaticMSM, 
			double screenIntervalMeanMSM,  
			double screenIntervalVarMSM,  
			double delayToSeekCareMSM, 
			double delayToRetreatmentMSM, 
			double assortativity,
			double activityGrouptransferProp,
			double activityGroupTransmissionRatio,
			double percentResistantA,
			int beginImportingB, double importingBInterval, 
			double probDevelopResistanceAExponent, double probDevelopResistanceBExponent,
			double DSTsensitivity, double DSTspecificity, double careCost, double testCost, double strainTestCost,
			double treatmentACost, double treatmentBCost, double treatmentXCost, double treatmentECost,
			double fitnessCostA, double fitnessCostB) {
		return setParameters(runNumber, endTime, seed, resistance, counterfactual, yearX, 5.0, 80, 80, 80, 0.5, 0.5, 0.0,
				initialInfected, propHighActivity, transmissionMSM, recoveryTime, probSymptomaticMSM, screenIntervalMeanMSM, screenIntervalVarMSM, delayToSeekCareMSM, delayToRetreatmentMSM, assortativity, activityGrouptransferProp, activityGroupTransmissionRatio,
				percentResistantA, beginImportingB, importingBInterval, probDevelopResistanceAExponent, probDevelopResistanceBExponent, DSTsensitivity, DSTspecificity, careCost, testCost, strainTestCost, treatmentACost, treatmentBCost, treatmentXCost, treatmentECost,
				fitnessCostA, fitnessCostB);
	}
	

	public Parameters setParameters(int runNumber, int endTime, int seed, 
			String resistance, String counterfactual, int yearX, double switchThreshold, int availrDST, int adhereTOCsympt, int adhereTOCasympt,
			double realisticRandom, double realisticTOC, double realisticDST,
			int initialInfected, 
			double propHighActivity,
			double transmissionMSM,  
			double recoveryTime, 
			double probSymptomaticMSM, 
			double screenIntervalMeanMSM,
			double screenIntervalVarMSM,
			double delayToSeekCareMSM, 
			double delayToRetreatmentMSM, 
			double assortativity,
			double activityGroupTransferProp,
			double activityGroupTransmissionRatio,
			double percentResistantA,
			int beginImportingB, double importingBInterval, 
			double probDevelopResistanceAExponent,
			double probDevelopResistanceBExponent,
			double DSTsensitivity, double DSTspecificity, double careCost, double testCost, double strainTestCost,
			double treatmentACost, double treatmentBCost, double treatmentXCost, double treatmentECost,
			double fitnessCostA, double fitnessCostB) {
		DefaultParameters params = new DefaultParameters();
		params.addParameter("runNumber", "runNumber", int.class, runNumber, false);
		params.addParameter("randomSeed", "random seed", int.class, 1, false);
		
		params.addParameter("counterfactual", "counterfactual", String.class, counterfactual, false);
		params.addParameter("yearX", "yearX", int.class, yearX, false);
		params.addParameter("resistance", "Resistance", String.class, resistance, false);
		
		params.addParameter("switchThreshold", "switchThreshold", double.class, switchThreshold, false);
		params.addParameter("availrDST", "availrDST", int.class, availrDST, false);
		params.addParameter("adhereTOCsympt", "adhereTOCsympt", int.class, adhereTOCsympt, false);
		params.addParameter("adhereTOCasympt", "adhereTOCasympt", int.class, adhereTOCasympt, false);

		params.addParameter("realisticRandom", "realisticRandom", double.class, realisticRandom, false);
		params.addParameter("realisticTOC", "realisticTOC", double.class, realisticTOC, false);
		params.addParameter("realisticDST", "realisticDST", double.class, realisticDST, false);

		
		params.addParameter("transmissionMSM", "TransmissionMSM", double.class, transmissionMSM, false);
		params.addParameter("recovery_time", "RecoveryTime", double.class, recoveryTime, false);
		params.addParameter("prob_symptomatic_msm", "ProbSymptomaticMSM", double.class, probSymptomaticMSM, false);
		params.addParameter("screen_interval_mean_MSM", "ScreenIntervalMeanMSM", double.class, screenIntervalMeanMSM, false);
		params.addParameter("screen_interval_var_MSM", "ScreenIntervalVarMSM", double.class, screenIntervalVarMSM, false);

		params.addParameter("delay_to_seek_care_msm", "DelayToSeekCareMSM", double.class, delayToSeekCareMSM, false);
		params.addParameter("delay_to_retreatment_msm", "DelayToRetreatmentMSM", double.class, delayToRetreatmentMSM, false);
		params.addParameter("assortativity", "assortativity", double.class, assortativity, false);
		params.addParameter("activity_group_transfer_prop", "ActivityGroupTransferProp", double.class, activityGroupTransferProp, false);
		params.addParameter("activity_group_transmission_ratio", "ActivityGroupTransmissionRatio", double.class, activityGroupTransmissionRatio, false);


		params.addParameter("percent_resistant_A", "percent_resistant_A", double.class, percentResistantA, false);
		params.addParameter("begin_importing_B", "begin_importing_B", int.class, beginImportingB, false);
		params.addParameter("importing_B_interval", "importing_B_interval", double.class, importingBInterval, false);
		params.addParameter("prob_develop_resistance_A_exponent", "prob_develop_resistance_A_exponent", double.class, probDevelopResistanceAExponent, false);
		params.addParameter("prob_develop_resistance_B_exponent", "prob_develop_resistance_B_exponent", double.class, probDevelopResistanceBExponent, false);

		params.addParameter("DSTsensitivity", "DSTsensitivity,", double.class, DSTsensitivity, false);
		params.addParameter("DSTspecificity", "DSTspecificity,", double.class, DSTspecificity, false);
		
		params.addParameter("seed", "seed", int.class, seed, false);

		params.addParameter("population_size", "Pop Size", int.class, 100000, false);
		params.addParameter("infected_count_init", "Initial Infected", int.class, initialInfected, false);
		params.addParameter("propHighActivity", "propHighActivity", double.class, propHighActivity, false);
		params.addParameter("end_time", "EndTime", int.class, endTime, false);

	
		params.addParameter("care_cost", "care_cost", double.class, careCost, false);
		params.addParameter("test_cost", "test_cost", double.class, testCost, false);
		params.addParameter("strain_test_cost", "strain_test_cost", double.class, strainTestCost, false);
		params.addParameter("treatment_A_cost", "treatment_A_cost", double.class, treatmentACost, false);
		params.addParameter("treatment_B_cost", "treatment_B_cost", double.class, treatmentBCost, false);
		params.addParameter("treatment_X_cost", "treatment_X_cost", double.class, treatmentXCost, false);
		params.addParameter("treatment_E_cost", "treatment_E_cost", double.class, treatmentECost, false);

		params.addParameter("fitnessCostA", "fitnessCostA", double.class, fitnessCostA, false);
		params.addParameter("fitnessCostB", "fitnessCostB", double.class, fitnessCostB, false);

		// System.out.println(params.getSchema().parameterNames());

		return (Parameters) params;
	}
	
	public String makeExperimentDir() {
		LocalDate date = LocalDate.now();
		
		Month month = date.getMonth();
		int day = date.getDayOfMonth();
		int year = date.getYear();
		
		//String fullDate = month +"_"+ day +"_"+ year;
		
		// path root depends on machine. these are for my desktop and laptop respectively.
		//String root = "/usr/local/MSMoutput/";
		String root = "/Users/me597/Documents/MSMoutput/";

		//String dirname = root + "output_" + fullDate +"_debug_7_";
		
		//String dirname = root + "output_" + fullDate +"_overnight_";
		
		String dirname = root + "output_" + "MAY_09_2026_overnight_";
	
		//String dirname = "/Users/me597/Documents/MSMoutput/FEBRUARY_18_2026_overnight_";

		
		dirname += counterfactual;
		
		dirname += "_";
		
		dirname += resistance;
		
		dirname += "_";
		
		dirname += yearX;
		
		dirname += "_";
		
		dirname += String.valueOf(fitnessA);
		
		dirname += "_";
		
		dirname += String.valueOf(fitnessB);
		
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

		dirname += "_";
		
		dirname += String.valueOf(fitnessA);
		
		dirname += "_";
		
		dirname += String.valueOf(fitnessB);

		
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
        String combinedFile = (expdir + counterfactual + resistance + fitnessA + "_" + fitnessB + "_" + batchNumber + "combined.csv");
       
        try (BufferedWriter writer = Files.newBufferedWriter(Paths.get(combinedFile))){
        	boolean headerWritten = false;

        	List<Path> csvfiles = Files.list(Paths.get(dirpath)).
        			filter(p -> p.getFileName().toString().contains(counterfactual) && !p.getFileName().toString().contains("transmission")).
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
        						//System.out.println("Header written from" + csvFile);
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
        		
        		Files.delete(csvFile);
        	}
        } catch (IOException e) {
        		System.err.println("Error writing file: " + combinedFile.toString());
    			e.printStackTrace();
        	}

            
	        
			

            
        
	}
	
	
	
	public void analyzeAndCombineTPI(String dirpath) {
		 File directory = new File(dirpath);
	        
	        if (!directory.exists() || !directory.isDirectory()) {
	            throw new IllegalArgumentException("The specified path is not a valid directory: " + dirpath);
	        }
	        
	        
	     String combinedFile = (expdir + counterfactual + resistance + fitnessA + fitnessB + batchNumber + "combinedTPW.csv");
	     
	     try (BufferedWriter writer = Files.newBufferedWriter(Paths.get(combinedFile))){

	        	List<Path> csvfiles = Files.list(Paths.get(dirpath)).
	        			filter(p -> p.getFileName().toString().contains(counterfactual) && p.getFileName().toString().contains("transmission")).
	        			collect(Collectors.toList());
	        	
	        	if (csvfiles.isEmpty()) {
	        		throw new FileNotFoundException("No TPI files found in directory: " + dirpath);
	        	}
	        	
	        	String header = "source, Q1, median, Q3";
	        	writer.write(header);
	        	writer.newLine();

	        	for (Path csvFile : csvfiles) {
	        		//System.out.println("Reading from " + csvFile.getFileName());
	        		try (BufferedReader reader = Files.newBufferedReader(csvFile)){

	        			double q1value = 0;
	        			double medianvalue = 0;
	        			double q3value = 0;

	        			//collect the rates per each infection
	        			List<Double> transmissionsPerWeek = new ArrayList <Double>();

	        			String line;
	        			boolean isFirstLine = true;
	        			while ((line = reader.readLine()) != null) {
	        				if (isFirstLine == true) {
	        					isFirstLine = false;
	        					//skips first line that contains header info
	        				} else {
	        					line = line.replaceAll("\"", "");
	        					String[] values = line.split(",");
	        					//System.out.println(values[0]);
	        					//System.out.println(Double.parseDouble(values[0]));

	        					
	        					double countTransmissions = Double.parseDouble(values[0]);
	        					double duration = Double.parseDouble(values[1]);
	        					double rate = countTransmissions / duration;

	        					transmissionsPerWeek.add(rate);

	        				}

	        			}
	        			double[] TPIvalues = transmissionsPerWeek.stream().mapToDouble(Double::doubleValue).toArray();

	        			//System.out.println(TPIvalues);

	        			Percentile percentile = new Percentile();
	        			q1value = percentile.evaluate(TPIvalues, 25.0);
	        			medianvalue = percentile.evaluate(TPIvalues, 50.0);
	        			q3value = percentile.evaluate(TPIvalues, 75.0);

//	        			System.out.println("Q1:");
//	        			System.out.println(q1value);
//	        			System.out.println();
//
//	        			System.out.println("median");
//	        			System.out.println(medianvalue);
//	        			System.out.println();
//
	        			//System.out.println("Q3:");
	        			//System.out.println(q3value);
	        			//System.out.println();


	        			writer.write(csvFile.toString());
	        			writer.write(",");
	        			writer.write(String.valueOf(q1value));
	        			writer.write(",");
	        			writer.write(String.valueOf(medianvalue));
	        			writer.write(",");
	        			writer.write(String.valueOf(q3value));
	        			writer.newLine();

	        		} catch (IOException e){
	        			System.err.println("Error reading file: " + csvFile.toString());
	        			e.printStackTrace();
	        		}

	        		Files.delete(csvFile);
	        		
	        	}} catch (IOException e) {
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
	        String combinedFile = (expdir + counterfactual + resistance + fitnessA + fitnessB + "supercombined.csv");
	       
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
	        		Files.delete(csvFile);
	        	}
	        } catch (IOException e) {
	        		System.err.println("Error writing file: " + combinedFile.toString());
	    			e.printStackTrace();
	        	}

	        	
	        	
	
	
    System.out.println("All CSV files have been combined into " + combinedFile);

  
        
    }
}


