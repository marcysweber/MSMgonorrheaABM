/**
 * 
 */
package simpleSIR;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.time.LocalDate;
import java.time.Month;
import java.time.MonthDay;

import au.com.bytecode.opencsv.CSVWriter;

/**
 * @author me597
 *
 *this is a class for writing to a 
 *
 */
public class CustomFileOutput {
	
	private String counterfactual;
	private String resistance;
	private int yearX;
	private int RunNumber;
	private int seed;
	
	
	private String outputDir;
	private String outputFileName;
	private boolean isTest;
	
	
	public CustomFileOutput(String batchDirPath, String counterfactual, String resistance, int yearX, int RunNumber, int seed) {
		this.outputDir = batchDirPath;
		this.counterfactual = counterfactual;
		this.resistance = resistance;
		this.yearX = yearX;
		this.RunNumber = RunNumber;
		this.seed = seed;
		
		this.outputFileName = generateFileName();
		this.isTest = false;

	}
	
	//use this init for tests
	public CustomFileOutput(boolean test) {
		this.outputDir = "/Users/me597/Documents/output/";
		this.outputFileName = generateFileName();
		this.isTest = test;
	}
	
	public String generateFileName() {
		
		LocalDate date = LocalDate.now();
		
		Month month = date.getMonth();
		int day = date.getDayOfMonth();
		int year = date.getYear();
		
		String fullDate = month +"_"+ day +"_"+ year;
		
		String filename = "SimpleSIR_custom_output_" + fullDate +"_1_";
		//String filename = "SimpleSIR_custom_output_" + fullDate +"_debug2_";

		//String filename = "SimpleSIR_custom_output_MAY_13_2024_overnight_";
		
		filename += counterfactual;
		
		filename += "_";
		
		filename += resistance;
		
		filename += "_";
		
		filename += String.valueOf(yearX);
		
		filename += "_";
		
		filename += String.valueOf(RunNumber);
		
		filename += "-";
		
		filename += String.valueOf(seed);
		
		filename += ".csv";
		
		return filename;
	}
	
	public void createOutputFile() {
		//should create a new csv with the specified filename
		
		File file = new File(outputDir + outputFileName);
        FileWriter outputfile;
		try {
			outputfile = new FileWriter(file, true);
	        CSVWriter writer = new CSVWriter(outputfile); 
	        
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
	        
	        writer.writeNext(header); 
	        
	        writer.close();
	  
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} 
	}
	
	public void addOutputRow(
			
			//general info and parameters
			int runNumber, 
			int seed,
			String counterfactual,
			int initialInfected,
			
			double transmissionMSM, 
			double transmissionMSW, 
			double transmissionF,
			
			double RecoveryLambda, 
			
			double ProbSymptomaticMSM,
			double ProbSymptomaticMSW,
			double ProbSymptomaticF,
			
			double ScreenIntervalMSM,
			double ScreenIntervalMSW,
			double ScreenIntervalW,

			double delayToSeekCareMSM,
			double delayToSeekCareMSW,
			double delayToSeekCareF,

			double delayToRetreatmentMSM,
			double delayToRetreatmentMSW,
			double delayToRetreatmentF,
			
			double percentResistantA,
			int beginImportingB,
			double importingBInterval,
			double DSTsensitivity,
			double DSTspecificity,
			
			double careCost,
			double testCost,
			double strainTestCost,
			double drugAtreatmentCost,
			double drugBtreatmentCost,
			double drugXtreatmentCost,
			double drugEtreatmentCost,
			
			double tick, 
			
			//whole pop output
			double Prevalence, 
			double Incidence, 
			double resistAIncidence,
			double resistBIncidence,
			double resistBothIncidence,
			double symptomProportion,
			double treatments,
			double failedTreatments, 
			int detected,
			int detectedAndSymptoms,
			int detectedThruScreen,
			int successTreatmentsA,
			int successTreatmentsB, 
			int successTreatmentsX,
			int attemptTreatmentsA,
			int attemptTreatmentsB,
			int attemptTreatmentsX,
			int usageE,
			
			double surveillanceResultA,
			double surveillanceResultB,
			double surveillanceResultBoth,
			boolean switchedToB,
			boolean switchedToX,
			double monetaryCost,
			double QALYcost,
			
			//MSM output
			double MSMpop,
			double prevMSM,
			double incMSM,
			double detectedIncMSM,
			double detectedAndSymptomsMSM,
			double resistAIncidenceMSM,
			double resistBIncidenceMSM,
			double resistBothIncidenceMSM,
//			int knownFailedTreatmentsMSM,
//			int knownFailedTreatmentsAMSM,
//			int knownFailedTreatmentsBMSM,
//			int knownFailedTreatmentsBothMSM,
			int successTreatmentsAMSM,
			int successTreatmentsBMSM, 
			int successTreatmentsXMSM,
			int attemptTreatmentsAMSM,
			int attemptTreatmentsBMSM,
			int attemptTreatmentsXMSM,
			int usageEMSM,
			double monetaryCostMSM,
			double QALYcostMSM,
			
			//MSMW output
			double MSMWpop,
			double prevMSMW,
			double incMSMW,
			double detectedIncMSMW,
			double detectedAndSymptomsMSMW,
			double resistAIncidenceMSMW,
			double resistBIncidenceMSMW,
			double resistBothIncidenceMSMW,
//			int knownFailedTreatmentsMSMW,
//			int knownFailedTreatmentsAMSMW,
//			int knownFailedTreatmentsBMSMW,
//			int knownFailedTreatmentsBothMSMW,
			int successTreatmentsAMSMW,
			int successTreatmentsBMSMW, 
			int successTreatmentsXMSMW,
			int attemptTreatmentsAMSMW,
			int attemptTreatmentsBMSMW,
			int attemptTreatmentsXMSMW,
			int usageEMSMW,
			double monetaryCostMSMW,
			double QALYcostMSMW,
			
			//MSW output
			double MSWpop,
			double prevMSW,
			double incMSW,
			double detectedIncMSW,
			double detectedAndSymptomsMSW,
			double resistAIncidenceMSW,
			double resistBIncidenceMSW,
			double resistBothIncidenceMSW,
//			int knownFailedTreatmentsMSW,
//			int knownFailedTreatmentsAMSW,
//			int knownFailedTreatmentsBMSW,
//			int knownFailedTreatmentsBothMSW,
			int successTreatmentsAMSW,
			int successTreatmentsBMSW, 
			int successTreatmentsXMSW,
			int attemptTreatmentsAMSW,
			int attemptTreatmentsBMSW,
			int attemptTreatmentsXMSW,
			int usageEMSW,
			double monetaryCostMSW,
			double QALYcostMSW,
			
			//W output
			double Wpop,
			double prevW,
			double incW,
			double detectedIncW,
			double detectedAndSymptomsW,
			double resistAIncidenceW,
			double resistBIncidenceW,
			double resistBothIncidenceW,
//			int knownFailedTreatmentsW,
//			int knownFailedTreatmentsAW,
//			int knownFailedTreatmentsBW,
//			int knownFailedTreatmentsBothW,
			int successTreatmentsAW,
			int successTreatmentsBW, 
			int successTreatmentsXW,
			int attemptTreatmentsAW,
			int attemptTreatmentsBW,
			int attemptTreatmentsXW,
			int usageEW,
			double monetaryCostW,
			double QALYcostW,
			
			//NB output
			double NBpop,
			double prevNB,
			double incNB,
			double detectedIncNB,
			double detectedAndSymptomsNB,
			double resistAIncidenceNB,
			double resistBIncidenceNB,
			double resistBothIncidenceNB,
			//int knownFailedTreatmentsNB,
			//int knownFailedTreatmentsANB,
			//int knownFailedTreatmentsBNB,
			//int knownFailedTreatmentsBothNB,
			int successTreatmentsANB,
			int successTreatmentsBNB, 
			int successTreatmentsXNB,
			int attemptTreatmentsANB,
			int attemptTreatmentsBNB,
			int attemptTreatmentsXNB,
			int usageENB,
			double monetaryCostNB,
			double QALYcostNB
			) {
		
		if (isTest == false) {
		
		File file = new File(outputDir + outputFileName);
        FileWriter outputfile;
		try {
			outputfile = new FileWriter(file, true);
	        CSVWriter writer = new CSVWriter(outputfile); 
	        
	        String[] newRow = { 
	        		String.valueOf(runNumber), 
	        		String.valueOf(seed),
	        		String.valueOf(counterfactual),
	        		String.valueOf(initialInfected),
	        		
	        		String.valueOf(transmissionMSM),
	        		String.valueOf(transmissionMSW),
	        		String.valueOf(transmissionF),

	        		String.valueOf(RecoveryLambda),
	        		
	        		String.valueOf(ProbSymptomaticMSM),
	        		String.valueOf(ProbSymptomaticMSW),
	        		String.valueOf(ProbSymptomaticF),

	        		String.valueOf(ScreenIntervalMSM),
	        		String.valueOf(ScreenIntervalMSW),
	        		String.valueOf(ScreenIntervalW),

	        		String.valueOf(delayToSeekCareMSM),
	        		String.valueOf(delayToSeekCareMSW),
	        		String.valueOf(delayToSeekCareF),

	        		String.valueOf(delayToRetreatmentMSM),
	        		String.valueOf(delayToRetreatmentMSW),
	        		String.valueOf(delayToRetreatmentF),
	        		
	        		String.valueOf(percentResistantA),
	        		String.valueOf(beginImportingB),
	        		String.valueOf(importingBInterval),
	        		String.valueOf(DSTsensitivity),
	        		String.valueOf(DSTspecificity),
	        		
	        		String.valueOf(careCost),
	        		String.valueOf(testCost),
	        		String.valueOf(strainTestCost),
	        		String.valueOf(drugAtreatmentCost),
	        		String.valueOf(drugBtreatmentCost),	 
	        		String.valueOf(drugXtreatmentCost),
	        		String.valueOf(drugEtreatmentCost),
	        		
	        		String.valueOf(tick), 
	        		String.valueOf(Prevalence),
	        		String.valueOf(Incidence),
	        		String.valueOf(resistAIncidence),
	        		String.valueOf(resistBIncidence),
	        		String.valueOf(resistBothIncidence),
	        		String.valueOf(symptomProportion),
	        		String.valueOf(treatments),
	        		String.valueOf(failedTreatments),
	        		String.valueOf(detected),
	        		String.valueOf(detectedAndSymptoms),
	        		String.valueOf(detectedThruScreen), 

	        		String.valueOf(successTreatmentsA),
	        		String.valueOf(successTreatmentsB),
	        		String.valueOf(successTreatmentsX),
	        		String.valueOf(attemptTreatmentsA),
	        		String.valueOf(attemptTreatmentsB),
	        		String.valueOf(attemptTreatmentsX),	
	        		String.valueOf(usageE),
	        		String.valueOf(surveillanceResultA),
	        		String.valueOf(surveillanceResultB),
	        		String.valueOf(surveillanceResultBoth),
	        		String.valueOf(switchedToB),
	        		String.valueOf(switchedToX),
	        		String.valueOf(monetaryCost),
	        		String.valueOf(QALYcost),
	        		
	        		//MSM
	        		String.valueOf(MSMpop),
	        		String.valueOf(prevMSM),
	        		String.valueOf(incMSM),
	        		String.valueOf(detectedIncMSM),
	        		String.valueOf(detectedAndSymptomsMSM),
	        		String.valueOf(resistAIncidenceMSM),
	        		String.valueOf(resistBIncidenceMSM),
	        		String.valueOf(resistBothIncidenceMSM),
//	        		String.valueOf(knownFailedTreatmentsMSM),
//	        		String.valueOf(knownFailedTreatmentsAMSM),
//	        		String.valueOf(knownFailedTreatmentsBMSM),
//	        		String.valueOf(knownFailedTreatmentsBothMSM),
	        		String.valueOf(successTreatmentsAMSM),
	        		String.valueOf(successTreatmentsBMSM),
	        		String.valueOf(successTreatmentsXMSM),
	        		String.valueOf(attemptTreatmentsAMSM),
	        		String.valueOf(attemptTreatmentsBMSM),
	        		String.valueOf(attemptTreatmentsXMSM),	
	        		String.valueOf(usageEMSM),
	        		String.valueOf(monetaryCostMSM),
	        		String.valueOf(QALYcostMSM),
	        
	        		//MSMW
	        		String.valueOf(MSMWpop),
	        		String.valueOf(prevMSMW),
	        		String.valueOf(incMSMW),
	        		String.valueOf(detectedIncMSMW),
	        		String.valueOf(detectedAndSymptomsMSMW),
	        		String.valueOf(resistAIncidenceMSMW),
	        		String.valueOf(resistBIncidenceMSMW),
	        		String.valueOf(resistBothIncidenceMSMW),
//	        		String.valueOf(knownFailedTreatmentsMSMW),
//	        		String.valueOf(knownFailedTreatmentsAMSMW),
//	        		String.valueOf(knownFailedTreatmentsBMSMW),
//	        		String.valueOf(knownFailedTreatmentsBothMSMW),
	        		String.valueOf(successTreatmentsAMSMW),
	        		String.valueOf(successTreatmentsBMSMW),
	        		String.valueOf(successTreatmentsXMSMW),
	        		String.valueOf(attemptTreatmentsAMSMW),
	        		String.valueOf(attemptTreatmentsBMSMW),
	        		String.valueOf(attemptTreatmentsXMSMW),	
	        		String.valueOf(usageEMSMW),
	        		String.valueOf(monetaryCostMSMW),
	        		String.valueOf(QALYcostMSMW),
	        		
	        		//MSW
	        		String.valueOf(MSWpop),
	        		String.valueOf(prevMSW),
	        		String.valueOf(incMSW),
	        		String.valueOf(detectedIncMSW),
	        		String.valueOf(detectedAndSymptomsMSW),
	        		String.valueOf(resistAIncidenceMSW),
	        		String.valueOf(resistBIncidenceMSW),
	        		String.valueOf(resistBothIncidenceMSW),
//	        		String.valueOf(knownFailedTreatmentsMSW),
//	        		String.valueOf(knownFailedTreatmentsAMSW),
//	        		String.valueOf(knownFailedTreatmentsBMSW),
//	        		String.valueOf(knownFailedTreatmentsBothMSW),
	        		String.valueOf(successTreatmentsAMSW),
	        		String.valueOf(successTreatmentsBMSW),
	        		String.valueOf(successTreatmentsXMSW),
	        		String.valueOf(attemptTreatmentsAMSW),
	        		String.valueOf(attemptTreatmentsBMSW),
	        		String.valueOf(attemptTreatmentsXMSW),	
	        		String.valueOf(usageEMSW),
	        		String.valueOf(monetaryCostMSW),
	        		String.valueOf(QALYcostMSW),
	        		
	        		//W
	        		String.valueOf(Wpop),
	        		String.valueOf(prevW),
	        		String.valueOf(incW),
	        		String.valueOf(detectedIncW),
	        		String.valueOf(detectedAndSymptomsW),
	        		String.valueOf(resistAIncidenceW),
	        		String.valueOf(resistBIncidenceW),
	        		String.valueOf(resistBothIncidenceW),
//	        		String.valueOf(knownFailedTreatmentsW),
//	        		String.valueOf(knownFailedTreatmentsAW),
//	        		String.valueOf(knownFailedTreatmentsBW),
//	        		String.valueOf(knownFailedTreatmentsBothW),
	        		String.valueOf(successTreatmentsAW),
	        		String.valueOf(successTreatmentsBW),
	        		String.valueOf(successTreatmentsXW),
	        		String.valueOf(attemptTreatmentsAW),
	        		String.valueOf(attemptTreatmentsBW),
	        		String.valueOf(attemptTreatmentsXW),	
	        		String.valueOf(usageEW),
	        		String.valueOf(monetaryCostW),
	        		String.valueOf(QALYcostW),
	        		
	        		//NB
	        		String.valueOf(NBpop),
	        		String.valueOf(prevNB),
	        		String.valueOf(incNB),
	        		String.valueOf(detectedIncNB),
	        		String.valueOf(detectedAndSymptomsNB),
	        		String.valueOf(resistAIncidenceNB),
	        		String.valueOf(resistBIncidenceNB),
	        		String.valueOf(resistBothIncidenceNB),
//	        		String.valueOf(knownFailedTreatmentsNB),
//	        		String.valueOf(knownFailedTreatmentsANB),
//	        		String.valueOf(knownFailedTreatmentsBNB),
//	        		String.valueOf(knownFailedTreatmentsBothNB),
	        		String.valueOf(successTreatmentsANB),
	        		String.valueOf(successTreatmentsBNB),
	        		String.valueOf(successTreatmentsXNB),
	        		String.valueOf(attemptTreatmentsANB),
	        		String.valueOf(attemptTreatmentsBNB),
	        		String.valueOf(attemptTreatmentsXNB),	
	        		String.valueOf(usageENB),
	        		String.valueOf(monetaryCostNB),
	        		String.valueOf(QALYcostNB)

	        
	        }; 
	        
	        writer.writeNext(newRow); 
	        
	        writer.close();
	  
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} 
	}
	}
	

}
