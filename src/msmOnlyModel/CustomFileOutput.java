/**
 * 
 */
package msmOnlyModel;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.time.LocalDate;
import java.time.Month;

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
		
		//String filename = "MSMonly_output_" + fullDate +"_1_";
		//String filename = "MSMonly_output_" + fullDate +"_debug_2_";

		//String filename = "MSMonly_output_JANUARY_10_2025_overnight_";
		
		String filename = fullDate;
		
		filename += "_";
		
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
	        		"yearX",
	        		
	        		"switchThreshold",
	        		"availrDST",
	        		"adhereTOCsympt",
	        		"adhereTOCasympt",
	        		
	        		"realisticRandom",
	        		"realisticTOC",
	        		"realisticDST",

	        		
	        		
	        		"InitialInfected",
	        		"propHighRisk",
	        		
	        		"TransmissionMSM",
	        		
	        		
	        		"RecoveryLambda",
	        		
	        		"ProbSymptomaticMSM",
	        	
	        		
	        		"ScreenIntervalMSM",
	        		

	        		
	        		"DelayToSeekCareMSM",

	        		"DelayToRetreatmentMSM",
	        		
	        		"Assortativity",
	        		"riskGroupTransferProp",
	        		"riskGroupTransmissionRatio",

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
	        		"UnknownFailedTreatments",
	        		"DevelopedResistance",
	        		"Detected",
	        		"DetectedAndSymptoms",
	        		"DetectedThruScreen",

	        		"SuccessTreatmentsA",
	        		"SuccessTreatmentsB",
	        		"SuccessTreatmentsX",
	        		"AttemptTreatmentsA",
	        		"AttemptTreatmentsB",
	        		"AttemptTreatmentsX",
	        		"UsageofErtapenem",
	        		"OngoingTreatments",
	        		"RecoveredNaturallyDuringTreatment",
	        		"SurveillanceEstPropResistA",
	        		"SurveillanceEstPropResistB",
	        		"SurveillanceEstPropResistBoth",
	        		"SwitchedToB",
	        		"SwitchedToX",
	        		"AnnualMonetaryCost",
	        		"AnnualQALYsLost",
	        		"LowRiskPrev",
	        		"HighRiskPrev",
	        		"CountHighRisk"
	        		
	        		
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
			int yearX,
			
			double switchThreshold, 
			int availrDST,
			int adhereTOCsympt,
			int adhereTOCasympt,
			
			double realisticRandom,
			double realisticTOC,
			double realisticDST,

			
			int initialInfected,
			double propHighRisk,
			
			double transmissionMSM, 
			
			
			double RecoveryLambda, 
			
			double ProbSymptomaticMSM,
	
			double ScreenIntervalMSM,
		

			double delayToSeekCareMSM,
	

			double delayToRetreatmentMSM,
			
			double assortativity,
			double riskGroupTransferProp,
			double riskGroupTransmissionRatio,
			
			
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
			int developedResistance,
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
			int ongoingTreatments,
    		int recoveredNaturallyDuringTreatment,
			
			double surveillanceResultA,
			double surveillanceResultB,
			double surveillanceResultBoth,
			boolean switchedToB,
			boolean switchedToX,
			double monetaryCost,
			double QALYcost,
			double lowRiskPrev,
			double highRiskPrev,
			int countHighRisk
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
	        		String.valueOf(yearX),
	        		
	        		String.valueOf(switchThreshold),
	        		String.valueOf(availrDST),
	        		String.valueOf(adhereTOCsympt),
	        		String.valueOf(adhereTOCasympt),

	        		String.valueOf(realisticRandom),
	        		String.valueOf(realisticTOC),
	        		String.valueOf(realisticDST),
	        		
	        		String.valueOf(initialInfected),
	        		String.valueOf(propHighRisk),
	        		
	        		String.valueOf(transmissionMSM),
	        		
	        		String.valueOf(RecoveryLambda),
	        		
	        		String.valueOf(ProbSymptomaticMSM),
	        		
	        		String.valueOf(ScreenIntervalMSM),
	        		
	        		String.valueOf(delayToSeekCareMSM),
	        		
	        		String.valueOf(delayToRetreatmentMSM),
	        		String.valueOf(assortativity),
	        		
	        		String.valueOf(riskGroupTransferProp),
	        		String.valueOf(riskGroupTransmissionRatio),

	        		
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
	        		String.valueOf(developedResistance),
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
	        		String.valueOf(ongoingTreatments),
	        		String.valueOf(recoveredNaturallyDuringTreatment),

	        		
	        		String.valueOf(surveillanceResultA),
	        		String.valueOf(surveillanceResultB),
	        		String.valueOf(surveillanceResultBoth),
	        		String.valueOf(switchedToB),
	        		String.valueOf(switchedToX),
	        		String.valueOf(monetaryCost),
	        		String.valueOf(QALYcost),
	        		String.valueOf(lowRiskPrev),
	        		String.valueOf(highRiskPrev),
	        		String.valueOf(countHighRisk)
	        		

	        
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
