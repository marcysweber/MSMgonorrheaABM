/**
 * 
 */
package msmOnlyModel;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.time.LocalDate;
import java.time.Month;
import java.util.List;

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
	private String outputTransmissionFileName;


	public CustomFileOutput(String batchDirPath, String counterfactual, String resistance, int yearX, int RunNumber, int seed) {
		this.outputDir = batchDirPath;
		this.counterfactual = counterfactual;
		this.resistance = resistance;
		this.yearX = yearX;
		this.RunNumber = RunNumber;
		this.seed = seed;

		String fileStub = generateFileName();
		this.outputFileName = fileStub + ".csv";
		this.outputTransmissionFileName = fileStub + "transmission.csv";

		this.isTest = false;

	}

	//use this init for tests
	public CustomFileOutput(boolean test) {
		this.outputDir = "/Users/me597/Documents/output/";
		this.outputFileName = generateFileName();
		this.isTest = test;
	}

	public String generateFileName() {
//
//		LocalDate date = LocalDate.now();
//
//		Month month = date.getMonth();
//		int day = date.getDayOfMonth();
//		int year = date.getYear();
//
//		String fullDate = month +"_"+ day +"_"+ year;

		//String filename = "MSMonly_output_" + fullDate +"_1_";
		//String filename = "MSMonly_output_" + fullDate +"_debug_2_";

		//String filename = "MSMonly_output_JANUARY_10_2025_overnight_";

		//String filename = fullDate;

		//filename += "_";

		String filename = counterfactual;

		filename += "_";

		filename += resistance;

		filename += "_";

		filename += String.valueOf(yearX);

		filename += "_";

		filename += String.valueOf(RunNumber);

		filename += "-";

		filename += String.valueOf(seed);

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
					"propHighActivity",

					"TransmissionMSM",


					"NaturalRecoveryTime",

					"ProbSymptomaticMSM",


					"ScreenIntervalMeanMSM",
					"ScreenIntervalVarMSM",


					"DelayToSeekCareMSM",

					"DelayToRetreatmentMSM",

					"Assortativity",
					"activityGroupTransferProp",
					"activityGroupTransmissionRatio",

					"PercentResistantA",
					"BeginImportingB",
					"ImportingBInterval",
					"ProbDevelopResistanceAExponent",
					"ProbDevelopResistanceBExponent",
					
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
					"DetectedResistA",
					"DetectedResistB",
					"DetectedResistBoth",

					
					"StrainTests",
					"DiagnosticTests",
					"VisitsToClinic",

					"SuccessTreatmentsA",
					"SuccessTreatmentsB",
					"SuccessTreatmentsX",
					"SuccessTreatmentsAandB",
					"AttemptTreatmentsA",
					"AttemptTreatmentsB",
					"AttemptTreatmentsX",
					"UsageofErtapenem",
					"RecoveredNaturally",
					"RecoveredNaturallyDuringTreatment",
					"Reinfected",
					"ReinfectedDuringTreatment",
					"CasesEpidydimitis",
					"CasesDGI",
					"CasesBothSequelae",
					"CheckForSequelae",
					"CheckForSequelaeUnDetect",
					"CheckForSequelaeRecovNat",
					"CheckForSequelaeFailedA",
					"CheckForSequelaeFailedB",

					
					
					"SurveillanceEstPropResistA",
					"SurveillanceEstPropResistB",
					"SurveillanceEstPropResistBoth",
					"RemovedA",
					"RemovedB",
					"RemovedAandB",
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
		
//		File file2 = new File(outputDir + outputTransmissionFileName);
//		FileWriter outputfile2;
//		
//		try {
//			outputfile2 = new FileWriter(file2, true);
//			CSVWriter writer2 = new CSVWriter(outputfile2); 
//			
//			String[] header2 = { 
//					"CountTransmissions", 
//					"InfectionDuration",
//					"RiskGroup"};
//			writer2.writeNext(header2);
//			writer2.close();
//
//		} catch (IOException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		} 
		
		
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


			double RecoveryTime, 

			double ProbSymptomaticMSM,

			double ScreenIntervalMeanMSM,
			double ScreenIntervalVarMSM,


			double delayToSeekCareMSM,


			double delayToRetreatmentMSM,

			double assortativity,
			double riskGroupTransferProp,
			double riskGroupTransmissionRatio,


			double percentResistantA,
			int beginImportingB,
			double importingBInterval,
			double probDevelopResistanceAExponent,
			double probDevelopResistanceBExponent,
			
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
			int Incidence, 
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
			
			int detectedResistA,
			int detectedResistB,
			int detectedResistBoth,
			
			int strainTests,
			int diagnosticTests,
			int visitsToClinic,
			
			
			
			int successTreatmentsA,
			int successTreatmentsB, 
			int successTreatmentsX,
			int successTreatmentsAandB,
			int attemptTreatmentsA,
			int attemptTreatmentsB,
			int attemptTreatmentsX,
			int usageE,
			int recoveredNaturally,
			int recoveredNaturallyDuringTreatment,
			int reinfected,
			int reinfectedDuringTreatment,
			int casesEpi,
			int casesDGI,
			int casesBothSequelae,
			int checksForSequelae,
			
			int checksForSequelaeUnDetect,
			int checksForSequelaeRecovNat,
			int checksForSequelaeFailedA,
			int checksForSequelaeFailedB,

			

			double surveillanceResultA,
			double surveillanceResultB,
			double surveillanceResultBoth,
			boolean removedA,
			boolean removedB,
			boolean removedAandB,
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

						String.valueOf(RecoveryTime),

						String.valueOf(ProbSymptomaticMSM),

						String.valueOf(ScreenIntervalMeanMSM),
						String.valueOf(ScreenIntervalVarMSM),


						String.valueOf(delayToSeekCareMSM),

						String.valueOf(delayToRetreatmentMSM),
						String.valueOf(assortativity),

						String.valueOf(riskGroupTransferProp),
						String.valueOf(riskGroupTransmissionRatio),


						String.valueOf(percentResistantA),
						String.valueOf(beginImportingB),
						String.valueOf(importingBInterval),
						String.valueOf(probDevelopResistanceAExponent),
						String.valueOf(probDevelopResistanceBExponent),
						
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
						String.valueOf(detectedResistA),
						String.valueOf(detectedResistB),
						String.valueOf(detectedResistBoth),

						String.valueOf(strainTests),
						String.valueOf(diagnosticTests),
						String.valueOf(visitsToClinic),

						
						String.valueOf(successTreatmentsA),
						String.valueOf(successTreatmentsB),
						String.valueOf(successTreatmentsX),
						String.valueOf(successTreatmentsAandB),
						String.valueOf(attemptTreatmentsA),
						String.valueOf(attemptTreatmentsB),
						String.valueOf(attemptTreatmentsX),	
						String.valueOf(usageE),
						String.valueOf(recoveredNaturally),
						String.valueOf(recoveredNaturallyDuringTreatment),
						String.valueOf(reinfected),
						String.valueOf(reinfectedDuringTreatment),
						String.valueOf(casesEpi),
						String.valueOf(casesDGI),
						String.valueOf(casesBothSequelae),
						String.valueOf(checksForSequelae),
						String.valueOf(checksForSequelaeUnDetect),
						String.valueOf(checksForSequelaeRecovNat),
						String.valueOf(checksForSequelaeFailedA),
						String.valueOf(checksForSequelaeFailedB),


						String.valueOf(surveillanceResultA),
						String.valueOf(surveillanceResultB),
						String.valueOf(surveillanceResultBoth),
						String.valueOf(removedA),
						String.valueOf(removedB),
						String.valueOf(removedAandB),
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

	public void transmissionRateOutput(List<Infection> infectionList) {
		File file = new File(outputDir + outputTransmissionFileName);
		FileWriter outputfile;
		
		try {
			outputfile = new FileWriter(file, true);
			CSVWriter writer = new CSVWriter(outputfile); 


			for (Infection inf : infectionList) {
				int count = inf.transmissionEvents();
				double dur = inf.duration();
				String riskGroup = inf.getRiskGroup();

				String[] newRow = { 
						String.valueOf(count), 
						String.valueOf(dur),
						riskGroup};
				
				writer.writeNext(newRow);
			}
			
			writer.close();
			
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} 
	}


}
