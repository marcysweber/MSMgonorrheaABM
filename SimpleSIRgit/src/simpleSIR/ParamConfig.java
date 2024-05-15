/**
 * 
 */
package simpleSIR;

/**
 * @author me597
 *
 */
public class ParamConfig {
	//a class for storing combos of parameter values

	private int batchNumber;
	private int seed;
	private String resistance;
	private String counterfactual;
	private int yearX;
	private int initialInfected;
	
	private double TransmissionMSM;
	private double TransmissionMSW;
	private double TransmissionF;
	
	private double RecoveryLambda;
	
	private double ProbSymptomaticMSM;
	private double ProbSymptomaticMSW;
	private double ProbSymptomaticF;
	
	private double ScreenIntervalMSM;
	private double ScreenIntervalMSW;
	private double ScreenIntervalW;
	
	private double delayToSeekCareMSM;
	private double delayToSeekCareMSW;
	private double delayToSeekCareF;

	private double delayToRetreatmentMSM;
	private double delayToRetreatmentMSW;
	private double delayToRetreatmentF;
	
	private double percentResistantA;
	private int beginImportingB;
	private double importingBInterval;
	private double DSTsensitivity;
	private double DSTspecificity;
	
	private double careCost;
	private double testCost;
	private double strainTestCost;
	private double treatmentACost;
	private double treatmentBCost;
	private double treatmentXCost;
	private double treatmentECost;

	
	public ParamConfig(
			int batchNumber, 
			double seed,
			String resistance,
			String counterfactual,
			int yearX,
			int initialInfected,
			double TransmissionMSM, 
			double TransmissionMSW, 
			double TransmissionF,
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
			double amountResistantA,
			int beginImportingB,
			double importingBInterval,
			double DSTsensitivity,
			double DSTspecificity,
			double careCost,
			double testCost,
			double strainTestCost,
			double treatmentACost,
			double treatmentBCost,
			double treatmentXCost,
			double treatmentECost) {
		
		this.batchNumber = batchNumber;
		this.seed = (int) seed;
		this.resistance = resistance;
		this.counterfactual = counterfactual;
		this.yearX = yearX;
		this.initialInfected = initialInfected;
		this.TransmissionMSM = TransmissionMSM;
		this.TransmissionMSW = TransmissionMSW;
		this.TransmissionF = TransmissionF;
		this.RecoveryLambda = RecoveryLambda;
		this.ProbSymptomaticMSM = ProbSymptomaticMSM;
		this.ProbSymptomaticMSW = ProbSymptomaticMSW;
		this.ProbSymptomaticF = ProbSymptomaticF;
		this.ScreenIntervalMSM = ScreenIntervalMSM;
		this.ScreenIntervalMSW = ScreenIntervalMSW;
		this.ScreenIntervalW = ScreenIntervalW;
		this.delayToSeekCareMSM = delayToSeekCareMSM;
		this.delayToSeekCareMSW = delayToSeekCareMSW;
		this.delayToSeekCareF = delayToSeekCareF;
		
		this.delayToRetreatmentMSM = delayToRetreatmentMSM;
		this.delayToRetreatmentMSW = delayToRetreatmentMSW;
		this.delayToRetreatmentF = delayToRetreatmentF;
		
		this.percentResistantA = amountResistantA;
		this.beginImportingB = beginImportingB;
		this.importingBInterval = importingBInterval;
		this.DSTsensitivity = DSTsensitivity;
		this.DSTspecificity = DSTspecificity;
		this.careCost = careCost;
		this.testCost = testCost;
		this.strainTestCost = strainTestCost;
		this.treatmentACost = treatmentACost;
		this.treatmentBCost = treatmentBCost;
		this.treatmentXCost = treatmentXCost;
		this.treatmentECost = treatmentECost;

	}
	
	public int batchNumber() {
		return this.batchNumber;
	}

	public int getSeed() {
		return this.seed;
	}
	
	public String getResistance() {
		return this.resistance;
	}
	
	public String getCounterfactual() {
		return this.counterfactual;
	}
	
	public int getYearX() {
		return this.yearX;
	}
	
	public int getInitialInfected() {
		return this.initialInfected;
	}
	
	public double getTransmissionMSM() {
		return this.TransmissionMSM;
	}
	
	public double getTransmissionMSW() {
		return this.TransmissionMSW;
	}

	public double getTransmissionF() {
		return this.TransmissionF;
	}
	
	public double getRecoveryLambda() {
		return this.RecoveryLambda;
	}
	
	public double getProbSymptomaticMSM() {
		return this.ProbSymptomaticMSM;
	}
	
	public double getProbSymptomaticMSW() {
		return this.ProbSymptomaticMSW;
	}

	public double getProbSymptomaticF() {
		return this.ProbSymptomaticF;
	}
	
	public double getScreenIntervalMSM() {
		return this.ScreenIntervalMSM;
	}
	
	public double getScreenIntervalMSW() {
		return this.ScreenIntervalMSW;
	}
	
	public double getScreenIntervalW() {
		return this.ScreenIntervalW;
	}
	
	
	public double getDelayToSeekCareMSM() {
		return this.delayToSeekCareMSM;
	}
	
	public double getDelayToSeekCareMSW() {
		return this.delayToSeekCareMSW;
	}
	
	public double getDelayToSeekCareF() {
		return this.delayToSeekCareF;
	}
	
	public double getDelayToRetreatmentMSM() {
		return this.delayToRetreatmentMSM;
	}
	
	public double getDelayToRetreatmentMSW() {
		return this.delayToRetreatmentMSW;
	}
	
	public double getDelayToRetreatmentF() {
		return this.delayToRetreatmentF;
	}
	
	public double getPercentResistantA() {
		return this.percentResistantA;
	}
	
	public int getBeginImportingB() {
		return this.beginImportingB;
	}
	
	public double getImportingBInterval() {
		return this.importingBInterval;
	}
	
	public double getDSTsensitivity() {
		return this.DSTsensitivity;
	}
	
	public double getDSTspecificity() {
		return this.DSTspecificity;
	}
	
	public double getcareCost() {
		return this.careCost;
	}
	
	public double getTestCost() {
		return this.testCost;
	}

	public double getstrainTestCost() {
		return this.strainTestCost;
	}


	public double getTreatmentACost() {
		return this.treatmentACost;
	}
	
	public double getTreatmentBCost() {
		return this.treatmentBCost;
	}
	
	public double getTreatmentXCost() {
		return this.treatmentXCost;
	}
	
	public double getTreatmentECost() {
		return this.treatmentECost;
	}
}
