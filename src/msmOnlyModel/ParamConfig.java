/**
 * 
 */
package msmOnlyModel;

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
	
	private double propHighRisk;
	
	private double TransmissionMSM;

	
	private double RecoveryLambda;
	
	private double ProbSymptomaticMSM;

	
	private double ScreenIntervalMSM;
	
	
	private double delayToSeekCareMSM;
	

	private double delayToRetreatmentMSM;
	
	private double riskGroupTransferProp;
	private double riskGroupTransmissionRatio;

	
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
			double propHighRisk,
			double TransmissionMSM, 
		
			double RecoveryLambda, 
			double ProbSymptomaticMSM, 

			double ScreenIntervalMSM,
		
			double delayToSeekCareMSM,

			double delayToRetreatmentMSM,
			double riskGroupTransferProp,
			double riskGroupTransmissionRatio,

			
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
		this.propHighRisk = propHighRisk;
		this.TransmissionMSM = TransmissionMSM;

		this.RecoveryLambda = RecoveryLambda;
		this.ProbSymptomaticMSM = ProbSymptomaticMSM;
	
		this.ScreenIntervalMSM = ScreenIntervalMSM;
		
		this.delayToSeekCareMSM = delayToSeekCareMSM;
		
		
		this.delayToRetreatmentMSM = delayToRetreatmentMSM;
		
		this.riskGroupTransferProp = riskGroupTransferProp;
		this.riskGroupTransmissionRatio = riskGroupTransmissionRatio;
		
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
	
	public double getPropHighRisk() {
		return this.propHighRisk;
	}
	
	public double getTransmissionMSM() {
		return this.TransmissionMSM;
	}
	
	public double getRecoveryLambda() {
		return this.RecoveryLambda;
	}
	
	public double getProbSymptomaticMSM() {
		return this.ProbSymptomaticMSM;
	}
	
	
	public double getScreenIntervalMSM() {
		return this.ScreenIntervalMSM;
	}
	
	public double getDelayToSeekCareMSM() {
		return this.delayToSeekCareMSM;
	}
	
	public double getDelayToRetreatmentMSM() {
		return this.delayToRetreatmentMSM;
	}
	
	public double getRiskGroupTransferProp() {
		return this.riskGroupTransferProp;
	}
	
	public double getRiskGroupTransmissionRatio() {
		return this.riskGroupTransmissionRatio;
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
