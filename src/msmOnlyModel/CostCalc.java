/**
 * 
 */
package msmOnlyModel;

import repast.simphony.parameter.Parameters;

/**
 * @author me597
 *
 */
public class CostCalc {
	
	private Parameters parameters;

	private double careCost;
	private double testCost;
	private double strainTestCost;
	private double drugAtreatmentCost;
	private double drugBtreatmentCost;
	private double drugXtreatmentCost;
	private double drugEtreatmentCost;
	
	private double monetaryCostCumulative;
	private double monetaryCostAnnual;
	private double personDaysSymptomaticAnnual; //not using anymore
	private double QALYsLost;
	
	private double monetaryCostAnnualMSM;
	private double QALYsLostMSM;

	private double monetaryCostAnnualMSMW;
	private double QALYsLostMSMW;
	
	private double monetaryCostAnnualMSW;
	private double QALYsLostMSW;
	
	private double monetaryCostAnnualW;
	private double QALYsLostW;
	
	private double monetaryCostAnnualNB;
	private double QALYsLostNB;
	
	
	public CostCalc(Parameters parameters) {
		this.parameters = parameters;
		
		this.careCost = this.parameters.getDouble("care_cost");
		this.testCost = this.parameters.getDouble("test_cost");
		this.strainTestCost = this.parameters.getDouble("strain_test_cost");
		this.drugAtreatmentCost = this.parameters.getDouble("treatment_A_cost");
		this.drugBtreatmentCost = this.parameters.getDouble("treatment_B_cost");
		this.drugXtreatmentCost = this.parameters.getDouble("treatment_X_cost");
		this.drugEtreatmentCost = this.parameters.getDouble("treatment_E_cost");

		
		this.monetaryCostAnnual = 0.0;
		this.monetaryCostCumulative = 0.0;
		this.personDaysSymptomaticAnnual = 0.0;
		this.QALYsLost = 0.0;
	}
	
	public void addPersonDaysSymptomatic() {
		personDaysSymptomaticAnnual ++;
	}
	
	public void symptomaticQALYsLost(double delay) {
		double QALYs = (delay * 0.114) / 52.0;//GBD for moderate pelvic inflamatory diseases
		
		
		QALYsLost = QALYsLost + QALYs;
		
		
	}
	
	
	public void careCost(int count) {
		//System.out.println("Care");

		double convertedCount = (double) count;
		
		monetaryCostAnnual += (careCost * convertedCount);
		
		
	}
	
	public void testCost(int count) {
		//System.out.println("Dtest");
		double convertedCount = (double) count;


		monetaryCostAnnual += (testCost * convertedCount);
		
		
	}
	
	public void strainTestCost(int count) {
		//System.out.println("StrainTest");
		double convertedCount = (double) count;

		double totalCost = strainTestCost * convertedCount;
		
		monetaryCostAnnual += totalCost;
		
	}
	
	public void treatmentDrugACost(int count) {
		//System.out.println("DrugA");
		double convertedCount = (double) count;


		monetaryCostAnnual += (drugAtreatmentCost * convertedCount);
		
	}
	
	public void treatmentDrugBCost(int count) {
		//System.out.println("DrugB");

		double convertedCount = (double) count;

		monetaryCostAnnual += (drugBtreatmentCost * convertedCount);
		
	}
	
	public void treatmentDrugXCost(int count) {
		//System.out.println("DrugX");
		double convertedCount = (double) count;

		monetaryCostAnnual += (drugXtreatmentCost * convertedCount);
		
		
	}
	
	public void treatmentDrugECost(int count) {
		//System.out.println("DrugE");
		double convertedCount = (double) count;

		
		monetaryCostAnnual += (drugEtreatmentCost * convertedCount);
		double QALYs = (10.5 * 0.3) / 365.0;
		this.QALYsLost += QALYs;
		
		
	}
	
	
	public void recordSequelae(String sequelae) {
		//System.out.println("Sequelae");

		double QALYs = 0.0; 

		
		if (sequelae.equals("epididymitis")) {
			this.monetaryCostAnnual += 522.0;
			
			QALYs = (6.9 * 0.128) / 365.0;
			this.QALYsLost += QALYs;
			
			
		} else if (sequelae.equals("dgi")) {
			this.monetaryCostAnnual += 2916.0;
			QALYs = (8.8 * 0.37) / 365.0;
			this.QALYsLost += QALYs;
			
			
			
		} else if (sequelae.equals("both")) {
			this.monetaryCostAnnual += 3438.0;
			QALYs = ((6.9 + 8.8) * 0.7102) / 365.0;
			this.QALYsLost += QALYs;
			
		}
	
	}
	
	
	
	public void clearAnnualCosts() {
		monetaryCostAnnual = 0.0;
		personDaysSymptomaticAnnual = 0.0;
		QALYsLost = 0.0;
		
		monetaryCostAnnualW = 0.0;
		QALYsLostW = 0.0;
		
		monetaryCostAnnualNB = 0;
		QALYsLostNB = 0;
		monetaryCostAnnualMSM = 0;
		QALYsLostMSM = 0;
		monetaryCostAnnualMSMW = 0;
		QALYsLostMSMW = 0;
		monetaryCostAnnualMSW = 0;
		QALYsLostMSW = 0;
		
	}
	
	public double getMonetaryCost() {
		return monetaryCostAnnual;
	}
	
	public double getPersonDaysSymptomatic() {
		return personDaysSymptomaticAnnual;
	}
	
	public double getQALYsLost() {
		return QALYsLost;
	}



	
	
	public double getMonetaryCostMSM() {
		return monetaryCostAnnualMSM;
	}
	
	public double getQALYsLostMSM() {
		return QALYsLostMSM;
	}
	
	
	public double getMonetaryCostMSMW() {
		return monetaryCostAnnualMSMW;
	}
	
	public double getQALYsLostMSMW() {
		return QALYsLostMSMW;
	}
	
	public double getMonetaryCostMSW() {
		return monetaryCostAnnualMSW;
	}
	
	public double getQALYsLostMSW() {
		return QALYsLostMSW;
	}
	
	public double getMonetaryCostW() {
		return monetaryCostAnnualW;
	}
	
	public double getQALYsLostW() {
		return QALYsLostW;
	}
	
	public double getMonetaryCostNB() {
		return monetaryCostAnnualNB;
	}
	
	public double getQALYsLostNB() {
		return QALYsLostNB;
	}
	


}
