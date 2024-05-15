/**
 * 
 */
package simpleSIR;

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

		
		this.monetaryCostAnnual = 0;
		this.monetaryCostCumulative = 0;
		this.personDaysSymptomaticAnnual = 0;
		this.QALYsLost = 0;
	}
	
	public void addPersonDaysSymptomatic() {
		personDaysSymptomaticAnnual ++;
	}
	
	public void addQALYsLost(Indiv indiv, double delay) {
		double QALYs = (delay * 0.1) / 52.0;
		
		QALYsLost = QALYsLost + QALYs;
		
		if (indiv.getGender().equals("f")) {
			//W
			this.QALYsLostW+= QALYs;
		} else if (indiv.getGender().equals("nb")) {
			//NB
			this.QALYsLostNB+= QALYs;
		} else if (indiv.getSubPop().equals("msw")) {
			//MSW
			this.QALYsLostMSW+= QALYs;
		} else if (indiv.getSubPop().equals("msmw")) {
			//MSMW
			this.QALYsLostMSMW+= QALYs;
		} else {
			//MSM
			this.QALYsLostMSM+= QALYs;
		}
		
		
	}
	
	
	public void careCost(Indiv indiv) {
		monetaryCostAnnual += careCost;
		
		if (indiv.getGender().equals("f")) {
			//W
			this.monetaryCostAnnualW+= careCost;
		} else if (indiv.getGender().equals("nb")) {
			//NB
			this.monetaryCostAnnualNB+= careCost;
		} else if (indiv.getSubPop().equals("msw")) {
			//MSW
			this.monetaryCostAnnualMSW+= careCost;
		} else if (indiv.getSubPop().equals("msmw")) {
			//MSMW
			this.monetaryCostAnnualMSMW+= careCost;
		} else {
			//MSM
			this.monetaryCostAnnualMSM+= careCost;
		}
	}
	
	public void testCost(Indiv indiv) {
		monetaryCostAnnual += testCost;
		
		if (indiv.getGender().equals("f")) {
			//W
			this.monetaryCostAnnualW+= testCost;
		} else if (indiv.getGender().equals("nb")) {
			//NB
			this.monetaryCostAnnualNB+= testCost;
		} else if (indiv.getSubPop().equals("msw")) {
			//MSW
			this.monetaryCostAnnualMSW+= testCost;
		} else if (indiv.getSubPop().equals("msmw")) {
			//MSMW
			this.monetaryCostAnnualMSMW+= testCost;
		} else {
			//MSM
			this.monetaryCostAnnualMSM+= testCost;
		}
	}
	
	public void strainTestCost(int count) {
		double totalCost = strainTestCost * count;
		
		monetaryCostAnnual += totalCost;
		
	}
	
	public void treatmentDrugACost(Indiv indiv) {
		monetaryCostAnnual += drugAtreatmentCost;
		if (indiv.getGender().equals("f")) {
			//W
			this.monetaryCostAnnualW+= drugAtreatmentCost;
		} else if (indiv.getGender().equals("nb")) {
			//NB
			this.monetaryCostAnnualNB+= drugAtreatmentCost;
		} else if (indiv.getSubPop().equals("msw")) {
			//MSW
			this.monetaryCostAnnualMSW+= drugAtreatmentCost;
		} else if (indiv.getSubPop().equals("msmw")) {
			//MSMW
			this.monetaryCostAnnualMSMW+= drugAtreatmentCost;
		} else {
			//MSM
			this.monetaryCostAnnualMSM+= drugAtreatmentCost;
		}
	}
	
	public void treatmentDrugBCost(Indiv indiv) {
		monetaryCostAnnual += drugBtreatmentCost;
		if (indiv.getGender().equals("f")) {
			//W
			this.monetaryCostAnnualW+= drugBtreatmentCost;
		} else if (indiv.getGender().equals("nb")) {
			//NB
			this.monetaryCostAnnualNB+= drugBtreatmentCost;
		} else if (indiv.getSubPop().equals("msw")) {
			//MSW
			this.monetaryCostAnnualMSW+= drugBtreatmentCost;
		} else if (indiv.getSubPop().equals("msmw")) {
			//MSMW
			this.monetaryCostAnnualMSMW+= drugBtreatmentCost;
		} else {
			//MSM
			this.monetaryCostAnnualMSM+= drugBtreatmentCost;
		}
	}
	
	public void treatmentDrugXCost(Indiv indiv) {
		monetaryCostAnnual += drugXtreatmentCost;
		
		if (indiv.getGender().equals("f")) {
			//W
			this.monetaryCostAnnualW+= drugXtreatmentCost;
		} else if (indiv.getGender().equals("nb")) {
			//NB
			this.monetaryCostAnnualNB+= drugXtreatmentCost;
		} else if (indiv.getSubPop().equals("msw")) {
			//MSW
			this.monetaryCostAnnualMSW+= drugXtreatmentCost;
		} else if (indiv.getSubPop().equals("msmw")) {
			//MSMW
			this.monetaryCostAnnualMSMW+= drugXtreatmentCost;
		} else {
			//MSM
			this.monetaryCostAnnualMSM+= drugXtreatmentCost;
		}
	}
	
	public void treatmentDrugECost(Indiv indiv) {
		monetaryCostAnnual += drugEtreatmentCost;
		
		if (indiv.getGender().equals("f")) {
			//W
			this.monetaryCostAnnualW+= drugEtreatmentCost;
		} else if (indiv.getGender().equals("nb")) {
			//NB
			this.monetaryCostAnnualNB+= drugEtreatmentCost;
		} else if (indiv.getSubPop().equals("msw")) {
			//MSW
			this.monetaryCostAnnualMSW+= drugEtreatmentCost;
		} else if (indiv.getSubPop().equals("msmw")) {
			//MSMW
			this.monetaryCostAnnualMSMW+= drugEtreatmentCost;
		} else {
			//MSM
			this.monetaryCostAnnualMSM+= drugEtreatmentCost;
		}
	}
	
	public void clearAnnualCosts() {
		monetaryCostAnnual = 0;
		personDaysSymptomaticAnnual = 0;
		QALYsLost = 0;
		
		monetaryCostAnnualW = 0;
		QALYsLostW = 0;
		
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
