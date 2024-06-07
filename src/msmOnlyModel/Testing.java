/**
 * 
 */
package msmOnlyModel;

import cern.jet.random.Uniform;
import repast.simphony.parameter.Parameters;

/**
 * @author me597
 *
 */
public class Testing {
	private Indiv indiv;
	private Infection infection;
	private ThreadSafeRandomHelper randomHelper;
	private Parameters parameters;
	double sensitivity; //true positive rate. used for cases with actual resistance to determine the rate false negatives.
	double specificity; // true negative rate. used for susceptible cases to determine the rare false positives.
	private Uniform testsUniform;
	private String counterfactual;
	
	public Testing(Infection infection, Parameters parameters) {
		this.infection = infection;
		this.randomHelper = infection.getRandomHelper();
		this.testsUniform = (Uniform) randomHelper.getDistribution("testsUniform");
		this.parameters=parameters;
		this.counterfactual=parameters.getString("counterfactual");
		
		if (counterfactual.contains("GISP")) {
			this.specificity = 1.0;
			this.sensitivity = 1.0;
		} else {
			this.specificity = parameters.getDouble("DSTspecificity");
			this.sensitivity = parameters.getDouble("DSTsensitivity");
		}

	}
	
	public Testing(Indiv indiv) {
		this.indiv = indiv;
		this.parameters=indiv.getParameters();
		this.randomHelper = indiv.getRandomHelper();
		if (indiv.infectious()){
			this.infection = indiv.myInfection();
		}
		
		this.randomHelper = indiv.getRandomHelper();
		this.testsUniform = (Uniform) randomHelper.getDistribution("testsUniform");
		this.counterfactual=parameters.getString("counterfactual");

		if (counterfactual.contains("GISP")) {
			this.specificity = 1.0;
			this.sensitivity = 1.0;
		} else {
			this.specificity = parameters.getDouble("DSTspecificity");
			this.sensitivity = parameters.getDouble("DSTsensitivity");
		}
	}
	
	
	
	public boolean diagnosticTest() {
		//not using this currently
		boolean result = false;
		double sensitivity = 1.0;
		double specificity = 1.0;
		double randomValue = testsUniform.nextDouble();
		
		if (indiv.infectious()) {
			//false negative?
			if (randomValue <= sensitivity) {
				result = true;
			} else {
				result = false; //false negative
			}
		} else { //if NOT infectious...
			//false positive?
			if (randomValue <= specificity) {
				result = false;
			} else {
				result = true; //false positive
			}
		}
		
		return result;
	}
	
	public String drugSusceptibilityTest() {
		//this method should return the drug(s) that the infection is SUSCEPTIBLE to
		String result;
		String resultA = "A";
		String resultB = "B";
		double randomValueA = testsUniform.nextDouble();
		double randomValueB = testsUniform.nextDouble();
		String realStrain = infection.getStrain();

		//divide by actual strain status
		
		if (realStrain.equals("A")){ //if resists A, then sens for A and spec for B
			if (randomValueA > sensitivity) {
				resultA = "A"; //small chance that A will be incorrectly added back to sus list
			} else {resultA = "";}
			if (randomValueB > specificity) {
				resultB = "";
			}
			
		} else if (realStrain.equals("B")) { //if resists B, then spc for A and sens for B
			if (randomValueA > specificity) {
				resultA = ""; //small chance that A will be incorrectly removed from sus list
			}
			if (randomValueB > sensitivity) {
				resultB = "B";//small chance the B will be incorrectly added back to sus list
			} else {resultB = "";}
			
		} else if (realStrain.equals("Both")) { //sens for both
			if (randomValueA > sensitivity) {
				resultA = "A"; //small chance that A will be incorrectly removed from sus list
			} else { resultA = "";}
			if (randomValueB > sensitivity) {
				resultB = "B";
			} else {resultB = "";}
			
		} else { //if full susceptible, apply specificity for both A and B
			if (randomValueA > specificity) {
				resultA = ""; //small chance that A will be incorrectly removed from sus list
			}
			if (randomValueB > specificity) {
				resultB = "";
			}
		}
		
		result = resultA + resultB;
		if (result.equals("")) {
			result = "XE";
			}//if neither A nor B, then X or E
		return result;
	}
	
	public boolean drugSusceptibilityTestA(Infection infection) {
		boolean result = true;
		double sensitivity = 1.0;
		double specificity = 1.0;
		double randomValue = testsUniform.nextDouble();
		
		if (infection.susceptibleToA()) {
			if (randomValue <= sensitivity) {
				result = true;
			} else {
				result = false; //false negative
			}
		} else {
			if (randomValue <= specificity) {
				result = false;
			} else {
				result = true; //false positive
			}
		}
		
		return result;
	}
	
	public boolean drugSusceptibilityTestB(Infection infection) {
		boolean result = true;
		double sensitivity = 1.0;
		double specificity = 1.0;
		double randomValue = testsUniform.nextDouble();
		
		if (infection.susceptibleToB()) {
			if (randomValue <= sensitivity) {
				result = true;
			} else {
				result = false; //false negative
			}
		} else {
			if (randomValue <= specificity) {
				result = false;
			} else {
				result = true; //false positive
			}
		}
		
		return result;
	}

}
