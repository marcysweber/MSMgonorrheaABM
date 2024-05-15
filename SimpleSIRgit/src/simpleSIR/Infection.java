/**
 * 
 */
package simpleSIR;

import cern.jet.random.Uniform;
import repast.simphony.parameter.Parameters;

/**
 * @author me597
 *
 */
public class Infection {
	private Parameters parameters;
	private boolean resistanceToA;
	private boolean resistanceToB;
	private String strain;
	private boolean current;
	private String subPop; //gender of host, affects symptomology
	private boolean symptoms;
	private boolean starting; 
	private double naturalRecoveryTime;
	private ThreadSafeRandomHelper randomHelper;
	
	
	public Infection(Parameters parameters, String strain, boolean starting, double naturalRecoveryTime, String subPop, ThreadSafeRandomHelper randomHelper) {//different for start of sim
		this.strain = strain;
		this.subPop = subPop;
		this.current = true; 
		if (strain.equals("A")) {
			this.resistanceToA = true;
			this.resistanceToB = false;
		} else if (strain.equals("B")) {
			this.resistanceToA = false;
			this.resistanceToB = true;
		} else if (strain.equals("Both")) {
			this.resistanceToA = true;
			this.resistanceToB = true;
		} else {
			this.resistanceToA = false;
			this.resistanceToB = false;
		}
		this.parameters = parameters;
		this.starting = starting;
		this.randomHelper = randomHelper;

		this.symptoms = assignSymptoms(starting);
		
		this.naturalRecoveryTime = naturalRecoveryTime;
	}
	
	private boolean assignSymptoms(boolean starting) {
		boolean bool = false;
		
		double prob = 0.1;
		if (!starting) {
			if (subPop.contains("f")) {
				prob = parameters.getDouble("prob_symptomatic_f");
			} else if (subPop.equals("msm")){
				prob = parameters.getDouble("prob_symptomatic_msm");
			} else if (subPop.equals("msw")) {
				prob = parameters.getDouble("prob_symptomatic_msw");
			}
		}
		Uniform symptomUniform = (Uniform) randomHelper.getDistribution("symptomaticUniform");
		double randomValue = symptomUniform.nextDouble();
		
		if (randomValue < prob) {
			bool = true;
		}
		
		return bool;
	}
	

	
	public String checkForDoubleResist(String transmittedStrain) {
		String oldStrain = this.strain;
		String newStrain = transmittedStrain;

		if (!transmittedStrain.equals("none")) { //if transmitted strain is none, ignore
			if (!oldStrain.equals("none")) { // if old strain is none, ignore
				if (!oldStrain.equals(newStrain)) { //if strains are same, ignore
					newStrain = "Both";
				}
			}
		}


		return newStrain;
	}
	
	
	public void recoverInfection() {
		this.current = false;
	}
	
	public void overrideInfection() {
		this.current = false;
	}
	
	public String getStrain() {
		return this.strain;
	}
	
	public boolean symptoms() {
		//returns true if symptomatic
		//returns false if asymptomatic
		return this.symptoms;
	}
	
	public double naturalRecoveryTime() {
		return this.naturalRecoveryTime;
	}
	
	public boolean resistantToA() {
		return resistanceToA;
	}
	
	public boolean resistantToB() {
		return resistanceToB;
	}
	
	public boolean susceptibleToA() {
		return !resistanceToA;
	}
	
	public boolean susceptibleToB() {
		return !resistanceToB;
	}
	
	public boolean checkIfCurrent() {
		return this.current;
	}
	
	public String getHostGender() {
		return this.subPop;
	}
	
	public ThreadSafeRandomHelper getRandomHelper() {
		return randomHelper;
	}
	

}
