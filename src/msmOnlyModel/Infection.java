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
public class Infection {
	private Parameters parameters;
	private Indiv host;
	private int transmissionEvents;
	private boolean detected;
	private boolean soughtCare;
	private boolean resistanceToA;
	private boolean resistanceToB;
	private String strain;
	private boolean current;
	private String subPop; //gender of host, affects symptomology
	private boolean symptoms;
	private boolean screened;
	private boolean starting; 
	private double naturalRecoveryTime;
	private ThreadSafeRandomHelper randomHelper;
	
	//treatment history
	private boolean inTreatment;
	private boolean attemptedA;
	private boolean attemptedB;
	private boolean developedResistanceA;
	private boolean developedResistanceB;
	private boolean failedTreatment;
	
	//final outcome; all possible infection end-points mutually exclusive
	private boolean developedResistance;
	private boolean reInfected;
	private boolean succeededA;
	private boolean succeededB;
	private boolean succeededX;
	private boolean succeededE;
	private boolean recoveredNaturally;


	
	public Infection(Indiv host, Parameters parameters, String strain, boolean starting, double naturalRecoveryTime, String subPop, ThreadSafeRandomHelper randomHelper) {//different for start of sim
		this.host = host;
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
		
		this.detected = false;
		
		transmissionEvents = 0;
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
	
	
	public void ceaseInfection() {
		Observer obs = host.getObserver();
		
		try {
			obs.processCompleteInfection(this);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		this.current = false;
	}
	
	public void recordTransmission() {
		transmissionEvents++;
		
	}
	
	public int transmissionEvents() {
		return transmissionEvents;
	}
	
	public void overrideInfection() {
		reInfect();
		ceaseInfection();
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
	
	public boolean isDetected() {
		return detected;
	}
	
	public void detect() {
		detected = true;
	}
	
	public void undetect() {
		detected = false;
	}
	
	public void screen() {
		screened=true;
	}
	
	public boolean screened() {
		return screened;
	}
	
	public void seekCare() {
		soughtCare = true;
	}
	
	public boolean soughtCare() {
		return soughtCare;
	}
	
	public void attemptA() {
		attemptedA = true;
	}

	public void attemptB() {
		attemptedB = true;
	}
	
	public void recordInTreatment() {
		inTreatment = true;
	}
	
	public boolean inTreatment() {
		return inTreatment;
	}
	
	public void failTreatment() {
		failedTreatment = true;
		inTreatment = false;
		undetect();
	}
	
	public boolean failedTreatment() {
		return failedTreatment;
	}
	
	public void developResistance() {
		developedResistance = true;
	}
	
	public boolean developedResistance() {
		return developedResistance;
	}
	
	public void reInfect() {
		reInfected = true;
	}
	
	public boolean reInfected() {
		return reInfected;
	}
	
	public void recoverNaturally() {
		recoveredNaturally = true;
	}

	public boolean recoveredNaturally() {
		return recoveredNaturally;
	}
	
	public void successfulTreatment(String treatment) throws Exception {
		if (treatment.contains("A")) {
			succeededA = true;
		} else if (treatment.contains("B")) {
			succeededB = true;
		} else if (treatment.contains("X")) {
			succeededX = true;
		} else if (treatment.contains("E")) {
			succeededE = true;
		} else {
			throw new Exception("Invalid treatment recorded!");

		}
	}
	
	public boolean attemptedA() {
		return attemptedA;
	}
	
	public boolean attemptedB() {
		return attemptedB;
	}
	
	public boolean succeededA() {
		return succeededA;
	}
	
	public boolean succeededB() {
		return succeededB;
	}
	
	public boolean succeededX() {
		return succeededX;
	}
	
	public boolean succeededE() {
		return succeededE;
	}
	
	
	public String finalOutcome() throws Exception{
		String outcome = null;
		
		if (succeededA) {
			outcome = "succeededA";
		} else if (succeededB) {
			outcome = "succeededB";
		} else if (succeededX) {
			outcome = "succeededX";
		} else if (recoveredNaturally) {
			outcome = "recoveredNaturally";
		}else if (reInfected) {
			outcome = "reInfected";
		} else {
			throw new Exception("Invalid outcome recorded!");
		}
		return outcome;
	}
}
