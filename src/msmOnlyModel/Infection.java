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
	private double tickStarted;
	private boolean detected;
	private boolean everDetected;
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
	private String riskGroup;
	
	//treatment history
	private boolean inTreatment;
	private boolean attemptedA;
	private boolean attemptedB;
	private boolean developedResistanceA;
	private boolean developedResistanceB;
	private boolean failedTreatment;
	
	//final outcome; all possible infection end-points mutually exclusive
	private double tickEnded;
	private boolean developedResistance;
	private boolean reInfected;
	private boolean succeededA;
	private boolean succeededB;
	private boolean succeededX;
	private boolean succeededE;
	private boolean recoveredNaturally;


	
	public Infection(Indiv host, Parameters parameters, String strain, boolean starting, double naturalRecoveryTime, String subPop, ThreadSafeRandomHelper randomHelper, double currentTick) {//different for start of sim
		this.host = host;
		this.riskGroup = host.getRiskGroup();
		this.strain = strain;
		this.subPop = subPop;
		this.tickStarted = currentTick;
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
		this.tickEnded = obs.tickNow();
		host.clearSeekCareScheduled();
		
		try {
			obs.processCompleteInfection(this);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		this.current = false;
		this.host = null;
	}
	
	public void recordTransmission() {
		transmissionEvents++;
		
	}
	
	public int transmissionEvents() {
		return transmissionEvents;
	}
	
	public void overrideInfection() {
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
	
	public boolean wasEverDetected() {
		return everDetected;
	}
	
	public void detect() {
		detected = true;
		
		if (everDetected) {
			//if this infection was previous detected, override the past outcome, which should only be failed treatment
			failedTreatment = false;
			
		}
		
		everDetected = true;
	}
	
	public void undetect() {
		
		Observer obs = host.getObserver();
		host.clearSeekCareScheduled();
		
		try {
			obs.processUndetectInfection(this);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		detected = false;
		inTreatment = false;
		
		
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
		host.recordInTreatment();
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
			if (attemptedB==false) {
				System.out.print("issue");
			}
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
	
	public double duration() {
		double duration = 0;
		duration = tickEnded - tickStarted;
		if (duration < 0.002739726) {
			duration = 0.002739726; //duration cannot be shorter than 1 day, 
			//so that when dividing by duration if cannot be zero or artifically inflate rate
		}
		
		return duration;
	}
	
	public Indiv host() {
		return host;
	}
	
	public boolean current() {
		return current;
	}
	
	public String getRiskGroup() {
		return riskGroup;
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
