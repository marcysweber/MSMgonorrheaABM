/**
 * 
 */
package msmOnlyModel;

import cern.jet.random.Exponential;
import cern.jet.random.Uniform;
import repast.simphony.engine.schedule.ISchedule;
import repast.simphony.engine.schedule.ScheduleParameters;
import repast.simphony.parameter.Parameters;

/**
 * @author me597
 *
 */
public class Treatment {
	private Indiv indiv;
	private Infection infection;
	private Parameters parameters;
	private Observer observer;
	private ISchedule schedule;
	private ThreadSafeRandomHelper randomHelper;
	private CostCalc costCalc;
	private String counterfactual;
	private boolean switchToB;
	private boolean switchToX;
	private boolean addedX;
	
	private int availrDST;
	private int adhereTOCsympt;
	private int adhereTOCasympt;
	
	private double realisticRandom; //prob of random treatment under realistic combo scenario
	private double realisticTOC;//prob of TOC treatment under realistic combo scenario
	private double realisticDST;//prob of DST treatment under realistic combo scenario

	// both test-of-cure and strain test counterfactuals will happend within
	// Treatment

	public Treatment(Indiv indiv, Observer observer) {
	
		this.addedX = observer.getAddedX();
		this.indiv = indiv;
		this.parameters = indiv.getParameters();
		
		this.availrDST = parameters.getInteger("availrDST");
		this.adhereTOCsympt = parameters.getInteger("adhereTOCsympt");
		this.adhereTOCasympt = parameters.getInteger("adhereTOCasympt");
		
		this.realisticRandom = parameters.getDouble("realisticRandom");
		this.realisticTOC = parameters.getDouble("realisticTOC");
		this.realisticDST = parameters.getDouble("realisticDST");

		
		this.infection = indiv.myInfection();
		this.schedule = indiv.getSchedule();
		this.randomHelper = indiv.getRandomHelper();
		this.observer = observer;
		this.costCalc = observer.getCostCalc();
		this.counterfactual = parameters.getString("counterfactual");
		this.switchToB = observer.getSurveillance().getSwitchToB();
		this.switchToX = observer.getSurveillance().getSwitchToX();
	}
	
	
	public boolean administerA() {
		boolean success = false;
		indiv.recordTreatment();

		costCalc.treatmentDrugACost(indiv);
		observer.recordNewAttemptedTreatmentA(indiv);

		if (infection.susceptibleToA()) {
			success = true;
		} else {
			success = false;
		}
		
		return success;
	}
	
	public void tryDrugA() {
		boolean success = administerA();
		
		if (success) {
			indiv.recoverOrDevelopResistance("A");
		} else if (infection.symptoms()) {
			symptomaticTreatmentFailure("A");
		} else { // if asymptomatic, true fail
			fail(indiv);
		}
	}
	
	public boolean administerB() {
		boolean success = false;
		indiv.recordTreatment();

		costCalc.treatmentDrugBCost(indiv);
		observer.recordNewAttemptedTreatmentB(indiv);
		
		if (infection.susceptibleToB()) {
			success = true;
		} else {
			success = false;
		}
		
		return success;
	}

	public void tryDrugB() {
		boolean success = administerB();

		if (success) {
			indiv.recoverOrDevelopResistance("B");
		} else if (infection.symptoms()) { // if symptomatic, known failure, try again
			symptomaticTreatmentFailure("B");
		} else { // if asymptomatic, true fail
			fail(indiv);
		}
	}
	
	public boolean treatWithX() {
		boolean success = true;
		indiv.recordTreatment();
		costCalc.treatmentDrugXCost(indiv);
		observer.recordNewAttemptedTreatmentX(indiv);
		indiv.actuallyRecover("X");
		
		return success;
	}
	
	public boolean treatWithE() {
		boolean success = true;
		indiv.recordTreatment();
		costCalc.treatmentDrugECost(indiv);
		observer.recordUseE(indiv);
		indiv.actuallyRecover("E");
		
		return success;
	}
	


	public void tryDrugXorE() {
		
		addedX = observer.getAddedX();
		
		if (addedX) {
			treatWithX();
		} else {
			//if no X, what do?
			treatWithE();

			}
			
	}
	

	public void treat() throws Exception {
		
		indiv.recordInTreatment();

		// record treatment at all

		if (indiv.symptoms()) {
			//indiv.abstain();
		}

		//System.out.println(counterfactual);
		// determine the appropriate treatment scenario
		
		if (schedule.getTickCount() < 520) {
			tryDrugA();
		} else {
			
			//sweeps and non-AMR runs
			if (counterfactual.contains("sweep") || counterfactual.contains("none")){ // GISP pre-switch (default drug A)?
				treatDefaultBeforeSwitch();
			
			
			//GISP possibilities
			} else if (counterfactual.contains("GISP")){
				if (!switchToB && !switchToX) {
					treatDefaultBeforeSwitch();
				} else if (switchToB) { // have we switched to drug B?
					treatAfterSwitchB();
				} else if (switchToX) {
					treatAfterSwitchX();
				} 
				
			// TOC
			} else if (counterfactual.contains("test-of-cure")) { // test-of-cure?
				// in test of cure scenario
				whichTestOfCure();

			//RT
			} else if (counterfactual.equals("random")) {
				treatRandom();
				
			//DST
			} else if (counterfactual.contains("drug_sus_testing")) {
				treatDrugSusTesting();
				
			//RC
			} else if (counterfactual.contains("realistic_combo")) {
				try {
					treatRealisticCombo();
				} catch (Exception e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}  else {
				throw new Exception("No valid counterfactual argument supplied!");
			}
		}
	}

	public void treatRandom() {
		Uniform drugUniform = (Uniform) randomHelper.getDistribution("randomDrugUniform");
		double randomValue = drugUniform.nextDouble();

		if (addedX) {
			if (randomValue < 0.33333333) {
				// treat with drug A first
				tryDrugA();

			} else if (randomValue < 0.66666667){
				tryDrugB();
			} else {
				// treat with drug B first
				tryDrugXorE();
			}
		} else { //before X is added to possible treatments
			if (randomValue < 0.5) {
				// treat with drug A first
				tryDrugA();

			} else{
				tryDrugB();
			}
		}
	}

	public void treatDrugSusTesting() {
		//System.out.println("Doing DST!");

		if (counterfactual.contains("100")) {
			treatDrugSusTestingPerfect();
		} else {
			//System.out.println("Doing imperfect DST!");

			//System.out.println(adherence);
			
			treatDrugSusTestingImperfect(availrDST);
		}
	}
	
	
	
	public void treatDrugSusTestingPerfect() {
		costCalc.strainTestCost(1);
		Testing testing = new Testing(infection, parameters);
		String susProfile = testing.drugSusceptibilityTest();

		if (susProfile.contains("A")) {
			tryDrugA();
		} else if (susProfile.contains("B")) {
			tryDrugB();
		} else {
			tryDrugXorE();
		}
	}
	
	public void treatDrugSusTestingImperfect(double adherence) {
		Uniform adherenceUniform = (Uniform) randomHelper.getDistribution("adherenceUniform");
		double randomValue = adherenceUniform.nextDouble() * 100;
		//System.out.println(randomValue);

		if (randomValue < adherence) { // get retested and retreated if appropriate
			treatDrugSusTestingPerfect();
		} else { // only get re-treated if BOTH resistant and symptomatic
			//System.out.println("non-adherent");
			tryDrugA();

		}

	}



	public void symptomaticTreatmentFailure(String treatmentAttempted) {
		//indiv.abstain(); // should already be abstaining, but just to confirm
		indiv.checkForSequelae();

		//ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
		Exponential delayToRetreatmentExp = null;
		
		delayToRetreatmentExp = (Exponential) randomHelper.getDistribution("delayToRetreatmentMSMExp");
		
		double thisDelay = delayToRetreatmentExp.nextDouble();
		// System.out.println("Second delay:");
		// System.out.println(thisDelay);

		String nextTreatment;

		if (treatmentAttempted.equals("A")) {
			nextTreatment = "B";
		} else if (treatmentAttempted.equals("B") && !switchToB) {
			nextTreatment = "A";
		} else { // after switch to B, or catch-all
			nextTreatment = "X";
		}

		costCalc.symptomaticQALYsLost(indiv, thisDelay);

		ScheduleParameters schparams = ScheduleParameters.createOneTime(thisDelay + indiv.tickNow());
		schedule.schedule(schparams, this, "retreat", nextTreatment);

		// two possibilties for treatmentattempted
		// then two possibilities; susceptible to the other, or resistant to both

		// assume, even if resistant to both drugs, symptomatic case will eventually be
		// treated
	}

	public void retreat(String retreatment) {
		if (indiv.infectious()){//confirm still infectious
			// check that indiv is still infectious, bc there is chance of natural recovery

			//ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
			Exponential delayToRetreatmentExp = null;
			delayToRetreatmentExp = (Exponential) randomHelper.getDistribution("delayToRetreatmentMSMExp");
			double thisDelay = delayToRetreatmentExp.nextDouble();
			ScheduleParameters schparams = ScheduleParameters.createOneTime(thisDelay + indiv.tickNow());

			String success = null;

			if (retreatment.equals("A")) {
				costCalc.treatmentDrugACost(indiv);
				observer.recordNewAttemptedTreatmentA(indiv);
				if (infection.susceptibleToA()) {
					success = "A";
					indiv.recoverOrDevelopResistance(success);

				} else {
					// schedule retreatment with X
					schedule.schedule(schparams, this, "retreat", "X");
					costCalc.symptomaticQALYsLost(indiv, thisDelay);
				}
			} else if (retreatment.equals("B")) {
				costCalc.treatmentDrugBCost(indiv);
				observer.recordNewAttemptedTreatmentB(indiv);
				if (infection.susceptibleToB()) {
					success = "B";
					indiv.recoverOrDevelopResistance(success);

				} else {
					// schedule retreatment with X
					schedule.schedule(schparams, this, "retreat", "X");
					costCalc.symptomaticQALYsLost(indiv, thisDelay);
				}
			} else {
				tryDrugXorE();
				
			}

		
		}
	}

	public void treatDefaultBeforeSwitch() {
		tryDrugA();
	}

	public void treatAfterSwitchB() {
		tryDrugB();
	}

	public void treatAfterSwitchX() {
		tryDrugXorE();
	}

	public void whichTestOfCure() {
		
			if (indiv.symptoms()){
				treatTestOfCureImperfect(adhereTOCsympt);
			} else {
				treatTestOfCureImperfect(adhereTOCasympt);
			
		}
	}

	public void treatTestOfCurePerfect() {
		//indiv.abstain();

		costCalc.treatmentDrugACost(indiv);
		observer.recordNewAttemptedTreatmentA(indiv);

		// everybody incurs the cost of getting re-tested

		if (infection.susceptibleToA()) {
			indiv.recoverOrDevelopResistance("A");
			costCalc.testCost(indiv); // test confirming negative

		} else {
			// initial treatment failure
			//ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
			Exponential delayToRetreatmentExp = null;
			
			delayToRetreatmentExp = (Exponential) randomHelper.getDistribution("delayToRetreatmentMSMExp");
			
			double thisDelay = delayToRetreatmentExp.nextDouble();
			ScheduleParameters schparams = ScheduleParameters.createOneTime(thisDelay + indiv.tickNow());

			schedule.schedule(schparams, this, "retreatTestOfCure", "B");
			costCalc.symptomaticQALYsLost(indiv, thisDelay);
		}
	}

	public void retreatTestOfCure(String treatment) {
		costCalc.testCost(indiv);
		//indiv.abstain();

		if (indiv.infectious()) {

			if (treatment.equals("B")) {
				costCalc.treatmentDrugBCost(indiv);
				observer.recordNewAttemptedTreatmentB(indiv);

				if (infection.susceptibleToB()) {
					indiv.recoverOrDevelopResistance("B");
					costCalc.testCost(indiv);// test confirming negative
				} else {
					// schedule another retreatment
					//ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
					Exponential delayToRetreatmentExp = null;

					delayToRetreatmentExp = (Exponential) randomHelper.getDistribution("delayToRetreatmentMSMExp");

					double thisDelay = delayToRetreatmentExp.nextDouble();
					ScheduleParameters schparams = ScheduleParameters.createOneTime(thisDelay + indiv.tickNow());

					schedule.schedule(schparams, this, "retreatTestOfCure", "X");
					costCalc.symptomaticQALYsLost(indiv, thisDelay);
				}

			} else if (treatment.equals("X")) {
				tryDrugXorE();
				costCalc.testCost(indiv);
			}
		}
	}

	public void treatTestOfCureImperfect(double adherence) {
		// everybody gets drug A to start

		// "adherence" determines proportion that returns for second test
		Uniform adherenceUniform = (Uniform) randomHelper.getDistribution("adherenceUniform");
		double randomValue = adherenceUniform.nextDouble() * 100;

		if (randomValue < adherence) { // get retested and retreated if appropriate
			treatTestOfCurePerfect();
		} else { // only get re-treated if BOTH resistant and symptomatic
			tryDrugA();

		}

	}

	public void fail(Indiv indiv) {

		// true, unknown failed treatment
		indiv.recordFailedTreatment();
		indiv.recordEndTreatment();
		//indiv.stopAbstaining();
	}
	
	
	

	public void treatRealisticCombo() throws Exception {
		Uniform realisticUniform = (Uniform) randomHelper.getDistribution("realisticComboUniform");
		
		double randomCutOff = realisticRandom;
		double TOCCutOff = realisticRandom + realisticTOC;
		double DSTCutOff = TOCCutOff + realisticDST; //should always be 1.0
		
		double newValue = realisticUniform.nextDouble();
		
		if (newValue <= randomCutOff) {
			treatRandom();
		} else if (newValue <= TOCCutOff) {
			whichTestOfCure();
		} else if (newValue <= DSTCutOff) {
			treatDrugSusTestingPerfect();
		} else {
			throw new Exception("the realistic combo counterfactual had invalid probabilities");		}
		
		
	}
	
	
}
