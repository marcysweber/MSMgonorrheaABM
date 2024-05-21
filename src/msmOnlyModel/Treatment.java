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
	// both test-of-cure and strain test counterfactuals will happend within
	// Treatment

	public Treatment(Indiv indiv, Observer observer) {
	
		this.addedX = observer.getAddedX();
		this.indiv = indiv;
		this.parameters = indiv.getParameters();
		this.infection = indiv.myInfection();
		this.schedule = indiv.getSchedule();
		this.randomHelper = indiv.getRandomHelper();
		this.observer = observer;
		this.costCalc = observer.getCostCalc();
		this.counterfactual = parameters.getString("counterfactual");
		this.switchToB = observer.getSurveillance().getSwitchToB();
		this.switchToX = observer.getSurveillance().getSwitchToX();
	}

	public void treat() {

		// record treatment at all
		indiv.recordTreatment();

		if (indiv.symptoms()) {
			indiv.abstain();
		}

		//System.out.println(counterfactual);
		// determine the appropriate treatment scenario

		if (switchToX) {
			treatAfterSwitchX();
		} else if (switchToB) { // have we switched to drug B?
			treatAfterSwitchB();

		} else if (counterfactual.contains("test-of-cure")) { // test-of-cure?
			// in test of cure scenario
			whichTestOfCure();

		} else if (counterfactual.equals("random")) {
			treatRandom();
		} else if (counterfactual.contains("drug_sus_testing")) {
			
			treatDrugSusTesting();
		} else { // GISP pre-switch (default drug A)?
			treatDefaultBeforeSwitch();

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

			double adherence = Double.parseDouble(counterfactual.substring(17));
			//System.out.println(adherence);
			
			treatDrugSusTestingImperfect(adherence);
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

	public void tryDrugA() {
		costCalc.treatmentDrugACost(indiv);
		observer.recordNewAttemptedTreatmentA(indiv);

		if (infection.susceptibleToA()) {
			indiv.recoverOrDevelopResistance("A");
		} else if (infection.resistantToA() && infection.symptoms()) {
			symptomaticTreatmentFailure("A");
		} else { // if asymptomatic, true fail
			fail(indiv);
		}
	}

	public void tryDrugB() {
		costCalc.treatmentDrugBCost(indiv);
		observer.recordNewAttemptedTreatmentB(indiv);

		if (infection.susceptibleToB()) {
			indiv.recoverOrDevelopResistance("B");
		} else if (infection.resistantToB() && infection.symptoms()) { // if symptomatic, known failure, try again
			symptomaticTreatmentFailure("B");
		} else { // if asymptomatic, true fail
			fail(indiv);
		}
	}

	public void tryDrugXorE() {
		if (addedX) {
			costCalc.treatmentDrugXCost(indiv);
			observer.recordNewAttemptedTreatmentX(indiv);
			indiv.recoverOrDevelopResistance("X");
		} else {
			//if no X, what do?
			costCalc.treatmentDrugECost(indiv);
			observer.recordUseE(indiv);
			indiv.recoverOrDevelopResistance("E");

			}
			
	}
	

	public void symptomaticTreatmentFailure(String treatmentAttempted) {
		indiv.abstain(); // should already be abstaining, but just to confirm

		//ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
		Exponential delayToRetreatmentExp = null;
		
		if (indiv.getGender().equals("w")) {
			delayToRetreatmentExp = (Exponential) randomHelper.getDistribution("delayToRetreatmentFExp");
		} else if (indiv.getSubPop().equals("msm")) {
			delayToRetreatmentExp = (Exponential) randomHelper.getDistribution("delayToRetreatmentMSMExp");
		} else if (indiv.getSubPop().equals("msw")) {
			delayToRetreatmentExp = (Exponential) randomHelper.getDistribution("delayToRetreatmentMSWExp");
		} else {
			delayToRetreatmentExp = (Exponential) randomHelper.getDistribution("delayToRetreatmentFExp");
		}
		
		
		
		
		
		
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

		costCalc.addQALYsLost(indiv, thisDelay);

		ScheduleParameters schparams = ScheduleParameters.createOneTime(thisDelay + indiv.tickNow());
		schedule.schedule(schparams, this, "retreat", nextTreatment);

		// two possibilties for treatmentattempted
		// then two possibilities; susceptible to the other, or resistant to both

		// assume, even if resistant to both drugs, symptomatic case will eventually be
		// treated
	}

	public void retreat(String retreatment) {
		indiv.abstain(); // should already be abstaining, but just to confirm

		// check that indiv is still infectious, bc there is chance of natural recovery

		//ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
		Exponential delayToRetreatmentExp = null;
		
		if (indiv.getGender().equals("w")) {
			delayToRetreatmentExp = (Exponential) randomHelper.getDistribution("delayToRetreatmentFExp");
		} else if (indiv.getSubPop().equals("msm")) {
			delayToRetreatmentExp = (Exponential) randomHelper.getDistribution("delayToRetreatmentMSMExp");
		} else if (indiv.getSubPop().equals("msw")) {
			delayToRetreatmentExp = (Exponential) randomHelper.getDistribution("delayToRetreatmentMSWExp");
		} else {
			delayToRetreatmentExp = (Exponential) randomHelper.getDistribution("delayToRetreatmentFExp");
		}
		
		double thisDelay = delayToRetreatmentExp.nextDouble();
		ScheduleParameters schparams = ScheduleParameters.createOneTime(thisDelay + indiv.tickNow());

		String success = null;

		if (retreatment.equals("A")) {
			costCalc.treatmentDrugACost(indiv);
			observer.recordNewAttemptedTreatmentA(indiv);
			if (infection.susceptibleToA()) {
				success = "A";
			} else {
				// schedule retreatment with X
				schedule.schedule(schparams, this, "retreat", "X");
				costCalc.addQALYsLost(indiv, thisDelay);
			}
		} else if (retreatment.equals("B")) {
			costCalc.treatmentDrugBCost(indiv);
			observer.recordNewAttemptedTreatmentB(indiv);
			if (infection.susceptibleToB()) {
				success = "B";
			} else {
				// schedule retreatment with X
				schedule.schedule(schparams, this, "retreat", "X");
				costCalc.addQALYsLost(indiv, thisDelay);
			}
		} else {
			tryDrugXorE();
		}

		if (success != null) {
			indiv.recoverOrDevelopResistance(success);
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
		if (counterfactual.contains("100")) {
			treatTestOfCurePerfect();
		} else {
			double adherence = Double.parseDouble(counterfactual.substring(13));
			treatTestOfCureImperfect(adherence);
		}
	}

	// 1-17 i think this still needs some updating! delays to retreatment!
	public void treatTestOfCurePerfect() {
		indiv.abstain();

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
			
			if (indiv.getGender().equals("w")) {
				delayToRetreatmentExp = (Exponential) randomHelper.getDistribution("delayToRetreatmentFExp");
			} else if (indiv.getSubPop().equals("msm")) {
				delayToRetreatmentExp = (Exponential) randomHelper.getDistribution("delayToRetreatmentMSMExp");
			} else if (indiv.getSubPop().equals("msw")) {
				delayToRetreatmentExp = (Exponential) randomHelper.getDistribution("delayToRetreatmentMSWExp");
			} else {
				delayToRetreatmentExp = (Exponential) randomHelper.getDistribution("delayToRetreatmentFExp");
			}			
			
			
			double thisDelay = delayToRetreatmentExp.nextDouble();
			ScheduleParameters schparams = ScheduleParameters.createOneTime(thisDelay + indiv.tickNow());

			schedule.schedule(schparams, this, "retreatTestOfCure", "B");
			costCalc.addQALYsLost(indiv, thisDelay);
		}
	}

	public void retreatTestOfCure(String treatment) {
		costCalc.testCost(indiv);
		indiv.abstain();

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
				
				if (indiv.getGender().equals("w")) {
					delayToRetreatmentExp = (Exponential) randomHelper.getDistribution("delayToRetreatmentFExp");
				} else if (indiv.getSubPop().equals("msm")) {
					delayToRetreatmentExp = (Exponential) randomHelper.getDistribution("delayToRetreatmentMSMExp");
				} else if (indiv.getSubPop().equals("msw")) {
					delayToRetreatmentExp = (Exponential) randomHelper.getDistribution("delayToRetreatmentMSWExp");
				} else {
					delayToRetreatmentExp = (Exponential) randomHelper.getDistribution("delayToRetreatmentFExp");
				}				
				
				double thisDelay = delayToRetreatmentExp.nextDouble();
				ScheduleParameters schparams = ScheduleParameters.createOneTime(thisDelay + indiv.tickNow());

				schedule.schedule(schparams, this, "retreatTestOfCure", "X");
				costCalc.addQALYsLost(indiv, thisDelay);
			}

		} else if (treatment.equals("X")) {
			tryDrugXorE();
			costCalc.testCost(indiv);
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
		indiv.stopAbstaining();
	}

}
