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
	private Infection infection;
	private Parameters parameters;
	private Observer observer;
	private ISchedule schedule;
	private ThreadSafeRandomHelper randomHelper;
	private CostCalc costCalc;
	private String counterfactual;
	private boolean removedA;
	private boolean removedB;
	private boolean removedAandB;
	private boolean addedX;
	
	private int availrDST;
	private int adhereTOCsympt;
	private int adhereTOCasympt;
	
	private double realisticRandom; //prob of random treatment under realistic combo scenario
	private double realisticTOC;//prob of TOC treatment under realistic combo scenario
	private double realisticDST;//prob of DST treatment under realistic combo scenario

	// both test-of-cure and strain test counterfactuals will happend within
	// Treatment

	public Treatment(Infection infection, Observer observer) {
	
		this.addedX = observer.getAddedX();
		this.parameters = observer.getParameters();
		
		this.availrDST = parameters.getInteger("availrDST");
		this.adhereTOCsympt = parameters.getInteger("adhereTOCsympt");
		this.adhereTOCasympt = parameters.getInteger("adhereTOCasympt");
		
		this.realisticRandom = parameters.getDouble("realisticRandom");
		this.realisticTOC = parameters.getDouble("realisticTOC");
		this.realisticDST = parameters.getDouble("realisticDST");

		
		this.infection =infection;
		this.schedule = infection.host().getSchedule();
		this.randomHelper = infection.host().getRandomHelper();
		this.observer = observer;
		this.costCalc = observer.getCostCalc();
		this.counterfactual = parameters.getString("counterfactual");
		this.removedA = observer.getSurveillance().getRemovedA();
		this.removedB = observer.getSurveillance().getRemovedB();
		this.removedAandB = observer.getSurveillance().getRemovedAandB();
	}
	
	public void scheduleClearance(String treatment) {
		//even if the treatment is successful,
		//the indiv remains infectious for an average of 3 days
		
		Indiv indiv = this.infection.host();
		
		Exponential delayToClearanceExp = null;
		delayToClearanceExp = (Exponential) randomHelper.getDistribution("delayToClearanceExp");
		double thisDelay = delayToClearanceExp.nextDouble();
		ScheduleParameters schparams = ScheduleParameters.createOneTime(thisDelay + indiv.tickNow());

		schedule.schedule(schparams, indiv, "recoverOrDevelopResistance", treatment);
	}
	
	public boolean administerA() {
		//System.out.println("adminA");
		
		boolean success = false;
		infection.attemptA();

		if (infection.susceptibleToA()) {
			success = true;
		} else {
			success = false;
			try {
				infection.checkForSequelae("failedA");
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		//System.out.print(success);
		return success;
	}
	
	public void tryDrugA() {
		//System.out.println("tryA");

		boolean success = administerA();
		
		if (success) {
			this.scheduleClearance("A");
		} else if (infection.symptoms()) {
			symptomaticTreatmentFailure("A");
		} else { // if asymptomatic, true fail
			fail(infection.host());
		}
	}
	
	public boolean administerB() {
		//System.out.println("adminB");

		boolean success = false;
		infection.attemptB();
		
		if (infection.susceptibleToB()) {
			success = true;
		} else {
			success = false;
			try {
				infection.checkForSequelae("failedB");
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

		}
		
		return success;
	}

	public void tryDrugB() {
		//System.out.println("tryA");

		boolean success = administerB();

		if (success) {
			this.scheduleClearance("B");
		} else if (infection.symptoms()) { // if symptomatic, known failure, try again
			symptomaticTreatmentFailure("B");
		} else { // if asymptomatic, true fail
			fail(infection.host());
		}
	}
	
	public void tryAandBTogether() {
		boolean a = administerA();
		boolean b = administerB();
	
		if (a || b) {
			
			this.scheduleClearance("AandB");
			
		} else if (infection.symptoms()) {
			symptomaticTreatmentFailure("AandB");
		} else {
			fail(infection.host());
		}
		
	}
	
	public boolean treatWithX() {
		boolean success = true;
		scheduleClearance("X");
		
		return success;
	}
	
	public boolean treatWithE() {
		boolean success = true;
		scheduleClearance("E");
		
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

		infection.recordInTreatment();
		observer.addToDetectedList(infection);
		infection.recordVisitClinic();


		if (infection.current()) {

			if (schedule.getTickCount() < 520) {
				tryDrugA();
			} else {

				//sweeps and non-AMR runs
				if (counterfactual.contains("sweep") || counterfactual.contains("none")){ // GISP pre-switch (default drug A)?
					treatDefaultBeforeSwitch();


					//GISP possibilities
				} else if (counterfactual.contains("GISP")){
					treatGISP();

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
	}

	
	
	public void treatGISP() throws Exception {

		if (counterfactual.contains("rand")) {
			
				treatGISPRand();
			
		} else if (counterfactual.contains("emp")) {
				treatGISPEmp();
			
		} else {
			throw new Exception("GISP subscenario not specified");
		}
		
		
		
	}
	
	
	public void treatGISPEmp() throws Exception{

		if (!removedA && !removedB && !removedAandB) { //default, nothing removed yet
			tryDrugA();
			
		} else if (removedA && !removedB) { //if we've removed A but not B
			tryDrugB();
			
		} else if (removedB && !removedA) {
			tryDrugA();
			
		} else if (removedA && removedB && !removedAandB) { // if we've remove A and B separately but not combo therapy
			tryAandBTogether();
			
		} else if (removedA && removedB && removedAandB) {
			tryDrugXorE();
			
		} else {
			System.out.println("RemovedA:");
			System.out.println(removedA);
			System.out.println("removedB");
			System.out.println(removedB);
			System.out.println("removedAandB");
			System.out.println(removedAandB);

			
			throw new Exception("Invalid combo of drug removals");
		}
	}
	
	public void treatGISPRand() throws Exception {

		Uniform drugUniform = (Uniform) randomHelper.getDistribution("randomDrugUniform");
		double randomValue = drugUniform.nextDouble();
		
		
		if (!removedA && !removedB && !removedAandB) { //default, nothing removed yet
			
			if (addedX) { //if nothing has been removed and X is added...
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
			
		} else if (removedA && !removedB) { //if we've removed A but not B
			
			if (addedX) { //if A has been removed and X is added...
				if (randomValue < 0.5) {
					tryDrugB();
				} else {
					tryDrugXorE();
				}
			} else { //before X is added to possible treatments
					tryDrugB();
			}
			
		} else if (removedB && !removedA) {
			if (addedX) { //if A has been removed and X is added...
				if (randomValue < 0.5) {
					tryDrugA();
				} else {
					tryDrugXorE();
				}
			} else { //before X is added to possible treatments
					tryDrugA();
			}
			
			
		} else if (removedA && removedB && !removedAandB) { // if we've remove A and B separately but not combo therapy
			if (addedX) {
				tryDrugXorE();
			} else {
				tryAandBTogether();
			}
		} else if (removedA && removedB && removedAandB) {
			tryDrugXorE();
		} else {
			System.out.println("RemovedA:");
			System.out.println(removedA);
			System.out.println("removedB");
			System.out.println(removedB);
			System.out.println("removedAandB");
			System.out.println(removedAandB);

			
			throw new Exception("Invalid combo of drug removals");
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

		//ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
		Exponential delayToRetreatmentExp = null;
		
		delayToRetreatmentExp = (Exponential) randomHelper.getDistribution("delayToRetreatmentMSMExp");
		
		double thisDelay = delayToRetreatmentExp.nextDouble();
		// System.out.println("Second delay:");
		// System.out.println(thisDelay);

		String nextTreatment;

		if (treatmentAttempted.equals("A")) {
			nextTreatment = "B";
		} else if (treatmentAttempted.equals("B") && !removedA) {
			nextTreatment = "A";
		} else { // after switch to B, or catch-all
			nextTreatment = "X";
		}


		ScheduleParameters schparams = ScheduleParameters.createOneTime(thisDelay + infection.host().tickNow());
		schedule.schedule(schparams, this, "retreat", nextTreatment);

		// two possibilties for treatmentattempted
		// then two possibilities; susceptible to the other, or resistant to both

		// assume, even if resistant to both drugs, symptomatic case will eventually be
		// treated
	}

	public void retreat(String retreatment) {
		if (infection.current()){//confirm still infectious
			// check that indiv is still infectious, bc there is chance of natural recovery
			

			infection.recordVisitClinic();

			//ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
			Exponential delayToRetreatmentExp = null;
			delayToRetreatmentExp = (Exponential) randomHelper.getDistribution("delayToRetreatmentMSMExp");
			double thisDelay = delayToRetreatmentExp.nextDouble();
			ScheduleParameters schparams = ScheduleParameters.createOneTime(thisDelay + infection.host().tickNow());

			String success = null;

			if (retreatment.equals("A")) {
					boolean successA = administerA();
				if (successA) {
					success = "A";
					this.scheduleClearance(success);

				} else {
					// schedule retreatment with X
					schedule.schedule(schparams, this, "retreat", "X");
				}
			} else if (retreatment.equals("B")) {
				boolean successB = administerB();
				
				if (successB) {
					success = "B";
					this.scheduleClearance(success);

				} else {
					// schedule retreatment with X
					schedule.schedule(schparams, this, "retreat", "X");
				}
			} else {
				tryDrugXorE();
				
			}

		
		}
	}

	public void treatDefaultBeforeSwitch() {
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

	public void treatAfterSwitchB() {
		tryDrugB();
	}

	public void treatAfterSwitchX() {
		tryDrugXorE();
	}

	public void whichTestOfCure() {
		
			if (infection.symptoms()){
				treatTestOfCureImperfect(adhereTOCsympt);
			} else {
				treatTestOfCureImperfect(adhereTOCasympt);
			
		}
	}

	public void treatTestOfCurePerfect() {
		//indiv.abstain();
		if (infection.current()) {
		
		infection.attemptA();

		// everybody incurs the cost of getting re-tested

		if (infection.susceptibleToA()) {
			infection.recordDiagnosticTest();
			this.scheduleClearance("A");

		} else {
			// initial treatment failure
			//ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
			try {
				infection.checkForSequelae("failedATOC");
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

			Exponential delayToRetreatmentExp = null;
			
			delayToRetreatmentExp = (Exponential) randomHelper.getDistribution("delayToRetreatmentMSMExp");
			
			double thisDelay = delayToRetreatmentExp.nextDouble();
			ScheduleParameters schparams = ScheduleParameters.createOneTime(thisDelay + infection.host().tickNow());

			schedule.schedule(schparams, this, "retreatTestOfCure", "B");
			infection.recordDiagnosticTest();

		}
		}
	}

	public void retreatTestOfCure(String treatment) {

		//indiv.abstain();

		if (infection.current()) {
			
			infection.recordVisitClinic();
			if (treatment.equals("B")) {
				infection.attemptB();

				if (infection.susceptibleToB()) {
					infection.recordDiagnosticTest();
					this.scheduleClearance("B");
				} else {
					// schedule another retreatment
					//ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
					try {
						infection.checkForSequelae("failedBTOC");
					} catch (Exception e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}

					Exponential delayToRetreatmentExp = null;

					delayToRetreatmentExp = (Exponential) randomHelper.getDistribution("delayToRetreatmentMSMExp");

					double thisDelay = delayToRetreatmentExp.nextDouble();
					ScheduleParameters schparams = ScheduleParameters.createOneTime(thisDelay + infection.host().tickNow());

					schedule.schedule(schparams, this, "retreatTestOfCure", "X");
					infection.recordDiagnosticTest();
				}

			} else if (treatment.equals("X")) {
				infection.recordDiagnosticTest();
				tryDrugXorE();
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
		infection.failTreatment();
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
