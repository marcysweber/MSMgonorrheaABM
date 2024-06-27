/**
 * 
 */
package msmOnlyModel;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import repast.simphony.engine.schedule.ISchedule;
import repast.simphony.parameter.Parameters;

/**
 * @author marcy
 *
 */
public class Observer {
	
	private CustomFileOutput outputter;
	
	//general run info
	private Parameters parameters;
	private Population population;
	private SurveillanceProgram surveillanceProgram;
	private ISchedule schedule;
	private int runNumber;
	private int seed;
	private String counterfactual;
	
	
	private boolean addedX;
	
	private double prevalence;
	//prevalence is the % of population infected at a moment in time
	
	private double incidence;
	//incidence is the NEW infected cases over some time unit (*100,000)
	
	private int newCases;
	private int newCasesMSM;
	
	private int newResistACases;
	private double resistAIncidence;
	
	private int newResistBCases;
	private double resistBIncidence;
	
	
	private int newResistBothCases;
	private double resistBothIncidence;
	
	private double symptomProportion;
	private double treatments;
	private double failedTreatments;
	
	private int detected;
	private List<Infection> detectedList;
	

	
	private int detectedAndSymptoms;
	private int soughtCare;
	private int detectedThruScreen;
	private int successTreatmentsA;
	private int successTreatmentsB;
	private int successTreatmentsX;
	private int attemptTreatmentsA;
	private int attemptTreatmentsB;
	private int attemptTreatmentsX;
	private int usageE;
	private CostCalc costCalc;
	
	
	public Observer(Parameters parameters, CustomFileOutput outputter, int seed, double prev, double inc, ISchedule schedule) {
		this.parameters=parameters;
		this.seed = seed;
		this.schedule = schedule;
		this.outputter = outputter;
		this.prevalence = prev;
		this.incidence = inc;
		
		this.runNumber = parameters.getInteger("runNumber");
		//RunState.getInstance().getRunInfo().setRunNumber(this.runNumber);
		this.counterfactual = parameters.getString("counterfactual");

//		this.initialInfected = parameters.getInteger("infected_count_init");
//		this.transmissionM = parameters.getDouble("transmissionM");
//		this.transmissionF = parameters.getDouble("transmissionF");
//
//		//System.out.println(transmission);
//		this.RecoveryLambda = parameters.getDouble("recovery_lambda");
//		this.ProbSymptomaticM = parameters.getDouble("prob_symptomatic_m");
//		this.ProbSymptomaticF = parameters.getDouble("prob_symptomatic_f");
//
//		//this.ScreenInterval = parameters.getDouble("screen_interval");
//		this.delayToSeekCare = parameters.getDouble("delay_to_seek_care");
//		this.delayToRetreatment = parameters.getDouble("delay_to_retreatment");
//		this.percentResistantA = parameters.getDouble("percent_resistant_A");
//		this.beginImportingB = parameters.getInteger("begin_importing_B");
//		this.importingBInterval = parameters.getDouble("importing_B_interval");
//		this.DSTsensitivity = parameters.getDouble("DSTsensitivity");
//		this.DSTspecificity = parameters.getDouble("DSTspecificity");
//
//		
//		this.careCost = parameters.getDouble("care_cost");
//		this.testCost = parameters.getDouble("test_cost");
//		this.strainTestCost = parameters.getDouble("strain_test_cost");
//		this.drugAtreatmentCost = parameters.getDouble("treatment_A_cost");
//		this.drugBtreatmentCost = parameters.getDouble("treatment_B_cost");
//		this.drugXtreatmentCost = parameters.getDouble("treatment_X_cost");
//		this.drugEtreatmentCost = parameters.getDouble("treatment_E_cost");
//		
		this.addedX = false;

		
		this.symptomProportion = 0;
		this.newCases = 0;
		this.newResistACases = 0;
		this.resistAIncidence = 0;
		this.newResistBCases = 0;
		this.resistBIncidence = 0;
		this.newResistBothCases = 0;
		this.resistBothIncidence = 0;
		this.treatments = 0;
		this.failedTreatments = 0;
		this.detected = 0;
		this.detectedList = new ArrayList<Infection>();
		this.detectedAndSymptoms = 0;
		this.soughtCare = 0;
		this.detectedThruScreen = 0;
		this.successTreatmentsA = 0;
		this.successTreatmentsB = 0;
		this.successTreatmentsX = 0;
		
		this.attemptTreatmentsA = 0;
		this.attemptTreatmentsB = 0;
		this.attemptTreatmentsX = 0;
		this.usageE = 0;

		
		this.costCalc = new CostCalc(parameters);
	}
	
	public void setPopulation(Population population) {
		this.population=population;
	}
	
	public void setSurveillance(SurveillanceProgram surveillance) {
		this.surveillanceProgram = surveillance;
	}
	
	//@ScheduledMethod(start = 0, interval = 52, priority = 1) //for annual, make interval 52
	public void calcObserver() {
		
		//SurveillanceProgram surveillance = getSurveillance();
		//surveillance.collectSamples(detectedList);
		
		//Stream<Object> indivs = .getObjectsAsStream(Indiv.class); //grabs all objects of class Indiv

		//each time step, update the Observer's values
		this.prevalence = calcPrev();
		this.incidence = calcInc();  
		


		this.resistAIncidence = calcResistAInc();
		this.resistBIncidence = calcResistBInc();
		this.resistBothIncidence = calcResistBothInc();

		this.symptomProportion = calcSymptomProportion();
	
		double tick = schedule.getTickCount();
		
		SurveillanceProgram surveillance = getSurveillance();
		double surveillanceResultA = surveillance.calcDetectedResistantA();
		double surveillanceResultB = surveillance.calcDetectedResistantB();
		double surveillanceResultBoth = surveillance.calcDetectedResistantBoth();

		if (counterfactual.contains("GISP") && tick > 520) {
			if (!surveillance.getSwitchToB()) {//if not already switched to B
				if (surveillance.checkForSwitch(surveillanceResultA)) {
					surveillance.switchToDrugB();
				} else if (surveillance.checkForSwitch(surveillanceResultBoth)) {
					surveillance.switchToDrugX();
				}
			} else if (surveillance.getSwitchToB()){
				if (surveillance.checkForSwitch(surveillanceResultB) || (surveillance.checkForSwitch(surveillanceResultBoth))) {
					surveillance.switchToDrugX();
				}
			}
		}

		this.outputter.addOutputRow(
				this.runNumber,
				this.seed,
				this.counterfactual,
				this.parameters.getInteger("yearX"),
				
				this.parameters.getDouble("switchThreshold"),
				this.parameters.getInteger("availrDST"),
				this.parameters.getInteger("adhereTOCsympt"),
				this.parameters.getInteger("adhereTOCasympt"),

				
				
				
				
				this.parameters.getInteger("infected_count_init"),
				
				this.parameters.getDouble("transmissionMSM"),
				
				
				this.parameters.getDouble("recovery_lambda"),
				
				this.parameters.getDouble("prob_symptomatic_msm"), 
		
						
				this.parameters.getDouble("screen_interval_MSM"),
				

				this.parameters.getDouble("delay_to_seek_care_msm"), 
			

				this.parameters.getDouble("delay_to_retreatment_msm"),
				

				this.parameters.getDouble("percent_resistant_A"),
				this.parameters.getInteger("begin_importing_B"), 
				this.parameters.getDouble("importing_B_interval"),
				this.parameters.getDouble("DSTsensitivity"),
				this.parameters.getDouble("DSTspecificity"),
				
				this.parameters.getDouble("care_cost"),
				this.parameters.getDouble("test_cost"),
				this.parameters.getDouble("strain_test_cost"),
				this.parameters.getDouble("treatment_A_cost"),
				this.parameters.getDouble("treatment_B_cost"),
				this.parameters.getDouble("treatment_X_cost"),
				this.parameters.getDouble("treatment_E_cost"),	
				
				tick,
				this.prevalence,
				this.incidence,
				this.resistAIncidence,
				this.resistBIncidence,
				this.resistBothIncidence,
				this.symptomProportion,
				this.treatments,
				this.failedTreatments, 
				this.detected, 
				this.detectedAndSymptoms,
				this.detectedThruScreen, 
//				this.knownFailedTreatments,
//				this.knownFailedTreatmentsA,
//				this.knownFailedTreatmentsB,
//				this.knownFailedTreatmentsBoth,
				this.successTreatmentsA, 
				this.successTreatmentsB,
				this.successTreatmentsX,
				this.attemptTreatmentsA,
				this.attemptTreatmentsB,
				this.attemptTreatmentsX,
				this.usageE,
				surveillanceResultA, 
				surveillanceResultB,
				surveillanceResultBoth,
				surveillance.getSwitchToB(),
				surveillance.getSwitchToX(),
				costCalc.getMonetaryCost(),
				costCalc.getQALYsLost()
				
				

				
				);
		
		clearObserver();
		costCalc.clearAnnualCosts();
		
		//System.out.println(addedX);

	}
	
	public void clearObserver() {
		
		this.newCases = 0; //now that incidence has been calculated, clear newCases
		this.newCasesMSM = 0;
		this.newResistACases = 0;
		this.newResistBCases = 0;
		this.newResistBothCases = 0;
		this.treatments = 0;
		this.failedTreatments = 0;
		this.detected = 0;
		this.detectedList = new ArrayList<Infection>();
		
		new ArrayList<Infection>();

		
		this.detectedAndSymptoms = 0;
		this.soughtCare = 0;
		this.detectedThruScreen = 0;
//		this.knownFailedTreatments = 0;
//		this.knownFailedTreatmentsA = 0;
//		this.knownFailedTreatmentsB = 0;
//		this.knownFailedTreatmentsBoth = 0;
		this.successTreatmentsA = 0;
		this.successTreatmentsB = 0;
		this.successTreatmentsX = 0;
		this.attemptTreatmentsA = 0;
		this.attemptTreatmentsB = 0;
		this.attemptTreatmentsX = 0;
		this.usageE = 0;

	}
	
	public double calcPrev() {
		
		double popSize = (double) population.totalSize();
 
		//pass in the Stream as the argument here
		//use the filter as the intermediate operator
		//collect, then call size() on this collected list of infected
		
		//Stream<Object> indivs = .getObjectsAsStream(Indiv.class); //grabs all objects of class Indiv

		List<Object> infected = population.allIndivs()
				.filter(indiv -> ((Indiv) indiv).getState()==1)
				.collect(Collectors.toList());
		
		//System.out.println("Infected:");
		//System.out.println(infected.size());
		
		int countInfected = infected.size();
		
		double newPrev = (countInfected / popSize) * 100.0;
		
		return newPrev;
	}
	
	public double calcPrevMSM() {
		//ideally we would define MSM behaviorally within the model. for now i am using a preference threshold
		return population.msmInfected().count() / population.msmCount() * 100;
	}


	

	
	public double calcInc() {
		double popSize = (double) population.totalSize();

		double newInc = 0;
		newInc = (this.newCases / popSize) * 100000.0; 
		
		return newInc;
	}
	
	
	
	public double calcIncMSM() {
		return (this.newCasesMSM / population.msmCount()) * 100000;
	}
	
	
	
	
	
	
	public double calcAllStrainPrev() {
		
		double popSize = (double) population.totalSize();

		//Stream<Object> infectious = .getObjectsAsStream(Indiv.class); //grabs all objects of class Indiv
		List<Object> withStrain = population.allIndivs()
				.filter(infection -> ((Indiv) infection).getState()==1)
				.filter(infection -> !((Indiv) infection).getStrain().equals("none"))
				.collect(Collectors.toList());

		int countStrain = withStrain.size();
		
		double newPrev = (countStrain / popSize) * 100.0;
		
		return newPrev;
		
	}
	
	
	public double calcAllStrainInc() {
		double popSize = (double) population.totalSize();

		double newInc = 0;
		newInc = ((this.newResistACases + this.newResistBCases - this.newResistBothCases) / popSize) * 100000.0; 
		
		return newInc;
	}
	
	
	public double calcResistAPrev() {
		
		double popSize = (double) population.totalSize();

		//Stream<Object> infectious = .getObjectsAsStream(Indiv.class); //grabs all objects of class Indiv
		List<Object> withStrain = population.allIndivs()
				.filter(infection -> ((Indiv) infection).getState()==1)
				.filter(infection -> ((Indiv) infection).getStrain().equals("A")||((Indiv) infection).getStrain().equals("Both"))
				.collect(Collectors.toList());

		int countStrain = withStrain.size();
		
		double newPrev = (countStrain / popSize) * 100.0;
		
		return newPrev;
		
	}
	
	
	
	
	
	public double calcResistAInc() {
		double popSize = (double) population.totalSize();

		double newInc = 0;
		newInc = ((this.newResistACases) / popSize) * 100000.0; 
		
		return newInc;
	}
	
	
	public double calcResistBPrev() {
		
		double popSize = (double) population.totalSize();

	//	Stream<Object> infectious = .getObjectsAsStream(Indiv.class); //grabs all objects of class Indiv
		List<Object> withStrain = population.allIndivs()
				.filter(infection -> ((Indiv) infection).getState()==1)
				.filter(infection -> ((Indiv) infection).getStrain().equals("B")||((Indiv) infection).getStrain().equals("Both"))
				.collect(Collectors.toList());

		int countStrain = withStrain.size();
		
		double newPrev = (countStrain / popSize) * 100.0;
		
		return newPrev;
		
	}
	
	
	public double calcResistBInc() {
		double popSize = (double) population.totalSize();

		double newInc = 0;
		newInc = ((this.newResistBCases) / popSize) * 100000.0; 
		
		return newInc;
	}
	
	
	public double calcResistBothPrev() {
		
		double popSize = (double) population.totalSize();

		//Stream<Object> infectious = .getObjectsAsStream(Indiv.class); //grabs all objects of class Indiv
		List<Object> withStrain = population.allIndivs()
				.filter(infection -> ((Indiv) infection).getState()==1)
				.filter(infection -> ((Indiv) infection).getStrain().equals("Both"))
				.collect(Collectors.toList());

		int countStrain = withStrain.size();
		
		double newPrev = (countStrain / popSize) * 100.0;
		
		return newPrev;
		
	}
	
	
	public double calcResistBothInc() {
		double popSize = (double) population.totalSize();

		double newInc = 0;
		newInc = ((this.newResistBothCases) / popSize) * 100000.0; 
		
		return newInc;
	}
	
	
	
	public double calcSymptomProportion() {
		double prop = 0;
		
		Stream<Indiv> infectious = population.allIndivs().filter(inf -> ((Indiv) inf).infectious()); //grabs all objects of class Indiv
		Stream<Indiv> withSymptoms = population.allIndivs().filter(inf -> ((Indiv) inf).infectious()).filter(infection -> ((Indiv) infection).symptoms());

		prop = (double) withSymptoms.count() / (double) infectious.count();
		return prop;
	}
	
	
	
	
	
	
	
	
	//@ScheduledMethod(start = 4, interval = 4, priority = 1)
	public void checkStopCondition() {
//		
//		long infectedcount = .getObjectsAsStream(Indiv.class)
//				.filter(indiv -> ((Indiv) indiv).getState()==1)
//				.count();
		double prev = calcPrev();
		
		if ((prev > 10.0 || prev < 0.05) && counterfactual.equals("sweep")) {
			System.out.print("I should stop now!!! ");
			//ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
			schedule.setFinishing(true);
		} else if (prev > 99.0) {
			System.out.print("I should stop now!!! ");
			//ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
			schedule.setFinishing(true);
		}
	}

	
	
	
	
	
	

	public void recordNewCase(Indiv potentialNewCase) {
		if (potentialNewCase.getState() == 1) {
			this.newCases++;
			
			if (potentialNewCase.getGender().equals("f")) {
			} else if (potentialNewCase.getGender().equals("nb")) {
			} else if (potentialNewCase.getSubPop().equals("msw")) {
			} else if (potentialNewCase.getSubPop().equals("msmw")) {
			} else {
				//MSM
				this.newCasesMSM++;
			}
			
			
			if (potentialNewCase.myInfection().getStrain().equals("A")) {
				this.newResistACases++;
				
				if (potentialNewCase.getGender().equals("f")) {
				} else if (potentialNewCase.getGender().equals("nb")) {
				} else if (potentialNewCase.getSubPop().equals("msw")) {
				} else if (potentialNewCase.getSubPop().equals("msmw")) {
				} else {
				}
				
			} else if (potentialNewCase.myInfection().getStrain().equals("B")) {
				this.newResistBCases++;
				
				if (potentialNewCase.getGender().equals("f")) {
				} else if (potentialNewCase.getGender().equals("nb")) {
				} else if (potentialNewCase.getSubPop().equals("msw")) {
				} else if (potentialNewCase.getSubPop().equals("msmw")) {
				} else {
				}
				
			} else if (potentialNewCase.myInfection().getStrain().equals("Both")) {
				//this.newResistACases++;
				//this.newResistBCases++;
				this.newResistBothCases++;
				
				if (potentialNewCase.getGender().equals("f")) {
				} else if (potentialNewCase.getGender().equals("nb")) {
				} else if (potentialNewCase.getSubPop().equals("msw")) {
				} else if (potentialNewCase.getSubPop().equals("msmw")) {
				} else {
				}
			}
		}
	}
	
	
	public SurveillanceProgram getSurveillance() {
		return surveillanceProgram;
	}
	
	
	public void addX() {
		this.addedX = true;
	}
	
	public void recordSoughtCare(Indiv indiv) {
		soughtCare++;
		
		if (indiv.getGender().equals("f")) {
		} else if (indiv.getGender().equals("nb")) {
		} else if (indiv.getSubPop().equals("msw")) {
		} else if (indiv.getSubPop().equals("msmw")) {
		} else {
		}
		
	}
	
	public void recordNewTreatment(Indiv indiv) {
		 treatments++;

	 }
	
	public void recordNewFailedTreatment(Indiv indiv) {
		failedTreatments++;
		
	}
	
	public void recordNewKnownFailedTreatment(Indiv indiv) {
		
	}
	
	public void recordNewKnownFailedTreatmentA(Indiv indiv) {
	}
	
	public void recordNewKnownFailedTreatmentB(Indiv indiv) {
	}
	
	public void recordNewKnownFailedTreatmentBoth(Indiv indiv) {
	}
	
	public void recordNewSuccessTreatmentA(Indiv indiv) {
		successTreatmentsA++;
		if (indiv.getGender().equals("f")) {
		} else if (indiv.getGender().equals("nb")) {
		} else if (indiv.getSubPop().equals("msw")) {
		} else if (indiv.getSubPop().equals("msmw")) {
		} else {
		}
	}
	
	public void recordNewSuccessTreatmentB(Indiv indiv) {
		successTreatmentsB++;
		if (indiv.getGender().equals("f")) {
		} else if (indiv.getGender().equals("nb")) {
		} else if (indiv.getSubPop().equals("msw")) {
		} else if (indiv.getSubPop().equals("msmw")) {
		} else {
		}
	}
	
	public void recordNewSuccessTreatmentX(Indiv indiv) {
		successTreatmentsX++;
		if (indiv.getGender().equals("f")) {
		} else if (indiv.getGender().equals("nb")) {
		} else if (indiv.getSubPop().equals("msw")) {
		} else if (indiv.getSubPop().equals("msmw")) {
		} else {
		}
	}
	
	public void recordNewAttemptedTreatmentA(Indiv indiv) {
		attemptTreatmentsA++;
		if (indiv.getGender().equals("f")) {
		} else if (indiv.getGender().equals("nb")) {
		} else if (indiv.getSubPop().equals("msw")) {
		} else if (indiv.getSubPop().equals("msmw")) {
		} else {
		}
	}
	
	public void recordNewAttemptedTreatmentB(Indiv indiv) {
		attemptTreatmentsB++;
		
		if (indiv.getGender().equals("f")) {
		} else if (indiv.getGender().equals("nb")) {
		} else if (indiv.getSubPop().equals("msw")) {
		} else if (indiv.getSubPop().equals("msmw")) {
		} else {
		}
	}
	
	public void recordNewAttemptedTreatmentX(Indiv indiv) {
		attemptTreatmentsX++;
		
		if (indiv.getGender().equals("f")) {
		} else if (indiv.getGender().equals("nb")) {
		} else if (indiv.getSubPop().equals("msw")) {
		} else if (indiv.getSubPop().equals("msmw")) {
		} else {
		}
	}
	
	public void recordUseE(Indiv indiv) {
		usageE++;
		
		if (indiv.getGender().equals("f")) {
		} else if (indiv.getGender().equals("nb")) {
		} else if (indiv.getSubPop().equals("msw")) {
		} else if (indiv.getSubPop().equals("msmw")) {
		} else {
		}
	}
	
	
	
	public void recordNewDetected(Indiv indiv) {
		if (indiv.myInfection()==null) {
			throw new RuntimeException("Indiv" + indiv + "does not have an infection!");
		}
		detected++;
		detectedList.add(indiv.myInfection());
		
		if (indiv.getGender().equals("f")) {
		} else if (indiv.getGender().equals("nb")) {
		} else if (indiv.getSubPop().equals("msw")) {
		} else if (indiv.getSubPop().equals("msmw")) {
		} else {
		}
		
	}
	
	public void recordNewDetectedAndSymptoms(Indiv indiv) {
		detectedAndSymptoms++;
		
		if (indiv.getGender().equals("f")) {
		} else if (indiv.getGender().equals("nb")) {
		} else if (indiv.getSubPop().equals("msw")) {
		} else if (indiv.getSubPop().equals("msmw")) {
		} else {
		}
		
		
		
		
	}
	
	public void recordNewDetectedThruScreen(Indiv indiv) {
		detectedThruScreen++;
		recordNewDetected(indiv);
		
		if (indiv.getGender().equals("f")) {
		} else if (indiv.getGender().equals("nb")) {
		} else if (indiv.getSubPop().equals("msw")) {
		} else if (indiv.getSubPop().equals("msmw")) {
		} else {
		}
		
		
		
	}
	
	public void clearDetectedList() {
		this.detectedList = new ArrayList<Infection>();
	}
	
	
	public double getPrevalence() {
		double prev = this.prevalence;
		return prev;
	}
	
	public double getIncidence() {
		double inc = this.incidence;
		return inc;
	}
	
	
	public int getNewCases() {
		return this.newCases;
	}
	
	public List<Infection> getDetectedList(){
		return this.detectedList;
	}

	public int getSoughtCare() {
		return this.soughtCare;
	}
	
	public double getTreatments() {
		return this.treatments;
	}
	
	public CostCalc getCostCalc() {
		return this.costCalc;
	}
	
	public boolean getAddedX() {
		return this.addedX;
	}
	
	public Parameters getParameters() {
		return parameters;
	}
	
	public int detected() {
		return detected;
	}
	
}
