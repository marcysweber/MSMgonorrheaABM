/**
 * 
 */
package msmOnlyModel;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
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
	
	private int incidence;
	//incidence is the NEW infected cases over some time unit (*100,000)
	
	private int newCases;
	private int newCasesMSM;
	private List<Infection> newCasesList;
	
	private int newResistACases;
	private double resistAIncidence;
	
	private int newResistBCases;
	private double resistBIncidence;
	
	
	private int newResistBothCases;
	private double resistBothIncidence;
	
	private double symptomProportion;
	private double treatments;
	private double failedTreatments;
	private int developedResistance;
	
	private int detected;
	private List<Infection> detectedList;
	
	private int diagnosticTests;
	private int strainTests;
	private int visitsToClinic;
	
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
	private int recoveredNaturallyDuringTreatment;
	private int recoveredNaturally;
	private int reInfected;
	private int reInfectedDuringTreatment;
	private int casesEpidydimitis;
	private int casesDGI;
	private int casesBothSequelae;
	private int checksForSequelae;
	private int checksForSequelaeUnDetect;
	private int checksForSequelaeRecovNat;
	private int checksForSequelaeFailedA;
	private int checksForSequelaeFailedB;

	
	
	
	private CostCalc costCalc;
	
	
	public Observer(Parameters parameters, CustomFileOutput outputter, int seed, double prev, int inc, ISchedule schedule) {
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
		this.newCasesList = new ArrayList<Infection>();
		this.newResistACases = 0;
		this.resistAIncidence = 0;
		this.newResistBCases = 0;
		this.resistBIncidence = 0;
		this.newResistBothCases = 0;
		this.resistBothIncidence = 0;
		this.treatments = 0;
		this.failedTreatments = 0;
		this.developedResistance = 0;
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
		this.recoveredNaturally = 0;
		this.recoveredNaturallyDuringTreatment = 0;
		this.reInfected = 0;
		this.reInfectedDuringTreatment = 0;
		
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
	
		double tick = tickNow();
		
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

				this.parameters.getDouble("realisticRandom"),
				this.parameters.getDouble("realisticTOC"),
				this.parameters.getDouble("realisticDST"),
				
				
				
				this.parameters.getInteger("infected_count_init"),
				this.parameters.getDouble("propHighRisk"),
				
				this.parameters.getDouble("transmissionMSM"),
				
				
				this.parameters.getDouble("recovery_time"),
				
				this.parameters.getDouble("prob_symptomatic_msm"), 
		
						
				this.parameters.getDouble("screen_interval_MSM"),
				

				this.parameters.getDouble("delay_to_seek_care_msm"), 
			

				this.parameters.getDouble("delay_to_retreatment_msm"),
				
				this.parameters.getDouble("assortativity"),
				this.parameters.getDouble("risk_group_transfer_prop"),
				this.parameters.getDouble("risk_group_transmission_ratio"),


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
				this.developedResistance,
				this.detected, 
				this.detectedAndSymptoms,
				this.detectedThruScreen, 
				this.strainTests,
				this.diagnosticTests,
				this.visitsToClinic,
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
				this.recoveredNaturally,
				this.recoveredNaturallyDuringTreatment,
				this.reInfected,
				this.reInfectedDuringTreatment,
				this.casesEpidydimitis,
				this.casesDGI,
				this.casesBothSequelae,
				this.checksForSequelae,
				
				this.checksForSequelaeUnDetect,
				this.checksForSequelaeRecovNat,
				this.checksForSequelaeFailedA,
				this.checksForSequelaeFailedB,

				
				
				surveillanceResultA, 
				surveillanceResultB,
				surveillanceResultBoth,
				surveillance.getSwitchToB(),
				surveillance.getSwitchToX(),
				costCalc.getMonetaryCost(),
				costCalc.getQALYsLost(),
				
				calcLowRiskPrev(), 
				calcHighRiskPrev(),
				(int) population.highRiskCount()
				
				);
		
		if (!this.counterfactual.contains("sweep")) {
			//outputter.transmissionRateOutput(newCasesList);
		}
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
		this.developedResistance = 0;
		this.detected = 0;
		
		this.newCasesList = new ArrayList<Infection>();

		
		this.detectedAndSymptoms = 0;
		this.soughtCare = 0;
		this.detectedThruScreen = 0;
		this.diagnosticTests = 0;
		this.strainTests = 0;
		this.visitsToClinic = 0;
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
		this.recoveredNaturally = 0;
		this.recoveredNaturallyDuringTreatment = 0;
		this.reInfected = 0;
		this.reInfectedDuringTreatment = 0;
		this.casesBothSequelae = 0;
		this.casesDGI = 0;
		this.casesEpidydimitis = 0;
		this.checksForSequelae = 0;
		this.checksForSequelaeUnDetect = 0;
		this.checksForSequelaeRecovNat = 0;
		this.checksForSequelaeFailedA = 0;
		this.checksForSequelaeFailedB = 0;


	}
	
	public void processCompleteInfection(Infection infection) throws Exception {
		//System.out.println("processing");
		
		newCases++;
		newCasesList.add(infection);
		
		if (infection.resistantToA() && infection.resistantToB()) {
			newResistBothCases++;
		} else if (infection.resistantToA()) {
			newResistACases++;
		} else if (infection.resistantToB()) {
			newResistBCases++;
		}
		
		if (infection.isDetected()) {
			detected++;

				if (infection.symptoms()) {
					detectedAndSymptoms++;
				} else if (infection.screened()) {
					detectedThruScreen++;
				} else {
					System.out.println("detected case neither symptomatic nor screened");
				}
			

			Map<String, Boolean> finalOutcomes = new HashMap<String, Boolean>()
			{{
			     put("SucceededA", infection.succeededA());
			     put("SucceededB", infection.succeededB());
			     put("SucceededX", infection.succeededX());
			     put("SucceededE", infection.succeededE());
			     put("DevelopedResistance", infection.developedResistance());
			     put("ReInfected", infection.reInfected() && infection.inTreatment());
			     put("RecoveredNaturally", infection.recoveredNaturally()&& infection.inTreatment());
			     put("UnknownFailedTreatment", infection.failedTreatment());
			}};
			
			int outcomes = finalOutcomes.values().stream().map(a -> a ? 1 : 0).reduce(0, (a,b) -> a+b);
			
			if (outcomes != 1) {
				if (outcomes == 0 && !infection.failedTreatment()) {
					System.out.println(finalOutcomes);
				} else {
				System.out.println(finalOutcomes);
				}
			} 
			
		}
			
		
		
	
		if (infection.soughtCare()) {
			soughtCare++;
		}
		
		attemptTreatmentsA+=infection.attemptedA();
		
		attemptTreatmentsB+=infection.attemptedB();
		
		
		//final outcomes
		if (infection.succeededA()) {
			successTreatmentsA++;
			
			if (!infection.isDetected()) {
				//System.out.println("outcome without detection - a!");
			}
			
		} else if (infection.succeededB()) {
			successTreatmentsB++;
			if (!infection.isDetected()) {
				//System.out.println("outcome without detection - b!");
			}
		} else if (infection.succeededX()) {
			attemptTreatmentsX++;
			successTreatmentsX++;
			if (!infection.isDetected()) {
				//System.out.println("outcome without detection - x!");
			}
		} else if (infection.succeededE()) {
			usageE++;;
			if (!infection.isDetected()) {
				//System.out.println("outcome without detection - e!");
			}
		} else if (infection.recoveredNaturally()) {
			recoveredNaturally++;
			if (infection.inTreatment()) {
				recoveredNaturallyDuringTreatment++;
				if (!infection.isDetected()) {
					//System.out.println("outcome without detection - RN!");
				}
			}
		} else if (infection.developedResistance()) {
			developedResistance++;
		} else if (infection.reInfected()) {
			reInfected++;
			if (infection.inTreatment()) {
				reInfectedDuringTreatment++;
				if (!infection.isDetected()) {
					//System.out.println("outcome without detection - RI!");
				}
			}
		} else {
			throw new Exception("Invalid outcome reported to observer!");
		}
		
		//System.out.println(soughtCare);
		
		
		costCalc.strainTestCost(infection.strainTests());
		this.strainTests+=infection.strainTests();
		
		costCalc.testCost(infection.diagnosticTests());
		this.diagnosticTests+=infection.diagnosticTests();
		
		costCalc.careCost(infection.visitsToClinic());
		this.visitsToClinic+=infection.visitsToClinic();
	
		costCalc.treatmentDrugACost(infection.attemptedA());
		
		costCalc.treatmentDrugBCost(infection.attemptedB());
		
		if (infection.succeededX()) {
		costCalc.treatmentDrugXCost(1);
		}
		if (infection.succeededE()) {
			costCalc.treatmentDrugECost(1);
			}
		
		if (infection.symptoms()) {
			costCalc.symptomaticQALYsLost(infection.duration());
		}
		
		String sequelae = infection.getSequelae();
		if (sequelae.contains("epi")) {
			casesEpidydimitis++;
		} else if (sequelae.contains("dgi")) {
			casesDGI++;
		} else if (sequelae.contains("both")){
			casesBothSequelae++;
		}
		
		checksForSequelae += infection.accessCheckedForSequelae();
		checksForSequelaeUnDetect += infection.accessCheckedForSequelaeUnDetect();
		checksForSequelaeRecovNat += infection.accessCheckedForSequelaeRecovNat();
		checksForSequelaeFailedA += infection.accessCheckedForSequelaeFailedA();
		checksForSequelaeFailedB += infection.accessCheckedForSequelaeFailedB();

		
		costCalc.recordSequelae(sequelae);
		
	}
	
	public void processUndetectInfection(Infection infection) {
		if (infection.isDetected()) {
			detected++;
			
			if (infection.symptoms()) {
				detectedAndSymptoms++;
			} else if (infection.screened()) {
				detectedThruScreen++;
			} else {
				System.out.println("detected case neither symptomatic nor screened");
			}
			
			if (infection.failedTreatment()) {
				failedTreatments++;
				
				
				
			} else {
				System.out.println("undetect that was not a failed treatment!");
			}
			
		}
	}
	
	public double calcPrev() { //real time
		
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
	
	public double calcLowRiskPrev() {
		return 100.0 * population.lowRiskInfected().count() / population.lowRiskCount();
 	}
	
	public double calcHighRiskPrev() {
		return 100.0 * population.highRiskInfected().count() / population.highRiskCount();
	}
	
	
	
	public double calcPrevMSM() {
		//ideally we would define MSM behaviorally within the model. for now i am using a preference threshold
		return population.msmInfected().count() / population.msmCount() * 100;
	}


	

	
	public int calcInc() {
		
		int newInc = 0;
		newInc = this.newCases; 
		
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
	
	public int ongoingTreatments() {
		int ongoing = (int) population.allIndivs().filter(ind -> ((Indiv) ind).inTreatment() == true).count();
		
		
		
		return ongoing;
	}
	
	
	
	
	
	
	//@ScheduledMethod(start = 4, interval = 4, priority = 1)
	public void checkStopCondition() {
//		
//		long infectedcount = .getObjectsAsStream(Indiv.class)
//				.filter(indiv -> ((Indiv) indiv).getState()==1)
//				.count();
		double prev = calcPrev();
		double lowRiskPrev = calcLowRiskPrev();
		double highRiskPrev = calcHighRiskPrev();
		
		if (counterfactual.equals("sweep")){ //strict constraints for sweeps
			if (prev > 10.0 || prev < 0.05) {
				System.out.print("I should stop now!!! Overall prev was too extreme.");
				//ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
				schedule.setFinishing(true);
			
			} else if (lowRiskPrev > 4.0) {
				System.out.print("I should stop now!!! Low risk prev was too high.");
				//ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
				schedule.setFinishing(true);
			} else if (highRiskPrev < 5.0 || highRiskPrev > 25.0) {
				System.out.print("I should stop now!!! High Risk prev was too extreme.");
				//ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
				schedule.setFinishing(true);
			}
				
				
				
				
		} else if (prev > 99.0) {		//if it's not a sweep, only exclude most extreme trajectories.

				System.out.print("I should stop now!!! ");
				//ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();
				schedule.setFinishing(true);
			}
		
//		
//		if (attemptTreatmentsX != successTreatmentsX) {
//			System.out.println("BAD");
//		}
		
		
	
	}

	public void addToDetectedList(Infection infection) {
		detectedList.add(infection);
	}
	
	
	
	public SurveillanceProgram getSurveillance() {
		return surveillanceProgram;
	}
	
	public void recordSurveillanceStrainTests(int count) {
		strainTests += count;
	}
	
	public void addX() {
		this.addedX = true;
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
	
	public int successA() {
		return successTreatmentsA;
	}
	
	public int attemptsX() {
		return attemptTreatmentsX;
	}
	
	public int sucessesX() {
		return successTreatmentsX;
	}
	
	public double tickNow() {
		return schedule.getTickCount();
	}
	
	
	public int getChecksForSequelae() {
		return checksForSequelae;
	}
	
public int getCasesDGI() {
		return casesDGI;
	}

public int getCasesEpi() {
	return casesEpidydimitis;
}

public int getCasesBothSequelae() {
	return casesBothSequelae;
}
}
