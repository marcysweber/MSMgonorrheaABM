/**
 * 
 */
package simpleSIR;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import cern.jet.random.Uniform;
import repast.simphony.engine.schedule.ISchedule;
import repast.simphony.engine.schedule.ScheduledMethod;
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
	private int newCasesMSMW;
	private int newCasesMSW;
	private int newCasesW;
	private int newCasesNB;
	
	private double anyResistPrevalence;
	private double anyResistIncidence;
	
	private int newResistACases;
	private int newResistACasesMSM;
	private int newResistACasesMSMW;
	private int newResistACasesMSW;
	private int newResistACasesW;
	private int newResistACasesNB;
	
	private double resistAPrevalence;
	private double resistAIncidence;
	
	private int newResistBCases;
	private int newResistBCasesMSM;
	private int newResistBCasesMSMW;
	private int newResistBCasesMSW;
	private int newResistBCasesW;
	private int newResistBCasesNB;
	private double resistBPrevalence;
	private double resistBIncidence;
	
	
	private int newResistBothCases;
	private int newResistBothCasesMSM;
	private int newResistBothCasesMSMW;
	private int newResistBothCasesMSW;
	private int newResistBothCasesW;
	private int newResistBothCasesNB;
	private double resistBothPrevalence;
	private double resistBothIncidence;
	
	private double symptomProportion;
	private double treatments;
	private double failedTreatments;
	
	private int detected;
	private List<Infection> detectedList;
	
	private int detectedM;
	private List<Infection> detectedMList; //for GISP?
	
	private int detectedMSM;
	private List<Infection> detectedListMSM;
	
	private int detectedMSMW;
	private List<Infection> detectedListMSMW;
	
	private int detectedMSW;
	private List<Infection> detectedListMSW;
	
	private int detectedW;
	private List<Infection> detectedListW;
	
	private int detectedNB;
	private List<Infection> detectedListNB;

	private int detectedAndSymptoms;
	private int detectedAndSymptomsMSM;
	private int detectedAndSymptomsMSMW;
	private int detectedAndSymptomsMSW;
	private int detectedAndSymptomsW;
	private int detectedAndSymptomsNB;

	private int soughtCare;
	private int soughtCareMSM;
	private int soughtCareMSMW;
	private int soughtCareMSW;
	private int soughtCareW;
	private int soughtCareNB;
	
	private int detectedThruScreen;
	private int detectedThruScreenMSM;
	private int detectedThruScreenMSMW;
	private int detectedThruScreenMSW;
	private int detectedThruScreenW;
	private int detectedThruScreenNB;

	private int knownFailedTreatments;
	private int knownFailedTreatmentsMSM;
	private int knownFailedTreatmentsMSMW;
	private int knownFailedTreatmentsMSW;
	private int knownFailedTreatmentsW;
	private int knownFailedTreatmentsNB;

	private int knownFailedTreatmentsA;
	private int knownFailedTreatmentsAMSM;
	private int knownFailedTreatmentsAMSMW;
	private int knownFailedTreatmentsAMSW;
	private int knownFailedTreatmentsAW;
	private int knownFailedTreatmentsANB;

	private int knownFailedTreatmentsB;
	private int knownFailedTreatmentsBMSM;
	private int knownFailedTreatmentsBMSMW;
	private int knownFailedTreatmentsBMSW;
	private int knownFailedTreatmentsBW;
	private int knownFailedTreatmentsBNB;

	private int knownFailedTreatmentsBoth;
	private int knownFailedTreatmentsBothMSM;
	private int knownFailedTreatmentsBothMSMW;
	private int knownFailedTreatmentsBothMSW;
	private int knownFailedTreatmentsBothW;
	private int knownFailedTreatmentsBothNB;

	private int successTreatmentsA;
	private int successTreatmentsAMSM;
	private int successTreatmentsAMSMW;
	private int successTreatmentsAMSW;
	private int successTreatmentsAW;
	private int successTreatmentsANB;

	private int successTreatmentsB;
	private int successTreatmentsBMSM;
	private int successTreatmentsBMSMW;
	private int successTreatmentsBMSW;
	private int successTreatmentsBW;
	private int successTreatmentsBNB;

	private int successTreatmentsX;
	private int successTreatmentsXMSM;
	private int successTreatmentsXMSMW;
	private int successTreatmentsXMSW;
	private int successTreatmentsXW;
	private int successTreatmentsXNB;
	
	private int attemptTreatmentsA;
	private int attemptTreatmentsAMSM;
	private int attemptTreatmentsAMSMW;
	private int attemptTreatmentsAMSW;
	private int attemptTreatmentsAW;
	private int attemptTreatmentsANB;

	private int attemptTreatmentsB;
	private int attemptTreatmentsBMSM;
	private int attemptTreatmentsBMSMW;
	private int attemptTreatmentsBMSW;
	private int attemptTreatmentsBW;
	private int attemptTreatmentsBNB;

	private int attemptTreatmentsX;
	private int attemptTreatmentsXMSM;
	private int attemptTreatmentsXMSMW;
	private int attemptTreatmentsXMSW;
	private int attemptTreatmentsXW;
	private int attemptTreatmentsXNB;
	
	private int usageE;
	private int usageEMSM;
	private int usageEMSMW;
	private int usageEMSW;
	private int usageEW;
	private int usageENB;


	
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
		this.anyResistIncidence = 0;
		this.anyResistPrevalence = 0;
		this.newResistACases = 0;
		this.resistAPrevalence = 0;
		this.resistAIncidence = 0;
		this.newResistBCases = 0;
		this.resistBPrevalence = 0;
		this.resistBIncidence = 0;
		this.newResistBothCases = 0;
		this.resistBothPrevalence = 0;
		this.resistBothIncidence = 0;
		this.treatments = 0;
		this.failedTreatments = 0;
		this.detected = 0;
		this.detectedList = new ArrayList<Infection>();
		this.detectedAndSymptoms = 0;
		this.soughtCare = 0;
		this.detectedThruScreen = 0;
		this.knownFailedTreatments = 0;
		this.knownFailedTreatmentsA = 0;
		this.knownFailedTreatmentsB = 0;
		this.knownFailedTreatmentsBoth = 0;
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
		double popSize = population.totalSize();

		//each time step, update the Observer's values
		this.prevalence = calcPrev(popSize);
		this.incidence = calcInc(popSize);  
		
		this.anyResistPrevalence = calcAllStrainPrev(popSize);
		this.anyResistIncidence = calcAllStrainInc(popSize);

		this.resistAPrevalence = calcResistAPrev(popSize);
		this.resistAIncidence = calcResistAInc(popSize);
		this.resistBPrevalence = calcResistBPrev(popSize);
		this.resistBIncidence = calcResistBInc(popSize);
		this.resistBothPrevalence = calcResistBothPrev(popSize);
		this.resistBothIncidence = calcResistBothInc(popSize);

		this.symptomProportion = calcSymptomProportion();
	
		double tick = schedule.getTickCount();
		
		SurveillanceProgram surveillance = getSurveillance();
		double surveillanceResultA = surveillance.calcDetectedResistantA();
		double surveillanceResultB = surveillance.calcDetectedResistantB();
		double surveillanceResultBoth = surveillance.calcDetectedResistantBoth();

		if (counterfactual.contains("GISP")) {
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
				this.parameters.getInteger("infected_count_init"),
				
				this.parameters.getDouble("transmissionMSM"),
				this.parameters.getDouble("transmissionMSW"),
				this.parameters.getDouble("transmissionF"),
				
				this.parameters.getDouble("recovery_lambda"),
				
				this.parameters.getDouble("prob_symptomatic_msm"), 
				this.parameters.getDouble("prob_symptomatic_msw"), 
				this.parameters.getDouble("prob_symptomatic_f"), 
						
				this.parameters.getDouble("screen_interval_MSM"),
				this.parameters.getDouble("screen_interval_MSW"),
				this.parameters.getDouble("screen_interval_W"),

				this.parameters.getDouble("delay_to_seek_care_msm"), 
				this.parameters.getDouble("delay_to_seek_care_msw"), 
				this.parameters.getDouble("delay_to_seek_care_f"), 

				this.parameters.getDouble("delay_to_retreatment_msm"),
				this.parameters.getDouble("delay_to_retreatment_msw"),
				this.parameters.getDouble("delay_to_retreatment_f"),

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
				costCalc.getQALYsLost(),
				
				//MSM
				population.msmCount(),
				calcPrevMSM(),
				calcIncMSM(),
				this.detectedMSM,
				this.detectedAndSymptomsMSM,
				this.newResistACasesMSM,
				this.newResistBCasesMSM,
				this.newResistBothCasesMSM,
 				this.successTreatmentsAMSM, 
 				this.successTreatmentsBMSM,
 				this.successTreatmentsXMSM,
 				this.attemptTreatmentsAMSM,
 				this.attemptTreatmentsBMSM,
 				this.attemptTreatmentsXMSM,
 				this.usageEMSM,
    			 costCalc.getMonetaryCostMSM(),
    			 costCalc.getQALYsLostMSM(),

				//MSMW
    			 population.msmwCount(),
				calcPrevMSMW(),
				calcIncMSMW(),
				this.detectedMSMW,
				this.detectedAndSymptomsMSMW,
				this.newResistACasesMSMW,
				this.newResistBCasesMSMW,
				this.newResistBothCasesMSMW,
				this.successTreatmentsAMSMW, 
 				this.successTreatmentsBMSMW,
 				this.successTreatmentsXMSMW,
 				this.attemptTreatmentsAMSMW,
 				this.attemptTreatmentsBMSMW,
 				this.attemptTreatmentsXMSMW,
 				this.usageEMSMW,
 				 costCalc.getMonetaryCostMSMW(),
    			 costCalc.getQALYsLostMSMW(),

				
				//MSW
				population.mswCount(),
				calcPrevMSW(),
				calcIncMSW(),
				this.detectedMSW,
				this.detectedAndSymptomsMSW,
				this.newResistACasesMSW,
				this.newResistBCasesMSW,
				this.newResistBothCasesMSW,
				this.successTreatmentsAMSW, 
 				this.successTreatmentsBMSW,
 				this.successTreatmentsXMSW,
 				this.attemptTreatmentsAMSW,
 				this.attemptTreatmentsBMSW,
 				this.attemptTreatmentsXMSW,
 				this.usageEMSW,
 				 costCalc.getMonetaryCostMSW(),
    			 costCalc.getQALYsLostMSW(),
				
				//W
				population.wCount(),
				calcPrevW(),
				calcIncW(),
				this.detectedW,
				this.detectedAndSymptomsW,
				this.newResistACasesW,
				this.newResistBCasesW,
				this.newResistBothCasesW,
				this.successTreatmentsAW, 
 				this.successTreatmentsBW,
 				this.successTreatmentsXW,
 				this.attemptTreatmentsAW,
 				this.attemptTreatmentsBW,
 				this.attemptTreatmentsXW,
 				this.usageEW,
 				 costCalc.getMonetaryCostW(),
    			 costCalc.getQALYsLostW(),
    			 
    			 
				//NB
				population.nbCount(),
				calcPrevNB(),
				calcIncNB(),
				this.detectedNB,
				this.detectedAndSymptomsNB,
				this.newResistACasesNB,
				this.newResistBCasesNB,
				this.newResistBothCasesNB,
				this.successTreatmentsANB, 
 				this.successTreatmentsBNB,
 				this.successTreatmentsXNB,
 				this.attemptTreatmentsANB,
 				this.attemptTreatmentsBNB,
 				this.attemptTreatmentsXNB,
 				this.usageENB,
 				 costCalc.getMonetaryCostNB(),
    			 costCalc.getQALYsLostNB()

				
				);
		
		clearObserver();
		costCalc.clearAnnualCosts();
		
		//System.out.println(addedX);

	}
	
	public void clearObserver() {
		
		this.newCases = 0; //now that incidence has been calculated, clear newCases
		this.newCasesMSM = 0;
		this.newCasesMSMW = 0;
		this.newCasesMSW = 0;
		this.newCasesW = 0;
		this.newCasesNB = 0;
		
		
		
		this.newResistACases = 0;
		this.newResistACasesMSM = 0;
		this.newResistACasesMSMW = 0;
		this.newResistACasesMSW = 0;
		this.newResistACasesW = 0;
		this.newResistACasesNB = 0;

		this.newResistBCases = 0;
		this.newResistBCasesMSM = 0;
		this.newResistBCasesMSMW = 0;
		this.newResistBCasesMSW = 0;
		this.newResistBCasesW = 0;
		this.newResistBCasesNB = 0;

		
		this.newResistBothCases = 0;
		this.newResistBothCasesMSM = 0;
		this.newResistBothCasesMSMW = 0;
		this.newResistBothCasesMSW = 0;
		this.newResistBothCasesW = 0;
		this.newResistBothCasesNB = 0;

		
		
		this.treatments = 0;
		this.failedTreatments = 0;
		this.detected = 0;
		this.detectedMSM = 0;
		this.detectedMSMW = 0;
		this.detectedMSW = 0;
		this.detectedW = 0;
		this.detectedNB = 0;

		
		this.detectedList = new ArrayList<Infection>();
		this.detectedListMSM = new ArrayList<Infection>();
		this.detectedListMSMW = new ArrayList<Infection>();
		this.detectedListMSW = new ArrayList<Infection>();
		this.detectedListW = new ArrayList<Infection>();
		this.detectedListNB = new ArrayList<Infection>();

		
		this.detectedAndSymptoms = 0;
		this.detectedAndSymptomsMSM = 0;
		this.detectedAndSymptomsMSMW = 0;
		this.detectedAndSymptomsMSW = 0;
		this.detectedAndSymptomsW = 0;
		this.detectedAndSymptomsNB = 0;
		
		
		this.soughtCare = 0;
		this.soughtCareMSM = 0;
		this.soughtCareMSMW = 0;
		this.soughtCareMSW = 0;
		this.soughtCareW = 0;
		this.soughtCareNB = 0;

		this.detectedThruScreen = 0;
//		this.knownFailedTreatments = 0;
//		this.knownFailedTreatmentsA = 0;
//		this.knownFailedTreatmentsB = 0;
//		this.knownFailedTreatmentsBoth = 0;
		this.successTreatmentsA = 0;
		this.successTreatmentsAMSM = 0;
		this.successTreatmentsAMSMW = 0;
		this.successTreatmentsAMSW = 0;
		this.successTreatmentsAW = 0;
		this.successTreatmentsANB = 0;

		this.successTreatmentsB = 0;
		this.successTreatmentsBMSM = 0;
		this.successTreatmentsBMSMW = 0;
		this.successTreatmentsBMSW = 0;
		this.successTreatmentsBW = 0;
		this.successTreatmentsBNB = 0;

		this.successTreatmentsX = 0;
		this.successTreatmentsXMSM = 0;
		this.successTreatmentsXMSMW = 0;
		this.successTreatmentsXMSW = 0;
		this.successTreatmentsXW = 0;
		this.successTreatmentsXNB = 0;


		this.attemptTreatmentsA = 0;
		this.attemptTreatmentsAMSM = 0;
		this.attemptTreatmentsAMSMW = 0;
		this.attemptTreatmentsAMSW = 0;
		this.attemptTreatmentsAW = 0;
		this.attemptTreatmentsANB = 0;

		
		this.attemptTreatmentsB = 0;
		this.attemptTreatmentsBMSM = 0;
		this.attemptTreatmentsBMSMW = 0;
		this.attemptTreatmentsBMSW = 0;
		this.attemptTreatmentsBW = 0;
		this.attemptTreatmentsBNB = 0;

		this.attemptTreatmentsX = 0;
		this.attemptTreatmentsXMSM = 0;
		this.attemptTreatmentsXMSMW = 0;
		this.attemptTreatmentsXMSW = 0;
		this.attemptTreatmentsXW = 0;
		this.attemptTreatmentsXNB = 0;

		this.usageE = 0;
		this.usageEMSM = 0;
		this.usageEMSMW = 0;
		this.usageEMSW = 0;
		this.usageEW = 0;
		this.usageENB = 0;

	}
	
	public double calcPrev(double popSize) {
 
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
	
	public double calcPrevMSMW() {
		
		//ideally we would define MSM behaviorally within the model. for now i am using a preference threshold
		//return population.msmwInfected().count() / population.msmwCount();
		return 0.0;
	}
	
	public double calcPrevMSW() {
		return population.mswInfected().count() / population.mswCount() * 100;
		
	}
	
	public double calcPrevW() {
		return population.wInfected().count() / population.wCount() * 100;
	}
	
	public double calcPrevNB() {
		//return population.nbInfected().count() / population.nbCount();
		return 0.0;
	}
	

	
	public double calcInc(double popSize) {
		double newInc = 0;
		newInc = (this.newCases / popSize) * 100000.0; 
		
		return newInc;
	}
	
	
	
	public double calcIncMSM() {
		return (this.newCasesMSM / population.msmCount()) * 100000;
	}
	
	public double calcIncMSMW() {
		return (this.newCasesMSMW / population.msmwCount()) * 100000;
	}
	
	public double calcIncMSW() {
		return (this.newCasesMSW / population.mswCount()) * 100000;
	}
	
	public double calcIncW() {
		return (this.newCasesW / population.wCount()) * 100000;
	}
	
	public double calcIncNB() {
		return (this.newCasesNB / population.nbCount()) * 100000;
	}
	
	
	
	
	
	public double calcAllStrainPrev(double popSize) {
		
		
		//Stream<Object> infectious = .getObjectsAsStream(Indiv.class); //grabs all objects of class Indiv
		List<Object> withStrain = population.allIndivs()
				.filter(infection -> ((Indiv) infection).getState()==1)
				.filter(infection -> !((Indiv) infection).getStrain().equals("none"))
				.collect(Collectors.toList());

		int countStrain = withStrain.size();
		
		double newPrev = (countStrain / popSize) * 100.0;
		
		return newPrev;
		
	}
	
	
	public double calcAllStrainInc(double popSize) {
		double newInc = 0;
		newInc = ((this.newResistACases + this.newResistBCases - this.newResistBothCases) / popSize) * 100000.0; 
		
		return newInc;
	}
	
	
	public double calcResistAPrev(double popSize) {
		
		
		//Stream<Object> infectious = .getObjectsAsStream(Indiv.class); //grabs all objects of class Indiv
		List<Object> withStrain = population.allIndivs()
				.filter(infection -> ((Indiv) infection).getState()==1)
				.filter(infection -> ((Indiv) infection).getStrain().equals("A")||((Indiv) infection).getStrain().equals("Both"))
				.collect(Collectors.toList());

		int countStrain = withStrain.size();
		
		double newPrev = (countStrain / popSize) * 100.0;
		
		return newPrev;
		
	}
	
	
	
	
	
	public double calcResistAInc(double popSize) {
		double newInc = 0;
		newInc = ((this.newResistACases) / popSize) * 100000.0; 
		
		return newInc;
	}
	
	
	public double calcResistBPrev(double popSize) {
		
		
	//	Stream<Object> infectious = .getObjectsAsStream(Indiv.class); //grabs all objects of class Indiv
		List<Object> withStrain = population.allIndivs()
				.filter(infection -> ((Indiv) infection).getState()==1)
				.filter(infection -> ((Indiv) infection).getStrain().equals("B")||((Indiv) infection).getStrain().equals("Both"))
				.collect(Collectors.toList());

		int countStrain = withStrain.size();
		
		double newPrev = (countStrain / popSize) * 100.0;
		
		return newPrev;
		
	}
	
	
	public double calcResistBInc(double popSize) {
		double newInc = 0;
		newInc = ((this.newResistBCases) / popSize) * 100000.0; 
		
		return newInc;
	}
	
	
	public double calcResistBothPrev(double popSize) {
		
		
		//Stream<Object> infectious = .getObjectsAsStream(Indiv.class); //grabs all objects of class Indiv
		List<Object> withStrain = population.allIndivs()
				.filter(infection -> ((Indiv) infection).getState()==1)
				.filter(infection -> ((Indiv) infection).getStrain().equals("Both"))
				.collect(Collectors.toList());

		int countStrain = withStrain.size();
		
		double newPrev = (countStrain / popSize) * 100.0;
		
		return newPrev;
		
	}
	
	
	public double calcResistBothInc(double popSize) {
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
		double prev = calcPrev(100000);
		
		if ((prev > 10.0 || prev < 0.05) && counterfactual.equals("none")) {
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
				//W
				this.newCasesW++;
			} else if (potentialNewCase.getGender().equals("nb")) {
				//NB
				this.newCasesNB++;
			} else if (potentialNewCase.getSubPop().equals("msw")) {
				//MSW
				this.newCasesMSW++;
			} else if (potentialNewCase.getSubPop().equals("msmw")) {
				//MSMW
				this.newCasesMSMW++;
			} else {
				//MSM
				this.newCasesMSM++;
			}
			
			
			if (potentialNewCase.myInfection().getStrain().equals("A")) {
				this.newResistACases++;
				
				if (potentialNewCase.getGender().equals("f")) {
					//W
					this.newResistACasesW++;
				} else if (potentialNewCase.getGender().equals("nb")) {
					//NB
					this.newResistACasesNB++;
				} else if (potentialNewCase.getSubPop().equals("msw")) {
					//MSW
					this.newResistACasesMSW++;
				} else if (potentialNewCase.getSubPop().equals("msmw")) {
					//MSMW
					this.newResistACasesMSMW++;
				} else {
					//MSM
					this.newResistACasesMSM++;
				}
				
			} else if (potentialNewCase.myInfection().getStrain().equals("B")) {
				this.newResistBCases++;
				
				if (potentialNewCase.getGender().equals("f")) {
					//W
					this.newResistBCasesW++;
				} else if (potentialNewCase.getGender().equals("nb")) {
					//NB
					this.newResistBCasesNB++;
				} else if (potentialNewCase.getSubPop().equals("msw")) {
					//MSW
					this.newResistBCasesMSW++;
				} else if (potentialNewCase.getSubPop().equals("msmw")) {
					//MSMW
					this.newResistBCasesMSMW++;
				} else {
					//MSM
					this.newResistBCasesMSM++;
				}
				
			} else if (potentialNewCase.myInfection().getStrain().equals("Both")) {
				//this.newResistACases++;
				//this.newResistBCases++;
				this.newResistBothCases++;
				
				if (potentialNewCase.getGender().equals("f")) {
					//W
					this.newResistBothCasesW++;
				} else if (potentialNewCase.getGender().equals("nb")) {
					//NB
					this.newResistBothCasesNB++;
				} else if (potentialNewCase.getSubPop().equals("msw")) {
					//MSW
					this.newResistBothCasesMSW++;
				} else if (potentialNewCase.getSubPop().equals("msmw")) {
					//MSMW
					this.newResistBothCasesMSMW++;
				} else {
					//MSM
					this.newResistBothCasesMSM++;
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
			//W
			this.soughtCareW++;
		} else if (indiv.getGender().equals("nb")) {
			//NB
			this.soughtCareNB++;
		} else if (indiv.getSubPop().equals("msw")) {
			//MSW
			this.soughtCareMSW++;
		} else if (indiv.getSubPop().equals("msmw")) {
			//MSMW
			this.soughtCareMSMW++;
		} else {
			//MSM
			this.soughtCareMSM++;
		}
		
	}
	
	public void recordNewTreatment(Indiv indiv) {
		 treatments++;

	 }
	
	public void recordNewFailedTreatment(Indiv indiv) {
		failedTreatments++;
		
	}
	
	public void recordNewKnownFailedTreatment(Indiv indiv) {
		knownFailedTreatments++;
		
	}
	
	public void recordNewKnownFailedTreatmentA(Indiv indiv) {
		knownFailedTreatmentsA++;
	}
	
	public void recordNewKnownFailedTreatmentB(Indiv indiv) {
		knownFailedTreatmentsB++;
	}
	
	public void recordNewKnownFailedTreatmentBoth(Indiv indiv) {
		knownFailedTreatmentsBoth++;
	}
	
	public void recordNewSuccessTreatmentA(Indiv indiv) {
		successTreatmentsA++;
		if (indiv.getGender().equals("f")) {
			//W
			this.successTreatmentsAW++;
		} else if (indiv.getGender().equals("nb")) {
			//NB
			this.successTreatmentsANB++;
		} else if (indiv.getSubPop().equals("msw")) {
			//MSW
			this.successTreatmentsAMSW++;
		} else if (indiv.getSubPop().equals("msmw")) {
			//MSMW
			this.successTreatmentsAMSMW++;
		} else {
			//MSM
			this.successTreatmentsAMSM++;
		}
	}
	
	public void recordNewSuccessTreatmentB(Indiv indiv) {
		successTreatmentsB++;
		if (indiv.getGender().equals("f")) {
			//W
			this.successTreatmentsBW++;
		} else if (indiv.getGender().equals("nb")) {
			//NB
			this.successTreatmentsBNB++;
		} else if (indiv.getSubPop().equals("msw")) {
			//MSW
			this.successTreatmentsBMSW++;
		} else if (indiv.getSubPop().equals("msmw")) {
			//MSMW
			this.successTreatmentsBMSMW++;
		} else {
			//MSM
			this.successTreatmentsBMSM++;
		}
	}
	
	public void recordNewSuccessTreatmentX(Indiv indiv) {
		successTreatmentsX++;
		if (indiv.getGender().equals("f")) {
			//W
			this.successTreatmentsXW++;
		} else if (indiv.getGender().equals("nb")) {
			//NB
			this.successTreatmentsXNB++;
		} else if (indiv.getSubPop().equals("msw")) {
			//MSW
			this.successTreatmentsXMSW++;
		} else if (indiv.getSubPop().equals("msmw")) {
			//MSMW
			this.successTreatmentsXMSMW++;
		} else {
			//MSM
			this.successTreatmentsXMSM++;
		}
	}
	
	public void recordNewAttemptedTreatmentA(Indiv indiv) {
		attemptTreatmentsA++;
		if (indiv.getGender().equals("f")) {
			//W
			this.attemptTreatmentsAW++;
		} else if (indiv.getGender().equals("nb")) {
			//NB
			this.attemptTreatmentsANB++;
		} else if (indiv.getSubPop().equals("msw")) {
			//MSW
			this.attemptTreatmentsAMSW++;
		} else if (indiv.getSubPop().equals("msmw")) {
			//MSMW
			this.attemptTreatmentsAMSMW++;
		} else {
			//MSM
			this.attemptTreatmentsAMSM++;
		}
	}
	
	public void recordNewAttemptedTreatmentB(Indiv indiv) {
		attemptTreatmentsB++;
		
		if (indiv.getGender().equals("f")) {
			//W
			this.attemptTreatmentsBW++;
		} else if (indiv.getGender().equals("nb")) {
			//NB
			this.attemptTreatmentsBNB++;
		} else if (indiv.getSubPop().equals("msw")) {
			//MSW
			this.attemptTreatmentsBMSW++;
		} else if (indiv.getSubPop().equals("msmw")) {
			//MSMW
			this.attemptTreatmentsBMSMW++;
		} else {
			//MSM
			this.attemptTreatmentsBMSM++;
		}
	}
	
	public void recordNewAttemptedTreatmentX(Indiv indiv) {
		attemptTreatmentsX++;
		
		if (indiv.getGender().equals("f")) {
			//W
			this.attemptTreatmentsXW++;
		} else if (indiv.getGender().equals("nb")) {
			//NB
			this.attemptTreatmentsXNB++;
		} else if (indiv.getSubPop().equals("msw")) {
			//MSW
			this.attemptTreatmentsXMSW++;
		} else if (indiv.getSubPop().equals("msmw")) {
			//MSMW
			this.attemptTreatmentsXMSMW++;
		} else {
			//MSM
			this.attemptTreatmentsXMSM++;
		}
	}
	
	public void recordUseE(Indiv indiv) {
		usageE++;
		
		if (indiv.getGender().equals("f")) {
			//W
			this.usageEW++;
		} else if (indiv.getGender().equals("nb")) {
			//NB
			this.usageENB++;
		} else if (indiv.getSubPop().equals("msw")) {
			//MSW
			this.usageEMSW++;
		} else if (indiv.getSubPop().equals("msmw")) {
			//MSMW
			this.usageEMSMW++;
		} else {
			//MSM
			this.usageEMSM++;
		}
	}
	
	
	
	public void recordNewDetected(Indiv indiv) {
		if (indiv.myInfection()==null) {
			throw new RuntimeException("Indiv" + indiv + "does not have an infection!");
		}
		detected++;
		detectedList.add(indiv.myInfection());
		
		if (indiv.getGender().equals("f")) {
			//W
			this.detectedW++;
		} else if (indiv.getGender().equals("nb")) {
			//NB
			this.detectedNB++;
		} else if (indiv.getSubPop().equals("msw")) {
			//MSW
			this.detectedMSW++;
		} else if (indiv.getSubPop().equals("msmw")) {
			//MSMW
			this.detectedMSMW++;
		} else {
			//MSM
			this.detectedMSM++;
		}
		
	}
	
	public void recordNewDetectedAndSymptoms(Indiv indiv) {
		detectedAndSymptoms++;
		
		if (indiv.getGender().equals("f")) {
			//W
			this.detectedAndSymptomsW++;
		} else if (indiv.getGender().equals("nb")) {
			//NB
			this.detectedAndSymptomsNB++;
		} else if (indiv.getSubPop().equals("msw")) {
			//MSW
			this.detectedAndSymptomsMSW++;
		} else if (indiv.getSubPop().equals("msmw")) {
			//MSMW
			this.detectedAndSymptomsMSMW++;
		} else {
			//MSM
			this.detectedAndSymptomsMSM++;
		}
		
		
		
		
	}
	
	public void recordNewDetectedThruScreen(Indiv indiv) {
		detectedThruScreen++;
		recordNewDetected(indiv);
		
		if (indiv.getGender().equals("f")) {
			//W
			this.detectedThruScreenW++;
		} else if (indiv.getGender().equals("nb")) {
			//NB
			this.detectedThruScreenNB++;
		} else if (indiv.getSubPop().equals("msw")) {
			//MSW
			this.detectedThruScreenMSW++;
		} else if (indiv.getSubPop().equals("msmw")) {
			//MSMW
			this.detectedThruScreenMSMW++;
		} else {
			//MSM
			this.detectedThruScreenMSM++;
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
	
}
