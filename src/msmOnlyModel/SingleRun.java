package msmOnlyModel;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import cern.jet.random.Beta;
import cern.jet.random.Exponential;
import cern.jet.random.Normal;
import cern.jet.random.Uniform;
import cern.jet.random.engine.RandomEngine;
import repast.simphony.engine.schedule.ISchedule;
import repast.simphony.engine.schedule.ScheduleParameters;
import repast.simphony.parameter.Parameters;

public class SingleRun {
	private ThreadSafeSchedule schedule;
	private Parameters parameters;
	private String batchDirPath;
	
	//SimpleSIRBuilder builder;
	private Population population;
	//private Collection <Infection> infections;
	private Observer observer;
	private SurveillanceProgram surveillanceProgram;
	private InsertResistance resistanceInserter;
	private ThreadSafeRandomHelper randomHelper;
	private CustomFileOutput fileOutputter;
	private ChangeActivityGroups activityGroupChanger;
	
	private double endTime;
	private boolean finishing;

public SingleRun(String batchDirPath, Parameters parameters) {
	this.batchDirPath = batchDirPath;
	this.parameters = parameters;
	this.finishing = false;
	
}




public void setUp(double endTime) {
	//similar to how "build" works in the basic API
	this.endTime = endTime;

	schedule = new ThreadSafeSchedule();
     
		//access parameters
     Parameters params = this.parameters;
		int seed = params.getInteger("seed");
		String counterfactual = params.getString("counterfactual");
		String resistance = params.getString("resistance");
		int yearX = params.getInteger("yearX");
		
		randomHelper = registerDistributions();
		//context.add(randomHelper);	 
		
		this.fileOutputter = createOutputter();
		createObserver(seed, counterfactual, resistance, yearX);
		//context.add(observer);
		
		createSurveillance(counterfactual);
		
		createIndivs(params.getInteger("population_size"));
		//SubGrouping groups = new SubGrouping(population);
		
		
		infectInitialInfected(params.getInteger("infected_count_init"), params.getInteger("population_size"), population);
		
		scheduleActivityGroupChanges(parameters, randomHelper, schedule, population);
		 
		if (!resistance.equals("none")) {
			scheduleInsertResistance(resistance);
		}
		
		observer.setPopulation(population);
			
}

public void go() {
	start();
	
	while (!finishing) {
		step();
	}
	
	end();
}

public void start() {
	schedule.execute();
	
}

public void step() {
	//synchronized(schedule) {
	if (schedule.getActionCount()==0 || schedule.getTickCount() > endTime) {
		this.finishing = true;
	} else {
		schedule.execute();
	}
	//}
}


public void end() {
	schedule.actionQueue.clear();	
}



public ThreadSafeRandomHelper registerDistributions() {
	//set seed and random distributions
	
			int seed = parameters.getInteger("seed");
			ThreadSafeRandomHelper randomHelper = new ThreadSafeRandomHelper(seed);
	
			String uniqueGeneratorName = "myStream" + seed;
			
			//randomHelper.setSeed(seed);
			RandomEngine eng = randomHelper.registerGenerator(uniqueGeneratorName, seed);
			
			Uniform partnerActivityGroupUniform = new Uniform(0.0, 1.0, eng);
			randomHelper.registerDistribution("partnerActivityGroupUniform", partnerActivityGroupUniform);
			
			Beta genderPrefBeta = new Beta(0.5, 0.05, eng);
			randomHelper.registerDistribution("genderPrefBeta", genderPrefBeta);
			
			//dist for recovery
			double recoveryTime = parameters.getDouble("recovery_time") * 52.0; // convert from years to weeks/ticks
			Exponential recoveryExp = new Exponential(1/recoveryTime, eng);
			randomHelper.registerDistribution("recoveryExp", recoveryExp);
			
			//dist for contact
			Uniform zeroOneUniform = new Uniform(0.0, 1.0, eng);
			randomHelper.registerDistribution("zeroOneUniform", zeroOneUniform);
			
			Uniform partnerGenderUniform = new Uniform(0.0, 1.0, eng);
			randomHelper.registerDistribution("partnerGenderUniform", partnerGenderUniform);
			
			Uniform symptomaticUniform = new Uniform(0.0,1.0, eng);
			randomHelper.registerDistribution("symptomaticUniform", symptomaticUniform);
			
			Normal screenIntervalMSMNormal = new Normal(parameters.getDouble("screen_interval_MSM")*52, 52*parameters.getDouble("screen_interval_MSM")/10, eng);
			randomHelper.registerDistribution("screenIntervalMSMNormal", screenIntervalMSMNormal);
			Uniform screenFirstValueMSMUniform = new Uniform(0, parameters.getDouble("screen_interval_MSM")*52, eng);
			randomHelper.registerDistribution("screenFirstValueMSMUniform", screenFirstValueMSMUniform);
			
			
			Normal screenIntervalMSMHigh = new Normal(parameters.getDouble("screen_interval_MSM_high")*52, 52*parameters.getDouble("screen_interval_MSM_high")/10, eng);
			randomHelper.registerDistribution("screenIntervalMSMHigh", screenIntervalMSMHigh);
			Uniform screenFirstValueMSMHigh = new Uniform(0, parameters.getDouble("screen_interval_MSM_high")*52, eng);
			randomHelper.registerDistribution("screenFirstValueMSMHigh", screenFirstValueMSMHigh);
			
			
			
			
			Normal activityGroupTransferPropNormal = new Normal(parameters.getDouble("activity_group_transfer_prop"), parameters.getDouble("activity_group_transfer_prop")/10, eng);
			randomHelper.registerDistribution("activityGroupTransferPropNormal", activityGroupTransferPropNormal);
		
			
			Uniform sequelaeUniform = new Uniform(0.0, 1.0, eng);
			randomHelper.registerDistribution("sequelaeUniform", sequelaeUniform);
			
			Uniform adherenceUniform = new Uniform(0.0, 1.0, eng);
			randomHelper.registerDistribution("adherenceUniform", adherenceUniform);
			
			Uniform randomDrugUniform = new Uniform(0.0, 1.0, eng);
			randomHelper.registerDistribution("randomDrugUniform", randomDrugUniform);
			
			
			int popSize = parameters.getInteger("population_size");
			Uniform partnerSelectUniform = new Uniform(0, popSize - 1, eng);
			randomHelper.registerDistribution("partnerSelectUniform", partnerSelectUniform);
			
			Uniform importResistantUniform = new Uniform(0, popSize - 1, eng);
			randomHelper.registerDistribution("importResistantUniform", importResistantUniform);
			
			Uniform developResistanceUniform = new Uniform(0.0,1.0, eng);
			randomHelper.registerDistribution("developResistanceUniform", developResistanceUniform);
			
			
			double mean_delay_to_seek_care_MSM = parameters.getDouble("delay_to_seek_care_msm") * 52.0;
			Exponential delayToSeekCareMSMExp = new Exponential(1/mean_delay_to_seek_care_MSM, eng);
			randomHelper.registerDistribution("delayToSeekCareMSMExp", delayToSeekCareMSMExp);
			

			Uniform realisticComboUniform = new Uniform(0.0, 1.0, eng);
			randomHelper.registerDistribution("realisticComboUniform", realisticComboUniform);
			
			
			
			double mean_delay_to_retreatment_MSM = parameters.getDouble("delay_to_retreatment_msm") * 52.0;

			Exponential delayToRetreatmentMSMExp = new Exponential(1/mean_delay_to_retreatment_MSM, eng);
			randomHelper.registerDistribution("delayToRetreatmentMSMExp", delayToRetreatmentMSMExp);
			
		
			
			
			
			
			Uniform testsUniform = new Uniform(0.0, 1.0, eng);
			randomHelper.registerDistribution("testsUniform", testsUniform);
			
			return randomHelper;
			
}

public Observer createObserver(int seed, String counterfactual, String resistance, int yearX) {
		
		
	//add Observer
			Observer observer = new Observer(parameters, fileOutputter, seed, 0,0, schedule);
			
			ScheduleParameters schparams = ScheduleParameters.createRepeating(0.0, 52.0);
			schedule.schedule(schparams, observer, "calcObserver");
			
			ScheduleParameters schparams2 = ScheduleParameters.createRepeating(4.0, 4.0);
			schedule.schedule(schparams2, observer, "checkStopCondition");
			
			
			scheduleAddX(yearX, observer);
			this.observer = observer;
			return observer;

}


public ChangeActivityGroups scheduleActivityGroupChanges(Parameters parameters, ThreadSafeRandomHelper randomHelper, ISchedule schedule, Population population) {
	ChangeActivityGroups activityGroupChanger = new ChangeActivityGroups(parameters, randomHelper, schedule, population);

	ScheduleParameters schparams = ScheduleParameters.createRepeating(0.0, 52.0);
	schedule.schedule(schparams, activityGroupChanger, "changeActivityGroups");
	
	this.activityGroupChanger = activityGroupChanger;
	
	return activityGroupChanger;
}

public void createSurveillance(String counterfactual) {
	surveillanceProgram = new SurveillanceProgram(counterfactual, observer);
	observer.setSurveillance(surveillanceProgram);
	
	scheduleSurveillance(surveillanceProgram, counterfactual);
}

public void scheduleSurveillance(SurveillanceProgram surveillance, String counterfactual) {
	
	if (counterfactual.contains("GISP")) {
		scheduleGISP();
	} else if (counterfactual.contains("sporadic")) {
		scheduleSporadicSurveillance();
	}
}

public void scheduleGISP() {
	ScheduleParameters schparams = ScheduleParameters.createRepeating(4, 4);
	schedule.schedule(schparams, surveillanceProgram, "conductSurveillance");
}

public void scheduleSporadicSurveillance() {
	
}

public void createIndivs(int IndivCount) {
	//create population
	population = new Population(parameters, IndivCount, randomHelper, observer, schedule);
	
	population.allIndivs().forEach(indiv -> indiv.setPop(population));
	
			
}

public void infectInitialInfected(int InfectiousCount, int IndivCount, Population population) {
	List<Object> indivToInfectList = new ArrayList<Object>();
	
	//we want to start the high risk group with 3x higher prevalence than general pop.
	double initialPrev = (double) InfectiousCount/ (double) population.totalSize();
	double desiredHighRiskPrev = initialPrev * 3.0;
	
	
	double amountToInfectHighRisk = desiredHighRiskPrev * population.highRiskCount();  
	
	if (amountToInfectHighRisk > InfectiousCount) {
		amountToInfectHighRisk = InfectiousCount;
	}
	
	double amountToInfectLowRisk = InfectiousCount - amountToInfectHighRisk;
	
	List <Object> highRisktoInfect = population.highActivityGroupStream().limit((long) amountToInfectHighRisk).collect(Collectors.toList());
	indivToInfectList.addAll(highRisktoInfect);
	
	List <Object> lowRisktoInfect = population.lowActivityGroupStream().limit((long) amountToInfectLowRisk).collect(Collectors.toList());
	indivToInfectList.addAll(lowRisktoInfect);
	
	//infect the infectious indivs
	ScheduleParameters schparams = ScheduleParameters.createOneTime(-0.5);
	for (Object i : indivToInfectList) {
		Indiv infected = (Indiv) i;
		schedule.schedule(schparams, infected, "infectInit");
	}
}

public void scheduleInsertResistance(String resistance) {
	
	//use the new resistance param (String) to select which insert resistance version to use.
	// write an experiemtn in main which compares different resistance insertion "strategies" under GISP
	
	resistanceInserter = new InsertResistance(resistance, parameters, schedule, population, randomHelper);
	
	ScheduleParameters schparams = ScheduleParameters.createOneTime(521);
	
	if (resistance.equals("combo")) {
		schedule.schedule(schparams, resistanceInserter, "comboResistanceConvertAndImport");
	} else if (resistance.equals("constantImport")) {
		schedule.schedule(schparams, resistanceInserter, "insertResistance");
	} else if (resistance.equals("dropInOnce")) {
		schedule.schedule(schparams, resistanceInserter, "dropInResistant");
	} else if (resistance.equals("convertOnce")) {
		schedule.schedule(schparams, resistanceInserter, "convertToResistant");
	} else if (resistance.equals("developWithTreatment")) {
		
	}
	
}

public void scheduleAddX(int yearX, Observer observer) {
	double weekX = yearX * 52.0;
	
	ScheduleParameters schparams = ScheduleParameters.createOneTime(weekX);
	schedule.schedule(schparams, observer, "addX");
}


public CustomFileOutput createOutputter() {
	CustomFileOutput fileOutputter = new CustomFileOutput(batchDirPath, 
			parameters.getString("counterfactual"), parameters.getString("resistance"), parameters.getInteger("yearX"), 
			parameters.getInteger("runNumber"), parameters.getInteger("seed"));
	
	fileOutputter.createOutputFile();
	
	return fileOutputter;
}



public ThreadSafeSchedule schedule() {
	return schedule;
}

public Population population() {
	return population;
}

public Observer observer() {
	return observer;
}

public SurveillanceProgram surveillanceProgram() {
	return surveillanceProgram;
}

public InsertResistance resistanceInserter() {
	return resistanceInserter;
}

public ChangeActivityGroups riskGroupChanger() {
	return activityGroupChanger;
}

public void assignSchedule(ThreadSafeSchedule schedule) {
	this.schedule = schedule; //for testing
}

public void assignOutputter(CustomFileOutput outputter) {
	this.fileOutputter = outputter;
}



public void testSetUp(double endTime) {
	//creates the basic skeleton of a run without setting up the population,
	//so that more unique populations for testing purposes can be created
	this.endTime = endTime;

	schedule = new ThreadSafeSchedule();
     
		//access parameters
     Parameters params = this.parameters;
		int seed = params.getInteger("seed");
		String counterfactual = params.getString("counterfactual");
		String resistance = params.getString("resistance");
		int yearX = params.getInteger("yearX");
		
		randomHelper = registerDistributions();
		//context.add(randomHelper);	 
		
		this.fileOutputter = createOutputter();
		createObserver(seed, counterfactual, resistance, yearX);
		//context.add(observer);
		
		createSurveillance(counterfactual);
		
		ChangeActivityGroups riskGroupChanger = new ChangeActivityGroups(parameters, randomHelper, schedule, population);
		this.activityGroupChanger = riskGroupChanger;

			
}


}