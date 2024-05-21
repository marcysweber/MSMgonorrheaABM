package msmOnlyModel;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import cern.jet.random.Beta;
import cern.jet.random.Exponential;
import cern.jet.random.Normal;
import cern.jet.random.Uniform;
import cern.jet.random.engine.RandomEngine;
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
		observer = createObserver(seed, counterfactual, resistance, yearX);
		//context.add(observer);
		
		createSurveillance(counterfactual);
		
		createIndivs(params.getInteger("population_size"));
		//SubGrouping groups = new SubGrouping(population);
		
		
		infectInitialInfected(params.getInteger("infected_count_init"), params.getInteger("population_size"), population);
		
		 
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
	
	
	
}



public ThreadSafeRandomHelper registerDistributions() {
	//set seed and random distributions
	
			int seed = parameters.getInteger("seed");
			ThreadSafeRandomHelper randomHelper = new ThreadSafeRandomHelper(seed);
	
			String uniqueGeneratorName = "myStream" + seed;
			
			//randomHelper.setSeed(seed);
			RandomEngine eng = randomHelper.registerGenerator(uniqueGeneratorName, seed);
			
			Uniform genderUniform = new Uniform(0.0, 1.0, eng);
			randomHelper.registerDistribution("genderUniform", genderUniform);
			
			Beta genderPrefBeta = new Beta(0.5, 0.05, eng);
			randomHelper.registerDistribution("genderPrefBeta", genderPrefBeta);
			
			//dist for recovery
			Exponential recoveryExp = new Exponential(1/parameters.getDouble("recovery_lambda"), eng);
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
			
			
			
			Exponential delayToSeekCareMSMExp = new Exponential(1/parameters.getDouble("delay_to_seek_care_msm"), eng);
			randomHelper.registerDistribution("delayToSeekCareMSMExp", delayToSeekCareMSMExp);
			

	
			
			
			
			
			Exponential delayToRetreatmentMSMExp = new Exponential(1/parameters.getDouble("delay_to_retreatment_msm"), eng);
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
			return observer;

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
	
	double amountToInfectMSM = (InfectiousCount/(double)IndivCount) * population.msmCount();
	List <Object> MSMtoInfect = population.msmList.stream().limit((long) amountToInfectMSM).collect(Collectors.toList());
	indivToInfectList.addAll(MSMtoInfect);
	
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


}