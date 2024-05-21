package msmOnlyModel;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import cern.jet.random.Exponential;
import cern.jet.random.Uniform;
import cern.jet.random.engine.RandomEngine;
import repast.simphony.engine.schedule.ISchedule;
import repast.simphony.engine.schedule.ScheduleParameters;
import repast.simphony.parameter.Parameters;
import repast.simphony.util.collections.IndexedIterable;

public class InsertResistance {
	
	private Parameters params;
	private ISchedule schedule;
	private Population population;
	private ThreadSafeRandomHelper randomHelper;
	
	private int countTotalResist;
	private int countInitialResistA;
	private int countInitialResistB;
	private int countAnnualResistA;
	private int countAnnualResistB;
	private boolean chanceToDevelopResistance;
	Uniform developResistanceUniform;

	private double percentResistantACombo;
	private int beginImportingBCombo;
	private double importingBIntervalCombo;
	private List<Double> importingBSchedule;

	public InsertResistance(String resistanceMethod, Parameters params, ISchedule schedule, Population population, ThreadSafeRandomHelper randomHelper) {
		
		this.params=params;
		this.schedule=schedule;
		this.population=population;
		this.randomHelper=randomHelper;
		
		
		if (resistanceMethod.equals("combo")) {
			this.percentResistantACombo = params.getDouble("percent_resistant_A");
			this.beginImportingBCombo = params.getInteger("begin_importing_B");
			this.importingBIntervalCombo = params.getDouble("importing_B_interval");
			this.chanceToDevelopResistance = true;
		
			this.importingBSchedule = makeImportingBSchedule(importingBIntervalCombo);
			
		} else if (resistanceMethod.equals("developWithTreatment")) {
			chanceToDevelopResistance = false;
			developResistanceUniform = (Uniform) randomHelper.getDistribution("developResistanceUniform");
		} else {
			countTotalResist = 100;
			
			//make equal amounts of resistant to each drug
			countInitialResistA = countTotalResist/2;
			countInitialResistB = countTotalResist/2;
			
			countAnnualResistA = countTotalResist/20;
			countAnnualResistB = countTotalResist/20;
		}
	}
	
	public void insertResistance() {
		 
		  
		  ScheduleParameters schparams = ScheduleParameters.createRepeating(522.0, 52.0);
		  schedule.schedule(schparams, this, "constantImportResistant");
		  
	}
	
	public void comboResistanceConvertAndImport() {

		convertToResistantA();
	
	  ScheduleParameters schparams = ScheduleParameters.createOneTime(beginImportingBCombo);
	  schedule.schedule(schparams, this, "beginImportingB");
	}


	public void convertToResistantA() {
		
		population.allInfectious().
				limit((long) (percentResistantACombo * population.infectiousCount())).
				forEach(indiv -> indiv.infect("A"));
		
	}
	
	public void beginImportingB() {

		//first time
		stochasticImportB();
		
		//for loop to schedule OneTime all the importation of B events
		for (Double importEvent : importingBSchedule) {
			  ScheduleParameters schparams = ScheduleParameters.createOneTime(importEvent);
			  schedule.schedule(schparams, this, "stochasticImportB");
		}
		
	}
	
	public void stochasticImportB() {

		Uniform importResistantUniform = (Uniform) randomHelper.getDistribution("importResistantUniform");
		int newBResistant = importResistantUniform.nextInt();
		Indiv indiv = population.allIndivs().collect(Collectors.toList()).get(newBResistant);
		indiv.infect("B");
	}

	
	public void convertToResistant() {
		//changes existing cases to resistant
		
		//grab countTotalResist Indivs with state == 1
		List<Object> infectious = population.allInfectious()
				.limit(countTotalResist)
				.collect(Collectors.toList());

		for (int index = 0; index < countTotalResist; index++) {
			Indiv indiv = (Indiv) infectious.get(index);

			if (index < countInitialResistA) {
				// infect first half with "A"
				indiv.infect("A");
			} else {
				indiv.infect("B");
			}
		//change the strain of the first half to "A"
		
		//change the strain of the second half to "B"
		
		}
	}
	
	public void dropInResistant() {
		//creates a set amount of new cases with resistance (as if transmission from outside this pop)
		
		//grab countTotalResist Indivs
		List<Indiv> indivs = population.allIndivs().collect(Collectors.toList());
		
		for (int index = 0; index < countTotalResist; index++) {
			Indiv indiv = (Indiv) indivs.get(index);

			if (index < countInitialResistA) {
				// infect first half with "A"
				indiv.infect("A");
			} else {
				indiv.infect("B");
			}

		//infect second half with "B"
		}
	}
	
	public void constantImportResistant() {

		Uniform importResistantUniform = (Uniform) randomHelper.getDistribution("importResistantUniform");
		List<Indiv> indivs = population.allIndivs().collect(Collectors.toList());

		for (int index = 0; index < countAnnualResistA; index++) {
			int newAResistant = importResistantUniform.nextInt();
			Indiv indiv = (Indiv) indivs.get(newAResistant);
			indiv.infect("A");

		}
		
		for (int index = 0; index < countAnnualResistB; index++) {
			int newBResistant = importResistantUniform.nextInt();
			Indiv indiv = (Indiv) indivs.get(newBResistant);
			indiv.infect("B");

		}
		
	}
	
	public void checkForDevelopResistance(Indiv indiv, String treatment) {
		double randomValue = developResistanceUniform.nextDouble();
		
		double chanceDevelopResistance = 0.0001;
		
		if (randomValue <= chanceDevelopResistance && (!treatment.equals("X"))) {
			indiv.infect(treatment);
		} else {
			indiv.actuallyRecover(treatment);
		}
	}
	
	
	public List<Double> makeImportingBSchedule(Double interval) {
		List <Double> importingSchedule = new ArrayList<Double>();
		
		String uniqueGeneratorName = "myStream" + params.getInteger("seed");
		RandomEngine eng = randomHelper.getGenerator(uniqueGeneratorName);
		Exponential importingBIntervalExp = new Exponential(1/interval, eng);
		
		double endTime = 1560; //need to set to max, or else slight var between sweep and cal

		double firstValue = beginImportingBCombo + importingBIntervalExp.nextDouble();
		
		importingSchedule.add(firstValue);
		
		while (importingSchedule.get(importingSchedule.size()-1) < endTime) {
			double newInterval = importingBIntervalExp.nextDouble();
			double newValue = newInterval + importingSchedule.get(importingSchedule.size()-1);
			importingSchedule.add(newValue);
		}

		
		return importingSchedule;
	}
	
	
}


