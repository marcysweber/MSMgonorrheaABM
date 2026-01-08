/**
 * 
 */
package msmOnlyModel;


import java.util.List;
import cern.jet.random.Beta;
import cern.jet.random.Exponential;
import cern.jet.random.Uniform;
import repast.simphony.engine.schedule.ISchedule;
import repast.simphony.engine.schedule.ScheduleParameters;
import repast.simphony.parameter.Parameters;

/**
 * @author marcy
 *
 */
public class Indiv {
	private Parameters allParameters;
	private Population population;
	
	private String gender; //m, f, or nb
	//private double genderPref; //0.0 is strictly same-gender; 1.0 is strictly different-gender
	
	private String subPop; //discrete category of gender/sexuality
	private String activityGroup; //if true, excluded from sexual pool

	private int state;
	//private boolean abstaining;
	private Infection infection;
	private List<Integer> screenings;
	private boolean seekCareScheduled;
	private boolean inTreatment;
	//private int timeInfected;
	//private double tickInfected;
	
	private ThreadSafeRandomHelper randomHelper;
	private Observer observer;
	private ISchedule schedule;
	
	//params saved for convenience
	private double transmission;	
	
	public Indiv(Parameters allParameters, ThreadSafeRandomHelper randomHelper, Observer observer, ISchedule schedule) {
		this.allParameters = allParameters;

		this.gender = assignGender();
		//this.genderPref = assignGenderPref();
		this.subPop = assignSubPop();
		
		this.state = 0;
		//this.abstaining = false;
		this.seekCareScheduled = false;
		this.inTreatment = false;
		// state = 0 means susceptible
		// state = 1 means infectious
		
		//this.timeInfected = 0;
		//this.tickInfected = -1;
		this.transmission = allParameters.getDouble("transmission");
		allParameters.getInteger("population_size");
		this.infection = null;
		
		this.observer = observer;
		this.randomHelper = randomHelper;
		this.schedule = schedule;
		
		Screener screenScheduler = new Screener();
		this.screenings = screenScheduler.makeScreenSchedule(randomHelper, subPop);
	}
	 
	//this is the version that is actually getting used currently
	public Indiv(Parameters allParameters, String subPop, String activityGroup, ThreadSafeRandomHelper randomHelper, Observer observer, ISchedule schedule) {
		this.subPop = subPop;

		if (subPop.startsWith("m")) {
			this.gender = "m";
		} else if (subPop.startsWith("w")) {
			this.gender = "f";
		} else {
			this.gender = "nb";
		}
		this.allParameters = allParameters;

		this.randomHelper = randomHelper;
		this.observer = observer;
		this.schedule = schedule;
		
		this.activityGroup = activityGroup;
		
		this.state = 0;
		//this.abstaining = false;
		this.seekCareScheduled = false;
		this.inTreatment = false;

		// state = 0 means susceptible
		// state = 1 means infectious
		
		//this.timeInfected = 0;
		//this.tickInfected = -1;
		

		
		if (this.gender.equals("f")) {
			this.transmission = allParameters.getDouble("transmissionF");
		} else if (this.subPop.equals("msm")){
			this.transmission = allParameters.getDouble("transmissionMSM");
		} else if (this.subPop.equals("msw")) {
			this.transmission = allParameters.getDouble("transmissionMSW");
		}
		
		allParameters.getInteger("population_size");
		this.infection = null;
		
		Screener screenScheduler = new Screener();
		this.screenings = screenScheduler.makeScreenSchedule(randomHelper, subPop);
	}
	
	//for testing, so that gender and genderPref can be prescribed
	public Indiv(Parameters allParameters, String gender, double genderPref, ThreadSafeRandomHelper randomHelper, Observer observer, ISchedule schedule) {

		this.allParameters = allParameters;
		this.gender = gender;
		//this.genderPref = genderPref;
		this.subPop = assignSubPop();

		
		this.state = 0;
		//this.abstaining = false;
		this.seekCareScheduled = false;
		// state = 0 means susceptible
		// state = 1 means infectious
		
		//this.timeInfected = 0;
		//this.tickInfected = -1;
		this.transmission = allParameters.getDouble("transmission");
		allParameters.getInteger("population_size");
		this.infection = null;
		
		this.randomHelper = randomHelper;
		this.observer = observer;
		this.schedule = schedule;
		this.inTreatment = false;

		
		Screener screenScheduler = new Screener();
		this.screenings = screenScheduler.makeScreenSchedule(randomHelper, subPop);
	}

	public void setPop(Population population) {
		this.population = population;
	}
	
	
	public String assignGender() {
		String gender = "none";
		Uniform genderUniform = (Uniform) randomHelper.getDistribution("genderUniform");
		double randomGender = genderUniform.nextDouble();
		if (randomGender < 0.5) {gender="m";}
		else if (randomGender <= 1.0) {gender="f";}
		else {gender = "nb";}
		
		return gender;
	}
	
	public double assignGenderPref() {
		double genderPref = 0.0;
		Beta genderPrefBeta = (Beta) randomHelper.getDistribution("genderPrefBeta");
		genderPref = genderPrefBeta.nextDouble();
		return genderPref;

	}
	
	
	public String assignSubPop() {
		String subPop = "none";
		
		Uniform genderUniform = (Uniform) randomHelper.getDistribution("genderUniform");
		double randomGender = genderUniform.nextDouble();
		
		if (this.gender.equals("f")) {
			subPop = "w";
		} else if (this.gender.equals("nb")){
			subPop = "nb";
		} else if (randomGender < 0.1) {
			subPop = "msm";
//		} else if (this.genderPref < 0.75) {
//			subPop = "msmw";
		} else {
			subPop = "msw";
		}
		
		return subPop;
	}
	
	//KEY METHODS
	
	public void infectiousActions() {
		int roundedTick = (int) tickNow();

		if (this.infectious()) {
			

			//if not already detected, check for detection
			if (!infection.isDetected()) {
				if (this.symptoms() && !seekCareScheduled) { //if symptomatic and not already scheduled to seek care
					CareSeeking care = new CareSeeking(this, getObserver(), schedule);
					seekCareScheduled = care.scheduleSeekCare();
				} else if (screenings.contains(roundedTick)) {
					Screener screener = new Screener();
					screener.screen(this, getObserver());
				}
				
			}
			
			//if still infectious...
			if (this.infectious()) {
				if (attemptContact()) {//stochastic logic gate from annualContacts param
					infection.recordTransmission();
					Indiv partner = partnerSelect(); //find a partner
					if (this != partner) {//doublecheck that it's not myself
						if (partner.infectious()) {
							partner.infect(this.infection.getStrain(), "reinfect");
						} else {
							partner.infect(this.infection.getStrain());//infect the partner
						}
					}
				}
				//try to infect again next week
				scheduleInfectiousActions();
			}
		}
	}

	
	public boolean attemptContact() { 
		//this method checks if this infectious agent is actually contacting another this tick
		boolean result = false;
		Uniform contactUniform = (Uniform) randomHelper.getDistribution("zeroOneUniform");
		double randomValue = contactUniform.nextDouble();
			
		double weeklyProb = 1 - Math.exp(-transmission * 1/52);
		
		double fitnessCost = 0.0;
		
		if (infection.resistantToA()) {
			fitnessCost += allParameters.getDouble("fitnessCostA");
		}
		
		if (infection.resistantToB()) {
			fitnessCost += allParameters.getDouble("fitnessCostB");
		}
		
		double fitnessProbDec = fitnessCost * weeklyProb;
		
		weeklyProb = weeklyProb - fitnessProbDec;
		
		if (activityGroup.equals("low")) {
			weeklyProb = weeklyProb * allParameters.getDouble("activity_group_transmission_ratio");
		}
		
		if (weeklyProb > randomValue) { //50% chance of seeking a contact this timestep
			result = true;
		}
		return result;
	}
	
	public Indiv partnerSelect() {
		//this method finds a partner (i.e., a compatible sexual partner)

		//first, decide what gender I am searching for this time.
		//String genderSeeking = selectGenderSeeking();
//		System.out.println("I am seeking a ");
//		System.out.println(genderSeeking);
//		System.out.println("");
		
		//Context<Object> context = ContextUtils.getContext(this); //access context of perspective agent
		//SubGrouping subgrouping = new SubGrouping(population);
		
		List<Indiv> potentialPartners = null;
		
		if (this.subPop.equals("msm")) {
			
			//choose same risk group or different risk group
			double assort = this.allParameters.getDouble("assortativity");
			//greater than 0.5 means more likely to choose same risk group
			//so the random number should be less than assort for the indiv to look in their own risk group
			Uniform uniformDist = (Uniform) randomHelper.getDistribution("partnerActivityGroupUniform");
			
			double value = uniformDist.nextDouble();
			
			
			
			if (value <= assort) {
				if (this.activityGroup.equals("low")) {
					potentialPartners = population.lowActivityGroup;

				} else {
					potentialPartners = population.highActivityGroup;

				}
			} else {
				if (this.activityGroup.equals("high")) {
					potentialPartners = population.lowActivityGroup;

				} else {
					potentialPartners = population.highActivityGroup;

				}
			}
			
			
			
		} else {
			System.out.println("this was supposed to be an MSM only run but there was an Indiv of a different subPop!");
			System.exit(state);
		}

		Indiv partner = this;
		
		//make sure partner not self
		//String uniqueGeneratorName = "myStream" + allParameters.getInteger("seed");
		//RandomEngine eng = randomHelper.getGenerator(uniqueGeneratorName);
		
		Uniform partnerSelectUniform = randomHelper.getUniform();

		while (partner == this) {
				int toSkip = partnerSelectUniform.nextIntFromTo(0, potentialPartners.size()-1);
				partner = (Indiv) potentialPartners.get(toSkip);
			}

		return partner;
	}
	
//	public String selectGenderSeeking() {
//		String genderSeeking = "none";
//		Uniform partnerSelectUniform = (Uniform) RandomHelper.getDistribution("partnerGenderUniform");
//		double genderSeekingRandom = partnerSelectUniform.nextDouble();
//		
//		if (genderSeekingRandom > genderPref) {
//			genderSeeking = this.gender;
//		} else if (genderSeekingRandom > (genderPref + 0.1)) {
//			genderSeeking = "nb";
//		} else {
//			if (this.gender.equals("m")) {genderSeeking = "f";}
//			else if (this.gender.equals("f")) {genderSeeking = "m";}
//			else {genderSeeking = "nb";}
//		}
//		
//		return genderSeeking;
//	}
//	
//	public boolean verifyCompatibility(Indiv potentialPartner) {
//		boolean compatible = false;
//		Uniform partnerSelectUniform = (Uniform) RandomHelper.getDistribution("partnerGenderUniform");
//
//		
//		String partnerPref = "none";
//		double genderPartnerSeekingRandom = partnerSelectUniform.nextDouble();
//
//		if (genderPartnerSeekingRandom > potentialPartner.getGenderPref()) {
//			partnerPref = potentialPartner.getGender();
//		} else if (genderPartnerSeekingRandom > (potentialPartner.getGenderPref() + 0.1)) {
//			partnerPref = "nb";
//		} else {
//			if (potentialPartner.gender.equals("m")) {partnerPref = "f";}
//			else if (potentialPartner.gender.equals("f")) {partnerPref = "m";}
//			else {partnerPref = "nb";}
//		}
//		
//		//System.out.println(partnerPref);
//		//System.out.println(this.gender);
//
//		
//		if (partnerPref.equals(this.gender)) {
//			compatible = true;
//		}
//		
//		return compatible;
//	}
//	
	public void infect(String strain, String type) {
		if (type.contains("resist")) {
			infection.developResistance();
		} else if (type.contains("reinfect")) {
			infection.reInfect();
		}
		
		infect(strain);
	}
	
	public void infect(String strain) {
		//this method infects a susceptible partner, 
		//and schedules them to contact other agents until they recover,
		//and schedules their recovery
		
		//if (this.state == 0) { //can get infected if sus 
		double recoveryTime = getRecoveryTime();
		recoveryTime = tickNow() + recoveryTime;
		
		this.createInfection(strain, false, recoveryTime);
		this.changeStateTo(1);
			
		if (!this.symptoms()) {
			
		}
			
		this.scheduleInfectiousActions();
		this.scheduleRecover(recoveryTime);
			
		//}
	}
	
	public void infectInit() {
		double recoveryTime = getRecoveryTime();
		recoveryTime = tickNow() + recoveryTime;


		this.createInfection("none", true, recoveryTime);
		this.changeStateTo(1);
			
			
		this.scheduleInfectiousActions();
		this.scheduleRecover(recoveryTime);
			
	}
	
	public void infectInit(String strain) {
		double recoveryTime = getRecoveryTime();
		recoveryTime = tickNow() + recoveryTime;


		this.createInfection(strain, true, recoveryTime);
		this.changeStateTo(1);
			
			
		this.scheduleInfectiousActions();
		this.scheduleRecover(recoveryTime);
			
	}
	
	//revert to susceptible
	//previous infection DOES NOT convey protection
	public void recoverOrDevelopResistance(String treatment) {
		String resistance = allParameters.getString("resistance");

		if (tickNow() <=520) {
			actuallyRecoverwTreatment(treatment);
		} else if (treatment.equals("X")) {
			actuallyRecoverwTreatment(treatment);
		} else if (treatment.equals("E")) {
			actuallyRecoverwTreatment(treatment);
		} else {
			if (treatment.equals("A")) {
				if (resistance.equals("combo")) {
					InsertResistance resistanceInserter = new InsertResistance(resistance, allParameters, schedule, population, randomHelper);
					resistanceInserter.checkForDevelopResistance(this, treatment);
				} else {
					actuallyRecoverwTreatment(treatment);
				}
			} else if (treatment.equals("B")){
				if (resistance.equals("combo")) {
					InsertResistance resistanceInserter = new InsertResistance(resistance, allParameters, schedule, population, randomHelper);
					resistanceInserter.checkForDevelopResistance(this, treatment);
				} else {
					actuallyRecoverwTreatment(treatment);
				}
			} else if (treatment.equals("AandB")) {
				if (resistance.equals("combo")) {
					InsertResistance resistanceInserter = new InsertResistance(resistance, allParameters, schedule, population, randomHelper);
					resistanceInserter.checkForDevelopResistance(this, "AandB");
				} else {
					actuallyRecoverwTreatment(treatment);
				}
			}
		} 

	}
	
	
	
	
	public void recoverNaturally() {
		if (infectious()) {
		
			infection.recoverNaturally();
		
			actuallyRecover();
		}
	}

	public void actuallyRecoverwTreatment(String treatment) {
		if (infectious()) {
			try {
				infection.successfulTreatment(treatment);
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			this.recordEndTreatment();
		}
		actuallyRecover();
	}
	
	public void actuallyRecover() {
		if (state != 0) {
			this.infection.ceaseInfection();
			this.infection = null;
			this.changeStateTo(0);
		}
	}
	
	public void createInfection(String strain, boolean starting, double naturalRecoveryTime) {
		String newStrain = strain;
		//Context<Object> context = ContextUtils.getContext(this);
		if (this.myInfection()!=null){
			newStrain = myInfection().checkForDoubleResist(newStrain);
			this.myInfection().overrideInfection(); //essentially remove the old infection, in case different strain
			this.infection = null;
		}
		
		Infection newInfection = new Infection(this, allParameters, newStrain, starting, naturalRecoveryTime, this.subPop, randomHelper, tickNow());
		//context.add(newInfection);
		this.infection = newInfection;
	}
	
	public double getRecoveryTime() {
		Exponential recoveryExp = (Exponential)randomHelper.getDistribution("recoveryExp");
		double thisRecovery = recoveryExp.nextDouble();
		//System.out.println(thisRecovery);
		
		return thisRecovery;
	}
	
	public void changeStateTo(int newState) {
		this.state = newState;
		//this.stopAbstaining();
	}
	
	public void changeActivityGroup() {
		if (this.activityGroup.equals("low")){
			this.activityGroup = "high";
		} else if (this.activityGroup.equals("high")) {
			this.activityGroup = "low";
		}
	}
	
	
	
	
	
	//RECORDER METHODS
	
	public Observer getObserver() {
		
		return this.observer;
	}
	
	public ThreadSafeRandomHelper getRandomHelper() {
		
		return this.randomHelper;
	}
	
	public ISchedule getSchedule() {
		return this.schedule;
	}
	
	public Parameters getParameters() {
		return this.allParameters;
	}
	
	
	//send this new case to the observer
//	public void recordNewCase() {
//		Observer observer = getObserver();
//		observer.recordNewCase(this);
//	}
//	
//	public void recordTreatment() {
//		Observer observer = getObserver();;
//		observer.recordNewTreatment(this);
//	}
	

	public void recordDevelopedResistance() {
		infection.developResistance();
	}
	
	public void recordInTreatment() {
		this.inTreatment = true;
	}
	
	public void recordEndTreatment() {
		this.inTreatment = false;
	}
	
	//SCHEDULING METHODS
	
	//schedule the newly infected agent to infect others on the next tick
	public void scheduleInfectiousActions() {
		//this.tickInfected = schedule.getTickCount();
		double nexttick = schedule.getTickCount() + 1;
		
		ScheduleParameters schparams = ScheduleParameters.createOneTime(nexttick);
		schedule.schedule(schparams, this, "infectiousActions");
		
	}
	
	//schedule the newly infected agent to recover
	public void scheduleRecover(double recoveryTime) {
		
		//System.out.println(recoveryTick);
		ScheduleParameters schparams = ScheduleParameters.createOneTime(recoveryTime);			
		schedule.schedule(schparams, this, "recoverNaturally");
	}
	

//	public void abstain() {
//		//this.abstaining = true;
//	}
//	
//	public void stopAbstaining() {
//		this.abstaining = false;
//	}
	
	public void clearSeekCareScheduled() {
		this.seekCareScheduled = false;
	}
	
//	public void infectionTimer() {
//		if (this.state == 1) {
//			this.timeInfected ++;
//		}
//	}
	
	//SIMPLE "GETTER" METHODS
	
	public double getTimeToNaturalRecovery() {
		return this.infection.naturalRecoveryTime() - tickNow();
		
	}
	
	public double tickNow() {
		return schedule.getTickCount();
	}
	
	public Infection myInfection() {
		return this.infection;
	}
	
	public String getStrain() {
		return this.infection.getStrain();
	}
	
	public boolean symptoms() {
		return this.infection.symptoms();
	}
	
	public int getState() {
		if (this.infection == null) {
			state = 0;
		} else {
			state = 1;
		}
		return this.state;
	}
	
	public boolean susceptible() {
		if (this.infection == null) {
			state = 0;
			return true;

		} else {
			state = 1;
			return false;
		}
		
		
	}
	
	public boolean infectious() {
		if (this.infection == null) {
			state = 0;
			return false;

		} else {
			state = 1;
			return true;
		}
		
	}
	
	public String getGender() {
		return this.gender;
	}
	
//	public double getGenderPref() {
//		return this.genderPref;
//	}
	
	public String getSubPop() {
		return this.subPop;
	}
	
	public String getRiskGroup() {
		return this.activityGroup;
	}
	
	public boolean inTreatment() {
		return this.inTreatment;
	}
	
	
}

	