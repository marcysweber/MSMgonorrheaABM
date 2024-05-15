/**
 * 
 */
package simpleSIR;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import cern.jet.random.Beta;
import cern.jet.random.Exponential;
import cern.jet.random.Gamma;
import cern.jet.random.Normal;
import cern.jet.random.Uniform;
import cern.jet.random.engine.RandomEngine;
import repast.simphony.context.Context;
import repast.simphony.dataLoader.ContextBuilder;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.environment.RunState;
import repast.simphony.engine.schedule.ISchedule;
import repast.simphony.engine.schedule.Schedule;
import repast.simphony.engine.schedule.ScheduleParameters;
import repast.simphony.parameter.Parameters;
import repast.simphony.random.RandomHelper;


/**
 * @author marcy
 *
 */
public class SimpleSIRBuilder implements ContextBuilder<Object> {

	@Override
	public int hashCode() {
		// TODO Auto-generated method stub
		return super.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		// TODO Auto-generated method stub
		return super.equals(obj);
	}

	@Override
	protected Object clone() throws CloneNotSupportedException {
		// TODO Auto-generated method stub
		return super.clone();
	}

	@Override
	public String toString() {
		// TODO Auto-generated method stub
		return super.toString();
	}

	@Override
	protected void finalize() throws Throwable {
		// TODO Auto-generated method stub
		super.finalize();
	}
	
	@Override
	public Context build(Context <Object> context) {
		
		//System.out.print("building...");
		String contextID = String.valueOf(System.currentTimeMillis());
		
		context.setId("SimpleSIR"); 
		
        //ISchedule schedule = RunState.getInstance().getScheduleRegistry().getModelSchedule();
        ISchedule schedule = RunEnvironment.getInstance().getCurrentSchedule();

        
		//access parameters
        Parameters params = RunEnvironment.getInstance().getParameters();
		int seed = params.getInteger("randomSeed");
		String counterfactual = params.getString("counterfactual");
		String resistance = params.getString("resistance");
		int yearX = params.getInteger("yearX");
		
		ThreadSafeRandomHelper randomHelper = registerDistributions(context, seed, 
				params.getDouble("recovery_lambda"), 
				params.getDouble("screen_interval") * 52, 
				params.getInteger("population_size"),
				params.getDouble("delay_to_seek_care") * 52,
				params.getDouble("delay_to_retreatment") * 52);
		context.add(randomHelper);
		
		Observer observer = createObserver(context, seed, counterfactual, resistance, yearX, schedule);
		context.add(observer);
		
		createSurveillance(context, schedule, counterfactual);
		
		createIndivs(params.getInteger("population_size"), context, randomHelper, observer, schedule);
		SubGrouping groups = new SubGrouping(context);
		context.add(groups);
		
		
		infectInitialInfected(context, schedule, 
				params.getInteger("infected_count_init"), params.getInteger("population_size"), groups);
		
		 
		if (!resistance.equals("none")) {
			scheduleInsertResistance(context, schedule, resistance);
		}
		
	
		
		//RunEnvironment.getInstance().endAt(1300);
		
		return context;
	}
	
	
	public Context testBuild(Context <Object> testContext) {
		testContext.setId("SimpleSIRTest"); 

		return testContext;
		
	}

	public ThreadSafeRandomHelper registerDistributions(Context<Object> context, int seed, double recoveryLambda, double screenInterval, int popSize, double delayToSeekCare, double delayToRetreatment) {
		//set seed and random distributions
				ThreadSafeRandomHelper randomHelper = new ThreadSafeRandomHelper();
		
				randomHelper.setSeed(seed);
				RandomEngine eng = randomHelper.registerGenerator("myStream", seed);
				
				Uniform genderUniform = new Uniform(0.0, 1.0, eng);
				randomHelper.registerDistribution("genderUniform", genderUniform);
				
				Beta genderPrefBeta = new Beta(0.5, 0.05, eng);
				randomHelper.registerDistribution("genderPrefBeta", genderPrefBeta);
				
				//dist for recovery
				Exponential recoveryExp = new Exponential(1/recoveryLambda, eng);
				randomHelper.registerDistribution("recoveryExp", recoveryExp);
				
				//dist for contact
				Uniform zeroOneUniform = new Uniform(0.0, 1.0, eng);
				randomHelper.registerDistribution("zeroOneUniform", zeroOneUniform);
				
				Uniform partnerGenderUniform = new Uniform(0.0, 1.0, eng);
				randomHelper.registerDistribution("partnerGenderUniform", partnerGenderUniform);
				
				Uniform symptomaticUniform = new Uniform(0.0,1.0, eng);
				randomHelper.registerDistribution("symptomaticUniform", symptomaticUniform);
				
				Normal screenIntervalNormal = new Normal(screenInterval, screenInterval/10, eng);
				randomHelper.registerDistribution("screenIntervalNormal", screenIntervalNormal);
				Uniform screenFirstValueUniform = new Uniform(0, screenInterval, eng);
				randomHelper.registerDistribution("screenFirstValueUniform", screenFirstValueUniform);
				
				Uniform adherenceUniform = new Uniform(0.0, 1.0, eng);
				randomHelper.registerDistribution("adherenceUniform", adherenceUniform);
				
				Uniform randomDrugUniform = new Uniform(0.0, 1.0, eng);
				randomHelper.registerDistribution("randomDrugUniform", randomDrugUniform);
				
				Uniform partnerSelectUniform = new Uniform(0, popSize - 1, eng);
				randomHelper.registerDistribution("partnerSelectUniform", partnerSelectUniform);
				
				Uniform importResistantUniform = new Uniform(0, popSize - 1, eng);
				randomHelper.registerDistribution("importResistantUniform", importResistantUniform);
				
				Uniform developResistanceUniform = new Uniform(0.0,1.0, eng);
				randomHelper.registerDistribution("developResistanceUniform", developResistanceUniform);
				
				Exponential delayToSeekCareExp = new Exponential(1/delayToSeekCare, eng);
				randomHelper.registerDistribution("delayToSeekCareExp", delayToSeekCareExp);
				
				Exponential delayToRetreatmentExp = new Exponential(1/delayToRetreatment, eng);
				randomHelper.registerDistribution("delayToRetreatmentExp", delayToRetreatmentExp);
				
				Uniform testsUniform = new Uniform(0.0, 1.0, eng);
				randomHelper.registerDistribution("testsUniform", testsUniform);
				
				return randomHelper;
				
	}
	
	public Observer createObserver(Context <Object> context, int seed, String counterfactual, String resistance, int yearX, ISchedule schedule) {
		//add Observer
				Observer observer = new Observer(new CustomFileOutput(counterfactual, resistance, yearX),seed, 0,0, schedule);
				
				ScheduleParameters schparams = ScheduleParameters.createRepeating(0, 52);
				schedule.schedule(schparams, observer, "calcObserver");
				
				ScheduleParameters schparams2 = ScheduleParameters.createRepeating(4, 4);
				schedule.schedule(schparams2, observer, "checkStopCondition");
				
				
				scheduleAddX(context, schedule, yearX, observer);
				return observer;

	}
	
	public void createSurveillance(Context <Object> context, ISchedule schedule, String counterfactual) {
		SurveillanceProgram surveillance = new SurveillanceProgram();
		context.add(surveillance);
		
		scheduleSurveillance(schedule, surveillance, counterfactual);
	}
	
	public void scheduleSurveillance(ISchedule schedule, SurveillanceProgram surveillance, String counterfactual) {
		
		if (counterfactual.contains("GISP")) {
			scheduleGISP(schedule, surveillance);
		} else if (counterfactual.contains("sporadic")) {
			scheduleSporadicSurveillance(schedule, surveillance);
		}
	}
	
	public void scheduleGISP(ISchedule schedule, SurveillanceProgram surveillance) {
		ScheduleParameters schparams = ScheduleParameters.createRepeating(4, 4);
		schedule.schedule(schparams, surveillance, "conductSurveillance");
	}
	
	public void scheduleSporadicSurveillance(ISchedule schedule, SurveillanceProgram surveillance) {
		
	}
	
	public void createIndivs(int IndivCount, Context <Object> context, ThreadSafeRandomHelper randomHelper, Observer observer, ISchedule schedule) {
		//create population
				for ( int i = 0; i < (IndivCount * 0.5) ; i ++) {
					//initialize as susceptible, to start
					context.add(new Indiv("w", randomHelper, observer, schedule));
				}
				
				for ( int i = 0; i < (IndivCount * 0.45) ; i ++) {
					//initialize as susceptible, to start
					context.add(new Indiv("msw", randomHelper, observer, schedule));
				}
				
				for ( int i = 0; i < (IndivCount * 0.05) ; i ++) {
					//initialize as susceptible, to start
					context.add(new Indiv("msm", randomHelper, observer, schedule));
				}
	}
	
	public void infectInitialInfected(Context <Object> context, ISchedule schedule, int InfectiousCount, int IndivCount, SubGrouping subGroups) {
		List<Object> indivToInfectList = new ArrayList<Object>();
		
		double amountToInfectMSM = (InfectiousCount/(double)IndivCount) * subGroups.msmCount(context);
		List <Object> MSMtoInfect = subGroups.msmList.stream().limit((long) amountToInfectMSM).collect(Collectors.toList());
		indivToInfectList.addAll(MSMtoInfect);
		
		double amountToInfectMSW = (InfectiousCount/(double)IndivCount) * subGroups.mswCount(context);
		List <Object> MSWtoInfect = subGroups.mswList.stream().limit((long) amountToInfectMSW).collect(Collectors.toList());
		indivToInfectList.addAll(MSWtoInfect);
		
		double amountToInfectW = (InfectiousCount/(double)IndivCount) * subGroups.wCount(context);
		List <Object> WtoInfect = subGroups.wList.stream().limit((long) amountToInfectW).collect(Collectors.toList());
		indivToInfectList.addAll(WtoInfect);
		
		//infect the infectious indivs
		ScheduleParameters schparams = ScheduleParameters.createOneTime(-0.5);
		for (Object i : indivToInfectList) {
			Indiv infected = (Indiv) i;
			schedule.schedule(schparams, infected, "infectInit");
		}
	}
	
	public void scheduleInsertResistance(Context <Object> context, ISchedule schedule, String resistance) {
		
		//use the new resistance param (String) to select which insert resistance version to use.
		// write an experiemtn in main which compares different resistance insertion "strategies" under GISP
		
		InsertResistance resistanceInserter = new InsertResistance(resistance);
		context.add(resistanceInserter);
		
		ScheduleParameters schparams = ScheduleParameters.createOneTime(521);
		
		if (resistance.equals("combo")) {
			schedule.schedule(schparams, resistanceInserter, "comboResistanceConvertAndImport", context);
		} else if (resistance.equals("constantImport")) {
			schedule.schedule(schparams, resistanceInserter, "insertResistance", context);
		} else if (resistance.equals("dropInOnce")) {
			schedule.schedule(schparams, resistanceInserter, "dropInResistant", context);
		} else if (resistance.equals("convertOnce")) {
			schedule.schedule(schparams, resistanceInserter, "convertToResistant", context);
		} else if (resistance.equals("developWithTreatment")) {
			
		}
		
	}
	
	public void scheduleAddX(Context <Object> context, ISchedule schedule, int yearX, Observer observer) {
		double weekX = yearX * 52.0;
		
		ScheduleParameters schparams = ScheduleParameters.createOneTime(weekX);
		schedule.schedule(schparams, observer, "addX");
	}
	
	
}

