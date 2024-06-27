/**
 * 
 */
package msmOnlyModel;

import cern.jet.random.Exponential;
import repast.simphony.engine.schedule.ISchedule;
import repast.simphony.engine.schedule.ScheduleParameters;

/**
 * @author me597
 *
 */
public class CareSeeking {
	private Indiv indiv;
	private Observer observer;
	private ISchedule schedule;
	private ThreadSafeRandomHelper randomHelper;

	public CareSeeking(Indiv indiv, Observer observer, ISchedule schedule) {
		this.indiv = indiv;
		this.observer = observer;
		this.schedule = schedule;
		this.randomHelper = indiv.getRandomHelper();
	}
	
	public boolean scheduleSeekCare() {
		//schedule the careseeking according to delayToSeekCare
		Exponential delayToSeekCareExp = null;
		
		if (indiv.getGender().equals("w")) {
			delayToSeekCareExp = (Exponential) randomHelper.getDistribution("delayToSeekCareFExp");
		} else if (indiv.getSubPop().equals("msm")) {
			delayToSeekCareExp = (Exponential) randomHelper.getDistribution("delayToSeekCareMSMExp");
		} else if (indiv.getSubPop().equals("msw")) {
			delayToSeekCareExp = (Exponential) randomHelper.getDistribution("delayToSeekCareMSWExp");
		} else {
			delayToSeekCareExp = (Exponential) randomHelper.getDistribution("delayToSeekCareFExp");
		}
		
		double thisDelayToSeekCare = delayToSeekCareExp.nextDouble();
		
		//System.out.println("delay: ");
		//System.out.println(thisDelayToSeekCare);
		observer.getCostCalc().symptomaticQALYsLost(indiv, thisDelayToSeekCare);
		ScheduleParameters schparams = ScheduleParameters.createOneTime(thisDelayToSeekCare + indiv.tickNow());
		schedule.schedule(schparams, this, "seekCare");
		return true;
		//observer.getCostCalc().addPersonDaysSymptomatic();
	}
	
	public void seekCare() {
		indiv.clearSeekCareScheduled();
		observer.recordSoughtCare(indiv);
		observer.getCostCalc().careCost(indiv);
		observer.getCostCalc().testCost(indiv);

		if (indiv.infectious() && indiv.symptoms()) { //confirm infectious and symptoms
			observer.getCostCalc().addPersonDaysSymptomatic();
			observer.recordNewDetected(indiv);
			observer.recordNewDetectedAndSymptoms(indiv);

			Treatment treatment = new Treatment(indiv, observer);
			treatment.treat();
		}
		
	}
	
	
}
