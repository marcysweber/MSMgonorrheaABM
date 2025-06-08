/**
 * 
 */
package msmOnlyModel;

import java.util.ArrayList;
import java.util.List;

import cern.jet.random.Normal;
import cern.jet.random.Uniform;
import repast.simphony.parameter.Parameters;

/**
 * @author me597
 *
 */
public class Screener {

	public Screener() {}
	
	public void screen(Indiv indiv, Observer observer) {
		if (indiv.infectious() && !indiv.myInfection().isDetected()) {
			//observer.recordNewDetectedThruScreen(indiv);
			indiv.myInfection().detect();
			indiv.myInfection().screen();
			indiv.myInfection().recordDiagnosticTest();
			//if (indiv.symptoms()) {observer.recordNewDetectedAndSymptoms(indiv);}
			
			Treatment treatment = new Treatment(indiv.myInfection(), observer);
			try {
				treatment.treat();
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}
	
	public List<Integer> makeLowScreenSchedule(ThreadSafeRandomHelper randomHelper, String subPop) {
		Normal screenIntervalDist = null;
		Uniform firstValueDist = null;
		
		if (subPop.equals("msm")) {
				screenIntervalDist = (Normal) randomHelper.getDistribution("screenIntervalMSMNormal");
				firstValueDist = (Uniform) randomHelper.getDistribution("screenFirstValueMSMUniform");
		} 

		//Parameters params = RunEnvironment.getInstance().getParameters();
		double endTime = 1560; //need to set to max, or else slight var between sweep and cal
		
		List screenings = new ArrayList<Integer>();
		screenings.add(firstValueDist.nextInt());
		
		//while the last value is still less than 1300
		while ((int) screenings.get(screenings.size()-1) < endTime) {
			//draw a new interval value from distribution
			int newInterval = screenIntervalDist.nextInt();
			//add that interval to the last value
			int newValue = newInterval + (int) screenings.get(screenings.size()-1);
			//add this new value to the list
			screenings.add(newValue);
			
		}
		
		return screenings;
	}
	
	public List<Integer> makeHighScreenSchedule(ThreadSafeRandomHelper randomHelper, String subPop) {
		Normal screenIntervalDist = null;
		Uniform firstValueDist = null;
		
		if (subPop.equals("msm")) {
				screenIntervalDist = (Normal) randomHelper.getDistribution("screenIntervalMSMHigh");
				firstValueDist = (Uniform) randomHelper.getDistribution("screenFirstValueMSMHigh");
		} 

		//Parameters params = RunEnvironment.getInstance().getParameters();
		double endTime = 1560; //need to set to max, or else slight var between sweep and cal
		
		List screenings = new ArrayList<Integer>();
		screenings.add(firstValueDist.nextInt());
		
		//while the last value is still less than 1300
		while ((int) screenings.get(screenings.size()-1) < endTime) {
			//draw a new interval value from distribution
			int newInterval = screenIntervalDist.nextInt();
			//add that interval to the last value
			int newValue = newInterval + (int) screenings.get(screenings.size()-1);
			//add this new value to the list
			screenings.add(newValue);
			
		}
		
		return screenings;
	}
}
