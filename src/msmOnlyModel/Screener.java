/**
 * 
 */
package msmOnlyModel;

import java.util.ArrayList;
import java.util.List;
import cern.jet.random.Uniform;

import org.apache.commons.math3.distribution.GammaDistribution;

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
	
	public List<Integer> makeScreenSchedule(ThreadSafeRandomHelper randomHelper, String subPop) {
		GammaDistribution screenIntervalDist = randomHelper.getCMGammaDistribution("screeningIntervalMSMGamma");
		Uniform screenIntervalFirstValue = (Uniform) randomHelper.getDistribution("screeningIntervalUniform");
		
		double endTime = 1560; //need to set to max, or else slight var between sweep and cal

		List<Integer> screenings = new ArrayList<Integer>();
		screenings.add(screenIntervalFirstValue.nextInt());

		while (screenings.get(screenings.size()-1) < endTime) {
			int newInterval = (int) screenIntervalDist.sample();
			int newValue = newInterval + screenings.get(screenings.size()-1);
			screenings.add(newValue);
		}

		return screenings;
	}
}
