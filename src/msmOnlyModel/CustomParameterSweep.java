/**
 * 
 */
package msmOnlyModel;

import cern.jet.random.Uniform;
import cern.jet.random.Beta;
import cern.jet.random.Gamma;
import cern.jet.random.engine.RandomEngine;
import repast.simphony.random.RandomHelper;

import java.util.ArrayList;
import java.util.List;

/**
 * @author me597
 * 
 *this should be a class to run 
 *a parameter sweep - i.e., a 
 *set of simulation runs where the parameter
 *values are drawn from a distribution
 *
 */
public class CustomParameterSweep {
	
	public CustomParameterSweep() {}
	

	public List<Double> getSeedValues(int samples){
		double seedMin = 0;
		double seedMax = 100000;
		int seed = (int) System.currentTimeMillis();
		return getUniformSweepValues(seed, samples, seedMin, seedMax);
	}
	
	public List<Integer> getInitialInfectedValues(int samples){
		int infectedMin = 4000;
		int infectedMax = 5500;
		int seed = (int) System.currentTimeMillis() + 1;
		
		return getUniformIntSweepValues(seed, samples, infectedMin, infectedMax);
		
	}
		
	
	//transmission parameters
	public List<Double> getTransmissionMSMValues(int samples){
		double annualContactsMin = 1.5;
		double annualContactsMax = 5.5;
		int seed = (int) System.currentTimeMillis() + 2;
		return getUniformSweepValues(seed, samples, annualContactsMin, annualContactsMax);
	}
	
	public List<Double> getTransmissionMSWValues(int samples){
		double annualContactsMin = 0.0;
		double annualContactsMax = 10;
		int seed = (int) System.currentTimeMillis() + 3;
		return getUniformSweepValues(seed, samples, annualContactsMin, annualContactsMax);
	}
	
	public List<Double> getTransmissionFValues(int samples){
		double annualContactsMin = 0.0;
		double annualContactsMax = 10;
		int seed = (int) System.currentTimeMillis() + 4;
		return getUniformSweepValues(seed, samples, annualContactsMin, annualContactsMax);
	}
	
	
	//recovery parameter
	public List<Double> getRecoveryLambdaValues(int samples) {
		double recoveryLambdaMin = 0.3;
		double recoveryLambdaMax = 4.0;
		int seed = (int) System.currentTimeMillis() + 5;
		return getUniformSweepValues(seed, samples, recoveryLambdaMin, recoveryLambdaMax);
	}
	
	
	//probSymptomatic parameters
	public List<Double> getProbSymptomaticMSMValues(int samples){
		double probSymptomaticMin = 0.15;
		double probSymptomaticMax = 0.55;
		int seed = (int) System.currentTimeMillis() + 6;
		return getUniformSweepValues(seed, samples, probSymptomaticMin, probSymptomaticMax);
	}
	
	public List<Double> getProbSymptomaticMSWValues(int samples){
		double probSymptomaticMin = 0.1;
		double probSymptomaticMax = 0.9;
		int seed = (int) System.currentTimeMillis() + 7;
		return getUniformSweepValues(seed, samples, probSymptomaticMin, probSymptomaticMax);
	}

	public List<Double> getProbSymptomaticFValues(int samples){
		double probSymptomaticMin = 0.1;
		double probSymptomaticMax = 0.9;
		int seed = (int) System.currentTimeMillis() + 8;
		return getUniformSweepValues(seed, samples, probSymptomaticMin, probSymptomaticMax);
	}
	
	
	
	
	
	//screen interval parameters
	public List<Double> getScreenIntervalMSMValues(int samples){
		double screenIntervalMin = 1.75;
		double screenIntervalMax = 4.5;
		int seed = (int) System.currentTimeMillis() + 9;
		return getUniformSweepValues(seed, samples, screenIntervalMin, screenIntervalMax);
	}
	
	public List<Double> getScreenIntervalMSWValues(int samples){
		double screenIntervalMin = 0.3;
		double screenIntervalMax = 5;
		int seed = (int) System.currentTimeMillis() + 10;
		return getUniformSweepValues(seed, samples, screenIntervalMin, screenIntervalMax);
	}
	
	public List<Double> getScreenIntervalWValues(int samples){
		double screenIntervalMin = 0.3;
		double screenIntervalMax = 5;
		int seed = (int) System.currentTimeMillis() + 11;
		return getUniformSweepValues(seed, samples, screenIntervalMin, screenIntervalMax);
	}
	
	
	// delay to seek care parameters
	public List<Double> getDelayToSeekCareMSMValues(int samples){
		double delayToSeekCareMin = 0;
		double delayToSeekCareMax = 2.0/52.0;
		int seed = (int) System.currentTimeMillis() + 12;
		return getUniformSweepValues(seed, samples, delayToSeekCareMin, delayToSeekCareMax);
	}
	
	public List<Double> getDelayToSeekCareMSWValues(int samples){
		double delayToSeekCareMin = 0;
		double delayToSeekCareMax = 2.0/52.0;
		int seed = (int) System.currentTimeMillis() + 13;
		return getUniformSweepValues(seed, samples, delayToSeekCareMin, delayToSeekCareMax);
	}
	
	public List<Double> getDelayToSeekCareFValues(int samples){
		double delayToSeekCareMin = 0;
		double delayToSeekCareMax = 2.0/52.0;
		int seed = (int) System.currentTimeMillis() + 14;
		return getUniformSweepValues(seed, samples, delayToSeekCareMin, delayToSeekCareMax);
	}

	
	//delay to retreatment parameters
	public List<Double> getDelayToRetreatmentMSMValues(int samples){
		double delayToRetreatmentMin = 0;
		double delayToRetreatmentMax = 4.0/52.0;
		int seed = (int) System.currentTimeMillis() + 15;
		return getUniformSweepValues(seed, samples, delayToRetreatmentMin, delayToRetreatmentMax);
	}
	
	public List<Double> getDelayToRetreatmentMSWValues(int samples){
		double delayToRetreatmentMin = 0;
		double delayToRetreatmentMax = 4.0/52.0;
		int seed = (int) System.currentTimeMillis() + 16;
		return getUniformSweepValues(seed, samples, delayToRetreatmentMin, delayToRetreatmentMax);
	}
	
	public List<Double> getDelayToRetreatmentFValues(int samples){
		double delayToRetreatmentMin = 0;
		double delayToRetreatmentMax = 4.0/52.0;
		int seed = (int) System.currentTimeMillis() + 17;
		return getUniformSweepValues(seed, samples, delayToRetreatmentMin, delayToRetreatmentMax);
	}
	
	//resistance parameters
	public List<Double> getPercentResistantA(int samples){
		double min = 0.001;
		double max = 0.02;
		int seed = (int) System.currentTimeMillis() + 18;
		return getUniformSweepValues(seed, samples, min, max);
	}
	
	public List<Integer> getBeginImportingB(int samples){
		//between year 15 and year 20
		double min = 15.0 * 52.0;
		double max = 20.0 * 52.0;
		int seed = (int) System.currentTimeMillis() + 19;
		return getUniformIntSweepValues(seed, samples, min, max);
	}
	
	public List<Double> getImportingBInterval(int samples){
		//expected value of exponential in years
		double min = 4.0 / 52.0;
		double max = 2.0 * 52.0;
		int seed = (int) System.currentTimeMillis() + 20;
		return getUniformSweepValues(seed, samples, min, max);
	}
	
	
	//sensitivity and specificity parameters
	public List<Double> getDSTsensitivity(int samples){
		//expected value of exponential in years
		double mean = 0.95;
		double sd = 0.01;
		int seed = (int) System.currentTimeMillis() + 21;
		return getBetaSweepValues(seed, samples, mean, sd);
	}

	public List<Double> getDSTspecificity(int samples){
		//expected value of exponential in years
		double mean = 0.97;
		double sd = 0.005;
		int seed = (int) System.currentTimeMillis() + 22;
		return getBetaSweepValues(seed, samples, mean, sd);
	}
	
	
	
	
	
	
	public List<Double> getCareCostValues(int samples){
		//short clinic visit + treatment of urethritis
		double careCostMean = 41+92;
		double careCostSD = 61 + 136 - careCostMean;
		int seed = (int) System.currentTimeMillis() + 23;
		return getGammaSweepValues(seed, samples, careCostMean, careCostSD);

	}
	
	public List<Double> getTestCostValues(int samples){
		//cost of diagnosis
		double testCostMean = 68;
		double testCostSD = 100 - testCostMean;
		int seed = (int) System.currentTimeMillis() + 24;
		return getGammaSweepValues(seed, samples, testCostMean, testCostSD);

	}
	
	public List<Double> getStrainTestCostValues(int samples){
		//cost of drug susceptibility testing
		double strainTestMean = 150;
		double strainTestSD = 50;
		int seed = (int) System.currentTimeMillis() + 25;
		return getGammaSweepValues(seed, samples, strainTestMean, strainTestSD);

	}
	
	public List<Double> getTreatmentACostValues(int samples){
		//cost of treatments with drug A or B (ceftriaxone)
		double FLcostMean = 24;
		double FLcostSD = 12;
		int seed = (int) System.currentTimeMillis() + 26;
		return getGammaSweepValues(seed, samples, FLcostMean, FLcostSD);

	}
	public List<Double> getTreatmentBCostValues(int samples){
		//cost of treatments with drug A or B (ceftriaxone)
		double FLcostMean = 24;
		double FLcostSD = 12;
		int seed = (int) System.currentTimeMillis() + 27;
		return getGammaSweepValues(seed, samples, FLcostMean, FLcostSD);

	}
	
	public List<Double> getTreatmentXCostValues(int samples){
		//cost of treatments with drug A or B (ceftriaxone)
		double FLcostMean = 24;
		double FLcostSD = 12;
		int seed = (int) System.currentTimeMillis() + 28;
		return getGammaSweepValues(seed, samples, FLcostMean, FLcostSD);

	}
	
	public List<Double> getTreatmentECostValues(int samples){
		//cost of treatment with drug M (ertapenem)
		double SLCostMean = 537;
		double SLCostSD = 782 - SLCostMean;
		int seed = (int) System.currentTimeMillis() + 29;
		return getGammaSweepValues(seed, samples, SLCostMean, SLCostSD);

	}
	
	
	
	public List<Double> getUniformSweepValues(int seed, int samples, double min, double max){
		RandomEngine eng = RandomHelper.registerGenerator("myStream", seed);
		Uniform uniDist = new Uniform(min, max, eng);
		
		ArrayList<Double> values = new ArrayList<Double>();
		
		for (int i = 0; i < samples; i++) {
			double newValue = uniDist.nextDouble();
			values.add(newValue);
		}
		
		return values;
	}
	
	
	public List<Integer> getUniformIntSweepValues(int seed, int samples, double min, double max){
		RandomEngine eng = RandomHelper.registerGenerator("myStream", seed);
		Uniform uniDist = new Uniform(min, max, eng);
		
		ArrayList<Integer> values = new ArrayList<Integer>();
		
		for (int i = 0; i < samples; i++) {
			int newValue = uniDist.nextInt();
			values.add(newValue);
		}
		
		return values;
	}

	
	public List<Double> getGammaSweepValues(int seed, int samples, double mean, double sd){
		RandomEngine eng = RandomHelper.registerGenerator("myStream", seed);
		double alpha = (mean * mean) /sd;
		double lambda = 1 / (sd / mean);
		
		Gamma gammaDist = new Gamma(alpha, lambda, eng);
				
		ArrayList<Double> values = new ArrayList<Double>();
		
		for (int i = 0; i < samples; i++) {
			double newValue = gammaDist.nextDouble();
			values.add(newValue);
		}
		

		return values;
	}
	
	
	public List<Double> getBetaSweepValues(int seed, int samples, double mean, double sd){
		RandomEngine eng = RandomHelper.registerGenerator("myStream", seed);
		double v = ((mean * (1-mean))/Math.pow(sd,2)) - 1;
		double alpha = mean * v;
		double beta = v - alpha;
		Beta betaDist = new Beta(alpha, beta, eng);		
		
		ArrayList<Double> values = new ArrayList<Double>();
		
		for (int i = 0; i < samples; i++) {
			double newValue = betaDist.nextDouble();
			values.add(newValue);
		}
		
		return values;
	}
}
