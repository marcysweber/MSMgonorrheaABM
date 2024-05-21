/**
 * 
 */
package msmOnlyModel;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import repast.simphony.engine.schedule.ScheduledMethod;
import repast.simphony.parameter.Parameters;

/**
 * @author me597
 *
 */
public class SurveillanceProgram {
	private Observer observer; 
	private Parameters parameters;
	
	private String counterfactual;
	private int amountToTest;
	private List<Long> annualListDetectionsA;
	private long annualCumulativeDetectionsA;
	private long thisMonthDetectionsA;
	private double thisMonthRateA;
	
	private List<Long> annualListDetectionsB;
	private long annualCumulativeDetectionsB;
	private long thisMonthDetectionsB;
	private double thisMonthRateB;
	
	private List<Long> annualListDetectionsBoth;
	private long annualCumulativeDetectionsBoth;
	private long thisMonthDetectionsBoth;
	private double thisMonthRateBoth;
	
	private String firstLine;
	private boolean switchtoB;

	public SurveillanceProgram(String counterfactual, Observer observer) {
		this.counterfactual = counterfactual;
		this.observer=observer;
		this.parameters=observer.getParameters();
		if (counterfactual.contains("GISP")) {
			this.amountToTest = 25;
		} else {
			this.amountToTest = 100; //this shouldnt matter b/c conductSurveillance checks for counterfactual before running surveillance
		}
		
		this.annualListDetectionsA = new ArrayList<Long>();
		this.annualCumulativeDetectionsA = 0;
		this.thisMonthDetectionsA = 0;
		this.thisMonthRateA = 0.0;
		
		this.annualListDetectionsB = new ArrayList<Long>();
		this.annualCumulativeDetectionsB = 0;
		this.thisMonthDetectionsB = 0;
		this.thisMonthRateB = 0.0;
		
		this.annualListDetectionsBoth = new ArrayList<Long>();
		this.annualCumulativeDetectionsBoth = 0;
		this.thisMonthDetectionsBoth = 0;
		this.thisMonthRateBoth = 0.0;
		
		this.firstLine = "A";
		this.switchtoB = false;
		
		
	}
	
	@ScheduledMethod(start = 261, interval = 4, priority = 1)
	public void conductSurveillance() {
		if (counterfactual.contains("GISP")) {

			Observer observer = getObserver();
			List<Infection> detected = observer.getDetectedList();
			
			List <Infection >detectedM = detected.stream().filter(inf -> inf.getHostGender().equals("m")).collect(Collectors.toList());

			collectSamples(detectedM);
			addThisMonthToAnnual();
			clearThisMonth(observer);
		}
	}
	
	public double calcDetectedResistantA() {
		double prop = 0.0;
		
		int rounds = annualListDetectionsA.size();
		int totalTests = rounds * amountToTest;
		
		if (rounds > 0) {
			prop = annualCumulativeDetectionsA / (double) totalTests;
		}
		
		annualCumulativeDetectionsA = 0;
		annualListDetectionsA = new ArrayList<Long>();
		
		return prop;
	}
	
	public double calcDetectedResistantB() {
		double prop = 0.0;
		
		int rounds = annualListDetectionsB.size();
		int totalTests = rounds * amountToTest;
		
		if (rounds > 0) {
			prop = annualCumulativeDetectionsB / (double) totalTests;
		}
		
		annualCumulativeDetectionsB = 0;
		annualListDetectionsB = new ArrayList<Long>();
		
		return prop;
	}
	
	public double calcDetectedResistantBoth() {
		double prop = 0.0;
		
		int rounds = annualListDetectionsBoth.size();
		int totalTests = rounds * amountToTest;
		
		if (rounds > 0) {
			prop = annualCumulativeDetectionsBoth / (double) totalTests;
		}
		
		annualCumulativeDetectionsBoth = 0;
		annualListDetectionsBoth = new ArrayList<Long>();
		
		return prop;
	}
	
	public void collectSamples(List<Infection> detected) {
		//int amountToSkip = RandomHelper.nextIntFromTo(0, detected.size() - amountToTest);
	
		CostCalc costCalc = getCostCalc();
		List <Infection> sample = new ArrayList <Infection>();
		
		if (detected.size() > amountToTest) {
			sample = detected.stream()
					//.skip(amountToSkip)
					.limit(amountToTest)
					.collect(Collectors.toList());
			costCalc.strainTestCost(amountToTest);
		} else {
			sample = detected.stream().collect(Collectors.toList());
			costCalc.strainTestCost(detected.size());
		}

		List <String> susProfiles = new ArrayList <String>();

		//System.out.println(sample.size());
		
		for (Infection inf : sample) {
			susProfiles.add(detectSus(inf));
		}

		//System.out.print(strainDetected);

		long AMRcountA = susProfiles.stream().filter(i->!i.contains("A")).count();
		thisMonthDetectionsA = AMRcountA;
		
		long AMRcountB = susProfiles.stream().filter(i->!i.contains("B")).count();
		thisMonthDetectionsB = AMRcountB;
		
		long AMRcountBoth = susProfiles.stream().filter(i->i.equals("XE")).count();
		thisMonthDetectionsBoth = AMRcountBoth;

		double AMRrateA = AMRcountA / (double) sample.size();
		thisMonthRateA = AMRrateA;


		double AMRrateB = AMRcountB / (double) sample.size();
		thisMonthRateB = AMRrateB;


		double AMRrateBoth = AMRcountBoth / (double) sample.size();
		thisMonthRateBoth = AMRrateBoth;



	}
	
	
	public String detectSus(Infection inf) {
		Testing testing = new Testing(inf, parameters);
		String sus = testing.drugSusceptibilityTest();
		return sus;
	}
	
	
	public boolean checkForSwitch(double surveillanceResult) {
		boolean shouldSwitch = false;
		
		double switchPrev = Double.parseDouble(counterfactual.substring(5)) / 100.0;
		if (surveillanceResult >= switchPrev) {
			shouldSwitch = true;
		}
		
		return shouldSwitch;
	}
	
	public void switchToDrugB() {
		this.switchtoB = true;
		this.firstLine = "B";
	}
	
	public void switchToDrugX() {
		this.firstLine = "X";
	}
	
	public void addThisMonthToAnnual() {
		annualListDetectionsA.add(thisMonthDetectionsA);
		annualCumulativeDetectionsA += thisMonthDetectionsA;
		
		annualListDetectionsB.add(thisMonthDetectionsB);
		annualCumulativeDetectionsB += thisMonthDetectionsB;
		
		annualListDetectionsBoth.add(thisMonthDetectionsBoth);
		annualCumulativeDetectionsBoth += thisMonthDetectionsBoth;
	}
	
	public void clearThisMonth(Observer observer) {

		thisMonthDetectionsA = 0;
		thisMonthDetectionsB = 0;
		thisMonthDetectionsBoth = 0;
		observer.clearDetectedList();
	}
	
	public Observer getObserver() {
		return observer;
	}
	
	public CostCalc getCostCalc() {
		return getObserver().getCostCalc();
	}
	
	public long getThisMonthDetectedA() {
		return this.thisMonthDetectionsA;
	}
	
	public long getAnnualDetectedA() {
		return this.annualCumulativeDetectionsA;
	}
	
	public double getThisMonthRateA() {
		return this.thisMonthRateA;
	}
	 
	public boolean getSwitchToB() {
		return this.switchtoB;
	}
	
	public boolean getSwitchToX() {
		return this.firstLine.equals("X");
	}
}
