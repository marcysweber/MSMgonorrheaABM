package msmOnlyModel;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import repast.simphony.engine.schedule.ISchedule;
import repast.simphony.parameter.Parameters;

public class Population {

	private Collection <Indiv> indivs;
	public List<Indiv> msmList;
//	public List <Indiv> mswList;
//	public List <Indiv> wList;
//	
//	public List<Indiv> msmwList;
//	public List<Indiv> nbList;
	
	public List <Indiv> highActivityGroup;
	public List <Indiv> lowActivityGroup;
	
	
	
	public Population(Parameters parameters, int IndivCount, ThreadSafeRandomHelper randomHelper, Observer observer, ThreadSafeSchedule schedule) {
		indivs = new ArrayList<Indiv>();
		
		String subPop = "msm";
		//all MSM
		
		double propHighActivity = parameters.getDouble("propHighActivity");
		int countHighActivity = (int) (propHighActivity * IndivCount);
		int countLowActivity = IndivCount - countHighActivity;
		
		for ( int i = 0; i < countHighActivity; i ++) {
			//initialize as susceptible, to start
			indivs.add(new Indiv(parameters, subPop, "high", "high", randomHelper, observer, schedule));
		}
		
		for ( int i = 0; i < countLowActivity; i ++) {
			//initialize as susceptible, to start
			indivs.add(new Indiv(parameters, subPop, "low", "low", randomHelper, observer, schedule));
		}
		
		msmList = msm();
		
		highActivityGroup = highActivityGroup();
		lowActivityGroup = lowActivityGroup();
		

		
		
		//mswList = msw();
		//wList = w();
		
	}


	public List<Indiv> msm() {
		List<Indiv> msmList = indivs.stream().unordered().
				filter(indiv -> 
				//((Indiv) indiv).getGenderPref() < 0.25 && 
				indiv.getSubPop().equals("msm")).
				collect(Collectors.toList());
		
		return msmList;
		
	}
	
	public void updateActivityGroups() {
		highActivityGroup = highActivityGroup();
		lowActivityGroup = lowActivityGroup();

		
	}
	
	public List<Indiv> lowActivityGroup(){
		List<Indiv> lowRiskGroup = indivs.stream().unordered().
				filter(indiv -> 
				//((Indiv) indiv).getGenderPref() < 0.25 && 
				indiv.getRiskGroup().equals("low")).
				collect(Collectors.toList());
		
		return lowRiskGroup;
	}
	
	public List<Indiv> highActivityGroup(){
		List<Indiv> highRiskGroup = indivs.stream().unordered().
				filter(indiv -> 
				//((Indiv) indiv).getGenderPref() < 0.25 && 
				indiv.getRiskGroup().equals("high")).
				collect(Collectors.toList());
		
		return highRiskGroup;
	}
	
	


	public Stream<Indiv> lowActivityGroupStream(){
		return lowActivityGroup.stream().unordered();
	}


	public int lowRiskCount() {
		return lowActivityGroup.size();
	}


	public Stream <Indiv> highActivityGroupStream(){
		
		
		return highActivityGroup.stream().unordered();
	}
	
	public int highRiskCount() {
		return highActivityGroup.size();
	}

	
	public int totalSize() {
		return indivs.size();
	}
	
	public Stream <Indiv> allIndivs(){
		return indivs.stream().unordered();
	}
	
	public Stream <Indiv> allInfectious(){
		return allIndivs().filter(indiv -> indiv.getState()==1).collect(Collectors.toList()).stream().unordered();
	}
	
	
	public Stream <Indiv> lowRiskInfected(){
		return lowActivityGroup.stream().filter(indiv -> indiv.getState()==1).collect(Collectors.toList()).stream().unordered();

	}
	
	public Stream <Indiv> highRiskInfected(){
		return highActivityGroup.stream().filter(indiv -> indiv.getState()==1).collect(Collectors.toList()).stream().unordered();

	}
	
	
	public long infectiousCount(){
		return allIndivs().filter(indiv -> indiv.getState()==1).count();
	}
	
	public double msmCount() {
		return msmList.size();
	}
	
	public Stream<Indiv> msmInfected(){
		return msmList.stream().filter(indiv -> indiv.infectious()).collect(Collectors.toList()).stream().unordered();
	}
	
	public void add(Indiv indiv) {
		indivs.add(indiv);
		msmList = msm();
	}
	
	

}
