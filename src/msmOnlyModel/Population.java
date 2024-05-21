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
	
	public Population(Parameters parameters, int IndivCount, ThreadSafeRandomHelper randomHelper, Observer observer, ISchedule schedule) {
		indivs = new ArrayList<Indiv>();
		
		//all MSM
		
		for ( int i = 0; i < IndivCount; i ++) {
			//initialize as susceptible, to start
			indivs.add(new Indiv(parameters, "msm", randomHelper, observer, schedule));
		}
		
		msmList = msm();
		//mswList = msw();
		//wList = w();
		
	}


	public List<Indiv> msm() {
		List<Indiv> msmList = indivs.stream().
				filter(indiv -> 
				//((Indiv) indiv).getGenderPref() < 0.25 && 
				indiv.getSubPop().equals("msm")).
				collect(Collectors.toList());
		
		return msmList;
		
	}
	
	public int totalSize() {
		return indivs.size();
	}
	
	public Stream <Indiv> allIndivs(){
		return indivs.stream();
	}
	
	public Stream <Indiv> allInfectious(){
		return allIndivs().filter(indiv -> indiv.getState()==1).collect(Collectors.toList()).stream();
	}
	
	public long infectiousCount(){
		return allIndivs().filter(indiv -> indiv.getState()==1).count();
	}
	
	public double msmCount() {
		return msmList.size();
	}
	
	public Stream<Indiv> msmInfected(){
		return msmList.stream().filter(indiv -> indiv.infectious()).collect(Collectors.toList()).stream();
	}
	
	

}
