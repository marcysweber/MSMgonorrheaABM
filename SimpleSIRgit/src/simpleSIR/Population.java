package simpleSIR;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import repast.simphony.engine.schedule.ISchedule;
import repast.simphony.engine.schedule.Schedule;
import repast.simphony.parameter.Parameters;

public class Population {

	private Collection <Indiv> indivs;
	public List<Indiv> msmList;
	public List <Indiv> mswList;
	public List <Indiv> wList;
	
	public List<Indiv> msmwList;
	public List<Indiv> nbList;
	
	public Population(Parameters parameters, int IndivCount, ThreadSafeRandomHelper randomHelper, Observer observer, ISchedule schedule) {
		indivs = new ArrayList<Indiv>();
		for ( int i = 0; i < (IndivCount * 0.5) ; i ++) {
			//initialize as susceptible, to start
			indivs.add(new Indiv(parameters, "w", randomHelper, observer, schedule));
		}
		
		for ( int i = 0; i < (IndivCount * 0.45) ; i ++) {
			//initialize as susceptible, to start
			indivs.add(new Indiv(parameters, "msw", randomHelper, observer, schedule));
		}
		
		for ( int i = 0; i < (IndivCount * 0.05) ; i ++) {
			//initialize as susceptible, to start
			indivs.add(new Indiv(parameters, "msm", randomHelper, observer, schedule));
		}
		
		msmList = msm();
		mswList = msw();
		wList = w();
		
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
	
	
	public List<Indiv> msmw() {
		List<Indiv> msmwList = indivs.stream().
				filter(indiv -> 
				((Indiv) indiv).getSubPop().equals("msmw")).
				collect(Collectors.toList());
		
		return msmwList;
		
	}
	
	public double msmwCount() {
		return msmw().size();
	}
	
	public Stream<Indiv> msmwInfected(){
		return msmwList.stream().filter(indiv -> indiv.infectious()).collect(Collectors.toList()).stream();
	}
	
	
	public List<Indiv> msw() {
		List<Indiv> mswList = indivs.stream().
				filter(indiv -> 
				((Indiv) indiv).getSubPop().equals("msw")).
				collect(Collectors.toList());
		
		return mswList;
		
	}
	
	public double mswCount() {
		return msw().size();
	}
	public Stream<Indiv> mswInfected(){
		return mswList.stream().filter(indiv -> indiv.infectious()).collect(Collectors.toList()).stream();
	}
	
	
	public List<Indiv> w() {
		List<Indiv> wList = indivs.stream().
				filter(indiv->indiv.getGender().equals("f")).
				collect(Collectors.toList());
		
		return wList;

		
	}
	
	public double wCount() {
		return w().size();
	}
	
	public Stream<Indiv> wInfected(){
		return wList.stream().filter(indiv -> indiv.infectious()).collect(Collectors.toList()).stream();
	}
	
	
	public Stream<Indiv> nb() {
		List<Indiv> nbList = indivs.stream().
				filter(indiv->((Indiv) indiv).getGender().equals("nb")).
				collect(Collectors.toList());
		
		return nbList.stream();

		
	}
	
	public double nbCount() {
		return nb().count();
	}
	
	public Stream<Indiv> nbInfected(){
		return nb().filter(indiv -> indiv.infectious()).collect(Collectors.toList()).stream();
	}
	


}
