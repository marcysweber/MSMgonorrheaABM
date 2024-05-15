package simpleSIR;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import repast.simphony.util.collections.IndexedIterable;

public class SubGrouping {
	public Collection<Indiv> population;
	
	public static List<Indiv> msmList;
	public static List <Indiv> mswList;
	public static List <Indiv> wList;
	
	public static List<Indiv> msmwList;
	public static List<Indiv> nbList;

	
	public SubGrouping(Collection <Indiv> population){
		this.msmList = msm(population);
		this.mswList = msw(population);
		this.wList = w(population);
		
	}

	
	public static List<Indiv> msm(Collection <Indiv> population) {
		Stream<Indiv> indivs = population.stream(); //grabs all objects of class Indiv
		List<Indiv> msmList = indivs.
				filter(indiv -> 
				//((Indiv) indiv).getGenderPref() < 0.25 && 
				indiv.getSubPop().equals("msm")).
				collect(Collectors.toList());
		
		return msmList;
		
	}
	
	public static double msmCount(Collection <Indiv> population) {
		return msmList.size();
	}
	
	public static Stream<Indiv> msmInfected(Collection <Indiv> population){
		return msmList.stream().filter(indiv -> indiv.infectious()).collect(Collectors.toList()).stream();
	}
	
	
	public static List<Indiv> msmw(Collection <Indiv> population) {
		Stream<Indiv> indivs = population.stream();
		List<Indiv> msmwList = indivs.
				filter(indiv -> 
				((Indiv) indiv).getSubPop().equals("msmw")).
				collect(Collectors.toList());
		
		return msmwList;
		
	}
	
	public static double msmwCount(Collection <Indiv> population) {
		return msmw(population).size();
	}
	
	public static Stream<Indiv> msmwInfected(Collection <Indiv> population){
		return msmwList.stream().filter(indiv -> indiv.infectious()).collect(Collectors.toList()).stream();
	}
	
	
	public static List<Indiv> msw(Collection <Indiv> population) {
		Stream<Indiv> indivs = population.stream();
		List<Indiv> mswList = indivs.
				filter(indiv -> 
				((Indiv) indiv).getSubPop().equals("msw")).
				collect(Collectors.toList());
		
		return mswList;
		
	}
	
	public static double mswCount(Collection <Indiv> population) {
		return msw(population).size();
	}
	public static Stream<Indiv> mswInfected(Collection <Indiv> population){
		return mswList.stream().filter(indiv -> indiv.infectious()).collect(Collectors.toList()).stream();
	}
	
	
	public static List<Indiv> w(Collection <Indiv> population) {
		Stream<Indiv> indivs = population.stream();
		List<Indiv> wList = indivs.
				filter(indiv->indiv.getGender().equals("f")).
				collect(Collectors.toList());
		
		return wList;

		
	}
	
	public static double wCount(Collection <Indiv> population) {
		return w(population).size();
	}
	
	public static Stream<Indiv> wInfected(Collection <Indiv> population){
		return wList.stream().filter(indiv -> indiv.infectious()).collect(Collectors.toList()).stream();
	}
	
	
	public static Stream<Indiv> nb(Collection <Indiv> population) {
		Stream<Indiv> indivs = population.stream();
		List<Indiv> nbList = indivs.
				filter(indiv->((Indiv) indiv).getGender().equals("nb")).
				collect(Collectors.toList());
		
		return nbList.stream();

		
	}
	
	public static double nbCount(Collection <Indiv> population) {
		return nb(population).count();
	}
	
	public static Stream<Indiv> nbInfected(Collection <Indiv> population){
		return nb(population).filter(indiv -> indiv.infectious()).collect(Collectors.toList()).stream();
	}
	
	
}
