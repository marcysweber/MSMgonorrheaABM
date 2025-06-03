package msmOnlyModel;

import java.util.stream.Stream;

import cern.jet.random.Normal;
import repast.simphony.engine.schedule.ISchedule;
import repast.simphony.parameter.Parameters;

public class ChangeActivityGroups {
	private Parameters parameters;
	private ThreadSafeRandomHelper randomHelper;
	private ISchedule schedule;
	private Population population;
	private Normal propDist;
	
	public ChangeActivityGroups(Parameters parameters, ThreadSafeRandomHelper randomHelper, ISchedule schedule, Population population) {
		this.parameters = parameters;
		this.randomHelper = randomHelper;
		this.schedule = schedule;
		this.population = population;
		this.propDist = (Normal) randomHelper.getDistribution("riskGroupTransferPropNormal");
	}
	
	public void changeActivityGroups() {
		//happens once per year
		//parameter risk_group_transfer_prop determines what fraction of the pop swaps risk groups
		
		double propToChange = propDist.nextDouble();
		
		long numberToTransfer = (long) (propToChange * population.totalSize());
		
		
		//process low risk to high risk
		
		long numberToTransferLow = Math.round(numberToTransfer / 2);
		//System.out.println("Transfering " + numberToTransferLow + " from low to high.");
		
		Stream <Indiv> lowToTransfer = population.lowActivityGroupStream().limit(numberToTransferLow);

		
		//process high risk to low risk

		long numberToTransferHigh = Math.round(numberToTransfer / 2);
		//System.out.println("Transfering " + numberToTransferHigh + " from high to low.");

		Stream <Indiv> highToTransfer = population.highRiskGroupStream().limit(numberToTransferHigh);
				
		Stream.concat(lowToTransfer, highToTransfer).forEach(indiv -> indiv.changeRiskGroup());
		//System.out.println("New high risk count: " + population.highRiskCount());

		population.updateActivityGroups();
		
		
	}
	
	public void setPopulation(Population pop) {
		this.population = pop;
	}
	
}
