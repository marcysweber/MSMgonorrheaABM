package msmOnlyModel;

import java.util.stream.Stream;

import repast.simphony.engine.schedule.ISchedule;
import repast.simphony.parameter.Parameters;

public class ChangeRiskGroups {
	private Parameters parameters;
	private ThreadSafeRandomHelper randomHelper;
	private ISchedule schedule;
	private Population population;
	private double prop;
	
	public ChangeRiskGroups(Parameters parameters, ThreadSafeRandomHelper randomHelper, ISchedule schedule, Population population) {
		this.parameters = parameters;
		this.randomHelper = randomHelper;
		this.schedule = schedule;
		this.population = population;
		this.prop = parameters.getDouble("risk_group_transfer_prop");
	}
	
	public void changeRiskGroups() {
		//happens once per year
		//parameter risk_group_transfer_prop determines what fraction of both risk groups swaps
		
		//process low risk to high risk
		
		long numberToTransferLow = Math.round(population.lowRiskCount() * prop);
		System.out.println("Transfering " + numberToTransferLow + " from low to high.");
		
		Stream <Indiv> lowToTransfer = population.lowRiskGroupStream().limit(numberToTransferLow);

		
		//process high risk to low risk

		long numberToTransferHigh = Math.round(population.highRiskCount() * prop);
		System.out.println("Transfering " + numberToTransferHigh + " from high to low.");

		Stream <Indiv> highToTransfer = population.highRiskGroupStream().limit(numberToTransferHigh);
				
		Stream.concat(lowToTransfer, highToTransfer).forEach(indiv -> indiv.changeRiskGroup());
		System.out.println("New high risk count: " + population.highRiskCount());

	}
	
}
