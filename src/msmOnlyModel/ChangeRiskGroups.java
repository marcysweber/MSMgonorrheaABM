package msmOnlyModel;

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
		
		long numberToTransfer = Math.round(population.lowRiskCount() * prop);
		population.lowRiskGroupStream().limit(numberToTransfer).forEach(indiv -> indiv.changeRiskGroup());
		
		//process high risk to low risk

		numberToTransfer = Math.round(population.highRiskCount() * prop);
		population.highRiskGroupStream().limit(numberToTransfer).forEach(indiv -> indiv.changeRiskGroup());
		
	}
	
}
