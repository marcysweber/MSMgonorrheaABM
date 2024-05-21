/**
 * 
 */
package msmOnlyModel;

/**
 * @author me597
 *
 */
import java.io.File;

import repast.simphony.batch.BatchScenarioLoader;
import repast.simphony.engine.controller.Controller;
import repast.simphony.engine.controller.DefaultController;
import repast.simphony.engine.environment.AbstractRunner;
import repast.simphony.engine.environment.ControllerRegistry;
import repast.simphony.engine.environment.DefaultRunEnvironmentBuilder;
import repast.simphony.engine.environment.RunEnvironment;
import repast.simphony.engine.environment.RunEnvironmentBuilder;
import repast.simphony.engine.environment.RunState;
import repast.simphony.engine.schedule.ISchedule;
import repast.simphony.engine.schedule.Schedule;
import repast.simphony.parameter.Parameters;
import repast.simphony.parameter.SweeperProducer;
import simphony.util.messages.MessageCenter;

public class MyRunner extends AbstractRunner {

	private static MessageCenter msgCenter = MessageCenter.getMessageCenter(MyRunner.class);

	private RunEnvironmentBuilder runEnvironmentBuilder;
	protected Controller controller;
	protected boolean pause = false;
	protected Object monitor = new Object();
	protected SweeperProducer producer;
	private ISchedule schedule;
	private double endTime;
	private boolean finishing;

	public MyRunner() {
		runEnvironmentBuilder = new DefaultRunEnvironmentBuilder(this, true);
		controller = new DefaultController(runEnvironmentBuilder);
		controller.setScheduleRunner(this);
		this.endTime = 1300.0;
		this.finishing = false;
		//this.schedule = new Schedule();
	}

	public void load(File scenarioDir) throws Exception{
		if (scenarioDir.exists()) {
			//System.out.print("loading...");
			
			BatchScenarioLoader loader = new BatchScenarioLoader(scenarioDir);
			ControllerRegistry registry = loader.load(runEnvironmentBuilder);
			controller.setControllerRegistry(registry);
		} else {
			msgCenter.error("Scenario not found", new IllegalArgumentException(
					"Invalid scenario " + scenarioDir.getAbsolutePath()));
			return;
		}

		controller.batchInitialize();
		controller.runParameterSetters(null);
	}

	public void runInitialize(Parameters params, double endTime){
		//System.out.print("initializing...");
		this.endTime = endTime;
		controller.runInitialize(params);
		schedule = RunState.getInstance().getScheduleRegistry().getModelSchedule();
	}

	public void cleanUpRun(){
		controller.runCleanup();
	}
	public void cleanUpBatch(){
		controller.batchCleanup();
	}

	// returns the tick count of the next scheduled item
	public double getNextScheduledTime(){
		return ((Schedule)RunEnvironment.getInstance().getCurrentSchedule()).peekNextAction().getNextTime();
	}

	// returns the number of model actions on the schedule
	public int getModelActionCount(){
		return schedule.getModelActionCount();
	}

	// returns the number of non-model actions on the schedule
	public int getActionCount(){
		return schedule.getActionCount();
	}

	// Step the schedule
	public void step(){
		//System.out.print("stepping... ");
		//System.out.println(schedule.getTickCount());
		schedule.execute();
		this.finishing = schedule.isFinishing();
	}

	// stop the schedule
	public void stop(){
		//System.out.print("stopping...");
		schedule.setFinishing(true);
		if ( schedule != null )
			schedule.executeEndActions();
		//RunEnvironment.getInstance().endRun();
	}

	public void setFinishing(boolean fin){
		schedule.setFinishing(fin);
	}
	
	public boolean isFinishing() {
		return this.finishing;
	}

	public void execute(RunState toExecuteOn) {
		// required AbstractRunner stub.  We will control the
		//  schedule directly.
	}
	
	public double getEndTime() {
		return this.endTime;
	}
	
	public double getCurrentTick() {
		return schedule.getTickCount();
	}
}
