package msmOnlyModel;

public class MyAction {
	private Object agent;
	private double tick; 
	private Method methodToCall;
	
	public MyAction(double tick, Object agent, Method method) {
		this.tick = tick;
		this.agent = agent;
		this.methodToCall = method;
	}
	
}
