package msmOnlyModel;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class MySchedule <tick, value> {

	private double currentTick;
	private List<MyAction> actionList;
	
	public MySchedule() {
		this.actionList = new ArrayList<MyAction>();
		this.currentTick = -1.0;
	}
	
	public void execute(){
		sortActions();
		
	}
	
	public void remove() {
		
	}
	
	public void sortActions() {
		this.actionList = this.actionList.sort(null);
	}
	
}
