/**
 * 
 */
package msmOnlyModel;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author me597
 *
 */
public class CalibratedParameters {
	
	private String path;
	
	
	
	CalibratedParameters(){
		path = "/usr/local/MSM_calibrated_params/";
		//path = "/Users/me597/Documents/MSM_calibrated_params/";
	}
	
	public List<Integer> getInitialInfectedValues() throws IOException {
		//reads the resampled Annual Contacts values from file, and gives them back as a list of doubles

		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path +"initial_infected_resample.txt"));
		List<Integer> dataAsInts = dataAsStrings.stream().map(s -> Integer.parseInt(s)).collect(Collectors.toList());
		return dataAsInts;
	}
	
	public List<Double> getPropActivityRiskValues() throws IOException {
		//reads the resampled Annual Contacts values from file, and gives them back as a list of doubles

		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path +"propHighActivity_resample.txt"));
		//System.out.println(dataAsStrings);
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		//System.out.println(dataAsDoubles);
		return dataAsDoubles;
	}
	
	
	public List<Double> getTransmissionMSMValues() throws IOException {
		//reads the resampled Annual Contacts values from file, and gives them back as a list of doubles

		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path+ "transmissionMSM_resample.txt"));
		//System.out.println(dataAsStrings);
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		//System.out.println(dataAsDoubles);
		return dataAsDoubles;
	}
	
	public List<Double> getTransmissionMSWValues() throws IOException {
		//reads the resampled Annual Contacts values from file, and gives them back as a list of doubles

		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "transmissionMSW_resample.txt"));
		//System.out.println(dataAsStrings);
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		//System.out.println(dataAsDoubles);
		return dataAsDoubles;
	}
	

	public List<Double> getTransmissionFValues() throws IOException {
		//reads the resampled Annual Contacts values from file, and gives them back as a list of doubles

		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "transmissionF_resample.txt"));
		//System.out.println(dataAsStrings);
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		//System.out.println(dataAsDoubles);
		return dataAsDoubles;
	}

	public List<Double> getRecoveryTimeValues() throws IOException{
		//reads the resampled RecoveryLambda values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "recovery_lambda_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	public List<Double> getProbSymptomaticMSMValues() throws IOException{
		//reads the resampled ProbSymptomatic values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "prob_symptomatic_MSM_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}	
	
	public List<Double> getProbSymptomaticMSWValues() throws IOException{
		//reads the resampled ProbSymptomatic values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "prob_symptomatic_MSW_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	public List<Double> getProbSymptomaticFValues() throws IOException{
		//reads the resampled ProbSymptomatic values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "prob_symptomatic_F_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	public List<Double> getScreenIntervalMSMValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "screen_interval_MSM_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	public List<Double> getScreenIntervalMSWValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "screen_interval_MSW_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	public List<Double> getScreenIntervalWValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "screen_interval_W_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	
	
	public List<Double> getDelayToSeekCareMSMValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "delay_to_seek_care_MSM_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	public List<Double> getDelayToSeekCareMSWValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "delay_to_seek_care_MSW_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	public List<Double> getDelayToSeekCareFValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "delay_to_seek_care_F_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	public List<Double> getDelayToRetreatmentMSMValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "delay_to_retreatment_MSM_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	public List<Double> getDelayToRetreatmentMSWValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "delay_to_retreatment_MSW_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	public List<Double> getDelayToRetreatmentFValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "delay_to_retreatment_F_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	public List<Double> getAssortativityValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "assortativity_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	
	
	public List<Double> getActivityGroupTransferPropValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "risk_group_transfer_prop_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	public List<Double> getActivityGroupTransmissionRatioValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "risk_group_transmission_ratio_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	
	
	
	public List<Double> getPercentResistantAValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "percent_resistant_A_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	public List<Integer> getBeginImportingBValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "begin_importing_B_resample.txt"));		
		List<Integer> dataAsInts = dataAsStrings.stream().map(s -> Integer.parseInt(s)).collect(Collectors.toList());
		return dataAsInts;
		
	}
	
	public List<Double> getImportingBIntervalValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "importing_B_interval_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	
	
	public List<Double> getDSTsensitivityValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "DSTsensitivity_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	
	public List<Double> getDSTspecificityValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "DSTspecificity_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	
	
	
	
	public List<Double> getSeedValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "seed_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
	}
	
	public List<Double> getCareCostValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "care_cost_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
	}	
	
	public List<Double> getTestCostValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "test_cost_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
	}	
	
	public List<Double> getStrainTestCostValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "strain_test_cost_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
	}	
	public List<Double> getTreatmentACostValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "drug_a_treatment_cost_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
	}	
	public List<Double> getTreatmentBCostValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "drug_b_treatment_cost_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
	}
	
	public List<Double> getTreatmentXCostValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "drug_X_treatment_cost_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
	}	
	public List<Double> getTreatmentECostValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get(path + "drug_E_treatment_cost_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
	}
	
	
	
	
}
