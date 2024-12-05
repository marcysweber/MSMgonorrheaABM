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
	
	CalibratedParameters(){}
	
	public List<Integer> getInitialInfectedValues() throws IOException {
		//reads the resampled Annual Contacts values from file, and gives them back as a list of doubles

		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/initial_infected_resample.txt"));
		List<Integer> dataAsInts = dataAsStrings.stream().map(s -> Integer.parseInt(s)).collect(Collectors.toList());
		return dataAsInts;
	}
	
	public List<Double> getPropHighRiskValues() throws IOException {
		//reads the resampled Annual Contacts values from file, and gives them back as a list of doubles

		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/propHighRisk_resample.txt"));
		//System.out.println(dataAsStrings);
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		//System.out.println(dataAsDoubles);
		return dataAsDoubles;
	}
	
	
	public List<Double> getTransmissionMSMValues() throws IOException {
		//reads the resampled Annual Contacts values from file, and gives them back as a list of doubles

		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/transmissionMSM_resample.txt"));
		//System.out.println(dataAsStrings);
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		//System.out.println(dataAsDoubles);
		return dataAsDoubles;
	}
	
	public List<Double> getTransmissionMSWValues() throws IOException {
		//reads the resampled Annual Contacts values from file, and gives them back as a list of doubles

		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/transmissionMSW_resample.txt"));
		//System.out.println(dataAsStrings);
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		//System.out.println(dataAsDoubles);
		return dataAsDoubles;
	}
	

	public List<Double> getTransmissionFValues() throws IOException {
		//reads the resampled Annual Contacts values from file, and gives them back as a list of doubles

		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/transmissionF_resample.txt"));
		//System.out.println(dataAsStrings);
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		//System.out.println(dataAsDoubles);
		return dataAsDoubles;
	}

	public List<Double> getRecoveryLambdaValues() throws IOException{
		//reads the resampled RecoveryLambda values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/recovery_lambda_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	public List<Double> getProbSymptomaticMSMValues() throws IOException{
		//reads the resampled ProbSymptomatic values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/prob_symptomatic_MSM_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}	
	
	public List<Double> getProbSymptomaticMSWValues() throws IOException{
		//reads the resampled ProbSymptomatic values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/prob_symptomatic_MSW_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	public List<Double> getProbSymptomaticFValues() throws IOException{
		//reads the resampled ProbSymptomatic values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/prob_symptomatic_F_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	public List<Double> getScreenIntervalMSMValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/screen_interval_MSM_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	public List<Double> getScreenIntervalMSWValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/screen_interval_MSW_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	public List<Double> getScreenIntervalWValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/screen_interval_W_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	
	
	public List<Double> getDelayToSeekCareMSMValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/delay_to_seek_care_MSM_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	public List<Double> getDelayToSeekCareMSWValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/delay_to_seek_care_MSW_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	public List<Double> getDelayToSeekCareFValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/delay_to_seek_care_F_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	public List<Double> getDelayToRetreatmentMSMValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/delay_to_retreatment_MSM_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	public List<Double> getDelayToRetreatmentMSWValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/delay_to_retreatment_MSW_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	public List<Double> getDelayToRetreatmentFValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/delay_to_retreatment_F_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	
	public List<Double> getRiskGroupTransferPropValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/risk_group_transfer_freq_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	public List<Double> getRiskGroupTransmissionRatioValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/risk_group_transmission_ratio_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	
	
	
	public List<Double> getPercentResistantAValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/percent_resistant_A_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	public List<Integer> getBeginImportingBValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/begin_importing_B_resample.txt"));		
		List<Integer> dataAsInts = dataAsStrings.stream().map(s -> Integer.parseInt(s)).collect(Collectors.toList());
		return dataAsInts;
		
	}
	
	public List<Double> getImportingBIntervalValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/importing_B_interval_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	
	
	public List<Double> getDSTsensitivityValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/DSTsensitivity_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	
	public List<Double> getDSTspecificityValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/DSTspecificity_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
		
	}
	
	
	
	
	
	public List<Double> getSeedValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/seed_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
	}
	
	public List<Double> getCareCostValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/care_cost_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
	}	
	
	public List<Double> getTestCostValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/test_cost_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
	}	
	
	public List<Double> getStrainTestCostValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/strain_test_cost_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
	}	
	public List<Double> getTreatmentACostValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/drug_a_treatment_cost_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
	}	
	public List<Double> getTreatmentBCostValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/drug_b_treatment_cost_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
	}
	
	public List<Double> getTreatmentXCostValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/drug_X_treatment_cost_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
	}	
	public List<Double> getTreatmentECostValues() throws IOException{
		//reads the resampled ScreenInterval values from file, and gives them back as a list of doubles
		
		List<String> dataAsStrings = new ArrayList<String>();
		dataAsStrings = Files.readAllLines(Paths.get("/Users/me597/Documents/MSM_calibrated_params/drug_E_treatment_cost_resample.txt"));		
		List<Double> dataAsDoubles = dataAsStrings.stream().map(s -> Double.parseDouble(s)).collect(Collectors.toList());
		return dataAsDoubles;
	}
	
	
	
	
}
