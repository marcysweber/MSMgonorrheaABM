


library(ggplot2)
library(tidyverse)
library(data.table)
library(RColorBrewer)
library(egg)
library(epiR)
library(patchwork)


#copied from cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#actual analysis functions:
############

identify = function(df, resampledf){
  df$uniqueID <- paste(as.character(df$yearX), as.character(df$RunNumber), as.character(df$seed), sep='')
  
  
  
  dfrowcount <- matrix(nrow = 1, ncol = 2)
  for (i in unique(df$seed)){
    newrow <- c(i,count(df[df$seed == i,]))
    dfrowcount <- rbind(dfrowcount, newrow)
    
    
  }
  
  dfrowcount[,2] <- as.integer(dfrowcount[,2])
  duplicates <- dfrowcount[dfrowcount[,2]>31,1][-1]
  duplicate.seeds <- unlist(duplicates, use.names=FALSE)
  
  df <- df[!df$seed %in% duplicate.seeds,]

  
  df$resampled <- rep(0, length(df[,1]))
  for (i in df$seed){
    df$resampled[df$seed==i] <- sum(resampledf$seed==i)
  }
  
  return(df)
}


#likelihood functions with the real observed values for three targets
prev_binom_likelihood = function(x){
  return(dbinom(93, 2075, x / 100, log=TRUE)) # values from https://pubmed.ncbi.nlm.nih.gov/30973847/
}

MSM_prev_binom_likelihood = function(x){
  return(dbinom(93, 2075, x / 100, log=TRUE)) # values from https://pubmed.ncbi.nlm.nih.gov/30973847/
}

inc_norm_likelihood = function(x){
  return(dnorm(6508, x, x/5, log=TRUE)) # values from 2018 CDC report
}

M_inc_norm_likelihood = function(x){
  return(dnorm(212.8/100000, x, x/5, log=TRUE)) # values from 2018 CDC report
}

F_inc_norm_likelihood = function(x){
  return(dnorm(145.8/100000, x, x/5, log=TRUE)) # values from 2018 CDC report
}

MSM_inc_norm_likelihood = function(x){
  return(dnorm(6508/100000, x, x/5, log=TRUE)) # values from 2018 CDC report
}

sympt_binom_likelihood = function(x){
  return(dbinom(233, 343, x, log=TRUE))
}

MSM_sympt_binom_likelihood = function(x){
  return(dbinom(233, 343, x, log=TRUE))
}
prop.test(233, 343)


MSW_sympt_binom_likelihood = function(x){
  return(dbinom(474, 599, x, log=TRUE))
}
prop.test(474, 599)


W_sympt_binom_likelihood = function(x){
  return(dbinom(644, 1102, x, log=TRUE))
}
prop.test(644, 1102)



  

param_hist = function(df){
  a <- ggplot(df) + 
    geom_histogram(aes(x = AnnualContacts), color = "black", fill = "darkgrey", binwidth = 0.5) +
    xlim(0,9) + 
    theme_bw()
  return(a)
}


#calculates the likelihoods and returns the df with weights for the ends
calc_weights = function(df){
  #target_data <- df %>% filter(tick %% 52 == 0)
  target_data <- df %>% filter(tick > 261) #years 6, 7, 8, 9, 10
  
  data_ends_loc <- target_data %>% filter(tick == 10 * 52)
  
  #target_data = cbind(likelihood_prev = 0, likelihood_detect = 0, likelihood_sympt = 0, combined_log_likelihood = 0, target_data)
  
  #calc likelihood for each target for each year for each trajectory
  target_data$likelihood_prev = prev_binom_likelihood(target_data$Prevalence)
  target_data$likelihood_detect = inc_norm_likelihood(target_data$Detected)
  target_data$likelihood_sympt = sympt_binom_likelihood(target_data$DetectedAndSymptoms/target_data$Detected)
  
  #combine targets for a year for a trajectory
  target_data$combined_log_likelihood <- target_data$likelihood_prev + target_data$likelihood_detect + target_data$likelihood_sympt
  
  #sum across trajectory
  for (i in 1:nrow(data_ends_loc)){
    thisrun <- data_ends_loc$uniqueID[i]
    data_ends_loc$combined_log_likelihood[i] <- sum(target_data$combined_log_likelihood[target_data$uniqueID == thisrun]) #sum likelihoods across this trajectory
  }
  
  #subtract from max
  data_ends_loc$stable_combined_log_likelihood <- data_ends_loc$combined_log_likelihood - max(data_ends_loc$combined_log_likelihood)
  
  data_ends_loc$weight = exp(data_ends_loc$stable_combined_log_likelihood)
  
  sum_likelihood = sum(data_ends_loc$weight)
  
  

  
  return(data_ends_loc)
}

calc_weights_subpops = function(df){
  #target_data <- df %>% filter(tick %% 52 == 0)
  target_data <- df %>% filter(tick > 261) #years 6, 7, 8, 9, 10
  
  target_data$detectedIncM <- target_data$detectedIncMSM + target_data$detectedIncMSW + target_data$detectedIncMSMW
  target_data$MPopSize <- target_data$MSMPopSize + target_data$MSMWPopSize + target_data$MSWPopSize
  
  target_data = cbind(likelihood_prev_MSM = 0, likelihood_detect_M = 0, likelihood_detect_F = 0, likelihood_detect_MSM = 0, likelihood_sympt_MSW = 0, likelihood_sympt_MSM = 0, likelihood_sympt_W = 0, combined_log_likelihood = 0, target_data)
  
  #calc likelihood for each target for each year for each trajectory
  target_data$likelihood_prev_MSM = MSM_prev_binom_likelihood(target_data$prevMSM)
  
  target_data$likelihood_detect_M = M_inc_norm_likelihood(target_data$detectedIncM / target_data$MPopSize)
  target_data$likelihood_detect_F = F_inc_norm_likelihood(target_data$detectedIncW / target_data$WPopSize)
  target_data$likelihood_detect_MSM = MSM_inc_norm_likelihood(target_data$detectedIncMSM / target_data$MSMPopSize)
  
  target_data$likelihood_sympt_MSM = MSM_sympt_binom_likelihood(target_data$detectedAndSymptomsMSM / target_data$detectedIncMSM)
  target_data$likelihood_sympt_MSW = MSW_sympt_binom_likelihood(target_data$detectedAndSymptomsMSW / target_data$detectedIncMSW)
  target_data$likelihood_sympt_W = W_sympt_binom_likelihood(target_data$detectedAndSymptomsW / target_data$detectedIncW)
  
  
  
  #combine targets for a year for a trajectory
  target_data$combined_log_likelihood <- target_data$likelihood_prev_MSM + 
    target_data$likelihood_detect_M + 
    target_data$likelihood_detect_F + 
    target_data$likelihood_detect_MSM + 
    target_data$likelihood_sympt_MSM +
    target_data$likelihood_sympt_MSW +
    target_data$likelihood_sympt_W
    
  
  target_data$stable_combined_log_likelihood <- target_data$combined_log_likelihood - max(target_data$combined_log_likelihood)
  
  sum_likelihood = sum(exp(target_data$stable_combined_log_likelihood))
  
  data_ends_loc <- target_data %>% filter(tick == 10 * 52)
  
  # to use proper weights for resampling
  for (i in 1:nrow(data_ends_loc)){
    thisrun <- data_ends_loc$RunNumber[i]
    data_ends_loc$weight[i] <- exp(sum(target_data$combined_log_likelihood[target_data$RunNumber == thisrun])) #sum likelihoods across this trajectory
  }
  
  return(data_ends_loc)
}

#takes the df with weights and returns the best n values
best_ends = function(df, n){
  df <- df[order(df$combined_log_likelihood, decreasing = TRUE),]
  best_of_sweep <- df[1:n,]
  return(best_of_sweep)
}

#takes data ends and matches to full trajectories (to visualize best n trajectories)
best_traj = function(full_df, best_df){
  new_df <- full_df %>% filter(uniqueID %in% best_df$uniqueID)
  return(new_df)
}

#takes the df with weights and resamples with replacement
resample = function(df, n){
  combinedResample<-df[sample(nrow(df), size = n, prob = (df$weight), replace = TRUE),]
  return(combinedResample)
}

#calculates error in resistance estimate
calc_error = function(df){
  new_df <- df
  if ("GISP" %in% df$counterfactual){
    #estimate is SurveillanceEstPropResist
    new_df$ResistEstError <- new_df$TruePropResist - new_df$SurveillanceEstPropResist
  }
  
  if ("test-of-cure" %in% df$counterfactual){
    #estimate is KnownFailedTreatments/Treatments
    new_df$ResistEstError <- new_df$TruePropResist - (new_df$KnownFailedTreatments/new_df$Treatments)
  }
  return(new_df)
  
}






############

#summary statistics and summary plot
###########
 
calc_failure = function(df){
  failures <- c()
  
  failures <- (df$AttemptTreatmentsA - df$SuccessTreatmentsA) + (df$AttemptTreatmentsB - df$SuccessTreatmentsB) + (df$AttemptTreatmentsX - df$SuccessTreatmentsX)
  
  
  return(failures)
}

calc_failure_rate = function(df){
  failures <- calc_failure(df)
  
  attempts <- df$AttemptTreatmentsA + df$AttemptTreatmentsB + df$AttemptTreatmentsX + df$UsageofErtapenem
  
  rate <- failures/attempts
  
  return(rate)
  
}

get_ends = function(df){
  ends <- data.frame()
  
  runs <- unique(df$uniqueID)
  
  for (i in runs){
    thisRunData <- df %>% filter(uniqueID == i)
    
    thisRunLastRow <- thisRunData %>% filter(tick == max(tick))
    ends <- rbind(ends, thisRunLastRow)
  }
  
  
  return(ends)
}

calc_summary_stats = function(df){
  summarydf <- data.frame(matrix(ncol = 21, nrow = 1))
  colnames(summarydf) <- c("Counterfactual", 
                           "MeanPrev", "SDPrev", 
                           "MeanInc", "SDInc",                          
                           "MeanTotalResistProp", "SDTotalResistProp",
                           "MeanTotalResist", "SDTotalResist",
                           "MeanResistA", "SDResistA", 
                           "MeanResistB", "SDResistB", 
                           "MeanResistBoth", "SDResistBoth", 
                           "MeanCost", "SDCost", 
                           "MeanQALYsLost", "SDQALYsLost",
                           "cumulativecostsmean", "cumulativecostSD")
  
  #filter each df so that only the tip of each trajectory is used
  
  dfends <- get_ends(df)
  summarydf$Counterfactual <- dfends$counterfactual[1]
  
  #mean prevalence
  summarydf$MeanPrev <- mean(dfends$Prevalence)
  summarydf$SDPrev <- sd(dfends$Prevalence)
  
  #mean incidence
  summarydf$MeanInc <- mean(dfends$Detected)
  summarydf$SDInc <- sd(dfends$Detected)
  
  totalResist <- dfends$ResistAPrevalence + dfends$ResistBPrevalence + dfends$ResistBothPrevalence
  summarydf$MeanTotalResist <- mean(totalResist) 
  summarydf$SDTotalResist <- var(totalResist) 
  
  totalResistProp <- (dfends$ResistAPrevalence + dfends$ResistBPrevalence + dfends$ResistBothPrevalence) / dfends$Prevalence
  summarydf$MeanTotalResistProp <- mean(totalResistProp) * 100
  summarydf$SDTotalResistProp <- var(totalResistProp) * 100
  
  
  AResistProp <- dfends$ResistAPrevalence / dfends$Prevalence
  summarydf$MeanResistA <- mean(AResistProp) * 100
  summarydf$SDResistA <- var(AResistProp) *100
  
  BResistProp <- dfends$ResistBPrevalence / dfends$Prevalence
  summarydf$MeanResistB <- mean(BResistProp) * 100
  summarydf$SDResistB <- var(BResistProp) * 100
  
  summarydf$MeanResistBoth <- mean(dfends$ResistBothPrevalence)
  summarydf$SDResistBoth <- var(dfends$ResistBothPrevalence)
  
  summarydf$MeanCost <- mean(dfends$AnnualMonetaryCost)
  summarydf$SDCost <- var(dfends$AnnualMonetaryCost)
  
  summarydf$MeanQALYsLost <- mean(dfends$AnnualQALYsLost)
  summarydf$SDQALYsLost <- var(dfends$AnnualQALYsLost)  
  
  summarydf$cumulativecostsmean <- mean(cumulative_costs(df))
  summarydf$cumulativecostSD <- var(cumulative_costs(df))
  
  return(summarydf)
}

discountRate = 0.03

discountedQALYs = function(df){
  discountedValues <- c()
  i <- df$tick/52
  discountedValues <- c(discountedValues, df$AnnualQALYsLost/((1+discountRate)^i))
  return(discountedValues)
}


discountedQALYsMSM = function(df){
  discountedValues <- df$QALYcostMSM/((1+discountRate)^(df$tick/52))
  return(discountedValues)
}

discountedQALYsMSW = function(df){
  discountedValues <- c()
  i <- df$tick/52
  discountedValues <- c(discountedValues, df$QALYCostMSW/((1+discountRate)^i))
  return(discountedValues)
}

discountedQALYsW = function(df){
  discountedValues <- c()
  i <- df$tick/52
  discountedValues <- c(discountedValues, df$QALYCostW/((1+discountRate)^i))
  return(discountedValues)
}



discountedcost = function(df){
  discountedValues <- c()
  i <- df$tick/52
  discountedValues <- c(discountedValues, df$AnnualMonetaryCost/((1+discountRate)^i))
  return(discountedValues)
}

discountedcostMSM = function(df){
  discountedValues <- c()
  i <- df$tick/52
  discountedValues <- c(discountedValues, df$monetaryCostMSM/((1+discountRate)^i))
  return(discountedValues)
}

discountedcostMSW = function(df){
  discountedValues <- c()
  i <- df$tick/52
  discountedValues <- c(discountedValues, df$monetaryCostMSW/((1+discountRate)^i))
  return(discountedValues)
}

discountedcostW = function(df){
  discountedValues <- c()
  i <- df$tick/52
  discountedValues <- c(discountedValues, df$monetaryCostW/((1+discountRate)^i))
  return(discountedValues)
}



cumulative_QALYs = function(df){
  cumulative <- c()
  runs <- unique(df$uniqueID)
  
  for (i in runs){
    thisRunData <- df %>% filter(uniqueID == i)
    thisRunCumulative <- sum(discountedQALYs(thisRunData))
    cumulative <- c(cumulative, thisRunCumulative)
  }
  
  return(cumulative)
}

cumulative_QALYs_MSM = function(df){
  cumulative <- c()
  runs <- unique(df$seed)
  
  for (i in runs){
    thisRunData <- df %>% filter(seed == i)
    thisRunCumulative <- sum(discountedQALYsMSM(thisRunData))
    cumulative <- c(cumulative, thisRunCumulative)
  }
  
  return(cumulative)
}


cumulative_QALYs_MSW = function(df){
  cumulative <- c()
  runs <- unique(df$seed)
  
  for (i in runs){
    thisRunData <- df %>% filter(seed == i)
    thisRunCumulative <- sum(discountedQALYsMSW(thisRunData))
    cumulative <- c(cumulative, thisRunCumulative)
  }
  
  return(cumulative)
}


cumulative_QALYs_W = function(df){
  cumulative <- c()
  runs <- unique(df$seed)
  
  for (i in runs){
    thisRunData <- df %>% filter(seed == i)
    thisRunCumulative <- sum(discountedQALYsW(thisRunData))
    cumulative <- c(cumulative, thisRunCumulative)
  }
  
  return(cumulative)
}




cumulative_costs = function(df){
  
  cumulative <- c()
  runs <- unique(df$uniqueID)
  
  for (i in runs){
    thisRunData <- df %>% filter(uniqueID == i)
    thisRunCumulative <- sum(discountedcost(thisRunData))
    cumulative <- c(cumulative, thisRunCumulative)
  }
  
  return(cumulative)
  
}

cumulative_costs_MSM = function(df){
  
  cumulative <- c()
  runs <- unique(df$seed)
  
  for (i in runs){
    thisRunData <- df %>% filter(seed == i)
    thisRunCumulative <- sum(discountedcostMSM(thisRunData))
    cumulative <- c(cumulative, thisRunCumulative)
  }
  
  return(cumulative)
  
}

cumulative_costs_MSW = function(df){
  
  cumulative <- c()
  runs <- unique(df$seed)
  
  for (i in runs){
    thisRunData <- df %>% filter(seed == i)
    thisRunCumulative <- sum(discountedcostMSW(thisRunData))
    cumulative <- c(cumulative, thisRunCumulative)
  }
  
  return(cumulative)
  
}

cumulative_costs_W = function(df){
  
  cumulative <- c()
  runs <- unique(df$seed)
  
  for (i in runs){
    thisRunData <- df %>% filter(seed == i)
    thisRunCumulative <- sum(discountedcostW(thisRunData))
    cumulative <- c(cumulative, thisRunCumulative)
  }
  
  return(cumulative)
  
}

discountedX = function(df){
  discountedValues <- c()
  i <- df$tick/52
  discountedValues <- c(discountedValues, df$AttemptTreatmentsX/((1+discountRate)^i))
  return(discountedValues)
}

cumulative_X = function(df){
  
  cumulative <- c()
  runs <- unique(df$uniqueID)
  
  for (i in runs){
    thisRunData <- df %>% filter(uniqueID == i)
    thisRunCumulative <- sum(thisRunData$AttemptTreatmentsX)
    cumulative <- c(cumulative, thisRunCumulative)
  }
  
  return(cumulative)
  
}

cumulative_E = function(df){
  
  cumulative <- c()
  runs <- unique(df$uniqueID)
  
  for (i in runs){
    thisRunData <- df %>% filter(uniqueID==i)
    thisRunCumulative <- sum(thisRunData$UsageofErtapenem)
    cumulative <- c(cumulative, thisRunCumulative)
  }
  
  return(cumulative)
  
}

discountedInc = function(df){
  discountedValues <- c()
  i <- df$tick/52
  discountedValues <- c(discountedValues, df$Incidence/((1+discountRate)^i))
  return(discountedValues)
}


discountedResist = function(df){
  discountedValues <- c()
  i <- df$tick/52
  discountedValues <- c(discountedValues, (df$ResistAIncidence)/((1+discountRate)^i))
  return(discountedValues)
}

cumulative_inc = function(df){
  
  cumulative <- c()
  runs <- unique(df$uniqueID)
  
  for (i in runs){
    thisRunData <- df %>% filter(uniqueID == i)
    thisRunCumulative <- sum(thisRunData$Incidence)
    cumulative <- c(cumulative, thisRunCumulative)
  }
  
  return(cumulative)
  
}


cumulative_resist = function(df){
  
  cumulative <- c()
  runs <- unique(df$uniqueID)
  
  for (i in runs){
    thisRunData <- df %>% filter(uniqueID == i)
    thisRunCumulative <- sum(thisRunData$ResistAIncidence)
    cumulative <- c(cumulative, thisRunCumulative)
  }
  
  return(cumulative)
  
}

discounted_fail_A = function(df){
 # i <- df$tick/52
  fail <- df$AttemptTreatmentsA - df$SuccessTreatmentsA
  discountedValues <- fail#/((1+discountRate)^i)
  return(discountedValues)
}

discounted_fail_B = function(df){
  #i <- df$tick/52
  fail <- df$AttemptTreatmentsB - df$SuccessTreatmentsB
  discountedValues <- fail#/((1+discountRate)^i))
  return(discountedValues)
}

discounted_fail_X = function(df){
 # i <- df$tick/52
  fail <- df$AttemptTreatmentsX - df$SuccessTreatmentsX
  discountedValues <- fail#/((1+discountRate)^i))
  return(discountedValues)
}

discounted_attempts_A = function(df){
  #i <- df$tick/52
  discountedValues <- df$AttemptTreatmentsA#/((1+discountRate)^i))
  return(discountedValues)
}

discounted_attempts_B = function(df){
#  i <- df$tick/52
  discountedValues <- df$AttemptTreatmentsB#/((1+discountRate)^i))
  return(discountedValues)
}

discounted_attempts_X = function(df){
#  i <- df$tick/52
  discountedValues <- df$AttemptTreatmentsX#/((1+discountRate)^i))
  return(discountedValues)
}


discounted_E = function(df){
#  i <- df$tick/52
  discountedValues <- df$UsageofErtapenem#/((1+discountRate)^i))
  return(discountedValues)
}

sum_failures = function(df){
  failures_A <- sum(discounted_fail_A(df))
  
  failures_B <- sum(discounted_fail_B(df))
  
  failures_X <- sum(discounted_fail_X(df))

  return(failures_A + failures_B + failures_X)
}

sum_attempts = function(df){
  attempts_A = sum(df$AttemptTreatmentsA) 
  attempts_B = sum(df$AttemptTreatmentsB)
  attempts_X = sum(df$AttemptTreatmentsX)
  return(attempts_A + attempts_B + attempts_X)
}

cumulative_failure = function(df){
  
  cumulative <- c()
  runs <- unique(df$uniqueID)
  
  for (i in runs){
    thisRunData <- df %>% filter(uniqueID == i)
    
    total_fails <- sum_failures(thisRunData)
    total_attempts <- sum_attempts(thisRunData)
    
    thisRunCumulative <- total_fails/total_attempts
    cumulative <- c(cumulative, thisRunCumulative)
  }
  
  return(cumulative)
  
}

cumulative_everything = function(df){
  #get ends so that can save results from each separate trajectory
  dfends <- get_ends(df)
  
  #exclude burn-in period and calibration period
  df <- df %>% filter(tick > 521) 
  
  dfends$cumulativeCosts <- cumulative_costs(df)
  dfends$cumulativeQALYs <- cumulative_QALYs(df)
  dfends$cumulativeX <- cumulative_X(df)
  dfends$cumulativeInc <- cumulative_inc(df)
  dfends$cumulativeResist <- cumulative_resist(df)
  dfends$cumulativeFailure <- cumulative_failure(df)
  dfends$cumulativeE <- cumulative_E(df)
  
  #weight with resampled
  newdf <- dfends
  for (traj in row.names(dfends)){
    #how many times does traj appear in resampled df?
    dup <- dfends[traj,]$resampled-1
    
    #add that many duplicate rows to ceadf
    if (dup > 0){
      newdf <- rbind(newdf, dfends[rep(traj,dup),])
    }
  }
  
  return(newdf)
}

new_summary_plot = function(df1, df2, df3, df4, df5){ 
  df1ends <- cumulative_everything(df1)
  df2ends <- cumulative_everything(df2)
  df3ends <- cumulative_everything(df3)
  df4ends <- cumulative_everything(df4)
  df5ends <- cumulative_everything(df5)
  
  
  allends <- rbind(df1ends,
                   df2ends,
                   df3ends, 
                   df4ends,
                   df5ends)
  
  
  counter_levels <- c("realistic_combo_33_33_34", "drug_sus_testing_80","test-of-cure_80", "random","GISP")
  counter_labels <- c("RC", "DST", "TOC", "RT", "GISP")
  
  inc <- ggplot(data = allends, aes(x=cumulativeInc/1000, y = factor(counterfactual, levels = counter_levels))) +
   # geom_point() +
    stat_summary(fun = mean, fun.min = function(a) {quantile(a, 0.025)}, fun.max = function(a){quantile(a, 0.975)})+
    labs(
      title = "A.",
      y = "",
      x="Cumulative incidence over 20 years\nper 100,000 (thousands)"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels)+
    coord_cartesian(xlim=c(0, 1500))
  
  failure <- ggplot(data = allends, aes(x=cumulativeFailure * 100, y = factor(counterfactual, levels = counter_levels))) +
    stat_summary(fun = mean, fun.min = function(a) {quantile(a, 0.025)}, fun.max = function(a){quantile(a, 0.975)})+
    labs(
      title = "B.",
      y = "",
      x="% failures per treatment attempt\ncumulative over 20 years"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels) +
    coord_cartesian(xlim=c(0, 100))
  
  x <- ggplot(data = allends, aes(x=cumulativeE, y = factor(counterfactual, levels = counter_levels))) +
    stat_summary(fun = mean, fun.min = function(a) {quantile(a, 0.025)}, fun.max = function(a){quantile(a, 0.975)})+
    labs(
      title = "C.",
      y = "",
      x="Cumulative treatments with ertapenem\nper 100,000 over 20 years"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels)+
    coord_cartesian(xlim=c(0, 40000))
  
  
  summary <- ggarrange(inc +
                         theme(axis.text.y = element_text(size = 8),axis.text.x = element_text(size = 8),axis.title.x = element_text(size = 8)), failure + 
                         theme(axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.title.y = element_blank(),
                               axis.text.x = element_text(size = 8),
                               axis.title.x = element_text(size = 8)), x + 
                         theme(axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.title.y = element_blank() ,
                               axis.text.x = element_text(size = 8),
                               axis.title.x = element_text(size = 8)), nrow = 1)
  return(summary)
   
  
  }

summary_plot = function(df1, df2, df3, df4, df5){
  df1ends <- cumulative_everything(df1)
  df2ends <- cumulative_everything(df2)
  df3ends <- cumulative_everything(df3)
  df4ends <- cumulative_everything(df4)
  df5ends <- cumulative_everything(df5)
  
  
  allends <- rbind(df1ends,
                   df2ends,
                   df3ends, 
                   df4ends,
                   df5ends)
  

  counter_levels <- c("realistic_combo_33_33_34", "drug_sus_testing_80","test-of-cure_80", "random","GISP")
  counter_labels <- c("RC", "DST", "TOC", "RT", "GISP")
  
  inc <- ggplot(data = allends, aes(x=cumulativeInc/1000, y = factor(counterfactual, levels = counter_levels))) +
    geom_bar(outlier.shape = NA) +
    labs(
      title = "A.",
      y = "",
      x="Cumulative incidence over 20 years\nper 100,000 (thousands)"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels)+
    coord_cartesian(xlim=c(0, 1500))
  
  failure <- ggplot(data = allends, aes(x=cumulativeFailure * 100, y = factor(counterfactual, levels = counter_levels))) +
    geom_boxplot(outlier.shape = NA) +
    labs(
      title = "B.",
      y = "",
      x="% failures per treatment attempt\ncumulative over 20 years"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels) +
    coord_cartesian(xlim=c(0, 100))
  
  x <- ggplot(data = allends, aes(x=cumulativeE, y = factor(counterfactual, levels = counter_levels))) +
    geom_boxplot(outlier.shape = NA) +
    labs(
      title = "C.",
      y = "",
      x="Cumulative treatments with ertapenem\nper 100,000 over 20 years"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels)+
    coord_cartesian(xlim=c(0, 20000))
  
  
  summary <- ggarrange(inc +
                         theme(axis.text.y = element_text(size = 8),axis.text.x = element_text(size = 8),axis.title.x = element_text(size = 8)), failure + 
                         theme(axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.title.y = element_blank(),
                               axis.text.x = element_text(size = 8),
                               axis.title.x = element_text(size = 8)), x + 
                         theme(axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.title.y = element_blank() ,
                               axis.text.x = element_text(size = 8),
                               axis.title.x = element_text(size = 8)), nrow = 1)
  return(summary)
}


summary_plot_isemph = function(df1, df2, df3){
  df1ends <- cumulative_everything(df1)
  df2ends <- cumulative_everything(df2)
  df3ends <- cumulative_everything(df3)

  
  allends <- rbind(df1ends,
                   df2ends,
                   df3ends)
  
  
  counter_levels <- c("drug_sus_testing_80","GISP_05", "random")
  counter_labels <- c("DST", "GISP", "RT")
  
  inc <- ggplot(data = allends, aes(x=cumulativeInc/1000, y = factor(counterfactual, levels = counter_levels), fill = counterfactual)) +
    geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
    labs(
      title = "A.",
      y = "",
      x="Cumulative incidence over 20 years\nper 100,000 (thousands)"
    )+
    my_theme +    scale_fill_brewer(palette = "Set2", direction=-1)+
    scale_y_discrete(labels=counter_labels)+
    coord_cartesian(xlim=c(0, 800))
  
  failure <- ggplot(data = allends, aes(x=cumulativeFailure * 100, y = factor(counterfactual, levels = counter_levels), fill = counterfactual)) +
    geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
    labs(
      title = "B.",
      y = "",
      x="% failures per treatment attempt\ncumulative over 20 years"
    )+
    my_theme +    scale_fill_brewer(palette = "Set2", direction=-1)+
    scale_y_discrete(labels=counter_labels) +
    coord_cartesian(xlim=c(0, 50))
  
  x <- ggplot(data = allends, aes(x=cumulativeE, y = factor(counterfactual, levels = counter_levels), fill = counterfactual)) +
    geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
    labs(
      title = "C.",
      y = "",
      x="Cumulative treatments with\nertapenemover 20 years"
    )+
    my_theme +    scale_fill_brewer(palette = "Set2", direction=-1)+
    scale_y_discrete(labels=counter_labels)+
    coord_cartesian(xlim=c(0, 4000))
  
  
  summary <- ggarrange(inc, failure + 
                         theme(axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.title.y = element_blank() ), x + 
                         theme(axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.title.y = element_blank() ), nrow = 1)
  return(summary)
}

summary_plot_smdm = function(df1, df2, df3, df4){
  df1ends <- cumulative_everything(df1)
  df2ends <- cumulative_everything(df2)
  df3ends <- cumulative_everything(df3)
  df4ends <- cumulative_everything(df4)
  
  
  allends <- rbind(df1ends,
                   df2ends,
                   df3ends,
                   df4ends)
  
  
  counter_levels <- c("realistic_combo_33_33_34", "drug_sus_testing_80", "test-of-cure_80", "GISP_05")
  counter_labels <- c("RC", "DST", "TOC", "GISP")
  
  inc <- ggplot(data = allends, aes(x=cumulativeInc/1000, y = factor(counterfactual, levels = counter_levels), color = counterfactual)) +
    stat_summary(fun = mean, fun.min = function(a) {quantile(a, 0.025)}, fun.max = function(a){quantile(a, 0.975)}, show.legend = FALSE)+
    labs(
      title = "A.",
      y = "",
      x="Cumulative incidence over 20 years\nper 100,000 (thousands)"
    )+
    my_theme +    scale_fill_brewer(palette = "Set2", direction=-1)+
    scale_y_discrete(labels=counter_labels)+
    coord_cartesian(xlim=c(0, 1500))
  
  failure <- ggplot(data = allends, aes(x=cumulativeFailure * 100, y = factor(counterfactual, levels = counter_levels), color = counterfactual)) +
    stat_summary(fun = mean, fun.min = function(a) {quantile(a, 0.025)}, fun.max = function(a){quantile(a, 0.975)}, show.legend = FALSE)+
    labs(
      title = "B.",
      y = "",
      x="% failures per treatment attempt\ncumulative over 20 years"
    )+
    my_theme +    scale_fill_brewer(palette = "Set2", direction=-1)+
    scale_y_discrete(labels=counter_labels) +
    coord_cartesian(xlim=c(0, 100))
  
  x <- ggplot(data = allends, aes(x=cumulativeE, y = factor(counterfactual, levels = counter_levels), color = counterfactual)) +
    stat_summary(fun = mean, fun.min = function(a) {quantile(a, 0.025)}, fun.max = function(a){quantile(a, 0.975)}, show.legend = FALSE)+
    labs(
      title = "C.",
      y = "",
      x="Cumulative treatments with\nertapenemover 20 years"
    )+
    my_theme +    scale_fill_brewer(palette = "Set2", direction=-1)+
    scale_y_discrete(labels=counter_labels)+
    coord_cartesian(xlim=c(0, 40000))
  
  
  summary <- ggarrange(inc, failure + 
                         theme(axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.title.y = element_blank() ), x + 
                         theme(axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.title.y = element_blank() ), nrow = 1)
  return(summary)
}

summary_plot_sa = function(df1, df2, df3, df4, df5, counter_levels, counter_labels, failure_max, ertapenem_max){
  df1ends <- cumulative_everything(df1)
  df2ends <- cumulative_everything(df2)
  df3ends <- cumulative_everything(df3)
  df4ends <- cumulative_everything(df4)
  df5ends <- cumulative_everything(df5)
  
  
  allends <- rbind(df1ends,
                   df2ends,
                   df3ends, 
                   df4ends,
                   df5ends)
  
  
  
  inc <- ggplot(data = allends, aes(x=cumulativeInc/1000, y = factor(counterfactual, levels = counter_levels))) +
    stat_summary(fun = mean, fun.min = function(a) {quantile(a, 0.025)}, fun.max = function(a){quantile(a, 0.975)})+
    labs(
      title = "A.",
      y = "",
      x="Cumulative incidence over 20 years\nper 100,000 (thousands)"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels)+
    coord_cartesian(xlim=c(0, 1500))
  
  failure <- ggplot(data = allends, aes(x=cumulativeFailure * 100, y = factor(counterfactual, levels = counter_levels))) +
    stat_summary(fun = mean, fun.min = function(a) {quantile(a, 0.025)}, fun.max = function(a){quantile(a, 0.975)})+
    labs(
      title = "B.",
      y = "",
      x="% failures per treatment attempt\ncumulative over 20 years"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels) +
    coord_cartesian(xlim=c(0, failure_max))
  
  x <- ggplot(data = allends, aes(x=cumulativeE, y = factor(counterfactual, levels = counter_levels))) +
    stat_summary(fun = mean, fun.min = function(a) {quantile(a, 0.025)}, fun.max = function(a){quantile(a, 0.975)})+
    labs(
      title = "C.",
      y = "",
      x="Cumulative treatments with\nertapenem over 20 years"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels)+
    coord_cartesian(xlim=c(0, ertapenem_max))
  
  
  summary <- ggarrange(inc, failure + 
                         theme(axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.title.y = element_blank() ), x + 
                         theme(axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.title.y = element_blank() ), nrow = 1)
  return(summary)
}


summary_plot_sa_seven = function(df1, df2, df3, df4, df5, df6, df7, counter_levels, counter_labels, failure_max, ertapenem_max){
  df1ends <- cumulative_everything(df1)
  df2ends <- cumulative_everything(df2)
  df3ends <- cumulative_everything(df3)
  df4ends <- cumulative_everything(df4)
  df5ends <- cumulative_everything(df5)
  df6ends <- cumulative_everything(df6)
  df7ends <- cumulative_everything(df7)
  
  
  
  allends <- rbind(df1ends,
                   df2ends,
                   df3ends, 
                   df4ends,
                   df5ends,
                   df6ends,
                   df7ends)
  
  
  
  inc <- ggplot(data = allends, aes(x=cumulativeInc/1000, y = factor(counterfactual, levels = counter_levels))) +
    stat_summary(fun = mean, fun.min = function(a) {quantile(a, 0.025)}, fun.max = function(a){quantile(a, 0.975)})+
    labs(
      title = "A.",
      y = "",
      x="Cumulative incidence over 20 years\nper 100,000 (thousands)"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels)+
    coord_cartesian(xlim=c(0, 1500))
  
  failure <- ggplot(data = allends, aes(x=cumulativeFailure * 100, y = factor(counterfactual, levels = counter_levels))) +
    stat_summary(fun = mean, fun.min = function(a) {quantile(a, 0.025)}, fun.max = function(a){quantile(a, 0.975)})+
    labs(
      title = "B.",
      y = "",
      x="% failures per treatment attempt\ncumulative over 20 years"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels) +
    coord_cartesian(xlim=c(0, failure_max))
  
  x <- ggplot(data = allends, aes(x=cumulativeE, y = factor(counterfactual, levels = counter_levels))) +
    stat_summary(fun = mean, fun.min = function(a) {quantile(a, 0.025)}, fun.max = function(a){quantile(a, 0.975)})+
    labs(
      title = "C.",
      y = "",
      x="Cumulative treatments with\nertapenem over 20 years"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels)+
    coord_cartesian(xlim=c(0, ertapenem_max))
  
  
  summary <- ggarrange(inc, failure + 
                         theme(axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.title.y = element_blank() ), x + 
                         theme(axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.title.y = element_blank() ), nrow = 1)
  return(summary)
}


summary_plot_color = function(df1, df2, df3, df4){
  df1ends <- cumulative_everything(df1)
  df2ends <- cumulative_everything(df2)
  df3ends <- cumulative_everything(df3)
  df4ends <- cumulative_everything(df4)
  
  
  allends <- rbind(df1ends,
                   df2ends,
                   df3ends, 
                   df4ends)
  
  
  counter_levels <- c("drug_sus_testing_80","test-of-cure_80", "random","GISP")
  counter_labels <- c("DST", "TOC", "RT", "GISP")
  
  inc <- ggplot(data = allends, aes(x=cumulativeInc/1000, y = factor(counterfactual, levels = counter_levels), fill = counterfactual)) +
    geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
    labs(
      title = "A.",
      y = "",
      x="Cumulative incidence over 30 years\nper 100,000 (thousands)"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels)+
    scale_fill_brewer(palette = "Set2", direction=-1)+
    coord_cartesian(xlim=c(0, 400))
  
  failure <- ggplot(data = allends, aes(x=cumulativeFailure *100, y = factor(counterfactual, levels = counter_levels), fill = counterfactual)) +
    geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
    labs(
      title = "B.",
      y = "",
      x="% failures per treatment attempt\ncumulative, A, B, & X over 30 years"
    )+
    my_theme +
    scale_fill_brewer(palette = "Set2", direction=-1)+
    
    scale_y_discrete(labels=counter_labels) +
    coord_cartesian(xlim=c(0, 80))
  
  x <- ggplot(data = allends, aes(x=cumulativeX/1000, y = factor(counterfactual, levels = counter_levels), fill = counterfactual)) +
    geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
    labs(
      title = "C.",
      y = "",
      x="Treatments with drug X\nover 30 years (thousands)"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels)+
    scale_fill_brewer(palette = "Set2", direction=-1)+
    
    coord_cartesian(xlim=c(0, 60))
  
  cumulativeCosts <- ggplot(data=allends, aes(x=cumulativeCosts/1000000, y = factor(counterfactual, levels = counter_levels), fill = counterfactual))+
    geom_boxplot(outlier.shape = NA, show.legend = FALSE)+
    labs(
      title = "D.",
      y = "",
      x="Cumulative costs over 30 years in millions USD"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels)+
    scale_fill_brewer(palette = "Set2", direction=-1)+
    
    coord_cartesian(xlim=c(0, 120))
  
  summary <- ggarrange(inc, failure,
                         x, nrow = 3)
  return(summary)
}


smdm_summary_plot_color = function(df1, df2, df3, df4){
  df1ends <- cumulative_everything(df1)
  df2ends <- cumulative_everything(df2)
  df3ends <- cumulative_everything(df3)
  df4ends <- cumulative_everything(df4)
  
  
  allends <- rbind(df1ends,
                   df2ends,
                   df3ends, 
                   df4ends)
  
  
  counter_levels <- c("drug_sus_testing_80","test-of-cure_80", "random","GISP")
  counter_labels <- c("DST", "TOC", "RT", "GISP")
  
  inc <- ggplot(data = allends, aes(x=cumulativeInc/1000, y = factor(counterfactual, levels = counter_levels), fill = counterfactual)) +
    geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
    labs(
      title = "A.",
      y = "",
      x="Cumulative incidence over 30 years\nper 100,000 (thousands)"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels)+
    scale_fill_brewer(palette = "Set2", direction=-1)+
    coord_cartesian(xlim=c(0, 500))
  
  failure <- ggplot(data = allends, aes(x=cumulativeFailure *100, y = factor(counterfactual, levels = counter_levels), fill = counterfactual)) +
    geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
    labs(
      title = "B.",
      y = "",
      x="% failures per treatment attempt\ncumulative over 30 years"
    )+
    my_theme +
    scale_fill_brewer(palette = "Set2", direction=-1)+
    
    scale_y_discrete(labels=counter_labels) +
    coord_cartesian(xlim=c(0, 60))
  
  resist <- ggplot(data = allends, aes(x=cumulativeResist, y = factor(counterfactual, levels = counter_levels), fill = counterfactual)) +
    geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
    labs(
      title = "C.",
      y = "",
      x="Incidence of resistant cases"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels)+
    scale_fill_brewer(palette = "Set2", direction=-1)+
    
    coord_cartesian(xlim=c(0, 300000))
  
  
  summary <- ggarrange(inc, failure,
                       resist, nrow = 3)
  return(summary)
}

giant_inc_box = function(df, title){
  counter_levels <- c("realistic_combo","drug_sus_testing_80","test-of-cure_80", "random","GISP")
  counter_labels <- c("RC", "DST", "TOC", "RT", "GISP")
  
  inc <- ggplot(data = df, aes(x=cumulativeInc/1000, y = factor(counterfactual, levels = counter_levels))) +
    stat_summary(fun = mean, fun.min = function(a) {quantile(a, 0.025)}, fun.max = function(a){quantile(a, 0.975)})+
    labs(
      title = title,
      y = "",
      x="Cumulative incidence over 20 years\nper 100,000 (thousands)"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels)+
    coord_cartesian(xlim=c(0, 2000))
  
  return(inc)
  
}

giant_failure_box = function(df, title){
  counter_levels <- c("realistic_combo","drug_sus_testing_80","test-of-cure_80", "random","GISP")
  counter_labels <- c("RC","DST", "TOC", "RT", "GISP")
  
  failure <- ggplot(data = df, aes(x=cumulativeFailure * 100, y = factor(counterfactual, levels = counter_levels))) +
    stat_summary(fun = mean, fun.min = function(a) {quantile(a, 0.025)}, fun.max = function(a){quantile(a, 0.975)})+
    labs(
      title = title,
      y = "",
      x="% failures per treatment attempt\ncumulative over 20 years"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels) +
    coord_cartesian(xlim=c(0, 100))
  
  return(failure)
  
}

giant_e_box = function(df, title, xlim=150000){
  counter_levels <- c("realistic_combo","drug_sus_testing_80","test-of-cure_80", "random","GISP")
  counter_labels <- c("RC","DST", "TOC", "RT", "GISP")
  
  e <- ggplot(data = df, aes(x=cumulativeE, y = factor(counterfactual, levels = counter_levels))) +
    stat_summary(fun = mean, fun.min = function(a) {quantile(a, 0.025)}, fun.max = function(a){quantile(a, 0.975)})+
    labs(
      title = title,
      y = "",
      x="Treatments with ertapenem\nover 20 years"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels)+
    coord_cartesian(xlim=c(0, xlim))
  
  
}

giant_summary_plot = function(dfGISP15, dfGISP20, dfGISP25, dfGISP31,
                              dfrandom15, dfrandom20, dfrandom25, dfrandom31,
                              dfTOC15, dfTOC20, dfTOC25, dfTOC31, 
                              dfDST15, dfDST20, dfDST25, dfDST31,
                              dfreal15, dfreal20, dfreal25, dfreal31){
  #process the different avails of X
  # df1ends <- cumulative_everything(dfGISP10)
  # df2ends <- cumulative_everything(dfrandom10)
  # df3ends <- cumulative_everything(dfTOC10)
  # df4ends <- cumulative_everything(dfDST10)
  # 
  # 
  # allends10 <- rbind(df1ends,
  #                  df2ends,
  #                  df3ends, 
  #                  df4ends)
  # 
  df1ends <- cumulative_everything(dfGISP15)
  df2ends <- cumulative_everything(dfrandom15)
  df3ends <- cumulative_everything(dfTOC15)
  df4ends <- cumulative_everything(dfDST15)
  df5ends <- cumulative_everything(dfreal15)
  
  
  allends15 <- rbind(df1ends,
                     df2ends,
                     df3ends, 
                     df4ends,
                     df5ends)
  
  df1ends <- cumulative_everything(dfGISP20)
  df2ends <- cumulative_everything(dfrandom20)
  df3ends <- cumulative_everything(dfTOC20)
  df4ends <- cumulative_everything(dfDST20)
  df5ends <- cumulative_everything(dfreal20)
  
  
  allends20 <- rbind(df1ends,
                     df2ends,
                     df3ends, 
                     df4ends,
                     df5ends)
  
  df1ends <- cumulative_everything(dfGISP25)
  df2ends <- cumulative_everything(dfrandom25)
  df3ends <- cumulative_everything(dfTOC25)
  df4ends <- cumulative_everything(dfDST25)
  
  dfreal25$counterfactual = rep("realistic_combo", length(dfreal25$counterfactual))
  df5ends <- cumulative_everything(dfreal25)
  
  
  allends25 <- rbind(df1ends,
                     df2ends,
                     df3ends, 
                     df4ends,
                     df5ends)
  
  df1ends <- cumulative_everything(dfGISP31)
  df2ends <- cumulative_everything(dfrandom31)
  df3ends <- cumulative_everything(dfTOC31)
  df4ends <- cumulative_everything(dfDST31)
  df5ends <- cumulative_everything(dfreal31)
  
  
  allends31 <- rbind(df1ends,
                     df2ends,
                     df3ends, 
                     df4ends,
                     df5ends)
  
  
  counter_levels <- c("realistic_combo", "drug_sus_testing_80","test-of-cure_80", "random","GISP")
  counter_labels <- c("RC", "DST", "TOC", "RT", "GISP")
  
  # inc10 <- giant_inc_box(allends10, "A. Drug X available year 10")
  # 
  # failure10 <-  giant_failure_box(allends10, "B.")
  # 
  # x10 <- giant_e_box(allends10, "C.")
  # 
  
  
  inc15 <-  giant_inc_box(allends15, "A. Drug X available year 10")
    
  failure15 <- giant_failure_box(allends15, "B.")
     
  
  x15 <-  giant_e_box(allends15, "C.")
   
  
  
  inc20 <-  giant_inc_box(allends20,  "D. Drug X available year 15")
    
  
  failure20 <- giant_failure_box(allends20, "E.")

  
  x20 <- giant_e_box(allends20, "F.")
     
  
  inc25 <- giant_inc_box(allends25, "G. Drug X available year 20")
      
  
  failure25 <- giant_failure_box(allends25,  "H.")
    
  
  x25 <- giant_e_box(allends25,  "I.")
    
  
  inc31 <- giant_inc_box(allends31, "J. Drug X never available")
     
  
  failure31 <- giant_failure_box(allends31,  "K.")
    

  x31 <- giant_e_box(allends31, "L.")
     
  
  
   summary <- ggarrange(
     #inc10, failure10 + 
  #                        theme(axis.text.y = element_blank(),
  #                              axis.ticks.y = element_blank(),
  #                              axis.title.y = element_blank() ), x10 + 
  #                        theme(axis.text.y = element_blank(),
  #                              axis.ticks.y = element_blank(),
  #                              axis.title.y = element_blank() ), 
                       inc15, failure15 + 
                         theme(axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.title.y = element_blank() ), x15 + 
                         theme(axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.title.y = element_blank() ),
                       inc20, failure20 + 
                         theme(axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.title.y = element_blank() ), x20 + 
                         theme(axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.title.y = element_blank() ),
                       inc25, failure25 + 
                         theme(axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.title.y = element_blank() ), x25 + 
                         theme(axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.title.y = element_blank() ),
                       inc31, failure31 + 
                         theme(axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.title.y = element_blank() ), x31 + 
                         theme(axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.title.y = element_blank() ),nrow = 4, widths=c(1, 1, 1))
  return(summary)
}



summary_cost_plot = function(df1, df2, df3, df4){
  df1ends <- cumulative_everything(df1)
  df2ends <- cumulative_everything(df2)
  df3ends <- cumulative_everything(df3)
  df4ends <- cumulative_everything(df4)
  
  
  allends <- rbind(df1ends,
                   df2ends,
                   df3ends, 
                   df4ends)
  
  
  counter_levels <- c("drug_sus_testing_80","test-of-cure_80", "random","GISP")
  counter_labels <- c("DST 80%", "Test-of-Cure 80%", "Randomized", "GISP")
  
  cumulativeCosts <- ggplot(data=allends, aes(x=cumulativeCosts/1000000, y = factor(counterfactual, levels = counter_levels)))+
    geom_boxplot(outlier.shape = NA)+
    labs(
      title = "D.",
      y = "",
      x="Cumulative costs over 30 years in millions USD"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels)+
    coord_cartesian(xlim=c(0, 120))
  
  cumulativeQALYs <- ggplot(data=allends, aes(x=cumulativeQALYs, y = factor(counterfactual, levels = counter_levels)))+
    geom_boxplot(outlier.shape = NA)+
    labs(
      title = "D.",
      y = "",
      x="Cumulative QALYs lost over 30 years"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels)
    #coord_cartesian(xlim=c(0, 120))
  
  return(multiplot(cumulativeCosts, cumulativeQALYs))
  
}

############

#cost-effectiveness analysis
############
cea = function(dfGISP, dfRandom, dfTOC, dfDST){
#new df which contains the cumulative outcomes for cost and QALYs, relative to GISP
ceadf <- data.frame(matrix(ncol = 24, nrow = length(unique(dfGISP$seed))))
colnames(ceadf) <- c("seed", 
                    # "InitialInfected",
                     "TransmissionMSM",
                     "RecoveryLambda",
                     "ProbSymptomaticMSM",
                     "ScreenIntervalMSM",
                     "DelayToSeekCareMSM",
                     "DelayToRetreatmentMSM",
                     "PercentResistantA",
                     #"BeginImportingB",
                     #"ImportingBInterval",
                     "DSTsensitivity",
                     #"DSTspecificity",
                     "CareCost",
                     "TestCost",
                     "StrainTestCost",
                     "DrugATreatmentCost",
                     "DrugBTreatmentCost",
                     "DrugXTreatmentCost",
                     "DrugETreatmentCost",
                     "GISPcumulativeQALYs", 
                     "GISPcumulativeCosts", 
                     "RandomcumulativeCosts", 
                     "RandomcumulativeQALYs", 
                     "TOCcumulativeCosts", 
                     "TOCcumulativeQALYs", 
                     "DSTcumulativeCosts", 
                     "DSTcumulativeQALYs")

dfGISP <- dfGISP[order(dfGISP$seed),]
dfRandom <- dfRandom[order(dfRandom$seed),]
dfTOC <- dfTOC[order(dfTOC$seed),]
dfDST <- dfDST[order(dfDST$seed),]


ceadf$seed <- unique(dfGISP$seed)

#ceadf$InitialInfected <- unique(dfGISP$InitialInfected)

ceadf$TransmissionMSM<- unique(dfGISP$TransmissionMSM)
ceadf$RecoveryLambda<- unique(dfGISP$RecoveryLambda)
ceadf$ProbSymptomaticMSM<- unique(dfGISP$ProbSymptomaticMSM)
ceadf$ScreenIntervalMSM<- unique(dfGISP$ScreenIntervalMSM)
ceadf$DelayToSeekCareMSM<- unique(dfGISP$DelayToSeekCareMSM)
ceadf$DelayToRetreatmentMSM<- unique(dfGISP$DelayToRetreatmentMSM)
ceadf$PercentResistantA<- unique(dfGISP$PercentResistantA)
#ceadf$BeginImportingB<- unique(dfGISP$BeginImportingB)
#ceadf$ImportingBInterval<- unique(dfGISP$ImportingBInterval)
ceadf$DSTsensitivity<- unique(dfGISP$DSTsensitivity)
#ceadf$DSTspecificity<- unique(dfGISP$DSTspecificity)
ceadf$CareCost<- unique(dfGISP$CareCost)
ceadf$TestCost<- unique(dfGISP$TestCost)
ceadf$StrainTestCost<- unique(dfGISP$StrainTestCost)
ceadf$DrugATreatmentCost<- unique(dfGISP$DrugATreatmentCost)
ceadf$DrugBTreatmentCost<- unique(dfGISP$DrugBTreatmentCost)
ceadf$DrugXTreatmentCost<- unique(dfGISP$DrugXTreatmentCost)
ceadf$DrugETreatmentCost<- unique(dfGISP$DrugETreatmentCost)


ceadf$GISPcumulativeCosts <- cumulative_costs(dfGISP)
ceadf$GISPcumulativeQALYs <- cumulative_QALYs(dfGISP)
ceadf$RandomcumulativeCosts <- cumulative_costs(dfRandom)
ceadf$RandomcumulativeQALYs <- cumulative_QALYs(dfRandom)
ceadf$TOCcumulativeCosts <- cumulative_costs(dfTOC)
ceadf$TOCcumulativeQALYs <- cumulative_QALYs(dfTOC)
ceadf$DSTcumulativeCosts <- cumulative_costs(dfDST)
ceadf$DSTcumulativeQALYs <- cumulative_QALYs(dfDST)

ceadf$GISPcumulativeCostsAdj <- ceadf$GISPcumulativeCosts - ceadf$GISPcumulativeCosts
ceadf$RandomcumulativeCostsAdj <- ceadf$RandomcumulativeCosts - ceadf$GISPcumulativeCosts
ceadf$TOCcumulativeCostsAdj <- ceadf$TOCcumulativeCosts - ceadf$GISPcumulativeCosts
ceadf$DSTcumulativeCostsAdj <- ceadf$DSTcumulativeCosts - ceadf$GISPcumulativeCosts

ceadf$GISPcumulativeQALYsAdj <- ceadf$GISPcumulativeQALYs - ceadf$GISPcumulativeQALYs
ceadf$RandomcumulativeQALYsAdj <- ceadf$RandomcumulativeQALYs - ceadf$GISPcumulativeQALYs
ceadf$TOCcumulativeQALYsAdj <- ceadf$TOCcumulativeQALYs - ceadf$GISPcumulativeQALYs
ceadf$DSTcumulativeQALYsAdj <- ceadf$DSTcumulativeQALYs - ceadf$GISPcumulativeQALYs

#get the cumulative QALYS and cost for each scenario
#subtract GISP values from GISP, random, TOC, and DST values
#add each seed under each scenario as its own row in a df


return(ceadf)
}

cea_weighted = function(resampledf, dfGISP, dfRandom, dfTOC, dfDST){
  #new df which contains the cumulative outcomes for cost and QALYs, relative to GISP
  ceadf <- data.frame(matrix(ncol = 24, nrow = length(unique(dfGISP$seed))))
  colnames(ceadf) <- c("seed", 
                       # "InitialInfected",
                       "TransmissionMSM",
                       "RecoveryLambda",
                       "ProbSymptomaticMSM",
                       "ScreenIntervalMSM",
                       "DelayToSeekCareMSM",
                       "DelayToRetreatmentMSM",
                       "PercentResistantA",
                       #"BeginImportingB",
                       #"ImportingBInterval",
                       "DSTsensitivity",
                       #"DSTspecificity",
                       "CareCost",
                       "TestCost",
                       "StrainTestCost",
                       "DrugATreatmentCost",
                       "DrugBTreatmentCost",
                       "DrugXTreatmentCost",
                       "DrugETreatmentCost",
                       "GISPcumulativeQALYs", 
                       "GISPcumulativeCosts", 
                       "RandomcumulativeCosts", 
                       "RandomcumulativeQALYs", 
                       "TOCcumulativeCosts", 
                       "TOCcumulativeQALYs", 
                       "DSTcumulativeCosts", 
                       "DSTcumulativeQALYs")
  
  dfGISP <- dfGISP[order(dfGISP$seed),]
  dfRandom <- dfRandom[order(dfRandom$seed),]
  dfTOC <- dfTOC[order(dfTOC$seed),]
  dfDST <- dfDST[order(dfDST$seed),]
  
  
  ceadf$seed <- unique(dfGISP$seed)
  
  #ceadf$InitialInfected <- unique(dfGISP$InitialInfected)
  
  ceadf$TransmissionMSM<- unique(dfGISP$TransmissionMSM)
  ceadf$RecoveryLambda<- unique(dfGISP$RecoveryLambda)
  ceadf$ProbSymptomaticMSM<- unique(dfGISP$ProbSymptomaticMSM)
  ceadf$ScreenIntervalMSM<- unique(dfGISP$ScreenIntervalMSM)
  ceadf$DelayToSeekCareMSM<- unique(dfGISP$DelayToSeekCareMSM)
  ceadf$DelayToRetreatmentMSM<- unique(dfGISP$DelayToRetreatmentMSM)
  ceadf$PercentResistantA<- unique(dfGISP$PercentResistantA)
  #ceadf$BeginImportingB<- unique(dfGISP$BeginImportingB)
  #ceadf$ImportingBInterval<- unique(dfGISP$ImportingBInterval)
  ceadf$DSTsensitivity<- unique(dfGISP$DSTsensitivity)
  #ceadf$DSTspecificity<- unique(dfGISP$DSTspecificity)
  ceadf$CareCost<- unique(dfGISP$CareCost)
  ceadf$TestCost<- unique(dfGISP$TestCost)
  ceadf$StrainTestCost<- unique(dfGISP$StrainTestCost)
  ceadf$DrugATreatmentCost<- unique(dfGISP$DrugATreatmentCost)
  ceadf$DrugBTreatmentCost<- unique(dfGISP$DrugBTreatmentCost)
  ceadf$DrugXTreatmentCost<- unique(dfGISP$DrugXTreatmentCost)
  ceadf$DrugETreatmentCost<- unique(dfGISP$DrugETreatmentCost)
  
  
  ceadf$GISPcumulativeCosts <- cumulative_costs(dfGISP)
  ceadf$GISPcumulativeQALYs <- cumulative_QALYs(dfGISP)
  ceadf$RandomcumulativeCosts <- cumulative_costs(dfRandom)
  ceadf$RandomcumulativeQALYs <- cumulative_QALYs(dfRandom)
  ceadf$TOCcumulativeCosts <- cumulative_costs(dfTOC)
  ceadf$TOCcumulativeQALYs <- cumulative_QALYs(dfTOC)
  ceadf$DSTcumulativeCosts <- cumulative_costs(dfDST)
  ceadf$DSTcumulativeQALYs <- cumulative_QALYs(dfDST)
  
  ceadf$GISPcumulativeCostsAdj <- ceadf$GISPcumulativeCosts - ceadf$GISPcumulativeCosts
  ceadf$RandomcumulativeCostsAdj <- ceadf$RandomcumulativeCosts - ceadf$GISPcumulativeCosts
  ceadf$TOCcumulativeCostsAdj <- ceadf$TOCcumulativeCosts - ceadf$GISPcumulativeCosts
  ceadf$DSTcumulativeCostsAdj <- ceadf$DSTcumulativeCosts - ceadf$GISPcumulativeCosts
  
  ceadf$GISPcumulativeQALYsAdj <- ceadf$GISPcumulativeQALYs - ceadf$GISPcumulativeQALYs
  ceadf$RandomcumulativeQALYsAdj <- ceadf$RandomcumulativeQALYs - ceadf$GISPcumulativeQALYs
  ceadf$TOCcumulativeQALYsAdj <- ceadf$TOCcumulativeQALYs - ceadf$GISPcumulativeQALYs
  ceadf$DSTcumulativeQALYsAdj <- ceadf$DSTcumulativeQALYs - ceadf$GISPcumulativeQALYs
  
  #get the cumulative QALYS and cost for each scenario
  #subtract GISP values from GISP, random, TOC, and DST values
  #add each seed under each scenario as its own row in a df
  newceadf <- ceadf
  for (traj in row.names(ceadf)){
    #how many times does traj appear in resampled df?
    dup <- sum(resampledf$seed==ceadf$seed[as.numeric(traj)])-1
    
    #add that many duplicate rows to ceadf
    if (dup > 0){
     newceadf <- rbind(newceadf, ceadf[rep(traj,dup),])
    }
  }
  
  return(newceadf)
}

cea_real_weighted = function(resampledf, dfReal, dfGISP, dfRandom, dfTOC, dfDST){
  #new df which contains the cumulative outcomes for cost and QALYs, relative to GISP
  ceadf <- data.frame(matrix(ncol = 26, nrow = length(unique(dfGISP$uniqueID))))
  colnames(ceadf) <- c("seed", 
                       # "InitialInfected",
                       "TransmissionMSM",
                       "RecoveryLambda",
                       "ProbSymptomaticMSM",
                       "ScreenIntervalMSM",
                       "DelayToSeekCareMSM",
                       "DelayToRetreatmentMSM",
                       "PercentResistantA",
                       #"BeginImportingB",
                       #"ImportingBInterval",
                       "DSTsensitivity",
                       #"DSTspecificity",
                       "CareCost",
                       "TestCost",
                       "StrainTestCost",
                       "DrugATreatmentCost",
                       "DrugBTreatmentCost",
                       "DrugXTreatmentCost",
                       "DrugETreatmentCost",
                       "RCcumulativeCosts",
                       "RCcumulativeQALYs",
                       "GISPcumulativeQALYs", 
                       "GISPcumulativeCosts", 
                       "RandomcumulativeCosts", 
                       "RandomcumulativeQALYs", 
                       "TOCcumulativeCosts", 
                       "TOCcumulativeQALYs", 
                       "DSTcumulativeCosts", 
                       "DSTcumulativeQALYs")
  
  dfReal <- dfReal[order(dfReal$uniqueID),]
  dfGISP <- dfGISP[order(dfGISP$uniqueID),]
  dfRandom <- dfRandom[order(dfRandom$uniqueID),]
  dfTOC <- dfTOC[order(dfTOC$uniqueID),]
  dfDST <- dfDST[order(dfDST$uniqueID),]
  
  
  ceadf$seed <- unique(dfReal$seed)
  
  #ceadf$InitialInfected <- unique(dfGISP$InitialInfected)
  
  ceadf$TransmissionMSM<- unique(dfGISP$TransmissionMSM)
  ceadf$RecoveryLambda<- unique(dfGISP$RecoveryLambda)
  ceadf$ProbSymptomaticMSM<- unique(dfGISP$ProbSymptomaticMSM)
  ceadf$ScreenIntervalMSM<- unique(dfGISP$ScreenIntervalMSM)
  ceadf$DelayToSeekCareMSM<- unique(dfGISP$DelayToSeekCareMSM)
  ceadf$DelayToRetreatmentMSM<- unique(dfGISP$DelayToRetreatmentMSM)
  ceadf$PercentResistantA<- unique(dfGISP$PercentResistantA)
  #ceadf$BeginImportingB<- unique(dfGISP$BeginImportingB)
  #ceadf$ImportingBInterval<- unique(dfGISP$ImportingBInterval)
  ceadf$DSTsensitivity<- unique(dfGISP$DSTsensitivity)
  #ceadf$DSTspecificity<- unique(dfGISP$DSTspecificity)
  ceadf$CareCost<- unique(dfGISP$CareCost)
  ceadf$TestCost<- unique(dfGISP$TestCost)
  ceadf$StrainTestCost<- unique(dfGISP$StrainTestCost)
  ceadf$DrugATreatmentCost<- unique(dfGISP$DrugATreatmentCost)
  ceadf$DrugBTreatmentCost<- unique(dfGISP$DrugBTreatmentCost)
  ceadf$DrugXTreatmentCost<- unique(dfGISP$DrugXTreatmentCost)
  ceadf$DrugETreatmentCost<- unique(dfGISP$DrugETreatmentCost)
  
  ceadf$RCcumulativeCosts <- cumulative_costs(dfReal)
  ceadf$RCcumulativeQALYs <- cumulative_QALYs(dfReal)
  ceadf$GISPcumulativeCosts <- cumulative_costs(dfGISP)
  ceadf$GISPcumulativeQALYs <- cumulative_QALYs(dfGISP)
  ceadf$RandomcumulativeCosts <- cumulative_costs(dfRandom)
  ceadf$RandomcumulativeQALYs <- cumulative_QALYs(dfRandom)
  ceadf$TOCcumulativeCosts <- cumulative_costs(dfTOC)
  ceadf$TOCcumulativeQALYs <- cumulative_QALYs(dfTOC)
  ceadf$DSTcumulativeCosts <- cumulative_costs(dfDST)
  ceadf$DSTcumulativeQALYs <- cumulative_QALYs(dfDST)
  
  ceadf$RCcumulativeCostsAdj <- ceadf$RCcumulativeCosts - ceadf$RCcumulativeCosts
  ceadf$GISPcumulativeCostsAdj <- ceadf$GISPcumulativeCosts - ceadf$RCcumulativeCosts
  ceadf$RandomcumulativeCostsAdj <- ceadf$RandomcumulativeCosts - ceadf$RCcumulativeCosts
  ceadf$TOCcumulativeCostsAdj <- ceadf$TOCcumulativeCosts - ceadf$RCcumulativeCosts
  ceadf$DSTcumulativeCostsAdj <- ceadf$DSTcumulativeCosts - ceadf$RCcumulativeCosts
  
  ceadf$RCcumulativeQALYsAdj <- ceadf$RCcumulativeQALYs - ceadf$RCcumulativeQALYs
  ceadf$GISPcumulativeQALYsAdj <- ceadf$GISPcumulativeQALYs - ceadf$RCcumulativeQALYs
  ceadf$RandomcumulativeQALYsAdj <- ceadf$RandomcumulativeQALYs - ceadf$RCcumulativeQALYs
  ceadf$TOCcumulativeQALYsAdj <- ceadf$TOCcumulativeQALYs - ceadf$RCcumulativeQALYs
  ceadf$DSTcumulativeQALYsAdj <- ceadf$DSTcumulativeQALYs - ceadf$RCcumulativeQALYs
  
  #get the cumulative QALYS and cost for each scenario
  #subtract GISP values from GISP, random, TOC, and DST values
  #add each seed under each scenario as its own row in a df
  newceadf <- ceadf
 
  
   for (traj in row.names(ceadf)){
    #how many times does traj appear in resampled df?
    dup <- sum(resampledf$seed==ceadf$seed[as.numeric(traj)])-1

    #add that many duplicate rows to ceadf
    if (dup > 0){
      newceadf <- rbind(newceadf, ceadf[rep(traj,dup),])
    }
  }

  return(newceadf)
}

ceaMSM = function(dfGISP, dfRandom, dfTOC, dfDST){
  #new df which contains the cumulative outcomes for cost and QALYs, relative to GISP
  ceadf <- data.frame(matrix(ncol = 9, nrow = 200))
  colnames(ceadf) <- c("seed", 
                       "GISPcumulativeQALYs", 
                       "GISPcumulativeCosts", 
                       "RandomcumulativeCosts", 
                       "RandomcumulativeQALYs", 
                       "TOCcumulativeCosts", 
                       "TOCcumulativeQALYs", 
                       "DSTcumulativeCosts", 
                       "DSTcumulativeQALYs")
  
  
  ceadf$seed <- unique(dfGISP$seed)
  
  ceadf$GISPcumulativeCosts <- cumulative_costs_MSM(dfGISP)
  ceadf$GISPcumulativeQALYs <- cumulative_QALYs_MSM(dfGISP)
  ceadf$RandomcumulativeCosts <- cumulative_costs_MSM(dfRandom)
  ceadf$RandomcumulativeQALYs <- cumulative_QALYs_MSM(dfRandom)
  ceadf$TOCcumulativeCosts <- cumulative_costs_MSM(dfTOC)
  ceadf$TOCcumulativeQALYs <- cumulative_QALYs_MSM(dfTOC)
  ceadf$DSTcumulativeCosts <- cumulative_costs_MSM(dfDST)
  ceadf$DSTcumulativeQALYs <- cumulative_QALYs_MSM(dfDST)
  
  ceadf$GISPcumulativeCostsAdj <- ceadf$GISPcumulativeCosts - ceadf$GISPcumulativeCosts
  ceadf$RandomcumulativeCostsAdj <- ceadf$RandomcumulativeCosts - ceadf$GISPcumulativeCosts
  ceadf$TOCcumulativeCostsAdj <- ceadf$TOCcumulativeCosts - ceadf$GISPcumulativeCosts
  ceadf$DSTcumulativeCostsAdj <- ceadf$DSTcumulativeCosts - ceadf$GISPcumulativeCosts
  
  ceadf$GISPcumulativeQALYsAdj <- ceadf$GISPcumulativeQALYs - ceadf$GISPcumulativeQALYs
  ceadf$RandomcumulativeQALYsAdj <- ceadf$RandomcumulativeQALYs - ceadf$GISPcumulativeQALYs
  ceadf$TOCcumulativeQALYsAdj <- ceadf$TOCcumulativeQALYs - ceadf$GISPcumulativeQALYs
  ceadf$DSTcumulativeQALYsAdj <- ceadf$DSTcumulativeQALYs - ceadf$GISPcumulativeQALYs
  
  #get the cumulative QALYS and cost for each scenario
  #subtract GISP values from GISP, random, TOC, and DST values
  #add each seed under each scenario as its own row in a df
  
  
  return(ceadf)
}


ceaMSW = function(dfGISP, dfRandom, dfTOC, dfDST){
  #new df which contains the cumulative outcomes for cost and QALYs, relative to GISP
  ceadf <- data.frame(matrix(ncol = 9, nrow = 200))
  colnames(ceadf) <- c("seed", 
                       "GISPcumulativeQALYs", 
                       "GISPcumulativeCosts", 
                       "RandomcumulativeCosts", 
                       "RandomcumulativeQALYs", 
                       "TOCcumulativeCosts", 
                       "TOCcumulativeQALYs", 
                       "DSTcumulativeCosts", 
                       "DSTcumulativeQALYs")
  
  
  ceadf$seed <- unique(dfGISP$seed)
  
  ceadf$GISPcumulativeCosts <- cumulative_costs(dfGISP)
  ceadf$GISPcumulativeQALYs <- cumulative_QALYs(dfGISP)
  ceadf$RandomcumulativeCosts <- cumulative_costs(dfRandom)
  ceadf$RandomcumulativeQALYs <- cumulative_QALYs(dfRandom)
  ceadf$TOCcumulativeCosts <- cumulative_costs(dfTOC)
  ceadf$TOCcumulativeQALYs <- cumulative_QALYs(dfTOC)
  ceadf$DSTcumulativeCosts <- cumulative_costs(dfDST)
  ceadf$DSTcumulativeQALYs <- cumulative_QALYs(dfDST)
  
  ceadf$GISPcumulativeCostsAdj <- ceadf$GISPcumulativeCosts - ceadf$GISPcumulativeCosts
  ceadf$RandomcumulativeCostsAdj <- ceadf$RandomcumulativeCosts - ceadf$GISPcumulativeCosts
  ceadf$TOCcumulativeCostsAdj <- ceadf$TOCcumulativeCosts - ceadf$GISPcumulativeCosts
  ceadf$DSTcumulativeCostsAdj <- ceadf$DSTcumulativeCosts - ceadf$GISPcumulativeCosts
  
  ceadf$GISPcumulativeQALYsAdj <- ceadf$GISPcumulativeQALYs - ceadf$GISPcumulativeQALYs
  ceadf$RandomcumulativeQALYsAdj <- ceadf$RandomcumulativeQALYs - ceadf$GISPcumulativeQALYs
  ceadf$TOCcumulativeQALYsAdj <- ceadf$TOCcumulativeQALYs - ceadf$GISPcumulativeQALYs
  ceadf$DSTcumulativeQALYsAdj <- ceadf$DSTcumulativeQALYs - ceadf$GISPcumulativeQALYs
  
  #get the cumulative QALYS and cost for each scenario
  #subtract GISP values from GISP, random, TOC, and DST values
  #add each seed under each scenario as its own row in a df
  
  
  return(ceadf)
}


ceaW = function(dfGISP, dfRandom, dfTOC, dfDST){
  #new df which contains the cumulative outcomes for cost and QALYs, relative to GISP
  ceadf <- data.frame(matrix(ncol = 9, nrow = 200))
  colnames(ceadf) <- c("seed", 
                       "GISPcumulativeQALYs", 
                       "GISPcumulativeCosts", 
                       "RandomcumulativeCosts", 
                       "RandomcumulativeQALYs", 
                       "TOCcumulativeCosts", 
                       "TOCcumulativeQALYs", 
                       "DSTcumulativeCosts", 
                       "DSTcumulativeQALYs")
  
  
  ceadf$seed <- unique(dfGISP$seed)
  
  ceadf$GISPcumulativeCosts <- cumulative_costs(dfGISP)
  ceadf$GISPcumulativeQALYs <- cumulative_QALYs(dfGISP)
  ceadf$RandomcumulativeCosts <- cumulative_costs(dfRandom)
  ceadf$RandomcumulativeQALYs <- cumulative_QALYs(dfRandom)
  ceadf$TOCcumulativeCosts <- cumulative_costs(dfTOC)
  ceadf$TOCcumulativeQALYs <- cumulative_QALYs(dfTOC)
  ceadf$DSTcumulativeCosts <- cumulative_costs(dfDST)
  ceadf$DSTcumulativeQALYs <- cumulative_QALYs(dfDST)
  
  ceadf$GISPcumulativeCostsAdj <- ceadf$GISPcumulativeCosts - ceadf$GISPcumulativeCosts
  ceadf$RandomcumulativeCostsAdj <- ceadf$RandomcumulativeCosts - ceadf$GISPcumulativeCosts
  ceadf$TOCcumulativeCostsAdj <- ceadf$TOCcumulativeCosts - ceadf$GISPcumulativeCosts
  ceadf$DSTcumulativeCostsAdj <- ceadf$DSTcumulativeCosts - ceadf$GISPcumulativeCosts
  
  ceadf$GISPcumulativeQALYsAdj <- ceadf$GISPcumulativeQALYs - ceadf$GISPcumulativeQALYs
  ceadf$RandomcumulativeQALYsAdj <- ceadf$RandomcumulativeQALYs - ceadf$GISPcumulativeQALYs
  ceadf$TOCcumulativeQALYsAdj <- ceadf$TOCcumulativeQALYs - ceadf$GISPcumulativeQALYs
  ceadf$DSTcumulativeQALYsAdj <- ceadf$DSTcumulativeQALYs - ceadf$GISPcumulativeQALYs
  
  #get the cumulative QALYS and cost for each scenario
  #subtract GISP values from GISP, random, TOC, and DST values
  #add each seed under each scenario as its own row in a df
  
  
  return(ceadf)
}


rearrange_cea = function(ceadf){
  newdf <- data.frame(matrix(ncol=4, nrow = 0))
  colnames(newdf)<- c("seed",
                      "counterfactual",
                      "AdjustedCost",
                      "AdjustedQALYs")
  
  newdf <- rbind(newdf, data.frame(seed = ceadf$seed, counterfactual = rep("GISP", length(ceadf$GISPcumulativeCostsAdj)), AdjustedCost = ceadf$GISPcumulativeCostsAdj, AdjustedQALYs = ceadf$GISPcumulativeQALYsAdj))
  newdf <- rbind(newdf, data.frame(seed = ceadf$seed, counterfactual=rep("Random", length(ceadf$GISPcumulativeCostsAdj)), AdjustedCost =ceadf$RandomcumulativeCostsAdj, AdjustedQALYs = -ceadf$RandomcumulativeQALYsAdj))
  newdf <- rbind(newdf, data.frame(seed = ceadf$seed, counterfactual=rep("TOC", length(ceadf$GISPcumulativeCostsAdj)), AdjustedCost =ceadf$TOCcumulativeCostsAdj, AdjustedQALYs = -ceadf$TOCcumulativeQALYsAdj))
  newdf <- rbind(newdf, data.frame(seed = ceadf$seed, counterfactual=rep("DST", length(ceadf$GISPcumulativeCostsAdj)), AdjustedCost =ceadf$DSTcumulativeCostsAdj, AdjustedQALYs = -ceadf$DSTcumulativeQALYsAdj))
  
  return(newdf)
}

rearrange_cea_real = function(ceadf){
  newdf <- data.frame(matrix(ncol=5, nrow = 0))
  colnames(newdf)<- c("seed",
                      "counterfactual",
                      "AdjustedCost",
                      "AdjustedQALYs")
  
  newdf <- rbind(newdf, data.frame(seed = ceadf$seed, counterfactual = rep("RC", length(ceadf$GISPcumulativeCostsAdj)), AdjustedCost = ceadf$RCcumulativeCostsAdj, AdjustedQALYs = ceadf$RCcumulativeQALYsAdj) )
  newdf <- rbind(newdf, data.frame(seed = ceadf$seed, counterfactual = rep("GISP", length(ceadf$GISPcumulativeCostsAdj)), AdjustedCost = ceadf$GISPcumulativeCostsAdj, AdjustedQALYs = -ceadf$GISPcumulativeQALYsAdj))
  newdf <- rbind(newdf, data.frame(seed = ceadf$seed, counterfactual=rep("Random", length(ceadf$GISPcumulativeCostsAdj)), AdjustedCost =ceadf$RandomcumulativeCostsAdj, AdjustedQALYs = -ceadf$RandomcumulativeQALYsAdj))
  newdf <- rbind(newdf, data.frame(seed = ceadf$seed, counterfactual=rep("TOC", length(ceadf$GISPcumulativeCostsAdj)), AdjustedCost =ceadf$TOCcumulativeCostsAdj, AdjustedQALYs = -ceadf$TOCcumulativeQALYsAdj))
  newdf <- rbind(newdf, data.frame(seed = ceadf$seed, counterfactual=rep("DST", length(ceadf$GISPcumulativeCostsAdj)), AdjustedCost =ceadf$DSTcumulativeCostsAdj, AdjustedQALYs = -ceadf$DSTcumulativeQALYsAdj))
  
  return(newdf)
}

visualize_cea = function(title, df1, df2, df3, df4){
ceadf_sum <- cea(df1, df2, df3, df4)
ceadf_sum <- rbind(ceadf_sum, lapply(ceadf_sum[], mean))
ceadf_sum$seed[51] <- "mean"

ceadf <- rearrange_cea(ceadf_sum)

counter_levels <- c("GISP", "Random", "TOC", "DST")
counter_labels <- c("GISP", "RT", "TOC", "DST")

ceadf$counterfactual <- factor(ceadf$counterfactual, levels=counter_levels, labels=counter_labels)

ggplot() +
  geom_hline(yintercept=0, color = "gray") +
  geom_vline(xintercept = 0, color = "gray") +
  geom_point(data=ceadf, aes(x=AdjustedQALYs, y=AdjustedCost/1000000, color=counterfactual), size = 0.1, show.legend = FALSE) +
  geom_point(data=ceadf[ceadf$seed=="mean",], aes(x=AdjustedQALYs, y=AdjustedCost/1000000, fill=counterfactual), size = 2, shape = 23) +
  labs(title=title,
       x="Discounted incremental QALYs",
       y="Discounted incremental costs (in millions USD)",
       fill="Counterfactual")+
  scale_fill_brewer(palette="Set2")+
  scale_color_brewer(palette="Set2")+
  coord_cartesian(xlim=c(-5000, 20), ylim=c(-100, 160))+
  my_theme  
}

visualize_cea_weighted = function(title, resampledf, df1, df2, df3, df4){
  
  ceadf_sum <- cea_weighted(resampledf, df1, df2, df3, df4)
  ceadf_sum <- rbind(ceadf_sum, lapply(ceadf_sum[], mean))
  ceadf_sum$seed[nrow(ceadf_sum)] <- "mean"
  
  ceadf <- rearrange_cea(ceadf_sum)
  
  counter_levels <- c("GISP", "Random", "TOC", "DST")
  counter_labels <- c("GISP", "RT", "TOC", "DST")
  
  ceadf$counterfactual <- factor(ceadf$counterfactual, levels=counter_levels, labels=counter_labels)
  
  ggplot() +
   # geom_hline(yintercept=0, color = "gray") +
    #geom_vline(xintercept = 0, color = "gray") +
    geom_point(data=ceadf, aes(x=AdjustedQALYs, y=AdjustedCost/1000000, color=counterfactual), size = 0.1, show.legend = FALSE) +
    geom_point(data=ceadf[ceadf$seed=="mean",], aes(x=AdjustedQALYs, y=AdjustedCost/1000000, fill=counterfactual), size = 2, shape = 23) +
    labs(title=title,
         x="Discounted incremental QALYs",
         y="Discounted incremental costs\n(in millions USD)",
         fill="Counterfactual")+
    scale_fill_brewer(palette="Set2")+
    scale_color_brewer(palette="Set2")+
   # coord_cartesian(xlim=c(-600, 50), ylim=c(-5, 25))+
    my_theme  
}


visualize_cea_weighted_real = function(title, resampledf, df1, df2, df3, df4, df5){
  
  ceadf_sum <- cea_real_weighted(resampledf, df1, df2, df3, df4, df5)
  ceadf_sum <- rbind(ceadf_sum, lapply(ceadf_sum[], mean))
  ceadf_sum$seed[nrow(ceadf_sum)] <- "mean"
  
  ceadf <- rearrange_cea_real(ceadf_sum)
  
  counter_levels <- c("RC", "GISP", "Random", "TOC", "DST")
  counter_labels <- c("RC", "GISP", "RT", "TOC", "DST")
  
  ceadf$counterfactual <- factor(ceadf$counterfactual, levels=counter_levels, labels=counter_labels)
  
  ggplot() +
    # geom_hline(yintercept=0, color = "gray") +
    #geom_vline(xintercept = 0, color = "gray") +
    geom_point(data=ceadf, aes(x=AdjustedQALYs, y=AdjustedCost/1000000, color=counterfactual), size = 0.1, show.legend = FALSE) +
    geom_point(data=ceadf[ceadf$seed=="mean",], aes(x=AdjustedQALYs, y=AdjustedCost/1000000, fill=counterfactual), size = 2, shape = 23) +
    labs(title=title,
         x="Discounted incremental QALYs",
         y="Discounted\nincremental costs\n(in millions USD)",
         fill="Counterfactual")+
    scale_fill_brewer(palette="Set1")+
    scale_color_brewer(palette="Set1")+
    # coord_cartesian(xlim=c(-600, 50), ylim=c(-5, 25))+
    my_theme  
}


visualize_cea_MSM = function(title, df1, df2, df3, df4){
  ceadf_sum <- ceaMSM(df1, df2, df3, df4)
  ceadf_sum <- rbind(ceadf_sum, lapply(ceadf_sum[], mean))
  ceadf_sum$seed[51] <- "mean"
  
  ceadf <- rearrange_cea(ceadf_sum)
  
  counter_levels <- c("GISP", "Random", "TOC", "DST")
  counter_labels <- c("GISP", "RT", "TOC", "DST")
  
  ceadf$counterfactual <- factor(ceadf$counterfactual, levels=counter_levels, labels=counter_labels)
  
  ggplot() +
    geom_hline(yintercept=0, color = "gray") +
    geom_vline(xintercept = 0, color = "gray") +
    geom_point(data=ceadf, aes(x=AdjustedQALYs, y=AdjustedCost/1000000, color=counterfactual), size = 0.2, show.legend = FALSE) +
    geom_point(data=ceadf[ceadf$seed=="mean",], aes(x=AdjustedQALYs, y=AdjustedCost/1000000, fill=counterfactual), size = 2, shape = 23) +
    labs(title=title,
         x="Discounted incremental QALYs",
         y="Discounted incremental costs\n(in millions USD)",
         fill="Counterfactual")+
    scale_fill_brewer(palette="Set2")+
    scale_color_brewer(palette="Set2")+
    #coord_cartesian(xlim=c(0, 20), ylim=c(-20, 40))+
    my_theme 
}

visualize_cea_weighted_real = function(title, resampledf, df1, df2, df3, df4, df5){
  
  ceadf_sum <- cea_real_weighted(resampledf, df1, df2, df3, df4, df5)
  ceadf_sum <- rbind(ceadf_sum, lapply(ceadf_sum[], mean))
  ceadf_sum$seed[nrow(ceadf_sum)] <- "mean"
  
  ceadf <- rearrange_cea_real(ceadf_sum)
  
  counter_levels <- c("RC", "GISP", "Random", "TOC", "DST")
  counter_labels <- c("RC", "GISP", "RT", "TOC", "DST")
  
  ceadf$counterfactual <- factor(ceadf$counterfactual, levels=counter_levels, labels=counter_labels)
  
  ggplot() +
    # geom_hline(yintercept=0, color = "gray") +
    #geom_vline(xintercept = 0, color = "gray") +
    geom_point(data=ceadf, aes(x=AdjustedQALYs, y=AdjustedCost/1000000, color=counterfactual), size = 0.1, show.legend = FALSE) +
    geom_point(data=ceadf[ceadf$seed=="mean",], aes(x=AdjustedQALYs, y=AdjustedCost/1000000, fill=counterfactual), size = 2, shape = 23) +
    labs(title=title,
         x="Discounted incremental QALYs",
         y="Discounted\nincremental costs\n(in millions USD)",
         fill="Counterfactual")+
    scale_fill_brewer(palette="Set1")+
    scale_color_brewer(palette="Set1")+
    # coord_cartesian(xlim=c(-600, 50), ylim=c(-5, 25))+
    my_theme  
}


visualize_cea_weighted_real_nort = function(title, resampledf, df1, df2, df3, df4, df5){
  
  my_pal <- c("RC"="#53B0B5", "GISP"= "#77A030", "Random"= "#000000", "TOC"= "#AD63F7", "DST"= "#E26860")

  
  ceadf_sum <- cea_real_weighted(resampledf, df1, df2, df3, df4, df5)
  ceadf_sum <- rbind(ceadf_sum, lapply(ceadf_sum[], mean))
  ceadf_sum$seed[nrow(ceadf_sum)] <- "mean"
  
  ceadf <- rearrange_cea_real(ceadf_sum)
  
  ceadf <- ceadf %>% filter(counterfactual != "Random")
  
  
  counter_levels <- c("RC", "GISP", "Random", "TOC", "DST")
  counter_labels <- c("RC", "GISP", "RT", "TOC", "DST")
  
  ceadf$counterfactual <- factor(ceadf$counterfactual, levels=counter_levels, labels=counter_labels)
  
  
  ggplot() +
    # geom_hline(yintercept=0, color = "gray") +
    #geom_vline(xintercept = 0, color = "gray") +
    geom_point(data=ceadf, aes(x=AdjustedQALYs, y=AdjustedCost/1000000, color=counterfactual), size = 0.1, show.legend = FALSE) +
    geom_point(data=ceadf[ceadf$seed=="mean",], aes(x=AdjustedQALYs, y=AdjustedCost/1000000, fill=counterfactual), size = 2, shape = 23) +
    labs(title=title,
         x="Discounted incremental QALYs",
         y="Discounted\nincremental costs\n(in millions USD)",
         fill="Counterfactual")+
    scale_fill_manual(values=my_pal)+
    scale_color_manual(values=my_pal)+
    # coord_cartesian(xlim=c(-600, 50), ylim=c(-5, 25))+
    my_theme  
}

visualize_cea_MSM = function(title, df1, df2, df3, df4){
  ceadf_sum <- ceaMSM(df1, df2, df3, df4)
  ceadf_sum <- rbind(ceadf_sum, lapply(ceadf_sum[], mean))
  ceadf_sum$seed[51] <- "mean"
  
  ceadf <- rearrange_cea(ceadf_sum)
  
  counter_levels <- c("GISP", "Random", "TOC", "DST")
  counter_labels <- c("GISP", "RT", "TOC", "DST")
  
  ceadf$counterfactual <- factor(ceadf$counterfactual, levels=counter_levels, labels=counter_labels)
  
  ggplot() +
    geom_hline(yintercept=0, color = "gray") +
    geom_vline(xintercept = 0, color = "gray") +
    geom_point(data=ceadf, aes(x=AdjustedQALYs, y=AdjustedCost/1000000, color=counterfactual), size = 0.2, show.legend = FALSE) +
    geom_point(data=ceadf[ceadf$seed=="mean",], aes(x=AdjustedQALYs, y=AdjustedCost/1000000, fill=counterfactual), size = 2, shape = 23) +
    labs(title=title,
         x="Discounted incremental QALYs",
         y="Discounted incremental costs\n(in millions USD)",
         fill="Counterfactual")+
    scale_fill_brewer(palette="Set2")+
    scale_color_brewer(palette="Set2")+
    #coord_cartesian(xlim=c(0, 20), ylim=c(-20, 40))+
    my_theme 
}


CI_lower = function(wtp, Dcost, Deffect){
  n <- length(Dcost)
  estimate <- wtp * mean(Deffect) - mean(Dcost)
  error <- equation_five(wtp, Dcost, Deffect)
  return(estimate - error)
}

CI_upper = function(wtp, Dcost, Deffect){
  n <- length(Dcost)
  estimate <- wtp * mean(Deffect) - mean(Dcost)
  error<- equation_five(wtp, Dcost, Deffect)
  return(estimate + error)
}

equation_five = function(wtp, Dcost, Deffect){
  n <- length(Dcost)
  error <- qt(0.975, n-1) * sqrt(equation_six(wtp, Dcost, Deffect)/n)
  return(error)
}

equation_six = function(wtp, Dcost, Deffect){
  return((wtp^2 * var(Deffect)) + var(Dcost) - cov((wtp * Deffect), Dcost))
}


nmb = function(cea_df, title){
  #y = (x * QALYs) - costs
  
  random_mean_intercept <- mean(cea_df$RandomcumulativeCostsAdj)
  random_mean_slope <- mean(cea_df$RandomcumulativeQALYsAdj)
  random_lower_begin <- CI_lower(1, cea_df$RandomcumulativeCostsAdj, -cea_df$RandomcumulativeQALYsAdj)
  random_lower_end <- CI_lower(160000, cea_df$RandomcumulativeCostsAdj, -cea_df$RandomcumulativeQALYsAdj)
  random_upper_begin <- CI_upper(1, cea_df$RandomcumulativeCostsAdj, -cea_df$RandomcumulativeQALYsAdj)
  random_upper_end <- CI_upper(160000, cea_df$RandomcumulativeCostsAdj, -cea_df$RandomcumulativeQALYsAdj)
  
  TOC_intercepts<- t.test(cea_df$TOCcumulativeCostsAdj)
  TOC_mean_intercept <- TOC_intercepts$estimate
  
  TOC_slopes <- t.test(cea_df$TOCcumulativeQALYsAdj)
  TOC_mean_slope <- TOC_slopes$estimate
  TOC_lower_begin <- CI_lower(1, cea_df$TOCcumulativeCostsAdj, -cea_df$TOCcumulativeQALYsAdj)
  TOC_lower_end <- CI_lower(160000, cea_df$TOCcumulativeCostsAdj, -cea_df$TOCcumulativeQALYsAdj)
  TOC_upper_begin <- CI_upper(1, cea_df$TOCcumulativeCostsAdj, -cea_df$TOCcumulativeQALYsAdj)
  TOC_upper_end <- CI_upper(160000, cea_df$TOCcumulativeCostsAdj, -cea_df$TOCcumulativeQALYsAdj)
  
  DST_intercepts<- t.test(cea_df$DSTcumulativeCostsAdj)
  DST_mean_intercept <- DST_intercepts$estimate
  
  DST_slopes <- t.test(cea_df$DSTcumulativeQALYsAdj)
  DST_mean_slope <- DST_slopes$estimate
  DST_lower_begin <- CI_lower(1, cea_df$DSTcumulativeCostsAdj, -cea_df$DSTcumulativeQALYsAdj)
  DST_lower_end <- CI_lower(160000, cea_df$DSTcumulativeCostsAdj, -cea_df$DSTcumulativeQALYsAdj)
  DST_upper_begin <- CI_upper(1, cea_df$DSTcumulativeCostsAdj, -cea_df$DSTcumulativeQALYsAdj)
  DST_upper_end <- CI_upper(160000, cea_df$DSTcumulativeCostsAdj, -cea_df$DSTcumulativeQALYsAdj)
  
  GISP_mean_intercept <- mean(cea_df$GISPcumulativeCostsAdj)
  GISP_mean_slope <- mean(cea_df$GISPcumulativeQALYsAdj)
  GISP_lower_begin <- CI_lower(1, cea_df$GISPcumulativeCostsAdj, -cea_df$GISPcumulativeQALYsAdj)
  GISP_lower_end <- CI_lower(160000, cea_df$GISPcumulativeCostsAdj, -cea_df$GISPcumulativeQALYsAdj)
  GISP_upper_begin <- CI_upper(1, cea_df$GISPcumulativeCostsAdj, -cea_df$GISPcumulativeQALYsAdj)
  GISP_upper_end <- CI_upper(160000, cea_df$GISPcumulativeCostsAdj, -cea_df$GISPcumulativeQALYsAdj)
  
  
  
  ggplot(data = cea_df) + 
    # geom_point(aes(x=c(0,-1), y = c(0, -1)))+
    scale_x_continuous(expand = c(0, 0), limits=c(0, 160000), breaks=c(0, 50000, 100000, 150000), labels = c("0", "50", "100", "150"))+
    scale_y_continuous(expand = c(0,0), limits = c(-20000000, 10000000), breaks = c(-15000000,-10000000, -5000000, 0, 5000000, 10000000), labels = c("-15","-10","-5", "0", "5","10"))+
    geom_abline(aes(slope = 0, intercept = 0), color = "#E41A1C")+
    
    geom_abline(aes(slope = -GISP_mean_slope, intercept=-GISP_mean_intercept),color="#377EB8") +
    geom_segment(aes(x = 1, y = GISP_lower_begin, xend = 160000, yend = GISP_lower_end), colour = "#377EB8", linetype= 'dashed')+
    geom_segment(aes(x = 1, y = GISP_upper_begin, xend = 160000, yend = GISP_upper_end), colour = "#377EB8", linetype= 'dashed')+
    
    geom_abline(aes(slope = -random_mean_slope, intercept=-random_mean_intercept),color="#4DAF4A") +
    geom_segment(aes(x = 1, y = random_lower_begin, xend = 160000, yend = random_lower_end), colour = "#4DAF4A", linetype= 'dashed')+
    geom_segment(aes(x = 1, y = random_upper_begin, xend = 160000, yend = random_upper_end), colour = "#4DAF4A", linetype= 'dashed')+
    
    
    
    geom_abline(aes(slope = -TOC_mean_slope, intercept=-TOC_mean_intercept), color = "#984EA3") +
    geom_segment(aes(x = 1, y = TOC_lower_begin, xend = 160000, yend = TOC_lower_end), colour = "#984EA3", linetype= 'dashed')+
    geom_segment(aes(x = 1, y = TOC_upper_begin, xend = 160000, yend = TOC_upper_end), colour = "#984EA3", linetype= 'dashed')+
    
    
    
    geom_abline(aes(slope = -DST_mean_slope, intercept=-DST_mean_intercept), color = "#FF7F00") +
    geom_segment(aes(x = 1, y = DST_lower_begin, xend = 160000, yend = DST_lower_end), colour = "#FF7F00", linetype= 'dashed')+
    geom_segment(aes(x = 1, y = DST_upper_begin, xend = 160000, yend = DST_upper_end), colour = "#FF7F00", linetype= 'dashed')+
    
    
    
    
    my_theme+
    labs(
      title=title,
      x = "Cost-effectiveness Threshold (1000s USD)",
      y = "Incremental\nNMB (millions USD)"
    )
  
}


nmb_nort = function(cea_df, title){
  #y = (x * QALYs) - costs
  
  random_mean_intercept <- mean(cea_df$RandomcumulativeCostsAdj)
  random_mean_slope <- mean(cea_df$RandomcumulativeQALYsAdj)
  random_lower_begin <- CI_lower(1, cea_df$RandomcumulativeCostsAdj, -cea_df$RandomcumulativeQALYsAdj)
  random_lower_end <- CI_lower(160000, cea_df$RandomcumulativeCostsAdj, -cea_df$RandomcumulativeQALYsAdj)
  random_upper_begin <- CI_upper(1, cea_df$RandomcumulativeCostsAdj, -cea_df$RandomcumulativeQALYsAdj)
  random_upper_end <- CI_upper(160000, cea_df$RandomcumulativeCostsAdj, -cea_df$RandomcumulativeQALYsAdj)
  
  TOC_intercepts<- t.test(cea_df$TOCcumulativeCostsAdj)
  TOC_mean_intercept <- TOC_intercepts$estimate
  
  TOC_slopes <- t.test(cea_df$TOCcumulativeQALYsAdj)
  TOC_mean_slope <- TOC_slopes$estimate
  TOC_lower_begin <- CI_lower(1, cea_df$TOCcumulativeCostsAdj, -cea_df$TOCcumulativeQALYsAdj)
  TOC_lower_end <- CI_lower(160000, cea_df$TOCcumulativeCostsAdj, -cea_df$TOCcumulativeQALYsAdj)
  TOC_upper_begin <- CI_upper(1, cea_df$TOCcumulativeCostsAdj, -cea_df$TOCcumulativeQALYsAdj)
  TOC_upper_end <- CI_upper(160000, cea_df$TOCcumulativeCostsAdj, -cea_df$TOCcumulativeQALYsAdj)
  
  DST_intercepts<- t.test(cea_df$DSTcumulativeCostsAdj)
  DST_mean_intercept <- DST_intercepts$estimate
  
  DST_slopes <- t.test(cea_df$DSTcumulativeQALYsAdj)
  DST_mean_slope <- DST_slopes$estimate
  DST_lower_begin <- CI_lower(1, cea_df$DSTcumulativeCostsAdj, -cea_df$DSTcumulativeQALYsAdj)
  DST_lower_end <- CI_lower(160000, cea_df$DSTcumulativeCostsAdj, -cea_df$DSTcumulativeQALYsAdj)
  DST_upper_begin <- CI_upper(1, cea_df$DSTcumulativeCostsAdj, -cea_df$DSTcumulativeQALYsAdj)
  DST_upper_end <- CI_upper(160000, cea_df$DSTcumulativeCostsAdj, -cea_df$DSTcumulativeQALYsAdj)
  
  GISP_mean_intercept <- mean(cea_df$GISPcumulativeCostsAdj)
  GISP_mean_slope <- mean(cea_df$GISPcumulativeQALYsAdj)
  GISP_lower_begin <- CI_lower(1, cea_df$GISPcumulativeCostsAdj, -cea_df$GISPcumulativeQALYsAdj)
  GISP_lower_end <- CI_lower(160000, cea_df$GISPcumulativeCostsAdj, -cea_df$GISPcumulativeQALYsAdj)
  GISP_upper_begin <- CI_upper(1, cea_df$GISPcumulativeCostsAdj, -cea_df$GISPcumulativeQALYsAdj)
  GISP_upper_end <- CI_upper(160000, cea_df$GISPcumulativeCostsAdj, -cea_df$GISPcumulativeQALYsAdj)
  
  
  
  ggplot(data = cea_df) + 
    # geom_point(aes(x=c(0,-1), y = c(0, -1)))+
    scale_x_continuous(expand = c(0, 0), limits=c(0, 160000), breaks=c(0, 50000, 100000, 150000), labels = c("0", "50", "100", "150"))+
    scale_y_continuous(expand = c(0,0), limits = c(-10000000, 5000000), breaks = c(-10000000, -5000000, 0, 5000000), labels = c("-10","-5", "0", "5"))+
    geom_abline(aes(slope = 0, intercept = 0), color = "#53B0B5")+
    
    geom_abline(aes(slope = -GISP_mean_slope, intercept=-GISP_mean_intercept),color="#77A030") +
    geom_segment(aes(x = 1, y = GISP_lower_begin, xend = 160000, yend = GISP_lower_end), colour = "#77A030", linetype= 'dashed')+
    geom_segment(aes(x = 1, y = GISP_upper_begin, xend = 160000, yend = GISP_upper_end), colour = "#77A030", linetype= 'dashed')+
    
    #geom_abline(aes(slope = -random_mean_slope, intercept=-random_mean_intercept),color="#4DAF4A") +
    #geom_segment(aes(x = 1, y = random_lower_begin, xend = 160000, yend = random_lower_end), colour = "#4DAF4A", linetype= 'dashed')+
    #geom_segment(aes(x = 1, y = random_upper_begin, xend = 160000, yend = random_upper_end), colour = "#4DAF4A", linetype= 'dashed')+
    
    
    
    geom_abline(aes(slope = -TOC_mean_slope, intercept=-TOC_mean_intercept), color = "#AD63F7") +
    geom_segment(aes(x = 1, y = TOC_lower_begin, xend = 160000, yend = TOC_lower_end), colour = "#AD63F7", linetype= 'dashed')+
    geom_segment(aes(x = 1, y = TOC_upper_begin, xend = 160000, yend = TOC_upper_end), colour = "#AD63F7", linetype= 'dashed')+
    
    
    
    geom_abline(aes(slope = -DST_mean_slope, intercept=-DST_mean_intercept), color = "#E26860") +
    geom_segment(aes(x = 1, y = DST_lower_begin, xend = 160000, yend = DST_lower_end), colour = "#E26860", linetype= 'dashed')+
    geom_segment(aes(x = 1, y = DST_upper_begin, xend = 160000, yend = DST_upper_end), colour = "#E26860", linetype= 'dashed')+
    
    
    
    
    my_theme+
    labs(
      title=title,
      x = "Cost-effectiveness Threshold (1000s USD)",
      y = "Incremental\nNMB (millions USD)"
    )
  
}


# 
# ggplot(data = cea_example) + 
#   # geom_point(aes(x=c(0,-1), y = c(0, -1)))+
#   ylim(-1000000, 1000000) + xlim(0, 150000)+
#   geom_abline(aes(slope = random_mean_slope, intercept=-random_mean_intercept)) +
#   geom_abline(aes(slope = random_lower_slope, intercept=-random_lower_intercept),linetype=2) +
#   geom_abline(aes(slope = random_upper_slope, intercept=-random_upper_intercept),linetype=2) 
#   
# 
# random_intercepts <- t.test(cea_example$RandomcumulativeCostsAdj)
# random_mean_intercept <- random_intercepts$estimate
# random_lower_intercept <- random_intercepts$conf.int[1]
# random_upper_intercept <- random_intercepts$conf.int[2]
# 
# random_slopes <- t.test(cea_example$RandomcumulativeQALYsAdj)
# random_mean_slope <- random_slopes$estimate
# random_lower_slope <- random_slopes$conf.int[1]
# random_upper_slope <- random_slopes$conf.int[2]
# 
# TOC_intercepts
# TOC_slopes
# 
# DST_intercepts
# DST_slopes

############


#PRCC analysis
##########


combine_avail = function(df10, df15, df20, df25, df31){
  return(rbind(df10, df15, df20, df25, df31))
}

prcc_arrange = function(df, outcome_col){
  #for PRCC, each outcome needs its own df
  #the only columns should be parameters for analysis + the one outcome
  df <- get_ends(df)
  
  new_df <- df[,5:23]
  
  new_df[,c(1,9)] <- lapply(new_df[,c(1,9)], as.numeric)
  
  outcome <- df[,outcome_col]
  
  new_df <- cbind(new_df, outcome)
  
  return(new_df)
}

prcc = function(df, outcome_col){
  epi.prcc(prcc_arrange(df, outcome_col))
}

prcc_prev = function(df){
  combined_ends <- get_ends(df)
  return(prcc(combined_ends, 25))
}

prcc_inc = function(df){
  combined_ends <- get_ends(df)
  combined_ends$cumulativInc <- cumulative_inc(df)
  return(prcc(combined_ends, 52))}

prcc_sympt = function(df){
  combined_ends <- get_ends(df)
  return(prcc(combined_ends, 30))}

prcc_cost = function(df){
  #combineddf <- combine_avail(df10, df15, df20, df25, df31)
  combined_ends <- get_ends(df)
  combined_ends$cumulativeCosts <- cumulative_costs(df) #gives me a vector of the cumulative costs in the order of uniqueIDs
  return(prcc(combined_ends, 52))}

prcc_qalys = function(df){
 # combineddf <- combine_avail(df10, df15, df20, df25, df31)
  combined_ends <- get_ends(df)
  combined_ends$cumulativeQALYs <- cumulative_QALYs(df) #gives me a vector of the cumulative QALYs in the order of uniqueIDs
  return(prcc(combined_ends, 52))}
  
  


prcc_all = function(df){
  prevalence <- prcc_prev(df)
  incidence <- prcc_inc(df)
  symptomprop <- prcc_sympt(df)
  monetaryCost <- prcc_cost(df)
  qalys <- prcc_qalys(df)
  return(list(prevalance=prevalence, incidence=incidence, symptomprop=symptomprop, monetaryCost=monetaryCost, QALYs=qalys))
}




#########


#individual panel functions: "viz_"
########

my_theme = theme_bw(base_size = 8)
 #*
viz_prev_cal = function(df, title){
  #df <- df %>% filter(tick > 260)
  prev <- ggplot(data = df, aes(x = tick / 52, group = uniqueID)) + 
    geom_line(aes(y = Prevalence),size = 0.01, color = "black") +
    # geom_point(aes(y=4.5, x = 6), color="red", size = 1) +
    # geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 6), color = "red")+
    # geom_point(aes(y=4.5, x = 7), color="red", size = 1) +
    # geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 7), color = "red")+
    # geom_point(aes(y=4.5, x = 8), color="red", size = 1) +
    # geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 8), color = "red")+
    # geom_point(aes(y=4.5, x = 9), color="red", size = 1) +
    # geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 9), color = "red")+
    # geom_point(aes(y=4.5, x = 10), color="red", size = 1) +
    # geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 10), color = "red")+
    labs(title = title,
         x = "Year",
         y = "Prevalence (%) in MSM") +
    theme(plot.title = element_text(size=8)) +
    coord_cartesian(ylim=c(0,11), xlim=c(0, 10))+
    my_theme
  return(prev)
}


viz_incMSM_cal = function(df, title){
  df <- df %>% filter(tick >= 260)
  inc <- ggplot(data = df, aes(x = (tick / 52)-5, y = 100000 * (Detected / 100000), group = uniqueID)) + 
    geom_line( aes(y = 100000 * (Detected / 100000), alpha=resampled),size = 0.05, color="black") +
    geom_point(aes(y=6508, x = 1), color="red", size = 1) +
    geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 1), color = "red", size=0.25)+
    geom_point(aes(y=6508, x = 2), color="red", size = 1) +
    geom_errorbar(aes(ymin = 5206, ymax = 7809, x =2), color = "red", size=0.25)+
    geom_point(aes(y=6508, x = 3), color="red", size = 1) +
    geom_errorbar(aes(ymin = 5206, ymax = 7809,x = 3), color = "red", size=0.25)+
    geom_point(aes(y=6508, x = 4), color="red", size = 1) +
    geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 4), color = "red", size=0.25)+
    geom_point(aes(y=6508, x = 5), color="red", size = 1) +
    geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 5), color = "red", size=0.25)+
    labs(title = title,
         x = "Year",
         y = "Cases Detected per 100,000 MSM")+
    theme(plot.title = element_text(size=8)) +
    coord_cartesian(ylim= c(0,20000),xlim=c(0,25))+
    my_theme+
    theme(legend.position = "none")+
    scale_alpha(range=c(0.25, 1))+
    annotate("rect", xmin = 0, xmax=5, ymin=-Inf, ymax=Inf, alpha = 0.25)
  return(inc)
}

viz_incM_cal = function(df, title){
  df <- df %>% filter(tick > 260)
  df$detectedIncM <- df$detectedIncMSM + df$detectedIncMSW + df$detectedIncMSMW
  df$MPopSize <- df$MSMPopSize + df$MSMWPopSize + df$MSWPopSize
  
  
  inc <- ggplot(data = df, aes(x = tick / 52, y = 100000 * (detectedIncM / MPopSize), group = RunNumber)) + 
    geom_line( aes(y = 100000 * (detectedIncM / MPopSize)),size = 0.05, color="black") +
    geom_point(aes(y=212.8, x = 6), color="red", size = 1) +
    geom_errorbar(aes(ymin = 170.24, ymax = 255.36, x = 6), color = "red")+
    geom_point(aes(y=212.8, x = 7), color="red", size = 1) +
    geom_errorbar(aes(ymin = 170.24, ymax = 255.36, x = 7), color = "red")+
    geom_point(aes(y=212.8, x = 8), color="red", size = 1) +
    geom_errorbar(aes(ymin = 170.24, ymax = 255.36,x = 8), color = "red")+
    geom_point(aes(y=212.8, x = 9), color="red", size = 1) +
    geom_errorbar(aes(ymin = 170.24, ymax = 255.36, x = 9), color = "red")+
    geom_point(aes(y=212.8, x = 10), color="red", size = 1) +
    geom_errorbar(aes(ymin = 170.24, ymax = 255.36, x = 10), color = "red")+
    labs(title = title,
         x = "Year",
         y = "Cases Detected per 100,000 Males")+
    theme(plot.title = element_text(size=8)) +
    coord_cartesian(ylim= c(0,55000),xlim=c(5,30))+
    my_theme
  return(inc)
}


viz_incW_cal = function(df, title){
  df <- df %>% filter(tick > 260)
  inc <- ggplot(data = df, aes(x = tick / 52, y = 100000 * (detectedIncW / WPopSize), group = RunNumber)) + 
    geom_line( aes(y = 100000 * (detectedIncW / WPopSize)),size = 0.05, color="black") +
    geom_point(aes(y=145.8, x = 6), color="red", size = 1) +
    geom_errorbar(aes(ymin = 116.64, ymax = 174.96, x = 6), color = "red")+
    geom_point(aes(y=145.8, x = 7), color="red", size = 1) +
    geom_errorbar(aes(ymin = 116.64, ymax = 174.96, x = 7), color = "red")+
    geom_point(aes(y=145.8, x = 8), color="red", size = 1) +
    geom_errorbar(aes(ymin = 116.64, ymax = 174.96,x = 8), color = "red")+
    geom_point(aes(y=145.8, x = 9), color="red", size = 1) +
    geom_errorbar(aes(ymin = 116.64, ymax = 174.96, x = 9), color = "red")+
    geom_point(aes(y=145.8, x = 10), color="red", size = 1) +
    geom_errorbar(aes(ymin = 116.64, ymax = 174.96, x = 10), color = "red")+
    labs(title = title,
         x = "Year",
         y = "Cases Detected per 100,000")+
    theme(plot.title = element_text(size=8)) +
    coord_cartesian(ylim= c(0,55000),xlim=c(5,30))+
    my_theme
  return(inc)
}


viz_sympt_MSM_cal = function(df, title){
  df <- df %>% filter(tick >= 260)
  symptomatic <- ggplot(data = df, aes(x = (tick / 52)-5, y = DetectedAndSymptoms / Detected, group = uniqueID)) + 
    geom_line( aes(y = DetectedAndSymptoms / Detected, alpha=resampled),size = 0.05, color="black") +
    geom_point(aes(y=0.679, x = 1), color="red", size = 1) +
    geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 1), color = "red", size=0.25)+
    geom_point(aes(y=0.679, x = 2), color="red", size = 1) +
    geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 2), color = "red", size=0.25)+
    geom_point(aes(y=0.679, x = 3), color="red", size = 1) +
    geom_errorbar(aes(ymin = 0.628, ymax = 0.7265,x = 3), color = "red", size=0.25)+
    geom_point(aes(y=0.679, x = 4), color="red", size = 1) +
    geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 4), color = "red", size=0.25)+
    geom_point(aes(y=0.679, x = 5), color="red", size = 1) +
    geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 5), color = "red", size=0.25)+
    labs(x = "Year",
         y = "Prop. Detected Cases with Symptoms", 
         title = title) +
    coord_cartesian(ylim= c(0.5, 1.0),xlim=c(0,25))+
   # scale_y_continuous(limits = c(0.0, 1.0), labels = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) + #ylim(0.4, 0.9) +
    my_theme +
    theme(legend.position = "none")+
    scale_alpha(range=c(0.25, 1))+
    annotate("rect", xmin = 0, xmax=5, ymin=-Inf, ymax=Inf, alpha = 0.25)
  
  return(symptomatic)
}

viz_sympt_MSW_cal = function(df, title){
  df <- df %>% filter(tick > 260)
  symptomatic <- ggplot(data = df, aes(x = tick / 52, y = detectedAndSymptomsMSW / detectedIncMSW, group = RunNumber)) + 
    geom_line( aes(y = detectedAndSymptomsMSW / detectedIncMSW),size = 0.05, color="black") +
    geom_point(aes(y=0.79, x = 6), color="red", size = 1) +
    geom_errorbar(aes(ymin = 0.756, ymax = 0.8227, x = 6), color = "red")+
    geom_point(aes(y=0.79, x = 7), color="red", size = 1) +
    geom_errorbar(aes(ymin = 0.756, ymax = 0.8227, x = 7), color = "red")+
    geom_point(aes(y=0.79, x = 8), color="red", size = 1) +
    geom_errorbar(aes(ymin = 0.756, ymax = 0.8227,x = 8), color = "red")+
    geom_point(aes(y=0.79, x = 9), color="red", size = 1) +
    geom_errorbar(aes(ymin = 0.756, ymax = 0.8227, x = 9), color = "red")+
    geom_point(aes(y=0.79, x = 10), color="red", size = 1) +
    geom_errorbar(aes(ymin = 0.756, ymax = 0.8227, x = 10), color = "red")+
    labs(x = "Year",
         y = "Prop. Detected Cases with Symptoms", 
         title = title) +
    #scale_y_continuous(limits = c(0.0, 1.0), labels = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) + #ylim(0.4, 0.9) +
    my_theme
  
  return(symptomatic)
}


viz_sympt_W_cal = function(df, title){
  df <- df %>% filter(tick > 260)
  symptomatic <- ggplot(data = df, aes(x = tick / 52, y = detectedAndSymptomsW / detectedIncW, group = RunNumber)) + 
    geom_line( aes(y = detectedAndSymptomsW / detectedIncW),size = 0.05, color="black") +
    geom_point(aes(y=0.584, x = 6), color="red", size = 1) +
    geom_errorbar(aes(ymin = 0.5545941, ymax = 0.6135945, x = 6), color = "red")+
    geom_point(aes(y=0.584, x = 7), color="red", size = 1) +
    geom_errorbar(aes(ymin = 0.5545941, ymax = 0.6135945, x = 7), color = "red")+
    geom_point(aes(y=0.584, x = 8), color="red", size = 1) +
    geom_errorbar(aes(ymin = 0.5545941, ymax = 0.6135945,x = 8), color = "red")+
    geom_point(aes(y=0.584, x = 9), color="red", size = 1) +
    geom_errorbar(aes(ymin = 0.5545941, ymax = 0.6135945, x = 9), color = "red")+
    geom_point(aes(y=0.584, x = 10), color="red", size = 1) +
    geom_errorbar(aes(ymin = 0.5545941, ymax = 0.6135945, x = 10), color = "red")+
    labs(x = "Year",
         y = "Prop. Detected Cases with Symptoms", 
         title = title) +
    #scale_y_continuous(limits = c(0.0, 1.0), labels = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) + #ylim(0.4, 0.9) +
    my_theme
  
  return(symptomatic)
}



 #*
viz_prev = function(df, title, yearX){
  df <- df %>% filter(tick >= 260)
  prev <- ggplot(data = df, aes(x = (tick / 52) - 5, group = seed)) + 
     geom_line(aes(y = Prevalence, alpha = resampled),linewidth = 0.05) +
    # geom_point(aes(y=4.5, x = 6), color="red", size = 1) +
    # geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 6), color = "red")+
    # geom_point(aes(y=4.5, x = 7), color="red", size = 1) +
    # geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 7), color = "red")+
    # geom_point(aes(y=4.5, x = 8), color="red", size = 1) +
    # geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 8), color = "red")+
    # geom_point(aes(y=4.5, x = 9), color="red", size = 1) +
    # geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 9), color = "red")+
    # geom_point(aes(y=4.5, x = 10), color="red", size = 1) +
    # geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 10), color = "red")+
    labs(title = title,
         x = "Year",
         y = "Prevalence (%)") +
    theme(plot.title = element_text(size=8), legend.position = "none") +
    geom_vline(xintercept=yearX, linetype="dashed")+
    coord_cartesian(ylim=c(0,10), xlim=c(0, 25))+
    my_theme +
    theme(legend.position = "none")+
    scale_alpha(range=c(0.25, 1))+
    annotate("rect", xmin = 0, xmax=5, ymin=-Inf, ymax=Inf, alpha = 0.25)
    
  return(prev)
}

viz_prev_5 = function(df, title){
  prev <- ggplot(data = df, aes(x = tick / 52, group = RunNumber)) + 
    geom_line(aes(y = Prevalence),linewidth = 0.05, color = "black") +
    geom_point(aes(y=4.5, x = 1), color="red", size = 1) +
    geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 1), color = "red")+
    geom_point(aes(y=4.5, x = 2), color="red", size = 1) +
    geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 2), color = "red")+
    geom_point(aes(y=4.5, x = 3), color="red", size = 1) +
    geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 3), color = "red")+
    geom_point(aes(y=4.5, x = 4), color="red", size = 1) +
    geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 4), color = "red")+
    geom_point(aes(y=4.5, x = 5), color="red", size = 1) +
    geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 5), color = "red")+
    labs(title = title,
         x = "Year",
         y = "Prevalence (%)") +
    theme(plot.title = element_text(size=8)) +
    ylim(0,10)+
    xlim(0,5)+
    theme_bw()
  return(prev)
}

viz_prev_MSM = function(df, title, yearX){
  df <- df %>% filter(tick >= 260)
  prev <- ggplot(data = df, aes(x = (tick / 52)-5, group = RunNumber)) + 
     geom_line(aes(y = prevMSM),size = 0.05, color = "black") +
   
  labs(title = title,
       x = "Year",
       y = "Prevalence (%) in MSM") +
    theme(plot.title = element_text(size=8)) +
    geom_vline(xintercept=yearX, linetype="dashed")+
    coord_cartesian(ylim=c(0,20), xlim=c(5, 30))+
    my_theme
  return(prev)
}

viz_prev_MSM_cal = function(df, title){
  df <- df %>% filter(tick >= 260)
  prev <- ggplot(data = df, aes(x = (tick / 52)-5, group = uniqueID)) + 
    geom_line(aes(y = Prevalence, alpha = resampled),linewidth = 0.05, color = "black") +
    geom_point(aes(y=4.5, x = 1), color="red", size = 1) +
    geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 1), color = "red", size=0.25)+
    geom_point(aes(y=4.5, x = 2), color="red", size = 1) +
    geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 2), color = "red", size=0.25)+
    geom_point(aes(y=4.5, x = 3), color="red", size = 1) +
    geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 3), color = "red", size=0.25)+
    geom_point(aes(y=4.5, x = 4), color="red", size = 1) +
    geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 4), color = "red", size=0.25)+
    geom_point(aes(y=4.5, x = 5), color="red", size = 1) +
    geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 5), color = "red", size=0.25)+
    labs(title = title,
         x = "Year",
         y = "Prevalence (%) in MSM") +
    theme(plot.title = element_text(size=8)) +
    #geom_vline(xintercept=yearX, linetype="dashed")+
    coord_cartesian(ylim=c(0,10), xlim=c(0, 25))+
    my_theme+
    theme(legend.position = "none") +
    scale_alpha(range=c(0.25, 1))+
    annotate("rect", xmin = 0, xmax=5, ymin=-Inf, ymax=Inf, alpha = 0.25)
  return(prev)
}


viz_prev_MSW = function(df, title, yearX){
  df <- df %>% filter(tick > 260)
  prev <- ggplot(data = df, aes(x = tick / 52, group = RunNumber)) + 
    geom_line(aes(y = prevMSW),size = 0.05, color = "black") +
    
    labs(title = title,
         x = "Year",
         y = "Prevalence (%) in MSW") +
    theme(plot.title = element_text(size=8)) +
    geom_vline(xintercept=yearX, linetype="dashed")+
    coord_cartesian(ylim=c(0,20), xlim=c(5, 30))+
    my_theme
  return(prev)
}


viz_prev_W = function(df, title, yearX){
  df <- df %>% filter(tick > 260)
  prev <- ggplot(data = df, aes(x = tick / 52, group = RunNumber)) + 
    geom_line(aes(y = prevW),size = 0.05, color = "black") +
    
    labs(title = title,
         x = "Year",
         y = "Prevalence (%) in W") +
    theme(plot.title = element_text(size=8)) +
    geom_vline(xintercept=yearX, linetype="dashed")+
    coord_cartesian(ylim=c(0,20), xlim=c(5, 30))+
    my_theme
  return(prev)
}



#*
viz_inc = function(df, title, yearX){
  df <- df %>% filter(tick >= 260)
  inc <- ggplot(data = df, aes(x = (tick / 52) - 5, y = Detected, group = RunNumber)) + 
    geom_line( aes(y = Detected, alpha=resampled),size = 0.05, color="black") +
    # geom_point(aes(y=6508, x = 6), color="red", size = 1) +
    # geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 6), color = "red")+
    # geom_point(aes(y=6508, x = 7), color="red", size = 1) +
    # geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 7), color = "red")+
    # geom_point(aes(y=6508, x = 8), color="red", size = 1) +
    # geom_errorbar(aes(ymin = 5206, ymax = 7809,x = 8), color = "red")+
    # geom_point(aes(y=6508, x = 9), color="red", size = 1) +
    # geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 9), color = "red")+
    # geom_point(aes(y=6508, x = 10), color="red", size = 1) +
    # geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 10), color = "red")+
    labs(title = title,
         x = "Year",
         y = "Cases Detected")+
    theme(plot.title = element_text(size=8)) +
    coord_cartesian(ylim= c(0,20000),xlim=c(0,25))+
    geom_vline(xintercept=yearX-5, linetype="dashed")+
  
    my_theme +
    theme(legend.position = "none")+
    scale_alpha(range=c(0.25, 1)) +
    annotate("rect", xmin = 0, xmax=5, ymin=-Inf, ymax=Inf, alpha = 0.25)
  return(inc)
}

viz_true_inc = function(df, title, yearX){
  df <- df %>% filter(tick >= 260)
  inc <- ggplot(data = df, aes(x = (tick / 52) - 5, y = Detected, group = RunNumber)) + 
    geom_line( aes(y = Incidence),size = 0.05, color="black") +
   
    labs(title = title,
         x = "Year",
         y = "Incidence per 100,000 MSM")+
    theme(plot.title = element_text(size=8)) +
    coord_cartesian(ylim= c(0,75000))+
    geom_vline(xintercept=yearX, linetype="dashed")+
    my_theme+
    annotate("rect", xmin = 0, xmax=5, ymin=-Inf, ymax=Inf, alpha = 0.25)
  return(inc)
}

viz_symptomatic = function(df, title){
  df <- df %>% filter(tick > 260)
  symptomatic <- ggplot(data = df, aes(x = (tick / 52) - 5, y = DetectedAndSymptoms / Detected, group = RunNumber)) + 
    geom_line( aes(y = DetectedAndSymptoms / Detected, alpha=resampled),size = 0.1, color="black") +
    geom_point(aes(y=0.679, x = 6), color="red", size = 1) +
    geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 6), color = "red")+
    geom_point(aes(y=0.679, x = 7), color="red", size = 1) +
    geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 7), color = "red")+
    geom_point(aes(y=0.679, x = 8), color="red", size = 1) +
    geom_errorbar(aes(ymin = 0.628, ymax = 0.7265,x = 8), color = "red")+
    geom_point(aes(y=0.679, x = 9), color="red", size = 1) +
    geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 9), color = "red")+
    geom_point(aes(y=0.679, x = 10), color="red", size = 1) +
    geom_errorbar(aes(ymin = 0.628, ymax = 0.7265, x = 10), color = "red")+
    labs(x = "Year",
         y = "Prop. Detected Cases with Symptoms", 
         title = title) +
    scale_y_continuous(limits = c(0.4, 0.9), labels = c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) + #ylim(0.4, 0.9) +
    my_theme +
    theme(legend.position = "none")+
    scale_alpha(range=c(0.25, 1))
  
  return(symptomatic)
}

viz_treatments = function(df, title){
  df <- df %>% filter(tick > 260)
  treatments <- ggplot(data=df, aes(x=tick / 52, group=RunNumber))+
    geom_line(aes(y = Treatments), size = 0.05, color = "black") +
    geom_line(aes(y = FailedTreatments), size = 0.1, color = "red")+
    labs(x = "Year",
         y = "Count Treatments (annually)") +
    ylim(0,16000)+
    my_theme
  
  return(treatments)
}

#*
viz_true_resist_A = function(df, title, yearX){
  df <- df %>% filter(tick >= 260)
  amrA <- ggplot(data = df, aes(x = (tick / 52) - 5, group = RunNumber)) + 
    geom_line( aes(y = ResistAIncidence/Incidence, alpha=resampled),size = 0.05, color="black") +
    labs(x = "Year",
         y = "Prop. cases resistant\ndrug A",
         title = title) +
    coord_cartesian(ylim=c(0.0, 1.0)) +
    geom_vline(xintercept=yearX-5, linetype="dashed")+
    my_theme +
    theme(legend.position = "none")+
    scale_alpha(range=c(0.25, 1)) +
    annotate("rect", xmin = 0, xmax=5, ymin=-Inf, ymax=Inf, alpha = 0.25)
  return(amrA)
}

viz_true_resist_A_mean = function(df, title, yearX){
  df <- df %>% filter(tick >= 260)
  amrA <- ggplot(data = df, aes(x = (tick / 52) - 5, group = RunNumber)) + 
    geom_line( aes(y = ResistAIncidence/Incidence, alpha=resampled),size = 0.05, color="black") +
    stat_summary(aes(y = ResistAIncidence/Incidence, group = counterfactual), fun.y="mean", geom="line", color = "red")+
    labs(x = "Year",
         y = "Prop. cases resistant drug A",
         title = title) +
    coord_cartesian(ylim=c(0.0, 1.0)) +
    geom_vline(xintercept=yearX-5, linetype="dashed")+
    my_theme +
    theme(legend.position = "none")+
    scale_alpha(range=c(0.25, 1)) +
    annotate("rect", xmin = 0, xmax=5, ymin=-Inf, ymax=Inf, alpha = 0.25)
  return(amrA)
}


#*
viz_true_resist_B = function(df, title, yearX){
  df <- df %>% filter(tick >= 260)
  amrB <- ggplot(data = df, aes(x = (tick / 52) - 5, group = RunNumber)) + 
    geom_line( aes(y = ResistBIncidence/Incidence, alpha=resampled),size = 0.05, color="black") +
    labs(x = "Year",
         y = "Prop. cases resistant\ndrug B", 
         title = title) +
    coord_cartesian(ylim=c(0.0, 1.0)) +
    geom_vline(xintercept=yearX-5, linetype="dashed")+
    my_theme +
    theme(legend.position = "none")+
    scale_alpha(range=c(0.25, 1)) +
    annotate("rect", xmin = 0, xmax=5, ymin=-Inf, ymax=Inf, alpha = 0.25)
  return(amrB)
}
#*
viz_true_resist_both = function(df, title, yearX){
  df <- df %>% filter(tick >= 260)
  
  amrB <- ggplot(data = df, aes(x = (tick / 52) - 5, group = RunNumber)) + 
    geom_line( aes(y = ResistBothIncidence/Incidence, alpha=resampled),size = 0.05, color="black") +
    labs(x = "Year",
         y = "Prop. cases resistant\nboth drugs", 
         title = title) +
    coord_cartesian(ylim=c(0.0, 1.0)) +
    geom_vline(xintercept=yearX-5, linetype="dashed")+
    my_theme +
    theme(legend.position = "none")+
    scale_alpha(range=c(0.25, 1)) +
    annotate("rect", xmin = 0, xmax=5, ymin=-Inf, ymax=Inf, alpha = 0.25)
  return(amrB)
}


viz_resist_A_only_failure = function(df, title){
  df <- df %>% filter(tick > 520)
  
  amrA <- ggplot(data = df, aes(x = tick / 52, group = RunNumber)) + 
    geom_line( aes(y = 100* KnownFailedTreatmentsA/Treatments),size = 0.05, color="black") +
    labs(x = "Year",
         y = "% Failures, treatments with A",
         title = title) +
    ylim(0.0, 50) +
    xlim(10, 30)+
    my_theme
  return(amrA)
}

viz_resist_B_only_failure = function(df, title){
  df <- df %>% filter(tick > 520)
  
  amrB <- ggplot(data = df, aes(x = tick / 52, group = RunNumber)) + 
    geom_line( aes(y = KnownFailedTreatmentsB/Treatments * 100),size = 0.05, color="black") +
    labs(x = "Year",
         y = "% Failures, treatments with B", 
         title = title) +
    ylim(0.0, 50) +
    xlim(10, 30)+
    my_theme
  return(amrB)
}

viz_resist_both_failure = function(df, title){
  df <- df %>% filter(tick > 520)
  
  amrB <- ggplot(data = df, aes(x = tick / 52, group = RunNumber)) + 
    geom_line( aes(y = KnownFailedTreatmentsBoth/Treatments* 100),size = 0.05, color="black") +
    labs(x = "Year",
         y = "% Failures, treatments with both A+B", 
         title = title) +
    ylim(0.0, 50) +
    my_theme
  return(amrB)
}

viz_success_A_of_A = function(df, title){
  df <- df %>% filter(tick > 520)
  
  plotA <- ggplot(data = df, aes(x = tick/52, group = RunNumber)) +
    geom_line(aes(y = SuccessTreatmentsA/AttemptTreatmentsA * 100), size = 0.05, color = "black") +
    labs(x = "Year",
         y = "% treatments successful with A", 
         title = title) +
    ylim(0.0, 100) +
    my_theme
  return(plotA)
}

viz_success_B_of_B = function(df, title){
  df <- df %>% filter(tick > 520)
  
  plotB <- ggplot(data = df, aes(x = tick/52, group = RunNumber)) +
    geom_line(aes(y = SuccessTreatmentsB/AttemptTreatmentsB * 100), size = 0.05, color = "black") +
    labs(x = "Year",
         y = "% treatments successful with B", 
         title = title) +
    ylim(0.0, 100) +
    my_theme
  return(plotB)
}

viz_success_X_of_X = function(df, title){
  df <- df %>% filter(tick > 520)
  
  plotX <- ggplot(data = df, aes(x = tick/52, group = RunNumber)) +
    geom_line(aes(y = SuccessTreatmentsX/AttemptTreatmentsX * 100), size = 0.1, color = "black") +
    labs(x = "Year",
         y = "% treatments successful with X", 
         title = title) +
    ylim(0.0, 100) +
    my_theme
  return(plotX)
}


#*
viz_attempts_A = function(df, title, yearX){
  df <- df %>% filter(tick > 260)
  
  plotA <- ggplot(data = df, aes(x = (tick / 52) - 5, group = RunNumber)) +
    geom_line(aes(y = AttemptTreatmentsA, alpha=resampled), size = 0.05, color = "black") +
    labs(x = "Year",
         y = "Count treatments with A", 
         title = title) +
    coord_cartesian(ylim = c(0, 20000))+
    geom_vline(xintercept=yearX, linetype="dashed")+
    my_theme +
    theme(legend.position = "none")+
    scale_alpha(range=c(0.25, 1)) +
    annotate("rect", xmin = 0, xmax=5, ymin=-Inf, ymax=Inf, alpha = 0.25)
  return(plotA)
}
#*
viz_attempts_B = function(df, title, yearX){
  df <- df %>% filter(tick > 260)
  
  plotB <- ggplot(data = df, aes(x = (tick / 52) - 5, group = RunNumber)) +
    geom_line(aes(y = AttemptTreatmentsB, alpha=resampled), size = 0.05, color = "black") +
    labs(x = "Year",
         y = "Count treatments with B", 
         title = title) +
    coord_cartesian(ylim = c(0, 20000))+
    geom_vline(xintercept=yearX, linetype="dashed")+
    my_theme +
    theme(legend.position = "none")+
    scale_alpha(range=c(0.25, 1)) +
    annotate("rect", xmin = 0, xmax=5, ymin=-Inf, ymax=Inf, alpha = 0.25)
  return(plotB)
}
#*
viz_attempts_X = function(df, title, yearX){
  df <- df %>% filter(tick > 260)
  
  plotX <- ggplot(data = df, aes(x = (tick / 52) - 5, group = RunNumber)) +
    geom_line(aes(y = AttemptTreatmentsX, alpha=resampled), size = 0.05, color = "black") +
    labs(x = "Year",
         y = "Count treatments with X", 
         title = title) +
    coord_cartesian(ylim = c(0, 20000))+
    geom_vline(xintercept=yearX-5, linetype="dashed")+
    my_theme +
    theme(legend.position = "none")+
    scale_alpha(range=c(0.25, 1)) +
    annotate("rect", xmin = 0, xmax=5, ymin=-Inf, ymax=Inf, alpha = 0.25)
  return(plotX)
}
#*
viz_E = function(df, title, yearX, ylim){
  df <- df %>% filter(tick >= 260)
  
  plotX <- ggplot(data = df, aes(x = (tick / 52) - 5, group = RunNumber)) +
    geom_line(aes(y = UsageofErtapenem, alpha=resampled), size = 0.05, color = "black") +
    labs(x = "Year",
         y = "Count treatments\nwith ertapenem", 
         title = title) +
    coord_cartesian(ylim = c(0, ylim))+
    geom_vline(xintercept=yearX-5, linetype="dashed")+
    my_theme +
    theme(legend.position = "none")+
    scale_alpha(range=c(0.25, 1)) +
    annotate("rect", xmin = 0, xmax=5, ymin=-Inf, ymax=Inf, alpha = 0.25)
  return(plotX)
}


#viz_success_A_of_all

#viz_success_B_of_all

#viz_success_X_of_all


#*
viz_cost = function(df, title, yearX){
  df <- df %>% filter(tick > 260)
  
  cost <- ggplot(data=df[df$tick!=0,], aes(x=(tick / 52) - 5, group = RunNumber))+
    geom_line(aes(y=AnnualMonetaryCost/1000000, alpha=resampled), size = 0.05, color = "black")+
    labs(title = title, 
         x = "Year",
         y = "Cost in Millions of Dollars (annually)") +
    coord_cartesian(ylim= c(0, 15))+
    #xlim(0,25)+
    geom_vline(xintercept=yearX, linetype="dashed")+
    my_theme +
    theme(legend.position = "none")+
    scale_alpha(range=c(0.25, 1)) +
    annotate("rect", xmin = 0, xmax=5, ymin=-Inf, ymax=Inf, alpha = 0.25)
  return(cost)
}

viz_surveillanceA = function(df, title){
  df <- df %>% filter(tick > 260)
  
  surveillance <- ggplot(data = df, aes(x=(tick / 52) - 5, group = RunNumber))+
    geom_hline(yintercept = 5, linetype = "dashed", color = "red")+
    geom_line(aes(y=100 * SurveillanceEstPropResistA), linewidth = 0.05, color = "black")+
    #geom_line(aes(y=TruePropResist), linewidth = 0.05, color = "red")+
    my_theme+
  labs(title=title,
         y = "% GISP-estimated resistance to A",
         x = "Year")+
    ylim(0, 50)+
    xlim(10, 30)
  return(surveillance)
}

viz_surveillanceB = function(df, title){
  df <- df %>% filter(tick > 520)
  
  surveillance <- ggplot(data = df, aes(x=tick/52, group = RunNumber))+
    geom_hline(yintercept = 5, linetype = "dashed", color = "red")+
    geom_line(aes(y=100 * SurveillanceEstPropResistB), linewidth = 0.05, color = "black")+
    #geom_line(aes(y=TruePropResist), linewidth = 0.05, color = "red")+
    my_theme+
  labs(title=title,
         y = "% GISP-estimated resistance to B",
         x = "Year")+
    ylim(0, 50)+
    xlim(10, 30)
  return(surveillance)
}

viz_surveillanceBoth = function(df, title){
  df <- df %>% filter(tick > 520)
  
  surveillance <- ggplot(data = df, aes(x=tick/52, group = RunNumber))+
    geom_hline(yintercept = 5, linetype = "dashed", color = "red")+
    geom_line(aes(y=100 * SurveillanceEstPropResistBoth), linewidth = 0.05, color = "black")+
    #geom_line(aes(y=TruePropResist), linewidth = 0.05, color = "red")+
    my_theme+
  labs(title=title,
         y = "% GISP-estimated resistance to both A+B ",
         x = "Year")+
    ylim(0, 50)+
    xlim(10, 30)
  return(surveillance)
}


viz_failed = function(df, title){
  df <- df %>% filter(tick > 520)
  
  failed <- ggplot(data = df, aes(x=tick/52, group = RunNumber))+
    geom_line(aes(y=KnownFailedTreatments/Treatments), linewidth = 0.05, color = "blue")+
    geom_line(aes(y=TruePropResist), linewidth = 0.05, color = "red")+
    my_theme+
  labs(title=title)+
    ylim(0, 0.75)+
    xlim(0,20) +
    annotate("rect", xmin = 0, xmax=5, ymin=-Inf, ymax=Inf, alpha = 0.25)
  return(failed)
}

#*
viz_all_failed = function(df, title, yearX){
  df <- df %>% filter(tick >= 260)
  
  df$FailureRate <- calc_failure_rate(df)
  
  failed <- ggplot(data = df, aes(x=(tick / 52) - 5, group = RunNumber))+
    geom_line(aes(y=FailureRate, alpha=resampled), linewidth = 0.05, color = "black")+
    my_theme+
    labs(title=title, x= "Year", y = "Failure rate,\nall treatments")+
    geom_vline(xintercept=yearX-5, linetype="dashed")+
    
   coord_cartesian(ylim=c(0, 1)) +
    theme(legend.position = "none")+
    scale_alpha(range=c(0.25, 1)) +
    annotate("rect", xmin = 0, xmax=5, ymin=-Inf, ymax=Inf, alpha = 0.25)
  return(failed)
}

viz_error = function(df, title){
  df <- df %>% filter(tick > 520)
  
  error <- ggplot(data = df, aes(x=tick/52, group = RunNumber))+
    geom_line(aes(y=ResistEstError), linewidth = 0.05, color = "blue")+
    my_theme+    
    labs(title=title,
         y="Error in Resistance Estimate",
         x="Year")+
    ylim(0, 0.2)+
    xlim(0,20)
  return(error)
}

#*
viz_qaly = function(df, title, yearX){
  df <- df %>% filter(tick > 260)
  
  q <- ggplot(data = df, aes(x = (tick / 52) - 5, y = AnnualQALYsLost, group = RunNumber))+
    my_theme+
    geom_line(aes(alpha=resampled),linewidth = 0.05, color = "black") +
    labs(title = title,
         y = "Annual QALYs Lost per 100,000", 
         x = "Year") +
    geom_vline(xintercept=yearX, linetype="dashed")+
    coord_cartesian(ylim=c(0, 500)) +
    theme(legend.position = "none")+
    scale_alpha(range=c(0.25, 1)) +
    annotate("rect", xmin = 0, xmax=5, ymin=-Inf, ymax=Inf, alpha = 0.25)
  
  
  return(q)
}

###########

#functions for figures with multiple plots: "visualize_"
#################

visualize_calibration = function(df){
  multiplot(viz_prev(df, "A.", 0), viz_inc(df, "B.", 0), viz_symptomatic(df, "C."), cols = 3)
}

visualize_calibration_and_params = function(df){

  dfends <- get_ends(df)
  d <- ggplot(dfends) + 
    geom_histogram(aes(x = Transmission), color = "black", fill = "darkgrey", binwidth = 0.5) +
    labs(x = "Transmission",
         y = "Count", 
         title = "D.") +
    xlim(0, 10)+
    my_theme
  
  e <- ggplot(dfends) + 
    geom_histogram(aes(x = RecoveryLambda), color = "black", fill = "darkgrey", binwidth = 0.2) +
    labs(x = "RecoveryLambda",
         y = "Count", 
         title = "E.") +
    xlim(0, 4)+ 
    my_theme
  
f <- ggplot(dfends) + 
    geom_histogram(aes(x = ProbSymptomatic), color = "black", fill = "darkgrey", binwidth = 0.05) +
    labs(x = "ProbSymptomatic",
         y = "Count", 
         title = "F.") +
    xlim(0, 0.9)+ 
  my_theme

  g <-  ggplot(dfends) + 
    geom_histogram(aes(x = ScreenInterval), color = "black", fill = "darkgrey", binwidth = 0.5) +
    labs(x = "ScreenInterval",
         y = "Count", 
         title = "G.") +
    xlim(0, 5.5)+ 
    my_theme
  
  h <- ggplot(dfends) + 
    geom_histogram(aes(x = DelayToSeekCare), color = "black", fill = "darkgrey", binwidth = 0.002) +
    labs(x = "Delay to seek care (years)",
         y = "Count", 
         title = "H.") +
    #xlim(0, 0.9)+ 
    my_theme
  
  i <-  ggplot(dfends) + 
    geom_histogram(aes(x = DelayToRetreatment), color = "black", fill = "darkgrey", binwidth = 0.005) +
    labs(x = "Delay to seek retreatment (years)",
         y = "Count", 
         title = "I.") +
    #xlim(0, 5.5)+ 
    my_theme
  
  multiplot(viz_prev(df, "A."), 
            d,
            g,
            
            viz_inc(df, "B."), 
            e,
            h,
            
            viz_symptomatic(df, "C."), 
            f,   
            i, 
            
            cols = 3)
}

visualize_basic = function(df){
  multiplot(viz_prev(df, "A.", 0),viz_symptomatic(df, "C."), viz_inc(df, "B.", 0), viz_cost(df, "D."), cols = 2)
}

visualize_prev_and_cost = function(df){
  multiplot(viz_prev(df, "A."),viz_resist_B(df, "C."), viz_resist_A(df, "B."), viz_cost(df, "D."), cols = 2)
  
}

visualize_resistant_cost = function(df){
  multiplot(viz_resist_A(df, "A."), viz_resist_both(df, "C."), viz_resist_B(df, "B."), viz_cost(df, "D."), cols = 2)
  
}

visualize_parameters = function(df){
  n <- 4
  
  max<-15
  min <- 3.5
  unit <- (max-min)/n
  a <- ggplot(df) + 
    geom_histogram(aes(x = TransmissionMSM), color = "black", fill = "darkgrey", binwidth = unit/2, boundary = min) +
    labs(x = "TransmissionMSM",
         y = "Count", 
         title = "A.") +
    scale_x_continuous(breaks = seq(min,max,unit), labels = seq(min,max, unit))+
    geom_vline(xintercept=min, linetype="dashed")+
    geom_vline(xintercept=max, linetype="dashed")+
    theme_bw()

  max<-0.4
  min <- 0.05
  unit <- (max-min)/n
  b <- ggplot(df) + 
    geom_histogram(aes(x = RecoveryLambda), color = "black", fill = "darkgrey", binwidth = unit/2, boundary = min) +
    labs(x = "RecoveryLambda",
         y = "Count", 
         title = "B.") +
    scale_x_continuous(breaks = seq(min,max,unit), labels = seq(min,max, unit))+
    geom_vline(xintercept=min, linetype="dashed")+
    geom_vline(xintercept=max, linetype="dashed")+
    theme_bw()
    
  max<-0.3
  min <- 0.1
  unit <- (max-min)/n
  c <- ggplot(df) + 
    geom_histogram(aes(x = ProbSymptomaticMSM), color = "black", fill = "darkgrey", binwidth = unit/2, boundary = min) +
    labs(x = "ProbSymptomaticMSM",
         y = "Count", 
         title = "C.") +
    scale_x_continuous(breaks = seq(min,max,unit), labels = seq(min,max, unit))+
    geom_vline(xintercept=min, linetype="dashed")+
    geom_vline(xintercept=max, linetype="dashed")+
    theme_bw()
    
  n<-5
  max<-3
  min <- 1.5
  unit <- (max-min)/n
  d <-  ggplot(df) + 
    geom_histogram(aes(x = ScreenIntervalMSM), color = "black", fill = "darkgrey", binwidth = unit/2, boundary = min) +
    labs(x = "ScreenIntervalMSM",
         y = "Count", 
         title = "D.") +
    scale_x_continuous(breaks = seq(min,max,unit), labels = seq(min,max, unit))+
    geom_vline(xintercept=min, linetype="dashed")+
    geom_vline(xintercept=max, linetype="dashed")+
    theme_bw()
  
  n <- 4
  max<-0.04
  min <- 0
  unit <- (max-min)/n
  #Delay to Seek Care
  e <-ggplot(df) + 
    geom_histogram(aes(x = DelayToSeekCareMSM), color = "black", fill = "darkgrey", binwidth = unit/2, boundary = min) +
    labs(x = "DelayToSeekCareMSM",
         y = "Count", 
         title = "E.") +
    scale_x_continuous(breaks = seq(min,max,unit), labels = seq(min,max, unit))+
    geom_vline(xintercept=min, linetype="dashed")+
    geom_vline(xintercept=max, linetype="dashed")+
    theme_bw()
  
  max<-0.08
  min <- 0
  unit <- (max-min)/n
  #Delay to Retreatment
  f<-ggplot(df) + 
    geom_histogram(aes(x = DelayToRetreatmentMSM), color = "black", fill = "darkgrey", binwidth = unit/2, boundary = min) +
    labs(x = "DelayToRetreatmentMSM",
         y = "Count", 
         title = "F.") +
    scale_x_continuous(breaks = seq(min,max,unit), labels = seq(min,max, unit))+
    geom_vline(xintercept=min, linetype="dashed")+
    geom_vline(xintercept=max, linetype="dashed")+
    theme_bw()
  
  #sensitivity
  g<-ggplot(df) + 
    geom_histogram(aes(x = DSTsensitivity), color = "black", fill = "darkgrey", binwidth = 0.005) +
    labs(x = "DSTsensitivity",
         y = "Count", 
         title = "G.") +
    xlim(0.9, 1.0)+ 
    theme_bw()
  
  #specificity
  h<-ggplot(df) + 
    geom_histogram(aes(x = DSTspecificity), color = "black", fill = "darkgrey", binwidth = 0.005) +
    labs(x = "DSTspecificity",
         y = "Count", 
         title = "H.") +
    xlim(0.9, 1.0)+ 
    theme_bw()
  
  
    return(multiplot(a,b,c,d,e,f,g,h, cols = 2))
}

visualize_six_panel = function(df){
  multiplot(
    viz_prev(df, "A."),
    viz_resist_A_only(df, "D."),
    viz_inc(df, "B."),
    viz_resist_B_only(df, "E."),
    viz_symptomatic(df, "C."), 
    viz_resist_both(df, "F."),
    cols = 3
  )
}

visualize_surveillance = function(df){
  multiplot(
    viz_surveillanceA(df, "A."),
    viz_surveillanceB(df, "B."), 
    viz_surveillanceBoth(df, "C."),
    cols = 3
  )
}

visualize_failures_surveillance = function(df){
  multiplot(
    viz_surveillanceA(df, "A."),
    viz_resist_A_only(df, "D."),
    viz_surveillanceB(df, "B."), 
    viz_resist_B_only(df, "E."),
    viz_surveillanceBoth(df, "C."),
    viz_resist_both(df, "F."),
    cols = 3
  )
}

visualize_nine_panel = function(df){
  multiplot(
    viz_prev(df, "A."),
    viz_resist_A_only(df, "D."),
    viz_success_A(df, "G."),
    
    viz_inc(df, "B."),
    viz_resist_B_only(df, "E."),
    viz_success_B(df, "H."),
    
    viz_symptomatic(df, "C."), 
    viz_resist_both(df, "F."),
    viz_cost(df, "I."),
    
    cols = 3
  )
}

compare_everything_four = function(df1, df2, df3, df4){
  
  multiplot(
    viz_prev(df1, "Constant Import"),
    viz_inc(df1, ""),
    viz_true_resist_A(df1, ""), 
    viz_true_resist_B(df1, ""),
    viz_true_resist_both(df1, ""), 
    viz_cost(df1, ""),
    
    viz_prev(df2, "Drop in Once"),
    viz_inc(df2, ""),
    viz_true_resist_A(df2, ""), 
    viz_true_resist_B(df2, ""),
    viz_true_resist_both(df2, ""), 
    viz_cost(df2, ""),
    
    viz_prev(df3, "Convert Once"),
    viz_inc(df3, ""),
    viz_true_resist_A(df3, ""), 
    viz_true_resist_B(df3, ""),
    viz_true_resist_both(df3, ""), 
    viz_cost(df3, ""),
    
    viz_prev(df4, "Develop With Treatment"),
    viz_inc(df4, ""),
    viz_true_resist_A(df4, ""), 
    viz_true_resist_B(df4, ""),
    viz_true_resist_both(df4, ""), 
    viz_cost(df4, ""),
    
    cols = 4)
}


new_figure_four = function(df1, df2, df3, df4, df5, yearX){
  a <- viz_inc(df1, "A. GISP", yearX) + theme(axis.title.x = element_blank())
  f<-viz_true_resist_A(df1, "F.", yearX)+ theme(axis.title.x = element_blank()) 
  k<-viz_true_resist_B(df1, "K.", yearX)+ theme(axis.title.x = element_blank())
  p<-viz_true_resist_both(df1, "P.", yearX)+ theme(axis.title.x = element_blank())
  u<-viz_all_failed(df1, "U.", yearX)+ theme(axis.title.x = element_blank())
  z<-viz_E(df1, "Z.", yearX, 9000)
  
  b<- viz_inc(df2, "B. RT", yearX) + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.x = element_blank())
  g<-viz_true_resist_A(df2, "G.", yearX)+ theme(axis.title.y = element_blank(), axis.text.y = element_blank(),  axis.ticks.y = element_blank(),axis.title.x = element_blank())
  l<-viz_true_resist_B(df2, "L.", yearX)+ theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.x = element_blank())
  q<-viz_true_resist_both(df2, "Q.", yearX)+ theme(axis.title.y = element_blank(), axis.text.y = element_blank(),  axis.ticks.y = element_blank(),axis.title.x = element_blank())
  v<-viz_all_failed(df2, "V.", yearX)+ theme(axis.title.y = element_blank(), axis.text.y = element_blank(),  axis.ticks.y = element_blank(),axis.title.x = element_blank())
  aa<-viz_E(df2, "AA.", yearX, 9000)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank())
  
  
  c<-viz_inc(df3, "C. TOC", yearX)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  h<-viz_true_resist_A(df3, "H.", yearX)+ theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  m<-viz_true_resist_B(df3, "M.", yearX)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  r<-viz_true_resist_both(df3, "R.", yearX)+ theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank())
  w<-viz_all_failed(df3, "W.", yearX)+ theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank())
  bb<-viz_E(df3, "BB.", yearX, 9000)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank())
  
  d<-viz_inc(df4, "D. DST", yearX)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  i<-viz_true_resist_A(df4, "I.", yearX)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  n<-viz_true_resist_B(df4, "N.", yearX)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  s<-viz_true_resist_both(df4, "S.", yearX)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  x<-viz_all_failed(df4, "X.", yearX)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  cc<-viz_E(df4, "CC.", yearX, 9000)+ theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())
  
  e<-viz_inc(df5, "E. RC", yearX)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  j<-viz_true_resist_A(df5, "J.", yearX)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  o<-viz_true_resist_B(df5, "O.", yearX)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  t<-viz_true_resist_both(df5, "T.", yearX)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  y<-viz_all_failed(df5, "Y.", yearX)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  dd<-viz_E(df5, "DD.", yearX, 9000)+ theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())

  
gg<- ggarrange(
   a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x,y, z, aa, bb, cc, dd,
    nrow = 6)

return(gg)
}

burden_isemph = function(df1, df2, df3){
  yearX <- NA
  a <- viz_inc(df1, "A. RT", yearX) + theme(axis.title.x = element_blank())
  q<-viz_all_failed(df1, "D.", yearX)+ theme(axis.title.x = element_blank())
  u<-viz_E(df1, "G.", yearX, 9000)
  
  b<- viz_inc(df2, "B. GISP", yearX) + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.x = element_blank())
  r<-viz_all_failed(df2, "E.", yearX)+ theme(axis.title.y = element_blank(), axis.text.y = element_blank(),  axis.ticks.y = element_blank(),axis.title.x = element_blank())
  v<-viz_E(df2, "H.", yearX, 9000)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank())
  
  
  c<-viz_inc(df3, "C. DST", yearX)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  s<-viz_all_failed(df3, "F.", yearX)+ theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank())
  w<-viz_E(df3, "I.", yearX, 9000)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank())
  

  gg<- ggarrange(
    a, b, c, q, r, s, u, v, w,
    nrow = 3)
  
  return(gg)
}

resist_isemph = function(df1, df2, df3){
  yearX <- NA
  e<-viz_true_resist_A(df1, "A. RT", yearX)+ theme(axis.title.x = element_blank()) 
  i<-viz_true_resist_B(df1, "D.", yearX)+ theme(axis.title.x = element_blank())
  m<-viz_true_resist_both(df1, "G.", yearX)+ theme(axis.title.x = element_blank())
 
  
  f<-viz_true_resist_A(df2, "B. GISP", yearX)+ theme(axis.title.y = element_blank(), axis.text.y = element_blank(),  axis.ticks.y = element_blank(),axis.title.x = element_blank())
  j<-viz_true_resist_B(df2, "E.", yearX)+ theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.x = element_blank())
  n<-viz_true_resist_both(df2, "H.", yearX)+ theme(axis.title.y = element_blank(), axis.text.y = element_blank(),  axis.ticks.y = element_blank(),axis.title.x = element_blank())
 
  g<-viz_true_resist_A(df3, "C. DST", yearX)+ theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  k<-viz_true_resist_B(df3, "F.", yearX)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  o<-viz_true_resist_both(df3, "I.", yearX)+ theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank())
  
  gg<- ggarrange(
    e, f, g, i, j, k, m, n, o,
    nrow = 3)
  
  return(gg)
}



resist_zoom_isemph = function(df1, df2, df3){
  yearX <- NA
  e<-viz_true_resist_A_mean(df1, "A. RT", yearX)+ 
    coord_cartesian(xlim = c(5, 10), ylim=c(0, 0.2)) 
  
  f<-viz_true_resist_A_mean(df2, "B. GISP", yearX)+ theme(axis.title.y = element_blank(), axis.text.y = element_blank(),  axis.ticks.y = element_blank())+
    coord_cartesian(xlim = c(5, 10), ylim=c(0, 0.2))
 
  g<-viz_true_resist_A_mean(df3, "C. DST", yearX)+ theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(),axis.text.y = element_blank())+
    coord_cartesian(xlim = c(5, 10), ylim=c(0, 0.2))
 
  gg<- ggarrange(
    e, f, g, 
    nrow = 1)
  
  return(gg)
}


resist_smdm = function(df1, df2, df3, df4){
  yearX <- NA
  a<-viz_true_resist_A(df1, "A. GISP", yearX)+ theme(axis.title.x = element_blank()) 
  e<-viz_true_resist_B(df1, "E.", yearX)+ theme(axis.title.x = element_blank())
  i<-viz_true_resist_both(df1, "I.", yearX)+ theme(axis.title.x = element_blank())
  
  
  b<-viz_true_resist_A(df2, "B. TOC", yearX)+ theme(axis.title.y = element_blank(), axis.text.y = element_blank(),  axis.ticks.y = element_blank(),axis.title.x = element_blank())
  f<-viz_true_resist_B(df2, "F.", yearX)+ theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.x = element_blank())
  j<-viz_true_resist_both(df2, "J.", yearX)+ theme(axis.title.y = element_blank(), axis.text.y = element_blank(),  axis.ticks.y = element_blank(),axis.title.x = element_blank())
  
  c<-viz_true_resist_A(df3, "C. DST", yearX)+ theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  g<-viz_true_resist_B(df3, "G.", yearX)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  k<-viz_true_resist_both(df3, "K.", yearX)+ theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank())
  
  d <- viz_true_resist_A(df4, "D. RC", yearX)+ theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  h<-viz_true_resist_B(df4, "H.", yearX)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  l<-viz_true_resist_both(df4, "L.", yearX)+ theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank())
  
  gg<- ggarrange(
    a, b, c, d, e, f, g, h, i, j, k, l,
    nrow = 3)
  
  return(gg)
}



resist_zoom_smdm = function(df1, df2, df3){
  yearX <- NA
  e<-viz_true_resist_A_mean(df1, "A. GISP", yearX)+ 
    coord_cartesian(xlim = c(5, 10), ylim=c(0, 0.2)) 
  
  f<-viz_true_resist_A_mean(df2, "B. TOC", yearX)+ theme(axis.title.y = element_blank(), axis.text.y = element_blank(),  axis.ticks.y = element_blank())+
    coord_cartesian(xlim = c(5, 10), ylim=c(0, 0.2))
  
  g<-viz_true_resist_A_mean(df3, "C. DST", yearX)+ theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(),axis.text.y = element_blank())+
    coord_cartesian(xlim = c(5, 10), ylim=c(0, 0.2))
  
  gg<- ggarrange(
    e, f, g, 
    nrow = 1)
  
  return(gg)
}




compare_three_prev_inc = function(df1, df2, df3){
  multiplot(
    viz_prev(df1,"A. GISP"), 
    viz_inc(df1, "D."),
    
    viz_prev(df2,"B. Drug Sus. 100%"), 
    viz_inc(df2, "E."),
    
    viz_prev(df3,"C. Drug Sus. 80%"), 
    viz_inc(df3, "F."),
    
  
    
    cols = 3
  )
}

#*
compare_four_prev_inc = function(yearX, df1, title1, df2, title2, df3, title3, df4, title4){
  multiplot(
    viz_prev(df1, paste("A. ", title1), yearX), 
    viz_inc(df1, "E.", yearX),
    
    viz_prev(df2, paste("B. ", title2), yearX), 
    viz_inc(df2, "F.", yearX),
    
    viz_prev(df3, paste("C. ", title3), yearX), 
    viz_inc(df3, "G.", yearX),
    
    viz_prev(df4, paste("D. ", title4), yearX), 
    viz_inc(df4, "H.", yearX),
    
    cols = 4
  )
}

compare_four_prev_subpops = function(yearX, df1, title1, df2, title2, df3, title3, df4, title4){
  multiplot(
    viz_prev_MSM(df1, paste("A. ", title1), yearX), 
    viz_prev_MSW(df1, "E.", yearX),
    viz_prev_W(df1, "I.", yearX),
    
    viz_prev_MSM(df2, paste("B. ", title2), yearX), 
    viz_prev_MSW(df2, "F.", yearX),
    viz_prev_W(df2, "J.", yearX),
    
    viz_prev_MSM(df3, paste("C. ", title3), yearX), 
    viz_prev_MSW(df3, "G.", yearX),
    viz_prev_W(df3, "K.", yearX),
    
    viz_prev_MSM(df4, paste("D. ", title4), yearX), 
    viz_prev_MSW(df4, "H.", yearX),
    viz_prev_W(df4, "L.", yearX),
    
    cols = 4
  )
}

compare_five = function(df1, title1, df2, title2, df3, title3, df4, title4, df5, title5) {
  a <- viz_inc(df1, paste("A. ", title1), 25) + theme(axis.title.x = element_blank())
  e<-viz_true_resist_A(df1, "E.", 25)+ theme(axis.title.x = element_blank()) 
  i<-viz_true_resist_B(df1, "I.", 25)+ theme(axis.title.x = element_blank())
  m<-viz_true_resist_both(df1, "M.", 25)+ theme(axis.title.x = element_blank())
  q<-viz_all_failed(df1, "Q.", 25)+ theme(axis.title.x = element_blank())
  u<-viz_E(df1, "U.", 25, 9000)
  
  b<- viz_inc(df2, paste("B. ", title2), 25) + theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.x = element_blank())
  f<-viz_true_resist_A(df2, "F.", 25)+ theme(axis.title.y = element_blank(), axis.text.y = element_blank(),  axis.ticks.y = element_blank(),axis.title.x = element_blank())
  j<-viz_true_resist_B(df2, "J.", 25)+ theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.x = element_blank())
  n<-viz_true_resist_both(df2, "N.", 25)+ theme(axis.title.y = element_blank(), axis.text.y = element_blank(),  axis.ticks.y = element_blank(),axis.title.x = element_blank())
  r<-viz_all_failed(df2, "R.", 25)+ theme(axis.title.y = element_blank(), axis.text.y = element_blank(),  axis.ticks.y = element_blank(),axis.title.x = element_blank())
  v<-viz_E(df2, "V.", 25, 9000)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank())
  
  
  c<-viz_inc(df3, paste("C. ", title3), 25)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  g<-viz_true_resist_A(df3, "G.", 25)+ theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  k<-viz_true_resist_B(df3, "K.", 25)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  o<-viz_true_resist_both(df3, "O.", 25)+ theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank())
  s<-viz_all_failed(df3, "S.", 25)+ theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.x = element_blank())
  w<-viz_E(df3, "W.", 25, 9000)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank())
  
  d<-viz_inc(df4, paste("D. ", title4), 25)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  h<-viz_true_resist_A(df4, "H.", 25)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  l<-viz_true_resist_B(df4, "L.", 25)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  p<-viz_true_resist_both(df4, "P.", 25)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  t<-viz_all_failed(df4, "T.", 25)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  x<-viz_E(df4, "X.", 25, 9000)+ theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())
  
  
  dd<-viz_inc(df5, paste("E. ", title5), 25)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  hh<-viz_true_resist_A(df5, "H.", 25)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  ll<-viz_true_resist_B(df5, "L.", 25)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  pp<-viz_true_resist_both(df5, "P.", 25)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  tt<-viz_all_failed(df5, "T.", 25)+ theme(axis.title.y = element_blank(),  axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.title.x = element_blank())
  xx<-viz_E(df5, "S.", 25, 9000)+ theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank())
  
  
  gg<- ggarrange(
    a, b, c, d, dd, e, f, g, h,hh, i, j, k, l,ll, m, n, o, p, pp,q, r, s, t,tt, u, v, w, x,xx,
    nrow = 6)
  
  return(gg)
}

compare_five_prev_inc = function(df1, df2, df3, df4, df5){
  multiplot(
    viz_prev(df1,"A."), 
    viz_inc(df1, "F."),
    
    viz_prev(df2,"B."), 
    viz_inc(df2, "G."),
    
    viz_prev(df3,"C."), 
    viz_inc(df3, "H."),
    
    viz_prev(df4,"D."), 
    viz_inc(df4, "I."),
    
    viz_prev(df5,"E."), 
    viz_inc(df5, "J."),
    
    cols = 5
  )
}

compare_three_resistance = function(df1, df2, df3){
  multiplot(
    viz_true_resist_A(df1, "A. GISP"), 
    viz_true_resist_B(df1, "D."),
    viz_true_resist_both(df1, "G."), 
    
    
    viz_true_resist_A(df2, "B. Drug Sus. 100%"), 
    viz_true_resist_B(df2, "E."),
    viz_true_resist_both(df2, "H."), 
    
    viz_true_resist_A(df3, "C. Drug Sus. 80%"), 
    viz_true_resist_B(df3, "F."),
    viz_true_resist_both(df3, "I."), 
    
    cols = 3)
}

#*
compare_four_resistance = function(yearX, df1, title1, df2, title2, df3, title3, df4, title4){
  multiplot(
            viz_true_resist_A(df1, paste("A.", title1), yearX), 
            viz_true_resist_B(df1, "E.", yearX),
            viz_true_resist_both(df1, "I.", yearX), 

            
            viz_true_resist_A(df2, paste("B.", title2), yearX), 
            viz_true_resist_B(df2, "F.", yearX),
            viz_true_resist_both(df2, "J.", yearX), 

            viz_true_resist_A(df3, paste("C.", title3), yearX), 
            viz_true_resist_B(df3, "G.", yearX),
            viz_true_resist_both(df3, "K.", yearX), 


            viz_true_resist_A(df4, paste("D.", title4), yearX), 
            viz_true_resist_B(df4, "H.", yearX),
            viz_true_resist_both(df4, "L.", yearX), 

            cols = 4)
}


compare_five_resistance = function(df1, df2, df3, df4, df5){
  multiplot(
    viz_true_resist_A(df1, "A."), 
    viz_true_resist_B(df1, "F."),
    viz_true_resist_both(df1, "K."), 
    
    
    viz_true_resist_A(df2, "B."), 
    viz_true_resist_B(df2, "G."),
    viz_true_resist_both(df2, "L."), 
    
    viz_true_resist_A(df3, "C."), 
    viz_true_resist_B(df3, "H."),
    viz_true_resist_both(df3, "M."), 
    
    
    viz_true_resist_A(df4, "D."), 
    viz_true_resist_B(df4, "I."),
    viz_true_resist_both(df4, "N."), 
    

    viz_true_resist_A(df5, "E."), 
    viz_true_resist_B(df5, "J."),
    viz_true_resist_both(df5, "O."), 
    
    cols = 5)
}



compare_counterfact_basic = function(dfGISP, dfTOC, dfrandom) {
  multiplot(
  viz_prev(dfGISP, "A."),
  viz_inc(dfGISP, "D."),
  viz_symptomatic(dfGISP, "G."),
  
  viz_prev(dfTOC, "B."),
  viz_inc(dfTOC, "E."),
  viz_symptomatic(dfTOC, "H."),
  
  viz_prev(dfrandom, "C."),
  viz_inc(dfrandom, "F."),
  viz_symptomatic(dfrandom, "I."),
  
  cols = 3)
  
  #first col, GISP
  #second col, TOC
  # third col, random
  
  #first row, prev
  #second row, inc, 
  #third row, sympt
}

compare_counterfact_resist = function(dfGISP, dfTOC, dfrandom) {
  multiplot(
    viz_true_resist_A(dfGISP, "A."), 
    viz_true_resist_B(dfGISP, "D."),
    viz_true_resist_both(dfGISP, "G."), 
    
    viz_true_resist_A(dfTOC, "B."), 
    viz_true_resist_B(dfTOC, "E."),
    viz_true_resist_both(dfTOC, "H."),
    
    viz_true_resist_A(dfrandom, "C."), 
    viz_true_resist_B(dfrandom, "F."),
    viz_true_resist_both(dfrandom, "I."), 
    cols = 3
  )
  #first col, GISP
  #second col, TOC
  # third col, random
}

compare_counterfact_cost = function(dfGISP, dfTOC, dfrandom) {
  multiplot(
    viz_cost(dfGISP, "A. GISP"),
    viz_qaly(dfGISP, "D."),
    
    viz_cost(dfTOC, "B. Random treatment"), 
    viz_qaly(dfTOC, "E."),
    
    viz_cost(dfrandom, "C. Test-of-cure 80%"),
    viz_qaly(dfrandom, "F."),
    
    cols = 3
  )
  #first col, GISP
  #second col, TOC
  # third col, random
}

#*
compare_four_cost = function(yearX, df1, title1, df2, title2, df3, title3, df4, title4) {
  multiplot(
    viz_cost(df1, paste("A.", title1), yearX),
    viz_qaly(df1, "E.", yearX),
    
    viz_cost(df2, paste("B.", title2), yearX), 
    viz_qaly(df2, "F.", yearX),
    
    viz_cost(df3,paste( "C.", title3), yearX),
    viz_qaly(df3, "G.", yearX),
    
    viz_cost(df4, paste("D.", title4), yearX),
    viz_qaly(df4, "H.", yearX),
    
    cols = 4
  )
}

compare_five_cost = function(df1, title1, df2, title2, df3, title3, df4, title4, df5, title5) {
  yearX <- 20
  multiplot(
    viz_cost(df1, paste("A.", title1), yearX)  +  coord_cartesian(ylim= c(0, 10)),
    viz_qaly(df1, "F.", yearX) +  coord_cartesian(ylim= c(0, 150)),
    
    viz_cost(df2, paste("B.", title2), yearX) +  coord_cartesian(ylim= c(0, 10)), 
    viz_qaly(df2, "G.", yearX)+  coord_cartesian(ylim= c(0, 150)),
    
    viz_cost(df3,paste( "C.", title3), yearX) +  coord_cartesian(ylim= c(0, 10)),
    viz_qaly(df3, "H.", yearX)+  coord_cartesian(ylim= c(0, 150)),
    
    viz_cost(df4, paste("D.", title4), yearX) +  coord_cartesian(ylim= c(0, 10)),
    viz_qaly(df4, "I.", yearX)+  coord_cartesian(ylim= c(0, 150)),
    
    viz_cost(df5, paste("E.", title5), yearX) +  coord_cartesian(ylim= c(0, 10)),
    viz_qaly(df5, "J.", yearX)+  coord_cartesian(ylim= c(0, 150)),
    
    
    cols = 5
  )
}

#*
compare_source_of_cost = function(yearX, df1, title1, df2, title2, df3, title3, df4, title4){
  multiplot(
    viz_all_failed(df1, paste("A.", title1), yearX),
    viz_attempts_A(df1, "E.", yearX),
    viz_attempts_B(df1,"I.", yearX),
    viz_attempts_X(df1,"M.", yearX),
    viz_E(df1, "Q.", yearX),
    
    
    viz_all_failed(df2, paste("B.", title2), yearX),
    viz_attempts_A(df2, "F.", yearX),
    viz_attempts_B(df2, "J.", yearX),
    viz_attempts_X(df2,"N.", yearX),
    viz_E(df2, "R.", yearX),

    viz_all_failed(df3, paste("C.", title3), yearX),
    viz_attempts_A(df3, "G.", yearX),
    viz_attempts_B(df3, "K.", yearX),
    viz_attempts_X(df3, "O.", yearX),
    viz_E(df3, "S.", yearX),

    viz_all_failed(df4, paste("D.", title4), yearX),
    viz_attempts_A(df4, "H.", yearX),
    viz_attempts_B(df4, "L.", yearX),
    viz_attempts_X(df4, "P.", yearX),
    viz_E(df4, "T.", yearX),

    cols = 4
  )
}



visualize_E_avail_X = function(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31,
                               dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31,
                               dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31, 
                               dfDST10, dfDST15, dfDST20, dfDST25, dfDST31){
  multiplot(
  
    #viz_E(dfGISP10, "A. GISP", 10, 100), 
    viz_E(dfGISP15, "A. GISP.", 15, 100),
    viz_E(dfGISP20, "E.", 20, 250),
    viz_E(dfGISP25, "I.", 25, 10000),
    viz_E(dfGISP31, "M.", 30, 10000),
    
    #viz_E(dfrandom10, "B. Random Treatment", 10, 100), 
    viz_E(dfrandom15, "B. Random Treatment", 15, 100),
    viz_E(dfrandom20, "F.", 20, 250),
    viz_E(dfrandom25, "J.", 25, 10000),
    viz_E(dfrandom31, "N.", 30, 10000), 
    
    #viz_E(dfTOC10, "C. Test-of-Cure 80%", 10, 100), 
    viz_E(dfTOC15, "C. TOC", 15, 100),
    viz_E(dfTOC20, "G.", 20, 250),
    viz_E(dfTOC25, "K.", 25, 10000),
    viz_E(dfTOC31, "O.", 30, 10000),
    
    #viz_E(dfDST10, "D. Drug Sus. Testing 80%", 10, 100), 
    viz_E(dfDST15, "D. DST", 15, 100),
    viz_E(dfDST20, "H.", 20, 250),
    viz_E(dfDST25, "L.", 25, 10000),
    viz_E(dfDST31, "P.", 30, 10000),
    
  
  cols = 4)
}


visualize_E_avail_X_abbrv = function(dfGISP20, dfGISP25, dfGISP31,
                               dfrandom20, dfrandom25, dfrandom31,
                               dfTOC20, dfTOC25, dfTOC31, 
                               dfDST20, dfDST25, dfDST31){
  multiplot(
    
    viz_E(dfGISP20, "A. GISP", 20, 1000), 
    viz_E(dfGISP25, "E.", 25, 25000),
    viz_E(dfGISP31, "I.", 30, 100000),
    
    
    viz_E(dfrandom20, "B. Random Treatment", 20, 1000), 
    viz_E(dfrandom25, "F.", 25, 25000),
    viz_E(dfrandom31, "J.", 30, 100000),

    
    viz_E(dfTOC20, "C. Test-of-Cure 80%", 20, 1000), 
    viz_E(dfTOC25, "G.", 25, 25000),
    viz_E(dfTOC31, "K.", 30, 100000),
  
    
    viz_E(dfDST20, "D. Drug Sus. Testing 80%", 20, 1000), 
    viz_E(dfDST25, "H.", 25, 25000),
    viz_E(dfDST31, "L.", 30, 100000),
   
    
    cols = 4)
}

visualize_fail_all = function(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31,
                               dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31,
                               dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31, 
                               dfDST10, dfDST15, dfDST20, dfDST25, dfDST31){
  multiplot(
    
    viz_all_failed(dfGISP15, "A. GISP", 10), 
    viz_all_failed(dfGISP20, "E.", 15),
    viz_all_failed(dfGISP25, "I.", 20),
    viz_all_failed(dfGISP31, "M.", 25),
    #viz_all_failed(dfGISP31, "Q.", 30),
    
    viz_all_failed(dfrandom15, "B. Random Treatment", 10), 
    viz_all_failed(dfrandom20, "F.", 15),
    viz_all_failed(dfrandom25, "J.", 20),
    viz_all_failed(dfrandom31, "N.", 25),
    #viz_all_failed(dfrandom31, "R.", 30), 
    
    viz_all_failed(dfTOC15, "C. Test-of-Cure 80%", 10), 
    viz_all_failed(dfTOC20, "G.", 15),
    viz_all_failed(dfTOC25, "K.", 20),
    viz_all_failed(dfTOC31, "O.", 25),
    #viz_all_failed(dfTOC31, "S.", 30),
    
    viz_all_failed(dfDST15, "D. Drug Sus. Testing 80%", 10), 
    viz_all_failed(dfDST20, "H.", 15),
    viz_all_failed(dfDST25, "L.", 20),
    viz_all_failed(dfDST31, "P.", 25),
    #viz_all_failed(dfDST31, "T.", 30),
    
    
    cols = 4)
}


visualize_prev_all = function(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31,
                              dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31,
                              dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31, 
                              dfDST10, dfDST15, dfDST20, dfDST25, dfDST31){
  multiplot(
    
    #viz_prev(dfGISP10, "A. GISP", 10), 
    viz_prev(dfGISP15, "A. GISP", 10),
    viz_prev(dfGISP20, "E.", 15),
    viz_prev(dfGISP25, "I.", 20),
    viz_prev(dfGISP31, "M.", 25),
    
    #viz_prev(dfrandom10, "B. Random Treatment", 10), 
    viz_prev(dfrandom15, "B. RT", 10),
    viz_prev(dfrandom20, "F.", 15),
    viz_prev(dfrandom25, "J.", 20),
    viz_prev(dfrandom31, "N.", 25), 
    
    #viz_prev(dfTOC10, "C. Test-of-Cure 80%", 10), 
    viz_prev(dfTOC15, "C. TOC", 10),
    viz_prev(dfTOC20, "G.", 15),
    viz_prev(dfTOC25, "K.", 20),
    viz_prev(dfTOC31, "O.", 25),
    
    #viz_prev(dfDST10, "D. Drug Sus. Testing 80%", 10), 
    viz_prev(dfDST15, "D. DST", 10),
    viz_prev(dfDST20, "H.", 15),
    viz_prev(dfDST25, "L.", 20),
    viz_prev(dfDST31, "P.", 25),
    
    
    cols = 4)
}


visualize_inc_all = function(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31,
                               dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31,
                               dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31, 
                               dfDST10, dfDST15, dfDST20, dfDST25, dfDST31){
  multiplot(
    
    viz_true_inc(dfGISP15, "A. GISP", 10), 
    viz_true_inc(dfGISP20, "E.", 15),
    viz_true_inc(dfGISP25, "I.", 20),
    viz_true_inc(dfGISP31, "M.", 25),
    #viz_true_inc(dfGISP31, "Q.", 30),
    
    viz_true_inc(dfrandom15, "B. RT", 10), 
    viz_true_inc(dfrandom20, "F.", 15),
    viz_true_inc(dfrandom25, "J.", 20),
    viz_true_inc(dfrandom31, "N.", 25),
    #viz_true_inc(dfrandom31, "R.", 30), 
    
    viz_true_inc(dfTOC15, "C. TOC", 10), 
    viz_true_inc(dfTOC20, "G.", 15),
    viz_true_inc(dfTOC25, "K.", 20),
    viz_true_inc(dfTOC31, "O.", 25),
    #viz_true_inc(dfTOC31, "S.", 30),
    
    viz_true_inc(dfDST15, "D. DST", 10), 
    viz_true_inc(dfDST20, "H.", 15),
    viz_true_inc(dfDST25, "L.", 20),
    viz_true_inc(dfDST31, "P.", 25),
    #viz_true_inc(dfDST31, "T.", 30),
    
    
    cols = 4)
  
  } 
  
  
visualize_true_resist_A_all = function(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31,
                                         dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31,
                                         dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31, 
                                         dfDST10, dfDST15, dfDST20, dfDST25, dfDST31){
    multiplot(
      
      viz_true_resist_A(dfGISP15, "A. GISP", 10), 
      viz_true_resist_A(dfGISP20, "E.", 15),
      viz_true_resist_A(dfGISP25, "I.", 20),
      viz_true_resist_A(dfGISP31, "M.", 25),
      #viz_true_resist_A(dfGISP31, "Q.", 30),
      
      viz_true_resist_A(dfrandom15, "B. RT", 10), 
      viz_true_resist_A(dfrandom20, "F.", 15),
      viz_true_resist_A(dfrandom25, "J.", 20),
      viz_true_resist_A(dfrandom31, "N.", 25),
      #viz_true_resist_A(dfrandom31, "R.", 30), 
      
      viz_true_resist_A(dfTOC15, "C. TOC", 10), 
      viz_true_resist_A(dfTOC20, "G.", 15),
      viz_true_resist_A(dfTOC25, "K.", 20),
      viz_true_resist_A(dfTOC31, "O.", 25),
      #viz_true_resist_A(dfTOC31, "S.", 30),
      
      viz_true_resist_A(dfDST15, "D. DST", 10), 
      viz_true_resist_A(dfDST20, "H.", 15),
      viz_true_resist_A(dfDST25, "L.", 20),
      viz_true_resist_A(dfDST31, "P.", 25),
      #viz_true_resist_A(dfDST31, "T.", 30),
      
      
      cols = 4)
  }
  
  
visualize_true_resist_B_all = function(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31,
                                       dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31,
                                       dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31, 
                                       dfDST10, dfDST15, dfDST20, dfDST25, dfDST31){
  multiplot(
    
    viz_true_resist_B(dfGISP15, "A. GISP", 10), 
    viz_true_resist_B(dfGISP20, "E.", 15),
    viz_true_resist_B(dfGISP25, "I.", 20),
    viz_true_resist_B(dfGISP31, "M.", 25),
    #viz_true_resist_B(dfGISP31, "Q.", 30),
    
    viz_true_resist_B(dfrandom15, "B. Random Treatment", 10), 
    viz_true_resist_B(dfrandom20, "F.", 15),
    viz_true_resist_B(dfrandom25, "J.", 20),
    viz_true_resist_B(dfrandom31, "N.", 25),
    #viz_true_resist_B(dfrandom31, "R.", 30), 
    
    viz_true_resist_B(dfTOC15, "C. Test-of-Cure 80%", 10), 
    viz_true_resist_B(dfTOC20, "G.", 15),
    viz_true_resist_B(dfTOC25, "K.", 20),
    viz_true_resist_B(dfTOC31, "O.", 25),
    #viz_true_resist_B(dfTOC31, "S.", 30),
    
    viz_true_resist_B(dfDST15, "D. Drug Sus. Testing 80%", 10), 
    viz_true_resist_B(dfDST20, "H.", 15),
    viz_true_resist_B(dfDST25, "L.", 20),
    viz_true_resist_B(dfDST31, "P.", 25),
    #viz_true_resist_B(dfDST31, "T.", 30),
    
    
    cols = 4)
  
}

visualize_true_resist_both_all = function(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31,
                                          dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31,
                                          dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31, 
                                          dfDST10, dfDST15, dfDST20, dfDST25, dfDST31){
  multiplot(
    
    viz_true_resist_both(dfGISP15, "A. GISP", 10), 
    viz_true_resist_both(dfGISP20, "E.", 15),
    viz_true_resist_both(dfGISP25, "I.", 20),
    viz_true_resist_both(dfGISP31, "M.", 25),
    #viz_true_resist_both(dfGISP31, "Q.", 30),
    
    viz_true_resist_both(dfrandom15, "B. RT", 10), 
    viz_true_resist_both(dfrandom20, "F.", 15),
    viz_true_resist_both(dfrandom25, "J.", 20),
    viz_true_resist_both(dfrandom31, "N.", 25),
    #viz_true_resist_both(dfrandom31, "R.", 30), 
    
    viz_true_resist_both(dfTOC15, "C. TOC", 10), 
    viz_true_resist_both(dfTOC20, "G.", 15),
    viz_true_resist_both(dfTOC25, "K.", 20),
    viz_true_resist_both(dfTOC31, "O.", 25),
    #viz_true_resist_both(dfTOC31, "S.", 30),
    
    viz_true_resist_both(dfDST15, "D. DST", 10), 
    viz_true_resist_both(dfDST20, "H.", 15),
    viz_true_resist_both(dfDST25, "L.", 20),
    viz_true_resist_both(dfDST31, "P.", 25),
    #viz_true_resist_both(dfDST31, "T.", 30),
    
    
    cols = 4)}

visualize_attempts_A_all = function(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31,
                                       dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31,
                                       dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31, 
                                       dfDST10, dfDST15, dfDST20, dfDST25, dfDST31){
  multiplot(
    
    viz_attempts_A(dfGISP15, "A. GISP", 10), 
    viz_attempts_A(dfGISP20, "E.", 15),
    viz_attempts_A(dfGISP25, "I.", 20),
    viz_attempts_A(dfGISP31, "M.", 25),
    #viz_attempts_A(dfGISP31, "Q.", 30),
    
    viz_attempts_A(dfrandom15, "B. Random Treatment", 10), 
    viz_attempts_A(dfrandom20, "F.", 15),
    viz_attempts_A(dfrandom25, "J.", 20),
    viz_attempts_A(dfrandom31, "N.", 25),
    #viz_attempts_A(dfrandom31, "R.", 30), 
    
    viz_attempts_A(dfTOC15, "C. Test-of-Cure 80%", 10), 
    viz_attempts_A(dfTOC20, "G.", 15),
    viz_attempts_A(dfTOC25, "K.", 20),
    viz_attempts_A(dfTOC31, "O.", 25),
    #viz_attempts_A(dfTOC31, "S.", 30),
    
    viz_attempts_A(dfDST15, "D. Drug Sus. Testing 80%", 10), 
    viz_attempts_A(dfDST20, "H.", 15),
    viz_attempts_A(dfDST25, "L.", 20),
    viz_attempts_A(dfDST31, "P.", 25),
    #viz_attempts_A(dfDST31, "T.", 30),
    
    
    cols = 4)
}


visualize_attempts_B_all = function(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31,
                                    dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31,
                                    dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31, 
                                    dfDST10, dfDST15, dfDST20, dfDST25, dfDST31){
  multiplot(
    
    viz_attempts_B(dfGISP15, "A. GISP", 10), 
    viz_attempts_B(dfGISP20, "E.", 15),
    viz_attempts_B(dfGISP25, "I.", 20),
    viz_attempts_B(dfGISP31, "M.", 25),
    #viz_attempts_B(dfGISP31, "Q.", 30),
    
    viz_attempts_B(dfrandom15, "B. Random Treatment", 10), 
    viz_attempts_B(dfrandom20, "F.", 15),
    viz_attempts_B(dfrandom25, "J.", 20),
    viz_attempts_B(dfrandom31, "N.", 25),
    #viz_attempts_B(dfrandom31, "R.", 30), 
    
    viz_attempts_B(dfTOC15, "C. Test-of-Cure 80%", 10), 
    viz_attempts_B(dfTOC20, "G.", 15),
    viz_attempts_B(dfTOC25, "K.", 20),
    viz_attempts_B(dfTOC31, "O.", 25),
    #viz_attempts_B(dfTOC31, "S.", 30),
    
    viz_attempts_B(dfDST15, "D. Drug Sus. Testing 80%", 10), 
    viz_attempts_B(dfDST20, "H.", 15),
    viz_attempts_B(dfDST25, "L.", 20),
    viz_attempts_B(dfDST31, "P.", 25),
    #viz_attempts_B(dfDST31, "T.", 30),
    
    
    cols = 4)
}


visualize_attempts_X_all = function(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31,
                                    dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31,
                                    dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31, 
                                    dfDST10, dfDST15, dfDST20, dfDST25, dfDST31){
  multiplot(
    
    viz_attempts_X(dfGISP15, "A. GISP", 15), 
    viz_attempts_X(dfGISP20, "E.", 20),
    viz_attempts_X(dfGISP25, "I.", 25),
    viz_attempts_X(dfGISP31, "M.", 30),
    #viz_attempts_X(dfGISP31, "Q.", 30),
    
    viz_attempts_X(dfrandom15, "B. Random Treatment", 15), 
    viz_attempts_X(dfrandom20, "F.", 20),
    viz_attempts_X(dfrandom25, "J.", 25),
    viz_attempts_X(dfrandom31, "N.", 30),
   # viz_attempts_X(dfrandom31, "R.", 30), 
    
    viz_attempts_X(dfTOC15, "C. Test-of-Cure 80%", 15), 
    viz_attempts_X(dfTOC20, "G.", 20),
    viz_attempts_X(dfTOC25, "K.", 25),
    viz_attempts_X(dfTOC31, "O.", 30),
    #viz_attempts_X(dfTOC31, "S.", 30),
    
    viz_attempts_X(dfDST15, "D. Drug Sus. Testing 80%", 15), 
    viz_attempts_X(dfDST20, "H.", 20),
    viz_attempts_X(dfDST25, "L.", 25),
    viz_attempts_X(dfDST31, "P.", 30),
    #viz_attempts_X(dfDST31, "T.", 30),
    
    
    cols = 4)
}


visualize_cost_all = function(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31,
                              dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31,
                              dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31, 
                              dfDST10, dfDST15, dfDST20, dfDST25, dfDST31){
  multiplot(
    
    viz_cost(dfGISP15, "A. GISP", 10), 
    viz_cost(dfGISP20, "E.", 15),
    viz_cost(dfGISP25, "I.", 20),
    viz_cost(dfGISP31, "M.", 25),
    #viz_cost(dfGISP31, "Q.", 30),
    
    viz_cost(dfrandom15, "B. RT", 10), 
    viz_cost(dfrandom20, "F.", 15),
    viz_cost(dfrandom25, "J.", 20),
    viz_cost(dfrandom31, "N.", 25),
    #viz_cost(dfrandom31, "R.", 30), 
    
    viz_cost(dfTOC15, "C. TOC", 10), 
    viz_cost(dfTOC20, "G.", 15),
    viz_cost(dfTOC25, "K.", 20),
    viz_cost(dfTOC31, "O.", 25),
    #viz_cost(dfTOC31, "S.", 30),
    
    viz_cost(dfDST15, "D. DST", 10), 
    viz_cost(dfDST20, "H.", 15),
    viz_cost(dfDST25, "L.", 20),
    viz_cost(dfDST31, "P.", 25),
    #viz_cost(dfDST31, "T.", 30),
    
    
    cols = 4)
}


visualize_QALYs_all = function(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31,
                              dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31,
                              dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31, 
                              dfDST10, dfDST15, dfDST20, dfDST25, dfDST31){
  multiplot(
    
    viz_qaly(dfGISP15, "A. GISP", 10), 
    viz_qaly(dfGISP20, "E.", 15),
    viz_qaly(dfGISP25, "I.", 20),
    viz_qaly(dfGISP31, "M.", 25),
    #viz_qaly(dfGISP31, "Q.", 30),
    
    viz_qaly(dfrandom15, "B. RT", 10), 
    viz_qaly(dfrandom20, "F.", 15),
    viz_qaly(dfrandom25, "J.", 20),
    viz_qaly(dfrandom31, "N.", 25),
    #viz_qaly(dfrandom31, "R.", 30), 
    
    viz_qaly(dfTOC15, "C. TOC", 10), 
    viz_qaly(dfTOC20, "G.", 15),
    viz_qaly(dfTOC25, "K.", 20),
    viz_qaly(dfTOC31, "O.", 25),
    #viz_qaly(dfTOC31, "S.", 30),
    
    viz_qaly(dfDST15, "D. DST", 10), 
    viz_qaly(dfDST20, "H.", 15),
    viz_qaly(dfDST25, "L.", 20),
    viz_qaly(dfDST31, "P.", 25),
    #viz_qaly(dfDST31, "T.", 30),
    
    
    cols = 4)
}



visualize_calibration_MSM = function(dfcalibrated){
  multiplot(
    viz_prev_MSM_cal(dfcalibrated, "A."),
    viz_incMSM_cal(dfcalibrated, "B."),
  
    viz_sympt_MSM_cal(dfcalibrated, "C."),
  
    cols = 3)
}

visualize_calibration_subpops = function(dfcalibrated){
  multiplot(
  viz_prev_MSM_cal(dfcalibrated, "A. Prevalence in MSM"),
  viz_incMSM_cal(dfcalibrated, "B. Detected Incidence in MSM"),
  viz_incM_cal(dfcalibrated, "C. Detected Incidence, all males"),
  viz_incW_cal(dfcalibrated, "D. Detected Incidence, all females"),
  viz_sympt_MSM_cal(dfcalibrated, "E. Proportion Symptomatic, MSM"),
  viz_sympt_MSW_cal(dfcalibrated, "F. Proportion Symptomatic, MSW"),
  viz_sympt_W_cal(dfcalibrated, "G. Proportion Symptomatic, W") ,
  cols = 2)
}

########

#writes file for re-running calibrated trajectories 
######
#(for input each traj should have a single row, i.e., the data ends)
write_calibrated = function(df){
  
  resampleSeed <- df$seed
  resampleInitialInfected <- df$InitialInfected
  resampleTransmissionMSM <- df$TransmissionMSM
 
  resampleRecoveryLambda <- df$RecoveryLambda
  resampleProbSymptomaticMSM <- df$ProbSymptomaticMSM
 
  resampleScreenIntervalMSM <- df$ScreenIntervalMSM
  
  
  resampleDelayToSeekCareMSM <- df$DelayToSeekCareMSM
 
  resampleDelayToRetreatmentMSM <- df$DelayToRetreatmentMSM

  resamplePercentResistantA <- df$PercentResistantA
  resampleBeginImportingB <- df$BeginImportingB
  resampleImportingBInterval <- df$ImportingBInterval
  resampleDSTsensitivity <- df$DSTsensitivity
  resampleDSTspecifictiy <- df$DSTspecificity
  
  resampleCareCost<-df$CareCost
  resampleTestCost<-df$TestCost
  resampleStrainTestCost<-df$StrainTestCost
  resampleDrugAtreatmentCost<-df$DrugATreatmentCost
  resampleDrugBtreatmentCost<-df$DrugBTreatmentCost
  resampleDrugXtreatmentCost<-df$DrugXTreatmentCost
  resampleDrugEtreatmentCost<-df$DrugETreatmentCost
  
  fwrite(list(resampleSeed), file = "/Users/me597/Documents/MSM_calibrated_params/seed_resample.txt")
  fwrite(list(resampleInitialInfected), file = "/Users/me597/Documents/MSM_calibrated_params/initial_infected_resample.txt")
  fwrite(list(resampleTransmissionMSM), file = "/Users/me597/Documents/MSM_calibrated_params/transmissionMSM_resample.txt")
  fwrite(list(resampleRecoveryLambda), file = "/Users/me597/Documents/MSM_calibrated_params/recovery_lambda_resample.txt")
  fwrite(list(resampleProbSymptomaticMSM), file = "/Users/me597/Documents/MSM_calibrated_params/prob_symptomatic_MSM_resample.txt")
  fwrite(list(resampleScreenIntervalMSM), file = "/Users/me597/Documents/MSM_calibrated_params/screen_interval_MSM_resample.txt")
  fwrite(list(resampleDelayToSeekCareMSM), file = "/Users/me597/Documents/MSM_calibrated_params/delay_to_seek_care_MSM_resample.txt")
  fwrite(list(resampleDelayToRetreatmentMSM), file = "/Users/me597/Documents/MSM_calibrated_params/delay_to_retreatment_MSM_resample.txt")
  fwrite(list(resamplePercentResistantA), file = "/Users/me597/Documents/MSM_calibrated_params/percent_resistant_A_resample.txt")
  fwrite(list(resampleBeginImportingB), file = "/Users/me597/Documents/MSM_calibrated_params/begin_importing_B_resample.txt")
  fwrite(list(resampleImportingBInterval), file = "/Users/me597/Documents/MSM_calibrated_params/importing_B_interval_resample.txt")
  fwrite(list(resampleDSTsensitivity), file = "/Users/me597/Documents/MSM_calibrated_params/DSTsensitivity_resample.txt")
  fwrite(list(resampleDSTspecifictiy), file = "/Users/me597/Documents/MSM_calibrated_params/DSTspecificity_resample.txt")
  
  
  fwrite(list(resampleCareCost), file = "/Users/me597/Documents/MSM_calibrated_params/care_cost_resample.txt")
  fwrite(list(resampleTestCost), file = "/Users/me597/Documents/MSM_calibrated_params/test_cost_resample.txt")
  fwrite(list(resampleStrainTestCost), file = "/Users/me597/Documents/MSM_calibrated_params/strain_test_cost_resample.txt")
  fwrite(list(resampleDrugAtreatmentCost), file = "/Users/me597/Documents/MSM_calibrated_params/drug_a_treatment_cost_resample.txt")
  fwrite(list(resampleDrugBtreatmentCost), file = "/Users/me597/Documents/MSM_calibrated_params/drug_b_treatment_cost_resample.txt")
  fwrite(list(resampleDrugXtreatmentCost), file = "/Users/me597/Documents/MSM_calibrated_params/drug_X_treatment_cost_resample.txt")
  fwrite(list(resampleDrugEtreatmentCost), file = "/Users/me597/Documents/MSM_calibrated_params/drug_E_treatment_cost_resample.txt")
}

#######

#december process and visualize:
########
#load data
df <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_DECEMBER_6_2023_overnight.csv")

df <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_DECEMBER_8_2023_overnightnone.csv", skip =3)

df_ends <- calc_weights(df)
df_best_ends <- best_ends(df_ends)
df_best_traj <- best_traj(df,df_best_ends)

visualize(df_best_traj)

write_calibrated(df_best_ends)




df1 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_DECEMBER_11_2023_1_GISP.csv")
df2 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_DECEMBER_12_2023_2_test-of-cure.csv")
multiplot(visualize_cost(df1, "A,"), visualize_error(df1, "B."), visualize_cost(df2, "C."), visualize_error(df2, "D."), cols = 2)
visualize_basic(df1)
visualize(df2)
df1 <- calc_error(df1)
df2 <- calc_error(df2)

visualize_cost(df_best_traj, "A.")
visualize_cost(df1, "B.")
visualize_cost(df2, "C.")

visualize_prev_and_cost(df_best_traj)
visualize_prev_and_cost(df1)
visualize_prev_and_cost(df2)

visualize_surveillance(df1, "")



df1 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_DECEMBER_13_2023_1_test-of-cure_100.csv")
df2 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_DECEMBER_13_2023_1_test-of-cure_80.csv")
df3 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_DECEMBER_13_2023_1_test-of-cure_50.csv")
visualize_prev_and_cost(df1)
visualize_prev_and_cost(df2)
visualize_prev_and_cost(df3)

df1 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_DECEMBER_13_2023_2_none.csv")
visualize_prev_and_cost(df1)





newdf <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_DECEMBER_14_2023_5_none.csv")
visualize_basic(newdf)
df_ends <- calc_weights(newdf)
df_best_ends <- best_ends(df_ends)
df_best_traj <- best_traj(newdf,df_best_ends)
visualize_basic(df_best_traj)
write_calibrated(df_best_ends)




df1 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_DECEMBER_15_2023_2_GISP.csv")
df2 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_DECEMBER_15_2023_2_test-of-cure_80.csv")
df3 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_DECEMBER_15_2023_2_random.csv")
visualize_prev_and_cost(df1)
visualize_prev_and_cost(df2)
visualize_prev_and_cost(df3)

visualize_resistant_cost(df1)
visualize_resistant_cost(df2)
visualize_resistant_cost(df3)


multiplot(viz_prev_5(df_best_traj, "None"), viz_prev_5(df1, "GISP"), viz_prev_5(df2, "test of cure"), viz_prev_5(df3, "random"), cols = 2)
df_best_ends$AnnualContacts %in% df1$AnnualContacts
df1$AnnualContacts %in% df_best_ends$AnnualContacts
unique(df2$Transmission) %in% df_best_ends$Transmission
unique(df2$InitialInfected) %in% df_best_ends$InitialInfected
unique(df2$RecoveryLambda) %in% df_best_ends$RecoveryLambda
unique(df2$ScreenInterval) %in% df_best_ends$ScreenInterval
unique(df2$ProbSymptomatic) %in% df_best_ends$ProbSymptomatic
unique(df_best_ends$Transmission) %in% df_best_traj$Transmission

df_best_ends$TestCost %in% df1$TestCost
df_best_ends$Transmission %in% df1$Transmission
df_best_ends$Transmission %in% df2$Transmission

df_null <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_DECEMBER_15_2023_3_none.csv")
multiplot(viz_prev_5(df_best_traj, "original"), viz_prev_5(df_null, "rerun"), cols = 2)
unique(df_null$AnnualContacts) %in% df_best_traj$AnnualContacts
multiplot(param_hist(df_best_traj), param_hist(df_null), cols = 2)




#full parameter sweep ending at year 10
df1 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_4_2024_overnight_none.csv")
visualize_basic(df1)
df_ends <- calc_weights(df1)
df_best_ends <- best_ends(df_ends)
df_best_traj <- best_traj(df1,df_best_ends)
visualize_basic(df_best_traj)
df_best_ends <- na.omit(df_best_ends)

visualize_parameters(df_best_ends)

write_calibrated(df_best_ends)

#best trajectories run for 30 years
df2 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_5_2024_1_none.csv")
visualize_basic(df2)

df2_first_10 <- df2 %>% filter(tick < 521)
multiplot(viz_prev(df_best_traj, "A"), viz_prev(df2_first_10, "B"), cols = 2)

#best trajectories w/ resistance dropped in at year 10
df3 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_5_2024_2_none.csv")
visualize_basic(df3)

#GISP
df4 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_10_2024_1_GISP.csv")
visualize_basic(df4)

visualize_six_panel(df2)
visualize_six_panel(df3)
visualize_six_panel(df4)

viz_prev(df3, "")
viz_inc(df3, "")
viz_symptomatic(df3, "")


visualize_nine_panel(df4)

df_weird_success <- df4 %>% filter(tick == 52 * 19)
df_weird_success <- df_weird_success %>% filter(SuccessTreatmentsA != AttemptTreatmentsA)


##TROUBLESHOOTING - rerunning single traj
df_sweep <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_4_2024_debug2_none.csv")
write_calibrated(df_sweep %>% filter(tick ==0))
df_cal <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_3_2024_debug4_none.csv", skip = 23)
multiplot(viz_prev(df_best_traj, "50 best from param sweep"), viz_prev(df2%>%filter(tick < 521), "Calibrated re-run, first 10 years"), cols = 2)
#####


df1 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_11_2024_1_GISP.csv")
df2 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_11_2024_1_test-of-cure_80.csv")
df3 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_11_2024_1_random.csv")

visualize_calibration(df1)

#results figure 1 - compare overall metrics

compare_counterfact_basic(df1, df2, df3)

#results figure 2 - compare resistance

compare_counterfact_resist(df1, df2, df3)

#results figure 3 - compare costs

compare_counterfact_cost(df1, df2, df3)






#comparing the different kinds of resistance insertion under GISP
########
df1 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_15_2024_1_GISP_constantImport.csv")
df2 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_15_2024_1_GISP_dropInOnce.csv")
df3 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_15_2024_1_GISP_convertOnce.csv")
df4 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_15_2024_2_GISP_developWithTreatment.csv")

compare_everything_four = function(df1, df2, df3, df4){
  
  multiplot(
  viz_prev(df1, "Constant Import"),
  viz_inc(df1, ""),
  viz_true_resist_A(df1, ""), 
  viz_true_resist_B(df1, ""),
  viz_true_resist_both(df1, ""), 
  viz_cost(df1, ""),
  
  viz_prev(df2, "Drop in Once"),
  viz_inc(df2, ""),
  viz_true_resist_A(df2, ""), 
  viz_true_resist_B(df2, ""),
  viz_true_resist_both(df2, ""), 
  viz_cost(df2, ""),
  
  viz_prev(df3, "Convert Once"),
  viz_inc(df3, ""),
  viz_true_resist_A(df3, ""), 
  viz_true_resist_B(df3, ""),
  viz_true_resist_both(df3, ""), 
  viz_cost(df3, ""),
  
  viz_prev(df4, "Develop With Treatment"),
  viz_inc(df4, ""),
  viz_true_resist_A(df4, ""), 
  viz_true_resist_B(df4, ""),
  viz_true_resist_both(df4, ""), 
  viz_cost(df4, ""),
  
  cols = 4)
}

compare_everything_four(df1, df2, df3, df4)



########


#new results jan 19th 2024
#########

df1 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_18_2024_overnight_none_none.csv")

df_ends <- calc_weights(df1)
df_best_ends <- best_ends(df_ends)
df_best_traj <- best_traj(df1,df_best_ends)
df_best_ends <- na.omit(df_best_ends)
visualize_basic(df_best_traj)

visualize_parameters(df_best_ends)

write_calibrated(df_best_ends)





df2 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_19_2024_1_none_none.csv")
visualize_calibration(df2)

df3 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_19_2024_1_GISP_dropInOnce.csv")
df4 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_19_2024_1_random_dropInOnce.csv")
df5 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_19_2024_1_test-of-cure_80_dropInOnce.csv")

compare_counterfact_basic(df3, df4, df5)
compare_counterfact_resist(df3, df4, df5)


df6 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_19_2024_2_GISP_constantImport.csv")
df7 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_19_2024_2_GISP_dropInOnce.csv")
df8 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_19_2024_2_GISP_convertOnce.csv")
df9 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_19_2024_2_GISP_developWithTreatment.csv")

compare_everything_four(df6, df7, df8, df9)

#######

#figures for jan ppml meeting
############
df1 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_22_2024_overnight_none_none.csv")
df_ends <- calc_weights(df1)
df_best_ends <- best_ends(df_ends)
df_best_traj <- best_traj(df1,df_best_ends)
df_best_ends <- na.omit(df_best_ends)
visualize_basic(df_best_traj)

visualize_parameters(df_best_ends)

write_calibrated(df_best_ends)

df2 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_22_2024_overnight_none_none.csv", skip=57057)
df3 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_22_2024_overnight_GISP_dropInOnce.csv", skip = 1551)
df4 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_22_2024_overnight_random_dropInOnce.csv")
df5 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_22_2024_overnight_test-of-cure_80_dropInOnce.csv")
df6 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_24_2024_1_GISP_constantImport.csv")
df7 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_22_2024_overnight_GISP_dropInOnce.csv", skip = 1551)
df8 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_22_2024_overnight_GISP_convertOnce.csv")
df9 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_22_2024_overnight_GISP_developWithTreatment.csv")

#slide 8
visualize_calibration(df2)

#slide 10
compare_four_prev_inc(df7, df8, df6, df9)

#slide 11
compare_four_resistance(df7, df8, df6, df9)

#slide 13
compare_three_prev_inc(df4, df3, df5)

#slide 14
compare_three_resistance(df4, df3, df5)

#slide 15
compare_counterfact_cost(df4, df3, df5)

###########


#new combo resistance
######
df1 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_24_2024_overnight_none_none.csv", skip = 1)
df_ends <- calc_weights(df1)
df_best_ends <- best_ends(df_ends)
df_best_traj <- best_traj(df1,df_best_ends)
df_best_ends <- na.omit(df_best_ends)
visualize_basic(df_best_traj)

visualize_parameters(df_best_ends)

write_calibrated(df_best_ends)

df2 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_26_2024_1_none_none.csv")
visualize_calibration(df2)

df3 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_26_2024_1_GISP_combo.csv", skip = 1551)
df4 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_26_2024_1_random_combo.csv")
df5 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_26_2024_1_test-of-cure_80_combo.csv")
df6 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_26_2024_1_GISP_constantImport.csv")
df7 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_26_2024_1_GISP_dropInOnce.csv")
df8 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_26_2024_1_GISP_convertOnce.csv")
df9 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_26_2024_1_GISP_developWithTreatment.csv")

compare_three_prev_inc(df3, df4, df5)
compare_three_resistance(df3, df4, df5)
compare_counterfact_cost(df3, df4, df5)


compare_five_prev_inc(df3, df6, df7, df8, df9)
compare_five_resistance(df3, df6, df7, df8, df9)
######

#with drug susceptibility testing
######
df1 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_26_2024_1_GISP_combo.csv", skip = 1551)
df2 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_26_2024_1_random_combo.csv")
df3 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_26_2024_1_test-of-cure_80_combo.csv")
df4 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_30_2024_2_drug_sus_testing_90_combo.csv")
df5 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_30_2024_2_drug_sus_testing_80_combo.csv")

compare_four_prev_inc(df1, df2, df3, df4)
compare_four_resistance(df1, df2, df3, df4)
compare_four_cost(df1, df2, df3, df4)

df1 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_26_2024_1_GISP_combo.csv", skip = 1551)
df2 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_30_2024_1_drug_sus_testing_combo.csv")
df3 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_30_2024_2_drug_sus_testing_90_combo.csv")
df4 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_30_2024_2_drug_sus_testing_80_combo.csv")

compare_four_prev_inc(df1, "GISP", df2, "Drug Sus. 100%", df3, "Drug Sus. 90%", df4, "Drug Sus. 80%")
compare_four_resistance(df1, df2, df3, df4)

summary <- rbind(calc_summary_stats(df1), calc_summary_stats(df2), calc_summary_stats(df3), calc_summary_stats(df4), calc_summary_stats(df5))

#####


#with stop point removed
############

df1 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_31_2024_2_GISP_combo.csv")
df2 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_31_2024_2_random_combo.csv")
df3 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_31_2024_2_test-of-cure_80_combo.csv")
df4 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_31_2024_2_drug_sus_testing_80_combo.csv")

summary <- rbind(calc_summary_stats(df1), calc_summary_stats(df2), calc_summary_stats(df3), calc_summary_stats(df4))
summary_plot(df1, df2, df3, df4)
summary_plot_color(df1, df2, df3, df4)

compare_four_prev_inc(df1, "GISP", df2, "Randomized Treatment", df3, "Test-of-Cure 80%", df4, "Drug Sus. Testing 80%")
compare_four_resistance(df1, "GISP", df2, "Randomized Treatment", df3, "Test-of-Cure 80%", df4, "Drug Sus. Testing 80%")
compare_source_of_cost(df1, "GISP", df2, "Randomized Treatment", df3, "Test-of-Cure 80%", df4, "Drug Sus. Testing 80%")
compare_four_cost(df1, "GISP", df2, "Randomized Treatment", df3, "Test-of-Cure 80%", df4, "Drug Sus. Testing 80%")


############

#extremes
###########
df1 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_31_2024_2_GISP_combo.csv")
df2 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_5_2024_1_GISP_10_combo.csv")
df3 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_5_2024_1_GISP_50_combo.csv")
df4 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_5_2024_1_GISP_100_combo.csv")

#varying GISP switchpoint
compare_four_prev_inc(df1, "GISP 5%", df2, "GISP 10%", df3, "GISP 50%", df4, "GISP 100%")
compare_four_resistance(df1, "GISP 5%", df2, "GISP 10%", df3, "GISP 50%", df4, "GISP 100%")
compare_source_of_cost(df1, "GISP 5%", df2, "GISP 10%", df3, "GISP 50%", df4, "GISP 100%")
compare_four_cost(df1, "GISP 5%", df2, "GISP 10%", df3, "GISP 50%", df4, "GISP 100%")


#test-of-cure
df1 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_5_2024_1_test-of-cure_00_combo.csv")
df2 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_5_2024_1_test-of-cure_50_combo.csv")
df3 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_31_2024_2_test-of-cure_80_combo.csv")
df4 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_6_2024_1_test-of-cure_100_combo.csv")

compare_four_prev_inc(df1, "TOC 0%", df2, "TOC 50%", df3, "TOC 80%", df4, "TOC 100%")
compare_four_resistance(df1, "TOC 0%", df2, "TOC 50%", df3, "TOC 80%", df4, "TOC 100%")
compare_source_of_cost(df1, "TOC 0%", df2, "TOC 50%", df3, "TOC 80%", df4, "TOC 100%")
compare_four_cost(df1, "TOC 0%", df2, "TOC 50%", df3, "TOC 80%", df4, "TOC 100%")

ToC50_high_B_prev <- df2 %>% filter((ResistBIncidence/Incidence) > 0.1)
ToC80_high_B_prev <- df3 %>% filter((ResistBIncidence/Incidence) > 0.1)
ToC100_high_B_prev <- df4 %>% filter((ResistBIncidence/Incidence) > 0.1)


#DST
df1 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_5_2024_1_drug_sus_testing_00_combo.csv")
df2 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_5_2024_1_drug_sus_testing_50_combo.csv")
df3 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_31_2024_2_drug_sus_testing_80_combo.csv")
df4 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_6_2024_1_drug_sus_testing_100_combo.csv")

compare_four_prev_inc(df1, "DST 0%", df2, "DST 50%", df3, "DST 80%", df4, "DST 100%")
compare_four_resistance(df1, "DST 0%", df2, "DST 50%", df3, "DST 80%", df4, "DST 100%")
compare_source_of_cost(df1, "DST 0%", df2, "DST 50%", df3, "DST 80%", df4, "DST 100%")
compare_four_cost(df1, "DST 0%", df2, "DST 50%", df3, "DST 80%", df4, "DST 100%")


DST50_high_B_prev <- df2 %>% filter((ResistBIncidence/Incidence) > 0.1)
DST80_high_B_prev <- df3 %>% filter((ResistBIncidence/Incidence) > 0.1)
DST100_high_B_prev <- df4 %>% filter((ResistBIncidence/Incidence) > 0.1)



#0s should be identical
dfTOC <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_5_2024_1_test-of-cure_00_combo.csv")
dfDST <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_5_2024_1_drug_sus_testing_00_combo.csv")

multiplot(
  viz_prev(dfTOC, "TOC 0%"),
  viz_true_resist_A(dfTOC, ""),
  viz_cost(dfTOC, ""),
  
  viz_prev(dfDST, "DST 0%"),
  viz_true_resist_A(dfDST, ""),
  viz_cost(dfDST, ""),
  
  cols = 2
)


########

#new calibration figure w/ param values
######
df <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_22_2024_overnight_none_none.csv", skip=57057)
visualize_calibration_and_params(df)
visualize_parameters(df)

df1 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_7_2024_2_GISP_05_combo.csv")
df2 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_7_2024_1_random_combo.csv")
df3 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_31_2024_2_test-of-cure_80_combo.csv")
df4 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_31_2024_2_drug_sus_testing_80_combo.csv")
summary_plot(df1, df2, df3, df4)
summary_plot_color(df1, df2, df3, df4)


compare_four_prev_inc(df1, "GISP", df2, "Randomized Treatment", df3, "Test-of-Cure 80%", df4, "Drug Sus. Testing 80%")
compare_four_resistance(df1, "GISP", df2, "Randomized Treatment", df3, "Test-of-Cure 80%", df4, "Drug Sus. Testing 80%")
compare_source_of_cost(df1, "GISP", df2, "Randomized Treatment", df3, "Test-of-Cure 80%", df4, "Drug Sus. Testing 80%")
compare_four_cost(df1, "GISP", df2, "Randomized Treatment", df3, "Test-of-Cure 80%", df4, "Drug Sus. Testing 80%")
######


#cea
###########
df1 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_7_2024_2_GISP_05_combo.csv")
df2 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_7_2024_1_random_combo.csv")
df3 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_31_2024_2_test-of-cure_80_combo.csv")
df4 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_JANUARY_31_2024_2_drug_sus_testing_80_combo.csv")


compare_four_prev_inc(df1, "GISP", df2, "Randomized Treatment", df3, "Test-of-Cure 80%", df4, "Drug Sus. Testing 80%")
compare_four_resistance(df1, "GISP", df2, "Randomized Treatment", df3, "Test-of-Cure 80%", df4, "Drug Sus. Testing 80%")
compare_source_of_cost(df1, "GISP", df2, "Randomized Treatment", df3, "Test-of-Cure 80%", df4, "Drug Sus. Testing 80%")
compare_four_cost(df1, "GISP", df2, "Randomized Treatment", df3, "Test-of-Cure 80%", df4, "Drug Sus. Testing 80%")


summary_plot(df1, df2, df3, df4)
summary_plot_color(df1, df2, df3, df4)

summary_cost_plot_color(df1, df2, df3, df4)

ceadf_sum <- cea(df1, df2, df3, df4)
lapply(ceadf_sum[], sd)
ceadf_sum <- rbind(ceadf_sum, lapply(ceadf_sum[], mean))
ceadf_sum$seed[51] <- "mean"

ceadf <- rearrange_cea(ceadf_sum)
ceadf[ceadf$seed=="mean",]

visualize_cea(df1, df2, df3, df4)

##########


#availability of drug X
#########
df1 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_14_2024_overnight_none_none_11.csv")
colnames(df1) <- colnames(read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_14_2024_overnight_none_none_31.csv"))

df_ends <- calc_weights(df1)
df_best_ends <- best_ends(df_ends)
df_best_traj <- best_traj(df1,df_best_ends)
df_best_ends <- na.omit(df_best_ends)
visualize_basic(df_best_traj)

visualize_parameters(df_best_ends)

write_calibrated(df_best_ends)



dfGISP10 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_15_2024_1_GISP_05_combo_10.csv")
dfGISP15 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_15_2024_1_GISP_05_combo_15.csv")
dfGISP20 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_15_2024_1_GISP_05_combo_20.csv")
dfGISP25 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_15_2024_1_GISP_05_combo_25.csv")
dfGISP31 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_15_2024_1_GISP_05_combo_31.csv")

dfrandom10 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_15_2024_1_random_combo_10.csv")
dfrandom15 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_15_2024_1_random_combo_15.csv")
dfrandom20 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_15_2024_1_random_combo_20.csv")
dfrandom25 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_15_2024_1_random_combo_25.csv")
dfrandom31 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_15_2024_1_random_combo_31.csv")

dfTOC10 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_15_2024_1_test-of-cure_80_combo_10.csv")
dfTOC15 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_15_2024_1_test-of-cure_80_combo_15.csv")
dfTOC20 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_15_2024_1_test-of-cure_80_combo_20.csv")
dfTOC25 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_15_2024_1_test-of-cure_80_combo_25.csv")
dfTOC31 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_15_2024_1_test-of-cure_80_combo_31.csv")

dfDST10 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_15_2024_1_drug_sus_testing_80_combo_10.csv")
dfDST15 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_15_2024_1_drug_sus_testing_80_combo_15.csv")
dfDST20 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_15_2024_1_drug_sus_testing_80_combo_20.csv")
dfDST25 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_15_2024_1_drug_sus_testing_80_combo_25.csv")
dfDST31 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_15_2024_1_drug_sus_testing_80_combo_31.csv")


multiplot(viz_attempts_X(dfGISP10, "year 10"),viz_attempts_X(dfGISP15, "year 15"),viz_attempts_X(dfGISP20, "year 20"),viz_attempts_X(dfGISP25, "year 25"),viz_attempts_X(dfGISP31, "never"), cols = 5)
#######

#CEA & detailed avail X
######
multiplot(
  visualize_cea("A. ", dfGISP10, dfrandom10, dfTOC10, dfDST10),
  visualize_cea("C.", dfGISP20, dfrandom20, dfTOC20, dfDST20),  
    visualize_cea("E.", dfGISP31, dfrandom31, dfTOC31, dfDST31),

  visualize_cea("B.", dfGISP15, dfrandom15, dfTOC15, dfDST15),
 
  visualize_cea("D.", dfGISP25, dfrandom25, dfTOC25, dfDST25),
  
  
  cols = 2
)

summary_plot(dfGISP10, dfrandom10, dfTOC10, dfDST10)
compare_four_prev_inc(10, dfGISP10, "GISP", dfrandom10, "Randomized Treatment", dfTOC10, "Test-of-Cure 80%", dfDST10, "Drug Sus. Testing 80%")
compare_four_resistance(10, dfGISP10, "GISP", dfrandom10, "Randomized Treatment", dfTOC10, "Test-of-Cure 80%", dfDST10, "Drug Sus. Testing 80%")
compare_source_of_cost(10, dfGISP10, "GISP", dfrandom10, "Randomized Treatment", dfTOC10, "Test-of-Cure 80%", dfDST10, "Drug Sus. Testing 80%")
compare_four_cost(10, dfGISP10, "GISP", dfrandom10, "Randomized Treatment", dfTOC10, "Test-of-Cure 80%", dfDST10, "Drug Sus. Testing 80%")

summary_plot(dfGISP15, dfrandom15, dfTOC15, dfDST15)
compare_four_prev_inc(15, dfGISP15, "GISP", dfrandom15, "Randomized Treatment", dfTOC15, "Test-of-Cure 80%", dfDST15, "Drug Sus. Testing 80%")
compare_four_resistance(15, dfGISP15, "GISP", dfrandom15, "Randomized Treatment", dfTOC15, "Test-of-Cure 80%", dfDST15, "Drug Sus. Testing 80%")
compare_source_of_cost(15, dfGISP15, "GISP", dfrandom15, "Randomized Treatment", dfTOC15, "Test-of-Cure 80%", dfDST15, "Drug Sus. Testing 80%")
compare_four_cost(15, dfGISP15, "GISP", dfrandom15, "Randomized Treatment", dfTOC15, "Test-of-Cure 80%", dfDST15, "Drug Sus. Testing 80%")

summary_plot(dfGISP20, dfrandom20, dfTOC20, dfDST20)
compare_four_prev_inc(20, dfGISP20, "GISP", dfrandom20, "Randomized Treatment", dfTOC20, "Test-of-Cure 80%", dfDST20, "Drug Sus. Testing 80%")
compare_four_resistance(20, dfGISP20, "GISP", dfrandom20, "Randomized Treatment", dfTOC20, "Test-of-Cure 80%", dfDST20, "Drug Sus. Testing 80%")
compare_source_of_cost(20, dfGISP20, "GISP", dfrandom20, "Randomized Treatment", dfTOC20, "Test-of-Cure 80%", dfDST20, "Drug Sus. Testing 80%")
compare_four_cost(20, dfGISP20, "GISP", dfrandom20, "Randomized Treatment", dfTOC20, "Test-of-Cure 80%", dfDST20, "Drug Sus. Testing 80%")


summary_plot(dfGISP25, dfrandom25, dfTOC25, dfDST25)
compare_four_prev_inc(25, dfGISP25, "GISP", dfrandom25, "Randomized Treatment", dfTOC25, "Test-of-Cure 80%", dfDST25, "Drug Sus. Testing 80%")
compare_four_resistance(25, dfGISP25, "GISP", dfrandom25, "Randomized Treatment", dfTOC25, "Test-of-Cure 80%", dfDST25, "Drug Sus. Testing 80%")
compare_source_of_cost(25, dfGISP25, "GISP", dfrandom25, "Randomized Treatment", dfTOC25, "Test-of-Cure 80%", dfDST25, "Drug Sus. Testing 80%")
compare_four_cost(25, dfGISP25, "GISP", dfrandom25, "Randomized Treatment", dfTOC25, "Test-of-Cure 80%", dfDST25, "Drug Sus. Testing 80%")

summary_plot(dfGISP31, dfrandom31, dfTOC31, dfDST31)
compare_four_prev_inc(30, dfGISP31, "GISP", dfrandom31, "Randomized Treatment", dfTOC31, "Test-of-Cure 80%", dfDST31, "Drug Sus. Testing 80%")
compare_four_resistance(30, dfGISP31, "GISP", dfrandom31, "Randomized Treatment", dfTOC31, "Test-of-Cure 80%", dfDST31, "Drug Sus. Testing 80%")
compare_source_of_cost(30, dfGISP31, "GISP", dfrandom31, "Randomized Treatment", dfTOC31, "Test-of-Cure 80%", dfDST31, "Drug Sus. Testing 80%")
compare_four_cost(30, dfGISP31, "GISP", dfrandom31, "Randomized Treatment", dfTOC31, "Test-of-Cure 80%", dfDST31, "Drug Sus. Testing 80%")



compare_four_prev_inc(20, dfGISP20, "GISP", dfrandom20, "Randomized Treatment", dfTOC20, "Test-of-Cure 80%", dfDST20, "Drug Sus. Testing 80%")
compare_four_resistance(20, dfGISP20, "GISP", dfrandom20, "Randomized Treatment", dfTOC20, "Test-of-Cure 80%", dfDST20, "Drug Sus. Testing 80%")
compare_source_of_cost(20, dfGISP20, "GISP", dfrandom20, "Randomized Treatment", dfTOC20, "Test-of-Cure 80%", dfDST20, "Drug Sus. Testing 80%")
compare_four_cost(20, dfGISP20, "GISP", dfrandom20, "Randomized Treatment", dfTOC20, "Test-of-Cure 80%", dfDST20, "Drug Sus. Testing 80%")
visualize_cea("", dfGISP20, dfrandom20, dfTOC20, dfDST20)


summary_plot_color(dfGISP20, dfrandom20, dfTOC20, dfDST20)


ceadf_sum <- cea(dfGISP20, dfrandom20, dfTOC20, dfDST20)
lapply(ceadf_sum[], sd)
ceadf_sum <- rbind(ceadf_sum, lapply(ceadf_sum[], mean))
ceadf_sum$seed[51] <- "mean"

ceadf <- rearrange_cea(ceadf_sum)
ceadf[ceadf$seed=="mean",]


summary <- rbind(calc_summary_stats(dfGISP20), calc_summary_stats(dfrandom20), calc_summary_stats(dfTOC20), calc_summary_stats(dfDST20))


visualize_E_avail_X(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                    dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                    dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                    dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)


giant_summary_plot(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                    dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                    dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                    dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)


multiplot(summary_plot(dfGISP10, dfrandom10, dfTOC10, dfDST10), summary_plot(dfGISP15, dfrandom15, dfTOC15, dfDST15),
          summary_plot(dfGISP20, dfrandom20, dfTOC20, dfDST20), summary_plot(dfGISP25, dfrandom25, dfTOC25, dfDST25),
          summary_plot(dfGISP31, dfrandom31, dfTOC31, dfDST31),
           cols = 1)


##########


#combined for Spearman's
##########

combineddf <- rbind(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                    dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                    dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                    dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

combineddf <- rbind(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31)

combineddf <- rbind(dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31)

combineddf <- rbind(dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31)

combineddf <- rbind(dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

combinedcumulativedf <- cumulative_everything(combineddf)

cor.test(x=combinedcumulativedf$Transmission, y=combinedcumulativedf$Incidence, method='spearman')
cor.test(x=combinedcumulativedf$Transmission, y=combinedcumulativedf$AnnualMonetaryCost, method='spearman')
cor.test(x=combinedcumulativedf$Transmission, y=combinedcumulativedf$AnnualQALYsLost, method='spearman')

cor.test(x=combinedcumulativedf$ScreenInterval, y=combinedcumulativedf$Incidence, method='spearman')
cor.test(x=combinedcumulativedf$ScreenInterval, y=combinedcumulativedf$AnnualMonetaryCost, method='spearman')
cor.test(x=combinedcumulativedf$ScreenInterval, y=combinedcumulativedf$AnnualQALYsLost, method='spearman')

cor.test(x=combinedcumulativedf$ProbSymptomatic, y=combinedcumulativedf$Incidence, method='spearman')
cor.test(x=combinedcumulativedf$ProbSymptomatic, y=combinedcumulativedf$AnnualMonetaryCost, method='spearman')
cor.test(x=combinedcumulativedf$ProbSymptomatic, y=combinedcumulativedf$AnnualQALYsLost, method='spearman')

cor.test(x=combinedcumulativedf$DelayToSeekCare, y=combinedcumulativedf$AnnualQALYsLost, method='spearman')
cor.test(x=combinedcumulativedf$DelayToRetreatment, y=combinedcumulativedf$AnnualQALYsLost, method='spearman')



cor.test(x=combinedcumulativedf$CareCost, y=combinedcumulativedf$AnnualMonetaryCost, method='spearman')
cor.test(x=combinedcumulativedf$TestCost, y=combinedcumulativedf$AnnualMonetaryCost, method='spearman')
cor.test(x=combinedcumulativedf$StrainTestCost, y=combinedcumulativedf$AnnualMonetaryCost, method='spearman')
cor.test(x=combinedcumulativedf$DrugATreatmentCost, y=combinedcumulativedf$AnnualMonetaryCost, method='spearman')
cor.test(x=combinedcumulativedf$DrugBTreatmentCost, y=combinedcumulativedf$AnnualMonetaryCost, method='spearman')
cor.test(x=combinedcumulativedf$DrugXTreatmentCost, y=combinedcumulativedf$AnnualMonetaryCost, method='spearman')
cor.test(x=combinedcumulativedf$DrugETreatmentCost, y=combinedcumulativedf$AnnualMonetaryCost, method='spearman')


plot(x=combinedcumulativedf$ScreenInterval, y=combinedcumulativedf$Incidence)


#######


#all PPML results - feb 2024
##########
df1 <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_23_2024_overnight_none_none_31.csv")

df_ends <- calc_weights(df1)
df_best_ends <- best_ends(df_ends)
df_best_traj <- best_traj(df1,df_best_ends)
df_best_ends <- na.omit(df_best_ends)
visualize_basic(df_best_traj)

visualize_parameters(df_best_ends)

write_calibrated(df_best_ends)


dfcal <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_25_2024_overnight_none_none_10.csv")
visualize_calibration(dfcal)

dfGISP10 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_25_2024_overnight_GISP_05_combo_10.csv")
dfGISP15 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_25_2024_overnight_GISP_05_combo_15.csv")
dfGISP20 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_25_2024_overnight_GISP_05_combo_20.csv")
dfGISP25 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_25_2024_overnight_GISP_05_combo_25.csv")
dfGISP31 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_25_2024_overnight_GISP_05_combo_31.csv")

dfrandom10 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_25_2024_overnight_random_combo_10.csv")
dfrandom15 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_25_2024_overnight_random_combo_15.csv")
dfrandom20 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_25_2024_overnight_random_combo_20.csv")
dfrandom25 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_25_2024_overnight_random_combo_25.csv")
dfrandom31 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_25_2024_overnight_random_combo_31.csv")

dfTOC10 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_25_2024_overnight_test-of-cure_80_combo_10.csv")
dfTOC15 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_25_2024_overnight_test-of-cure_80_combo_15.csv")
dfTOC20 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_25_2024_overnight_test-of-cure_80_combo_20.csv")
dfTOC25 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_25_2024_overnight_test-of-cure_80_combo_25.csv")
dfTOC31 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_25_2024_overnight_test-of-cure_80_combo_31.csv")

dfDST10 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_25_2024_overnight_drug_sus_testing_80_combo_10.csv")
dfDST15 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_25_2024_overnight_drug_sus_testing_80_combo_15.csv")
dfDST20 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_25_2024_overnight_drug_sus_testing_80_combo_20.csv")
dfDST25 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_25_2024_overnight_drug_sus_testing_80_combo_25.csv")
dfDST31 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_25_2024_overnight_drug_sus_testing_80_combo_31.csv")


multiplot(
  visualize_cea("A. ", dfGISP10, dfrandom10, dfTOC10, dfDST10),
  visualize_cea("C.", dfGISP20, dfrandom20, dfTOC20, dfDST20),  
  visualize_cea("E.", dfGISP31, dfrandom31, dfTOC31, dfDST31),
  
  visualize_cea("B.", dfGISP15, dfrandom15, dfTOC15, dfDST15),
  
  visualize_cea("D.", dfGISP25, dfrandom25, dfTOC25, dfDST25),
  
  
  cols = 2
)

summary_plot(dfGISP10, dfrandom10, dfTOC10, dfDST10)
compare_four_prev_inc(10, dfGISP10, "GISP", dfrandom10, "Randomized Treatment", dfTOC10, "Test-of-Cure 80%", dfDST10, "Drug Sus. Testing 80%")
compare_four_resistance(10, dfGISP10, "GISP", dfrandom10, "Randomized Treatment", dfTOC10, "Test-of-Cure 80%", dfDST10, "Drug Sus. Testing 80%")
compare_source_of_cost(10, dfGISP10, "GISP", dfrandom10, "Randomized Treatment", dfTOC10, "Test-of-Cure 80%", dfDST10, "Drug Sus. Testing 80%")
compare_four_cost(10, dfGISP10, "GISP", dfrandom10, "Randomized Treatment", dfTOC10, "Test-of-Cure 80%", dfDST10, "Drug Sus. Testing 80%")

summary_plot(dfGISP15, dfrandom15, dfTOC15, dfDST15)
compare_four_prev_inc(15, dfGISP15, "GISP", dfrandom15, "Randomized Treatment", dfTOC15, "Test-of-Cure 80%", dfDST15, "Drug Sus. Testing 80%")
compare_four_resistance(15, dfGISP15, "GISP", dfrandom15, "Randomized Treatment", dfTOC15, "Test-of-Cure 80%", dfDST15, "Drug Sus. Testing 80%")
compare_source_of_cost(15, dfGISP15, "GISP", dfrandom15, "Randomized Treatment", dfTOC15, "Test-of-Cure 80%", dfDST15, "Drug Sus. Testing 80%")
compare_four_cost(15, dfGISP15, "GISP", dfrandom15, "Randomized Treatment", dfTOC15, "Test-of-Cure 80%", dfDST15, "Drug Sus. Testing 80%")

summary_plot(dfGISP20, dfrandom20, dfTOC20, dfDST20)
compare_four_prev_inc(20, dfGISP20, "GISP", dfrandom20, "Randomized Treatment", dfTOC20, "Test-of-Cure 80%", dfDST20, "Drug Sus. Testing 80%")
compare_four_resistance(20, dfGISP20, "GISP", dfrandom20, "Randomized Treatment", dfTOC20, "Test-of-Cure 80%", dfDST20, "Drug Sus. Testing 80%")
compare_source_of_cost(20, dfGISP20, "GISP", dfrandom20, "Randomized Treatment", dfTOC20, "Test-of-Cure 80%", dfDST20, "Drug Sus. Testing 80%")
compare_four_cost(20, dfGISP20, "GISP", dfrandom20, "Randomized Treatment", dfTOC20, "Test-of-Cure 80%", dfDST20, "Drug Sus. Testing 80%")


summary_plot(dfGISP25, dfrandom25, dfTOC25, dfDST25)
compare_four_prev_inc(25, dfGISP25, "GISP", dfrandom25, "Randomized Treatment", dfTOC25, "Test-of-Cure 80%", dfDST25, "Drug Sus. Testing 80%")
compare_four_resistance(25, dfGISP25, "GISP", dfrandom25, "Randomized Treatment", dfTOC25, "Test-of-Cure 80%", dfDST25, "Drug Sus. Testing 80%")
compare_source_of_cost(25, dfGISP25, "GISP", dfrandom25, "Randomized Treatment", dfTOC25, "Test-of-Cure 80%", dfDST25, "Drug Sus. Testing 80%")
compare_four_cost(25, dfGISP25, "GISP", dfrandom25, "Randomized Treatment", dfTOC25, "Test-of-Cure 80%", dfDST25, "Drug Sus. Testing 80%")

summary_plot(dfGISP31, dfrandom31, dfTOC31, dfDST31)
compare_four_prev_inc(30, dfGISP31, "GISP", dfrandom31, "Randomized Treatment", dfTOC31, "Test-of-Cure 80%", dfDST31, "Drug Sus. Testing 80%")
compare_four_resistance(30, dfGISP31, "GISP", dfrandom31, "Randomized Treatment", dfTOC31, "Test-of-Cure 80%", dfDST31, "Drug Sus. Testing 80%")
compare_source_of_cost(30, dfGISP31, "GISP", dfrandom31, "Randomized Treatment", dfTOC31, "Test-of-Cure 80%", dfDST31, "Drug Sus. Testing 80%")
compare_four_cost(30, dfGISP31, "GISP", dfrandom31, "Randomized Treatment", dfTOC31, "Test-of-Cure 80%", dfDST31, "Drug Sus. Testing 80%")



compare_four_prev_inc(20, dfGISP20, "GISP", dfrandom20, "Randomized Treatment", dfTOC20, "Test-of-Cure 80%", dfDST20, "Drug Sus. Testing 80%")
compare_four_resistance(20, dfGISP20, "GISP", dfrandom20, "Randomized Treatment", dfTOC20, "Test-of-Cure 80%", dfDST20, "Drug Sus. Testing 80%")
compare_source_of_cost(20, dfGISP20, "GISP", dfrandom20, "Randomized Treatment", dfTOC20, "Test-of-Cure 80%", dfDST20, "Drug Sus. Testing 80%")
compare_four_cost(20, dfGISP20, "GISP", dfrandom20, "Randomized Treatment", dfTOC20, "Test-of-Cure 80%", dfDST20, "Drug Sus. Testing 80%")
visualize_cea("", dfGISP20, dfrandom20, dfTOC20, dfDST20)


summary_plot_color(dfGISP20, dfrandom20, dfTOC20, dfDST20)


ceadf_sum <- cea(dfGISP20, dfrandom20, dfTOC20, dfDST20)
lapply(ceadf_sum[], sd)
ceadf_sum <- rbind(ceadf_sum, lapply(ceadf_sum[], mean))
ceadf_sum$seed[51] <- "mean"

ceadf <- rearrange_cea(ceadf_sum)
ceadf[ceadf$seed=="mean",]


summary <- rbind(calc_summary_stats(dfGISP20), calc_summary_stats(dfrandom20), calc_summary_stats(dfTOC20), calc_summary_stats(dfDST20))


visualize_E_avail_X(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                    dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                    dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                    dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)


visualize_fail_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                    dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                    dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                    dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)



visualize_E_avail_X_abbrv(dfGISP20, dfGISP25, dfGISP31, 
                    dfrandom20, dfrandom25, dfrandom31, 
                     dfTOC20, dfTOC25, dfTOC31,
                    dfDST20, dfDST25, dfDST31)



giant_summary_plot(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                   dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                   dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                   dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)


multiplot(summary_plot(dfGISP10, dfrandom10, dfTOC10, dfDST10), summary_plot(dfGISP15, dfrandom15, dfTOC15, dfDST15),
          summary_plot(dfGISP20, dfrandom20, dfTOC20, dfDST20), summary_plot(dfGISP25, dfrandom25, dfTOC25, dfDST25),
          summary_plot(dfGISP31, dfrandom31, dfTOC31, dfDST31),
          cols = 1)





##############

#NMB
#########

multiplot(
  visualize_cea("A.", dfGISP10, dfrandom10, dfTOC10, dfDST10)+theme(legend.position = "none"),
  visualize_cea("C.", dfGISP15, dfrandom15, dfTOC15, dfDST15)+theme(legend.position = "none"),
  visualize_cea("E.", dfGISP20, dfrandom20, dfTOC20, dfDST20)+theme(legend.position = "none"),  
  #visualize_cea("G.", dfGISP25, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none"),
  #visualize_cea("I.", dfGISP31, dfrandom31, dfTOC31, dfDST31)+theme(legend.position = "none"),
  
  
  nmb(cea(dfGISP10, dfrandom10, dfTOC10, dfDST10), "B."),
  nmb(cea(dfGISP15, dfrandom15, dfTOC15, dfDST15), "D."),
  nmb(cea(dfGISP20, dfrandom20, dfTOC20, dfDST20), "F."),
  #nmb(cea(dfGISP25, dfrandom25, dfTOC25, dfDST25), "H."),
  #nmb(cea(dfGISP31, dfrandom31, dfTOC31, dfDST31), "J."),
  
  cols = 2
)

multiplot(
  visualize_cea("G.", dfGISP25, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none"),
  visualize_cea("I.", dfGISP31, dfrandom31, dfTOC31, dfDST31)+theme(legend.position = "none"),
  
  nmb(cea(dfGISP25, dfrandom25, dfTOC25, dfDST25), "H."),
  nmb(cea(dfGISP31, dfrandom31, dfTOC31, dfDST31), "J."),
  cols=2
)


nmb(cea(dfGISP10, dfrandom10, dfTOC10, dfDST10), "A.")
nmb(cea(dfGISP15, dfrandom15, dfTOC15, dfDST15), "B.")
nmb(cea(dfGISP20, dfrandom20, dfTOC20, dfDST20), "C.")
nmb(cea(dfGISP25, dfrandom25, dfTOC25, dfDST25), "D.")
nmb(cea(dfGISP31, dfrandom31, dfTOC31, dfDST31), "E.")



#######

#MS figures from late feb data
############

#figure 4: calibration targets without AMR over 30 years
dfcal <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_25_2024_overnight_none_none_10.csv")
visualize_calibration(dfcal)

#figure 5: summary plot of cumulative outcomes (inc, failures, X)
tiff("Figure_5.tiff", units="mm", width=180, height=210, res=200)
giant_summary_plot(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                   dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                   dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                   dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)
dev.off()

#figure 6:prevalence
tiff("Figure_6.tiff", units="mm", width=220, height=260, res=200)
visualize_prev_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                   dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                   dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                   dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)
dev.off()

#figure 7: incidence
tiff("Figure_7.tiff", units="mm",  width=220, height=260, res=200)
visualize_inc_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                   dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                   dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                   dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)
dev.off()

#figure 8: prop resistant to A
tiff("Figure_8.tiff", units="mm",  width=220, height=260, res=200)
visualize_true_resist_A_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                  dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                  dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                  dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)
dev.off()

#figure 9: proportion resistant to B
tiff("Figure_9.tiff", units="mm",  width=220, height=260, res=200)
visualize_true_resist_B_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                  dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                  dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                  dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)
dev.off()

#figure 10: proportion resistant to both A and B
tiff("Figure_10.tiff", units="mm",  width=220, height=260, res=200)
visualize_true_resist_both_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                  dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                  dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                  dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)
dev.off()

#figure 11: failure 
tiff("Figure_11.tiff", units="mm", width=220, height=260, res=200)
visualize_fail_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                   dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                   dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                   dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)
dev.off()


#figure 12: treatments with A
tiff("Figure_12.tiff", units="mm", width=220, height=260, res=200)
visualize_attempts_A_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                         dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                         dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                         dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)
dev.off()

# figure 13: treatments with B
tiff("Figure_13.tiff", units="mm", width=220, height=260, res=200)
visualize_attempts_B_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                         dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                         dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                         dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)
dev.off()

#figure 14: treatments with X
tiff("Figure_14.tiff", units="mm", width=220, height=260, res=200)
visualize_attempts_X_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                         dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                         dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                         dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)
dev.off()

#figure 15: treatments with E
tiff("Figure_15.tiff", units="mm", width=220, height=260, res=200)
visualize_E_avail_X(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                    dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                    dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                    dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)
dev.off()

#figure 16: cost in dollars
tiff("Figure_16.tiff", units="mm", width=220, height=260, res=200)
visualize_cost_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                   dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                   dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                   dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)
dev.off()

#figure 17: QALYs lost
tiff("Figure_17.tiff", units="mm", width=220, height=260, res=200)
visualize_QALYs_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                   dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                   dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                   dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)
dev.off()

#figure 18: CEA
tiff("Figure_18.tiff", units="mm", width=160, height=340, res=200)
multiplot(
  visualize_cea("A.", dfGISP10, dfrandom10, dfTOC10, dfDST10)+theme(legend.position = "none"),
  visualize_cea("C.", dfGISP15, dfrandom15, dfTOC15, dfDST15)+theme(legend.position = "none"),
  visualize_cea("E.", dfGISP20, dfrandom20, dfTOC20, dfDST20)+theme(legend.position = "none"),  
  visualize_cea("G.", dfGISP25, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none"),
  visualize_cea("I.", dfGISP31, dfrandom31, dfTOC31, dfDST31)+theme(legend.position = "none"),
  nmb(cea(dfGISP10, dfrandom10, dfTOC10, dfDST10), "B."),
  nmb(cea(dfGISP15, dfrandom15, dfTOC15, dfDST15), "D."),
  nmb(cea(dfGISP20, dfrandom20, dfTOC20, dfDST20), "F."),
  nmb(cea(dfGISP25, dfrandom25, dfTOC25, dfDST25), "H."),
  nmb(cea(dfGISP31, dfrandom31, dfTOC31, dfDST31), "J."),
  cols = 2
)
dev.off()
######



#Partial rank correlation coef analysis
#####

combine_avail = function(df10, df15, df20, df25, df31){
  return(rbind(df10, df15, df20, df25, df31))
}

prcc_arrange = function(df, outcome_col){
  #for PRCC, each outcome needs its own df
  #the only columns should be parameters for analysis + the one outcome
  df <- get_ends(df)
  
  new_df <- df[,4:20]
  
  new_df <- cbind(new_df, df[,outcome_col])
  
  return(new_df)
}

prcc = function(df, outcome_col){
  epi.prcc(prcc_arrange(df, outcome_col))
}

prcc_prev = function(df10, df15, df20, df25, df31){
  return(prcc(combine_avail(df10, df15, df20, df25, df31), 22))
}

prcc_inc = function(df10, df15, df20, df25, df31){
  return(prcc(combine_avail(df10, df15, df20, df25, df31), 23))}

prcc_sympt = function(df10, df15, df20, df25, df31){
  return(prcc(combine_avail(df10, df15, df20, df25, df31), 30))}

prcc_cost = function(df10, df15, df20, df25, df31){
  return(prcc(combine_avail(df10, df15, df20, df25, df31), 52))}

prcc_qalys = function(df10, df15, df20, df25, df31){
  return(prcc(combine_avail(df10, df15, df20, df25, df31), 53))}


prcc_all = function(df10, df15, df20, df25, df31){
  prevalence <- prcc_prev(df10, df15, df20, df25, df31)
  incidence <- prcc_inc(df10, df15, df20, df25, df31)
  symptomprop <- prcc_sympt(df10, df15, df20, df25, df31)
  monetaryCost <- prcc_cost(df10, df15, df20, df25, df31)
  qalys <- prcc_qalys(df10, df15, df20, df25, df31)
  return(list(prevalance=prevalence, incidence=incidence, symptomprop=symptomprop, monetaryCost=monetaryCost, QALYs=qalys))
}

GISPallPRCC <- prcc_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31)
RandomallPRCC <- prcc_all(dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31)
TOCallPRCC <- prcc_all(dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31)
DSTallPRCC <- prcc_all(dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

write.csv(GISPallPRCC$prevalance, file = "PRCCGISPPrevalence.csv")
write.csv(GISPallPRCC$incidence, file = "PRCCGISPIncidence.csv")
write.csv(GISPallPRCC$symptomprop, file = "PRCCGISPSymptProp.csv")
write.csv(GISPallPRCC$monetaryCost, file = "PRCCGISPMonetaryCost.csv")
write.csv(GISPallPRCC$QALYs, file = "PRCCGISPQALYs.csv")

write.csv(RandomallPRCC$prevalance, file = "PRCCRandomPrevalence.csv")
write.csv(RandomallPRCC$incidence, file = "PRCCRandomIncidence.csv")
write.csv(RandomallPRCC$symptomprop, file = "PRCCRandomSymptProp.csv")
write.csv(RandomallPRCC$monetaryCost, file = "PRCCRandomMonetaryCost.csv")
write.csv(RandomallPRCC$QALYs, file = "PRCCRandomQALYs.csv")

write.csv(TOCallPRCC$prevalance, file = "PRCCTOCPrevalence.csv")
write.csv(TOCallPRCC$incidence, file = "PRCCTOCIncidence.csv")
write.csv(TOCallPRCC$symptomprop, file = "PRCCTOCSymptProp.csv")
write.csv(TOCallPRCC$monetaryCost, file = "PRCCTOCMonetaryCost.csv")
write.csv(TOCallPRCC$QALYs, file = "PRCCTOCQALYs.csv")


write.csv(DSTallPRCC$prevalance, file = "PRCCDSTPrevalence.csv")
write.csv(DSTallPRCC$incidence, file = "PRCCDSTIncidence.csv")
write.csv(DSTallPRCC$symptomprop, file = "PRCCDSTSymptProp.csv")
write.csv(DSTallPRCC$monetaryCost, file = "PRCCDSTMonetaryCost.csv")
write.csv(DSTallPRCC$QALYs, file = "PRCCDSTQALYs.csv")








#######


#april 10 - honing down main text figures
############

#figure 4: calibration targets without AMR over 30 years
dfcal <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_25_2024_overnight_none_none_10.csv")
visualize_calibration(dfcal)

#figure 5: when drug X is available in year 25, prev, incidence, resistance
new_figure_five(dfGISP25, dfrandom25, dfTOC25, dfDST25)


#figure 6: summary for only when drug X available in year 25




# figure 7: CEA figure, when drug X avail in year 25




#supplemental figures show details on different availabilities of drug X

#############


gfg = seq(0, 1, by = 0.1)

# Plotting the beta density
plot(gfg, dbeta(gfg, 0.6,0.1), xlab="X",
     ylab = "Beta Density", type = "l",
     col = "Red")




#April 2024 new results w sub-populations
#########
dfsweep <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_APRIL_26_2024_overnight_none_none_31.csv")



viz_prev(dfsweep, "Sweep", 10)


df_ends <- calc_weights_subpops(dfsweep)
df_best_ends <- best_ends(df_ends)
df_best_traj <- best_traj(dfsweep,df_best_ends)
#df_best_ends <- na.omit(df_best_ends)
visualize_basic(df_best_traj)

visualize_parameters(df_best_ends)

write_calibrated(df_best_ends)

dfcalibrated  <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_APRIL_26_2024_overnight_none_none_10.csv")

visualize_calibration_subpops(dfcalibrated)


dfGISP10 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_APRIL_26_2024_overnight_GISP_05_combo_10.csv")
dfGISP15 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_APRIL_26_2024_overnight_GISP_05_combo_15.csv")
dfGISP20 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_APRIL_26_2024_overnight_GISP_05_combo_20.csv")
dfGISP25 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_APRIL_26_2024_overnight_GISP_05_combo_25.csv")
dfGISP31 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_APRIL_26_2024_overnight_GISP_05_combo_31.csv")

dfrandom10 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_APRIL_26_2024_overnight_random_combo_10.csv")
dfrandom15 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_APRIL_26_2024_overnight_random_combo_15.csv")
dfrandom20 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_APRIL_26_2024_overnight_random_combo_20.csv")
dfrandom25 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_APRIL_26_2024_overnight_random_combo_25.csv")
dfrandom31 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_APRIL_26_2024_overnight_random_combo_31.csv")

dfTOC10 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_APRIL_26_2024_overnight_test-of-cure_80_combo_10.csv")
dfTOC15 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_APRIL_26_2024_overnight_test-of-cure_80_combo_15.csv")
dfTOC20 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_APRIL_26_2024_overnight_test-of-cure_80_combo_20.csv")
dfTOC25 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_APRIL_26_2024_overnight_test-of-cure_80_combo_25.csv")
dfTOC31 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_APRIL_26_2024_overnight_test-of-cure_80_combo_31.csv")

dfDST10 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_APRIL_26_2024_overnight_drug_sus_testing_80_combo_10.csv")
dfDST15 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_APRIL_26_2024_overnight_drug_sus_testing_80_combo_15.csv")
dfDST20 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_APRIL_26_2024_overnight_drug_sus_testing_80_combo_20.csv")
dfDST25 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_APRIL_26_2024_overnight_drug_sus_testing_80_combo_25.csv")
dfDST31 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_APRIL_26_2024_overnight_drug_sus_testing_80_combo_31.csv")


compare_four_prev_subpops(25, dfGISP25, "GISP", dfrandom25, "Random", dfTOC25, "Test of Cure", dfDST25, "Drug Sus. Testing")


#########

#May 2024 new results w sub-pops
##########
dfsweep <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_MAY_01_2024_overnight_none_none_31.csv")



#viz_prev(dfsweep, "Sweep", 10)


df_ends <- calc_weights_subpops(dfsweep)
df_best_ends <- best_ends(df_ends, 200)
df_best_traj <- best_traj(dfsweep,df_best_ends)
#df_best_ends <- na.omit(df_best_ends)
visualize_basic(df_best_traj)

visualize_parameters(df_best_ends)

write_calibrated(df_best_ends)


dfcalibrated  <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_MAY_3_2024_1_none_none_10.csv")

visualize_calibration_subpops(dfcalibrated)


#testing
df_ends <- calc_weights_subpops(dfsweep)
df_best_ends <- best_ends(df_ends, 5)
write_calibrated(df_best_ends)





dfsweep <- read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_MAY_12_2024_test2_none_none_31.csv")
dfsweep <- rbind(dfsweep, read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_MAY_13_2024_overnight_none_none_31.csv"))

df_ends <- calc_weights_subpops(dfsweep)
df_best_ends <- best_ends(df_ends, 200)
df_best_traj <- best_traj(dfsweep,df_best_ends)
#df_best_ends <- na.omit(df_best_ends)
visualize_calibration_MSM(df_best_traj)

visualize_parameters(df_best_ends)

write_calibrated(df_best_ends)

dfcalibrated  <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_MAY_14_2024_1_none_none_10.csv")

visualize_calibration_subpops(dfcalibrated)


dfGISP25 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_MAY_14_2024_1_GISP_05_combo_25.csv")
dfrandom25 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_MAY_14_2024_1_random_combo_25.csv")
dfTOC25 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_MAY_14_2024_1_test-of-cure_80_combo_25.csv")
dfDST25 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_MAY_14_2024_1_drug_sus_testing_80_combo_25.csv")

visualize_cea("A.", dfGISP25, dfrandom25, dfTOC25, dfDST25)

visualize_cea_MSM("A.", dfGISP25, dfrandom25, dfTOC25, dfDST25)

ceaMSM(dfGISP25, dfrandom25, dfTOC25, dfDST25)



##########



#SMDM abstract 
############

dfGISP25 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_25_2024_overnight_GISP_05_combo_25.csv")
dfrandom25 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_25_2024_overnight_random_combo_25.csv")
dfTOC25 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_25_2024_overnight_test-of-cure_80_combo_25.csv")
dfDST25 <-  read.csv("/Users/me597/Documents/output/SimpleSIR_custom_output_FEBRUARY_25_2024_overnight_drug_sus_testing_80_combo_25.csv")

summary <- rbind(calc_summary_stats(dfGISP25), calc_summary_stats(dfrandom25), calc_summary_stats(dfTOC25), calc_summary_stats(dfDST25))
nmb(cea(dfGISP25, dfrandom25, dfTOC25, dfDST25), "A.")
smdm_summary_plot_color(dfGISP25, dfrandom25, dfTOC25, dfDST25)






#############



#May 2024 scaled-back MSM only model
#################

dfsweep <- read.csv("/Users/me597/Documents/MSMoutput/output_MAY_30_2024_overnight_sweep_none_0/sweepnone0supercombined.csv")
dfsweep$uniqueID <- as.integer(paste(as.character(dfsweep$RunNumber), as.character(dfsweep$seed), sep=''))
df_ends <- calc_weights(dfsweep)
df_best_ends <- best_ends(df_ends, 1000)
df_best_traj <- best_traj(dfsweep,df_best_ends)
visualize_calibration_MSM(df_best_traj)
visualize_parameters(df_best_ends)
write_calibrated(df_best_ends)


dfcalibrated  <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_1_none_none_10/nonenone101combined.csv")
visualize_calibration_MSM(dfcalibrated)


dfGISP25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_overnight_all_combo_10/GISP_05combo251combined.csv")
dfrandom25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_overnight_all_combo_10/randomcombo251combined.csv")

dfTOC25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_overnight_all_combo_10/test-of-cure_80combo251combined.csv")
dfDST25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_overnight_all_combo_10/drug_sus_testing_80combo251combined.csv")

new_figure_five(dfGISP25, dfrandom25, dfTOC25, dfDST25)

smdm_summary_plot_color(dfGISP25, dfrandom25, dfTOC25, dfDST25)

compare_four_cost(25, dfGISP25, "GISP",  dfrandom25, "random", dfTOC25, "TOC", dfDST25, "DST")
compare_four_resistance(25, dfGISP25, "GISP",  dfrandom25, "random", dfTOC25, "TOC", dfDST25, "DST")

multiplot(
  visualize_cea("A.", dfGISP25, dfrandom25, dfTOC25, dfDST25),#+theme(legend.position = "none"),
  nmb(cea(dfGISP25, dfrandom25, dfTOC25, dfDST25), "B."),
  cols = 2
)

ceadf_sum <- cea(dfGISP25, dfrandom25, dfTOC25, dfDST25)
ceadf_sum <- rbind(ceadf_sum, lapply(ceadf_sum[], mean))
ceadf_sum$seed[51] <- "mean"

ceadf <- rearrange_cea(ceadf_sum)

multiplot(
viz_E(dfGISP25, "A. GISP", 25, 50000),
viz_E(dfrandom25, "B. Random", 25, 50000),
viz_E(dfTOC25, "C. TOC", 25, 50000),
viz_E(dfDST25, "D. DST", 25, 50000), cols = 2)



dfGISP10 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_1_all_combo_10/GISP_05combo101combined.csv")
dfGISP15 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_1_all_combo_10/GISP_05combo151combined.csv")
dfGISP20 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_1_all_combo_10/GISP_05combo201combined.csv")
dfGISP25 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_1_all_combo_10/GISP_05combo251combined.csv")
dfGISP31 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_1_all_combo_10/GISP_05combo311combined.csv")

dfrandom10 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_1_all_combo_10/randomcombo101combined.csv")
dfrandom15 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_1_all_combo_10/randomcombo151combined.csv")
dfrandom20 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_1_all_combo_10/randomcombo201combined.csv")
dfrandom25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_1_all_combo_10/randomcombo251combined.csv")
dfrandom31 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_1_all_combo_10/randomcombo311combined.csv")

dfTOC10 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_1_all_combo_10/test-of-cure_80combo101combined.csv")
dfTOC15 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_1_all_combo_10/test-of-cure_80combo151combined.csv")
dfTOC20 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_1_all_combo_10/test-of-cure_80combo201combined.csv")
dfTOC25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_1_all_combo_10/test-of-cure_80combo251combined.csv")
dfTOC31 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_1_all_combo_10/test-of-cure_80combo311combined.csv")

dfDST10 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_1_all_combo_10/drug_sus_testing_80combo101combined.csv")
dfDST15 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_1_all_combo_10/drug_sus_testing_80combo151combined.csv")
dfDST20 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_1_all_combo_10/drug_sus_testing_80combo201combined.csv")
dfDST25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_1_all_combo_10/drug_sus_testing_80combo251combined.csv")
dfDST31 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_1_all_combo_10/drug_sus_testing_80combo311combined.csv")




##############

#investigating outliers, june 5 2024
#################

#these are the data
dfGISP25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_7_2024_1_all_combo_10/GISP_05combo251combined.csv")
dfGISP25 <- identify(dfGISP25)
dfrandom25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_overnight_all_combo_10/randomcombo251combined.csv")
dfrandom25 <- identify(dfrandom25)

dfTOC25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_overnight_all_combo_10/test-of-cure_80combo251combined.csv")
dfTOC25 <- identify(dfTOC25)

dfDST25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_overnight_all_combo_10/drug_sus_testing_80combo251combined.csv")
dfDST25 <- identify(dfDST25)


#here we can visualize the outliers; those with change in cost below -10 or change in QALYS below -500
multiplot(
  visualize_cea("A.", dfGISP25, dfrandom25, dfTOC25, dfDST25),#+theme(legend.position = "none"),
  nmb(cea(dfGISP25, dfrandom25, dfTOC25, dfDST25), "B."),
  cols = 2
)


#added the code below to identify() so that duplicates are removed
dfrowcount <- matrix(nrow = 1, ncol = 2)
for (i in unique(dfGISP25$seed)){
  newrow <- c(i,count(dfGISP25[dfGISP25$seed == i,]))
  dfrowcount <- rbind(dfrowcount, newrow)
  
  
}

dfrowcount[,2] <- as.integer(dfrowcount[,2])
duplicates <- dfrowcount[dfrowcount[,2]>31,1][-1]
duplicate.seeds <- unlist(duplicates, use.names=FALSE)
#these are the seeds with more than one trajectory.


#now we need to perform the CEA step by step, to find the rows/trajectories where those outliers originate
ceadf_sum <- cea(dfGISP25, dfrandom25, dfTOC25, dfDST25)
ceadf <- rearrange_cea(ceadf_sum)

outliers<-ceadf[ceadf$AdjustedCost<=-100000000 | ceadf$AdjustedQALYs<=-500 ,]
outliers<-ceadf_sum[ceadf_sum$RandomcumulativeCostsAdj<=-100000000,]

#something weird here: many trajectories are outliers for both TOC and DST. could be high cost of E or X?
outlierscosts<-ceadf[ceadf$AdjustedCost<=-100000000,]

outliersQALYs<-ceadf[ceadf$AdjustedQALYs<=-500 ,]


outliers_params <- ceadf_sum[ceadf_sum$seed %in% outliers$seed,]
outliers_costs_params <- ceadf_sum[ceadf_sum$seed %in% outliers$seed,]


hist(unique(dfGISP25$TransmissionMSM))
hist(outliers_costs_params$TransmissionMSM, col="red", add = TRUE)


hist(unique(dfGISP25$ScreenIntervalMSM))
hist(outliers_costs_params$ScreenIntervalMSM, col="red", add = TRUE)


hist(unique(dfGISP25$ProbSymptomaticMSM))
hist(outliers_costs_params$ProbSymptomaticMSM, col="red", add = TRUE)


hist(unique(dfGISP25$RecoveryLambda))
hist(outliers_costs_params$RecoveryLambda, col="red", add = TRUE)


hist(unique(dfGISP25$DelayToSeekCareMSM))
hist(outliers_costs_params$DelayToSeekCareMSM, col="red", add = TRUE)


hist(unique(dfGISP25$DelayToRetreatmentMSM))
hist(outliers_costs_params$DelayToRetreatmentMSM, col="red", add = TRUE)

hist(unique(dfGISP25$DSTsensitivity))
hist(outliers_costs_params$DSTsensitivity, col="red", add = TRUE)




hist(unique(dfGISP25$CareCost))
hist(outliers_costs_params$CareCost, col="red", add = TRUE)

hist(unique(dfGISP25$TestCost))
hist(outliers_costs_params$TestCost, col="red", add = TRUE)

hist(unique(dfGISP25$StrainTestCost))
hist(outliers_costs_params$StrainTestCost, col="red", add = TRUE)

hist(unique(dfGISP25$DrugATreatmentCost))
hist(outliers_costs_params$DrugATreatmentCost, col="red", add = TRUE)

hist(unique(dfGISP25$DrugBTreatmentCost))
hist(outliers_costs_params$DrugBTreatmentCost, col="red", add = TRUE)

hist(unique(dfGISP25$DrugXTreatmentCost))
hist(outliers_costs_params$DrugXTreatmentCost, col="red", add = TRUE)

hist(unique(dfGISP25$DrugETreatmentCost))
hist(outliers_costs_params$DrugETreatmentCost, col="red", add = TRUE)


#the entire trajectories for seeds which creat CEA "outliers"
outliers_costs_traj_GISP <- dfGISP25[dfGISP25$seed %in% outlierscosts$seed,]
outliers_costs_traj_random <- dfrandom25[dfrandom25$seed %in% outlierscosts$seed,]
outliers_costs_traj_TOC <- dfTOC25[dfTOC25$seed %in% outlierscosts$seed,]
outliers_costs_traj_DST <- dfDST25[dfDST25$seed %in% outlierscosts$seed,]

multiplot(
  visualize_cea("A.", outliers_costs_traj_GISP, outliers_costs_traj_random, outliers_costs_traj_TOC, outliers_costs_traj_DST),#+theme(legend.position = "none"),
  nmb(cea(outliers_costs_traj_GISP, outliers_costs_traj_random, outliers_costs_traj_TOC, outliers_costs_traj_DST), "B."),
  cols = 2
)

new_figure_five(outliers_costs_traj_GISP, outliers_costs_traj_random, outliers_costs_traj_TOC, outliers_costs_traj_DST)
compare_four_cost(25, outliers_costs_traj_GISP, "GISP",  outliers_costs_traj_random, "random", outliers_costs_traj_TOC, "TOC", outliers_costs_traj_DST, "DST")


multiplot(
  viz_E(outliers_costs_traj_GISP, "A. GISP", 25, 20000),
  viz_E(outliers_costs_traj_random, "B. Random", 25, 20000),
  viz_E(outliers_costs_traj_TOC, "C. TOC", 25, 20000),
  viz_E(outliers_costs_traj_DST, "D. DST", 25, 20000), cols = 2)








#seed 11198 is one of the most outlier ones
new_figure_five(dfGISP25[dfGISP25$seed == 11198,], dfrandom25[dfrandom25$seed == 11198,], dfTOC25[dfTOC25$seed == 11198,], dfDST25[dfDST25$seed == 11198,])
compare_four_cost(25, dfGISP25[dfGISP25$seed == 11198,], "GISP", dfrandom25[dfrandom25$seed == 11198,], "Random", dfTOC25[dfTOC25$seed == 11198,], "TOC", dfDST25[dfDST25$seed == 11198,], "DST")




ceadf[ceadf$AdjustedCost<=-100000000,]


##################

#June 7 ms figures
###################

#figure 4 - calibrated trajectories. 1000 best of 300,000
dfcalibrated  <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_overnight_none_none_10/nonenone101combined.csv")
visualize_calibration_MSM(dfcalibrated)

dfGISP25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_7_2024_1_all_combo_10/GISP_05combo251combined.csv")
dfGISP25 <- identify(dfGISP25)
dfrandom25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_overnight_all_combo_10/randomcombo251combined.csv")
dfrandom25 <- identify(dfrandom25)

dfTOC25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_overnight_all_combo_10/test-of-cure_80combo251combined.csv")
dfTOC25 <- identify(dfTOC25)

dfDST25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_overnight_all_combo_10/drug_sus_testing_80combo251combined.csv")
dfDST25 <- identify(dfDST25)

#figure 5
summary_plot(dfGISP25, dfrandom25, dfTOC25, dfDST25)


#figure 6
new_figure_five(dfGISP25, dfrandom25, dfTOC25, dfDST25)


#figure 7
multiplot(
  visualize_cea("A.", dfGISP25, dfrandom25, dfTOC25, dfDST25)+ 
    geom_segment(aes(x=-100, y =-10, xend=-1300, yend=-85), colour = "gray60")+
    geom_segment(aes(x=-510, y =5, xend=-1300, yend=-5), colour = "gray60")+
    theme(legend.position = "bottom", legend.title = element_blank()) + 
    inset_element(
      visualize_cea("", dfGISP25, dfrandom25, dfTOC25, dfDST25)+
        theme(legend.position = "none", axis.title = element_blank(), title = element_blank()) +
        coord_cartesian(ylim=c(-1, 15), xlim = c(-10, 5)),
      left=0.01,
      bottom = 0.01,
      right = 0.5,
      top = 0.5) ,


  nmb(cea(dfGISP25, dfrandom25, dfTOC25, dfDST25), "B."),
  cols = 2
)


# for supplemental materials
dfGISP10 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_7_2024_1_all_combo_10/GISP_05combo101combined.csv")
dfGISP10 <- identify(dfGISP10)
dfGISP15 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_7_2024_1_all_combo_10/GISP_05combo151combined.csv")
dfGISP15 <- identify(dfGISP15)
dfGISP20 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_7_2024_1_all_combo_10/GISP_05combo201combined.csv")
dfGISP20 <- identify(dfGISP20)
dfGISP25 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_7_2024_1_all_combo_10/GISP_05combo251combined.csv")
dfGISP25 <- identify(dfGISP25)
dfGISP31 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_7_2024_1_all_combo_10/GISP_05combo311combined.csv")
dfGISP31 <- identify(dfGISP31)

dfrandom10 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_overnight_all_combo_10/randomcombo101combined.csv")
dfrandom10 <- identify(dfrandom10)
dfrandom15 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_overnight_all_combo_10/randomcombo151combined.csv")
dfrandom15 <- identify(dfrandom15)
dfrandom20 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_overnight_all_combo_10/randomcombo201combined.csv")
dfrandom20 <- identify(dfrandom20)
dfrandom25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_overnight_all_combo_10/randomcombo251combined.csv")
dfrandom25 <- identify(dfrandom25)
dfrandom31 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_overnight_all_combo_10/randomcombo311combined.csv")
dfrandom31 <- identify(dfrandom31)

dfTOC10 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_overnight_all_combo_10/test-of-cure_80combo101combined.csv")
dfTOC10 <- identify(dfTOC10)
dfTOC15 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_overnight_all_combo_10/test-of-cure_80combo151combined.csv")
dfTOC15 <- identify(dfTOC15)
dfTOC20 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_overnight_all_combo_10/test-of-cure_80combo201combined.csv")
dfTOC20 <- identify(dfTOC20)
dfTOC25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_overnight_all_combo_10/test-of-cure_80combo251combined.csv")
dfTOC25 <- identify(dfTOC25)
dfTOC31 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_overnight_all_combo_10/test-of-cure_80combo311combined.csv")
dfTOC31 <- identify(dfTOC31)

dfDST10 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_overnight_all_combo_10/drug_sus_testing_80combo101combined.csv")
dfDST10 <- identify(dfDST10)
dfDST15 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_overnight_all_combo_10/drug_sus_testing_80combo151combined.csv")
dfDST15 <- identify(dfDST15)
dfDST20 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_overnight_all_combo_10/drug_sus_testing_80combo201combined.csv")
dfDST20 <- identify(dfDST20)
dfDST25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_overnight_all_combo_10/drug_sus_testing_80combo251combined.csv")
dfDST25 <- identify(dfDST25)
dfDST31 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_overnight_all_combo_10/drug_sus_testing_80combo311combined.csv")
dfDST31 <- identify(dfDST31)




#figure s1
giant_summary_plot(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                   dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                   dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                   dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

#figure s2
visualize_prev_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                   dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                   dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                   dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

#figure s3
visualize_inc_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                  dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                  dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                  dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

#figure s4
visualize_true_resist_A_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                            dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                            dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                            dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

#figure s5
visualize_true_resist_B_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                            dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                            dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                            dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

#figure s6
visualize_true_resist_both_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                               dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                               dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                               dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

#figure s7
visualize_fail_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                   dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                   dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                   dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

#figure s8
visualize_attempts_A_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                         dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                         dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                         dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

#figure s9
visualize_attempts_B_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                         dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                         dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                         dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

#figure s10
visualize_attempts_X_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                         dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                         dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                         dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

#figure s11
visualize_E_avail_X(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                    dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                    dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                    dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

#figure s12
visualize_cost_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                   dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                   dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                   dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

#figure s13
visualize_QALYs_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                    dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                    dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                    dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)





#figure s14
multiplot(
  visualize_cea("A. Drug X available year 10", dfGISP10, dfrandom10, dfTOC10, dfDST10)+theme(legend.position = "none")+coord_cartesian(ylim=c(-25, 150), xlim = c(-4000, 100)),
  visualize_cea("D. Drug X available year 15", dfGISP15, dfrandom15, dfTOC15, dfDST15)+theme(legend.position = "none")+coord_cartesian(ylim=c(-25, 150), xlim = c(-4000, 100)),
  visualize_cea("G. Drug X available year 20", dfGISP20, dfrandom20, dfTOC20, dfDST20)+theme(legend.position = "none")+coord_cartesian(ylim=c(-25, 150), xlim = c(-4000, 100)),  
  visualize_cea("J. Drug X available year 25", dfGISP25, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-25, 150), xlim = c(-4000, 100)),
  visualize_cea("M. Drug X never available", dfGISP31, dfrandom31, dfTOC31, dfDST31)+theme(legend.position = "none")+coord_cartesian(ylim=c(-25, 150), xlim = c(-4000, 100)),
  
  visualize_cea("B.", dfGISP10, dfrandom10, dfTOC10, dfDST10)+theme(legend.position = "none")+coord_cartesian(ylim=c(-1, 15), xlim = c(-10, 5)),
  visualize_cea("E.", dfGISP15, dfrandom15, dfTOC15, dfDST15)+theme(legend.position = "none")+coord_cartesian(ylim=c(-1, 15), xlim = c(-10, 5)),
  visualize_cea("H.", dfGISP20, dfrandom20, dfTOC20, dfDST20)+theme(legend.position = "none")+coord_cartesian(ylim=c(-1, 15), xlim = c(-10, 5)),  
  visualize_cea("K.", dfGISP25, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-1, 15), xlim = c(-10, 5)),
  visualize_cea("N.", dfGISP31, dfrandom31, dfTOC31, dfDST31)+theme(legend.position = "none")+coord_cartesian(ylim=c(-1, 15), xlim = c(-10, 5)),
  
  nmb(cea(dfGISP10, dfrandom10, dfTOC10, dfDST10), "C."),
  nmb(cea(dfGISP15, dfrandom15, dfTOC15, dfDST15), "F."),
  nmb(cea(dfGISP20, dfrandom20, dfTOC20, dfDST20), "I."),
  nmb(cea(dfGISP25, dfrandom25, dfTOC25, dfDST25), "L."),
  nmb(cea(dfGISP31, dfrandom31, dfTOC31, dfDST31), "O."),
  cols = 3
)
##################

#resampling again
###########

dfsweep <- read.csv("/Users/me597/Documents/MSMoutput/output_MAY_30_2024_overnight_sweep_none_0/sweepnone0supercombined.csv")
dfsweep$uniqueID <- as.integer(paste(as.character(dfsweep$RunNumber), as.character(dfsweep$seed), sep=''))
df_ends <- calc_weights(dfsweep)
df_best_ends <- resample(df_ends, 1000)

df_best_ends_unique <- data.frame(matrix(ncol=length(df_best_ends[1,]), nrow = 0))
colnames(df_best_ends_unique) <- colnames(df_best_ends)

unique_resamples <- unique(df_best_ends$seed)
for (unique_seed in unique_resamples){
  newrow <- first(df_best_ends[df_best_ends$seed == unique_seed,])
  df_best_ends_unique <- rbind(df_best_ends_unique, newrow)
}


df_best_traj <- best_traj(dfsweep,df_best_ends_unique)
visualize_calibration_MSM(df_best_traj)
visualize_parameters(df_best_ends)

hist(df_best_ends$TransmissionMSM)

write_calibrated(df_best_ends_unique)

length(dfsweep[dfsweep$tick==520,]$seed)



dfcalibrated  <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_11_2024_overnight_none_none_10/nonenone101combined.csv")
visualize_calibration_MSM(dfcalibrated)

dfGISP25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_11_2024_overnight_all_combo_10/GISP_05combo251combined.csv")
dfGISP25 <- identify(dfGISP25, df_best_ends)
dfrandom25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_12_2024_1_all_combo_10/randomcombo251combined.csv")
dfrandom25 <- identify(dfrandom25, df_best_ends)

dfTOC25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_11_2024_overnight_all_combo_10/test-of-cure_80combo251combined.csv")
dfTOC25 <- identify(dfTOC25, df_best_ends)

dfDST25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_12_2024_1_all_combo_10/drug_sus_testing_80combo251combined.csv")
dfDST25 <- identify(dfDST25, df_best_ends)

#figure 5
summary_plot(dfGISP25, dfrandom25, dfTOC25, dfDST25)


#figure 6
new_figure_five(dfGISP25, dfrandom25, dfTOC25, dfDST25)


#figure 7
multiplot(
  visualize_cea_weighted("A.", df_best_ends, dfGISP25, dfrandom25, dfTOC25, dfDST25)+ 
    #geom_segment(aes(x=-100, y =-10, xend=-1300, yend=-85), colour = "gray60")+
    #geom_segment(aes(x=-510, y =5, xend=-1300, yend=-5), colour = "gray60")+
    theme(legend.position = "bottom", legend.title = element_blank()) ,
    # inset_element(
    #   visualize_cea_weighted("",df_best_ends, dfGISP25, dfrandom25, dfTOC25, dfDST25)+
    #     theme(legend.position = "none", axis.title = element_blank(), title = element_blank()) +
    #     coord_cartesian(ylim=c(-1, 15), xlim = c(-10, 5)),
    #   left=0.01,
    #   bottom = 0.01,
    #   right = 0.5,
    #   top = 0.5) ,
  
  
  nmb(cea_weighted(df_best_ends, dfGISP25, dfrandom25, dfTOC25, dfDST25), "B."),
  cols = 2
)

cearesults <- cea_weighted(df_best_ends, dfGISP25, dfrandom25, dfTOC25, dfDST25)

cearesults[cearesults$DSTcumulativeCosts==max(cearesults$DSTcumulativeCosts),]

outlier<-dfDST25[dfDST25$seed==37549,]







dfsweep <- read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_12_2024_2_sweep_none_0/sweepnone0supercombined.csv")
dfsweep$uniqueID <- as.integer(paste(as.character(dfsweep$RunNumber), as.character(dfsweep$seed), sep=''))
df_ends <- calc_weights(dfsweep)
df_best_ends <- resample(df_ends, 1000)

df_best_ends_unique <- data.frame(matrix(ncol=length(df_best_ends[1,]), nrow = 0))
colnames(df_best_ends_unique) <- colnames(df_best_ends)

unique_resamples <- unique(df_best_ends$seed)
for (unique_seed in unique_resamples){
  newrow <- first(df_best_ends[df_best_ends$seed == unique_seed,])
  df_best_ends_unique <- rbind(df_best_ends_unique, newrow)
}

best_ends_unique <- identify(df_best_ends_unique, df_best_ends)

df_best_traj <- best_traj(dfsweep,df_best_ends_unique)
df_best_traj <- identify(df_best_traj, df_best_ends)

visualize_calibration_MSM(df_best_traj)
visualize_parameters(df_best_ends)

hist(df_best_ends$TransmissionMSM)

write_calibrated(df_best_ends_unique)

#test resampling with narrower parameter sweep
dfcalibrated  <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_12_2024_2_none_none_10/nonenone101combined.csv")
dfcalibrated <- identify(dfcalibrated, df_best_ends)
visualize_calibration_MSM(dfcalibrated)

dfGISP25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_12_2024_2_all_combo_10/GISP_05combo251combined.csv")
dfGISP25 <- identify(dfGISP25, df_best_ends)
dfrandom25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_12_2024_2_all_combo_10/randomcombo251combined.csv")
dfrandom25 <- identify(dfrandom25, df_best_ends)

dfTOC25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_12_2024_2_all_combo_10/test-of-cure_80combo251combined.csv")
dfTOC25 <- identify(dfTOC25, df_best_ends)

dfDST25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_12_2024_2_all_combo_10/drug_sus_testing_80combo251combined.csv")
dfDST25 <- identify(dfDST25, df_best_ends)

#figure 5
summary_plot(dfGISP25, dfrandom25, dfTOC25, dfDST25)


#figure 6
new_figure_five(dfGISP25, dfrandom25, dfTOC25, dfDST25)


#figure 7
multiplot(
  visualize_cea_weighted("A.", df_best_ends, dfGISP25, dfrandom25, dfTOC25, dfDST25)+ 
    #geom_segment(aes(x=-100, y =-10, xend=-1300, yend=-85), colour = "gray60")+
    #geom_segment(aes(x=-510, y =5, xend=-1300, yend=-5), colour = "gray60")+
    theme(legend.position = "bottom", legend.title = element_blank()) ,
  # inset_element(
  #   visualize_cea_weighted("",df_best_ends, dfGISP25, dfrandom25, dfTOC25, dfDST25)+
  #     theme(legend.position = "none", axis.title = element_blank(), title = element_blank()) +
  #     coord_cartesian(ylim=c(-1, 15), xlim = c(-10, 5)),
  #   left=0.01,
  #   bottom = 0.01,
  #   right = 0.5,
  #   top = 0.5) ,
  
  
  nmb(cea_weighted(df_best_ends, dfGISP25, dfrandom25, dfTOC25, dfDST25), "B."),
  cols = 2
)


test <- cea_weighted(df_best_ends, dfGISP25, dfrandom25, dfTOC25, dfDST25)
test <- rbind(test, lapply(test[], mean))

test[test$TOCcumulativeCostsAdj == max(test$TOCcumulativeCostsAdj),]$seed
test[test$RandomcumulativeCostsAdj == max(test$RandomcumulativeCostsAdj),]$seed
dfGISP25[dfGISP25$seed==90477,]$resampled

sum(df_best_ends$seed==90477)
sum(df_best_ends$seed==27281)

test2<-test
for (traj in row.names(test)){
  dup <- sum(df_best_ends$seed==test$seed[as.numeric(traj)])-1
  
  #add that many duplicate rows to ceadf
  if (dup > 0){
    test2 <- rbind(test2, test[rep(traj,dup),])
  }
}


#########

# june 13 larger fixed sweep
##############

dfsweep <- read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_12_2024_overnight_sweep_none_0/sweepnone0supercombined.csv")
dfsweep$uniqueID <- as.integer(paste(as.character(dfsweep$RunNumber), as.character(dfsweep$seed), sep=''))
df_ends <- calc_weights(dfsweep)
df_best_ends <- resample(df_ends, 1000)

df_best_ends_unique <- data.frame(matrix(ncol=length(df_best_ends[1,]), nrow = 0))
colnames(df_best_ends_unique) <- colnames(df_best_ends)

unique_resamples <- unique(df_best_ends$seed)
for (unique_seed in unique_resamples){
  newrow <- first(df_best_ends[df_best_ends$seed == unique_seed,])
  df_best_ends_unique <- rbind(df_best_ends_unique, newrow)
}

best_ends_unique <- identify(df_best_ends_unique, df_best_ends)

df_best_traj <- best_traj(dfsweep,df_best_ends_unique)
df_best_traj <- identify(df_best_traj, df_best_ends)

visualize_calibration_MSM(df_best_traj)
visualize_parameters(df_best_ends)

hist(df_best_ends$TransmissionMSM)

write_calibrated(df_best_ends_unique)



dfcalibrated  <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_13_2024_overnight_none_none_10/nonenone101combined.csv")
dfcalibrated <- identify(dfcalibrated, df_best_ends)
visualize_calibration_MSM(dfcalibrated)

dfGISP25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_13_2024_overnight_all_combo_10/GISP_05combo251combined.csv")
dfGISP25 <- identify(dfGISP25, df_best_ends)
dfrandom25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_13_2024_overnight_all_combo_10/randomcombo251combined.csv")
dfrandom25 <- identify(dfrandom25, df_best_ends)

dfTOC25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_13_2024_overnight_all_combo_10/test-of-cure_80combo251combined.csv")
dfTOC25 <- identify(dfTOC25, df_best_ends)

dfDST25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_13_2024_overnight_all_combo_10/drug_sus_testing_80combo251combined.csv")
dfDST25 <- identify(dfDST25, df_best_ends)

#figure 5
summary_plot(dfGISP25, dfrandom25, dfTOC25, dfDST25)


#figure 6
new_figure_five(dfGISP25, dfrandom25, dfTOC25, dfDST25)


#figure 7
multiplot(
  visualize_cea_weighted("A.", df_best_ends, dfGISP25, dfrandom25, dfTOC25, dfDST25)+ 
    #geom_segment(aes(x=-100, y =-10, xend=-1300, yend=-85), colour = "gray60")+
    #geom_segment(aes(x=-510, y =5, xend=-1300, yend=-5), colour = "gray60")+
    theme(legend.position = "bottom", legend.title = element_blank()) ,
  # inset_element(
  #   visualize_cea_weighted("",df_best_ends, dfGISP25, dfrandom25, dfTOC25, dfDST25)+
  #     theme(legend.position = "none", axis.title = element_blank(), title = element_blank()) +
  #     coord_cartesian(ylim=c(-1, 15), xlim = c(-10, 5)),
  #   left=0.01,
  #   bottom = 0.01,
  #   right = 0.5,
  #   top = 0.5) ,
  
  
  nmb(cea_weighted(df_best_ends, dfGISP25, dfrandom25, dfTOC25, dfDST25), "B."),
  cols = 2
)


#double-checking min values for difference in QALYs
june14cea<- cea_weighted(df_best_ends, dfGISP25, dfrandom25, dfTOC25, dfDST25)

june14cea[june14cea$DSTcumulativeQALYsAdj == max(june14cea$DSTcumulativeQALYsAdj),1]
june14cea[june14cea$TOCcumulativeQALYsAdj == max(june14cea$TOCcumulativeQALYsAdj),]
june14cea[june14cea$RandomcumulativeQALYsAdj == max(june14cea$RandomcumulativeQALYsAdj),]

mean(june14cea$DelayToRetreatmentMSM)
mean(june14cea$DelayToSeekCareMSM)

multiplot(
viz_qaly(dfGISP25[dfGISP25$seed==27503,], "GISP", 25),
viz_qaly(dfrandom25[dfrandom25$seed==27503,], "random", 25),
viz_qaly(dfTOC25[dfTOC25$seed==27503,], "TOC", 25),
viz_qaly(dfDST25[dfDST25$seed==27503,], "DST", 25),

# viz_all_failed(dfGISP25[dfGISP25$seed==27503,], "GISP", 25),
# viz_all_failed(dfrandom25[dfrandom25$seed==27503,], "random",25),
# viz_all_failed(dfTOC25[dfTOC25$seed==27503,], "TOC",25),
# viz_all_failed(dfDST25[dfDST25$seed==27503,], "DST",25),

cols=4
)

outlier<-dfGISP25[dfGISP25$seed==27503,]

new_figure_five(dfGISP25[dfGISP25$seed==69955,],dfrandom25[dfrandom25$seed==69955,],dfTOC25[dfTOC25$seed==69955,],dfDST25[dfDST25$seed==69955,])
new_figure_five(dfGISP25[dfGISP25$seed==30134,],dfrandom25[dfrandom25$seed==30134,],dfTOC25[dfTOC25$seed==30134,],dfDST25[dfDST25$seed==30134,])








#MS figures for june 14th

#figure 2
visualize_calibration_MSM(dfcalibrated)

#figure 3
summary_plot(dfGISP25, dfrandom25, dfTOC25, dfDST25)


#figure 4
new_figure_five(dfGISP25, dfrandom25, dfTOC25, dfDST25)


#figure 5
multiplot(
  visualize_cea_weighted("A.", df_best_ends, dfGISP25, dfrandom25, dfTOC25, dfDST25)+ 
    theme(legend.position = "bottom", legend.title = element_blank()) ,
  nmb(cea_weighted(df_best_ends, dfGISP25, dfrandom25, dfTOC25, dfDST25), "B."),
  cols = 2
)



# supplemental figures for june 14th 

dfGISP10 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_13_2024_overnight_all_combo_10/GISP_05combo101combined.csv")
dfGISP10 <- identify(dfGISP10, df_best_ends)
dfGISP15 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_13_2024_overnight_all_combo_10/GISP_05combo151combined.csv")
dfGISP15 <- identify(dfGISP15, df_best_ends)
dfGISP20 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_13_2024_overnight_all_combo_10/GISP_05combo201combined.csv")
dfGISP20 <- identify(dfGISP20, df_best_ends)
dfGISP25 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_13_2024_overnight_all_combo_10/GISP_05combo251combined.csv")
dfGISP25 <- identify(dfGISP25, df_best_ends)
dfGISP31 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_13_2024_overnight_all_combo_10/GISP_05combo311combined.csv")
dfGISP31 <- identify(dfGISP31, df_best_ends)

dfrandom10 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_13_2024_overnight_all_combo_10/randomcombo101combined.csv")
dfrandom10 <- identify(dfrandom10, df_best_ends)
dfrandom15 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_13_2024_overnight_all_combo_10/randomcombo151combined.csv")
dfrandom15 <- identify(dfrandom15, df_best_ends)
dfrandom20 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_13_2024_overnight_all_combo_10/randomcombo201combined.csv")
dfrandom20 <- identify(dfrandom20, df_best_ends)
dfrandom25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_13_2024_overnight_all_combo_10/randomcombo251combined.csv")
dfrandom25 <- identify(dfrandom25, df_best_ends)
dfrandom31 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_13_2024_overnight_all_combo_10/randomcombo311combined.csv")
dfrandom31 <- identify(dfrandom31, df_best_ends)

dfTOC10 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_13_2024_overnight_all_combo_10/test-of-cure_80combo101combined.csv")
dfTOC10 <- identify(dfTOC10, df_best_ends)
dfTOC15 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_13_2024_overnight_all_combo_10/test-of-cure_80combo151combined.csv")
dfTOC15 <- identify(dfTOC15, df_best_ends)
dfTOC20 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_13_2024_overnight_all_combo_10/test-of-cure_80combo201combined.csv")
dfTOC20 <- identify(dfTOC20, df_best_ends)
dfTOC25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_13_2024_overnight_all_combo_10/test-of-cure_80combo251combined.csv")
dfTOC25 <- identify(dfTOC25, df_best_ends)
dfTOC31 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_13_2024_overnight_all_combo_10/test-of-cure_80combo311combined.csv")
dfTOC31 <- identify(dfTOC31, df_best_ends)

dfDST10 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_13_2024_overnight_all_combo_10/drug_sus_testing_80combo101combined.csv")
dfDST10 <- identify(dfDST10, df_best_ends)
dfDST15 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_13_2024_overnight_all_combo_10/drug_sus_testing_80combo151combined.csv")
dfDST15 <- identify(dfDST15, df_best_ends)
dfDST20 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_13_2024_overnight_all_combo_10/drug_sus_testing_80combo201combined.csv")
dfDST20 <- identify(dfDST20, df_best_ends)
dfDST25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_13_2024_overnight_all_combo_10/drug_sus_testing_80combo251combined.csv")
dfDST25 <- identify(dfDST25, df_best_ends)
dfDST31 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_13_2024_overnight_all_combo_10/drug_sus_testing_80combo311combined.csv")
dfDST31 <- identify(dfDST31, df_best_ends)


visualize_parameters(df_best_ends)


#figure s1
giant_summary_plot(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                   dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                   dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                   dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

#figure s2
visualize_prev_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                   dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                   dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                   dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

#figure s3
visualize_inc_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                  dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                  dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                  dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

#figure s4
visualize_true_resist_A_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                            dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                            dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                            dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

#figure s5
visualize_true_resist_B_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                            dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                            dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                            dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

#figure s6
visualize_true_resist_both_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                               dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                               dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                               dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

#figure s7
visualize_fail_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                   dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                   dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                   dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

#figure s8
visualize_attempts_A_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                         dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                         dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                         dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

#figure s9
visualize_attempts_B_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                         dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                         dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                         dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

#figure s10
visualize_attempts_X_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                         dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                         dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                         dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

#figure s11
visualize_E_avail_X(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                    dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                    dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                    dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

#figure s12
visualize_cost_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                   dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                   dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                   dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

#figure s13
visualize_QALYs_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                    dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                    dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                    dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)





#figure s14
multiplot(
  visualize_cea_weighted("A. Drug X available year 10", df_best_ends,dfGISP10, dfrandom10, dfTOC10, dfDST10)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-300, 50)),
  visualize_cea_weighted("D. Drug X available year 15", df_best_ends,dfGISP15, dfrandom15, dfTOC15, dfDST15)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-300, 50)),
  visualize_cea_weighted("G. Drug X available year 20", df_best_ends,dfGISP20, dfrandom20, dfTOC20, dfDST20)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-300, 50)),  
  visualize_cea_weighted("J. Drug X available year 25", df_best_ends,dfGISP25, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-300, 50)),
  visualize_cea_weighted("M. Drug X never available",df_best_ends, dfGISP31, dfrandom31, dfTOC31, dfDST31)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-300, 2000)),
  
  
  nmb(cea_weighted(df_best_ends,dfGISP10, dfrandom10, dfTOC10, dfDST10), "C."),
  nmb(cea_weighted(df_best_ends,dfGISP15, dfrandom15, dfTOC15, dfDST15), "F."),
  nmb(cea_weighted(df_best_ends,dfGISP20, dfrandom20, dfTOC20, dfDST20), "I."),
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOC25, dfDST25), "L."),
  nmb(cea_weighted(df_best_ends,dfGISP31, dfrandom31, dfTOC31, dfDST31), "O."),
  cols = 2
)

###########


#June 20th partial rank correlations
##############

setwd("/Users/me597/Documents/MSMoutput/PRCC")





GISPallPRCC <- prcc_all(dfGISP25)
RandomallPRCC <- prcc_all(dfrandom25)
TOCallPRCC <- prcc_all(dfTOC25)
DSTallPRCC <- prcc_all(dfDST25)

write.csv(GISPallPRCC$prevalance, file = "PRCCGISPPrevalence.csv")
write.csv(GISPallPRCC$incidence, file = "PRCCGISPIncidence.csv")
write.csv(GISPallPRCC$symptomprop, file = "PRCCGISPSymptProp.csv")
write.csv(GISPallPRCC$monetaryCost, file = "PRCCGISPMonetaryCost.csv")
write.csv(GISPallPRCC$QALYs, file = "PRCCGISPQALYs.csv")

write.csv(RandomallPRCC$prevalance, file = "PRCCRandomPrevalence.csv")
write.csv(RandomallPRCC$incidence, file = "PRCCRandomIncidence.csv")
write.csv(RandomallPRCC$symptomprop, file = "PRCCRandomSymptProp.csv")
write.csv(RandomallPRCC$monetaryCost, file = "PRCCRandomMonetaryCost.csv")
write.csv(RandomallPRCC$QALYs, file = "PRCCRandomQALYs.csv")

write.csv(TOCallPRCC$prevalance, file = "PRCCTOCPrevalence.csv")
write.csv(TOCallPRCC$incidence, file = "PRCCTOCIncidence.csv")
write.csv(TOCallPRCC$symptomprop, file = "PRCCTOCSymptProp.csv")
write.csv(TOCallPRCC$monetaryCost, file = "PRCCTOCMonetaryCost.csv")
write.csv(TOCallPRCC$QALYs, file = "PRCCTOCQALYs.csv")


write.csv(DSTallPRCC$prevalance, file = "PRCCDSTPrevalence.csv")
write.csv(DSTallPRCC$incidence, file = "PRCCDSTIncidence.csv")
write.csv(DSTallPRCC$symptomprop, file = "PRCCDSTSymptProp.csv")
write.csv(DSTallPRCC$monetaryCost, file = "PRCCDSTMonetaryCost.csv")
write.csv(DSTallPRCC$QALYs, file = "PRCCDSTQALYs.csv")


combined_df <- combine_avail(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31)
combined_ends <- get_ends(combined_df)
cumulative_inc(combined_df)



#############

#test - added E side effects and sequelae
############
dfsweep <- read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_debug_sweep_none_0/sweepnone01combined.csv")
dfsweep$uniqueID <- as.integer(paste(as.character(dfsweep$RunNumber), as.character(dfsweep$seed), sep=''))
df_ends <- calc_weights(dfsweep)
df_best_ends <- resample(df_ends, 1000)

df_best_ends_unique <- data.frame(matrix(ncol=length(df_best_ends[1,]), nrow = 0))
colnames(df_best_ends_unique) <- colnames(df_best_ends)

unique_resamples <- unique(df_best_ends$seed)
for (unique_seed in unique_resamples){
  newrow <- first(df_best_ends[df_best_ends$seed == unique_seed,])
  df_best_ends_unique <- rbind(df_best_ends_unique, newrow)
}

best_ends_unique <- identify(df_best_ends_unique, df_best_ends)

df_best_traj <- best_traj(dfsweep,df_best_ends_unique)
df_best_traj <- identify(df_best_traj, df_best_ends)

visualize_calibration_MSM(df_best_traj)
visualize_parameters(df_best_ends)

write_calibrated(df_best_ends_unique)



dfcalibrated  <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_debug_none_none_10/nonenone101combined.csv")
dfcalibrated <- identify(dfcalibrated, df_best_ends)
visualize_calibration_MSM(dfcalibrated)

dfGISP25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_debug_all_combo_10/GISP_05combo251combined.csv")
dfGISP25 <- identify(dfGISP25, df_best_ends)
dfrandom25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_debug_all_combo_10/randomcombo251combined.csv")
dfrandom25 <- identify(dfrandom25, df_best_ends)

dfTOC25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_debug_all_combo_10/test-of-cure_80combo251combined.csv")
dfTOC25 <- identify(dfTOC25, df_best_ends)

dfDST25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_debug_all_combo_10/drug_sus_testing_80combo251combined.csv")
dfDST25 <- identify(dfDST25, df_best_ends)

#figure 5
summary_plot(dfGISP25, dfrandom25, dfTOC25, dfDST25)


#figure 6
new_figure_five(dfGISP25, dfrandom25, dfTOC25, dfDST25)


#figure 7
multiplot(
  visualize_cea_weighted("A.", df_best_ends, dfGISP25, dfrandom25, dfTOC25, dfDST25)+ 
    #geom_segment(aes(x=-100, y =-10, xend=-1300, yend=-85), colour = "gray60")+
    #geom_segment(aes(x=-510, y =5, xend=-1300, yend=-5), colour = "gray60")+
    theme(legend.position = "bottom", legend.title = element_blank()) ,
  # inset_element(
  #   visualize_cea_weighted("",df_best_ends, dfGISP25, dfrandom25, dfTOC25, dfDST25)+
  #     theme(legend.position = "none", axis.title = element_blank(), title = element_blank()) +
  #     coord_cartesian(ylim=c(-1, 15), xlim = c(-10, 5)),
  #   left=0.01,
  #   bottom = 0.01,
  #   right = 0.5,
  #   top = 0.5) ,
  
  
  nmb(cea_weighted(df_best_ends, dfGISP25, dfrandom25, dfTOC25, dfDST25), "B."),
  cols = 2
)



dfGISP10 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_debug_all_combo_10/GISP_05combo101combined.csv")
dfGISP10 <- identify(dfGISP10, df_best_ends)
dfGISP15 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_debug_all_combo_10/GISP_05combo151combined.csv")
dfGISP15 <- identify(dfGISP15, df_best_ends)
dfGISP20 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_debug_all_combo_10/GISP_05combo201combined.csv")
dfGISP20 <- identify(dfGISP20, df_best_ends)
dfGISP25 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_debug_all_combo_10/GISP_05combo251combined.csv")
dfGISP25 <- identify(dfGISP25, df_best_ends)
dfGISP31 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_debug_all_combo_10/GISP_05combo311combined.csv")
dfGISP31 <- identify(dfGISP31, df_best_ends)

dfrandom10 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_debug_all_combo_10/randomcombo101combined.csv")
dfrandom10 <- identify(dfrandom10, df_best_ends)
dfrandom15 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_debug_all_combo_10/randomcombo151combined.csv")
dfrandom15 <- identify(dfrandom15, df_best_ends)
dfrandom20 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_debug_all_combo_10/randomcombo201combined.csv")
dfrandom20 <- identify(dfrandom20, df_best_ends)
dfrandom25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_debug_all_combo_10/randomcombo251combined.csv")
dfrandom25 <- identify(dfrandom25, df_best_ends)
dfrandom31 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_debug_all_combo_10/randomcombo311combined.csv")
dfrandom31 <- identify(dfrandom31, df_best_ends)

dfTOC10 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_debug_all_combo_10/test-of-cure_80combo101combined.csv")
dfTOC10 <- identify(dfTOC10, df_best_ends)
dfTOC15 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_debug_all_combo_10/test-of-cure_80combo151combined.csv")
dfTOC15 <- identify(dfTOC15, df_best_ends)
dfTOC20 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_debug_all_combo_10/test-of-cure_80combo201combined.csv")
dfTOC20 <- identify(dfTOC20, df_best_ends)
dfTOC25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_debug_all_combo_10/test-of-cure_80combo251combined.csv")
dfTOC25 <- identify(dfTOC25, df_best_ends)
dfTOC31 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_debug_all_combo_10/test-of-cure_80combo311combined.csv")
dfTOC31 <- identify(dfTOC31, df_best_ends)

dfDST10 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_debug_all_combo_10/drug_sus_testing_80combo101combined.csv")
dfDST10 <- identify(dfDST10, df_best_ends)
dfDST15 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_debug_all_combo_10/drug_sus_testing_80combo151combined.csv")
dfDST15 <- identify(dfDST15, df_best_ends)
dfDST20 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_debug_all_combo_10/drug_sus_testing_80combo201combined.csv")
dfDST20 <- identify(dfDST20, df_best_ends)
dfDST25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_debug_all_combo_10/drug_sus_testing_80combo251combined.csv")
dfDST25 <- identify(dfDST25, df_best_ends)
dfDST31 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_debug_all_combo_10/drug_sus_testing_80combo311combined.csv")
dfDST31 <- identify(dfDST31, df_best_ends)

#figure s12
visualize_cost_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                   dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                   dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                   dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

#figure s13
visualize_QALYs_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                    dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                    dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                    dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)





############




#June 21-24 sweep and experiment
############

dfsweep <- read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_overnight_sweep_none_0/sweepnone0supercombined.csv")
dfsweep$uniqueID <- as.integer(paste(as.character(dfsweep$RunNumber), as.character(dfsweep$seed), sep=''))
df_ends <- calc_weights(dfsweep)
df_best_ends <- resample(df_ends, 1000)

df_best_ends_unique <- data.frame(matrix(ncol=length(df_best_ends[1,]), nrow = 0))
colnames(df_best_ends_unique) <- colnames(df_best_ends)

unique_resamples <- unique(df_best_ends$seed)
for (unique_seed in unique_resamples){
  newrow <- first(df_best_ends[df_best_ends$seed == unique_seed,])
  df_best_ends_unique <- rbind(df_best_ends_unique, newrow)
}

best_ends_unique <- identify(df_best_ends_unique, df_best_ends)

df_best_traj <- best_traj(dfsweep,df_best_ends_unique)
df_best_traj <- identify(df_best_traj, df_best_ends)

visualize_calibration_MSM(df_best_traj)
visualize_parameters(df_best_ends)

write_calibrated(df_best_ends_unique)






dfcalibrated  <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_overnight_none_none_10/nonenone101combined.csv")
dfcalibrated <- identify(dfcalibrated, df_best_ends)

#figure 2
visualize_calibration_MSM(dfcalibrated)

dfGISP25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_overnight_all_combo_10/GISP_05combo251combined.csv")
dfGISP25 <- identify(dfGISP25, df_best_ends)
dfrandom25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_overnight_all_combo_10/randomcombo251combined.csv")
dfrandom25 <- identify(dfrandom25, df_best_ends)

dfTOC25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_overnight_all_combo_10/test-of-cure_80combo251combined.csv")
dfTOC25 <- identify(dfTOC25, df_best_ends)

dfDST25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_overnight_all_combo_10/drug_sus_testing_80combo251combined.csv")
dfDST25 <- identify(dfDST25, df_best_ends)



#figure 3
summary_plot(dfGISP25, dfrandom25, dfTOC25, dfDST25)


#figure 4
new_figure_four(dfGISP25, dfrandom25, dfTOC25, dfDST25)


#figure 5
multiplot(
  visualize_cea_weighted("A.", df_best_ends, dfGISP25, dfrandom25, dfTOC25, dfDST25)+ 
    theme(legend.position = "bottom", legend.title = element_blank()) ,
  nmb(cea_weighted(df_best_ends, dfGISP25, dfrandom25, dfTOC25, dfDST25), "B."),
  cols = 2
)

#######


#for supplement - sensitivity analysis
########

#supplement 2 figure 1
visualize_parameters(df_best_ends)

#supplement 3
dfGISP10 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_overnight_all_combo_10/GISP_05combo101combined.csv")
dfGISP10 <- identify(dfGISP10, df_best_ends)
dfGISP15 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_overnight_all_combo_10/GISP_05combo151combined.csv")
dfGISP15 <- identify(dfGISP15, df_best_ends)
dfGISP20 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_overnight_all_combo_10/GISP_05combo201combined.csv")
dfGISP20 <- identify(dfGISP20, df_best_ends)
dfGISP25 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_overnight_all_combo_10/GISP_05combo251combined.csv")
dfGISP25 <- identify(dfGISP25, df_best_ends)
dfGISP31 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_overnight_all_combo_10/GISP_05combo311combined.csv")
dfGISP31 <- identify(dfGISP31, df_best_ends)

dfrandom10 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_overnight_all_combo_10/randomcombo101combined.csv")
dfrandom10 <- identify(dfrandom10, df_best_ends)
dfrandom15 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_overnight_all_combo_10/randomcombo151combined.csv")
dfrandom15 <- identify(dfrandom15, df_best_ends)
dfrandom20 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_overnight_all_combo_10/randomcombo201combined.csv")
dfrandom20 <- identify(dfrandom20, df_best_ends)
dfrandom25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_overnight_all_combo_10/randomcombo251combined.csv")
dfrandom25 <- identify(dfrandom25, df_best_ends)
dfrandom31 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_overnight_all_combo_10/randomcombo311combined.csv")
dfrandom31 <- identify(dfrandom31, df_best_ends)

dfTOC10 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_overnight_all_combo_10/test-of-cure_80combo101combined.csv")
dfTOC10 <- identify(dfTOC10, df_best_ends)
dfTOC15 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_overnight_all_combo_10/test-of-cure_80combo151combined.csv")
dfTOC15 <- identify(dfTOC15, df_best_ends)
dfTOC20 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_overnight_all_combo_10/test-of-cure_80combo201combined.csv")
dfTOC20 <- identify(dfTOC20, df_best_ends)
dfTOC25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_overnight_all_combo_10/test-of-cure_80combo251combined.csv")
dfTOC25 <- identify(dfTOC25, df_best_ends)
dfTOC31 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_overnight_all_combo_10/test-of-cure_80combo311combined.csv")
dfTOC31 <- identify(dfTOC31, df_best_ends)

dfDST10 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_overnight_all_combo_10/drug_sus_testing_80combo101combined.csv")
dfDST10 <- identify(dfDST10, df_best_ends)
dfDST15 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_overnight_all_combo_10/drug_sus_testing_80combo151combined.csv")
dfDST15 <- identify(dfDST15, df_best_ends)
dfDST20 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_overnight_all_combo_10/drug_sus_testing_80combo201combined.csv")
dfDST20 <- identify(dfDST20, df_best_ends)
dfDST25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_overnight_all_combo_10/drug_sus_testing_80combo251combined.csv")
dfDST25 <- identify(dfDST25, df_best_ends)
dfDST31 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_overnight_all_combo_10/drug_sus_testing_80combo311combined.csv")
dfDST31 <- identify(dfDST31, df_best_ends)



#figure s3.1
giant_summary_plot(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                   dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                   dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                   dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

#figure s3.2
multiplot(
  # visualize_cea_weighted("A. Drug X available year 10", df_best_ends,dfGISP10, dfrandom10, dfTOC10, dfDST10)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-600, 500)),
  # visualize_cea_weighted("D. Drug X available year 15", df_best_ends,dfGISP15, dfrandom15, dfTOC15, dfDST15)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-600, 500)),
  # visualize_cea_weighted("G. Drug X available year 20", df_best_ends,dfGISP20, dfrandom20, dfTOC20, dfDST20)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-600, 500)),  
  # visualize_cea_weighted("J. Drug X available year 25", df_best_ends,dfGISP25, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-600, 500)),
  # visualize_cea_weighted("M. Drug X never available",df_best_ends, dfGISP31, dfrandom31, dfTOC31, dfDST31)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-600, 500)),
  
  visualize_cea_weighted("A. Drug X available year 10", df_best_ends,dfGISP15, dfrandom15, dfTOC15, dfDST15)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-600, 500)),
  visualize_cea_weighted("C. Drug X available year 15", df_best_ends,dfGISP20, dfrandom20, dfTOC20, dfDST20)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-600, 500)),
  visualize_cea_weighted("E. Drug X available year 20", df_best_ends,dfGISP25, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-600, 500)),  
  visualize_cea_weighted("G. Drug X never available",df_best_ends, dfGISP31, dfrandom31, dfTOC31, dfDST31)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-600, 500)),
  
  
  # nmb(cea_weighted(df_best_ends,dfGISP10, dfrandom10, dfTOC10, dfDST10), "C."),
  # nmb(cea_weighted(df_best_ends,dfGISP15, dfrandom15, dfTOC15, dfDST15), "F."),
  # nmb(cea_weighted(df_best_ends,dfGISP20, dfrandom20, dfTOC20, dfDST20), "I."),
  # nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOC25, dfDST25), "L."),
  # nmb(cea_weighted(df_best_ends,dfGISP31, dfrandom31, dfTOC31, dfDST31), "O."),
  
  nmb(cea_weighted(df_best_ends,dfGISP15, dfrandom15, dfTOC15, dfDST15), "B."),
  nmb(cea_weighted(df_best_ends,dfGISP20, dfrandom20, dfTOC20, dfDST20), "D."),
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOC25, dfDST25), "F."),
  nmb(cea_weighted(df_best_ends,dfGISP31, dfrandom31, dfTOC31, dfDST31), "H."),
  
  cols = 2
)


#figure s3.3
visualize_attempts_X_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                         dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                         dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                         dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

#figure s3.4
visualize_E_avail_X(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                    dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                    dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                    dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)



visualize_prev_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                   dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                   dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                   dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

visualize_inc_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                  dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                  dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                  dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)


visualize_true_resist_A_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                            dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                            dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                            dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

visualize_true_resist_B_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                            dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                            dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                            dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

visualize_true_resist_both_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                               dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                               dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                               dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

visualize_fail_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                   dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                   dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                   dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

visualize_attempts_A_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                         dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                         dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                         dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

visualize_attempts_B_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                         dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                         dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                         dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)


visualize_cost_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                   dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                   dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                   dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

visualize_QALYs_all(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                    dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                    dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                    dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)







# switch threshold
dfGISP4 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_24_2024_1_switch_combo/GISP_4combo251combined.csv")
dfGISP4 <- identify(dfGISP4, df_best_ends)

dfGISP45 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_24_2024_1_switch_combo/GISP_45combo251combined.csv")
dfGISP45 <- identify(dfGISP45, df_best_ends)

dfGISP5 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_24_2024_1_switch_combo/GISP_5combo251combined.csv")
dfGISP5 <- identify(dfGISP5, df_best_ends)

dfGISP55 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_24_2024_1_switch_combo/GISP_55combo251combined.csv")
dfGISP55 <- identify(dfGISP55, df_best_ends)

dfGISP6 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_24_2024_1_switch_combo/GISP_6combo251combined.csv")
dfGISP6 <- identify(dfGISP6, df_best_ends)

compare_five(dfGISP4, "Switch 4%",dfGISP45,"Switch 4.5%", dfGISP5,"Switch 5%",dfGISP55,"Switch 5.5%",dfGISP6, "Switch 6%")

compare_five_cost(dfGISP4, "Switch 4%",dfGISP45,"Switch 4.5%", dfGISP5,"Switch 5%",dfGISP55,"Switch 5.5%",dfGISP6, "Switch 6%")


dfGISP2 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JULY_24_2024_1_switch_combo/GISP_2combo251combined.csv")
dfGISP2 <- identify(dfGISP2, df_best_ends)

dfGISP8 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JULY_24_2024_1_switch_combo/GISP_8combo251combined.csv")
dfGISP8 <- identify(dfGISP8, df_best_ends)

compare_five(dfGISP2, "Switch 2%", dfGISP4, "Switch 4%", dfGISP5,"Switch 5%",dfGISP6, "Switch 6%", dfGISP8, "Switch 8%")

compare_five_cost(dfGISP2, "Switch 2%", dfGISP4, "Switch 4%", dfGISP5,"Switch 5%",dfGISP6, "Switch 6%", dfGISP8, "Switch 8%")


#rDST coverage
dfDST50 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_24_2024_1_availrDST_combo/drug_sus_testing_50combo251combined.csv")
dfDST50 <- identify(dfDST50, df_best_ends)

dfDST60 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_24_2024_1_availrDST_combo/drug_sus_testing_60combo251combined.csv")
dfDST60 <- identify(dfDST60, df_best_ends)

dfDST70 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_24_2024_1_availrDST_combo/drug_sus_testing_70combo251combined.csv")
dfDST70 <- identify(dfDST70, df_best_ends)

dfDST80 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_24_2024_1_availrDST_combo/drug_sus_testing_80combo251combined.csv")
dfDST80 <- identify(dfDST80, df_best_ends)

dfDST90 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_24_2024_1_availrDST_combo/drug_sus_testing_90combo251combined.csv")
dfDST90 <- identify(dfDST90, df_best_ends)

dfDST100 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_24_2024_1_availrDST_combo/drug_sus_testing_100combo251combined.csv")
dfDST100 <- identify(dfDST100, df_best_ends)

compare_five(dfDST60, "DST 60%",dfDST70,"DST 70%", dfDST80,"DST 80%",dfDST90,"DST 90%",dfDST100, "DST 100%")
compare_five_cost(dfDST60, "DST 60%",dfDST70,"DST 70%", dfDST80,"DST 80%",dfDST90,"DST 90%",dfDST100, "DST 100%")


#adhereTOCsympt

dfTOCadsympt20 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_24_2024_1_adhereTOCsympt_combo/test-of-cure_sympt_20combo251combined.csv")
dfTOCadsympt20 <- identify(dfTOCadsympt20, df_best_ends)

dfTOCadsympt40 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_24_2024_1_adhereTOCsympt_combo/test-of-cure_sympt_40combo251combined.csv")
dfTOCadsympt40 <- identify(dfTOCadsympt40, df_best_ends)

dfTOCadsympt60 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_24_2024_1_adhereTOCsympt_combo/test-of-cure_sympt_60combo251combined.csv")
dfTOCadsympt60 <- identify(dfTOCadsympt60, df_best_ends)

dfTOCadsympt70 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_26_2024_1_adhereTOCsympt_combo/test-of-cure_sympt_70combo251combined.csv")
dfTOCadsympt70 <- identify(dfTOCadsympt70, df_best_ends)

dfTOCadsympt80 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_24_2024_1_adhereTOCsympt_combo/test-of-cure_sympt_80combo251combined.csv")
dfTOCadsympt80 <- identify(dfTOCadsympt80, df_best_ends)

dfTOCadsympt90 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_26_2024_1_adhereTOCsympt_combo/test-of-cure_sympt_90combo251combined.csv")
dfTOCadsympt90 <- identify(dfTOCadsympt90, df_best_ends)

dfTOCadsympt100 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_22_2024_1_adhereTOCsympt_combo/test-of-cure_sympt_100combo251combined.csv")
dfTOCadsympt100 <- identify(dfTOCadsympt100, df_best_ends)

compare_five(dfTOCadsympt60,"Sympt Adherence 60%",dfTOCadsympt70,"Sympt Adherence 70%",dfTOCadsympt80,"Sympt Adherence 80%%",dfTOCadsympt90,"Sympt Adherence 90%",dfTOCadsympt100, "Sympt Adherence 100%")
compare_five_cost(dfTOCadsympt60,"Sympt Adherence 60%",dfTOCadsympt70,"Sympt Adherence 70%",dfTOCadsympt80,"Sympt Adherence 80%",dfTOCadsympt90,"Sympt Adherence 90%",dfTOCadsympt100, "Sympt Adherence 100%")


#adhereTOCasympt
dfTOCadasympt20 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_24_2024_1_adhereTOCasympt_combo/test-of-cure_asympt_20combo251combined.csv")
dfTOCadasympt20 <- identify(dfTOCadasympt20, df_best_ends)

dfTOCadasympt40 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_24_2024_1_adhereTOCasympt_combo/test-of-cure_asympt_40combo251combined.csv")
dfTOCadasympt40 <- identify(dfTOCadasympt40, df_best_ends)

dfTOCadasympt60 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_24_2024_1_adhereTOCasympt_combo/test-of-cure_asympt_60combo251combined.csv")
dfTOCadasympt60 <- identify(dfTOCadasympt60, df_best_ends)

dfTOCadasympt70 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_26_2024_1_adhereTOCasympt_combo/test-of-cure_asympt_70combo251combined.csv")
dfTOCadasympt70 <- identify(dfTOCadasympt70, df_best_ends)

dfTOCadasympt80 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_24_2024_1_adhereTOCasympt_combo/test-of-cure_asympt_80combo251combined.csv")
dfTOCadasympt80 <- identify(dfTOCadasympt80, df_best_ends)

dfTOCadasympt90 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_26_2024_1_adhereTOCasympt_combo/test-of-cure_asympt_90combo251combined.csv")
dfTOCadasympt90 <- identify(dfTOCadasympt90, df_best_ends)

dfTOCadasympt100 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_22_2024_1_adhereTOCasympt_combo/test-of-cure_asympt_100combo251combined.csv")
dfTOCadasympt100 <- identify(dfTOCadasympt100, df_best_ends)

compare_five(dfTOCadasympt60,"aSympt Adherence 60%",dfTOCadasympt70,"aSympt Adherence 70%",dfTOCadasympt80,"aSympt Adherence 80%%",dfTOCadasympt90,"aSympt Adherence 90%",dfTOCadasympt100, "aSympt Adherence 100%")
compare_five_cost(dfTOCadasympt60,"aSympt Adherence 60%",dfTOCadasympt70,"aSympt Adherence 70%",dfTOCadasympt80,"aSympt Adherence 80%%",dfTOCadasympt90,"aSympt Adherence 90%",dfTOCadasympt100, "aSympt Adherence 100%")

##############


#july 25-26 2024 counterfactuals and sensitivity analysis
#################

#main text

dfcalibrated  <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_overnight_none_none_10/nonenone101combined.csv")
dfcalibrated <- identify(dfcalibrated, df_best_ends)

#figure 2
visualize_calibration_MSM(dfcalibrated)

dfGISP25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_all_all/GISP_05combo251combined.csv")
dfGISP25 <- identify(dfGISP25, df_best_ends)
dfrandom25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_all_all/randomcombo251combined.csv")
dfrandom25 <- identify(dfrandom25, df_best_ends)

dfTOC25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_all_all/test-of-cure_80combo251combined.csv")
dfTOC25 <- identify(dfTOC25, df_best_ends)

dfDST25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_all_all/drug_sus_testing_80combo251combined.csv")
dfDST25 <- identify(dfDST25, df_best_ends)



#figure 3
summary_plot(dfGISP25, dfrandom25, dfTOC25, dfDST25)


#figure 4
new_figure_four(dfGISP25, dfrandom25, dfTOC25, dfDST25, 25)


#figure 5
multiplot(
  visualize_cea_weighted("A.", df_best_ends, dfGISP25, dfrandom25, dfTOC25, dfDST25)+ 
    theme(legend.position = "bottom", legend.title = element_blank()) ,
  nmb(cea_weighted(df_best_ends, dfGISP25, dfrandom25, dfTOC25, dfDST25), "B."),
  cols = 2
)



#supplement PRCC

setwd("/Users/me597/Documents/MSMoutput/PRCC")

GISPallPRCC <- prcc_all(dfGISP25)
RandomallPRCC <- prcc_all(dfrandom25)
TOCallPRCC <- prcc_all(dfTOC25)
DSTallPRCC <- prcc_all(dfDST25)

write.csv(GISPallPRCC$prevalance, file = "PRCCGISPPrevalence.csv")
write.csv(GISPallPRCC$incidence, file = "PRCCGISPIncidence.csv")
write.csv(GISPallPRCC$symptomprop, file = "PRCCGISPSymptProp.csv")
write.csv(GISPallPRCC$monetaryCost, file = "PRCCGISPMonetaryCost.csv")
write.csv(GISPallPRCC$QALYs, file = "PRCCGISPQALYs.csv")

write.csv(RandomallPRCC$prevalance, file = "PRCCRandomPrevalence.csv")
write.csv(RandomallPRCC$incidence, file = "PRCCRandomIncidence.csv")
write.csv(RandomallPRCC$symptomprop, file = "PRCCRandomSymptProp.csv")
write.csv(RandomallPRCC$monetaryCost, file = "PRCCRandomMonetaryCost.csv")
write.csv(RandomallPRCC$QALYs, file = "PRCCRandomQALYs.csv")

write.csv(TOCallPRCC$prevalance, file = "PRCCTOCPrevalence.csv")
write.csv(TOCallPRCC$incidence, file = "PRCCTOCIncidence.csv")
write.csv(TOCallPRCC$symptomprop, file = "PRCCTOCSymptProp.csv")
write.csv(TOCallPRCC$monetaryCost, file = "PRCCTOCMonetaryCost.csv")
write.csv(TOCallPRCC$QALYs, file = "PRCCTOCQALYs.csv")


write.csv(DSTallPRCC$prevalance, file = "PRCCDSTPrevalence.csv")
write.csv(DSTallPRCC$incidence, file = "PRCCDSTIncidence.csv")
write.csv(DSTallPRCC$symptomprop, file = "PRCCDSTSymptProp.csv")
write.csv(DSTallPRCC$monetaryCost, file = "PRCCDSTMonetaryCost.csv")
write.csv(DSTallPRCC$QALYs, file = "PRCCDSTQALYs.csv")


#supplement parameter values
visualize_parameters(df_best_ends)





#supplement sensitivity analysis

#availability of X

dfGISP10 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_all_combo/GISP_05combo101combined.csv")
dfGISP10 <- identify(dfGISP10, df_best_ends)
dfGISP15 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_all_combo/GISP_05combo151combined.csv")
dfGISP15 <- identify(dfGISP15, df_best_ends)
dfGISP20 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_all_combo/GISP_05combo201combined.csv")
dfGISP20 <- identify(dfGISP20, df_best_ends)
dfGISP25 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_all_all/GISP_05combo251combined.csv")
dfGISP25 <- identify(dfGISP25, df_best_ends)
dfGISP31 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_all_combo/GISP_05combo311combined.csv")
dfGISP31 <- identify(dfGISP31, df_best_ends)

dfrandom10 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_all_combo/randomcombo101combined.csv")
dfrandom10 <- identify(dfrandom10, df_best_ends)
dfrandom15 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_all_combo/randomcombo151combined.csv")
dfrandom15 <- identify(dfrandom15, df_best_ends)
dfrandom20 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_all_combo/randomcombo201combined.csv")
dfrandom20 <- identify(dfrandom20, df_best_ends)
dfrandom25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_all_all/randomcombo251combined.csv")
dfrandom25 <- identify(dfrandom25, df_best_ends)
dfrandom31 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_all_combo/randomcombo311combined.csv")
dfrandom31 <- identify(dfrandom31, df_best_ends)

dfTOC10 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_all_combo/test-of-cure_80combo101combined.csv")
dfTOC10 <- identify(dfTOC10, df_best_ends)
dfTOC15 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_all_combo/test-of-cure_80combo151combined.csv")
dfTOC15 <- identify(dfTOC15, df_best_ends)
dfTOC20 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_all_combo/test-of-cure_80combo201combined.csv")
dfTOC20 <- identify(dfTOC20, df_best_ends)
dfTOC25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_all_all/test-of-cure_80combo251combined.csv")
dfTOC25 <- identify(dfTOC25, df_best_ends)
dfTOC31 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_all_combo/test-of-cure_80combo311combined.csv")
dfTOC31 <- identify(dfTOC31, df_best_ends)

dfDST10 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_all_combo/drug_sus_testing_80combo101combined.csv")
dfDST10 <- identify(dfDST10, df_best_ends)
dfDST15 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_all_combo/drug_sus_testing_80combo151combined.csv")
dfDST15 <- identify(dfDST15, df_best_ends)
dfDST20 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_all_combo/drug_sus_testing_80combo201combined.csv")
dfDST20 <- identify(dfDST20, df_best_ends)
dfDST25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_all_all/drug_sus_testing_80combo251combined.csv")
dfDST25 <- identify(dfDST25, df_best_ends)
dfDST31 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_all_combo/drug_sus_testing_80combo311combined.csv")
dfDST31 <- identify(dfDST31, df_best_ends)

#figure s3.1
giant_summary_plot(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                   dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                   dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                   dfDST10, dfDST15, dfDST20, dfDST25, dfDST31)

#figure s3.2
multiplot(
  
  visualize_cea_weighted("A. Drug X available year 10", df_best_ends,dfGISP15, dfrandom15, dfTOC15, dfDST15)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-600, 500)),
  visualize_cea_weighted("C. Drug X available year 15", df_best_ends,dfGISP20, dfrandom20, dfTOC20, dfDST20)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-600, 500)),
  visualize_cea_weighted("E. Drug X available year 20", df_best_ends,dfGISP25, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-600, 500)),  
  visualize_cea_weighted("G. Drug X never available",df_best_ends, dfGISP31, dfrandom31, dfTOC31, dfDST31)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-600, 500)),
  
  nmb(cea_weighted(df_best_ends,dfGISP15, dfrandom15, dfTOC15, dfDST15), "B."),
  nmb(cea_weighted(df_best_ends,dfGISP20, dfrandom20, dfTOC20, dfDST20), "D."),
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOC25, dfDST25), "F."),
  nmb(cea_weighted(df_best_ends,dfGISP31, dfrandom31, dfTOC31, dfDST31), "H."),
  
  cols = 2
)


new_figure_four(dfGISP15, dfrandom15, dfTOC15, dfDST15, 15)
new_figure_four(dfGISP20, dfrandom20, dfTOC20, dfDST20, 20)
new_figure_four(dfGISP25, dfrandom25, dfTOC25, dfDST25, 25)
new_figure_four(dfGISP31, dfrandom31, dfTOC31, dfDST31, 30)





# switch threshold



dfGISP3 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_switch_combo/GISP_3combo251combined.csv")
dfGISP3 <- identify(dfGISP3, df_best_ends)

dfGISP4 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_switch_combo/GISP_4combo251combined.csv")
dfGISP4 <- identify(dfGISP4, df_best_ends)

dfGISP5 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_switch_combo/GISP_5combo251combined.csv")
dfGISP5 <- identify(dfGISP5, df_best_ends)

dfGISP6 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_switch_combo/GISP_6combo251combined.csv")
dfGISP6 <- identify(dfGISP6, df_best_ends)

dfGISP7 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_switch_combo/GISP_7combo251combined.csv")
dfGISP7 <- identify(dfGISP7, df_best_ends)


#add GISP 3% and GISP 7% instead of 2 and 8

summary_plot_sa(dfGISP3, dfGISP4, dfGISP5, dfGISP6, dfGISP7, 
                c("GISP_3", "GISP_4", "GISP_5", "GISP_6", "GISP_7"), 
                c("GISP 3%", "GISP 4%", "GISP 5%", "GISP 6%", "GISP 7%"), 
                20, 2000)

multiplot(
  
  visualize_cea_weighted("A. GISP 3%", df_best_ends,dfGISP3, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  visualize_cea_weighted("C. GISP 4%", df_best_ends,dfGISP4, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  visualize_cea_weighted("E. GISP 5% (default)", df_best_ends,dfGISP5, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),  
  visualize_cea_weighted("G. GISP 6%",df_best_ends, dfGISP6, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  visualize_cea_weighted("I. GISP 7%",df_best_ends, dfGISP7, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  
  nmb(cea_weighted(df_best_ends,dfGISP3, dfrandom25, dfTOC25, dfDST25), "B."),
  nmb(cea_weighted(df_best_ends,dfGISP4, dfrandom25, dfTOC25, dfDST25), "D."),
  nmb(cea_weighted(df_best_ends,dfGISP5, dfrandom25, dfTOC25, dfDST25), "F."),
  nmb(cea_weighted(df_best_ends,dfGISP6, dfrandom25, dfTOC25, dfDST25), "H."),
  nmb(cea_weighted(df_best_ends,dfGISP7, dfrandom25, dfTOC25, dfDST25), "J."),
  
  cols = 2
)

#rDST coverage
dfDST60 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_availrDST_combo/drug_sus_testing_60combo251combined.csv")
dfDST60 <- identify(dfDST60, df_best_ends)

dfDST70 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_availrDST_combo/drug_sus_testing_70combo251combined.csv")
dfDST70 <- identify(dfDST70, df_best_ends)

dfDST80 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_availrDST_combo/drug_sus_testing_80combo251combined.csv")
dfDST80 <- identify(dfDST80, df_best_ends)

dfDST90 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_availrDST_combo/drug_sus_testing_90combo251combined.csv")
dfDST90 <- identify(dfDST90, df_best_ends)

dfDST100 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_availrDST_combo/drug_sus_testing_100combo251combined.csv")
dfDST100 <- identify(dfDST100, df_best_ends)

summary_plot_sa(dfDST60, dfDST70, dfDST80, dfDST90, dfDST100, 
                c("drug_sus_testing_60", "drug_sus_testing_70", "drug_sus_testing_80", "drug_sus_testing_90", "drug_sus_testing_100"), 
                c("DST 60%", "DST 70%", "DST 80%", "DST 90%", "DST 100%"), 
                20, 600)

multiplot(
  
  visualize_cea_weighted("A. DST 60%", df_best_ends,dfGISP25, dfrandom25, dfTOC25, dfDST60)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  visualize_cea_weighted("C. DST 70%", df_best_ends,dfGISP25, dfrandom25, dfTOC25, dfDST70)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  visualize_cea_weighted("E. DST 80% (default)", df_best_ends,dfGISP25, dfrandom25, dfTOC25, dfDST80)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),  
  visualize_cea_weighted("G. DST 90%",df_best_ends, dfGISP25, dfrandom25, dfTOC25, dfDST90)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  visualize_cea_weighted("I. DST 100%",df_best_ends, dfGISP25, dfrandom25, dfTOC25, dfDST100)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOC25, dfDST60), "B."),
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOC25, dfDST70), "D."),
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOC25, dfDST80), "F."),
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOC25, dfDST90), "H."),
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOC25, dfDST100), "J."),
  
  cols = 2
)




#adhereTOCsympt

dfTOCadsympt60 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_adhereTOCsympt_combo/test-of-cure_sympt_60combo251combined.csv")
dfTOCadsympt60 <- identify(dfTOCadsympt60, df_best_ends)

dfTOCadsympt70 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_adhereTOCsympt_combo/test-of-cure_sympt_70combo251combined.csv")
dfTOCadsympt70 <- identify(dfTOCadsympt70, df_best_ends)

dfTOCadsympt80 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_adhereTOCsympt_combo/test-of-cure_sympt_80combo251combined.csv")
dfTOCadsympt80 <- identify(dfTOCadsympt80, df_best_ends)

dfTOCadsympt90 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_adhereTOCsympt_combo/test-of-cure_sympt_90combo251combined.csv")
dfTOCadsympt90 <- identify(dfTOCadsympt90, df_best_ends)

dfTOCadsympt100 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_adhereTOCsympt_combo/test-of-cure_sympt_100combo251combined.csv")
dfTOCadsympt100 <- identify(dfTOCadsympt100, df_best_ends)

summary_plot_sa(dfTOCadsympt60, dfTOCadsympt70, dfTOCadsympt80, dfTOCadsympt90, dfTOCadsympt100, 
                c("test-of-cure_sympt_60", "test-of-cure_sympt_70", "test-of-cure_sympt_80", "test-of-cure_sympt_90", "test-of-cure_sympt_100"), 
                c("TOC symptomatic 60%", "TOC symptomatic 70%", "TOC symptomatic 80%", "TOC symptomatic 90%", "TOC symptomatic 100%"), 
                40, 100)
multiplot(
  
  visualize_cea_weighted("A. TOC symptomatic 60%", df_best_ends,dfGISP25, dfrandom25, dfTOCadsympt60, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  visualize_cea_weighted("C. TOC symptomatic 70%", df_best_ends,dfGISP25, dfrandom25, dfTOCadsympt70, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  visualize_cea_weighted("E. TOC symptomatic 80% (default)", df_best_ends,dfGISP25, dfrandom25, dfTOCadsympt80, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),  
  visualize_cea_weighted("G. TOC symptomatic 90%",df_best_ends, dfGISP25, dfrandom25, dfTOCadsympt90, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  visualize_cea_weighted("I. TOC symptomatic 100%",df_best_ends, dfGISP25, dfrandom25, dfTOCadsympt100, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOCadsympt60, dfDST25), "B."),
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOCadsympt70, dfDST25), "D."),
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOCadsympt80, dfDST25), "F."),
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOCadsympt90, dfDST25), "H."),
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOCadsympt100, dfDST25), "J."),
  
  cols = 2
)





#adhereTOCasympt

dfTOCadasympt60 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_adhereTOCasympt_combo/test-of-cure_asympt_60combo251combined.csv")
dfTOCadasympt60 <- identify(dfTOCadasympt60, df_best_ends)

dfTOCadasympt70 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_adhereTOCasympt_combo/test-of-cure_asympt_70combo251combined.csv")
dfTOCadasympt70 <- identify(dfTOCadasympt70, df_best_ends)

dfTOCadasympt80 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_adhereTOCasympt_combo/test-of-cure_asympt_80combo251combined.csv")
dfTOCadasympt80 <- identify(dfTOCadasympt80, df_best_ends)

dfTOCadasympt90 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_adhereTOCasympt_combo/test-of-cure_asympt_90combo251combined.csv")
dfTOCadasympt90 <- identify(dfTOCadasympt90, df_best_ends)

dfTOCadasympt100 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_adhereTOCasympt_combo/test-of-cure_asympt_100combo251combined.csv")
dfTOCadasympt100 <- identify(dfTOCadasympt100, df_best_ends)

summary_plot_sa(dfTOCadasympt60, dfTOCadasympt70, dfTOCadasympt80, dfTOCadasympt90, dfTOCadasympt100, 
                c("test-of-cure_asympt_60", "test-of-cure_asympt_70", "test-of-cure_asympt_80", "test-of-cure_asympt_90", "test-of-cure_asympt_100"), 
                c("TOC asymptomatic 60%", "TOC asymptomatic 70%", "TOC asymptomatic 80%", "TOC asymptomatic 90%", "TOC asymptomatic 100%"), 
                50, 200)
multiplot(
  
  visualize_cea_weighted("A. TOC asymptomatic 60%", df_best_ends,dfGISP25, dfrandom25, dfTOCadasympt60, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  visualize_cea_weighted("C. TOC asymptomatic 70%", df_best_ends,dfGISP25, dfrandom25, dfTOCadasympt70, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  visualize_cea_weighted("E. TOC asymptomatic 80% (default)", df_best_ends,dfGISP25, dfrandom25, dfTOCadasympt80, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),  
  visualize_cea_weighted("G. TOC asymptomatic 90%",df_best_ends, dfGISP25, dfrandom25, dfTOCadasympt90, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  visualize_cea_weighted("I. TOC asymptomatic 100%",df_best_ends, dfGISP25, dfrandom25, dfTOCadasympt100, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOCadasympt60, dfDST25), "B."),
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOCadasympt70, dfDST25), "D."),
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOCadasympt80, dfDST25), "F."),
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOCadasympt90, dfDST25), "H."),
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOCadasympt100, dfDST25), "J."),
  
  cols = 2
)








#ISEMPH pres figures

#data

dfcalibrated  <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_overnight_none_none_10/nonenone101combined.csv")
dfcalibrated <- identify(dfcalibrated, df_best_ends)

dfrandom25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_all_all/randomcombo251combined.csv")
dfrandom25 <- identify(dfrandom25, df_best_ends)
dfGISP25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_all_all/GISP_05combo251combined.csv")
dfGISP25 <- identify(dfGISP25, df_best_ends)
dfDST25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_all_all/drug_sus_testing_80combo251combined.csv")
dfDST25 <- identify(dfDST25, df_best_ends)



#slide 9
#calibration figure
visualize_calibration_MSM(dfcalibrated)

#slide 10
#disease burden, RT, GISP, and DST: incidence, failure, ertapenem ("usage of last-line treatment")
summary_plot_isemph(dfrandom25, dfGISP25, dfDST25)

burden_isemph(dfrandom25, dfGISP25, dfDST25)

#slide 11
#resistance RT, GISP, DST
resist_isemph(dfrandom25, dfGISP25, dfDST25)

#zoomed in resistance years 5-10
resist_zoom_isemph(dfrandom25, dfGISP25, dfDST25)



#################




#aug 21st resample, rerun, & reanalyze
############

#still using old sweep
dfsweep <- read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_21_2024_overnight_sweep_none_0/sweepnone0supercombined.csv")
dfsweep$uniqueID <- as.integer(paste(as.character(dfsweep$RunNumber), as.character(dfsweep$seed), sep=''))
df_ends <- calc_weights(dfsweep)
df_best_ends <- resample(df_ends, 1000)

#save the resample including the replicates
write.csv(df_best_ends, file = "/Users/me597/Documents/MSM_calibrated_params/resample_w_replicates21aug24.csv")

df_best_ends_unique <- data.frame(matrix(ncol=length(df_best_ends[1,]), nrow = 0))
colnames(df_best_ends_unique) <- colnames(df_best_ends)

unique_resamples <- unique(df_best_ends$uniqueID)
for (unique_ID in unique_resamples){
  newrow <- first(df_best_ends[df_best_ends$uniqueID == unique_ID,])
  df_best_ends_unique <- rbind(df_best_ends_unique, newrow)
}

best_ends_unique <- identify(df_best_ends_unique, df_best_ends)

df_best_traj <- best_traj(dfsweep,df_best_ends_unique)
df_best_traj <- identify(df_best_traj, df_best_ends)

visualize_calibration_MSM(df_best_traj)
visualize_parameters(df_best_ends)

write_calibrated(df_best_ends_unique)



df_best_ends <- read.csv("/Users/me597/Documents/MSM_calibrated_params/resample_w_replicates21aug24.csv")

#calibration runs
dfcalibrated  <-  read.csv("/Users/me597/Documents/MSMoutput/output_AUGUST_22_2024_1_none_none/nonenone251combined.csv")
dfcalibrated <- identify(dfcalibrated, df_best_ends)

#figure 2
visualize_calibration_MSM(dfcalibrated)

visualize_calibration_MSM(dfsweep)


#main results
directory <- "/Users/me597/Documents/MSMoutput/output_AUGUST_21_2024_1_all_all/"

dfGISP25 <-   read.csv(paste(directory,"GISP_05combo251combined.csv", sep=""))
dfGISP25 <- identify(dfGISP25, df_best_ends)
dfrandom25 <-  read.csv(paste(directory,"randomcombo251combined.csv", sep=""))
dfrandom25 <- identify(dfrandom25, df_best_ends)
dfTOC25 <-  read.csv(paste(directory,"test-of-cure_80combo251combined.csv", sep=""))
dfTOC25 <- identify(dfTOC25, df_best_ends)
dfDST25 <-  read.csv(paste(directory,"drug_sus_testing_80combo251combined.csv", sep=""))
dfDST25 <- identify(dfDST25, df_best_ends)

#figure 3
summary_plot(dfGISP25, dfrandom25, dfTOC25, dfDST25)

#summary data
summary_GISP <- cumulative_everything(dfGISP25)
summary_random <-cumulative_everything(dfrandom25)
summary_TOC <-cumulative_everything(dfTOC25)
summary_DST <-cumulative_everything(dfDST25)

median(summary_GISP$cumulativeFailure) *100
median(summary_random$cumulativeFailure)*100
median(summary_TOC$cumulativeFailure)*100
median(summary_DST$cumulativeFailure)*100

median(summary_GISP$cumulativeE)
median(summary_random$cumulativeE)
median(summary_TOC$cumulativeE)
median(summary_DST$cumulativeE)



#figure 4
new_figure_four(dfGISP25, dfrandom25, dfTOC25, dfDST25, 25)

#figure 5
multiplot(
  visualize_cea_weighted("A.", df_best_ends, dfGISP25, dfrandom25, dfTOC25, dfDST25)+ 
    theme(legend.position = "bottom", legend.title = element_blank()) ,
  nmb(cea_weighted(df_best_ends, dfGISP25, dfrandom25, dfTOC25, dfDST25), "B."),
  cols = 2
)





#supplement sensitivity analysis

#availability of X

directory <- "/Users/me597/Documents/MSMoutput/output_AUGUST_21_2024_1_all_all/"
directoryX <- "/Users/me597/Documents/MSMoutput/output_AUGUST_21_2024_1_all_combo/"

dfGISP15 <-   read.csv(paste(directoryX,"GISP_05combo151combined.csv", sep=""))
dfGISP15 <- identify(dfGISP15, df_best_ends)
dfGISP20 <-   read.csv(paste(directoryX,"GISP_05combo201combined.csv", sep=""))
dfGISP20 <- identify(dfGISP20, df_best_ends)
dfGISP25 <-   read.csv(paste(directory,"GISP_05combo251combined.csv", sep=""))
dfGISP25 <- identify(dfGISP25, df_best_ends)
dfGISP31 <-  read.csv(paste(directoryX,"GISP_05combo311combined.csv", sep=""))
dfGISP31 <- identify(dfGISP31, df_best_ends)


dfrandom15 <-  read.csv(paste(directoryX,"randomcombo151combined.csv", sep=""))
dfrandom15 <- identify(dfrandom15, df_best_ends)
dfrandom20 <-  read.csv(paste(directoryX,"randomcombo201combined.csv", sep=""))
dfrandom20 <- identify(dfrandom20, df_best_ends)
dfrandom25 <-  read.csv(paste(directory,"randomcombo251combined.csv", sep=""))
dfrandom25 <- identify(dfrandom25, df_best_ends)
dfrandom31 <-  read.csv(paste(directoryX,"randomcombo311combined.csv", sep=""))
dfrandom31 <- identify(dfrandom31, df_best_ends)


dfTOC15 <-  read.csv(paste(directoryX,"test-of-cure_80combo151combined.csv", sep=""))
dfTOC15 <- identify(dfTOC15, df_best_ends)
dfTOC20 <-  read.csv(paste(directoryX,"test-of-cure_80combo201combined.csv", sep=""))
dfTOC20 <- identify(dfTOC20, df_best_ends)
dfTOC25 <-  read.csv(paste(directory,"test-of-cure_80combo251combined.csv", sep=""))
dfTOC25 <- identify(dfTOC25, df_best_ends)
dfTOC31 <-  read.csv(paste(directoryX,"test-of-cure_80combo311combined.csv", sep=""))
dfTOC31 <- identify(dfTOC31, df_best_ends)


dfDST15 <-  read.csv(paste(directoryX,"drug_sus_testing_80combo151combined.csv", sep=""))
dfDST15 <- identify(dfDST15, df_best_ends)
dfDST20 <-  read.csv(paste(directoryX,"drug_sus_testing_80combo201combined.csv", sep=""))
dfDST20 <- identify(dfDST20, df_best_ends)
dfDST25 <-  read.csv(paste(directory,"drug_sus_testing_80combo251combined.csv", sep=""))
dfDST25 <- identify(dfDST25, df_best_ends)
dfDST31 <-  read.csv(paste(directoryX,"drug_sus_testing_80combo311combined.csv", sep=""))
dfDST31 <- identify(dfDST31, df_best_ends)

#figure s3.1
giant_summary_plot(dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                   dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                   dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                   dfDST15, dfDST20, dfDST25, dfDST31)

#figure s3.2
multiplot(
  
  visualize_cea_weighted("A. Drug X available year 10", df_best_ends,dfGISP15, dfrandom15, dfTOC15, dfDST15)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-600, 500)),
  visualize_cea_weighted("C. Drug X available year 15", df_best_ends,dfGISP20, dfrandom20, dfTOC20, dfDST20)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-600, 500)),
  visualize_cea_weighted("E. Drug X available year 20", df_best_ends,dfGISP25, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-600, 500)),  
  visualize_cea_weighted("G. Drug X never available",df_best_ends, dfGISP31, dfrandom31, dfTOC31, dfDST31)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-600, 500)),
  
  nmb(cea_weighted(df_best_ends,dfGISP15, dfrandom15, dfTOC15, dfDST15), "B."),
  nmb(cea_weighted(df_best_ends,dfGISP20, dfrandom20, dfTOC20, dfDST20), "D."),
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOC25, dfDST25), "F."),
  nmb(cea_weighted(df_best_ends,dfGISP31, dfrandom31, dfTOC31, dfDST31), "H."),
  
  cols = 2
)


# switch threshold

directory_switch<- "/Users/me597/Documents/MSMoutput/output_AUGUST_21_2024_1_all_all/"

dfGISP3 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_switch_combo/GISP_3combo251combined.csv")
dfGISP3 <- identify(dfGISP3, df_best_ends)

dfGISP4 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_switch_combo/GISP_4combo251combined.csv")
dfGISP4 <- identify(dfGISP4, df_best_ends)

dfGISP5 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_switch_combo/GISP_5combo251combined.csv")
dfGISP5 <- identify(dfGISP5, df_best_ends)

dfGISP6 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_switch_combo/GISP_6combo251combined.csv")
dfGISP6 <- identify(dfGISP6, df_best_ends)

dfGISP7 <-   read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_switch_combo/GISP_7combo251combined.csv")
dfGISP7 <- identify(dfGISP7, df_best_ends)


#add GISP 3% and GISP 7% instead of 2 and 8

summary_plot_sa(dfGISP3, dfGISP4, dfGISP5, dfGISP6, dfGISP7, 
                c("GISP_3", "GISP_4", "GISP_5", "GISP_6", "GISP_7"), 
                c("GISP 3%", "GISP 4%", "GISP 5%", "GISP 6%", "GISP 7%"), 
                20, 2000)

multiplot(
  
  visualize_cea_weighted("A. GISP 3%", df_best_ends,dfGISP3, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  visualize_cea_weighted("C. GISP 4%", df_best_ends,dfGISP4, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  visualize_cea_weighted("E. GISP 5% (default)", df_best_ends,dfGISP5, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),  
  visualize_cea_weighted("G. GISP 6%",df_best_ends, dfGISP6, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  visualize_cea_weighted("I. GISP 7%",df_best_ends, dfGISP7, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  
  nmb(cea_weighted(df_best_ends,dfGISP3, dfrandom25, dfTOC25, dfDST25), "B."),
  nmb(cea_weighted(df_best_ends,dfGISP4, dfrandom25, dfTOC25, dfDST25), "D."),
  nmb(cea_weighted(df_best_ends,dfGISP5, dfrandom25, dfTOC25, dfDST25), "F."),
  nmb(cea_weighted(df_best_ends,dfGISP6, dfrandom25, dfTOC25, dfDST25), "H."),
  nmb(cea_weighted(df_best_ends,dfGISP7, dfrandom25, dfTOC25, dfDST25), "J."),
  
  cols = 2
)

#rDST coverage
dfDST60 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_availrDST_combo/drug_sus_testing_60combo251combined.csv")
dfDST60 <- identify(dfDST60, df_best_ends)

dfDST70 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_availrDST_combo/drug_sus_testing_70combo251combined.csv")
dfDST70 <- identify(dfDST70, df_best_ends)

dfDST80 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_availrDST_combo/drug_sus_testing_80combo251combined.csv")
dfDST80 <- identify(dfDST80, df_best_ends)

dfDST90 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_availrDST_combo/drug_sus_testing_90combo251combined.csv")
dfDST90 <- identify(dfDST90, df_best_ends)

dfDST100 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_availrDST_combo/drug_sus_testing_100combo251combined.csv")
dfDST100 <- identify(dfDST100, df_best_ends)

summary_plot_sa(dfDST60, dfDST70, dfDST80, dfDST90, dfDST100, 
                c("drug_sus_testing_60", "drug_sus_testing_70", "drug_sus_testing_80", "drug_sus_testing_90", "drug_sus_testing_100"), 
                c("DST 60%", "DST 70%", "DST 80%", "DST 90%", "DST 100%"), 
                20, 600)

multiplot(
  
  visualize_cea_weighted("A. DST 60%", df_best_ends,dfGISP25, dfrandom25, dfTOC25, dfDST60)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  visualize_cea_weighted("C. DST 70%", df_best_ends,dfGISP25, dfrandom25, dfTOC25, dfDST70)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  visualize_cea_weighted("E. DST 80% (default)", df_best_ends,dfGISP25, dfrandom25, dfTOC25, dfDST80)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),  
  visualize_cea_weighted("G. DST 90%",df_best_ends, dfGISP25, dfrandom25, dfTOC25, dfDST90)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  visualize_cea_weighted("I. DST 100%",df_best_ends, dfGISP25, dfrandom25, dfTOC25, dfDST100)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOC25, dfDST60), "B."),
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOC25, dfDST70), "D."),
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOC25, dfDST80), "F."),
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOC25, dfDST90), "H."),
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOC25, dfDST100), "J."),
  
  cols = 2
)




#adhereTOCsympt

dfTOCadsympt60 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_adhereTOCsympt_combo/test-of-cure_sympt_60combo251combined.csv")
dfTOCadsympt60 <- identify(dfTOCadsympt60, df_best_ends)

dfTOCadsympt70 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_adhereTOCsympt_combo/test-of-cure_sympt_70combo251combined.csv")
dfTOCadsympt70 <- identify(dfTOCadsympt70, df_best_ends)

dfTOCadsympt80 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_adhereTOCsympt_combo/test-of-cure_sympt_80combo251combined.csv")
dfTOCadsympt80 <- identify(dfTOCadsympt80, df_best_ends)

dfTOCadsympt90 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_adhereTOCsympt_combo/test-of-cure_sympt_90combo251combined.csv")
dfTOCadsympt90 <- identify(dfTOCadsympt90, df_best_ends)

dfTOCadsympt100 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_adhereTOCsympt_combo/test-of-cure_sympt_100combo251combined.csv")
dfTOCadsympt100 <- identify(dfTOCadsympt100, df_best_ends)

summary_plot_sa(dfTOCadsympt60, dfTOCadsympt70, dfTOCadsympt80, dfTOCadsympt90, dfTOCadsympt100, 
                c("test-of-cure_sympt_60", "test-of-cure_sympt_70", "test-of-cure_sympt_80", "test-of-cure_sympt_90", "test-of-cure_sympt_100"), 
                c("TOC symptomatic 60%", "TOC symptomatic 70%", "TOC symptomatic 80%", "TOC symptomatic 90%", "TOC symptomatic 100%"), 
                40, 100)
multiplot(
  
  visualize_cea_weighted("A. TOC symptomatic 60%", df_best_ends,dfGISP25, dfrandom25, dfTOCadsympt60, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  visualize_cea_weighted("C. TOC symptomatic 70%", df_best_ends,dfGISP25, dfrandom25, dfTOCadsympt70, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  visualize_cea_weighted("E. TOC symptomatic 80% (default)", df_best_ends,dfGISP25, dfrandom25, dfTOCadsympt80, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),  
  visualize_cea_weighted("G. TOC symptomatic 90%",df_best_ends, dfGISP25, dfrandom25, dfTOCadsympt90, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  visualize_cea_weighted("I. TOC symptomatic 100%",df_best_ends, dfGISP25, dfrandom25, dfTOCadsympt100, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOCadsympt60, dfDST25), "B."),
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOCadsympt70, dfDST25), "D."),
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOCadsympt80, dfDST25), "F."),
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOCadsympt90, dfDST25), "H."),
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOCadsympt100, dfDST25), "J."),
  
  cols = 2
)





#adhereTOCasympt

dfTOCadasympt60 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_adhereTOCasympt_combo/test-of-cure_asympt_60combo251combined.csv")
dfTOCadasympt60 <- identify(dfTOCadasympt60, df_best_ends)

dfTOCadasympt70 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_adhereTOCasympt_combo/test-of-cure_asympt_70combo251combined.csv")
dfTOCadasympt70 <- identify(dfTOCadasympt70, df_best_ends)

dfTOCadasympt80 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_adhereTOCasympt_combo/test-of-cure_asympt_80combo251combined.csv")
dfTOCadasympt80 <- identify(dfTOCadasympt80, df_best_ends)

dfTOCadasympt90 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_adhereTOCasympt_combo/test-of-cure_asympt_90combo251combined.csv")
dfTOCadasympt90 <- identify(dfTOCadasympt90, df_best_ends)

dfTOCadasympt100 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JULY_25_2024_overnight_adhereTOCasympt_combo/test-of-cure_asympt_100combo251combined.csv")
dfTOCadasympt100 <- identify(dfTOCadasympt100, df_best_ends)

summary_plot_sa(dfTOCadasympt60, dfTOCadasympt70, dfTOCadasympt80, dfTOCadasympt90, dfTOCadasympt100, 
                c("test-of-cure_asympt_60", "test-of-cure_asympt_70", "test-of-cure_asympt_80", "test-of-cure_asympt_90", "test-of-cure_asympt_100"), 
                c("TOC asymptomatic 60%", "TOC asymptomatic 70%", "TOC asymptomatic 80%", "TOC asymptomatic 90%", "TOC asymptomatic 100%"), 
                50, 200)
multiplot(
  
  visualize_cea_weighted("A. TOC asymptomatic 60%", df_best_ends,dfGISP25, dfrandom25, dfTOCadasympt60, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  visualize_cea_weighted("C. TOC asymptomatic 70%", df_best_ends,dfGISP25, dfrandom25, dfTOCadasympt70, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  visualize_cea_weighted("E. TOC asymptomatic 80% (default)", df_best_ends,dfGISP25, dfrandom25, dfTOCadasympt80, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),  
  visualize_cea_weighted("G. TOC asymptomatic 90%",df_best_ends, dfGISP25, dfrandom25, dfTOCadasympt90, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  visualize_cea_weighted("I. TOC asymptomatic 100%",df_best_ends, dfGISP25, dfrandom25, dfTOCadasympt100, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-500, 300)),
  
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOCadasympt60, dfDST25), "B."),
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOCadasympt70, dfDST25), "D."),
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOCadasympt80, dfDST25), "F."),
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOCadasympt90, dfDST25), "H."),
  nmb(cea_weighted(df_best_ends,dfGISP25, dfrandom25, dfTOCadasympt100, dfDST25), "J."),
  
  cols = 2
)

############



##october 2024 
##########
dfsweep <- read.csv("/Users/me597/Documents/MSMoutput/output_OCTOBER_23_2024_overnight_sweep_none/sweepnone0supercombined.csv")
dfsweep$uniqueID <- as.integer(paste(as.character(dfsweep$RunNumber), as.character(dfsweep$seed), sep=''))
df_ends <- calc_weights(dfsweep)
df_best_ends <- resample(df_ends, 1000)

#save the resample including the replicates
write.csv(df_best_ends, file = "/Users/me597/Documents/MSM_calibrated_params/resample_w_replicates23oct24.csv")

df_best_ends_unique <- data.frame(matrix(ncol=length(df_best_ends[1,]), nrow = 0))
colnames(df_best_ends_unique) <- colnames(df_best_ends)

unique_resamples <- unique(df_best_ends$uniqueID)
for (unique_ID in unique_resamples){
  newrow <- first(df_best_ends[df_best_ends$uniqueID == unique_ID,])
  df_best_ends_unique <- rbind(df_best_ends_unique, newrow)
}

best_ends_unique <- identify(df_best_ends_unique, df_best_ends)

df_best_traj <- best_traj(dfsweep,df_best_ends_unique)
df_best_traj <- identify(df_best_traj, df_best_ends)

visualize_calibration_MSM(df_best_traj)
visualize_parameters(df_best_ends)

write_calibrated(df_best_ends_unique)



df_best_ends <- read.csv("/Users/me597/Documents/MSM_calibrated_params/resample_w_replicates23oct24.csv")



#calibration runs
dfcalibrated  <-  read.csv("/Users/me597/Documents/MSMoutput/output_OCTOBER_25_2024_overnight_none_none/nonenone251combined.csv")
dfcalibrated <- identify(dfcalibrated, df_best_ends)

#figure 2
visualize_calibration_MSM(dfcalibrated)

visualize_calibration_MSM(dfsweep)



directory <- "/Users/me597/Documents/MSMoutput/output_OCTOBER_25_2024_overnight_all_all/"

dfGISP25 <-   read.csv(paste(directory,"GISP_05combo251combined.csv", sep=""))
dfGISP25 <- identify(dfGISP25, df_best_ends)
dfrandom25 <-  read.csv(paste(directory,"randomcombo251combined.csv", sep=""))
dfrandom25 <- identify(dfrandom25, df_best_ends)
dfTOC25 <-  read.csv(paste(directory,"test-of-cure_80combo251combined.csv", sep=""))
dfTOC25 <- identify(dfTOC25, df_best_ends)
dfDST25 <-  read.csv(paste(directory,"drug_sus_testing_80combo251combined.csv", sep=""))
dfDST25 <- identify(dfDST25, df_best_ends)
dfreal25 <- read.csv(paste(directory,"realistic_combo_33_33_34combo251combined.csv", sep=""))
dfreal25<-identify(dfreal25, df_best_ends)

#figure 3
new_summary_plot(dfGISP25, dfrandom25, dfTOC25, dfDST25, dfreal25)


summary_plot_smdm(dfGISP25, dfTOC25, dfDST25)

#summary data
summary_GISP <- cumulative_everything(dfGISP25)
summary_random <-cumulative_everything(dfrandom25)
summary_TOC <-cumulative_everything(dfTOC25)
summary_DST <-cumulative_everything(dfDST25)
summary_real <-cumulative_everything(dfreal25)


mean(summary_GISP$cumulativeFailure *100)
quantile(summary_GISP$cumulativeFailure *100, probs = c(0.025, 0.975))

mean(summary_random$cumulativeFailure*100)
quantile(summary_random$cumulativeFailure*100, probs = c(0.025, 0.975))

mean(summary_TOC$cumulativeFailure*100)
quantile(summary_TOC$cumulativeFailure*100, probs = c(0.025, 0.975))

mean(summary_DST$cumulativeFailure*100)
quantile(summary_DST$cumulativeFailure*100, probs = c(0.025, 0.975))

mean(summary_real$cumulativeFailure*100)
quantile(summary_real$cumulativeFailure*100, probs = c(0.025, 0.975))


mean(summary_GISP$cumulativeE)
quantile(summary_GISP$cumulativeE, probs = c(0.025, 0.975))

mean(summary_random$cumulativeE)
quantile(summary_random$cumulativeE, probs = c(0.025, 0.975))

mean(summary_TOC$cumulativeE)
quantile(summary_TOC$cumulativeE, probs = c(0.025, 0.975))

mean(summary_DST$cumulativeE)
quantile(summary_DST$cumulativeE, probs = c(0.025, 0.975))

mean(summary_real$cumulativeE)
quantile(summary_real$cumulativeE, probs = c(0.025, 0.975))


mean(summary_GISP$cumulativeCosts)
quantile(summary_GISP$cumulativeCosts, probs = c(0.025, 0.975))

mean(summary_TOC$cumulativeCosts)
quantile(summary_TOC$cumulativeCosts, probs = c(0.025, 0.975))

mean(summary_DST$cumulativeCosts)
quantile(summary_DST$cumulativeCosts, probs = c(0.025, 0.975))

mean(summary_real$cumulativeCosts)
quantile(summary_real$cumulativeCosts, probs = c(0.025, 0.975))



all_summary<- rbind(summary_GISP, summary_random, summary_TOC, summary_DST, summary_real)

ggplot(all_summary, aes(x=cumulativeE, y = factor(counterfactual, levels = counter_levels))) +
  geom_boxplot(outlier.shape = NA) +
  labs(
    title = "C.",
    y = "",
    x="Cumulative treatments with ertapenem\nper 100,000 over 20 years"
  )+
  my_theme +
  scale_y_discrete(labels=counter_labels)+
  coord_cartesian(xlim=c(0, 5000))


#figure 4
new_figure_four(dfGISP25, dfrandom25, dfTOC25, dfDST25, dfreal25, 25)

#figure 5
multiplot(
  visualize_cea_weighted_real("A.", df_best_ends, dfreal25, dfGISP25, dfrandom25, dfTOC25, dfDST25)+ 
    theme(legend.position = "bottom", legend.title = element_blank()) ,
  nmb(cea_real_weighted(df_best_ends, dfreal25, dfGISP25, dfrandom25, dfTOC25, dfDST25), "B."),
  cols = 2
)






#supplement sensitivity analysis

#availability of X

directory <- "/Users/me597/Documents/MSMoutput/output_OCTOBER_25_2024_overnight_all_all/"
directoryX <- "/Users/me597/Documents/MSMoutput/output_OCTOBER_25_2024_overnight_all_combo/"


dfGISP15 <-   read.csv(paste(directoryX,"GISP_05combo151combined.csv", sep=""))
dfGISP15 <- identify(dfGISP15, df_best_ends)
dfGISP20 <-   read.csv(paste(directoryX,"GISP_05combo201combined.csv", sep=""))
dfGISP20 <- identify(dfGISP20, df_best_ends)

dfGISP31 <-  read.csv(paste(directoryX,"GISP_05combo311combined.csv", sep=""))
dfGISP31 <- identify(dfGISP31, df_best_ends)


dfrandom15 <-  read.csv(paste(directoryX,"randomcombo151combined.csv", sep=""))
dfrandom15 <- identify(dfrandom15, df_best_ends)
dfrandom20 <-  read.csv(paste(directoryX,"randomcombo201combined.csv", sep=""))
dfrandom20 <- identify(dfrandom20, df_best_ends)

dfrandom31 <-  read.csv(paste(directoryX,"randomcombo311combined.csv", sep=""))
dfrandom31 <- identify(dfrandom31, df_best_ends)


dfTOC15 <-  read.csv(paste(directoryX,"test-of-cure_80combo151combined.csv", sep=""))
dfTOC15 <- identify(dfTOC15, df_best_ends)
dfTOC20 <-  read.csv(paste(directoryX,"test-of-cure_80combo201combined.csv", sep=""))
dfTOC20 <- identify(dfTOC20, df_best_ends)

dfTOC31 <-  read.csv(paste(directoryX,"test-of-cure_80combo311combined.csv", sep=""))
dfTOC31 <- identify(dfTOC31, df_best_ends)


dfDST15 <-  read.csv(paste(directoryX,"drug_sus_testing_80combo151combined.csv", sep=""))
dfDST15 <- identify(dfDST15, df_best_ends)
dfDST20 <-  read.csv(paste(directoryX,"drug_sus_testing_80combo201combined.csv", sep=""))
dfDST20 <- identify(dfDST20, df_best_ends)

dfDST31 <-  read.csv(paste(directoryX,"drug_sus_testing_80combo311combined.csv", sep=""))
dfDST31 <- identify(dfDST31, df_best_ends)

directoryX <- "/Users/me597/Documents/MSMoutput/output_OCTOBER_25_2024_overnight_all_combo/"

dfreal15 <-  read.csv(paste(directoryX,"realistic_combocombo151combined.csv", sep=""))
dfreal15<-identify(dfreal15, df_best_ends)
dfreal20<-  read.csv(paste(directoryX,"realistic_combocombo201combined.csv", sep=""))
dfreal20<-identify(dfreal20, df_best_ends)
dfreal31<-  read.csv(paste(directoryX,"realistic_combocombo311combined.csv", sep=""))
dfreal31<-identify(dfreal31, df_best_ends)

#figure s3.1
giant_summary_plot(dfGISP15, dfGISP20, dfGISP25, dfGISP31, 
                   dfrandom15, dfrandom20, dfrandom25, dfrandom31, 
                   dfTOC15, dfTOC20, dfTOC25, dfTOC31,
                   dfDST15, dfDST20, dfDST25, dfDST31,
                   dfreal15, dfreal20, dfreal25, dfreal31)

#figure s3.2
multiplot(
  
  visualize_cea_weighted_real("A. Drug X available year 10", df_best_ends,dfreal15, dfGISP15, dfrandom15, dfTOC15, dfDST15)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-300, 200)),
  visualize_cea_weighted_real("C. Drug X available year 15", df_best_ends,dfreal20, dfGISP20, dfrandom20, dfTOC20, dfDST20)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-300, 200)),
  visualize_cea_weighted_real("E. Drug X available year 20", df_best_ends,dfreal25, dfGISP25, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-300, 200)),  
  visualize_cea_weighted_real("G. Drug X never available",df_best_ends, dfreal31, dfGISP31, dfrandom31, dfTOC31, dfDST31)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-300, 200)),
  
  nmb(cea_real_weighted(df_best_ends,dfreal15, dfGISP15, dfrandom15, dfTOC15, dfDST15), "B.")+ coord_cartesian(ylim = c(-15000000, 10000000)),
  nmb(cea_real_weighted(df_best_ends,dfreal20, dfGISP20, dfrandom20, dfTOC20, dfDST20), "D.")+ coord_cartesian(ylim = c(-15000000, 10000000)),
  nmb(cea_real_weighted(df_best_ends,dfreal25, dfGISP25, dfrandom25, dfTOC25, dfDST25), "F.")+ coord_cartesian(ylim = c(-15000000, 10000000)),
  nmb(cea_real_weighted(df_best_ends,dfreal31, dfGISP31, dfrandom31, dfTOC31, dfDST31), "H.")+ coord_cartesian(ylim = c(-15000000, 10000000)),
  
  cols = 2
)

cea_real_weighted(df_best_ends,dfreal31, dfGISP31, dfrandom31, dfTOC31, dfDST31)


new_figure_four(dfGISP15, dfrandom15, dfTOC15, dfDST15, dfreal15, 15)
new_figure_four(dfGISP20, dfrandom20, dfTOC20, dfDST20, dfreal20, 20)
new_figure_four(dfGISP31, dfrandom31, dfTOC31, dfDST31, dfreal31, 31)


# switch threshold

directory_switch<- "/Users/me597/Documents/MSMoutput/output_OCTOBER_25_2024_overnight_switch_combo/"

dfGISP3 <-   read.csv(paste(directory_switch, "GISP_3combo251combined.csv", sep=""))
dfGISP3 <- identify(dfGISP3, df_best_ends)

dfGISP4 <-   read.csv(paste(directory_switch, "GISP_4combo251combined.csv", sep=""))
dfGISP4 <- identify(dfGISP4, df_best_ends)

dfGISP5 <- dfGISP25

dfGISP6 <-   read.csv(paste(directory_switch, "GISP_6combo251combined.csv", sep=""))
dfGISP6 <- identify(dfGISP6, df_best_ends)

dfGISP7 <-   read.csv(paste(directory_switch, "GISP_7combo251combined.csv", sep=""))
dfGISP7 <- identify(dfGISP7, df_best_ends)


#add GISP 3% and GISP 7% instead of 2 and 8

summary_plot_sa(dfGISP3, dfGISP4, dfGISP5, dfGISP6, dfGISP7, 
                c("GISP_3", "GISP_4", "GISP_5", "GISP_6", "GISP_7"), 
                c("GISP 3%", "GISP 4%", "GISP 5%", "GISP 6%", "GISP 7%"), 
                20, 60000)

multiplot(
  
  visualize_cea_weighted_real("A. GISP 3%", df_best_ends,dfreal25,dfGISP3, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-200, 300)),
  visualize_cea_weighted_real("C. GISP 4%", df_best_ends,dfreal25,dfGISP4, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-200, 300)),
  visualize_cea_weighted_real("E. GISP 5% (default)", df_best_ends,dfreal25,dfGISP5, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-200, 300)),  
  visualize_cea_weighted_real("G. GISP 6%",df_best_ends, dfreal25,dfGISP6, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-200, 300)),
  visualize_cea_weighted_real("I. GISP 7%",df_best_ends, dfreal25,dfGISP7, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-200, 300)),
  
  nmb(cea_real_weighted(df_best_ends,dfreal25,dfGISP3, dfrandom25, dfTOC25, dfDST25), "B.")+ coord_cartesian(ylim = c(-15000000, 10000000)),
  nmb(cea_real_weighted(df_best_ends,dfreal25,dfGISP4, dfrandom25, dfTOC25, dfDST25), "D.")+ coord_cartesian(ylim = c(-15000000, 10000000)),
  nmb(cea_real_weighted(df_best_ends,dfreal25,dfGISP5, dfrandom25, dfTOC25, dfDST25), "F.")+ coord_cartesian(ylim = c(-15000000, 10000000)),
  nmb(cea_real_weighted(df_best_ends,dfreal25,dfGISP6, dfrandom25, dfTOC25, dfDST25), "H.")+ coord_cartesian(ylim = c(-15000000, 10000000)),
  nmb(cea_real_weighted(df_best_ends,dfreal25,dfGISP7, dfrandom25, dfTOC25, dfDST25), "J.")+ coord_cartesian(ylim = c(-15000000, 10000000)),
  
  cols = 2
)

#rDST coverage

directoryDST <- "/Users/me597/Documents/MSMoutput/output_OCTOBER_25_2024_overnight_availrDST_combo/"
dfDST60 <-  read.csv(paste(directoryDST, "drug_sus_testing_60combo251combined.csv", sep=""))
dfDST60 <- identify(dfDST60, df_best_ends)

dfDST70 <-  read.csv(paste(directoryDST,"drug_sus_testing_70combo251combined.csv", sep=""))
dfDST70 <- identify(dfDST70, df_best_ends)

dfDST80 <- dfDST25 

dfDST90 <-  read.csv(paste(directoryDST,"drug_sus_testing_90combo251combined.csv", sep=""))
dfDST90 <- identify(dfDST90, df_best_ends)

dfDST100 <-  read.csv(paste(directoryDST,"drug_sus_testing_100combo251combined.csv", sep=""))
dfDST100 <- identify(dfDST100, df_best_ends)

summary_plot_sa(dfDST60, dfDST70, dfDST80, dfDST90, dfDST100, 
                c("drug_sus_testing_60", "drug_sus_testing_70", "drug_sus_testing_80", "drug_sus_testing_90", "drug_sus_testing_100"), 
                c("DST 60%", "DST 70%", "DST 80%", "DST 90%", "DST 100%"), 
                30, 2500)

multiplot(
  
  visualize_cea_weighted_real("A. DST 60%", df_best_ends,dfreal25, dfGISP25, dfrandom25, dfTOC25, dfDST60)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-200, 50)),
  visualize_cea_weighted_real("C. DST 70%", df_best_ends,dfreal25, dfGISP25, dfrandom25, dfTOC25, dfDST70)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-200, 50)),
  visualize_cea_weighted_real("E. DST 80% (default)", df_best_ends,dfreal25, dfGISP25, dfrandom25, dfTOC25, dfDST80)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-200, 50)),  
  visualize_cea_weighted_real("G. DST 90%",df_best_ends, dfreal25, dfGISP25, dfrandom25, dfTOC25, dfDST90)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-200, 50)),
  visualize_cea_weighted_real("I. DST 100%",df_best_ends, dfreal25, dfGISP25, dfrandom25, dfTOC25, dfDST100)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-200, 50)),
  
  nmb(cea_real_weighted(df_best_ends,dfreal25, dfGISP25, dfrandom25, dfTOC25, dfDST60), "B.")+ coord_cartesian(ylim = c(-15000000, 10000000)),
  nmb(cea_real_weighted(df_best_ends,dfreal25, dfGISP25, dfrandom25, dfTOC25, dfDST70), "D.")+ coord_cartesian(ylim = c(-15000000, 10000000)),
  nmb(cea_real_weighted(df_best_ends,dfreal25, dfGISP25, dfrandom25, dfTOC25, dfDST80), "F.")+ coord_cartesian(ylim = c(-15000000, 10000000)),
  nmb(cea_real_weighted(df_best_ends,dfreal25, dfGISP25, dfrandom25, dfTOC25, dfDST90), "H.")+ coord_cartesian(ylim = c(-15000000, 10000000)),
  nmb(cea_real_weighted(df_best_ends,dfreal25, dfGISP25, dfrandom25, dfTOC25, dfDST100), "J.")+ coord_cartesian(ylim = c(-15000000, 10000000)),
  
  cols = 2
)




#adhereTOCsympt
directoryTOCsympt <- "/Users/me597/Documents/MSMoutput/output_OCTOBER_25_2024_overnight_adhereTOCsympt_combo/"


dfTOCadsympt60 <-  read.csv(paste(directoryTOCsympt, "test-of-cure_sympt_60combo251combined.csv", sep=""))
dfTOCadsympt60 <- identify(dfTOCadsympt60, df_best_ends)

dfTOCadsympt70 <-  read.csv(paste(directoryTOCsympt, "test-of-cure_sympt_70combo251combined.csv", sep=""))
dfTOCadsympt70 <- identify(dfTOCadsympt70, df_best_ends)

dfTOCadsympt80 <-  dfTOC25

dfTOCadsympt90 <-  read.csv(paste(directoryTOCsympt, "test-of-cure_sympt_90combo251combined.csv", sep=""))
dfTOCadsympt90 <- identify(dfTOCadsympt90, df_best_ends)

dfTOCadsympt100 <-  read.csv(paste(directoryTOCsympt, "test-of-cure_sympt_100combo251combined.csv", sep=""))
dfTOCadsympt100 <- identify(dfTOCadsympt100, df_best_ends)

summary_plot_sa(dfTOCadsympt60, dfTOCadsympt70, dfTOCadsympt80, dfTOCadsympt90, dfTOCadsympt100, 
                c("test-of-cure_sympt_60", "test-of-cure_sympt_70", "test-of-cure_sympt_80", "test-of-cure_sympt_90", "test-of-cure_sympt_100"), 
                c("TOC symptomatic 60%", "TOC symptomatic 70%", "TOC symptomatic 80%", "TOC symptomatic 90%", "TOC symptomatic 100%"), 
                100, 40000)
multiplot(
  
  visualize_cea_weighted_real("A. TOC symptomatic 60%", df_best_ends,dfreal25,dfGISP25, dfrandom25, dfTOCadsympt60, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-200, 50)),
  visualize_cea_weighted_real("C. TOC symptomatic 70%", df_best_ends,dfreal25,dfGISP25, dfrandom25, dfTOCadsympt70, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-200, 50)),
  visualize_cea_weighted_real("E. TOC symptomatic 80% (default)", df_best_ends,dfreal25,dfGISP25, dfrandom25, dfTOCadsympt80, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-200, 50)),  
  visualize_cea_weighted_real("G. TOC symptomatic 90%",df_best_ends, dfreal25,dfGISP25, dfrandom25, dfTOCadsympt90, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-200, 50)),
  visualize_cea_weighted_real("I. TOC symptomatic 100%",df_best_ends, dfreal25,dfGISP25, dfrandom25, dfTOCadsympt100, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-200, 50)),
  
  nmb(cea_real_weighted(df_best_ends,dfreal25,dfGISP25, dfrandom25, dfTOCadsympt60, dfDST25), "B.")+ coord_cartesian(ylim = c(-15000000, 10000000)),
  nmb(cea_real_weighted(df_best_ends,dfreal25,dfGISP25, dfrandom25, dfTOCadsympt70, dfDST25), "D.")+ coord_cartesian(ylim = c(-15000000, 10000000)),
  nmb(cea_real_weighted(df_best_ends,dfreal25,dfGISP25, dfrandom25, dfTOCadsympt80, dfDST25), "F.")+ coord_cartesian(ylim = c(-15000000, 10000000)),
  nmb(cea_real_weighted(df_best_ends,dfreal25,dfGISP25, dfrandom25, dfTOCadsympt90, dfDST25), "H.")+ coord_cartesian(ylim = c(-15000000, 10000000)),
  nmb(cea_real_weighted(df_best_ends,dfreal25,dfGISP25, dfrandom25, dfTOCadsympt100, dfDST25), "J.")+ coord_cartesian(ylim = c(-15000000, 10000000)),
  
  cols = 2
)





#adhereTOCasympt
directoryTOCasympt <- "/Users/me597/Documents/MSMoutput/output_OCTOBER_25_2024_overnight_adhereTOCasympt_combo/"


dfTOCadasympt60 <-  read.csv(paste(directoryTOCasympt, "test-of-cure_asympt_60combo251combined.csv", sep=""))
dfTOCadasympt60 <- identify(dfTOCadasympt60, df_best_ends)

dfTOCadasympt70 <-  read.csv(paste(directoryTOCasympt, "test-of-cure_asympt_70combo251combined.csv", sep=""))
dfTOCadasympt70 <- identify(dfTOCadasympt70, df_best_ends)

dfTOCadasympt80 <-  dfTOC25

dfTOCadasympt90 <-  read.csv(paste(directoryTOCasympt, "test-of-cure_asympt_90combo251combined.csv", sep=""))
dfTOCadasympt90 <- identify(dfTOCadasympt90, df_best_ends)

dfTOCadasympt100 <-  read.csv(paste(directoryTOCasympt, "test-of-cure_asympt_100combo251combined.csv", sep=""))
dfTOCadasympt100 <- identify(dfTOCadasympt100, df_best_ends)

summary_plot_sa(dfTOCadasympt60, dfTOCadasympt70, dfTOCadasympt80, dfTOCadasympt90, dfTOCadasympt100, 
                c("test-of-cure_asympt_60", "test-of-cure_asympt_70", "test-of-cure_asympt_80", "test-of-cure_asympt_90", "test-of-cure_asympt_100"), 
                c("TOC asymptomatic 60%", "TOC asymptomatic 70%", "TOC asymptomatic 80%", "TOC asymptomatic 90%", "TOC asymptomatic 100%"), 
                100, 40000)
multiplot(
  
  visualize_cea_weighted_real("A. TOC asymptomatic 60%", df_best_ends,dfreal25,dfGISP25, dfrandom25, dfTOCadasympt60, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-200, 50)),
  visualize_cea_weighted_real("C. TOC asymptomatic 70%", df_best_ends,dfreal25,dfGISP25, dfrandom25, dfTOCadasympt70, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-200, 50)),
  visualize_cea_weighted_real("E. TOC asymptomatic 80% (default)", df_best_ends,dfreal25,dfGISP25, dfrandom25, dfTOCadasympt80, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-200, 50)),  
  visualize_cea_weighted_real("G. TOC asymptomatic 90%",df_best_ends, dfreal25,dfGISP25, dfrandom25, dfTOCadasympt90, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-200, 50)),
  visualize_cea_weighted_real("I. TOC asymptomatic 100%",df_best_ends, dfreal25,dfGISP25, dfrandom25, dfTOCadasympt100, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 20), xlim = c(-200, 50)),
  
  nmb(cea_real_weighted(df_best_ends,dfreal25,dfGISP25, dfrandom25, dfTOCadasympt60, dfDST25), "B.")+ coord_cartesian(ylim = c(-15000000, 10000000)),
  nmb(cea_real_weighted(df_best_ends,dfreal25,dfGISP25, dfrandom25, dfTOCadasympt70, dfDST25), "D.")+ coord_cartesian(ylim = c(-15000000, 10000000)),
  nmb(cea_real_weighted(df_best_ends,dfreal25,dfGISP25, dfrandom25, dfTOCadasympt80, dfDST25), "F.")+ coord_cartesian(ylim = c(-15000000, 10000000)),
  nmb(cea_real_weighted(df_best_ends,dfreal25,dfGISP25, dfrandom25, dfTOCadasympt90, dfDST25), "H.")+ coord_cartesian(ylim = c(-15000000, 10000000)),
  nmb(cea_real_weighted(df_best_ends,dfreal25,dfGISP25, dfrandom25, dfTOCadasympt100, dfDST25), "J.")+ coord_cartesian(ylim = c(-15000000, 10000000)),
  
  cols = 2
)


#RC distribution
directoryRC <- "/Users/me597/Documents/MSMoutput/output_OCTOBER_25_2024_overnight_realistic_combo_combo/"

dfRC20_20_60 <- read.csv(paste(directoryRC, "realistic_combo_20_20_60combo251combined.csv", sep=""))
dfRC20_20_60 <- identify(dfRC20_20_60, df_best_ends)

dfRC20_40_40 <- read.csv(paste(directoryRC, "realistic_combo_20_40_40combo251combined.csv", sep=""))
dfRC20_40_40 <- identify(dfRC20_40_40, df_best_ends)

dfRC20_60_20 <- read.csv(paste(directoryRC, "realistic_combo_20_60_20combo251combined.csv", sep=""))
dfRC20_60_20 <- identify(dfRC20_60_20, df_best_ends)

dfRC40_20_40 <- read.csv(paste(directoryRC, "realistic_combo_40_20_40combo251combined.csv", sep=""))
dfRC40_20_40 <- identify(dfRC40_20_40, df_best_ends)

dfRC40_40_20 <- read.csv(paste(directoryRC, "realistic_combo_40_40_20combo251combined.csv", sep=""))
dfRC40_40_20 <- identify(dfRC40_40_20, df_best_ends)

dfRC60_20_20 <- read.csv(paste(directoryRC, "realistic_combo_60_20_20combo251combined.csv", sep=""))
dfRC60_20_20 <- identify(dfRC60_20_20, df_best_ends)

dfRC33_33_34 <- dfreal25

summary_plot_sa_seven(dfRC33_33_34, dfRC20_20_60, dfRC20_40_40, dfRC20_60_20, dfRC40_20_40, dfRC40_40_20, dfRC60_20_20,
                c("realistic_combo_20_20_60", "realistic_combo_20_40_40", "realistic_combo_20_60_20", "realistic_combo_40_20_40", "realistic_combo_40_40_20", "realistic_combo_60_20_20", "realistic_combo_33_33_34"), 
                c("RC 20-20-60", "RC 20-40-40", "RC 20-60-20", "RC 40-20-40", "RC 40-40-20", "RC 60-20-20", "RC 33-33-34"),  
                70, 11000)

multiplot(
  
  visualize_cea_weighted_real("A. RC 33-33-34", df_best_ends,dfRC33_33_34,dfGISP25, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-200, 50)),
  visualize_cea_weighted_real("C. RC 20-20-60", df_best_ends,dfRC20_20_60,dfGISP25, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-200, 50)),
  visualize_cea_weighted_real("E. RC 20-40-40", df_best_ends, dfRC20_40_40,dfGISP25, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-200, 50)),  
  visualize_cea_weighted_real("G. RC 20-60-20",df_best_ends, dfRC20_60_20,dfGISP25, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-200, 50)),
  visualize_cea_weighted_real("I. RC 40-20-40",df_best_ends, dfRC40_20_40,dfGISP25, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-200, 50)),
  visualize_cea_weighted_real("K. RC 40-40-20",df_best_ends, dfRC40_40_20,dfGISP25, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-200, 50)),
  visualize_cea_weighted_real("M. RC 60-20-20",df_best_ends, dfRC60_20_20,dfGISP25, dfrandom25, dfTOC25, dfDST25)+theme(legend.position = "none")+coord_cartesian(ylim=c(-10, 30), xlim = c(-200, 50)),
  
  
  nmb(cea_real_weighted(df_best_ends,dfRC33_33_34,dfGISP25, dfrandom25, dfTOC25, dfDST25), "B.") + coord_cartesian(ylim = c(-15000000, 10000000)),
  nmb(cea_real_weighted(df_best_ends,dfRC20_20_60,dfGISP25, dfrandom25, dfTOC25, dfDST25), "D.")+ coord_cartesian(ylim = c(-15000000, 10000000)),
  nmb(cea_real_weighted(df_best_ends,dfRC20_40_40,dfGISP25, dfrandom25, dfTOC25, dfDST25), "F.")+ coord_cartesian(ylim = c(-15000000, 10000000)),
  nmb(cea_real_weighted(df_best_ends,dfRC20_60_20,dfGISP25, dfrandom25, dfTOC25, dfDST25), "H.")+ coord_cartesian(ylim = c(-15000000, 10000000)),
  nmb(cea_real_weighted(df_best_ends,dfRC40_20_40,dfGISP25, dfrandom25, dfTOC25, dfDST25), "J.")+ coord_cartesian(ylim = c(-15000000, 10000000)),
  nmb(cea_real_weighted(df_best_ends,dfRC40_40_20,dfGISP25, dfrandom25, dfTOC25, dfDST25), "L.")+ coord_cartesian(ylim = c(-15000000, 10000000)),
  nmb(cea_real_weighted(df_best_ends,dfRC60_20_20,dfGISP25, dfrandom25, dfTOC25, dfDST25), "N.")+ coord_cartesian(ylim = c(-15000000, 10000000)),
  
  cols = 2
)

########



#sdmd
#####

resist_smdm(dfGISP25, dfTOC25, dfDST25, dfreal25)
summary_plot_smdm(dfGISP25, dfTOC25, dfDST25, dfreal25)
######

#MIDAS
######

directory <- "/Users/me597/Documents/MSMoutput/output_OCTOBER_25_2024_overnight_all_all/"

dfGISP25 <-   read.csv(paste(directory,"GISP_05combo251combined.csv", sep=""))
dfGISP25 <- identify(dfGISP25, df_best_ends)
dfrandom25 <-  read.csv(paste(directory,"randomcombo251combined.csv", sep=""))
dfrandom25 <- identify(dfrandom25, df_best_ends)
dfTOC25 <-  read.csv(paste(directory,"test-of-cure_80combo251combined.csv", sep=""))
dfTOC25 <- identify(dfTOC25, df_best_ends)
dfDST25 <-  read.csv(paste(directory,"drug_sus_testing_80combo251combined.csv", sep=""))
dfDST25 <- identify(dfDST25, df_best_ends)
dfreal25 <- read.csv(paste(directory,"realistic_combo_33_33_34combo251combined.csv", sep=""))
dfreal25<-identify(dfreal25, df_best_ends)

summary_plot_smdm(dfGISP25, dfTOC25, dfDST25, dfreal25)

multiplot(
  visualize_cea_weighted_real_nort("A.", df_best_ends, dfreal25, dfGISP25, dfrandom25, dfTOC25, dfDST25)+ 
    theme(legend.position = "bottom", legend.title = element_blank()) ,
  nmb_nort(cea_real_weighted(df_best_ends, dfreal25, dfGISP25, dfrandom25, dfTOC25, dfDST25), "B."),
  cols = 2
)

visualize_cea_weighted_real_nort("", df_best_ends, dfreal25, dfGISP25, dfrandom25, dfTOC25, dfDST25)+ 
  theme(legend.position = "none", 
        legend.title = element_blank(), 
        axis.title = element_blank()) + 
  coord_cartesian(ylim = c(-5, 10), xlim = c(-20,5))

######





######### december 2024

dfsweep <- read.csv("/Users/me597/Documents/MSMoutput/output_DECEMBER_3_2024_debug_sweep_none/sweepnone0supercombined.csv")
dfsweep$uniqueID <- as.integer(paste(as.character(dfsweep$RunNumber), as.character(dfsweep$seed), sep=''))
df_ends <- calc_weights(dfsweep)
df_best_ends <- resample(df_ends, 1000)

#save the resample including the replicates
write.csv(df_best_ends, file = "/Users/me597/Documents/MSM_calibrated_params/resample_w_replicatesdec24.csv")

df_best_ends_unique <- data.frame(matrix(ncol=length(df_best_ends[1,]), nrow = 0))
colnames(df_best_ends_unique) <- colnames(df_best_ends)

unique_resamples <- unique(df_best_ends$uniqueID)
for (unique_ID in unique_resamples){
  newrow <- first(df_best_ends[df_best_ends$uniqueID == unique_ID,])
  df_best_ends_unique <- rbind(df_best_ends_unique, newrow)
}

best_ends_unique <- identify(df_best_ends_unique, df_best_ends)

df_best_traj <- best_traj(dfsweep,df_best_ends_unique)
df_best_traj <- identify(df_best_traj, df_best_ends)

visualize_calibration_MSM(df_best_traj)
visualize_parameters(df_best_ends)

write_calibrated(df_best_ends_unique)



df_best_ends <- read.csv("/Users/me597/Documents/MSM_calibrated_params/resample_w_replicatesdec24.csv")



##########

