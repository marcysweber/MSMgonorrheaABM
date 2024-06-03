


library(ggplot2)
library(tidyverse)
library(data.table)
library(RColorBrewer)
library(egg)
library(epiR)


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
  
  
  target_data = cbind(likelihood_prev = 0, likelihood_detect = 0, likelihood_sympt = 0, combined_log_likelihood = 0, target_data)
  
  #calc likelihood for each target for each year for each trajectory
  target_data$likelihood_prev = prev_binom_likelihood(target_data$Prevalence)
  target_data$likelihood_detect = inc_norm_likelihood(target_data$Detected)
  target_data$likelihood_sympt = sympt_binom_likelihood(target_data$DetectedAndSymptoms/target_data$Detected)
  
  #combine targets for a year for a trajectory
  target_data$combined_log_likelihood <- target_data$likelihood_prev + target_data$likelihood_detect + target_data$likelihood_sympt
  
  target_data$stable_combined_log_likelihood <- target_data$combined_log_likelihood - max(target_data$combined_log_likelihood)
  
  sum_likelihood = sum(exp(target_data$stable_combined_log_likelihood))
  
  data_ends_loc <- target_data %>% filter(tick == 10 * 52)
  
  # to use proper weights for resampling
  for (i in 1:nrow(data_ends_loc)){
    thisrun <- data_ends_loc$uniqueID[i]
    data_ends_loc$weight[i] <- exp(sum(target_data$combined_log_likelihood[target_data$uniqueID == thisrun])) #sum likelihoods across this trajectory
  }
  
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
  
  runs <- unique(df$seed)
  
  for (i in runs){
    thisRunData <- df %>% filter(seed == i)
    
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
  runs <- unique(df$seed)
  
  for (i in runs){
    thisRunData <- df %>% filter(seed == i)
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
  runs <- unique(df$seed)
  
  for (i in runs){
    thisRunData <- df %>% filter(seed == i)
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
  runs <- unique(df$seed)
  
  for (i in runs){
    thisRunData <- df %>% filter(seed == i)
    thisRunCumulative <- sum(discountedX(thisRunData))
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
  runs <- unique(df$seed)
  
  for (i in runs){
    thisRunData <- df %>% filter(seed == i)
    thisRunCumulative <- sum(discountedInc(thisRunData))
    cumulative <- c(cumulative, thisRunCumulative)
  }
  
  return(cumulative)
  
}


cumulative_resist = function(df){
  
  cumulative <- c()
  runs <- unique(df$seed)
  
  for (i in runs){
    thisRunData <- df %>% filter(seed == i)
    thisRunCumulative <- sum(discountedResist(thisRunData))
    cumulative <- c(cumulative, thisRunCumulative)
  }
  
  return(cumulative)
  
}

discounted_fail_A = function(df){
  i <- df$tick/52
  fail <- df$AttemptTreatmentsA - df$SuccessTreatmentsA
  discountedValues <- fail/((1+discountRate)^i)
  return(discountedValues)
}

discounted_fail_B = function(df){
  discountedValues <- c()
  i <- df$tick/52
  fail <- df$AttemptTreatmentsB - df$SuccessTreatmentsB
  discountedValues <- c(discountedValues, fail/((1+discountRate)^i))
  return(discountedValues)
}

discounted_fail_X = function(df){
  discountedValues <- c()
  i <- df$tick/52
  fail <- df$AttemptTreatmentsX - df$SuccessTreatmentsX
  discountedValues <- c(discountedValues, fail/((1+discountRate)^i))
  return(discountedValues)
}

discounted_attempts_A = function(df){
  discountedValues <- c()
  i <- df$tick/52
  discountedValues <- c(discountedValues, df$AttemptTreatmentsA/((1+discountRate)^i))
  return(discountedValues)
}

discounted_attempts_B = function(df){
  discountedValues <- c()
  i <- df$tick/52
  discountedValues <- c(discountedValues, df$AttemptTreatmentsB/((1+discountRate)^i))
  return(discountedValues)
}

discounted_attempts_X = function(df){
  discountedValues <- c()
  i <- df$tick/52
  discountedValues <- c(discountedValues, df$AttemptTreatmentsX/((1+discountRate)^i))
  return(discountedValues)
}

sum_failures = function(df){
  failures_A <- sum(discounted_fail_A(df))
  
  failures_B <- sum(discounted_fail_B(df))
  
  failures_X <- sum(discounted_fail_X(df))

  return(failures_A + failures_B + failures_X)
}

cumulative_failure = function(df){
  
  cumulative <- c()
  runs <- unique(df$RunNumber)
  
  for (i in runs){
    thisRunData <- df %>% filter(RunNumber == i)
    
    total_fails <- sum_failures(thisRunData)
    total_attempts <- sum(discounted_attempts_A(thisRunData)) + sum(discounted_attempts_B(thisRunData)) + sum(discounted_attempts_X(thisRunData))
    
    thisRunCumulative <- total_fails/total_attempts
    cumulative <- c(cumulative, thisRunCumulative)
  }
  
  return(cumulative)
  
}

cumulative_everything = function(df){
  #get ends so that can save results from each separate trajectory
  dfends <- get_ends(df)
  
  #exclude burn-in period
  df <- df %>% filter(tick > 521) 
  
  dfends$cumulativeCosts <- cumulative_costs(df)
  dfends$cumulativeQALYs <- cumulative_QALYs(df)
  dfends$cumulativeX <- cumulative_X(df)
  dfends$cumulativeInc <- cumulative_inc(df)
  dfends$cumulativeResist <- cumulative_resist(df)
  dfends$cumulativeFailure <- cumulative_failure(df)
  
  
  return(dfends)
}

summary_plot = function(df1, df2, df3, df4){
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
  
  inc <- ggplot(data = allends, aes(x=cumulativeInc/1000, y = factor(counterfactual, levels = counter_levels))) +
    geom_boxplot(outlier.shape = NA) +
    labs(
      title = "A.",
      y = "",
      x="Cumulative incidence over 30 years\nper 100,000 (thousands)"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels)+
    coord_cartesian(xlim=c(0, 600))
  
  failure <- ggplot(data = allends, aes(x=cumulativeFailure * 100, y = factor(counterfactual, levels = counter_levels))) +
    geom_boxplot(outlier.shape = NA) +
    labs(
      title = "B.",
      y = "",
      x="% failures per treatment attempt\ncumulative, A, B, & X over 30 years"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels) +
    coord_cartesian(xlim=c(0, 35))
  
  x <- ggplot(data = allends, aes(x=cumulativeX/1000, y = factor(counterfactual, levels = counter_levels))) +
    geom_boxplot(outlier.shape = NA) +
    labs(
      title = "C.",
      y = "",
      x="Treatments with drug X\nover 30 years (thousands)"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels)+
    coord_cartesian(xlim=c(0, 80))
  
  
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
  counter_labels <- c("DST 80%", "Test-of-Cure 80%", "Randomized", "GISP")
  
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
  counter_labels <- c("DST 80%", "Test-of-Cure 80%", "Randomized", "GISP")
  
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


giant_summary_plot = function(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31,
                              dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31,
                              dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31, 
                              dfDST10, dfDST15, dfDST20, dfDST25, dfDST31){
  #process the different avails of X
  df1ends <- cumulative_everything(dfGISP10)
  df2ends <- cumulative_everything(dfrandom10)
  df3ends <- cumulative_everything(dfTOC10)
  df4ends <- cumulative_everything(dfDST10)
  
  
  allends10 <- rbind(df1ends,
                   df2ends,
                   df3ends, 
                   df4ends)
  
  df1ends <- cumulative_everything(dfGISP15)
  df2ends <- cumulative_everything(dfrandom15)
  df3ends <- cumulative_everything(dfTOC15)
  df4ends <- cumulative_everything(dfDST15)
  
  
  allends15 <- rbind(df1ends,
                     df2ends,
                     df3ends, 
                     df4ends)
  
  df1ends <- cumulative_everything(dfGISP20)
  df2ends <- cumulative_everything(dfrandom20)
  df3ends <- cumulative_everything(dfTOC20)
  df4ends <- cumulative_everything(dfDST20)
  
  
  allends20 <- rbind(df1ends,
                     df2ends,
                     df3ends, 
                     df4ends)
  
  df1ends <- cumulative_everything(dfGISP25)
  df2ends <- cumulative_everything(dfrandom25)
  df3ends <- cumulative_everything(dfTOC25)
  df4ends <- cumulative_everything(dfDST25)
  
  
  allends25 <- rbind(df1ends,
                     df2ends,
                     df3ends, 
                     df4ends)
  
  df1ends <- cumulative_everything(dfGISP31)
  df2ends <- cumulative_everything(dfrandom31)
  df3ends <- cumulative_everything(dfTOC31)
  df4ends <- cumulative_everything(dfDST31)
  
  
  allends31 <- rbind(df1ends,
                     df2ends,
                     df3ends, 
                     df4ends)
  
  
  counter_levels <- c("drug_sus_testing_80","test-of-cure_80", "random","GISP")
  counter_labels <- c("DST 80%", "Test-of-Cure 80%", "Randomized", "GISP")
  
  inc10 <- ggplot(data = allends10, aes(x=cumulativeInc/1000, y = factor(counterfactual, levels = counter_levels))) +
    geom_boxplot(outlier.shape = NA) +
    labs(
      title = "A. Drug X available year 10",
      y = "",
      x="Cumulative incidence over 30 years\nper 100,000 (thousands)"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels)+
    coord_cartesian(xlim=c(0, 400))
  
  failure10 <- ggplot(data = allends10, aes(x=cumulativeFailure * 100, y = factor(counterfactual, levels = counter_levels))) +
    geom_boxplot(outlier.shape = NA) +
    labs(
      title = "B.",
      y = "",
      x="% failures per treatment attempt\ncumulative, A, B, & X over 30 years"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels) +
    coord_cartesian(xlim=c(0, 70))
  
  x10 <- ggplot(data = allends10, aes(x=cumulativeX/1000, y = factor(counterfactual, levels = counter_levels))) +
    geom_boxplot(outlier.shape = NA) +
    labs(
      title = "C.",
      y = "",
      x="Treatments with drug X\nover 30 years (thousands)"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels)+
    coord_cartesian(xlim=c(0, 60))
  
  
  
  inc15 <- ggplot(data = allends15, aes(x=cumulativeInc/1000, y = factor(counterfactual, levels = counter_levels))) +
    geom_boxplot(outlier.shape = NA) +
    labs(
      title = "D. Drug X available year 15",
      y = "",
      x="Cumulative incidence over 30 years\nper 100,000 (thousands)"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels)+
    coord_cartesian(xlim=c(0, 400))
  
  failure15 <- ggplot(data = allends15, aes(x=cumulativeFailure * 100, y = factor(counterfactual, levels = counter_levels))) +
    geom_boxplot(outlier.shape = NA) +
    labs(
      title = "E.",
      y = "",
      x="% failures per treatment attempt\ncumulative, A, B, & X over 30 years"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels) +
    coord_cartesian(xlim=c(0, 70))
  
  x15 <- ggplot(data = allends15, aes(x=cumulativeX/1000, y = factor(counterfactual, levels = counter_levels))) +
    geom_boxplot(outlier.shape = NA) +
    labs(
      title = "F.",
      y = "",
      x="Treatments with drug X\nover 30 years (thousands)"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels)+
    coord_cartesian(xlim=c(0, 60))
  
  
  inc20 <- ggplot(data = allends20, aes(x=cumulativeInc/1000, y = factor(counterfactual, levels = counter_levels))) +
    geom_boxplot(outlier.shape = NA) +
    labs(
      title = "G. Drug X available year 20",
      y = "",
      x="Cumulative incidence over 30 years\nper 100,000 (thousands)"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels)+
    coord_cartesian(xlim=c(0, 400))
  
  failure20 <- ggplot(data = allends20, aes(x=cumulativeFailure * 100, y = factor(counterfactual, levels = counter_levels))) +
    geom_boxplot(outlier.shape = NA) +
    labs(
      title = "H.",
      y = "",
      x="% failures per treatment attempt\ncumulative, A, B, & X over 30 years"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels) +
    coord_cartesian(xlim=c(0, 70))
  
  x20 <- ggplot(data = allends20, aes(x=cumulativeX/1000, y = factor(counterfactual, levels = counter_levels))) +
    geom_boxplot(outlier.shape = NA) +
    labs(
      title = "I.",
      y = "",
      x="Treatments with drug X\nover 30 years (thousands)"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels)+
    coord_cartesian(xlim=c(0, 60))
  
  
  inc25 <- ggplot(data = allends25, aes(x=cumulativeInc/1000, y = factor(counterfactual, levels = counter_levels))) +
    geom_boxplot(outlier.shape = NA) +
    labs(
      title = "J. Drug X available year 25",
      y = "",
      x="Cumulative incidence over 30 years\nper 100,000 (thousands)"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels)+
    coord_cartesian(xlim=c(0, 400))
  
  failure25 <- ggplot(data = allends25, aes(x=cumulativeFailure * 100, y = factor(counterfactual, levels = counter_levels))) +
    geom_boxplot(outlier.shape = NA) +
    labs(
      title = "K.",
      y = "",
      x="% failures per treatment attempt\ncumulative, A, B, & X over 30 years"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels) +
    coord_cartesian(xlim=c(0, 70))
  
  x25 <- ggplot(data = allends25, aes(x=cumulativeX/1000, y = factor(counterfactual, levels = counter_levels))) +
    geom_boxplot(outlier.shape = NA) +
    labs(
      title = "L.",
      y = "",
      x="Treatments with drug X\nover 30 years (thousands)"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels)+
    coord_cartesian(xlim=c(0, 60))
  
  
  inc31 <- ggplot(data = allends31, aes(x=cumulativeInc/1000, y = factor(counterfactual, levels = counter_levels))) +
    geom_boxplot(outlier.shape = NA) +
    labs(
      title = "M. Drug X never available",
      y = "",
      x="Cumulative incidence over 30 years\nper 100,000 (thousands)"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels)+
    coord_cartesian(xlim=c(0, 400))
  
  failure31 <- ggplot(data = allends31, aes(x=cumulativeFailure * 100, y = factor(counterfactual, levels = counter_levels))) +
    geom_boxplot(outlier.shape = NA) +
    labs(
      title = "N.",
      y = "",
      x="% failures per treatment attempt\ncumulative, A, B, & X over 30 years"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels) +
    coord_cartesian(xlim=c(0, 70))
  
  x31 <- ggplot(data = allends31, aes(x=cumulativeX/1000, y = factor(counterfactual, levels = counter_levels))) +
    geom_boxplot(outlier.shape = NA) +
    labs(
      title = "O.",
      y = "",
      x="Treatments with drug X\nover 30 years (thousands)"
    )+
    my_theme +
    scale_y_discrete(labels=counter_labels)+
    coord_cartesian(xlim=c(0, 60))
  
  summary <- ggarrange(inc10, failure10 + 
                         theme(axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.title.y = element_blank() ), x10 + 
                         theme(axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.title.y = element_blank() ), 
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
                               axis.title.y = element_blank() ),nrow = 5)
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
  
  newdf <- rbind(newdf, data.frame(seed = ceadf$seed, counterfactual = rep("GISP", 201), AdjustedCost = ceadf$GISPcumulativeCostsAdj, AdjustedQALYs = ceadf$GISPcumulativeQALYsAdj))
  newdf <- rbind(newdf, data.frame(seed = ceadf$seed, counterfactual=rep("Random", 201), AdjustedCost =ceadf$RandomcumulativeCostsAdj, AdjustedQALYs = -ceadf$RandomcumulativeQALYsAdj))
  newdf <- rbind(newdf, data.frame(seed = ceadf$seed, counterfactual=rep("TOC", 201), AdjustedCost =ceadf$TOCcumulativeCostsAdj, AdjustedQALYs = -ceadf$TOCcumulativeQALYsAdj))
  newdf <- rbind(newdf, data.frame(seed = ceadf$seed, counterfactual=rep("DST", 201), AdjustedCost =ceadf$DSTcumulativeCostsAdj, AdjustedQALYs = -ceadf$DSTcumulativeQALYsAdj))
  
  return(newdf)
}

visualize_cea = function(title, df1, df2, df3, df4){
ceadf_sum <- cea(df1, df2, df3, df4)
ceadf_sum <- rbind(ceadf_sum, lapply(ceadf_sum[], mean))
ceadf_sum$seed[51] <- "mean"

ceadf <- rearrange_cea(ceadf_sum)

counter_levels <- c("GISP", "Random", "TOC", "DST")
counter_labels <- c("GISP", "Randomized", "TOC 80%", "DST 80%")

ceadf$counterfactual <- factor(ceadf$counterfactual, levels=counter_levels, labels=counter_labels)

ggplot() +
  geom_hline(yintercept=0, color = "gray") +
  geom_vline(xintercept = 0, color = "gray") +
  geom_point(data=ceadf, aes(x=AdjustedQALYs, y=AdjustedCost/1000000, color=counterfactual), size = 0.2, show.legend = FALSE) +
  geom_point(data=ceadf[ceadf$seed=="mean",], aes(x=AdjustedQALYs, y=AdjustedCost/1000000, fill=counterfactual), size = 2, shape = 23) +
  labs(title=title,
       x="Difference in QALYs relative to GISP",
       y="Change in cost relative to GISP (in millions USD)",
       fill="Counterfactual")+
  scale_fill_brewer(palette="Set2")+
  scale_color_brewer(palette="Set2")+
 # coord_cartesian(xlim=c(0, 20), ylim=c(-20, 40))+
  my_theme 
}


visualize_cea_MSM = function(title, df1, df2, df3, df4){
  ceadf_sum <- ceaMSM(df1, df2, df3, df4)
  ceadf_sum <- rbind(ceadf_sum, lapply(ceadf_sum[], mean))
  ceadf_sum$seed[51] <- "mean"
  
  ceadf <- rearrange_cea(ceadf_sum)
  
  counter_levels <- c("GISP", "Random", "TOC", "DST")
  counter_labels <- c("GISP", "Randomized", "TOC 80%", "DST 80%")
  
  ceadf$counterfactual <- factor(ceadf$counterfactual, levels=counter_levels, labels=counter_labels)
  
  ggplot() +
    geom_hline(yintercept=0, color = "gray") +
    geom_vline(xintercept = 0, color = "gray") +
    geom_point(data=ceadf, aes(x=AdjustedQALYs, y=AdjustedCost/1000000, color=counterfactual), size = 0.2, show.legend = FALSE) +
    geom_point(data=ceadf[ceadf$seed=="mean",], aes(x=AdjustedQALYs, y=AdjustedCost/1000000, fill=counterfactual), size = 2, shape = 23) +
    labs(title=title,
         x="Difference in QALYs relative to GISP",
         y="Change in cost relative to GISP (in millions USD)",
         fill="Counterfactual")+
    scale_fill_brewer(palette="Set2")+
    scale_color_brewer(palette="Set2")+
    #coord_cartesian(xlim=c(0, 20), ylim=c(-20, 40))+
    my_theme 
}

nmb = function(cea_df, title){
  random_intercepts <- t.test(cea_df$RandomcumulativeCostsAdj)
  random_mean_intercept <- random_intercepts$estimate
  random_lower_intercept <- random_intercepts$conf.int[1]
  random_upper_intercept <- random_intercepts$conf.int[2]
  
  random_slopes <- t.test(cea_df$RandomcumulativeQALYsAdj)
  random_mean_slope <- random_slopes$estimate
  random_lower_slope <- random_slopes$conf.int[1]
  random_upper_slope <- random_slopes$conf.int[2]
  
  TOC_intercepts<- t.test(cea_df$TOCcumulativeCostsAdj)
  TOC_mean_intercept <- TOC_intercepts$estimate
  TOC_lower_intercept <- TOC_intercepts$conf.int[1]
  TOC_upper_intercept <- TOC_intercepts$conf.int[2]
  TOC_slopes <- t.test(cea_df$TOCcumulativeQALYsAdj)
  TOC_mean_slope <- TOC_slopes$estimate
  TOC_lower_slope <- TOC_slopes$conf.int[1]
  TOC_upper_slope <- TOC_slopes$conf.int[2]
  
  DST_intercepts<- t.test(cea_df$DSTcumulativeCostsAdj)
  DST_mean_intercept <- DST_intercepts$estimate
  DST_lower_intercept <- DST_intercepts$conf.int[1]
  DST_upper_intercept <- DST_intercepts$conf.int[2]
  DST_slopes <- t.test(cea_df$DSTcumulativeQALYsAdj)
  DST_mean_slope <- DST_slopes$estimate
  DST_lower_slope <- DST_slopes$conf.int[1]
  DST_upper_slope <- DST_slopes$conf.int[2]
  
  ggplot(data = cea_df) + 
   # geom_point(aes(x=c(0,-1), y = c(0, -1)))+
    ylim(-20000000, 15000000) +
    scale_x_continuous(expand = c(0, 0), limits=c(0, 160000), breaks=c(0, 50000, 100000, 150000))+
    geom_abline(aes(slope = 0, intercept = 0), color = "#66C2A5")+
    
    geom_abline(aes(slope = -random_mean_slope, intercept=-random_mean_intercept),color="#FC8D62") +
    geom_abline(aes(slope = -random_lower_slope, intercept=-random_lower_intercept),linetype=2,color="#FC8D62") +
    geom_abline(aes(slope = -random_upper_slope, intercept=-random_upper_intercept),linetype=2,color="#FC8D62") +
    
    geom_abline(aes(slope = -TOC_mean_slope, intercept=-TOC_mean_intercept), color = "#8DA0CB") +
    geom_abline(aes(slope = -TOC_lower_slope, intercept=-TOC_lower_intercept),linetype=2, color = "#8DA0CB") +
    geom_abline(aes(slope = -TOC_upper_slope, intercept=-TOC_upper_intercept),linetype=2, color = "#8DA0CB") +
    
    geom_abline(aes(slope = -DST_mean_slope, intercept=-DST_mean_intercept), color = "#E78AC3") +
    geom_abline(aes(slope = -DST_lower_slope, intercept=-DST_lower_intercept),linetype=2, color = "#E78AC3") +
    geom_abline(aes(slope = -DST_upper_slope, intercept=-DST_upper_intercept),linetype=2, color = "#E78AC3") +
    my_theme+
    labs(
      title=title,
      x = "Willingness to Pay for each QALY averted (USD)",
      y = "Expected Net Monetary Benefit (USD)"
    )
    
}

ggplot(data = cea_example) + 
  # geom_point(aes(x=c(0,-1), y = c(0, -1)))+
  ylim(-1000000, 1000000) + xlim(0, 150000)+
  geom_abline(aes(slope = random_mean_slope, intercept=-random_mean_intercept)) +
  geom_abline(aes(slope = random_lower_slope, intercept=-random_lower_intercept),linetype=2) +
  geom_abline(aes(slope = random_upper_slope, intercept=-random_upper_intercept),linetype=2) 
  

random_intercepts <- t.test(cea_example$RandomcumulativeCostsAdj)
random_mean_intercept <- random_intercepts$estimate
random_lower_intercept <- random_intercepts$conf.int[1]
random_upper_intercept <- random_intercepts$conf.int[2]

random_slopes <- t.test(cea_example$RandomcumulativeQALYsAdj)
random_mean_slope <- random_slopes$estimate
random_lower_slope <- random_slopes$conf.int[1]
random_upper_slope <- random_slopes$conf.int[2]

TOC_intercepts
TOC_slopes

DST_intercepts
DST_slopes

############

#individual panel functions: "viz_"
########

my_theme = theme_bw(base_size = 8)
 #*
viz_prev_cal = function(df, title){
  df <- df %>% filter(tick > 260)
  prev <- ggplot(data = df, aes(x = tick / 52, group = RunNumber)) + 
    geom_line(aes(y = prevMSM),size = 0.05, color = "black") +
    geom_point(aes(y=4.5, x = 6), color="red", size = 1) +
    geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 6), color = "red")+
    geom_point(aes(y=4.5, x = 7), color="red", size = 1) +
    geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 7), color = "red")+
    geom_point(aes(y=4.5, x = 8), color="red", size = 1) +
    geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 8), color = "red")+
    geom_point(aes(y=4.5, x = 9), color="red", size = 1) +
    geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 9), color = "red")+
    geom_point(aes(y=4.5, x = 10), color="red", size = 1) +
    geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 10), color = "red")+
    labs(title = title,
         x = "Year",
         y = "Prevalence (%) in MSM") +
    theme(plot.title = element_text(size=8)) +
    coord_cartesian(ylim=c(0,20), xlim=c(5, 30))+
    my_theme
  return(prev)
}


viz_incMSM_cal = function(df, title){
  df <- df %>% filter(tick > 260)
  inc <- ggplot(data = df, aes(x = tick / 52, y = 100000 * (Detected / 100000), group = RunNumber)) + 
    geom_line( aes(y = 100000 * (Detected / 100000)),size = 0.05, color="black") +
    geom_point(aes(y=6508, x = 6), color="red", size = 1) +
    geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 6), color = "red")+
    geom_point(aes(y=6508, x = 7), color="red", size = 1) +
    geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 7), color = "red")+
    geom_point(aes(y=6508, x = 8), color="red", size = 1) +
    geom_errorbar(aes(ymin = 5206, ymax = 7809,x = 8), color = "red")+
    geom_point(aes(y=6508, x = 9), color="red", size = 1) +
    geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 9), color = "red")+
    geom_point(aes(y=6508, x = 10), color="red", size = 1) +
    geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 10), color = "red")+
    labs(title = title,
         x = "Year",
         y = "Cases Detected per 100,000 MSM")+
    theme(plot.title = element_text(size=8)) +
    coord_cartesian(ylim= c(0,55000),xlim=c(5,30))+
    my_theme
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
  df <- df %>% filter(tick > 260)
  symptomatic <- ggplot(data = df, aes(x = tick / 52, y = DetectedAndSymptoms / Detected, group = RunNumber)) + 
    geom_line( aes(y = DetectedAndSymptoms / Detected),size = 0.05, color="black") +
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
   # scale_y_continuous(limits = c(0.0, 1.0), labels = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) + #ylim(0.4, 0.9) +
    my_theme
  
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
  df <- df %>% filter(tick > 260)
  prev <- ggplot(data = df, aes(x = tick / 52, group = seed)) + 
     geom_line(aes(y = Prevalence),linewidth = 0.05, color = "black") +
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
    theme(plot.title = element_text(size=8)) +
    geom_vline(xintercept=yearX, linetype="dashed")+
    coord_cartesian(ylim=c(0,20), xlim=c(5, 30))+
    my_theme
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
  df <- df %>% filter(tick > 260)
  prev <- ggplot(data = df, aes(x = tick / 52, group = RunNumber)) + 
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
  df <- df %>% filter(tick > 260)
  prev <- ggplot(data = df, aes(x = tick / 52, group = RunNumber)) + 
    geom_line(aes(y = Prevalence),linewidth = 0.05, color = "black") +
    geom_point(aes(y=4.5, x = 6), color="red", size = 1) +
    geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 6), color = "red")+
    geom_point(aes(y=4.5, x = 7), color="red", size = 1) +
    geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 7), color = "red")+
    geom_point(aes(y=4.5, x = 8), color="red", size = 1) +
    geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 8), color = "red")+
    geom_point(aes(y=4.5, x = 9), color="red", size = 1) +
    geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 9), color = "red")+
    geom_point(aes(y=4.5, x = 10), color="red", size = 1) +
    geom_errorbar(aes(ymin = 3.6, ymax = 5.4, x = 10), color = "red")+
    labs(title = title,
         x = "Year",
         y = "Prevalence (%) in MSM") +
    theme(plot.title = element_text(size=8)) +
    #geom_vline(xintercept=yearX, linetype="dashed")+
    coord_cartesian(ylim=c(0,20), xlim=c(5, 30))+
    my_theme
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
  df <- df %>% filter(tick > 260)
  inc <- ggplot(data = df, aes(x = tick / 52, y = Detected, group = RunNumber)) + 
    geom_line( aes(y = Detected),size = 0.05, color="black") +
    geom_point(aes(y=6508, x = 6), color="red", size = 1) +
    geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 6), color = "red")+
    geom_point(aes(y=6508, x = 7), color="red", size = 1) +
    geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 7), color = "red")+
    geom_point(aes(y=6508, x = 8), color="red", size = 1) +
    geom_errorbar(aes(ymin = 5206, ymax = 7809,x = 8), color = "red")+
    geom_point(aes(y=6508, x = 9), color="red", size = 1) +
    geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 9), color = "red")+
    geom_point(aes(y=6508, x = 10), color="red", size = 1) +
    geom_errorbar(aes(ymin = 5206, ymax = 7809, x = 10), color = "red")+
    labs(title = title,
         x = "Year",
         y = "Cases Detected per 100,000 MSM")+
    theme(plot.title = element_text(size=8)) +
    coord_cartesian(ylim= c(0,55000),xlim=c(5,30))+
    geom_vline(xintercept=yearX, linetype="dashed")+
  
    my_theme
  return(inc)
}

viz_true_inc = function(df, title, yearX){
  df <- df %>% filter(tick > 520)
  inc <- ggplot(data = df, aes(x = tick / 52, y = Detected, group = RunNumber)) + 
    geom_line( aes(y = Incidence),size = 0.05, color="black") +
   
    labs(title = title,
         x = "Year",
         y = "Incidence per 100,000 MSM")+
    theme(plot.title = element_text(size=8)) +
    coord_cartesian(ylim= c(0,75000))+
    geom_vline(xintercept=yearX, linetype="dashed")+
    my_theme
  return(inc)
}

viz_symptomatic = function(df, title){
  df <- df %>% filter(tick > 260)
  symptomatic <- ggplot(data = df, aes(x = tick / 52, y = DetectedAndSymptoms / Detected, group = RunNumber)) + 
    geom_line( aes(y = DetectedAndSymptoms / Detected),size = 0.05, color="black") +
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
    my_theme
  
  return(symptomatic)
}

viz_treatments = function(df, title){
  df <- df %>% filter(tick > 260)
  treatments <- ggplot(data=df, aes(x=tick / 52, group=RunNumber))+
    geom_line(aes(y = Treatments), size = 0.05, color = "black") +
    geom_line(aes(y = FailedTreatments), size = 0.05, color = "red")+
    labs(x = "Year",
         y = "Count Treatments (annually)") +
    ylim(0,16000)+
    my_theme
  
  return(treatments)
}

#*
viz_true_resist_A = function(df, title, yearX){
  df <- df %>% filter(tick > 520)
  amrA <- ggplot(data = df, aes(x = tick / 52, group = RunNumber)) + 
    geom_line( aes(y = ResistAIncidence/Incidence),size = 0.05, color="black") +
    labs(x = "Year",
         y = "Proportion cases resistant drug A",
         title = title) +
    coord_cartesian(ylim=c(0.0, 1.0)) +
    geom_vline(xintercept=yearX, linetype="dashed")+
    my_theme
  return(amrA)
}
#*
viz_true_resist_B = function(df, title, yearX){
  df <- df %>% filter(tick > 520)
  amrB <- ggplot(data = df, aes(x = tick / 52, group = RunNumber)) + 
    geom_line( aes(y = ResistBIncidence/Incidence),size = 0.05, color="black") +
    labs(x = "Year",
         y = "Proportion cases resistant drug B", 
         title = title) +
    coord_cartesian(ylim=c(0.0, 1.0)) +
    geom_vline(xintercept=yearX, linetype="dashed")+
    my_theme
  return(amrB)
}
#*
viz_true_resist_both = function(df, title, yearX){
  df <- df %>% filter(tick > 520)
  
  amrB <- ggplot(data = df, aes(x = tick / 52, group = RunNumber)) + 
    geom_line( aes(y = ResistBothIncidence/Incidence),size = 0.05, color="black") +
    labs(x = "Year",
         y = "Proportion cases resistant both drugs", 
         title = title) +
    coord_cartesian(ylim=c(0.0, 1.0)) +
    geom_vline(xintercept=yearX, linetype="dashed")+
    my_theme
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
    geom_line(aes(y = SuccessTreatmentsX/AttemptTreatmentsX * 100), size = 0.05, color = "black") +
    labs(x = "Year",
         y = "% treatments successful with X", 
         title = title) +
    ylim(0.0, 100) +
    my_theme
  return(plotX)
}


#*
viz_attempts_A = function(df, title, yearX){
  df <- df %>% filter(tick > 520)
  
  plotA <- ggplot(data = df, aes(x = tick/52, group = RunNumber)) +
    geom_line(aes(y = AttemptTreatmentsA), size = 0.05, color = "black") +
    labs(x = "Year",
         y = "Count treatments with A", 
         title = title) +
    coord_cartesian(ylim = c(0, 60000))+
    geom_vline(xintercept=yearX, linetype="dashed")+
    my_theme
  return(plotA)
}
#*
viz_attempts_B = function(df, title, yearX){
  df <- df %>% filter(tick > 520)
  
  plotB <- ggplot(data = df, aes(x = tick/52, group = RunNumber)) +
    geom_line(aes(y = AttemptTreatmentsB), size = 0.05, color = "black") +
    labs(x = "Year",
         y = "Count treatments with B", 
         title = title) +
    coord_cartesian(ylim = c(0, 60000))+
    geom_vline(xintercept=yearX, linetype="dashed")+
    my_theme
  return(plotB)
}
#*
viz_attempts_X = function(df, title, yearX){
  df <- df %>% filter(tick > 520)
  
  plotX <- ggplot(data = df, aes(x = tick/52, group = RunNumber)) +
    geom_line(aes(y = AttemptTreatmentsX ), size = 0.05, color = "black") +
    labs(x = "Year",
         y = "Count treatments with X", 
         title = title) +
    coord_cartesian(ylim = c(0, 60000))+
    geom_vline(xintercept=yearX, linetype="dashed")+
    my_theme
  return(plotX)
}
#*
viz_E = function(df, title, yearX, ylim){
  df <- df %>% filter(tick > 520)
  
  plotX <- ggplot(data = df, aes(x = tick/52, group = RunNumber)) +
    geom_line(aes(y = UsageofErtapenem ), size = 0.05, color = "black") +
    labs(x = "Year",
         y = "Count treatments with ertapenem", 
         title = title) +
    coord_cartesian(ylim = c(0, ylim))+
    geom_vline(xintercept=yearX, linetype="dashed")+
    my_theme
  return(plotX)
}


#viz_success_A_of_all

#viz_success_B_of_all

#viz_success_X_of_all


#*
viz_cost = function(df, title, yearX){
  df <- df %>% filter(tick > 260)
  
  cost <- ggplot(data=df[df$tick!=0,], aes(x=tick/52, group = RunNumber))+
    geom_line(aes(y=AnnualMonetaryCost/1000000), size = 0.05, color = "black")+
    labs(title = title, 
         x = "Year",
         y = "Cost in Millions of Dollars (annually)") +
    coord_cartesian(ylim= c(0, 15))+
    #xlim(0,25)+
    geom_vline(xintercept=yearX, linetype="dashed")+
    my_theme
  return(cost)
}

viz_surveillanceA = function(df, title){
  df <- df %>% filter(tick > 520)
  
  surveillance <- ggplot(data = df, aes(x=tick/52, group = RunNumber))+
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
    xlim(0,20)
  return(failed)
}

#*
viz_all_failed = function(df, title, yearX){
  df <- df %>% filter(tick > 520)
  
  df$FailureRate <- calc_failure_rate(df)
  
  failed <- ggplot(data = df, aes(x=tick/52, group = RunNumber))+
    geom_line(aes(y=FailureRate), linewidth = 0.05, color = "black")+
    my_theme+
    labs(title=title, x= "Year", y = "Failure rate, all treatments")+
    geom_vline(xintercept=yearX, linetype="dashed")+
    
   coord_cartesian(ylim=c(0, 1))
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
  
  q <- ggplot(data = df, aes(x = tick / 52, y = AnnualQALYsLost, group = RunNumber))+
    my_theme+
    geom_line(linewidth = 0.05, color = "black") +
    labs(title = title,
         y = "Annual QALYs Lost per 100,000", 
         x = "Year") +
    geom_vline(xintercept=yearX, linetype="dashed")+
    coord_cartesian(ylim=c(0, 100))
  
  
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
  a <- ggplot(df) + 
    geom_histogram(aes(x = TransmissionM), color = "black", fill = "darkgrey", binwidth = 0.5) +
    labs(x = "TransmissionM",
         y = "Count", 
         title = "A.") +
    xlim(0, 10)+
    theme_bw()
  
  b <- ggplot(df) + 
    geom_histogram(aes(x = TransmissionF), color = "black", fill = "darkgrey", binwidth = 0.5) +
    labs(x = "TransmissionF",
         y = "Count", 
         title = "B.") +
    xlim(0, 10)+
    theme_bw()
  
  
    
  c <- ggplot(df) + 
    geom_histogram(aes(x = RecoveryLambda), color = "black", fill = "darkgrey", binwidth = 0.2) +
    labs(x = "RecoveryLambda",
         y = "Count", 
         title = "C.") +
    xlim(0, 4)+ 
    theme_bw()
    
  d <- ggplot(df) + 
    geom_histogram(aes(x = ProbSymptomaticM), color = "black", fill = "darkgrey", binwidth = 0.05) +
    labs(x = "ProbSymptomaticM",
         y = "Count", 
         title = "D.") +
    xlim(0, 0.9)+ 
    theme_bw()
    
  e <- ggplot(df) + 
    geom_histogram(aes(x = ProbSymptomaticF), color = "black", fill = "darkgrey", binwidth = 0.05) +
    labs(x = "ProbSymptomaticF",
         y = "Count", 
         title = "E.") +
    xlim(0, 0.9)+ 
    theme_bw()
  
  f <-  ggplot(df) + 
    geom_histogram(aes(x = ScreenIntervalMSM), color = "black", fill = "darkgrey", binwidth = 0.5) +
    labs(x = "ScreenIntervalMSM",
         y = "Count", 
         title = "F.") +
    xlim(0, 5.5)+ 
    theme_bw()
  
  g <-  ggplot(df) + 
    geom_histogram(aes(x = ScreenIntervalMSW), color = "black", fill = "darkgrey", binwidth = 0.5) +
    labs(x = "ScreenIntervalMSW",
         y = "Count", 
         title = "G.") +
    xlim(0, 5.5)+ 
    theme_bw()
  
  h <-  ggplot(df) + 
    geom_histogram(aes(x = ScreenIntervalW), color = "black", fill = "darkgrey", binwidth = 0.5) +
    labs(x = "ScreenIntervalW",
         y = "Count", 
         title = "H.") +
    xlim(0, 5.5)+ 
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


new_figure_five = function(df1, df2, df3, df4){
  
  multiplot(
    viz_prev(df1, "A. GISP", 25),
    viz_inc(df1, "E.", 25),
    viz_true_resist_A(df1, "I.", 25), 
    viz_true_resist_B(df1, "M.", 25),
    viz_true_resist_both(df1, "Q.", 25), 

    viz_prev(df2, "B. Randomized", 25),
    viz_inc(df2, "F.", 25),
    viz_true_resist_A(df2, "J.", 25), 
    viz_true_resist_B(df2, "N.", 25),
    viz_true_resist_both(df2, "R.", 25), 

    viz_prev(df3, "C. Test-of-Cure", 25),
    viz_inc(df3, "G.", 25),
    viz_true_resist_A(df3, "K.", 25), 
    viz_true_resist_B(df3, "O.", 25),
    viz_true_resist_both(df3, "S.", 25), 

    viz_prev(df4, "D. DST", 25),
    viz_inc(df4, "H.", 25),
    viz_true_resist_A(df4, "L.", 25), 
    viz_true_resist_B(df4, "P.", 25),
    viz_true_resist_both(df4, "T.", 25), 

    cols = 4)
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
  
    viz_all(dfGISP10, "A. GISP", 10, 1000), 
    viz_E(dfGISP15, "E.", 15, 1000),
    viz_E(dfGISP20, "I.", 20, 1000),
    viz_E(dfGISP25, "M.", 25, 16000),
    viz_E(dfGISP31, "Q.", 30, 100000),
    
    viz_E(dfrandom10, "B. Random Treatment", 10, 1000), 
    viz_E(dfrandom15, "F.", 15, 1000),
    viz_E(dfrandom20, "J.", 20, 1000),
    viz_E(dfrandom25, "N.", 25, 16000),
    viz_E(dfrandom31, "R.", 30, 100000), 
    
    viz_E(dfTOC10, "C. Test-of-Cure 80%", 10, 1000), 
    viz_E(dfTOC15, "G.", 15, 1000),
    viz_E(dfTOC20, "K.", 20, 1000),
    viz_E(dfTOC25, "O.", 25, 16000),
    viz_E(dfTOC31, "S.", 30, 100000),
    
    viz_E(dfDST10, "D. Drug Sus. Testing 80%", 10, 1000), 
    viz_E(dfDST15, "H.", 15, 1000),
    viz_E(dfDST20, "L.", 20, 1000),
    viz_E(dfDST25, "P.", 25, 16000),
    viz_E(dfDST31, "T.", 30, 100000),
    
  
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
    
    viz_all_failed(dfGISP10, "A. GISP", 10), 
    viz_all_failed(dfGISP15, "E.", 15),
    viz_all_failed(dfGISP20, "I.", 20),
    viz_all_failed(dfGISP25, "M.", 25),
    viz_all_failed(dfGISP31, "Q.", 30),
    
    viz_all_failed(dfrandom10, "B. Random Treatment", 10), 
    viz_all_failed(dfrandom15, "F.", 15),
    viz_all_failed(dfrandom20, "J.", 20),
    viz_all_failed(dfrandom25, "N.", 25),
    viz_all_failed(dfrandom31, "R.", 30), 
    
    viz_all_failed(dfTOC10, "C. Test-of-Cure 80%", 10), 
    viz_all_failed(dfTOC15, "G.", 15),
    viz_all_failed(dfTOC20, "K.", 20),
    viz_all_failed(dfTOC25, "O.", 25),
    viz_all_failed(dfTOC31, "S.", 30),
    
    viz_all_failed(dfDST10, "D. Drug Sus. Testing 80%", 10), 
    viz_all_failed(dfDST15, "H.", 15),
    viz_all_failed(dfDST20, "L.", 20),
    viz_all_failed(dfDST25, "P.", 25),
    viz_all_failed(dfDST31, "T.", 30),
    
    
    cols = 4)
}


visualize_prev_all = function(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31,
                              dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31,
                              dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31, 
                              dfDST10, dfDST15, dfDST20, dfDST25, dfDST31){
  multiplot(
    
    viz_prev(dfGISP10, "A. GISP", 10), 
    viz_prev(dfGISP15, "E.", 15),
    viz_prev(dfGISP20, "I.", 20),
    viz_prev(dfGISP25, "M.", 25),
    viz_prev(dfGISP31, "Q.", 30),
    
    viz_prev(dfrandom10, "B. Random Treatment", 10), 
    viz_prev(dfrandom15, "F.", 15),
    viz_prev(dfrandom20, "J.", 20),
    viz_prev(dfrandom25, "N.", 25),
    viz_prev(dfrandom31, "R.", 30), 
    
    viz_prev(dfTOC10, "C. Test-of-Cure 80%", 10), 
    viz_prev(dfTOC15, "G.", 15),
    viz_prev(dfTOC20, "K.", 20),
    viz_prev(dfTOC25, "O.", 25),
    viz_prev(dfTOC31, "S.", 30),
    
    viz_prev(dfDST10, "D. Drug Sus. Testing 80%", 10), 
    viz_prev(dfDST15, "H.", 15),
    viz_prev(dfDST20, "L.", 20),
    viz_prev(dfDST25, "P.", 25),
    viz_prev(dfDST31, "T.", 30),
    
    
    cols = 4)
}


  visualize_inc_all = function(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31,
                               dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31,
                               dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31, 
                               dfDST10, dfDST15, dfDST20, dfDST25, dfDST31){
  multiplot(
    
    viz_true_inc(dfGISP10, "A. GISP", 10), 
    viz_true_inc(dfGISP15, "E.", 15),
    viz_true_inc(dfGISP20, "I.", 20),
    viz_true_inc(dfGISP25, "M.", 25),
    viz_true_inc(dfGISP31, "Q.", 30),
    
    viz_true_inc(dfrandom10, "B. Random Treatment", 10), 
    viz_true_inc(dfrandom15, "F.", 15),
    viz_true_inc(dfrandom20, "J.", 20),
    viz_true_inc(dfrandom25, "N.", 25),
    viz_true_inc(dfrandom31, "R.", 30), 
    
    viz_true_inc(dfTOC10, "C. Test-of-Cure 80%", 10), 
    viz_true_inc(dfTOC15, "G.", 15),
    viz_true_inc(dfTOC20, "K.", 20),
    viz_true_inc(dfTOC25, "O.", 25),
    viz_true_inc(dfTOC31, "S.", 30),
    
    viz_true_inc(dfDST10, "D. Drug Sus. Testing 80%", 10), 
    viz_true_inc(dfDST15, "H.", 15),
    viz_true_inc(dfDST20, "L.", 20),
    viz_true_inc(dfDST25, "P.", 25),
    viz_true_inc(dfDST31, "T.", 30),
    
    
    cols = 4)
  
  } 
  
  
  visualize_true_resist_A_all = function(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31,
                                         dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31,
                                         dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31, 
                                         dfDST10, dfDST15, dfDST20, dfDST25, dfDST31){
    multiplot(
      
      viz_true_resist_A(dfGISP10, "A. GISP", 10), 
      viz_true_resist_A(dfGISP15, "E.", 15),
      viz_true_resist_A(dfGISP20, "I.", 20),
      viz_true_resist_A(dfGISP25, "M.", 25),
      viz_true_resist_A(dfGISP31, "Q.", 30),
      
      viz_true_resist_A(dfrandom10, "B. Random Treatment", 10), 
      viz_true_resist_A(dfrandom15, "F.", 15),
      viz_true_resist_A(dfrandom20, "J.", 20),
      viz_true_resist_A(dfrandom25, "N.", 25),
      viz_true_resist_A(dfrandom31, "R.", 30), 
      
      viz_true_resist_A(dfTOC10, "C. Test-of-Cure 80%", 10), 
      viz_true_resist_A(dfTOC15, "G.", 15),
      viz_true_resist_A(dfTOC20, "K.", 20),
      viz_true_resist_A(dfTOC25, "O.", 25),
      viz_true_resist_A(dfTOC31, "S.", 30),
      
      viz_true_resist_A(dfDST10, "D. Drug Sus. Testing 80%", 10), 
      viz_true_resist_A(dfDST15, "H.", 15),
      viz_true_resist_A(dfDST20, "L.", 20),
      viz_true_resist_A(dfDST25, "P.", 25),
      viz_true_resist_A(dfDST31, "T.", 30),
      
      
      cols = 4)
  }
  
  
visualize_true_resist_B_all = function(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31,
                                       dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31,
                                       dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31, 
                                       dfDST10, dfDST15, dfDST20, dfDST25, dfDST31){
  multiplot(
    
    viz_true_resist_B(dfGISP10, "A. GISP", 10), 
    viz_true_resist_B(dfGISP15, "E.", 15),
    viz_true_resist_B(dfGISP20, "I.", 20),
    viz_true_resist_B(dfGISP25, "M.", 25),
    viz_true_resist_B(dfGISP31, "Q.", 30),
    
    viz_true_resist_B(dfrandom10, "B. Random Treatment", 10), 
    viz_true_resist_B(dfrandom15, "F.", 15),
    viz_true_resist_B(dfrandom20, "J.", 20),
    viz_true_resist_B(dfrandom25, "N.", 25),
    viz_true_resist_B(dfrandom31, "R.", 30), 
    
    viz_true_resist_B(dfTOC10, "C. Test-of-Cure 80%", 10), 
    viz_true_resist_B(dfTOC15, "G.", 15),
    viz_true_resist_B(dfTOC20, "K.", 20),
    viz_true_resist_B(dfTOC25, "O.", 25),
    viz_true_resist_B(dfTOC31, "S.", 30),
    
    viz_true_resist_B(dfDST10, "D. Drug Sus. Testing 80%", 10), 
    viz_true_resist_B(dfDST15, "H.", 15),
    viz_true_resist_B(dfDST20, "L.", 20),
    viz_true_resist_B(dfDST25, "P.", 25),
    viz_true_resist_B(dfDST31, "T.", 30),
    
    
    cols = 4)
  
}
visualize_true_resist_both_all = function(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31,
                                          dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31,
                                          dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31, 
                                          dfDST10, dfDST15, dfDST20, dfDST25, dfDST31){
  multiplot(
    
    viz_true_resist_both(dfGISP10, "A. GISP", 10), 
    viz_true_resist_both(dfGISP15, "E.", 15),
    viz_true_resist_both(dfGISP20, "I.", 20),
    viz_true_resist_both(dfGISP25, "M.", 25),
    viz_true_resist_both(dfGISP31, "Q.", 30),
    
    viz_true_resist_both(dfrandom10, "B. Random Treatment", 10), 
    viz_true_resist_both(dfrandom15, "F.", 15),
    viz_true_resist_both(dfrandom20, "J.", 20),
    viz_true_resist_both(dfrandom25, "N.", 25),
    viz_true_resist_both(dfrandom31, "R.", 30), 
    
    viz_true_resist_both(dfTOC10, "C. Test-of-Cure 80%", 10), 
    viz_true_resist_both(dfTOC15, "G.", 15),
    viz_true_resist_both(dfTOC20, "K.", 20),
    viz_true_resist_both(dfTOC25, "O.", 25),
    viz_true_resist_both(dfTOC31, "S.", 30),
    
    viz_true_resist_both(dfDST10, "D. Drug Sus. Testing 80%", 10), 
    viz_true_resist_both(dfDST15, "H.", 15),
    viz_true_resist_both(dfDST20, "L.", 20),
    viz_true_resist_both(dfDST25, "P.", 25),
    viz_true_resist_both(dfDST31, "T.", 30),
    
    
    cols = 4)}

visualize_attempts_A_all = function(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31,
                                       dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31,
                                       dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31, 
                                       dfDST10, dfDST15, dfDST20, dfDST25, dfDST31){
  multiplot(
    
    viz_attempts_A(dfGISP10, "A. GISP", 10), 
    viz_attempts_A(dfGISP15, "E.", 15),
    viz_attempts_A(dfGISP20, "I.", 20),
    viz_attempts_A(dfGISP25, "M.", 25),
    viz_attempts_A(dfGISP31, "Q.", 30),
    
    viz_attempts_A(dfrandom10, "B. Random Treatment", 10), 
    viz_attempts_A(dfrandom15, "F.", 15),
    viz_attempts_A(dfrandom20, "J.", 20),
    viz_attempts_A(dfrandom25, "N.", 25),
    viz_attempts_A(dfrandom31, "R.", 30), 
    
    viz_attempts_A(dfTOC10, "C. Test-of-Cure 80%", 10), 
    viz_attempts_A(dfTOC15, "G.", 15),
    viz_attempts_A(dfTOC20, "K.", 20),
    viz_attempts_A(dfTOC25, "O.", 25),
    viz_attempts_A(dfTOC31, "S.", 30),
    
    viz_attempts_A(dfDST10, "D. Drug Sus. Testing 80%", 10), 
    viz_attempts_A(dfDST15, "H.", 15),
    viz_attempts_A(dfDST20, "L.", 20),
    viz_attempts_A(dfDST25, "P.", 25),
    viz_attempts_A(dfDST31, "T.", 30),
    
    
    cols = 4)
}


visualize_attempts_B_all = function(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31,
                                    dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31,
                                    dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31, 
                                    dfDST10, dfDST15, dfDST20, dfDST25, dfDST31){
  multiplot(
    
    viz_attempts_B(dfGISP10, "A. GISP", 10), 
    viz_attempts_B(dfGISP15, "E.", 15),
    viz_attempts_B(dfGISP20, "I.", 20),
    viz_attempts_B(dfGISP25, "M.", 25),
    viz_attempts_B(dfGISP31, "Q.", 30),
    
    viz_attempts_B(dfrandom10, "B. Random Treatment", 10), 
    viz_attempts_B(dfrandom15, "F.", 15),
    viz_attempts_B(dfrandom20, "J.", 20),
    viz_attempts_B(dfrandom25, "N.", 25),
    viz_attempts_B(dfrandom31, "R.", 30), 
    
    viz_attempts_B(dfTOC10, "C. Test-of-Cure 80%", 10), 
    viz_attempts_B(dfTOC15, "G.", 15),
    viz_attempts_B(dfTOC20, "K.", 20),
    viz_attempts_B(dfTOC25, "O.", 25),
    viz_attempts_B(dfTOC31, "S.", 30),
    
    viz_attempts_B(dfDST10, "D. Drug Sus. Testing 80%", 10), 
    viz_attempts_B(dfDST15, "H.", 15),
    viz_attempts_B(dfDST20, "L.", 20),
    viz_attempts_B(dfDST25, "P.", 25),
    viz_attempts_B(dfDST31, "T.", 30),
    
    
    cols = 4)
}


visualize_attempts_X_all = function(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31,
                                    dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31,
                                    dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31, 
                                    dfDST10, dfDST15, dfDST20, dfDST25, dfDST31){
  multiplot(
    
    viz_attempts_X(dfGISP10, "A. GISP", 10), 
    viz_attempts_X(dfGISP15, "E.", 15),
    viz_attempts_X(dfGISP20, "I.", 20),
    viz_attempts_X(dfGISP25, "M.", 25),
    viz_attempts_X(dfGISP31, "Q.", 30),
    
    viz_attempts_X(dfrandom10, "B. Random Treatment", 10), 
    viz_attempts_X(dfrandom15, "F.", 15),
    viz_attempts_X(dfrandom20, "J.", 20),
    viz_attempts_X(dfrandom25, "N.", 25),
    viz_attempts_X(dfrandom31, "R.", 30), 
    
    viz_attempts_X(dfTOC10, "C. Test-of-Cure 80%", 10), 
    viz_attempts_X(dfTOC15, "G.", 15),
    viz_attempts_X(dfTOC20, "K.", 20),
    viz_attempts_X(dfTOC25, "O.", 25),
    viz_attempts_X(dfTOC31, "S.", 30),
    
    viz_attempts_X(dfDST10, "D. Drug Sus. Testing 80%", 10), 
    viz_attempts_X(dfDST15, "H.", 15),
    viz_attempts_X(dfDST20, "L.", 20),
    viz_attempts_X(dfDST25, "P.", 25),
    viz_attempts_X(dfDST31, "T.", 30),
    
    
    cols = 4)
}


visualize_cost_all = function(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31,
                              dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31,
                              dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31, 
                              dfDST10, dfDST15, dfDST20, dfDST25, dfDST31){
  multiplot(
    
    viz_cost(dfGISP10, "A. GISP", 10), 
    viz_cost(dfGISP15, "E.", 15),
    viz_cost(dfGISP20, "I.", 20),
    viz_cost(dfGISP25, "M.", 25),
    viz_cost(dfGISP31, "Q.", 30),
    
    viz_cost(dfrandom10, "B. Random Treatment", 10), 
    viz_cost(dfrandom15, "F.", 15),
    viz_cost(dfrandom20, "J.", 20),
    viz_cost(dfrandom25, "N.", 25),
    viz_cost(dfrandom31, "R.", 30), 
    
    viz_cost(dfTOC10, "C. Test-of-Cure 80%", 10), 
    viz_cost(dfTOC15, "G.", 15),
    viz_cost(dfTOC20, "K.", 20),
    viz_cost(dfTOC25, "O.", 25),
    viz_cost(dfTOC31, "S.", 30),
    
    viz_cost(dfDST10, "D. Drug Sus. Testing 80%", 10), 
    viz_cost(dfDST15, "H.", 15),
    viz_cost(dfDST20, "L.", 20),
    viz_cost(dfDST25, "P.", 25),
    viz_cost(dfDST31, "T.", 30),
    
    
    cols = 4)
}


visualize_QALYs_all = function(dfGISP10, dfGISP15, dfGISP20, dfGISP25, dfGISP31,
                              dfrandom10, dfrandom15, dfrandom20, dfrandom25, dfrandom31,
                              dfTOC10, dfTOC15, dfTOC20, dfTOC25, dfTOC31, 
                              dfDST10, dfDST15, dfDST20, dfDST25, dfDST31){
  multiplot(
    
    viz_qaly(dfGISP10, "A. GISP", 10), 
    viz_qaly(dfGISP15, "E.", 15),
    viz_qaly(dfGISP20, "I.", 20),
    viz_qaly(dfGISP25, "M.", 25),
    viz_qaly(dfGISP31, "Q.", 30),
    
    viz_qaly(dfrandom10, "B. Random Treatment", 10), 
    viz_qaly(dfrandom15, "F.", 15),
    viz_qaly(dfrandom20, "J.", 20),
    viz_qaly(dfrandom25, "N.", 25),
    viz_qaly(dfrandom31, "R.", 30), 
    
    viz_qaly(dfTOC10, "C. Test-of-Cure 80%", 10), 
    viz_qaly(dfTOC15, "G.", 15),
    viz_qaly(dfTOC20, "K.", 20),
    viz_qaly(dfTOC25, "O.", 25),
    viz_qaly(dfTOC31, "S.", 30),
    
    viz_qaly(dfDST10, "D. Drug Sus. Testing 80%", 10), 
    viz_qaly(dfDST15, "H.", 15),
    viz_qaly(dfDST20, "L.", 20),
    viz_qaly(dfDST25, "P.", 25),
    viz_qaly(dfDST31, "T.", 30),
    
    
    cols = 4)
}



visualize_calibration_MSM = function(dfcalibrated){
  multiplot(
    viz_prev_MSM_cal(dfcalibrated, "A. Prevalence in MSM"),
    viz_incMSM_cal(dfcalibrated, "B. Detected Cases in MSM"),
  
    viz_sympt_MSM_cal(dfcalibrated, "C. Proportion Symptomatic, MSM"),
  
    cols = 2)
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



#process and visualize:
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
df_best_ends <- best_ends(df_ends, 200)
df_best_traj <- best_traj(dfsweep,df_best_ends)
visualize_calibration_MSM(df_best_traj)
visualize_parameters(df_best_ends)
write_calibrated(df_best_ends)


dfcalibrated  <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_1_none_none_10/nonenone101combined.csv")
visualize_calibration_MSM(dfcalibrated)


dfGISP25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_1_all_combo_10/GISP_05combo251combined.csv")
dfrandom25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_1_all_combo_10/randomcombo251combined.csv")

dfTOC25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_1_all_combo_10/test-of-cure_80combo251combined.csv")
dfDST25 <-  read.csv("/Users/me597/Documents/MSMoutput/output_JUNE_3_2024_1_all_combo_10/drug_sus_testing_80combo251combined.csv")

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
