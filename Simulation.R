##############################################################################
## A Simplified Model of the Cost-Effectiveness of Screening:               ##
## An Open-Source Teaching and Research Tool Coded in R                     ##
##############################################################################

# Authors:
# James O'Mahony, PhD (1)
# Yi-Shu Lin, MSc, MBA (1)
# Joost van Rosmalen, PhD (2)

# 1 Trinity College Dublin, Ireland
# 2 Department of Biostatistics, Erasmus Medical Centre Rotterdam, Netherlands

##################################################################################
# Please cite our publications when using this code
# O'Mahony JF. Simplified Model of the Cost-Effectiveness of Screening in R: a Teaching and Research Tool. 17th Biennial European Meeting of the Society for Medical Decision Making Leiden, the Netherlands, June 10-12, 2018. (2018). Medical Decision Making, 38(6), E372-E610. https://doi.org/10.1177/0272989X18793413

##################################################################################
# See GitHub for code updates
# https://github.com/yishu-lin/Pedagogical-CEA-Model-of-Screening.git

##################################### Background Setting #####################################
rm(list = ls())  # Delete everything that is in R's memory
time <- Sys.time()  # Save system time

# Set the working file directory path. Paste your working directory within the quotes
# Ensure that the backslashes \ are changed to forward slashes /
# You can put as many file paths in as you like here, R will take the last one that worked
# The try function simply masks error reporting: it will work if one of the directories attempted is correct
try(setwd("Your working directory 1"), silent = TRUE)
try(setwd("Your working directory 2"), silent = TRUE)
try(setwd(dirname(sys.frame(1)$ofile)), silent = TRUE)  # Drive file path
try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)), silent = TRUE)  # Drive file path

##################################### Model Input #####################################
# Import the required data from the data sheets created by the Excel file
InputTable       <- data.frame(read.table(file = "InputFiles/InputTable.txt",       header = T, row.names = 1))  # Apply the first column as row names
DefineStages     <- data.frame(read.table(file = "InputFiles/Stages.txt",           header = T))  # Stage; Name;Type;Scale;Shape;Utility
Incidence        <- data.frame(read.table(file = "InputFiles/Incidence.txt",        header = T))  # Age_Interval; Annual_Incidence; Cumulative_Failure
Survival         <- data.frame(read.table(file = "InputFiles/LifeTable.txt",        header = T))  # Interval_LifeTable; Survival
ScreenSchedule   <- data.frame(read.table(file = "InputFiles/ScreenSchedule.txt",   header = T))  # ScheduleNumber; StartAge; StopAge; TestApplied1; Interval1
TestPerformance  <- data.frame(read.table(file = "InputFiles/TestPerformance.txt",  header = T))  # Test; Sn; Sp; Disutility
TreatmentSuccess <- data.frame(read.table(file = "InputFiles/TreatmentSuccess.txt", header = T))  # PreClinicalProbability; ClinicalProbability
Disutility       <- data.frame(read.table(file = "InputFiles/OtherDisutility.txt",  header = T))  # PreClinicalProbability; ClinicalProbability
DiscountRates    <- data.frame(read.table(file = "InputFiles/DiscountRates.txt",    header = T))  # Costs; Effects; DiscountYear
Costs            <- data.frame(read.table(file = "InputFiles/Costs.txt",            header = T))  # PrimaryScreen; FollowUp; TreatmentScreenDetected; TreatmentClinical
source("InputFiles/Misc.txt")  # SampleSize; Threshold
source("InputFiles/Options.txt")

# To make a list of parameters that are included in scenario analysis
ParameterNames <- c("StageScale2",
                    "StageScale3",
                    #"StageUtility1",  # These parameters are not included in our scenario analysis
                    #"StageUtility2",
                    #"StageUtility3",
                    "TestSensitivity1",
                    "TestSpecificity1",
                    #"TestDisutility1",
                    "PreClinicalProbability",
                    "ClinicalProbability",
                    #"DisutilityTriage",
                    #"DisutilityTrt",
                    #"DiscountRateCost",
                    #"DiscountRateEffect",
                    #"DiscountYear",
                    "CostPrimaryScreen",
                    "CostFollowUp",
                    "CostTrtScreen",
                    "CostTrtClinical",
                    "Incidence",  # In this example, we change the value of incidence and survival proportionally across age groups
                    "Survival"
                    )

Input <- data.frame(InputTable[, "Input"], row.names = rownames(InputTable))  # Create a table to record parameter inputs
ScenarioNames <- "Input"  # Create a variable to save the names of scenarios

if (ScenarioAnaylsis == TRUE){
  ScenarioValue <- c("Low", "High")  # Our default has two scenarios for each parameter
  for (Parameter in ParameterNames){  # The list of  parameters for scenario analysis
    for (Scenarios in ScenarioValue){
      InputValue <- InputTable[, "Input"]
      if (!(Parameter %in% c("Incidence", "Survival"))){  # Parameters, incidence and survival, have different risks for age groups
        InputValue[rownames(InputTable) %in% Parameter] <- InputTable[rownames(InputTable) %in% Parameter, Scenarios]
      } else if (Parameter == "Incidence") {
          InputValue[rownames(InputTable) %in% paste("Incidence", Incidence$AgeInterval, sep = "_")] <- 
          InputTable[rownames(InputTable) %in% paste("Incidence", Incidence$AgeInterval, sep = "_"), Scenarios]
        } else {
            InputValue[rownames(InputTable) %in% paste("Survival", Survival$AgeInterval, sep = "_")] <- 
            InputTable[rownames(InputTable) %in% paste("Survival", Survival$AgeInterval, sep = "_"), Scenarios]
          }
      Input <- cbind(Input, InputValue)  # Save parameter value to the input table
      ScenarioNames <- c(ScenarioNames, paste(Parameter, Scenarios, sep = "_"))
    }
  }  
}

colnames(Input) <- ScenarioNames

# We are able to redefine the screen schedule
if (ExcelDefinedScreening == TRUE){
  StartAges           <- ScreenSchedule[, "StartAge"]
  StopAges            <- ScreenSchedule[, "StopAge"]
  IntervalSwitchAge1s <- ScreenSchedule[, "IntervalSwitchAge1"]
  IntervalSwitchAge2s <- ScreenSchedule[, "IntervalSwitchAge2"]
  IntervalSwitchAge3s <- ScreenSchedule[, "IntervalSwitchAge3"]
  Interval1s          <- ScreenSchedule[, "Interval1"]
  Interval2s          <- ScreenSchedule[, "Interval2"]
  Interval3s          <- ScreenSchedule[, "Interval3"]
  Interval4s          <- ScreenSchedule[, "Interval4"]
  TestSwitchAges      <- ScreenSchedule[, "ScreenSwitchAge"]
  ScreenTest1s        <- ScreenSchedule[, "TestApplied1"]
  ScreenTest2s        <- ScreenSchedule[, "TestApplied2"]
} else {
    # Simulate all the possible screening strategies, assuming the screen interval remains fixed throughout the programme
    Start <- 25  # The starting age of screening
    Stop <- 100  # The stop age of screening
    Frequency <- c(1:10)  # The screen interval is an integer in the range of 1-10 years
    
    # One-time screen in the lifetime
    StartAges <- StopAges <- c(seq(Start, Stop, by = 1))
    Interval1s <- rep(0, (Stop - Start + 1))  # No screening interval
    ScreenTest1s <- rep(1, (Stop - Start + 1))  # We assume all the strategies use the first screening modality
    IntervalSwitchAge1s <- IntervalSwitchAge2s <- IntervalSwitchAge3s <- 
    Interval2s          <- Interval3s          <- Interval4s          <- 
    TestSwitchAges      <-  ScreenTest2s       <- rep(NA, (100 - 25 + 1))    # No screening interval and other screening modality
  
    # The example here only changes starting age, stop age, and screening intervals
    for (i in c(seq(Start ,Stop, by = 1))){ 
      for (j in c(seq(Start, Stop, by = 1))){
        for (k in Frequency){
          if ((i < j) & ((j - i) %% k == 0)){  # Remove non-integer screens 
            StartAges           <- c(StartAges, i)
            StopAges            <- c(StopAges, j)
            IntervalSwitchAge1s <- c(IntervalSwitchAge1s, NA)
            IntervalSwitchAge2s <- c(IntervalSwitchAge2s, NA)
            IntervalSwitchAge3s <- c(IntervalSwitchAge3s, NA)
            Interval1s          <- c(Interval1s, k)
            Interval2s          <- c(Interval2s, NA)
            Interval3s          <- c(Interval3s, NA)
            Interval4s          <- c(Interval4s, NA)
            TestSwitchAges      <- c(TestSwitchAges, NA)
            ScreenTest1s        <- c(ScreenTest1s, 1)  # We assume all the strategies use the same screening modality
            ScreenTest2s        <- c(ScreenTest2s, NA) 
          }
        }
      }
    }
}

# We give strategies shorter names
strategies <- paste(StartAges, StopAges, Interval1s, sep = "_")
#strategies <- paste(StartAges, StopAges, Interval1s, IntervalSwitchAge1s, IntervalSwitchAge2s, IntervalSwitchAge3s,
#                    Interval2s, Interval3s, Interval4s, TestSwitchAges, ScreenTest1s, ScreenTest2s, sep = "_")

for (CurrentRun in c(1: ncol(Input))){
  ##################################### Simulation: Natural History of Disease #####################################
  set.seed(2021)  # Set a seed to be able to reproduce the same results
  
  # Define number of health stages with the stage arrival
  # This model only features five states: (1)Disease Free; (2)Preclinical Disease; (3)Clinical Disease; (4)Cause-Specific Death; (5)Other-Cause Death
  Outcomes <- array(NA, dim = c(SampleSize, 6))  # Create an array of the length of the sample size
  colnames(Outcomes) <- c("PersonNumber", paste(DefineStages[2:nrow(DefineStages), "Name"]), "AllCauseDeath")  # Set column names
  Outcomes[, "PersonNumber"] <- c(1:SampleSize)  # Set the first column to be the unique person-number for each individual
  
  # The intervening columns correspond to the arrival of the intermediate disease states
  # This model studies the cohort with the same age, so here does not need to simulate the "Stage1Arrival"
  # Define the generic onset function which applies an age-specific probability of entering a specific stage
  OnsetFunction <- function(x){
    unlist(approx(probability, age, x, ties = max)[2], use.names = F)  # Ties = max is required because of the possibility of multiple zero probabilities of disease at younger ages
  }
  CumulativeFailure <- 1
  IncidenceTable <- data.frame(Incidence$AgeInterval, 
                               as.numeric(Input[paste("Incidence", Incidence$AgeInterval, sep = "_"), CurrentRun]), 
                               CumulativeFailure
                               )
  colnames(IncidenceTable) <- c("AgeInterval", "AnnualIncidence", "CumulativeFailure")
  for (c in c(2:nrow(IncidenceTable))){
    IncidenceTable[c, "CumulativeFailure"] <- (IncidenceTable[c - 1, "CumulativeFailure"] - (IncidenceTable[c, "AnnualIncidence"] * (IncidenceTable[c, "AgeInterval"] - IncidenceTable[c - 1, "AgeInterval"])))
  }
  
  # Define the specific values of probability and age for disease onset
  probability <- IncidenceTable$CumulativeFailure
  age         <- IncidenceTable$AgeInterval
  # The probability distribution need to be curtailed at the top and bottom for non-unique probability for the approx function to work as intended
  age         <- age[max(which(probability == max(probability))):length(probability)]  # Remove the lower bound values
  probability <- probability[max(which(probability == max(probability))):length(probability)]  # Remove the lower bound values
  age         <- age[1:min(which(probability == min(probability)))]  # Remove the upper bound values
  probability <- probability[1:min(which(probability == min(probability)))]  # Remove the upper bound values
  x           <- runif(SampleSize)  # Create a vector of random values
  Outcomes[, "Preclinical"] <- OnsetFunction(x)  # Find the age of disease onset by applying the general function
  
  # Define the specific values of probability and age for other cause death
  # The intervals in the age-specific incidence are defined by those used in the life table
  probability <- as.numeric(Input[paste("Survival", Survival$AgeInterval, sep = "_"), CurrentRun])
  age         <- Survival$AgeInterval
  # The probability distribution need to be curtailed at the top and bottom for non-unique probability for the approx function to work as intended
  age         <- age[max(which(probability == max(probability))):length(probability)]  # Remove the lower bound values
  probability <- probability[max(which(probability == max(probability))):length(probability)]  # Remove the lower bound values
  age         <- age[1:min(which(probability == min(probability)))]  # Remove the upper bound values
  probability <- probability[1:min(which(probability == min(probability)))]  # Remove the upper bound values
  x           <- runif(SampleSize)  # Create a vector of random values
  Outcomes[, "OtherCauseDeath"] <- OnsetFunction(x)  # Find the age at other-cause death by applying the generic onset function
  
  # The model needs a loop here to go through the disease stages
  for (Stage in 1:(nrow(DefineStages) - 1)){
    # Apply the sojourn time to the stages
    # Retrieve the distribution type, scale and shape
    DurationType <- Input[paste("StageType", Stage, sep = ""), CurrentRun]
    DurationScale <- Input[paste("StageScale", Stage, sep = ""), CurrentRun]
    DurationShape <- Input[paste("StageShape", Stage, sep = ""), CurrentRun]
    
    if (!(is.na(DurationType))){
      if (DurationType == 1){Duration <- rep(DurationScale, SampleSize)}  # Set the preclinical distribution to be constant
      if (DurationType == 2){Duration <- -(log(1 - runif(SampleSize))) * DurationScale}  # Set the preclinical distribution to be exponentially distributed
      if (DurationType == 3){Duration <- ((-log(1 - runif(SampleSize))) ^ (1 / DurationShape)) * DurationScale}  # Set the preclinical distribution to be Weibull distribution, and the alternative code: rweibull(SampleSize, shape = DurationShape, scale = DurationScale)
      Outcomes[, Stage + 1] <- Outcomes[, Stage] + Duration  # Find the end of the preclinical period by adding the onset to the duration
    }
    
    # This study assumes all the people died before aged 100
    Outcomes[which(Outcomes[, Stage + 1] > 100), Stage + 1] <- 100
  }
  
  # Find the all-cause death
  Outcomes[, "AllCauseDeath"] <- pmin(Outcomes[, "CauseSpecificDeath"], Outcomes[, "OtherCauseDeath"], na.rm = TRUE)
  
  # Identify individuals presenting with clinical disease
  DiseaseFree <- which((Outcomes[, "Preclinical"] >= Outcomes[, "OtherCauseDeath"]) | (is.na(Outcomes[, "Preclinical"])))  # Censor onset ages greater than death; first identify those who never develop disease
  Sick <- Outcomes[-DiseaseFree, "PersonNumber"]
  ClinicalCases <- Sick[which(Outcomes[Sick, "Clinical"] <= Outcomes[Sick, "OtherCauseDeath"])]
  
  # Identify individuals cured successfully
  CureSeed <- runif(SampleSize)  # Save random numbers for treatment
  CuredCases <- ClinicalCases[which(CureSeed[ClinicalCases] <= Input["ClinicalProbability", CurrentRun])]  # Find treatment to be successful if probabilities less than probability of successful treatment
  Outcomes[CuredCases, "AllCauseDeath"] <- Outcomes[CuredCases, "OtherCauseDeath"]
  
  # Create a reference series of discount factors for 100 years
  DF_Effects <- round((1 + Input["DiscountRateEffect", CurrentRun]) ^ -(c(1:101) - Input["DiscountYear", CurrentRun]), digits = 4)
  DF_Costs   <- round((1 + Input["DiscountRateCost",   CurrentRun]) ^ -(c(1:101) - Input["DiscountYear", CurrentRun]), digits = 4)
  Acc_DF_Effects <- NA  # Create a variable to save accumulated discounted life-years
  for (year in c(1:101)){
    Acc_DF_Effects <- c(Acc_DF_Effects, sum(DF_Effects[1:year]))
  }
  Acc_DF_Effects <- Acc_DF_Effects[-1]  # Remove NA
  
  # Define the present value function for effectiveness
  PresentValue <- function(x){
    if (x >= 1 & !(is.na(x))){
      Acc_DF_Effects[trunc(x)] + (x - trunc(x)) * DF_Effects[trunc(x) + 1]
    } else if (!(is.na(x))){
      x * DF_Effects[1]
      } else {
        x
        }
  }
  
  # Calculate the effects
  UD_QALYS <- Outcomes[, "AllCauseDeath"]
  D_QALYS <- D_LYS <- sapply(UD_QALYS, PresentValue)
  StagesInOrder <- c(paste(DefineStages[2:(nrow(DefineStages) - 2), "Name"]), "AllCauseDeath")
  SickOutcome <- as.data.frame(Outcomes[Sick, StagesInOrder])  # Create a table that only sick people included
  Dis_SickOutcome <- apply(SickOutcome, MARGIN = c(1, 2), PresentValue)  # Discount life-years for each health state among sick people
  Utility <- Input[paste("StageUtility", c(1:(nrow(DefineStages) - 2)), sep = ""), CurrentRun]  # Quality-of-life utility weights for each state
  
  # Define the function for quality-of-life adjustment
  Accumulated_QALYS <- function(x, output){
    QALYS <- x[1] * Utility[1]  # For the first health state, simply multiply the life-years by the corresponding utility weight
    for (state in (2:length(StagesInOrder))){  # For the remaining health states
        QALYS <- QALYS + Utility[state] * (x[state] - x[state - 1])  # Add up the QALYs in each health state
        if (state == length(StagesInOrder)){  # The last health state
          break
        }
    }
    return(QALYS)
  }
  
  UD_QALYS[DiseaseFree] <- UD_QALYS[DiseaseFree] * Utility[1]  # Quality-of-life adjusted but not discounted
  UD_QALYS[Sick] <- apply(SickOutcome, 1, Accumulated_QALYS)
  D_QALYS[DiseaseFree] <- D_QALYS[DiseaseFree] * Utility[1]  # Quality-of-life adjusted and discounted
  D_QALYS[Sick] <- apply(Dis_SickOutcome, 1, Accumulated_QALYS)
  
  # Dis-utility due to the treatment
  ClinicalOnsetAges <- ceiling(Outcomes[ClinicalCases, "Clinical"])  # Identify the clinical ages and round up to integers for discounting
  UD_TreatmentHarm <- length(ClinicalCases) * Input["DisutilityTrt", CurrentRun]  # Undiscounted QALYs loss due to treatment
  D_TreatmentHarm <- sum(Input["DisutilityTrt", CurrentRun] * DF_Effects[ClinicalOnsetAges])  # Discounted QALYs loss due to treatment
  
  # Calculate the effectiveness outcomes without screening
  UD_NoScreening_LYG <- sum(Outcomes[, "AllCauseDeath"])
  D_NoScreening_LYG <- sum(D_LYS)
  UD_NoScreening_QALY <- sum(UD_QALYS) - UD_TreatmentHarm
  D_NoScreening_QALY <- sum(D_QALYS) - D_TreatmentHarm
  
  # Retrieve the costs parameters
  ScreenCost <- Input["CostPrimaryScreen", CurrentRun]
  TriageCost <- Input["CostFollowUp", CurrentRun]
  EarlyTreatmentCost <- Input["CostTrtScreen", CurrentRun]
  LateTreatmentCost <- Input["CostTrtClinical", CurrentRun]
  
  # Calculate the costs of treating clinical disease under no screening
  UD_NoScreening_Costs <- length(ClinicalCases) * LateTreatmentCost
  D_NoScreening_Costs <- sum(DF_Costs[ClinicalOnsetAges] * LateTreatmentCost)  # Find the discounted treatment costs
  
  # Calculate intermediate outcomes: effects
  PreClinicalDuration <- mean(Outcomes[ClinicalCases, "Clinical"] - Outcomes[ClinicalCases, "Preclinical"])  # Average preclinical sojourn time
  N_CancerDeaths <- length(which(Outcomes[, "AllCauseDeath"] == Outcomes[, "CauseSpecificDeath"]))  # The number of cases that die at the age of disease death
  N_Overdiagnosed <- 0
  N_ClinicalCases <- length(ClinicalCases)  # The number of cases that present clinically
  
  # Record cost-effectiveness results
  CostEffectivenessResults <- data.frame(StrategyName = c("NoScreening", strategies),
                                         UD_EffectsL  = as.numeric(UD_NoScreening_LYG),
                                         UD_EffectsQ  = as.numeric(UD_NoScreening_QALY),
                                         UD_Costs     = as.numeric(UD_NoScreening_Costs),
                                         D_EffectsL   = as.numeric(D_NoScreening_LYG),
                                         D_EffectsQ   = as.numeric(D_NoScreening_QALY),
                                         D_Costs      = as.numeric(D_NoScreening_Costs),
                                         UD_ICER_L    = as.numeric(NA),
                                         UD_ICER_Q    = as.numeric(NA),
                                         D_ICER_L     = as.numeric(NA),
                                         D_ICER_Q     = as.numeric(NA)
                                         )
  
  # Record age distribution of each health state
  AgeDistribution<- function(x, output){
    Age.cut <- cut(x, seq(0, 100, by = 1), right = FALSE)
    return(table(Age.cut))
  }
  IntermediateOutcomes_AgeDist <- apply(Outcomes[, !(colnames(Outcomes) %in% "PersonNumber")], 2, AgeDistribution)
  write.table(IntermediateOutcomes_AgeDist, 
              paste("OutputFiles/LatestResults/IntermediateOutcomtes/AgeDistribution/", colnames(Input)[CurrentRun], "_NoScreening", ".txt", sep = ""),
              row.names = F, col.names = T, sep = '\t')
  
  # Record intermediate outcomes
  IntermediateOutcomes <- data.frame(StrategyName           = c("NoScreening", strategies),
                                     UD_ScreenCosts         = as.numeric(0),
                                     UD_TriageCosts         = as.numeric(0),
                                     UD_EarlyTreatmentCosts = as.numeric(0),
                                     UD_LateTreatmentCosts  = as.numeric(UD_NoScreening_Costs),
                                     D_ScreenCosts          = as.numeric(0),
                                     D_TriageCosts          = as.numeric(0),
                                     D_EarlyTreatmentCosts  = as.numeric(0),
                                     D_LateTreatmentCosts   = as.numeric(D_NoScreening_Costs),
                                     N_CancerDeaths         = as.numeric(N_CancerDeaths),
                                     N_Overdiagnosed        = as.numeric(N_Overdiagnosed),
                                     N_ClinicalCases        = as.numeric(N_ClinicalCases)
                                     )
  
  ##################################### Screening Strategy Generation #####################################
  for (Strategy in c(1:length(strategies))){
    # To track the progress
    print(paste("Simulation ", CurrentRun, " / ", ncol(Input), " : Strategy ", Strategy, " / ", length(strategies), sep = ""))
    
    # Retrieve the strategy-specific values
    StartAge           <- StartAges[Strategy]
    StopAge            <- StopAges[Strategy]
    IntervalSwitchAge1 <- IntervalSwitchAge1s[Strategy]
    IntervalSwitchAge2 <- IntervalSwitchAge2s[Strategy]
    IntervalSwitchAge3 <- IntervalSwitchAge3s[Strategy]
    Interval1          <- Interval1s[Strategy]
    Interval2          <- Interval2s[Strategy]
    Interval3          <- Interval3s[Strategy]
    Interval4          <- Interval4s[Strategy]
    TestSwitchAge      <- TestSwitchAges[Strategy]
    ScreenTest1        <- ScreenTest1s[Strategy]
    ScreenTest2        <- ScreenTest2s[Strategy]
    
    # Amend the screening schedule
    if (Interval1 == 0){  # One-time screen in the lifetime
      Screens <- StartAge
      NumberOfScreens <- 1
    } else {
        Intervals <- 1  # Determine the number of intervals, and the default is one interval
        if (!is.na(IntervalSwitchAge1)){Intervals <- 2}
        if (!is.na(IntervalSwitchAge2)){Intervals <- 3}
        if (!is.na(IntervalSwitchAge3)){Intervals <- 4}
        
        IntervalVector <- c(Interval1, Interval2, Interval3, Interval4)  # Create a vector of the screening intervals
        ChangeAges <- sort(na.omit(c(StopAge, IntervalSwitchAge1, IntervalSwitchAge2, IntervalSwitchAge3)))  # Create a vector of ages at which screening changes
        Screens <- NULL  # Create an empty vector to bind the sequence of screens
        for (Interval in 1:Intervals ){  # Loops through the Intervals
          StopAge <- ChangeAges[Interval]  # Redefine the StopAge as the ChangeAge in this interval
          NumberOfScreens <- (StopAge - StartAge) / IntervalVector[Interval] + 1  # Find the number of screens        
          NumberOfScreens <- ifelse(NumberOfScreens %% 1 == 0.5, ceiling(NumberOfScreens), round(NumberOfScreens))  # Rounding the number of screens
          StopAge <- StartAge + (NumberOfScreens - 1) * IntervalVector[Interval]  # Redefine the actual StopAge following rounding
          Screens <- c(Screens, seq(StartAge, StopAge, IntervalVector[Interval]))  # Create the sequence of screens from the start and stop ages
          StartAge <- max(Screens)  # Update the final screen age as the Start Age for cases in which the interval changes
        }
        Screens <- unique(Screens)  # Remove duplicates from the screen schedule
        NumberOfScreens <- length(Screens)  # Update the number of screens
    }
    
    # Create an array of the screen counts: This is an array recording the number of screens and consequent results
    ScreenCounts <- array(NA, dim = c(NumberOfScreens, 7))  # Create an array from the screen schedule
    colnames(ScreenCounts) <- c("ScreenNumber", "TestApplied", "ScreenAge", "N_Screens", "RealPostives", "TruePositives", "FalsePositives")  # Name the columns
    ScreenCounts[, "ScreenNumber"] <- c(1:NumberOfScreens)  # Write in a list equivalent to the number of screens and the per round screening age
    ScreenCounts[, "ScreenAge"] <- Screens
    ScreenCounts[, "TestApplied"] <- ScreenTest1  # Insert the appropriate test number
    ScreenCounts[which(ScreenCounts[, "ScreenAge"] >= TestSwitchAge), "TestApplied"] <- ScreenTest2
    
    # Now run screening
    ScreenedOutcomes <- Outcomes  # Create the screen-adjusted outcomes from the natural history outcomes
    ScreenedClinicalCases <- NULL  # Create an empty vector to record clinical cases
    ScreenedCuredCases <- NULL  # Create an empty vector to record cured cases
    
    for (ScreenNumber in 1:nrow(ScreenCounts)){
      set.seed(ScreenCounts[ScreenNumber, "ScreenAge"] + 2020)  # Fixed sample seed for the same screening age across screening programmes
      ScreenSnSeed <- runif(SampleSize)  # Save random numbers for the test sensitivity
      ScreenSpSeed <- runif(SampleSize)  # Save random numbers for the test specificity
      # Define the screen age
      ScreenAge <- ScreenCounts[ScreenNumber, "ScreenAge"]
      # Retrieve the test sensitivity and specificity
      ScreenSn <- Input[paste("TestSensitivity", ScreenCounts[ScreenNumber, "TestApplied"], sep = ""), CurrentRun]
      ScreenSp <- Input[paste("TestSpecificity", ScreenCounts[ScreenNumber, "TestApplied"], sep = ""), CurrentRun]
      
      Alive <- ScreenedOutcomes[, "AllCauseDeath"] >= ScreenAge
      NotDiagnosed <- ScreenedOutcomes[, "Clinical"] >= ScreenAge
      NotDiagnosed[is.na(NotDiagnosed)] <- TRUE  # People who never develop the disease are also not diagnosed
      ScreenEligible <- Alive * NotDiagnosed  # Only when both conditions (alive and not diagnosed) meet
      
      # Identify those in the preclinical stage at the screen age
      Preclinical <- ScreenedOutcomes[, "Preclinical"] <= ScreenAge
      Preclinical[is.na(Preclinical)] <- FALSE  # People who never develop the disease are not in the preclinical stage
      AllPositives <- which((Preclinical * ScreenEligible) == 1)  # Save patient numbers that can be screened for positive
      # Identify the negatives as the complement of the positives from within the ScreenEligible set
      AllNegatives <- which(ScreenEligible == 1)[!(which(ScreenEligible == 1) %in% AllPositives)]
      
      # Find the true positives by sampling without replacement over all positives in proportion to the test sensitivity
      TruePositives <- AllPositives[ScreenSnSeed[AllPositives] <= ScreenSn]
      # Find the false positives by sampling without replacement over the negatives in proportion to the test specificity
      FalsePositives <- AllNegatives[ScreenSpSeed[AllNegatives] >= ScreenSp]
      # It's useful to have a per screening round count of the number of positives, all positives, true positives and false positives
      ScreenCounts[ScreenNumber, c("N_Screens", "RealPostives", "TruePositives", "FalsePositives")] <- cbind(length(which(ScreenEligible == 1)),
                                                                                                             length(AllPositives),
                                                                                                             length(TruePositives),
                                                                                                             length(FalsePositives)
                                                                                                             )
      # Censor these successfully treated individuals
      ScreenedCured <- TruePositives[which(CureSeed[TruePositives] <= Input["PreClinicalProbability", CurrentRun])]
      
      # Now update the screen-adjusted outcomes
      ScreenedOutcomes[TruePositives, "Clinical"] <- ScreenAge  # Update the age of entering clinical state
      ScreenedOutcomes[ScreenedCured, "AllCauseDeath"] <- ScreenedOutcomes[ScreenedCured, "OtherCauseDeath"]  # Update all-cause death
      # Record clinical cases
      ScreenedClinicalCases <- c(ScreenedClinicalCases, TruePositives)  # Save patient numbers that are diagnosed by screening
      ScreenedCuredCases <- c(ScreenedCuredCases, ScreenedCured)  # Save patient numbers that are cured by screening
    }
    
    # Record intermediate outcomes
    write.table(ScreenCounts, paste("OutputFiles/LatestResults/IntermediateOutcomtes/ScreenCounts/",
                                    colnames(Input)[CurrentRun], "_", strategies[Strategy], ".txt", sep = ""),
                row.names = F, col.names = T, sep = '\t')
    
    N_Overdiagnosed <- sum(!ScreenedClinicalCases %in% ClinicalCases)  # Patients are not diagnosed in their natural history but diagnosed with screening
    N_ClinicalCases <- length(ClinicalCases) + sum(!ScreenedClinicalCases %in% ClinicalCases)  # The number of patients diagnosed in natural history and screening
    NotScreenedClinicalCases <- ClinicalCases[!ClinicalCases %in% ScreenedClinicalCases]  # Patient numbers who are diagnosed without screening
    NotScreenedCuredCases <- NotScreenedClinicalCases[which(CureSeed[NotScreenedClinicalCases] <= Input["ClinicalProbability", CurrentRun])]  # Receive treatment with clinical probability
    N_CancerDeaths <- length(which(ScreenedOutcomes[, "AllCauseDeath"] == ScreenedOutcomes[, "CauseSpecificDeath"]))  # The number of cases that die at the age of disease death
    
    # Record age distribution of each health state
    IntermediateOutcomes_AgeDist <- apply(ScreenedOutcomes[, !(colnames(ScreenedOutcomes) %in% "PersonNumber")], 2, AgeDistribution)
    write.table(IntermediateOutcomes_AgeDist, paste("OutputFiles/LatestResults/IntermediateOutcomtes/AgeDistribution/",
                                                    colnames(Input)[CurrentRun], "_", strategies[Strategy], ".txt", sep = ""),
                row.names = F, col.names = T, sep = '\t')
    
    ##################################### Effects and Costs of Screening Strategy #####################################
    # Assess effects
    UD_QALYS <- ScreenedOutcomes[, "AllCauseDeath"]
    D_QALYS <- D_LYS <- sapply(UD_QALYS, PresentValue)  # Apply the discounting function that we defined above
    
    # Quality-of-life only changes when entering another health state in their natural history
    SickOutcome <- cbind(SickOutcome[ , - length(StagesInOrder)], ScreenedOutcomes[Sick, "AllCauseDeath"])  # Update the age of death in the natural history of all the sick people
    Dis_SickOutcome <- apply(SickOutcome, MARGIN = c(1, 2), PresentValue)  # Apply the discounting function
    
    UD_QALYS[DiseaseFree] <- UD_QALYS[DiseaseFree] * Utility[1]  # Quality-of-life adjusted but not discounted
    UD_QALYS[Sick] <- apply(SickOutcome, 1, Accumulated_QALYS)
    D_QALYS[DiseaseFree] <- D_QALYS[DiseaseFree] * Utility[1]  # Quality-of-life adjusted and discounted
    D_QALYS[Sick] <- apply(Dis_SickOutcome, 1, Accumulated_QALYS)
    
    # Calculate utility loss due to screening test
    DiscountFactor_Screen <- DF_Effects[ScreenCounts[, "ScreenAge"]]
    UD_ScreenHarm <- sum(ScreenCounts[,"N_Screens"] * Input[paste("TestDisutility", ScreenCounts[,"TestApplied"], sep=""), CurrentRun])
    D_ScreenHarm <- sum(ScreenCounts[,"N_Screens"] * Input[paste("TestDisutility", ScreenCounts[,"TestApplied"], sep=""), CurrentRun] * DiscountFactor_Screen)  # Discounted
    UD_TriageHarm <- sum((ScreenCounts[, "TruePositives"] + ScreenCounts[, "FalsePositives"]) * Input["DisutilityTriage", CurrentRun])  # All the test positives are triaged
    D_TriageHarm <- sum((ScreenCounts[, "TruePositives"] + ScreenCounts[, "FalsePositives"]) * Input["DisutilityTriage", CurrentRun] * DiscountFactor_Screen)
    
    # Calculate utility loss due to treatment
    ClinicalOnsetAges <- ceiling(ScreenedOutcomes[NotScreenedClinicalCases, "Clinical"])  # Identify the clinical ages and round up to integers for discounting
    UD_TreatmentHarm <- sum(ScreenCounts[,"TruePositives"] * Input["DisutilityTrt", CurrentRun]) +   # Diagnosed with screening
                        length(NotScreenedClinicalCases) * Input["DisutilityTrt", CurrentRun]  # Diagnosed without screening
    D_TreatmentHarm <- sum(ScreenCounts[,"TruePositives"] * Input["DisutilityTrt", CurrentRun] * DiscountFactor_Screen) +   # Receive treatment at the age of screening
                       sum(Input["DisutilityTrt", CurrentRun] * DF_Effects[ClinicalOnsetAges])  # Receive treatment when entering clinical state in natural history
    
    # Calculate the discounted life-years without screening
    UD_Strategy_LYG <- sum(ScreenedOutcomes[, "AllCauseDeath"])
    D_Strategy_LYG <- sum(D_LYS)
    UD_Strategy_QALY <- sum(UD_QALYS) - UD_ScreenHarm - UD_TriageHarm - UD_TreatmentHarm
    D_Strategy_QALY <- sum(D_QALYS) - D_ScreenHarm - D_TriageHarm - D_TreatmentHarm
    
    # Assess costs
    # Count the late treatment costs
    DiscountFactor_Screen <- DF_Costs[ScreenCounts[, "ScreenAge"]]
    UD_LateTreatmentCosts <- length(NotScreenedClinicalCases) * LateTreatmentCost
    D_LateTreatmentCosts <- sum(DF_Costs[ClinicalOnsetAges] * LateTreatmentCost)  # Find the discounted treatment costs
    
    # First calculate the undiscounted amounts: the costs of primary screening, triage and treating preclinical disease under screening
    UD_ScreenCosts <- sum(ScreenCounts[, "N_Screens"] * ScreenCost)  # The primary test cost applies to all screens
    UD_TriageCosts <- sum((ScreenCounts[, "TruePositives"] + ScreenCounts[, "FalsePositives"]) * TriageCost)  # The cost of triage applies to all screen true and false positives
    UD_EarlyTreatmentCosts <- sum(ScreenCounts[, "TruePositives"] * EarlyTreatmentCost)  # The early treatment costs applies to all true positives (assuming a 100% specific triage test)
    
    # Now calculate the discounted values
    ScreenCostArray <- array(NA, dim = c(NumberOfScreens, 4))  # Create a blank array for the discounted costs which can also be saved as intermediate outcomes
    colnames(ScreenCostArray) <- c("Age", "D_Screen", "D_Triage", "D_EarlyTreatment")  # Name the columns in this array
    ScreenCostArray[, "Age"] <- ScreenCounts[, "ScreenAge"]  # Apply the appropriate discount factors according to the screen ages
    ScreenCostArray[, "D_Screen"] <- DiscountFactor_Screen * ScreenCounts[, "N_Screens"] * ScreenCost  # Multiply the screen, triage and early treatment numbers by the discount factor
    ScreenCostArray[, "D_Triage"] <- DiscountFactor_Screen * (ScreenCounts[, "TruePositives"] + ScreenCounts[, "FalsePositives"]) * TriageCost
    ScreenCostArray[, "D_EarlyTreatment"] <- DiscountFactor_Screen * ScreenCounts[, "TruePositives"] * EarlyTreatmentCost
    
    # Sum up the undiscounted and discounted costs
    UD_TotalCosts <- UD_ScreenCosts + UD_TriageCosts + UD_EarlyTreatmentCosts + UD_LateTreatmentCosts
    D_TotalCosts <- sum(ScreenCostArray[, c("D_Screen", "D_Triage", "D_EarlyTreatment")]) + D_LateTreatmentCosts
    
    # Record intermediate outcomes and the first column is the name of the strategy
    IntermediateOutcomes[Strategy + 1, - 1] <- c(UD_ScreenCosts         = UD_ScreenCosts, 
                                                 UD_TriageCosts         = UD_TriageCosts, 
                                                 UD_EarlyTreatmentCosts = UD_EarlyTreatmentCosts, 
                                                 UD_LateTreatmentCosts  = UD_LateTreatmentCosts, 
                                                 D_ScreenCosts          = sum(ScreenCostArray[, "D_Screen"]), 
                                                 D_TriageCosts          = sum(ScreenCostArray[, "D_Triage"]), 
                                                 D_EarlyTreatmentCosts  = sum(ScreenCostArray[, "D_EarlyTreatment"]), 
                                                 D_LateTreatmentCosts   = D_LateTreatmentCosts, 
                                                 N_CancerDeaths, 
                                                 N_Overdiagnosed, 
                                                 N_ClinicalCases 
                                                 )
    # Sum up the undiscounted and discounted costs
    CostEffectivenessResults[Strategy + 1, c("UD_EffectsL", "UD_EffectsQ", "UD_Costs", "D_EffectsL", "D_EffectsQ", "D_Costs")] <-c(UD_Strategy_LYG, 
                                                                                                                                   UD_Strategy_QALY, 
                                                                                                                                   UD_TotalCosts, 
                                                                                                                                   D_Strategy_LYG, 
                                                                                                                                   D_Strategy_QALY, 
                                                                                                                                   D_TotalCosts 
                                                                                                                                   )
  }  # Close the loop over all the strategies
  ##################################### Calculation of ICERs (Please See the References) #####################################
  # Mark those subject to simple dominance as SD
  SDfinder <- function(x, output){  # This simply compares each strategy to all others to find cases of simple dominance
    if (sum((as.numeric(x[1]) <= AllEffects) * (as.numeric(x[2]) > AllCosts)) >= 1){
      return("SD")
    }
    if (sum((as.numeric(x[1]) < AllEffects) * (as.numeric(x[2]) >= AllCosts)) >= 1){
      return("SD")
    }
    return("NA")
  }
  
  AllCosts <- as.numeric(CostEffectivenessResults[, "D_Costs"])
  AllEffects <- as.numeric(CostEffectivenessResults[, "D_EffectsL"])
  CostEffectivenessResults[, "D_ICER_L"] <- apply(CostEffectivenessResults[, c('D_EffectsL', 'D_Costs')], 1, SDfinder)  # Discounted LYs gained
  AllEffects <- as.numeric(CostEffectivenessResults[, "D_EffectsQ"])
  CostEffectivenessResults[, "D_ICER_Q"] <- apply(CostEffectivenessResults[, c('D_EffectsQ', 'D_Costs')], 1, SDfinder)  # Discounted QALYs gained
  AllCosts <- as.numeric(CostEffectivenessResults[, "UD_Costs"])
  AllEffects <- as.numeric(CostEffectivenessResults[, "UD_EffectsL"])
  CostEffectivenessResults[, "UD_ICER_L"] <- apply(CostEffectivenessResults[, c('UD_EffectsL', 'UD_Costs')], 1, SDfinder)  # Undiscounted LYs gained
  AllEffects <- as.numeric(CostEffectivenessResults[, "UD_EffectsQ"])
  CostEffectivenessResults[, "UD_ICER_Q"] <- apply(CostEffectivenessResults[, c('UD_EffectsQ', 'UD_Costs')], 1, SDfinder)  # Undiscounted QALYs gained
  
  # Find cost-effective screening strategies on the efficient frontier
  EDfinder <- function(CEresults, Effects, Costs, ICER, output){
    # Define the boundary set as the non strictly-dominated strategies
    BoundarySet <- CEresults[setdiff(seq(1:nrow(CEresults)), which(CEresults[, ICER] == "SD")),]
    # Order the boundary set in terms of effectiveness
    BoundarySet <- BoundarySet[order(BoundarySet[, Effects]),]
    # The first efficient strategy is the strategy with the least effectiveness
    EfficientSet <- 1
    
    if (nrow(BoundarySet) == 1 | (length(unique(BoundarySet[, Effects])) == 1 & length(unique(BoundarySet[, Costs])) == 1)){  # Only one strategy or strategies with the same outcomes
      BoundarySet[, ICER] <- "reference"  # This strategy is the only strategy on the efficient frontier, so without ICER
    } else {
        # Set the ICERs as numeric
        BoundarySet[, ICER] <- as.numeric(BoundarySet[, ICER])
        # Put a the break condition at the beginning that stops the routine if the end of the set is reached
        if (max(EfficientSet) < nrow(BoundarySet)){
          repeat{
            # Update the ICER calculation relative to the new reference
            for (i in ((max(EfficientSet) + 1): nrow(BoundarySet))){
              # Calculate the column of ACERs relative to the least-effective non simply dominated strategy
              BoundarySet[i, ICER] <- round((BoundarySet[i, Costs] - BoundarySet[max(EfficientSet), Costs])/
                                            (BoundarySet[i, Effects] - BoundarySet[max(EfficientSet), Effects]), 3)
            }
            # Find the lowest ICER
            j <- max(EfficientSet) + which(BoundarySet[(max(EfficientSet) + 1):nrow(BoundarySet), ICER] == 
                                           min(BoundarySet[(max(EfficientSet) + 1):nrow(BoundarySet), ICER], na.rm=TRUE))
            # Update what forms the efficient set
            EfficientSet <- c(EfficientSet, j)
            
            if (max(EfficientSet) == nrow(BoundarySet)){break}
          }
        }
        BoundarySet[-EfficientSet, ICER] <- "ED"
        # Set the ICER of the least effective strategy in the efficient set to "reference"
        BoundarySet[1, ICER] <- "reference"
      }
    return(BoundarySet)
  }
  
  BoundarySet_L <- EDfinder(CostEffectivenessResults, "D_EffectsL", "D_Costs", "D_ICER_L")
  BoundarySet_Q <- EDfinder(CostEffectivenessResults, "D_EffectsQ", "D_Costs", "D_ICER_Q")
  BoundarySet_UL <- EDfinder(CostEffectivenessResults, "UD_EffectsL", "UD_Costs", "UD_ICER_L")
  BoundarySet_UQ <- EDfinder(CostEffectivenessResults, "UD_EffectsQ", "UD_Costs", "UD_ICER_Q")
  
  # Write in the ICERs of the efficient strategies
  CostEffectivenessResults[match(BoundarySet_L[, "StrategyName"], CostEffectivenessResults[, "StrategyName"]), "D_ICER_L"] <- BoundarySet_L[, "D_ICER_L"]
  CostEffectivenessResults[match(BoundarySet_Q[, "StrategyName"], CostEffectivenessResults[, "StrategyName"]), "D_ICER_Q"] <- BoundarySet_Q[, "D_ICER_Q"]
  CostEffectivenessResults[match(BoundarySet_UL[, "StrategyName"], CostEffectivenessResults[, "StrategyName"]), "UD_ICER_L"] <- BoundarySet_UL[, "UD_ICER_L"]
  CostEffectivenessResults[match(BoundarySet_UQ[, "StrategyName"], CostEffectivenessResults[, "StrategyName"]), "UD_ICER_Q"] <- BoundarySet_UQ[, "UD_ICER_Q"]
  BoundarySet_L <- BoundarySet_L[which(!BoundarySet_L[, "D_ICER_L"] %in% "ED"), ]  # Update the BoundarySet by removing strategies with "ED"
  BoundarySet_Q <- BoundarySet_Q[which(!BoundarySet_Q[, "D_ICER_Q"] %in% "ED"), ]
  BoundarySet_UL <- BoundarySet_UL[which(!BoundarySet_UL[, "UD_ICER_L"] %in% "ED"), ]
  BoundarySet_UQ <- BoundarySet_UQ[which(!BoundarySet_UQ[, "UD_ICER_Q"] %in% "ED"), ]
  
  ##################################### Save Results #####################################
  # Return the results for all values and the efficient set within the R session
  # Save the overall results and efficient frontier results
  write.table(IntermediateOutcomes, paste("OutputFiles/LatestResults/IntermediateOutcomtes/CEbreakdown/", colnames(Input)[CurrentRun], ".txt", sep = ""), 
              row.names = F, col.names = T, sep = '\t')
  write.table(CostEffectivenessResults, paste("OutputFiles/LatestResults/MainCEresults/OutputTable_", colnames(Input)[CurrentRun], ".txt", sep=""), 
              row.names = F, col.names = T, sep = '\t')
  write.table(BoundarySet_L, paste("OutputFiles/LatestResults/MainCEresults/BoundarySet/BoundarySet_L_", colnames(Input)[CurrentRun], ".txt", sep=""), 
              row.names = F, col.names = T, sep = '\t')
  write.table(BoundarySet_Q, paste("OutputFiles/LatestResults/MainCEresults/BoundarySet/BoundarySet_Q_", colnames(Input)[CurrentRun], ".txt", sep=""), 
              row.names = F, col.names = T, sep = '\t')
  write.table(BoundarySet_UL, paste("OutputFiles/LatestResults/MainCEresults/BoundarySet/BoundarySet_UL_", colnames(Input)[CurrentRun], ".txt", sep=""), 
              row.names = F, col.names = T, sep = '\t')
  write.table(BoundarySet_UQ, paste("OutputFiles/LatestResults/MainCEresults/BoundarySet/BoundarySet_UQ_", colnames(Input)[CurrentRun], ".txt", sep=""), 
              row.names = F, col.names = T, sep = '\t')
  
  # Plot cost-effectiveness plane 
  if ((colnames(Input)[CurrentRun] == "Input") & (CEplane == TRUE)){  # Only plot when it is the base-case
    # Define the outcome measure
    if (MeasureQALYs == TRUE){
      OutcomeMeasure <- "D_EffectsQ"
      ICERMeasure <- "D_ICER_Q"
      BoundarySet <- BoundarySet_Q
    } else {
        OutcomeMeasure <- "D_EffectsL"
        ICERMeasure <- "D_ICER_L"
        BoundarySet <- BoundarySet_L
      }
    # Define the costs and effects, reference to the no-screening
    Effects <- (CostEffectivenessResults[, OutcomeMeasure] - CostEffectivenessResults[1, OutcomeMeasure]) / SampleSize
    Costs <- (CostEffectivenessResults[, "D_Costs"] - CostEffectivenessResults[1, "D_Costs"]) / SampleSize
    BoundarySet$EfficientEffects <- (BoundarySet[, OutcomeMeasure] - BoundarySet[1, OutcomeMeasure]) / SampleSize
    BoundarySet$EfficientCosts <- (BoundarySet[, "D_Costs"] - BoundarySet[1, "D_Costs"]) / SampleSize
    
    # Identify the highest of the cost-effective ICERs given the threshold
    ICERs <- as.numeric(BoundarySet[, ICERMeasure])  # All the ICERs
    if (min(ICERs, na.rm = TRUE) <= Threshold){
      OptimalStrategy <- rownames(BoundarySet)[which(ICERs == (max(ICERs[ICERs <= Threshold], na.rm=TRUE)))]  # Find the maximum ICER lower than the threshold
    } else {
        OptimalStrategy <- rownames(BoundarySet)[BoundarySet[, ICERMeasure] %in% "reference"]  # All the ICERs are higher than the threshold, so the reference is the optimal strategy
      }
    CostEffectiveICER <- BoundarySet[OptimalStrategy, ICERMeasure]  # Find the corresponding ICER
    CostEffectiveEffects <- as.numeric(BoundarySet[OptimalStrategy, "EfficientEffects"])  # Find the corresponding costs and effect
    CostEffectiveCosts <- as.numeric(BoundarySet[OptimalStrategy, "EfficientCosts"])
    
    # Set the tick marks: after plotting several times, these numbers are chosen to best present the axis in our example
    TicksEffects <- pretty(c(round(min(Effects) - (max(Effects) - min(Effects)) / 20, 5), round(max(Effects) + (max(Effects) - min(Effects)) / 20, 5)))
    TicksCosts <- pretty(c(0, max(Costs) * 1.02))
    # Set the ranges for both values
    xRange <- range(TicksEffects)
    yRange <- range(TicksCosts)
    
    # Create ICER and axis labels
    ICERLabels <- gsub("NA", "", noquote(format(BoundarySet[, ICERMeasure], big.mark = "")))
    if (MeasureQALYs == TRUE){LabLabel <- "Effects (QALY, per capita)"} else {LabLabel <- "Effects (LYG, per capita)"}
    
    # Set the plot dimension
    windows.options(width = 12, height = 9)
    # Set the margin parameters
    par(mar = c(5, 6, 1, 1))
    
    if (HealthOnXAxis == TRUE){
      # Plot the plane
      plot(Effects, Costs, yaxt = "n", xaxt = "n", xlab = "", ylab = "", xlim = xRange, ylim = yRange, pch = 16, cex = 0.5, col = "grey")
      # Add the axes
      axis(1, at = TicksEffects, las = 1)
      axis(2, at = TicksCosts, las = 1)
      # Add in the X and Y axis labels with spacing
      title(ylab = "Costs ($, per capita)", xlab = LabLabel, mgp = c(3.75,1.75,0), cex.lab = 1.2)
      # Add the efficient frontier
      lines(BoundarySet$EfficientEffects, BoundarySet$EfficientCosts, lwd = 2)
      points(BoundarySet$EfficientEffects, BoundarySet$EfficientCosts, pch = 19, lwd = 2)
      # Add the optimal strategy
      if (ShowOptimalStrategy == TRUE){points(CostEffectiveEffects, CostEffectiveCosts, pch = 19, lwd = 3, cex = 1, col = "forestgreen")}
      # Add ICERs to the frontier
      LabelYOffset <- max(BoundarySet$EfficientCosts) / 10  # Generate a label offset to place the values at an offset
      if (ShowICERs == TRUE){text(BoundarySet$EfficientEffects, BoundarySet$EfficientCosts - LabelYOffset, labels = ICERLabels, pos = 4, cex = 0.6)}  # Apply the text
    } else {
        plot(Costs, Effects, yaxt = "n", xaxt = "n", xlab = "", ylab = "", xlim = yRange, ylim = xRange, pch = 16, cex = 0.5, col = "grey")
        # Add the axes
        axis(2, at = TicksEffects, las = 1)
        axis(1, at = TicksCosts, las = 1)
        # Add in the X and Y axis labels with spacing
        title(xlab = "Costs ($, per capita)", ylab = LabLabel, mgp = c(3.75,1.75,0), cex.lab = 1.2)
        # Add the efficient frontier
        lines(BoundarySet$EfficientCosts, BoundarySet$EfficientEffects, lwd = 2)
        points(BoundarySet$EfficientCosts, BoundarySet$EfficientEffects, pch = 19, lwd = 2)
        # Add the optimal strategy
        if (ShowOptimalStrategy == TRUE){points(CostEffectiveCosts, CostEffectiveEffects, pch = 19, lwd = 3, cex = 1, col = "forestgreen")}
        # Add ICERs to the frontier
        LabelYOffset <- max(BoundarySet$EfficientEffects) / 18   # Generate a label offset to place the values at an offset
        LabelXOffset <- max(BoundarySet$EfficientCosts) / 40
        if (ShowICERs == TRUE){text(BoundarySet$EfficientCosts + LabelXOffset, BoundarySet$EfficientEffects + LabelYOffset, labels = ICERLabels, pos = 2,cex = 0.6)}  # Apply the text
      }
    
    # Save the graph as a .jpeg as a matter of course
    SaveCEplane <- recordPlot()
    jpeg("OutputFiles/Figures/Figure.jpeg", width = 4000, height = 2400, res = 300)
    replayPlot(SaveCEplane)
    dev.off()
    
    # Save the graph as a PDF or png if that option is ticked
    if (SaveGraphPDF == TRUE){
      pdf("OutputFiles/Figures/Figure.pdf", width = 15, height = 9)
      replayPlot(SaveCEplane)
      dev.off()
    }
    if (SaveGraphPNG == TRUE){
      png("OutputFiles/Figures/Figure.png", width = 4000, height = 2400, res = 300)
      replayPlot(SaveCEplane)
      dev.off()
    }
  }
}
Sys.time() - time  # The execution time
##################################### End #####################################
