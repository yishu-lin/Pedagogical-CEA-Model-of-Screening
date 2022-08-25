# Plot results with R Shiny
# Set the working file directory path
try(setwd(dirname(sys.frame(1)$ofile)), silent = TRUE)
try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)), silent = TRUE)

#install.packages("shiny")
library(shiny) 

# Import the required data from the data sheets created by the Excel file
InputTable <- read.table(file = paste("InputFiles/InputTable.txt"), header = T)
source("InputFiles/Misc.txt")  # To obtain the sample size

ui <- fluidPage(title = "Cost-effectiveness Analysis",  # Add a browser window title
                fluidRow(column(2, # The width of this column
                                h2("Cost-effectiveness Analysis"),  # Add a second level header
                                radioButtons("AnalysisType", label = "Analysis Type", choices = list("Deterministic analysis" = "DeterministicAnalysis", 
                                                                                                     "Scenario analysis" = "Owsa")),  # Create a list to choose
                                radioButtons("EffectMeasurement", label = "Effect measurement", choices = list("Quality-adjusted life-years" = "QALYs", 
                                                                                                               "Life-years" = "LYs")), 
                                radioButtons("Discount", label = "Discount", choices = list("Yes" = "Yes", "No" = "No")), 
                                hr(),  # Add a line
                                h3("Scenario Analysis"),  # Add a third level header
                                sliderInput("slider1",  # Give a variable name to stand for this slider
                                            #label = "Parameter Value",  # The title of this slide can be added
                                            label = div(style = 'width: 230px;', div(style = 'float: left;', 'Minimum'), div(style = 'float: right;', 'Maximum')),  # Adjust position 
                                            min = 1, max = 3, value = 1, step = 1),  # Output range is 1 to 3
                                radioButtons("parameters", label = "Parameters", choices = list("Test sensitivity" = "TestSensitivity1", # Create a list to choose
                                                                                                "Test specificity" = "TestSpecificity1", 
                                                                                                "Incidence rate" = "Incidence", 
                                                                                                "Life expectancy" = "Survival", 
                                                                                                "Stage sojourn time: preclinical" = "StageScale2", 
                                                                                                "Stage sojourn time: clinical" = "StageScale3", 
                                                                                                "Treatment sucess probability: preclinical" = "PreClinicalProbability", 
                                                                                                "Treatment sucess probability: clinical" = "ClinicalProbability", 
                                                                                                "Cost: primary screen" = "CostPrimaryScreen", 
                                                                                                "Cost: followUp" = "CostFollowUp", 
                                                                                                "Cost: treatment (screen-detected)" = "CostTrtScreen", 
                                                                                                "Cost: treatment (clinical)" = "CostTrtClinical"
                                                                                                #"Test disutility" = "TestDisutility1",  # These variables are not analysed in our example
                                                                                                #"Stage utility: healthy" = "StageUtility1", 
                                                                                                #"Stage utility: preclinical" = "StageUtility2", 
                                                                                                #"Stage utility: clinical" = "StageUtility3", 
                                                                                                #"Disutility: triage" = "DisutilityTriage", 
                                                                                                #"Disutility: treatment" = "DisutilityTrt", 
                                                                                                #"Discount rate - costs" = "DiscountRateCosts", 
                                                                                                #"Discount rate - effects" = "DiscountRateEffects", 
                                                                                                #"Discount year" = "DiscountYear"
                                                                                                )
                                             )
                                ), 
                         column(10,  # Create a new column with the width 10
                                plotOutput("distPlot1"),  # Plot the diagram
                                hr()  # Draw a line
                                ), 
                         column(3,  # Create a new column with the width 3
                                h4("Figure Settings"),  # Add a fourth level header
                                radioButtons("CEaxis", label = "CE plane axis", choices = list("Effects on X axis" = "EffectX", "Effects on Y axis" = "EffectY")), 
                                radioButtons("FigureRange", label = "Set the axis range, according to...", choices = list("All the parameters" = "Large", 
                                                                                                                          "The chosen parameter" = "Small")), 
                                sliderInput("slider2", label = "Threshold", min = 10000, max = 200000, value = 50000, step = 5000)
                                ), 
                         column(3,  # Create a new column with the width 3
                                h4("The Observed Strategy"),
                                sliderInput("startage", label = "Screen startage", min = 25, max = 75, value = 30, step = 1),  # Use three variables to define strategy
                                sliderInput("stopage", label = "Screen stopage", min = 50, max = 100, value = 70, step = 1), 
                                sliderInput("interval", label = "Screen interval", min = 1, max = 10, value = 1, step = 1), 
                                br(),  # Add a line break
                                tableOutput("ExtraResults")  # Plot the table
                                ), 
                         column(4,  # Create a new column with the width 4
                                plotOutput("ScreenCountPlot"),  # Plot the diagram
                                br(),  # Add a line break
                                plotOutput("AgeDist")  # Plot the diagram
                                )
                         )
                )

server <- function(input, output){  # We have four outputs in total: "distPlot1", "ExtraResults", "ScreenCountPlot", and "AgeDist"
  output$distPlot1 <- renderPlot({if (input$AnalysisType == "DeterministicAnalysis" | input$slider1 == "2"){  # To retrieve cost-effectiveness results
                                    ICERTable <- read.table(file = "OutputFiles/LatestResults/MainCEresults/OutputTable_Input.txt", header = T)  # For base-case
                                  } else if (input$slider1 == "1"){
                                      ICERTable <- read.table(file = paste("OutputFiles/LatestResults/MainCEresults/OutputTable_", 
                                                                           input$parameters, "_Low.txt", sep = ""), header = T)  # The scenario of lower value
                                    } else {
                                    ICERTable <- read.table(file = paste("OutputFiles/LatestResults/MainCEresults/OutputTable_", 
                                                                         input$parameters, "_High.txt", sep = ""), header = T)  # The scenario of higher value
                                      }
                                  ObsearvationPoint <- paste(input$startage, "_", input$stopage, "_", input$interval, sep = "")  # Name the interested strategy to fit ICERTable
                                  
                                  # Define the measurements of cost, effect, and ICER
                                  if ((input$EffectMeasurement == "QALYs") & (input$Discount == "Yes")){
                                    CostMeasure <- "D_Costs"
                                    EffectMeasure <- "D_EffectsQ"
                                    ICERMeasure <- "D_ICER_Q"
                                  } else if ((input$EffectMeasurement == "QALYs") & (input$Discount == "No")){
                                      CostMeasure <- "UD_Costs"
                                      EffectMeasure <- "UD_EffectsQ"
                                      ICERMeasure <- "UD_ICER_Q"
                                    } else if ((input$EffectMeasurement == "LYs") & (input$Discount == "Yes")){
                                        CostMeasure <- "D_Costs"
                                        EffectMeasure <- "D_EffectsL"
                                        ICERMeasure <- "D_ICER_L"
                                      } else {
                                          CostMeasure <- "UD_Costs"
                                          EffectMeasure <- "UD_EffectsL"
                                          ICERMeasure <- "UD_ICER_L"
                                        }
                                  PlotData <- ICERTable[, c("StrategyName", EffectMeasure, CostMeasure, ICERMeasure)]  # Create a table that included the interested result measurements
                                  colnames(PlotData) <- c("StrategyName", "Effects", "Costs", "ICER")  # Name the table
                                  PlotData[, c("Effects", "Costs")] <- PlotData[, c("Effects", "Costs")] / SampleSize  # Results in per capita
                                  Ref <- which(PlotData$ICER == "reference")  # Find the reference case
                                  PlotData[, "ICER"] <- as.numeric(as.character(PlotData$ICER))  #  Make ICER to a numeric variable
                                  BoundarySet <- PlotData[!(is.na(PlotData$ICER)), ]  # Create a table that only strategies on the efficient frontier
                                  
                                  SameParameterEffect <- NA  # Give this variable an initial value for recording the results from other scenarios
                                  SameParameterCost <- NA
                                  if ((input$AnalysisType == "DeterministicAnalysis")){  # Base-case
                                    MaxCost <- max(as.numeric(PlotData[, "Costs"]))
                                    MaxEffect <- max(as.numeric(PlotData[, "Effects"]))
                                    MinCost <- min(as.numeric(PlotData[, "Costs"]))
                                    MinEffect <- min(as.numeric(PlotData[, "Effects"]))
                                  } else if (input$AnalysisType == "Owsa"){  # Scenario analysis
                                      if (input$FigureRange == "Small"){  # The axis is set for this parameter
                                        for (filenumber in c("Low", "High")){  # Read low and high scenarios
                                          SameParameterTable <- read.table(file = paste("OutputFiles/LatestResults/MainCEresults/OutputTable_", 
                                                                                        input$parameters, "_", filenumber, ".txt", sep = ""), header = T)
                                          SameParameterEffect <- c(SameParameterEffect, SameParameterTable[, EffectMeasure] / SampleSize)  # Record effect results
                                          SameParameterCost <- c(SameParameterCost, as.numeric(SameParameterTable[, CostMeasure]) / SampleSize)  # Record cost results
                                        }
                                      } else {  # The axis is set for all the scenarios
                                          for (filenumber in c("Low", "High")){  # Read low and high scenarios
                                            for (ListParameter in c("TestSensitivity1",  # All the parameters included in our scenario analysis
                                                                    "TestSpecificity1", 
                                                                    "Incidence", 
                                                                    "Survival", 
                                                                    "StageScale2", 
                                                                    "StageScale3", 
                                                                    "PreClinicalProbability", 
                                                                    "ClinicalProbability", 
                                                                    "CostPrimaryScreen", 
                                                                    "CostFollowUp", 
                                                                    "CostTrtScreen", 
                                                                    "CostTrtClinical"
                                                                    #"TestDisutility1",  # These parameters are not analysed by our example
                                                                    #"StageUtility1", 
                                                                    #"StageUtility2", 
                                                                    #"StageUtility3", 
                                                                    #"DisutilityTriage", 
                                                                    #"DisutilityTrt", 
                                                                    #"DiscountRateCosts", 
                                                                    #"DiscountRateEffects", 
                                                                    #"DiscountYear"
                                                                    )){
                                              SameParameterTable <- read.table(file = paste("OutputFiles/LatestResults/MainCEresults/OutputTable_", 
                                                                                            ListParameter, "_", filenumber, ".txt", sep = ""), header = T)  # Read the table
                                              SameParameterEffect <- c(SameParameterEffect, SameParameterTable[, EffectMeasure] / SampleSize)  # Record effect results
                                              SameParameterCost <- c(SameParameterCost, as.numeric(SameParameterTable[, CostMeasure]) / SampleSize)  # Record cost results
                                            }
                                          }
                                        }
                                      MaxEffect <- max(SameParameterEffect, na.rm = TRUE)  # Find the maximum of effect results so we can define the range of the axis
                                      MinEffect <- min(SameParameterEffect, na.rm = TRUE)  # Find the minimum of effect result
                                      MaxCost <- max(SameParameterCost, na.rm = TRUE)  # Find the maximum of cost results
                                      MinCost <- min(SameParameterCost, na.rm = TRUE)  # Find the minimum of cost results
                                    }
                                  # Identify the highest of the cost-effective ICERs given the threshold
                                  Threshold <- input$slider2
                                  if (min(BoundarySet$ICER, na.rm = TRUE) <= Threshold){
                                    OptimalStrategy <- rownames(BoundarySet)[which(BoundarySet$ICER == (max(BoundarySet$ICER[BoundarySet$ICER <= Threshold], na.rm = TRUE)))]
                                    # Find the corresponding costs and effect
                                    CostEffectiveICER <- BoundarySet[OptimalStrategy, "ICER"]
                                    CostEffectiveEffects <- as.numeric(BoundarySet[OptimalStrategy, "Effects"])
                                    CostEffectiveCosts <- as.numeric(BoundarySet[OptimalStrategy, "Costs"])
                                  } else {  # When all the ICERs are higher than threshold, the optimal strategy is the reference case
                                      CostEffectiveICER <- "Ref"
                                      CostEffectiveEffects <- as.numeric(PlotData[Ref, "Effects"])
                                      CostEffectiveCosts <- as.numeric(PlotData[Ref, "Costs"])
                                    }
                                  
                                  Effects <- PlotData[, "Effects"]  # Prepare for plotting
                                  Costs <- PlotData[, "Costs"]
                                  
                                  # Set the tick marks to include zero and any big number
                                  TicksEffects <- pretty(c(round(MinEffect - (MaxEffect - MinEffect) / 20, 5), round(MaxEffect + (MaxEffect - MinEffect) / 20, 5)))
                                  TicksCosts <- pretty(c(0, MaxCost + MaxCost / 20))  # After trying, dividing by "20" plots the CE plane best which might change with different conditions
                                  
                                  # Set the ranges for both values
                                  xRange <- range(TicksEffects)
                                  yRange <- range(TicksCosts)
                                  
                                  # Create labels
                                  LabelTitle <- c("Low", "Medium", "High")  # The title for the CE plane specifies which scenario
                                  LabelTitle1 <- c("Low", "Input", "High")  # To read the corresponding parameter input
                                  if (input$AnalysisType == "DeterministicAnalysis"){  # To create the title label for the CE plane
                                    TitleLabel <- "Default scenario"
                                    } else if (!(input$parameters %in% c("Incidence", "Survival"))){
                                        TitleLabel <- paste("Scenario", LabelTitle[input$slider1], ":", input$parameters, "=", 
                                                            InputTable[which(InputTable$Parameter == input$parameters), LabelTitle1[input$slider1]], sep = " ")
                                      } else {TitleLabel <- paste("Scenario",LabelTitle[input$slider1], ":", input$parameters)}
                                  # To create ICER label
                                  if (CostEffectiveICER == "Ref"){ICERLabel <- "reference"} else {ICERLabel <- prettyNum(round(CostEffectiveICER), big.mark = ",")}
                                  
                                  # Plot the CE plane
                                  par(mar = c(5, 10, 5, 5))  # Set the margins
                                  if (input$CEaxis == "EffectX"){  # The X axis is the effectiveness
                                    plot(Effects, Costs, xlim = xRange, ylim = yRange, xlab = "", ylab = "", yaxt = "n", xaxt = "n", cex = 0.2)
                                    # Add the axes
                                    axis(1, at = TicksEffects,las = 1)
                                    axis(2, at = TicksCosts, las = 1)
                                    # Add a line to show the threshold
                                    abline(a = (CostEffectiveCosts - CostEffectiveEffects * Threshold), b = Threshold, col = "red", lwd = 2, lty = 2)
                                    # Add the efficient frontier
                                    sequence <- order(as.numeric(BoundarySet$Effects))
                                    lines(BoundarySet$Effects[sequence], BoundarySet$Costs[sequence], lwd = 2)
                                    points(BoundarySet$Effects, BoundarySet$Costs, pch = 19, lwd = 3, cex = 0.7)
                                    # Add the optimal strategy
                                    points(CostEffectiveEffects, CostEffectiveCosts, pch = 19, lwd = 3, cex = 1, col = "forestgreen")
                                    # Add the specific strategy that we want to observe
                                    if (ObsearvationPoint %in% PlotData$StrategyName){
                                      points(PlotData[PlotData$StrategyName == ObsearvationPoint, "Effects"], 
                                             PlotData[PlotData$StrategyName == ObsearvationPoint, "Costs"], 
                                             pch = 19, lwd = 3, cex = 1, col = "red")
                                    }
                                    # Add the ICERs to the frontier
                                    text(CostEffectiveEffects * 1.000001, CostEffectiveCosts * 0.6, labels = ICERLabel, pos = 4)  # Adjust the position of texts
                                    # Add in the X and Y axis labels with spacing
                                    title(TitleLabel, ylab = "Costs ($M)", xlab = paste("Effects,", input$EffectMeasurement), mgp = c(3.75,1.75,0), cex.lab = 1.2)
                                  } else {
                                      plot(Costs, Effects, xlim = yRange, ylim = xRange, xlab = "", ylab = "", yaxt = "n", xaxt = "n", cex = 0.2)
                                      # Add the axes
                                      axis(2, at = TicksEffects, las = 1)
                                      axis(1, at = TicksCosts, las = 1)
                                      # Add a line to show the threshold
                                      abline(a = CostEffectiveEffects - CostEffectiveCosts * (1 / Threshold), b = 1 / Threshold, col = "red", lwd = 2, lty = 2)
                                      # Add the efficient frontier
                                      sequence <- order(as.numeric(BoundarySet$Effects))
                                      lines(BoundarySet$Costs[sequence], BoundarySet$Effects[sequence], lwd = 2)
                                      points(BoundarySet$Costs, BoundarySet$Effects, pch = 19, lwd = 3, cex = 0.7)
                                      # Add the optimal strategy
                                      points(CostEffectiveCosts, CostEffectiveEffects, pch = 19, lwd = 3, cex = 1, col = "forestgreen")
                                      # Add the specific strategy that we want to observe
                                      if (ObsearvationPoint %in% PlotData$StrategyName){
                                        points(PlotData[PlotData$StrategyName == ObsearvationPoint, "Costs"], 
                                               PlotData[PlotData$StrategyName == ObsearvationPoint, "Effects"], 
                                               pch = 19, lwd = 3, cex = 1, col = "red")
                                      }
                                      # Add the ICERs to the frontier
                                      text(CostEffectiveCosts * 0.6, CostEffectiveEffects * 1.000001, labels = ICERLabel, pos = 2)  # Adjust the position of texts
                                      # Add in the X and Y axis labels with spacing
                                      title(TitleLabel, ylab = paste("Effects,", input$EffectMeasurement), xlab = "Costs ($M)", mgp = c(3.75,1.75,0), cex.lab = 1.2)
                                    }
                                  })
  output$ExtraResults <- renderTable({ObsearvationPoint <- paste(input$startage, "_", input$stopage, "_", input$interval, sep = "")  # Name the strategy to fit the result table
                                      if (input$AnalysisType == "DeterministicAnalysis" | input$slider1 == "2"){  # Base-case
                                        CEBreakDown <- read.table(file = paste("OutputFiles/LatestResults/IntermediateOutcomtes/CEBreakDown/Input.txt", sep = "_"), header = T)
                                      } else if (input$slider1 == "1"){  # Low scenario
                                          CEBreakDown <- read.table(file = paste("OutputFiles/LatestResults/IntermediateOutcomtes/CEBreakDown/", 
                                                                                 input$parameters, "_Low.txt", sep = ""), header = T)
                                        } else {  # High scenario
                                            CEBreakDown <- read.table(file = paste("OutputFiles/LatestResults/IntermediateOutcomtes/CEBreakDown/", 
                                                                                   input$parameters, "_High.txt", sep = ""), header = T)
                                          }
                                      if (sum(CEBreakDown$StrategyName == ObsearvationPoint) == 1){  # Only when one record is found for this strategy
                                        ResultTable <- cbind(c("Undiscounted screen costs",  # Variable label
                                                               "Undiscounted triage costs", 
                                                               "Undiscounted early treatment costs", 
                                                               "Undiscounted late treatment costs", 
                                                               "Discounted screen costs", 
                                                               "Discounted triage costs", 
                                                               "Discounted early treatment costs", 
                                                               "Discounted late treatment costs", 
                                                               "Number of cancer deaths", 
                                                               "Number of overdiagnosed", 
                                                               "Number of clinical cases"
                                                               ), 
                                                             t(CEBreakDown[CEBreakDown$StrategyName == ObsearvationPoint, ][-1])
                                                             )
                                        colnames(ResultTable) <- c(paste("Aged ", input$startage, " to ",input$stopage, ", ", input$interval, " yearly",sep=""), " ")  # Column label
                                      } else {
                                          ResultTable <- c("No result is available for the chosen strategy.")
                                        }
                                      ResultTable
                                      })
  output$ScreenCountPlot <- renderPlot({ObsearvationPoint <- paste(input$startage, "_", input$stopage, "_", input$interval, sep = "")  # Name the strategy to fit the result table
                                        Folder <- c("OutputFiles/LatestResults/IntermediateOutcomtes/ScreenCounts/")  # For redirecting the file directory path
                                        if ((input$AnalysisType == "DeterministicAnalysis" | input$slider1 == "2") & 
                                            (file.exists(paste(Folder, "Input_", ObsearvationPoint, ".txt", sep = "")))){  # Check if we have this intermediate result
                                          ScreenCounts <- read.table(file = paste(Folder, "Input_", ObsearvationPoint, ".txt", sep = ""), header = T)  # Base-case
                                        } else if ((input$slider1 == "1") & (file.exists(paste(Folder, input$parameters, "_Low_", ObsearvationPoint, ".txt", sep = "")))){
                                            ScreenCounts <- read.table(file = paste(Folder, input$parameters, "_Low_", ObsearvationPoint, ".txt", sep = ""), header = T)
                                          } else if (file.exists(paste(Folder, input$parameters, "_High_", ObsearvationPoint, ".txt", sep = ""))){
                                              ScreenCounts <- read.table(file = paste(Folder, input$parameters, "_High_", ObsearvationPoint, ".txt", sep = ""), header = T)
                                            } else {
                                                ScreenCounts <- NA  # If there is no file recording this intermediate result
                                              }
                                        
                                        if (!(is.na(ScreenCounts))){
                                          PlotData <- as.matrix(rbind(ScreenCounts$RealPostives, ScreenCounts$RealPostives - ScreenCounts$TruePositives))  # Create a matrix for plotting
                                          # Add extra space to the margin of the plot within frame
                                          par(mar = c(5, 4, 4, 6) + 0.1)
                                          barplot(PlotData, main = "Screen Performance", axes = FALSE, xlab = "Screen age", beside = FALSE, 
                                                  col = c("lightblue", "darkgrey"), border = "white")  # Plot a bar chart
                                          axis(2, col = "black", las = 1)  # Add the axis on the left
                                          mtext("Number of cases", side = 2, line = 2.5)  # Add the axis label on the left
                                          legend("topleft", legend = c("Detected Cases", "Undetected Cases"), 
                                                 col = c("lightblue", "darkgrey"), lty = 1, cex = 0.8)  # Add a legend to the bar chart
                                          box()  # Add a box around the plot
                                          # Allow a second plot on the same graph
                                          par(new = TRUE)
                                          plot(ScreenCounts$ScreenAge, ScreenCounts$FalsePositives, pch = 15, 
                                               xlab = "", ylab = "", axes = FALSE, type = "l", col = "brown")  # Add a line
                                          axis(4, col = "brown", col.axis = "brown", las = 1)  # Add the axis on the right
                                          mtext("Number of false positives", side = 4, col = "brown", line = 4)  # Add the axis label on the right
                                        }
                                        })
  output$AgeDist <- renderPlot({ObsearvationPoint <- paste(input$startage, "_", input$stopage, "_", input$interval, sep = "")  # Name the strategy to fit the result table
                                Folder <- c("OutputFiles/LatestResults/IntermediateOutcomtes/AgeDistribution/")  # For redirecting the file directory path
                                if ((input$AnalysisType == "DeterministicAnalysis" | input$slider1 == "2") & 
                                    (file.exists(paste(Folder, "Input_", ObsearvationPoint, ".txt", sep = "")))){  # Check if we have this intermediate result
                                  AgeDist <- read.table(file = paste(Folder, "Input_", ObsearvationPoint, ".txt", sep = ""), header = T)  # Base-case
                                } else if ((input$slider1 == "1") & (file.exists(paste(Folder, input$parameters, "_Low_", ObsearvationPoint, ".txt", sep = "")))){
                                    AgeDist <- read.table(file = paste(Folder, input$parameters, "_Low_", ObsearvationPoint, ".txt", sep = ""), header = T)
                                  } else if (file.exists(paste(Folder, input$parameters, "_High_", ObsearvationPoint, ".txt", sep = ""))){
                                      AgeDist <- read.table(file = paste(Folder, input$parameters, "_High_", ObsearvationPoint, ".txt", sep = ""), header = T)
                                    } else {
                                        AgeDist <- NA  # If there is no file recording this intermediate result
                                      }
                                
                                if (!(is.na(AgeDist))){
                                  AgeDist$group <- rep(seq(5, 100, by = 5), each = 5)  # Group by 5 years
                                  AgeDist <- aggregate(AgeDist, by = list(Category = AgeDist$group), FUN = sum)  # Sum up the numbers in each health state in each group
                                  
                                  par(mar = c(5, 4, 4, 6) + 0.1)  # Add extra space to the margin of the plot within frame
                                  plot(AgeDist$Category, AgeDist$OtherCauseDeath / 1000, axes = FALSE, 
                                       main = "Disease History", xlab = "Screen Age", ylab = "", type = "l", col = c("#F5C710"), lwd = 2)  # Draw a line for other-cause death
                                  box()  # Add a box around the plot
                                  axis(1, col = "black", las = 1)  # Add the x axis
                                  lines(AgeDist$Category, AgeDist$AllCauseDeath / 1000, col = "gray62", lwd = 2)  # Draw another line for all-cause death
                                  axis(4, col = "gray62", las = 1)  # Add the axis on the right
                                  mtext("Number of other-cuase and all-cause deaths (thousands)", side = 4, line = 2.5, col = "gray62", col.axis = "gray62")  # Add the axis label (right)
                                  legend("topleft", legend = c("Preclinical", "Clinical", "Cause-Specific Death", "Other-Cause Death", "All-Cause Death"), 
                                         col = c("#DF536B", "#61D04F", "#2297E6", "#F5C710", "gray62"), lty = c(1, 1, 1, 1, 1), cex = 0.8)  # Add a legend
                                  # Allow a second plot on the same graph
                                  par(new = TRUE)  # The following health states share the same axis
                                  plot(AgeDist$Category, AgeDist$Preclinical, pch = 16, axes = FALSE, xlab = "", ylab = "", type = "l", col = "#DF536B", lwd = 2)  # Preclinical
                                  lines(AgeDist$Category, AgeDist$Clinical, col = "#61D04F", lwd = 2)  # Clinical
                                  lines(AgeDist$Category, AgeDist$CauseSpecificDeath, col = "#2297E6", lwd = 2)  # Cause-specific death
                                  axis(2, col = "black", las = 1)    # Add the axis on the left
                                  mtext("Number of preclinical/ clinical/ cause-specific deaths", side = 2, line = 2.5)  # Add the axis label (left)
                                }
                                })
}
shinyApp(ui = ui, server = server)  # Apply Shiny app to connect ui and server