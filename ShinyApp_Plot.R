# Plot results with R Shiny
# Set the working file directory path
try(setwd(dirname(sys.frame(1)$ofile)), silent = TRUE)
try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)), silent = TRUE)

#install.packages("shiny")
library(shiny) 
#library(shinyscreenshot)  # For screenshot

# Import the required data from the data sheets created by the Excel file
InputTable <- read.table(file = paste("InputFiles/InputTable.txt"), header = T)
source("InputFiles/Misc.txt")  # SampleSize

ui <- fluidPage(title = "Cost-effectiveness Analysis", 
                fluidRow(column(2, 
                                h2("Cost-effectiveness Analysis"), 
                                radioButtons("AnalysisType", label = "Analysis Type", choices = list("Deterministic analysis" = "DeterministicAnalysis", 
                                                                                                     "Scenario analysis" = "Owsa")), 
                                radioButtons("EffectMeasurement", label = "Effect measurement", choices = list("Quality-adjusted life-years" = "QALYs", 
                                                                                                               "Life-years" = "LYs")), 
                                radioButtons("Discount", label = "Discount", choices = list("Yes" = "Yes", "No" = "No")), 
                                hr(), 
                                h3("Scenario Analysis"), 
                                sliderInput("slider1", 
                                            #label = "Parameter Value", 
                                            label = div(style = 'width: 230px;', div(style = 'float: left;', 'Minimum'), div(style = 'float: right;', 'Maximum')), 
                                            min = 1, max = 3, value = 1, step = 1), 
                                radioButtons("parameters", label = "Parameters", choices = list("Test sensitivity" = "TestSensitivity1", 
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
                                                                                                #"Test disutility" = "TestDisutility1", 
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
                         column(10, 
                                plotOutput("distPlot1"), 
                                hr()
                                ), 
                         column(3, 
                                h4("Figure Settings"), 
                                radioButtons("CEaxis", label = "CE plane axis", choices = list("Effects on X axis" = "EffectX", "Effects on Y axis" = "EffectY")), 
                                radioButtons("FigureRange", label = "Set the axis range, according to...", choices = list("All the parameters" = "Large", 
                                                                                                                          "The chosen parameter" = "Small")), 
                                sliderInput("slider2", label = "Threshold", min = 10000, max = 200000, value = 50000, step = 5000)
                                ), 
                         column(3, 
                                h4("The Observed Strategy"), 
                                sliderInput("startage", label = "Screen startage", min = 25, max = 75, value = 30, step = 1), 
                                sliderInput("stopage", label = "Screen stopage", min = 50, max = 100, value = 70, step = 1), 
                                sliderInput("interval", label = "Screen interval", min = 1, max = 10, value = 1, step = 1), 
                                br(), 
                                tableOutput("ExtraResults")
                                ), 
                         column(4, 
                                plotOutput("ScreenCountPlot"), 
                                br(), 
                                plotOutput("AgeDist")
                                )
                         )
                )

server <- function(input, output){
  output$distPlot1 <- renderPlot({if (input$AnalysisType == "DeterministicAnalysis" | input$slider1 == "2"){
                                    ICERTable <- read.table(file = "OutputFiles/LatestResults/MainCEresults/OutputTable_Input.txt", header = T)
                                  } else if (input$slider1 == "1"){
                                      ICERTable <- read.table(file = paste("OutputFiles/LatestResults/MainCEresults/OutputTable_", 
                                                                           input$parameters, "_Low.txt", sep = ""), header = T)
                                    } else {
                                    ICERTable <- read.table(file = paste("OutputFiles/LatestResults/MainCEresults/OutputTable_", 
                                                                         input$parameters, "_High.txt", sep = ""), header = T)
                                      }
                                  ObsearvationPoint <- paste(input$startage, "_", input$stopage, "_", input$interval, sep = "")
                                  
                                  # Define the costs and effects
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
                                  PlotData <- ICERTable[, c("StrategyName", EffectMeasure, CostMeasure, ICERMeasure)]
                                  colnames(PlotData) <- c("StrategyName", "Effects", "Costs", "ICER")
                                  PlotData[, c("Effects", "Costs")] <- PlotData[, c("Effects", "Costs")] / SampleSize
                                  Ref <- which(PlotData$ICER == "reference")
                                  PlotData[, "ICER"] <- as.numeric(as.character(PlotData$ICER))
                                  BoundarySet <- PlotData[!(is.na(PlotData$ICER)), ]
                                  
                                  SameParameterEffect <- NA
                                  SameParameterCost <- NA
                                  if ((input$AnalysisType == "DeterministicAnalysis")){
                                    MaxCost <- max(as.numeric(PlotData[, "Costs"]))
                                    MaxEffect <- max(as.numeric(PlotData[, "Effects"]))
                                    MinCost <- min(as.numeric(PlotData[, "Costs"]))
                                    MinEffect <- min(as.numeric(PlotData[, "Effects"]))
                                  } else if (input$AnalysisType == "Owsa"){
                                      if (input$FigureRange == "Small"){
                                        for (filenumber in c("Low", "High")){
                                          SameParameterTable <- read.table(file = paste("OutputFiles/LatestResults/MainCEresults/OutputTable_", 
                                                                                        input$parameters, "_", filenumber, ".txt", sep = ""), header = T)
                                          SameParameterEffect <- c(SameParameterEffect, SameParameterTable[, EffectMeasure] / SampleSize)
                                          SameParameterCost <- c(SameParameterCost, as.numeric(SameParameterTable[, CostMeasure]) / SampleSize)
                                        }
                                      } else {
                                          for (filenumber in c("Low", "High")){
                                            for (ListParameter in c("TestSensitivity1", 
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
                                                                    #"TestDisutility1", 
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
                                                                                            ListParameter, "_", filenumber, ".txt", sep = ""), header = T)
                                              SameParameterEffect <- c(SameParameterEffect, SameParameterTable[, EffectMeasure] / SampleSize)
                                              SameParameterCost <- c(SameParameterCost, as.numeric(SameParameterTable[, CostMeasure]) / SampleSize)
                                            }
                                          }
                                        }
                                      MaxEffect <- max(SameParameterEffect, na.rm = TRUE)
                                      MinEffect <- min(SameParameterEffect, na.rm = TRUE)
                                      MaxCost <- max(SameParameterCost, na.rm = TRUE)
                                      MinCost <- min(SameParameterCost, na.rm = TRUE)
                                    }
                                  # Identify the highest of the cost-effective ICERs given the threshold
                                  Threshold <- input$slider2
                                  if (min(BoundarySet$ICER, na.rm = TRUE) <= Threshold){
                                    OptimalStrategy <- rownames(BoundarySet)[which(BoundarySet$ICER == (max(BoundarySet$ICER[BoundarySet$ICER <= Threshold], na.rm = TRUE)))]
                                    # Find the corresponding costs and effect
                                    CostEffectiveICER <- BoundarySet[OptimalStrategy, "ICER"]
                                    CostEffectiveEffects <- as.numeric(BoundarySet[OptimalStrategy, "Effects"])
                                    CostEffectiveCosts <- as.numeric(BoundarySet[OptimalStrategy, "Costs"])
                                  } else {
                                      CostEffectiveICER <- "Ref"
                                      CostEffectiveEffects <- as.numeric(PlotData[Ref, "Effects"])
                                      CostEffectiveCosts <- as.numeric(PlotData[Ref, "Costs"])
                                    }
                                  
                                  Effects <- PlotData[, "Effects"]
                                  Costs <- PlotData[, "Costs"]
                                  
                                  # Set the tick marks to incude zero and any big number
                                  TicksEffects <- pretty(c(round(MinEffect - (MaxEffect - MinEffect) / 20, 5), round(MaxEffect + (MaxEffect - MinEffect) / 20, 5)))
                                  TicksCosts <- pretty(c(0, MaxCost + MaxCost / 20))
                                  
                                  # Set the ranges for both values
                                  xRange <- range(TicksEffects)
                                  yRange <- range(TicksCosts)
                                  
                                  # Create labels
                                  LabelTitle <- c("Low", "Medium", "High")
                                  LabelTitle1 <- c("Low", "Input", "High")
                                  if (CostEffectiveICER == "Ref"){ICERLabel <- "reference"} else {ICERLabel <- prettyNum(round(CostEffectiveICER), big.mark = ",")}
                                  if (input$AnalysisType == "DeterministicAnalysis"){
                                    TitleLabel <- "Default scenario"
                                    } else if (!(input$parameters %in% c("Incidence", "Survival"))){
                                        TitleLabel <- paste("Scenario", LabelTitle[input$slider1], ":", input$parameters, "=", 
                                                            InputTable[which(InputTable$Parameter == input$parameters), LabelTitle1[input$slider1]], sep = " ")
                                      } else {TitleLabel <- paste("Scenario",LabelTitle[input$slider1], ":", input$parameters)}
                                  
                                  # Plot the CE plane
                                  par(mar = c(5, 10, 5, 5))
                                  if (input$CEaxis == "EffectX"){
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
                                    text(CostEffectiveEffects * 1.000001, CostEffectiveCosts * 0.6, labels = ICERLabel, pos = 4)
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
                                      text(CostEffectiveCosts * 0.6, CostEffectiveEffects * 1.000001, labels = ICERLabel, pos = 2)
                                      # Add in the X and Y axis labels with spacing
                                      title(TitleLabel, ylab = paste("Effects,", input$EffectMeasurement), xlab = "Costs ($M)", mgp = c(3.75,1.75,0), cex.lab = 1.2)
                                    }
                                  })
  output$ExtraResults <- renderTable({ObsearvationPoint <- paste(input$startage, "_", input$stopage, "_", input$interval, sep = "")
                                      if (input$AnalysisType == "DeterministicAnalysis" | input$slider1 == "2"){
                                        CEBreakDown <- read.table(file = paste("OutputFiles/LatestResults/IntermediateOutcomtes/CEBreakDown/Input.txt", sep = "_"), header = T)
                                      } else if (input$slider1 == "1"){
                                          CEBreakDown <- read.table(file = paste("OutputFiles/LatestResults/IntermediateOutcomtes/CEBreakDown/", 
                                                                                 input$parameters, "_Low.txt", sep = ""), header = T)
                                        } else {
                                            CEBreakDown <- read.table(file = paste("OutputFiles/LatestResults/IntermediateOutcomtes/CEBreakDown/", 
                                                                                   input$parameters, "_High.txt", sep = ""), header = T)
                                          }
                                      if (sum(CEBreakDown$StrategyName == ObsearvationPoint) == 1){
                                        ResultTable <- cbind(c("Undiscounted screen costs", 
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
                                        colnames(ResultTable) <- c(paste("Aged ", input$startage, " to ",input$stopage, ", ", input$interval, " yearly",sep=""), " ")
                                      } else {
                                          ResultTable <- c("No result is available for the chosen strategy.")
                                        }
                                      ResultTable
                                      })
  output$ScreenCountPlot <- renderPlot({ObsearvationPoint <- paste(input$startage, "_", input$stopage, "_", input$interval, sep = "")
                                        Folder <- c("OutputFiles/LatestResults/IntermediateOutcomtes/ScreenCounts/")
                                        if ((input$AnalysisType == "DeterministicAnalysis" | input$slider1 == "2") & 
                                            (file.exists(paste(Folder, "Input_", ObsearvationPoint, ".txt", sep = "")))){
                                          ScreenCounts <- read.table(file = paste(Folder, "Input_", ObsearvationPoint, ".txt", sep = ""), header = T)
                                        } else if ((input$slider1 == "1") & (file.exists(paste(Folder, input$parameters, "_Low_", ObsearvationPoint, ".txt", sep = "")))){
                                            ScreenCounts <- read.table(file = paste(Folder, input$parameters, "_Low_", ObsearvationPoint, ".txt", sep = ""), header = T)
                                          } else if (file.exists(paste(Folder, input$parameters, "_High_", ObsearvationPoint, ".txt", sep = ""))){
                                              ScreenCounts <- read.table(file = paste(Folder, input$parameters, "_High_", ObsearvationPoint, ".txt", sep = ""), header = T)
                                            } else {
                                                ScreenCounts <- NA
                                              }
                                        
                                        if (!(is.na(ScreenCounts))){
                                          PlotData <- as.matrix(rbind(ScreenCounts$RealPostives, ScreenCounts$RealPostives - ScreenCounts$TruePositives))
                                          # Add extra space to right margin of plot within frame
                                          par(mar = c(5, 4, 4, 6) + 0.1)
                                          barplot(PlotData, main = "Screen Performance", axes = FALSE, xlab = "Screen age", beside = FALSE, 
                                                  col = c("lightblue", "darkgrey"), border = "white")
                                          axis(2, col = "black", las = 1)
                                          mtext("Number of cases", side = 2, line = 2.5)
                                          legend("topleft", legend = c("Detected Cases", "Undetected Cases"), 
                                                 col = c("lightblue", "darkgrey"), lty = 1, cex = 0.8)
                                          box()
                                          # Allow a second plot on the same graph
                                          par(new = TRUE)
                                          plot(ScreenCounts$ScreenAge, ScreenCounts$FalsePositives, pch = 15, 
                                               xlab = "", ylab = "", axes = FALSE, type = "l", col = "brown")
                                          mtext("Number of false positives", side = 4, col = "brown", line = 4)
                                          axis(4, col = "brown", col.axis = "brown", las = 1)
                                        }
                                        })
  output$AgeDist <- renderPlot({ObsearvationPoint <- paste(input$startage, "_", input$stopage, "_", input$interval, sep = "")
                                Folder <- c("OutputFiles/LatestResults/IntermediateOutcomtes/AgeDistribution/")
                                if ((input$AnalysisType == "DeterministicAnalysis" | input$slider1 == "2") & 
                                    (file.exists(paste(Folder, "Input_", ObsearvationPoint, ".txt", sep = "")))){
                                  AgeDist <- read.table(file = paste(Folder, "Input_", ObsearvationPoint, ".txt", sep = ""), header = T)
                                } else if ((input$slider1 == "1") & (file.exists(paste(Folder, input$parameters, "_Low_", ObsearvationPoint, ".txt", sep = "")))){
                                    AgeDist <- read.table(file = paste(Folder, input$parameters, "_Low_", ObsearvationPoint, ".txt", sep = ""), header = T)
                                  } else if (file.exists(paste(Folder, input$parameters, "_High_", ObsearvationPoint, ".txt", sep = ""))){
                                      AgeDist <- read.table(file = paste(Folder, input$parameters, "_High_", ObsearvationPoint, ".txt", sep = ""), header = T)
                                    } else {
                                        AgeDist <- NA
                                      }
                                
                                if (!(is.na(AgeDist))){
                                  AgeDist$group <- rep(seq(5, 100, by = 5), each = 5)
                                  AgeDist <- aggregate(AgeDist, by = list(Category = AgeDist$group), FUN = sum)
                                  
                                  par(mar = c(5, 4, 4, 6) + 0.1)
                                  plot(AgeDist$Category, AgeDist$OtherCauseDeath / 1000, axes = FALSE, 
                                       main = "Disease History", xlab = "Screen Age", ylab = "", type = "l", col = c("#F5C710"), lwd = 2)
                                  box()
                                  axis(1, col = "black", las = 1)
                                  lines(AgeDist$Category, AgeDist$AllCauseDeath / 1000, col = "gray62", lwd = 2)
                                  axis(4, col = "gray62", las = 1)
                                  mtext("Number of other-cuase and all-cause deaths (thousands)", side = 4, line = 2.5, col = "gray62", col.axis = "gray62")
                                  legend("topleft", legend = c("Preclinical", "Clinical", "Cause-Specific Death", "Other-Cause Death", "All-Cause Death"), 
                                         col = c("#DF536B", "#61D04F", "#2297E6", "#F5C710", "gray62"), lty = c(1, 1, 1, 1, 1), cex = 0.8)
                                  # Allow a second plot on the same graph
                                  par(new = TRUE)
                                  plot(AgeDist$Category, AgeDist$Preclinical, pch = 16, axes = FALSE, xlab = "", ylab = "", type = "l", col = "#DF536B", lwd = 2)
                                  lines(AgeDist$Category, AgeDist$Clinical, col = "#61D04F", lwd = 2)
                                  lines(AgeDist$Category, AgeDist$CauseSpecificDeath, col = "#2297E6", lwd = 2)
                                  axis(2, col = "black", las = 1)
                                  mtext("Number of preclinical/ clinical/ cause-specific deaths", side = 2, line = 2.5)
                                }
                                })
}
shinyApp(ui = ui, server = server)