# #check package ready
source(file.path(.libPaths()[1],"ICARUSviewer/global.R"))
library(ICARUSviewer)

check.packages("Rcpp")
check.packages("dplyr")
check.packages("reshape2")
check.packages("ggplot2")
check.packages("plotly")
check.packages("shiny")
check.packages("SqlRender")
check.packages("DatabaseConnector")
check.packages("FeatureExtraction")
check.packages("PatientLevelPrediction")
check.packages("shinydashboard")
check.packages("shinyWidgets")
check.packages("shinydashboardPlus")
check.packages("tidyverse")
check.packages("epitools")
check.packages("mgcv")
check.packages("lme4")
check.packages("lmerTest")
check.packages("lcmm")
check.packages("ggfortify")
check.packages("survival")
check.packages("ICARUSviewer")

server <- function(input, output, session) {
  #####1.right side menu : DB connection#####
  output$sqltype<-renderUI({
    selectInput("sqltype", "Select DBMS",
                choices = c("sql server" = "sql server",
                            "PostgreSQL" = "postresql",
                            "Amazon Redshift" = "redshift",
                            "Microsoft Parallel Data Warehouse" = "pdw",
                            "IBM Netezza" = "netezza",
                            "Google BigQuery" = "bigquery") )
  })
  DBconnect <- eventReactive(input$dbConnect, {
    connectionDetails <<- DatabaseConnector::createConnectionDetails(dbms = input$sqltype,
                                                                     server = input$ip,
                                                                     user = input$user,
                                                                     password = input$pw)
    CDMschema <<- input$CDMschema
    CohortSchema <<- input$Resultschema
    cohortTable <<- input$cohortTable
    connection <<-DatabaseConnector::connect(connectionDetails)
    
    "Database connection is done!"
  })
  output$dbconnectDone <- renderText({ DBconnect() })
  #####2. right side tab menu : call cohort table#####
  callCohortTable <- eventReactive(input$loadData,{
    calledData <- call_dataList(connectionDetails = connectionDetails,
                                connection = connection,
                                Resultschema = CohortSchema,
                                CDMschema = CDMschema,
                                cohortTable = cohortTable)
    totalCohort <<- calledData[[2]]
    demographicData <<- calledData[[1]]
    setting()
    "Call Cohort Table finished!"
  })
  output$loadDataDone <- renderText({ callCohortTable() })
  #####Menu Item 1 : Trajectory clustering#####
  output$cohortId_trajectory<-renderUI({
    selectInput("target_cluster_cohort","Target cohort ID",choices = sort(unique(totalCohort$cohortDefinitionId)) )
  })
  
  #####Java script#####
  output$test <- renderPrint( input$myInput )
  
  trajectoryClustering <- eventReactive(input$do_cluster,{
    #load temporal measurement data
    allclustering_target <- getAllLongitudinal(connectionDetails = connectionDetails,
                                               CDMschema = CDMschema,
                                               Resultschema = CohortSchema,
                                               cohortTable = cohortTable,
                                               cohortId = as.numeric(input$target_cluster_cohort) )
    #do LCMM
    lcmm_cluster_result_list <<- latent_class_classification(all_longitudinal_data_for_cluster = allclustering_target,
                                                             measurementConceptId_Trajectory = input$measurementConceptId_Trajectory,
                                                             degreeOfPolynomial = input$degreeOfPolynomial_select,
                                                             cluster_number = input$clusterNumber)
    "Trajectory clustering is done!"
  })
  output$show_results <- renderText({ withProgress(message = "trajectory clustering...",value = 1, {trajectoryClustering()} ) })
  # # progress bar for clustering
  # observeEvent(input$do_cluster,{
  #   for(i in 1:100){
  #     updateProgressBar( session = session, id = "clustering", value = i, total = 100, title = paste("clustering", trunc(i/10)) )
  #     Sys.sleep(0.1)
  #   }
  # })
  ##plot lcmm results
  plot_lcmm_cluster_withIndividual <- eventReactive(input$show_cluster,{
    plot_cluster <- latent_longitudinal_plot(lcmm_classification_result_list = lcmm_cluster_result_list,
                                             individual_trajectories = TRUE,
                                             cluster_number = input$clusterNumber)
    return(plot_cluster)
  })
  output$TrajectoryClustering_withIndividual<- renderPlot({ plot_lcmm_cluster_withIndividual() })
  
  plot_lcmm_cluster_onlyCI <- eventReactive(input$show_cluster,{
    plot_cluster <- latent_longitudinal_plot(lcmm_classification_result_list = lcmm_cluster_result_list,
                                             individual_trajectories = FALSE,
                                             cluster_number = input$clusterNumber)
    return(plot_cluster)
  })
  output$TrajectoryClustering_onlyCI<- renderPlot({ plot_lcmm_cluster_onlyCI() })
  
  table_lcmm_cluster <- eventReactive(input$show_cluster,{
    table_cluster <- latent_longitudinal_table(lcmm_classification_result_list = lcmm_cluster_result_list,
                                               cluster_number = input$clusterNumber)
    return(table_cluster)
  })
  output$TrajectoryClusteringTable <- renderDataTable({ table_lcmm_cluster() })
  #BIC and AIC score
  BICAIC <- eventReactive(input$show_cluster,{
    paste("BIC score is",lcmm_cluster_result_list$fit_BIC, "and", "AIC score is",lcmm_cluster_result_list$fit_AIC,sep = " ")
  })
  output$BICandAIC <- renderText({ BICAIC() }) 
  #insert at cohort table
  insertCohortNew <- eventReactive(input$insert_trajectory_cluster,{
    insertCohort(newCohortIdSet = as.character(input$trajectoryCohortIdSet),
                 target_cluster_cohort = as.numeric(input$target_cluster_cohort),
                 resultOflcmm = lcmm_cluster_result_list,
                 connection = connection,
                 Resultschema = CohortSchema,
                 cohortTable = cohortTable)
    "Insert done!"
  })
  output$insertDone <- renderText({ withProgress(message = "Insert into CohortTable...",value = 1, { insertCohortNew() }) })
  #####Menu Item 2 : compare cohorts #####
  output$cohort1 <- renderUI({
    selectInput("cohort1","cohort1",choices = c("select",sort(unique(totalCohort$cohortDefinitionId))), selected = "select" )
  })
  output$cohort2 <- renderUI({
    selectInput("cohort2","cohort2",choices = c("select",sort(unique(totalCohort$cohortDefinitionId))), selected = "select" )
  })
  output$cohort3 <- renderUI({
    selectInput("cohort3","cohort3",choices = c("select",sort(unique(totalCohort$cohortDefinitionId))), selected = "select" )
  })
  output$cohort4 <- renderUI({
    selectInput("cohort4","cohort4",choices = c("select",sort(unique(totalCohort$cohortDefinitionId))), selected = "select" )
  })
  output$cohort5 <- renderUI({
    selectInput("cohort5","cohort5",choices = c("select",sort(unique(totalCohort$cohortDefinitionId))), selected = "select" )
  })
  output$ClinicalEventCohortId <- renderUI({
    selectInput("ClinicalEventCohortId","Clinical Event Cohort ID",choices = sort(unique(totalCohort$cohortDefinitionId)), selected = 0)
  })
  #####Tab set : baseline characteristics#####
  demographic_result <- eventReactive(input$characteristic_analyze,{
    # cohort_id_set setting
    cohortDefinitionIdsetFive <- c(input$cohort1,input$cohort2,input$cohort3,input$cohort4,input$cohort5)
    cohortDefinitionIdset_selected <- cohortDefinitionIdsetFive[cohortDefinitionIdsetFive != 'select'] 
    cohortDefinitionIdSet <- as.numeric(cohortDefinitionIdset_selected)
    # demographics 
    demographicRaw <- charterstic_manufacture(cohort_definition_id_set = cohortDefinitionIdSet )
    demographicSummary <- characteristic_summary(characteristic_manufac = demographicRaw)
    demographicpvalue <- characteristic_pvalue(characteristic_manufac = demographicRaw)
    demographicSum <- merge(demographicSummary,demographicpvalue,by = "demographicName")
    colnames(demographicSum)[1] <- "baseline_Measure"
    # comorbidities
    comorbidityRaw <- baseline_comorbidity(connectionDetails = connectionDetails,
                                           Resultschema = CohortSchema,
                                           CDMschema = CDMschema,
                                           cohortTable = cohortTable,
                                           cohort_definition_id_set = cohortDefinitionIdSet )
    comorb_prev <- co_prevtable(comorbManufacData = comorbidityRaw)
    RR_pvalue  <- calculateRR(comorbManufacData = comorbidityRaw)
    comorbSum <-  merge(comorb_prev,RR_pvalue%>%select(diseaseName,pvalue),by = "diseaseName")
    colnames(comorbSum)[1] <- "baseline_Measure"
    # baseline measured values
    baselinemeasurement <<- baselineMeasure_compare(connectionDetails = connectionDetails,
                                                    Resultschema = CohortSchema,
                                                    CDMschema = CDMschema,
                                                    cohortTable = cohortTable,
                                                    cohort_definition_id_set = cohortDefinitionIdSet,
                                                    measurementConceptIdSet = c(2,3,5,6,7,8,3028930,4169578,44786758,4010492,3046594,2212469,
                                                                                3011708,3006504,3005322,3005600,3017501,3026514,3005791,3021940,
                                                                                3011505,3013115,3018010,3022096) )
    measureSum <- baselinemeasurement
    colnames(measureSum)[1] <- "baseline_Measure"
    characteristicsAllSum <- rbind(demographicSum,comorbSum,measureSum)
    RRplot     <- RRplot(RRResult = RR_pvalue)
    
    resultList <- list(allCharacteristicsTable = characteristicsAllSum,
                       plotForComorbidity = RRplot)
  })
  output$RRplot <- renderPlot({  demographic_result()[[2]] })
  output$totalBaselineCharacteristicsTable <- renderDataTable({ withProgress(message = "Baseline characteristics were analyzed...",value = 1, { demographic_result()[[1]] }) })
  
  #####longitudinal Analysis#####
  load_longitudinal <- eventReactive(input$load_all_measurement,{
    # cohort_id_set setting
    cohortDefinitionIdsetFive <- c(input$cohort1,input$cohort2,input$cohort3,input$cohort4,input$cohort5)
    cohortDefinitionIdset_selected <- cohortDefinitionIdsetFive[cohortDefinitionIdsetFive != 'select'] 
    cohortDefinitionIdSet <- as.numeric(cohortDefinitionIdset_selected)
    # load all long-term measured data
    allLongitudinalList <- list()
    for(i in 1:length(cohortDefinitionIdSet)){
      allLongitudinal <- getAllLongitudinal(connectionDetails = connectionDetails,
                                            CDMschema = CDMschema,
                                            Resultschema = CohortSchema,
                                            cohortTable = cohortTable,
                                            cohortId = cohortDefinitionIdSet[i] )
      allLongitudinalList[[ i ]] <- list(cohortId = cohortDefinitionIdSet[i],
                                         allLongitudinal = allLongitudinal )
    }
    allLongitudinalList <<- allLongitudinalList
    "Load all longitudinal data is done!"
  })
  output$load <- renderText({ withProgress(message = "Measurement data were loaded...",value = 1, { load_longitudinal() }) })
  # do lme (lonaigitudinal analysis)
  longitudinalLME <- eventReactive(input$do_longitudinalAnalysis,{
    lmeResultList <- lapply(allLongitudinalList, FUN = function(x){
      subLongitudinalData <- getLongitudinal(all_longitudinal_data = x$allLongitudinal,
                                             measurement_concept_id = input$measurementConceptId,
                                             time_unit = 'year')
      lmeDone <- lme_logitudinal(longitudinalData = subLongitudinalData)
      longitudinalResult <- list(cohortId = x[[1]],
                                 subLongitudinalData = subLongitudinalData,
                                 lmeResult1 = lmeDone[[2]],
                                 lmeResult2 = lmeDone[[1]])
      return(longitudinalResult)
    })
    plot_lme_allMeasurement <- plotLmm(longitudinal_result = lmeResultList,
                                       pftIndividual = TRUE)
    plot_lme_onlyEstimated <- plotLmm(longitudinal_result = lmeResultList,
                                      pftIndividual = FALSE)
    table_lme_Estimated <- tableLmm(longitudinal_result = lmeResultList) 
    
    lme_result <- list(plot_lme_allMeasurement,plot_lme_onlyEstimated,table_lme_Estimated)
  })
  
  output$longitudinalAnalysis <- renderPlot({ withProgress(message = "Longitudinal Analysis...",value = 1, { longitudinalLME()[[1]] }) })
  output$longutidinalAnalysis_only <- renderPlot({ longitudinalLME()[[2]] })
  output$longitudinalTable <- renderDataTable({ longitudinalLME()[[3]] })
  
  #####Tab set : clinical event#####
  clinicalEvent_frequency <- eventReactive(input$analyzeEvent,{
    # cohort_id_set setting
    cohortDefinitionIdsetFive <- c(input$cohort1,input$cohort2,input$cohort3,input$cohort4,input$cohort5)
    cohortDefinitionIdset_selected <- cohortDefinitionIdsetFive[cohortDefinitionIdsetFive != 'select'] 
    cohortDefinitionIdSet <- as.numeric(cohortDefinitionIdset_selected)
    
    # filter cohort I need
    call_event_data <- call_event(cohort_definition_id_set = cohortDefinitionIdSet,
                                  cohortId_event = as.numeric(input$ClinicalEventCohortId))
    
    # calculate yearly count and its figure
    event_table <- event_incidence(callEvent_result = call_event_data)
    event_plot  <- plot_event_rate(event_result = event_table)
    
    event_out <- list(eventTable = event_table,
                      eventPlot = event_plot)
    return(event_out)
  })
  
  output$ClinicalEventPlot <- renderPlot({ clinicalEvent_frequency()[[2]] })
  output$ClinicalEventTable <- renderDataTable({ clinicalEvent_frequency()[[1]] })
  #survival
  eventFreesurvival <- eventReactive(input$analyzeSurvival,{
    # cohort_id_set setting
    cohortDefinitionIdsetFive <- c(input$cohort1,input$cohort2,input$cohort3,input$cohort4,input$cohort5)
    cohortDefinitionIdset_selected <- cohortDefinitionIdsetFive[cohortDefinitionIdsetFive != 'select'] 
    cohortDefinitionIdSet <- as.numeric(cohortDefinitionIdset_selected)
    
    eventFreeSurvivalResults <- eventFreeSurvival(cohort_definition_id_set = cohortDefinitionIdSet,
                                                  cohortId_event = as.numeric(input$ClinicalEventCohortId),
                                                  targetSurvivalEndDate = as.numeric(input$survivalEndDate))
  })
  output$EventSurvivalPlot <- renderPlot({ eventFreesurvival()[[2]] })
  output$survivalAnalysisResults <- renderPrint({ eventFreesurvival()[[1]] })
  
  #####Menu Item 3 : prediction#####
  output$Target_cohort  <- renderUI({ selectInput("Target_cohort","Target Cohort ID",choices = sort(unique(totalCohort$cohortDefinitionId)) )   })
  output$Outcome_cohort <- renderUI({ selectInput("Outcome_cohort","Outcome Cohort ID",choices = sort(unique(totalCohort$cohortDefinitionId)) ) })
  output$modelSelect    <- renderUI({ selectInput("modelSelect","Machine Learning Model Select",choices = c("Lasso Logistic","Gradient Boosting")) })
  switchModelSelect     <- reactive({ switchselect_model(input$modelSelect) })
  #predict
  prediction <- eventReactive(input$DoPredict,{
    plpdata <- getPlpDataList(connectionDetails = connectionDetails,
                              CDMschema = CDMschema,
                              Resultschema = CohortSchema,
                              cohortTable = cohortTable,
                              targetCohortConceptId = as.numeric(input$Target_cohort),
                              outcomeCohortConceptId = as.numeric(input$Outcome_cohort),
                              covariateSetting = covariateSetting,
                              washoutPeriod = 0,
                              removeSubjectsWithPriorOutcome = FALSE,
                              riskWindowStart = as.numeric(input$Risk_window_start),
                              riskWindowEnd = as.numeric(input$Risk_window_end),
                              minTimeAtRisk = as.numeric(input$Minimum_TAR) )
    plp_result <<- RunPlp(getplpOut = plpdata,
                          learningModel = switchModelSelect(),
                          splitSeed = NULL,
                          outputFolder = outputFolder)
    "Prediction is done!"
  })
  output$predictDone <- renderText({ withProgress(message = "Prediction is running...",value = 1, { prediction() }) })
  
  # show prediction results
  AUROCplot <- eventReactive(input$ShowPredict,{
    auroc <- AUROCcurve(machineLearningData = plp_result)
    return(auroc)
  })
  output$AUROCcurve <- renderPlot({ AUROCplot() })
  
  contributedCovariatePlot <- eventReactive(input$ShowPredict,{
    predictiveVariable <- plotPredictiveVariables(machineLearningData = plp_result,
                                                  rankCount = 20)
    return(predictiveVariable)
  })
  output$contributedCovariates <- renderPlot({ contributedCovariatePlot() })
  
  contributedCovariateTable <- eventReactive(input$ShowPredict,{
    covariateTable <- tablePredictiveVariables(machineLearningData = plp_result,
                                               rankCount = 40)
    return(covariateTable)
  })
  output$covariateTable <- renderDataTable({ contributedCovariateTable() })
  
  eraseAllData <- eventReactive(input$eraseData,{
    removeTempAndOutput()
    "your traces were erased!"
  })
  output$eraseDone <- renderText({ eraseAllData() })
}
