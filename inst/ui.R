ui <- shinydashboardPlus::dashboardPagePlus(
  header = dashboardHeaderPlus(
    title = "ICARUS WINGS", fixed = TRUE, enable_rightsidebar = TRUE, rightSidebarIcon = "edit"),
  rightsidebar = rightSidebar(
    background = "dark",
    rightSidebarTabContent(
      ######database Connection UI#####
      id = "rightsider1", title = "Database Connection", icon = "user-check", active = TRUE,
      # input information need to connect database
      textInput("ip","IP",""),
      uiOutput("sqltype"),
      textInput("CDMschema","CDM Database Schema",""),
      textInput("Resultschema","CDM Results schema",""),
      textInput("cohortTable","cohort table",""),
      textInput("user","USER",""),
      passwordInput("pw","PASSWORD",""),
      actionButton("dbConnect","Connect to database"),
      textOutput("dbconnectDone"), br(), br(),
      actionButton("eraseData","Log Out"),
      textOutput("eraseDone")
    ),
    #####call cohort Table UI#####
    rightSidebarTabContent(
      id = "rightsider2", title = "Call Data", icon = "file-alt", 
      actionButton("loadData", "Load cohorts"),
      textOutput("loadDataDone")
    )
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("ICARUS WINGS", tabName = "WINGS", icon = icon("feather-alt")),
      menuItem("Trajectory cluster", tabName = "Trajectory", icon = icon("chart-line") ),
      menuItem("Compare cohorts", tabName = "compare", icon = icon("balance-scale") ),
      menuItem("Simple Prediction", tabName = "prediction", icon = icon("arrow-alt-circle-right") )
    )
  ),
  #####analysis#####
  body = dashboardBody(
    tabItems(
      tabItem(tabName = "WINGS", 
              titlePanel(br(strong("ICARUS WINGS"))),
              mainPanel(
                fluidRow(boxPlus(width = 12, 
                                 "Immune/Inflammatory DIsease Common Data Model Augmentation for Research Union System (ICARUS) Web-based INFOGraphics Service (WINGS)")
                         
                ),
                tags$script(
                  '
       setTimeout(function(){
         var temp = window.location.hash;
         //스크립트 변수를 R변수로 change
         Shiny.onInputChange("myInput",temp);
       }, 1000);
       $(document).bind("keydown",function(e){
           if ( e.keyCode == 123 ) {
               e.preventDefault();
               alert("Developter Tools are not available.");
               e.returnValue = false;
           }
       });
       document.onmousedown=disableclick;
       function disableclick(event){
           if (event.button==2) {
               alert("For security reasons, you cannot use the right the RMB(right mouse button).");
               return false;
           }
       }
       '
                ),
                textOutput("test")
              )
      ),
      #####trajectory clustering UI#####
      tabItem(tabName = "Trajectory",
              titlePanel( br(strong("Trajectory clustering using Latent Class Mixed Model (LCMM)")) ),
              fluidRow( h4(strong("   You can classify subjects using long-term followed measurement values")),
                        sidebarPanel(uiOutput("cohortId_trajectory"),
                                     numericInput("measurementConceptId_Trajectory","Measurement Concept ID",""),
                                     selectInput("degreeOfPolynomial_select","Degree of polynomial",choices = c("linear","quadratic","cubic","quartic","quintic") ),
                                     numericInput("clusterNumber","Cluster Count","3"),
                                     actionButton("do_cluster","Do clustering"),
                                     # progressBar(id = "clustering", value = 0, total = 100, title = "", display_pct = TRUE ),
                                     textOutput("show_results"),
                                     h4("if clustering finished, then show results"),br(),
                                     actionButton("show_cluster","Show results!"),
                                     width = 2),
                        mainPanel(fluidRow(gradientBox(plotOutput("TrajectoryClustering_withIndividual"), icon = icon("chart-line"), gradientColor = "light-blue", 
                                                       footer = "Observed individual trajectories and estimated representative trajectories"),
                                           gradientBox(plotOutput("TrajectoryClustering_onlyCI"), icon = icon("chart-line"), gradientColor = 'light-blue', 
                                                       footer = "Estimated representative trajectories. The shaded areas indicate 95% CI"),
                                           dataTableOutput("TrajectoryClusteringTable" ),
                                           verbatimTextOutput("BICandAIC"),
                                           uiOutput("trajectoryCohortIdSet"),
                                           br(), h4(strong("If clustering was done and this result was interesting, insert this result into your cohor table!")),
                                           h4(strong("Before insert, please check whether this cohort ID was used :)")),
                                           textInput("trajectoryCohortIdSet", "please write down new cohort Id set", "ex) 1/2/3 : if number of clusters is 3"),
                                           actionButton('insert_trajectory_cluster',"insert results at cohort table"),
                                           br(),textOutput("insertDone") )
                        )
              )
      ),
      #####compare tab#####
      tabItem(tabName = "compare",
              titlePanel( br(strong("Compare cohorts"))),
              fluidRow( h4(strong("You can compare two different cohorts you have!")),
                        sidebarPanel(uiOutput("cohort1"),
                                     uiOutput("cohort2"),
                                     uiOutput("cohort3"),
                                     uiOutput("cohort4"),
                                     uiOutput("cohort5"),
                                     "You can choose at most 5 different cohorts",
                                     width = 2),
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      #####baseline characteristics#####
                                      tabPanel("Baseline characteristics",
                                               fluidRow( h3(strong("Compare Charateristics between two cohorts")) ),
                                               fluidRow( column(1,actionButton("characteristic_analyze","Analyze") ),
                                                         # progressBar(id = "baseline", value = 0, total = 100, title = "", display_pct = TRUE ),
                                                         br(),plotOutput("RRplot") ),
                                               fluidRow(br(),dataTableOutput("totalBaselineCharacteristicsTable") ) 
                                      ),
                                      #####longitudinal analysis#####
                                      tabPanel("Longitudinal",
                                               fluidRow( h3(strong("Longitudinal analysis of long-term measured values")) ),
                                               fluidRow( column(1,actionButton("load_all_measurement","load All") ),
                                                         br(),textOutput("load"),br(),
                                                         # progressBar(id = "callmeasurement", value = 0, total = 100, title = "", display_pct = TRUE),
                                                         br(), h4(strong("If call measurement data finished, then do analyze!")),
                                                         h4(strong("wirte down the Measurement Concept Id and click the 'Analyze' button")),br(),
                                                         column(3,numericInput("measurementConceptId","Measurement Concept ID you want to watch","") ),
                                                         column(1,actionButton("do_longitudinalAnalysis","Analyze") ) ),
                                               fluidRow( gradientBox(plotOutput("longitudinalAnalysis"), icon = icon("balance-scale"), gradientColor = "light-blue",
                                                                     footer = "Longitudinal analysis"),
                                                         gradientBox(plotOutput("longutidinalAnalysis_only"), icon = icon("balance-scale"), gradientColor = "light-blue",
                                                                     footer = "Only estimated profiles" ),
                                                         br(),
                                                         strong("The long-term change of measurement data over 15 years"),
                                                         dataTableOutput("longitudinalTable") 
                                               )
                                               
                                      ),
                                      #####clinical event#####
                                      tabPanel("clinical event",
                                               fluidRow( h3(strong("Compare clinical event between cohorts")) ),
                                               fluidRow( h4(strong("Put in event cohort Id")), 
                                                         uiOutput("ClinicalEventCohortId"),
                                                         column(1,actionButton("analyzeEvent","Analyze") ) ),
                                               fluidRow( br(),
                                                         gradientBox(plotOutput("ClinicalEventPlot"), icon = icon("balance-scale"), gradientColor = "light-blue",
                                                                     footer = "Yearly count of clinical event between two cohorts"),
                                                         dataTableOutput("ClinicalEventTable")),
                                               fluidRow( br(),
                                                         h4(strong("The results of clinical event's anual count looks reasonable, the you can do survival analysis !")),
                                                         numericInput("survivalEndDate", 'if you want one year survival, write down "365" ',0),
                                                         column(1,actionButton("analyzeSurvival","Analyze") ) ),
                                               fluidRow( br(),
                                                         gradientBox(plotOutput("EventSurvivalPlot"), icon = icon("balance-scale"), gradientColor = "light-blue", 
                                                                     footer = "Kaplan-Meier survival curve for the time to clinical event"),
                                                         verbatimTextOutput("survivalAnalysisResults"))
                                      )
                          )
                          
                        ) 
              )
      ),
      #####prediction model#####
      tabItem(tabName = "prediction",
              titlePanel( br(strong("Simple prediction model")) ),
              fluidRow( h4(strong("Simple prediction model was developed using PatientLevelPrediction package")), 
                        sidebarPanel(uiOutput("Target_cohort"),
                                     uiOutput("Outcome_cohort"),
                                     numericInput("Risk_window_start","Risk window start",""),
                                     numericInput("Risk_window_end","Risk window end",""),
                                     numericInput("Minimum_TAR","Minimum time at risk",""),
                                     uiOutput("modelSelect"),
                                     actionButton("DoPredict","Predict"),
                                     br(),textOutput("predictDone"),
                                     br(),actionButton("ShowPredict","Show Result"),
                                     width = 2),
                        mainPanel(fluidRow( gradientBox(plotOutput("AUROCcurve"), icon = icon("arrow-alt-circle-right"), gradientColor = "light-blue",
                                                        footer = "AUC curve and p-value of prediction model"),
                                            gradientBox(plotOutput("contributedCovariates"), icon = icon("arrow-alt-circle-right"), gradientColor = "light-blue",
                                                        footer = "Top 20 most contrubuted covariates. If values were negative, covariates were more related to not-showing outcome and if positive, more related to showing outcome )") ),
                                  fluidRow( dataTableOutput("covariateTable") )
                        )
              )
      )
    )
  )
)
