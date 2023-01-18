list.of.packages <- c("RBesT", "shinyjs", "memoise", "data.table",
                      "DT", "shiny", "ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(RBesT); library(shinyjs); library(memoise)
library(data.table); library(DT); library(shiny)
library(ggplot2)
## some gMAP calls below require this option
options(RBesT.MC.control=list(adapt_delta=0.999999))

ui = navbarPage("", 
                tabPanel("Compute MAP prior",
                         sidebarLayout(
                           sidebarPanel(selectInput("endpt", "(1) Choose endpoint type.", choices = c("Binary", "Normal", "Poisson")),
                                        conditionalPanel("input.endpt == 'Normal'", numericInput("sigma", "Reference Scale (i.e., the standard deviation of the individual-level data)",
                                                                                                 value = 10, min = .Machine$double.eps)),
                                        checkboxInput("use_hist" ,"Use historical trial meta-data", value = TRUE),
                                        conditionalPanel("input.use_hist", hr(),
                                                         tags$b(HTML("(2) Enter in historical meta-data.")),
                                                         selectInput("hist_data_option", "Enter in data manually or upload .csv", c("Manual", "Upload")),
                                                         conditionalPanel("input.hist_data_option == 'Manual'",
                                                                          DT::dataTableOutput("table", width = 3),
                                                                          tags$hr(),
                                                                          checkboxInput("add_study", "Add study to table?"),
                                                                          conditionalPanel("input.add_study",
                                                                                           textInput("study_add", "Name of added study", value = "New Study"),
                                                                                           numericInput("n_add", "Sample size of added study", value = 0, step = 1),
                                                                                           conditionalPanel("input.endpt == 'Binary'",
                                                                                                            numericInput("r_add", "Number of events in added study", value = 0, step = 1)),
                                                                                           conditionalPanel("input.endpt == 'Normal'", 
                                                                                                            numericInput("m_add", "Sample mean in added study", value = 0)),
                                                                                           conditionalPanel("input.endpt == 'Poisson'",
                                                                                                            numericInput("m_add_pois", "Total count in added study", value = 1000),
                                                                                                            numericInput("t_add", "Trial duration of added study", value = 10)),
                                                                                           actionButton("add_btn", "Add study")),
                                                                          checkboxInput("delete_study", "Delete study from table?"),
                                                                          conditionalPanel("input.delete_study",
                                                                                           numericInput("delete_row", "Number of row to delete", value = 1, step = 1),
                                                                                           actionButton("delete_btn", "Delete study")) 
                                                         ), # close conditionalPanel, Manual
                                                         conditionalPanel("input.hist_data_option == 'Upload'",
                                                                          conditionalPanel("input.endpt == 'Binary'",
                                                                                           fileInput("file1.bin", "Please upload a .csv file containing your trial meta-data. The first column should
                                                                                                     contain a unique name for the study. The second column should contain the size of the 
                                                                                                     control arm. The third column should contain the number of events in the control arm.",
                                                                                                     accept = c(
                                                                                                       "text/csv",
                                                                                                       "text/comma-separated-values,text/plain",
                                                                                                       ".csv"))),
                                                                          conditionalPanel("input.endpt == 'Normal'",
                                                                                           fileInput("file1.norm", "Please upload a .csv file containing your trial meta-data. The first column should
                                                                                                     contain a unique name for the study. The second column should contain the size of the 
                                                                                                     control arm. The third column should contain a normally-distributed outcome.",
                                                                                                     accept = c(
                                                                                                       "text/csv",
                                                                                                       "text/comma-separated-values,text/plain",
                                                                                                       ".csv"))),
                                                                          conditionalPanel("input.endpt == 'Poisson'",
                                                                                           fileInput("file1.pois", "Please upload a .csv file containing your trial meta-data. The first column should
                                                                                                     contain a unique name for the study. The second column should contain the size of the 
                                                                                                     control arm. The third column should contain the sample mean. The fourth column is optional, and should 
                                                                                                     contain the length of time the trial lasted.",
                                                                                                     accept = c(
                                                                                                       "text/csv",
                                                                                                       "text/comma-separated-values,text/plain",
                                                                                                       ".csv"))
                                                                          ),
                                                                          checkboxInput("header", "First row of table contains headers", TRUE)
                                                         ), # close conditionalPanel, Upload
                                                         
                                                         conditionalPanel("input.use_hist", hr(), tags$b(HTML("<p> (3) Specify priors on &tau; and &beta;. </p>"))),
                                                         numericInput("seed", "Random seed (positive integer)", value=sample(1:10000, 1), min = 1),
                                                         conditionalPanel("input.use_hist",
                                                                          selectInput("tau_dist", HTML("<p> Specify prior for &tau; (between-trial standard deviation). </p>"), 
                                                                                      choices = c(`HalfNormal` = "HalfNormal", `TruncNormal` = "TruncNormal", 
                                                                                                  `Uniform` = "Uniform", `Gamma` = "Gamma", `InvGamma` = "InvGamma", 
                                                                                                  `LogNormal` = "LogNormal", `TruncCauchy` = "TruncCauchy", 
                                                                                                  `Exponential` = "Exp", `Fixed`  ="Fixed")),
                                                                          conditionalPanel("input.tau_dist == 'HalfNormal'", HTML("<p> &tau; ~ HalfNormal(&sigma;) </p>"), 
                                                                                           numericInput("tau_arg1_HN", HTML("<p> &sigma; </p>"), value = 1, min = .Machine$double.xmin)),
                                                                          conditionalPanel("input.tau_dist == 'TruncNormal'", HTML("<p> &tau; ~ TruncNormal(&mu;, &sigma;) </p>"), 
                                                                                           numericInput("tau_arg1_TN", HTML("<p> &mu; </p>"), value = 0, min = .Machine$double.xmin), 
                                                                                           numericInput("tau_arg2_TN", HTML("<p> &sigma; </p>"), value = 1, min = .Machine$double.xmin)),
                                                                          conditionalPanel("input.tau_dist == 'Uniform'", HTML("<p> &tau; ~ Uniform(a, b) </p>"), 
                                                                                           numericInput("tau_arg1_Uni", HTML("a"), value = 0), 
                                                                                           numericInput("tau_arg2_Uni", HTML("b"), value = 1)),
                                                                          conditionalPanel("input.tau_dist == 'Gamma'", HTML("<p> &tau; ~ Gamma(shape = &alpha;, rate = &beta;) </p>"), 
                                                                                           numericInput("tau_arg1_G", HTML("<p> &alpha; </p>"), value = 0.1), 
                                                                                           numericInput("tau_arg2_G", HTML("<p> &beta; </p>"), value = 0.1)),
                                                                          conditionalPanel("input.tau_dist == 'InvGamma'", HTML("<p> &tau; ~ InverseGamma(shape = &alpha;, scale = &beta;) </p>"), 
                                                                                           numericInput("tau_arg1_IG", HTML("<p> &alpha; </p>"), value = 0.001), 
                                                                                           numericInput("tau_arg2_IG", HTML("<p> &beta; </p>"), value = 0.001)),
                                                                          conditionalPanel("input.tau_dist == 'LogNormal'", HTML("<p> &tau; ~ LogNormal(&mu;, &sigma;) </p>"), 
                                                                                           numericInput("tau_arg1_LN", HTML("<p> &mu; </p>"), value = 0), 
                                                                                           numericInput("tau_arg2_LN", HTML("<p> &sigma; </p>"), value = 1, min = .Machine$double.xmin)),
                                                                          conditionalPanel("input.tau_dist == 'TruncCauchy'", HTML("<p> &tau; ~ TruncCauchy(&mu;, &sigma;) </p>"), 
                                                                                           numericInput("tau_arg1_TC", HTML("<p> &mu; </p>"), value = 0), 
                                                                                           numericInput("tau_arg2_TC", HTML("<p> &sigma; </p>"), value = 1, min = .Machine$double.xmin)),
                                                                          conditionalPanel("input.tau_dist == 'Exp'", HTML("<p> &tau; ~ Exp(rate = &lambda;) </p>"), 
                                                                                           numericInput("tau_arg_exp", HTML("<p> &lambda; </p>"), value = 1)), 
                                                                          conditionalPanel("input.tau_dist == 'Fixed'", HTML("<p> &tau; = &tau;<sub>0</sub> </p>"), 
                                                                                           numericInput("tau_fixed", HTML("<p> &tau;<sub>0</sub> </p>"), value = 1)),
                                                                          selectInput("beta_choice", HTML("<p> Specify prior for &beta; (common mean). </p>"), choices = c("Default", "Specify")),
                                                                          conditionalPanel("input.beta_choice == 'Specify'", HTML("<p> &beta; ~ Normal(&mu;, &sigma;) </p>"),
                                                                                           numericInput("beta_mu", HTML("<p> &mu; </p>"), value = 0), 
                                                                                           numericInput("beta_sigma", HTML("<p> &sigma; </p>"), value = 100, min = .Machine$double.xmin)),
                                                                          conditionalPanel("input.beta_choice == 'Default'", 
                                                                                           conditionalPanel("input.endpt == 'Binary'", HTML("<p> Default prior is &beta; ~ N(0, 2) </p>")),
                                                                                           conditionalPanel("input.endpt == 'Normal'", HTML("<p> Default prior is &beta; ~ N(0, 100 * sd(y))) </p>")),
                                                                                           conditionalPanel("input.endpt == 'Poisson'", HTML("<p> Default prior is &beta; ~ N(0, sd(log(y + 0.5 + log(t*n))))" )))
                                                                          
                                                         )
                                        )),
                           mainPanel(
                             conditionalPanel("input.use_hist",
                                              actionButton("make.forest", tags$b("(4) Visualize meta-data and MAP prior.")), 
                                              br(),
                                              textOutput("samp_size_warn"),
                                              br(),
                                              plotOutput("forest"),
                                              htmlOutput("plot_err"),
                                              textOutput("unique_warn"),
                                              tableOutput("MAP_summ"),
                                              textOutput("ESS"), br(),
                                              textOutput("ESS.rob"))
                           )
                         )),
                
                tabPanel("PoS calculation",
                         fluidRow(column(5, tags$b("(1) Enter interim data."), 
                                         selectInput("samp", "One or two group trial?", c("One", "Two")),
                                         checkboxInput("parallel", "Concurrent trial data at interim?"),
                                         conditionalPanel("input.samp == 'One' & !input.parallel", DT::dataTableOutput("table1", width = 3)),
                                         conditionalPanel("input.samp == 'One' & input.parallel", DT::dataTableOutput("table2", width = 3)),
                                         conditionalPanel("input.samp == 'Two' & !input.parallel", DT::dataTableOutput("table3", width = 3)),
                                         conditionalPanel("input.samp == 'Two' & input.parallel", DT::dataTableOutput("table4", width = 3)),
                                         br(),
                                         numericInput("wt", "Weight for robust prior", value = 0.2, min = 0, max = 1)
                         ), # close column()
                         column(3, tags$b("(2) Enter the definition of trial success."),
                                selectInput("ncrit", "One or two success criteria?", c("One", "Two")),
                                selectInput("sidedness", "Lower or upper tail?", c("Upper", "Lower")),
                                conditionalPanel("input.samp == 'Two' & (input.endpt == 'Binary' | input.endpt == 'Poisson')",
                                                 selectInput("link", "Select a link function g() for the decision rule", 
                                                             choices = c("identity", "logit", "log"))),
                                conditionalPanel("input.ncrit == 'One'",
                                                 conditionalPanel("input.endpt == 'Binary'",
                                                                  conditionalPanel("input.sidedness == 'Lower' & input.samp == 'One'",  HTML("<p> Decision rule: <br>P(p < p<sub>0</sub>) > p<sub>crit</sub> </p>")),
                                                                  conditionalPanel("input.sidedness == 'Upper' & input.samp == 'One'",  HTML("<p> Decision rule: <br>P(p > p<sub>0</sub>) > p<sub>crit</sub> </p>")),
                                                                  conditionalPanel("input.sidedness == 'Lower' & input.samp == 'Two'", HTML("<p> Decision rule: <br>P(g(p<sub>ctrl</sub>) - g(p<sub>trt</sub>) < &Delta;<sub>0</sub>) > p<sub>crit</sub> </p>")),
                                                                  conditionalPanel("input.sidedness == 'Upper' & input.samp == 'Two'", HTML("<p> Decision rule: <br>P(g(p<sub>ctrl</sub>) - g(p<sub>trt</sub>) > &Delta;<sub>0</sub>) > p<sub>crit</sub> </p>"))
                                                 ),
                                                 conditionalPanel("input.endpt == 'Normal'",
                                                                  conditionalPanel("input.sidedness == 'Lower' & input.samp == 'One'", HTML("<p> Decision rule: <br>P(&mu; < &mu;<sub>0</sub>) > p<sub>crit</sub> </p>")),
                                                                  conditionalPanel("input.sidedness == 'Upper' & input.samp == 'One'", HTML("<p> Decision rule: <br>P(&mu; > &mu;<sub>0</sub>) > p<sub>crit</sub> </p>")),
                                                                  conditionalPanel("input.sidedness == 'Lower' & input.samp == 'Two'", HTML("<p> Decision rule: <br>P(&mu;<sub>ctrl</sub> - &mu;<sub>trt</sub> < &Delta;<sub>0</sub>) > p<sub>crit</sub> </p>")),
                                                                  conditionalPanel("input.sidedness == 'Upper' & input.samp == 'Two'", HTML("<p> Decision rule: <br>P(&mu;<sub>ctrl</sub> - &mu;<sub>trt</sub> > &Delta;<sub>0</sub>) > p<sub>crit</sub> </p>"))
                                                 ),
                                                 
                                                 conditionalPanel("input.endpt == 'Poisson'",
                                                                  conditionalPanel("input.samp == 'One' & input.sidedness == 'Lower'", HTML("<p> Decision rule: <br>P(&lambda; < &lambda;<sub>0</sub>) > p<sub>crit</sub> </p>")),
                                                                  conditionalPanel("input.samp == 'One' & input.sidedness == 'Upper'", HTML("<p> Decision rule: <br>P(&lambda; > &lambda;<sub>0</sub>) > p<sub>crit</sub> </p>")),
                                                                  conditionalPanel("input.samp == 'Two' & input.sidedness == 'Lower'", HTML("<p> Decision rule: <br>P(g(&lambda;<sub>ctrl</sub>) - g(&lambda;<sub>trt</sub>) < &Delta;<sub>0</sub>) > p<sub>crit</sub> </p>")),
                                                                  conditionalPanel("input.samp == 'Two' & input.sidedness == 'Upper'", HTML("<p> Decision rule: <br>P(g(&lambda;<sub>ctrl</sub>) - g(&lambda;<sub>trt</sub>) > &Delta;<sub>0</sub>) > p<sub>crit</sub> </p>"))
                                                 )
                                ), # close one-criteria conditionalPanel
                                
                                
                                conditionalPanel("input.ncrit == 'Two'" ,
                                                 conditionalPanel("input.endpt == 'Binary'",
                                                                  conditionalPanel("input.samp == 'One' & input.sidedness == 'Lower'",  HTML("<p> Decision rules: <br> (1) P(p < p<sub>0</sub>) > p<sub>crit</sub><br> (2) P(p < p<sub>02</sub>) > p<sub>crit2</sub> </p>")),
                                                                  conditionalPanel("input.samp == 'One' & input.sidedness == 'Upper'",  HTML("<p> Decision rules: <br> (1) P(p > p<sub>0</sub>) > p<sub>crit</sub><br> (2) P(p > p<sub>02</sub>) > p<sub>crit2</sub> </p>")),
                                                                  conditionalPanel("input.samp == 'Two' & input.sidedness == 'Lower'", HTML("<p> Decision rules: <br> (1) P(g(p<sub>ctrl</sub>) - g(p<sub>trt</sub>) < &Delta;<sub>0</sub>) > p<sub>crit</sub><br> (2) P(g(p<sub>ctrl</sub>) - g(p<sub>trt</sub>) < &Delta;<sub>02</sub>) > p<sub>crit2</sub></p>")),
                                                                  conditionalPanel("input.samp == 'Two' & input.sidedness == 'Upper'", HTML("<p> Decision rules: <br> (1) P(g(p<sub>ctrl</sub>) - g(p<sub>trt</sub>) > &Delta;<sub>0</sub>) > p<sub>crit</sub><br> (2) P(g(p<sub>ctrl</sub>) - g(p<sub>trt</sub>) > &Delta;<sub>02</sub>) > p<sub>crit2</sub></p>"))
                                                 ),
                                                 
                                                 conditionalPanel("input.endpt == 'Normal'",
                                                                  conditionalPanel("input.samp == 'One' & input.sidedness == 'Lower'", HTML("<p> Decision rules: <br> (1) P(&mu; < &mu;<sub>0</sub>) > p<sub>crit</sub><br> (2) P(&mu; < &mu;<sub>02</sub>) > p<sub>crit2</sub> </p>")),
                                                                  conditionalPanel("input.samp == 'One' & input.sidedness == 'Upper'", HTML("<p> Decision rules: <br> (1) P(&mu; > &mu;<sub>0</sub>) > p<sub>crit</sub><br> (2) P(&mu; > &mu;<sub>02</sub>) > p<sub>crit2</sub> </p>")),
                                                                  conditionalPanel("input.samp == 'Two' & input.sidedness == 'Lower'", HTML("<p> Decision rules: <br> (1) P(&mu;<sub>ctrl</sub> - &mu;<sub>trt</sub> < &Delta;<sub>0</sub>) > p<sub>crit</sub><br> (2) P(&mu;<sub>ctrl</sub> - &mu;<sub>trt</sub> < &Delta;<sub>02</sub>) > p<sub>crit2</sub> </p>")),
                                                                  conditionalPanel("input.samp == 'Two' & input.sidedness == 'Upper'", HTML("<p> Decision rules: <br> (1) P(&mu;<sub>ctrl</sub> - &mu;<sub>trt</sub> > &Delta;<sub>0</sub>) > p<sub>crit</sub><br> (2) P(&mu;<sub>ctrl</sub> - &mu;<sub>trt</sub> > &Delta;<sub>02</sub>) > p<sub>crit2</sub> </p>"))
                                                 ),
                                                 
                                                 conditionalPanel("input.endpt == 'Poisson'",
                                                                  conditionalPanel("input.samp == 'One' & input.sidedness == 'Lower'", HTML("<p> Decision rules: <br> (1) P(&lambda; < &lambda;<sub>0</sub>) > p<sub>crit</sub><br> (2) P(&lambda; < &lambda;<sub>02</sub>) > p<sub>crit2</sub> </p>")),
                                                                  conditionalPanel("input.samp == 'One' & input.sidedness == 'Upper'", HTML("<p> Decision rules: <br> (1) P(&lambda; > &lambda;<sub>0</sub>) > p<sub>crit</sub><br> (2) P(&lambda; > &lambda;<sub>02</sub>) > p<sub>crit2</sub> </p>")),
                                                                  conditionalPanel("input.samp == 'Two' & input.sidedness == 'Lower'", HTML("<p> Decision rules: <br> (1) P(g(&lambda;<sub>ctrl</sub>) - g(&lambda;<sub>trt</sub>) < &Delta;<sub>0</sub>) > p<sub>crit</sub><br> (2)  P(g(&lambda;<sub>ctrl</sub>) - g(&lambda;<sub>trt</sub>) < &Delta;<sub>02</sub>) > p<sub>crit2</sub> </p>")),
                                                                  conditionalPanel("input.samp == 'Two' & input.sidedness == 'Upper'", HTML("<p> Decision rules: <br> (1) P(g(&lambda;<sub>ctrl</sub>) - g(&lambda;<sub>trt</sub>) > &Delta;<sub>0</sub>) > p<sub>crit</sub><br> (2) P(g(&lambda;<sub>ctrl</sub>) - g(&lambda;<sub>trt</sub>) > &Delta;<sub>02</sub>) > p<sub>crit2</sub> </p>"))
                                                                  
                                                 ) # close conditionalPanel Poisson
                                ), # close two-criteria conditionalPanel
                                conditionalPanel("input.endpt == 'Binary' & input.samp == 'One'", numericInput("p0", HTML("<p> Null value, p<sub>0</sub> </p>"), value = 0.2, min = 0.01, max = 0.99)),
                                conditionalPanel("input.endpt == 'Binary' & input.samp == 'Two'", numericInput("diff0_bin", HTML("<p> Null value, &Delta;<sub>0</sub> </p>"), value = 0)),
                                conditionalPanel("input.endpt == 'Normal' & input.samp == 'One'", numericInput("mu0", HTML("<p> Null value, &mu;<sub>0</sub> </p>"), value = 18, min = 0.01, max = 0.99)),
                                conditionalPanel("input.endpt == 'Normal' & input.samp == 'Two'", numericInput("diff0_norm", HTML("<p> Null value, &Delta;<sub>0</sub> </p>"), value = 0)),
                                conditionalPanel("input.endpt == 'Poisson' & input.samp == 'One'", numericInput("lambda0", HTML("<p> Null value, &lambda;<sub>0</sub> </p>"), value = 1, min = 0.01, max = 0.99)),
                                conditionalPanel("input.endpt == 'Poisson' & input.samp == 'Two'", numericInput("diff0_pois", HTML("<p> Null value, &Delta;<sub>0</sub> </p>"), value = 0)),
                                
                                
                                numericInput("pc1", HTML("<p> Critical probability threshold, p<sub>crit</sub> </p>"), value = 0.95, min = .Machine$double.xmin, max = 1 - .Machine$double.xmin), 
                                conditionalPanel("input.ncrit == 'Two'", 
                                                 conditionalPanel("input.endpt == 'Binary' & input.samp == 'One'", numericInput("qc2_bin", HTML("<p> 2nd null value, p<sub>02</sub> </p>"), value = 0.3)),
                                                 conditionalPanel("input.endpt == 'Normal' & input.samp == 'One'", numericInput("qc2_norm", HTML("<p> 2nd null value, &mu;<sub>02</sub> </p>"), value = 19)),
                                                 conditionalPanel("input.endpt == 'Poisson' & input.samp == 'One'", numericInput("qc2_pois", HTML("<p> 2nd null value, &lambda;<sub>02</sub> </p>"), value = 0.8)),
                                                 conditionalPanel("input.samp == 'Two'", numericInput("qc2", HTML("<p> 2nd null value, &Delta;<sub>02</sub> </p>"), value = 0)),
                                                 numericInput("pc2", HTML("<p> Critical probability threshold, p<sub>crit2</sub> </p>"), value = 0.5, min = .Machine$double.xmin, max = 1 - .Machine$double.xmin)
                                )
                                
                         ),
                         
                         column(3, actionButton("compute.pos", tags$b("(3) Compute PoS!")), br(),
                                
                                textOutput("warning.wt"),
                                textOutput("warning.alpha"),
                                textOutput("warning.alpha2"),
                                br(),
                                htmlOutput("results")
                         ) # close column
                         ) # close fluidRow
                ), # close tabPanel 
                tabPanel("Help", uiOutput("link0"), br(), uiOutput("link"), br(), uiOutput("link2"), 
                         br(), uiOutput("link3"), br(), uiOutput("link4")),
                tags$style(HTML("
                                input[type=number] {
                                -moz-appearance:textfield;
                                }
                                input[type=number]::{
                                -moz-appearance:textfield;
                                }
                                input[type=number]::-webkit-outer-spin-button,
                                input[type=number]::-webkit-inner-spin-button {
                                -webkit-appearance: none;
                                margin: 0;
                                }
                                "))
) # close NavBarPanel

server = function(input, output, session) {
  
  this_table_bin = data.frame(study = c("Study 1", "Study 2", "Study 3"), n = rep(100, 3), r = c(40, 50, 60), stringsAsFactors = FALSE)
  names(this_table_bin) = c("Study", "n at interim", "# of events")
  this_table_norm = data.frame(study = c("Study 1", "Study 2", "Study 3"), n = rep(100, 3), m = c(10, 20, 30), stringsAsFactors = FALSE)
  names(this_table_norm) = c("Study", "n at interim", "Sample mean")
  this_table_pois = data.frame(study = c("Study 1", "Study 2", "Study 3"), n = rep(100, 3), y = c(1113, 980, 1020), t = rep(12, 3), stringsAsFactors = FALSE)
  names(this_table_pois) = c("Study", "n at interim", "Total count", "Trial duration")
  
  val = reactiveValues(mat = this_table_bin)
  val2 = reactiveValues(mat = this_table_norm)
  val3 = reactiveValues(mat = this_table_pois)
  
  which_val = function() {
    if(input$endpt == "Binary") {
      return(val)   
    } else if(input$endpt == "Normal") {
      return(val2)
    } else{
      return(val3)
    }
  }
  
  observeEvent(input$add_btn, {
    if(input$endpt == "Binary") {
      t = as.data.frame(rbind(which_val()$mat, c(input$study_add, input$n_add, input$r_add)))
      val$mat <<- t    
    } else if(input$endpt ==   "Normal") {
      t = as.data.frame(rbind(which_val()$mat, c(input$study_add, input$n_add, input$m_add)))
      val2$mat <<- t
    } else{
      t = as.data.frame(rbind(which_val()$mat, c(input$study_add, input$n_add, input$m_add_pois, input$t_add)))
      val3$mat <<- t
    }
  })
  
  observeEvent(input$delete_btn, {
    t = which_val()$mat
    if(!is.numeric(input$delete_row) | is.null(input$delete_row)) {
      t = t
    } else if(input$delete_row%%1 != 0) {
      t = t
    } else if(input$delete_row > nrow(t) | input$delete_row < 1) {
      t = t
    } else if(nrow(t) == 1) {
      t = t
    } else {
      t = t[-as.numeric(input$delete_row),]
    }
    if(input$endpt == "Binary") {
      val$mat <<- t    
    } else if(input$endpt == "Normal") {
      val2$mat <<- t
    } else{
      val3$mat <<- t
    }
  })
  
  meta.data = reactive({
    if(input$endpt == "Binary") {
      inFile = input$file1.bin
    } else if(input$endpt == "Normal") {
      inFile = input$file1.norm
    } else{
      inFile = input$file1.pois
    }
    if(is.null(inFile)) return(NULL)
    if(file_ext(inFile$datapath) != "csv") return("File upload must be .csv extension!")
    dat = read.csv(inFile$datapath, header = input$header)
    dat 
  })
  
  # Create editable DataTabe of meta-data.
  output$table <- DT::renderDataTable({
    if(input$endpt == "Binary") {
      DT::datatable(val$mat, options = list(dom = 't', columnDefs = list(list(width = "50px", targets = "_all"))), 
                    editable = TRUE, selection="single", rownames = FALSE)
    } else if(input$endpt == "Normal") {
      DT::datatable(val2$mat, options = list(dom = 't', columnDefs = list(list(width = "50px", targets = "_all"))), 
                    editable = TRUE, selection="single", rownames = FALSE)
    } else{
      DT::datatable(val3$mat, options = list(dom = 't', columnDefs = list(list(width = "50px", targets = "_all"))), 
                    editable = TRUE, selection="single", rownames = FALSE)
    }
    
  }, server = TRUE)
  
  proxy = dataTableProxy('table')
  
  observeEvent(input$table_cell_edit, {
    info = input$table_cell_edit
    
    i = info$row 
    j = info$col+1
    v = info$val
    
    if(input$endpt == "Binary") {
      val$mat[i,j] = DT::coerceValue(v, val$mat[i,j])
      replaceData(proxy, val$mat, rownames = FALSE)
    } else if(input$endpt == "Normal") {
      val2$mat[i,j] = DT::coerceValue(v, val2$mat[i,j])
      replaceData(proxy, val2$mat, rownames = FALSE)
    } else{
      val3$mat[i,j] = DT::coerceValue(v, val3$mat[i,j])
      replaceData(proxy, val3$mat, rownames = FALSE)
    }
    
    
  })
  
  this_table1_bin = data.frame(study = "Trial A", n = 100, r = 35, n.target = 150, stringsAsFactors = FALSE)
  names(this_table1_bin) = c("Trial", "n at interim", "# of events", "# target patients")
  this_table1_norm = data.frame(study = "Trial A", n = 100, m = 20, n.target = 150, stringsAsFactors = FALSE)
  names(this_table1_norm) = c("Trial", "n at interim", "Sample mean", "# target patients")
  this_table1_pois = data.frame(study = "Trial A", n = 100, y = 1260, n.target = 150, t = 12, stringsAsFactors = FALSE)
  names(this_table1_pois) = c("Trial", "n at interim", "Total count", "# target patients", "Trial duration")
  
  val.1.bin = reactiveValues(mat = this_table1_bin)
  val.1.norm = reactiveValues(mat = this_table1_norm)
  val.1.pois = reactiveValues(mat = this_table1_pois)
  
  which_val1 = function() {
    if(input$endpt == "Binary") {
      return(val.1.bin)   
    } else if(input$endpt == "Normal") {
      return(val.1.norm)
    } else{
      return(val.1.pois)
    }
  }
  
  output$table1 <- DT::renderDataTable({
    if(input$endpt == "Binary") {
      DT::datatable(val.1.bin$mat, options = list(dom = 't', columnDefs = list(list(width = "50px", targets = "_all"))), 
                    editable = TRUE, selection="single", rownames = FALSE)
    } else if(input$endpt == "Normal") {
      DT::datatable(val.1.norm$mat, options = list(dom = 't', columnDefs = list(list(width = "50px", targets = "_all"))), 
                    editable = TRUE, selection="single", rownames = FALSE)
    } else{
      DT::datatable(val.1.pois$mat, options = list(dom = 't', columnDefs = list(list(width = "50px", targets = "_all"))), 
                    editable = TRUE, selection="single", rownames = FALSE)
    }
    
  }, server = TRUE)
  
  proxy1 = dataTableProxy('table1')
  
  observeEvent(input$table1_cell_edit, {
    info = input$table1_cell_edit
    
    i = info$row
    j = info$col+1
    v = info$val
    
    if(input$endpt == "Binary") {
      val.1.bin$mat[i,j] = DT::coerceValue(v, val.1.bin$mat[i,j])
      replaceData(proxy1, val.1.bin$mat)
    } else if(input$endpt == "Normal") {
      val.1.norm$mat[i,j] = DT::coerceValue(v, val.1.norm$mat[i,j])
      replaceData(proxy1, val.1.norm$mat)
    } else{
      val.1.pois$mat[i,j] = DT::coerceValue(v, val.1.pois$mat[i,j])
      replaceData(proxy1, val.1.pois$mat)
    }
    
  })
  
  this_table2_bin = data.frame(study = c("Trial A", "Trial B"), n = c(100, 100), r = c(35, 33), n.target = c(150,150), stringsAsFactors = FALSE)
  names(this_table2_bin) = c("Trial", "n at interim", "# of events", "# target patients")
  this_table2_norm = data.frame(study = c("Trial A", "Trial B"), n = c(100, 100), m = c(20, 19), n.target = c(150,150), stringsAsFactors = FALSE)
  names(this_table2_norm) = c("Trial", "n at interim", "Sample mean", "# target patients")
  this_table2_pois = data.frame(study = c("Trial A", "Trial B"), n = c(100, 100), y = c(1270, 1260), n.target = c(150,150), t = c(12, 12), stringsAsFactors = FALSE)
  names(this_table2_pois) = c("Trial", "n at interim", "Total count", "# target patients", "Trial duration")
  
  val.2.bin = reactiveValues(mat = this_table2_bin)
  val.2.norm = reactiveValues(mat = this_table2_norm)
  val.2.pois = reactiveValues(mat = this_table2_pois)
  
  which_val2 = function() {
    if(input$endpt == "Binary") {
      return(val.2.bin)   
    } else if(input$endpt == "Normal") {
      return(val.2.norm)
    } else{
      return(val.2.pois)
    }
  }
  
  output$table2 <- DT::renderDataTable({
    if(input$endpt == "Binary") {
      DT::datatable(val.2.bin$mat, options = list(dom = 't', columnDefs = list(list(width = "50px", targets = "_all"))), 
                    editable = TRUE, selection="single", rownames = FALSE)
    } else if(input$endpt == "Normal") {
      DT::datatable(val.2.norm$mat, options = list(dom = 't', columnDefs = list(list(width = "50px", targets = "_all"))), 
                    editable = TRUE, selection="single", rownames = FALSE)
    } else{
      DT::datatable(val.2.pois$mat, options = list(dom = 't', columnDefs = list(list(width = "50px", targets = "_all"))), 
                    editable = TRUE, selection="single", rownames = FALSE)
    }
    
  }, server = TRUE)
  
  proxy2 = dataTableProxy('table2')
  
  observeEvent(input$table2_cell_edit, {
    info = input$table2_cell_edit
    
    i = info$row
    j = info$col+1
    v = info$val
    
    if(input$endpt == "Binary") {
      val.2.bin$mat[i,j] = DT::coerceValue(v, val.2.bin$mat[i,j])
      replaceData(proxy2, val.2.bin$mat, rownames = FALSE)
    } else if(input$endpt == "Normal") {
      val.2.norm$mat[i,j] = DT::coerceValue(v, val.2.norm$mat[i,j])
      replaceData(proxy2, val.2.norm$mat, rownames = FALSE)
    } else{
      val.2.pois$mat[i,j] = DT::coerceValue(v, val.2.pois$mat[i,j])
      replaceData(proxy2, val.2.pois$mat, rownames = FALSE)
    }
  })
  
  this_table3_bin = data.frame(group = c("Treatment", "Control"), n = c(100, 100), r = c(35, 50), n.target = c(150,150), stringsAsFactors = FALSE)
  names(this_table3_bin) = c("Group", "n at interim", "# of events", "# target patients")
  this_table3_norm = data.frame(group = c("Treatment", "Control"), n = c(100, 100), m = c(18, 20), n.target = c(150,150), stringsAsFactors = FALSE)
  names(this_table3_norm) = c("Group", "n at interim", "Sample mean", "# target patients")
  this_table3_pois = data.frame(group = c("Treatment", "Control"), n = c(100, 100), y = c(1113, 1200), n.target = c(150,150), t = c(12, 12), stringsAsFactors = FALSE)
  names(this_table3_pois) = c("Group", "n at interim", "Total count", "# target patients", "Trial duration")
  
  val.3.bin = reactiveValues(mat = this_table3_bin)
  val.3.norm = reactiveValues(mat = this_table3_norm)
  val.3.pois = reactiveValues(mat = this_table3_pois)
  
  which_val3 = function() {
    if(input$endpt == "Binary") {
      return(val.3.bin)   
    } else if(input$endpt == "Normal") {
      return(val.3.norm)
    } else{
      return(val.3.pois)
    }
  }
  
  output$table3 <- DT::renderDataTable({
    if(input$endpt == "Binary") {
      DT::datatable(val.3.bin$mat, options = list(dom = 't', columnDefs = list(list(width = "50px", targets = "_all"))), 
                    editable = TRUE, selection="single", rownames = FALSE)
    } else if(input$endpt == "Normal") {
      DT::datatable(val.3.norm$mat, options = list(dom = 't', columnDefs = list(list(width = "50px", targets = "_all"))), 
                    editable = TRUE, selection="single", rownames = FALSE)
    } else{
      DT::datatable(val.3.pois$mat, options = list(dom = 't', columnDefs = list(list(width = "50px", targets = "_all"))), 
                    editable = TRUE, selection="single", rownames = FALSE)
    }
    
  }, server = TRUE)
  
  proxy3 = dataTableProxy('table3')
  
  observeEvent(input$table3_cell_edit, {
    info = input$table3_cell_edit
    
    i = info$row
    j = info$col+1
    v = info$val
    
    if(input$endpt == "Binary") {
      val.3.bin$mat[i,j] = DT::coerceValue(v, val.3.bin$mat[i,j])
      replaceData(proxy3, val.3.bin$mat, rownames = FALSE)
    } else if(input$endpt == "Normal") {
      val.3.norm$mat[i,j] = DT::coerceValue(v, val.3.norm$mat[i,j])
      replaceData(proxy3, val.3.norm$mat, rownames = FALSE)
    } else{
      val.3.pois$mat[i,j] = DT::coerceValue(v, val.3.pois$mat[i,j])
      replaceData(proxy3, val.3.pois$mat, rownames = FALSE)
    }
  })
  
  this_table4_bin = data.frame(group = c("Trt, Trial A", "Ctrl, Trial A", "Trt, Trial B", "Ctrl, Trial B"), n = rep(100, 4), r = c(42, 48, 35, 48), n.target = rep(150, 4), stringsAsFactors = FALSE)
  names(this_table4_bin) = c("Group", "n at interim", "# of events", "# target patients")
  this_table4_norm = data.frame(group = c("Trt, Trial A", "Ctrl, Trial A", "Trt, Trial B", "Ctrl, Trial B"), n = rep(100, 4), m = c(18, 20, 20.5, 21.5), n.target = rep(150, 4), stringsAsFactors = FALSE)
  names(this_table4_norm) = c("Group", "n at interim", "Sample mean", "# target patients")
  this_table4_pois = data.frame(group = c("Trt, Trial A", "Ctrl, Trial A", "Trt, Trial B", "Ctrl, Trial B"), n = rep(100, 4), y = c(1113, 1020, 1113, 1020), n.target = rep(150, 4), t = rep(12, 4), stringsAsFactors = FALSE)
  names(this_table4_pois) = c("Group", "n at interim", "Total count", "# target patients", "Trial duration")
  
  val.4.bin = reactiveValues(mat = this_table4_bin)
  val.4.norm = reactiveValues(mat = this_table4_norm)
  val.4.pois = reactiveValues(mat = this_table4_pois)
  
  which_val4 = function() {
    if(input$endpt == "Binary") {
      return(val.4.bin)   
    } else if(input$endpt == "Normal") {
      return(val.4.norm)
    } else{
      return(val.4.pois)
    }
  }
  
  output$table4 <- DT::renderDataTable({
    if(input$endpt == "Binary") {
      DT::datatable(val.4.bin$mat, options = list(dom = 't', columnDefs = list(list(width = "50px", targets = "_all"))), 
                    editable = TRUE, selection="single", rownames = FALSE)
    } else if(input$endpt == "Normal") {
      DT::datatable(val.4.norm$mat, options = list(dom = 't', columnDefs = list(list(width = "50px", targets = "_all"))), 
                    editable = TRUE, selection="single", rownames = FALSE)
    } else{
      DT::datatable(val.4.pois$mat, options = list(dom = 't', columnDefs = list(list(width = "50px", targets = "_all"))), 
                    editable = TRUE, selection="single", rownames = FALSE)
    }
    
  }, server = TRUE)
  
  proxy4 = dataTableProxy('table4')
  
  observeEvent(input$table4_cell_edit, {
    info = input$table4_cell_edit
    
    i = info$row
    j = info$col+1
    v = info$val
    
    if(input$endpt == "Binary") {
      val.4.bin$mat[i,j] = DT::coerceValue(v, val.4.bin$mat[i,j])
      replaceData(proxy4, val.4.bin$mat, rownames = FALSE)
    } else if(input$endpt == "Normal") {
      val.4.norm$mat[i,j] = DT::coerceValue(v, val.4.norm$mat[i,j])
      replaceData(proxy4, val.4.norm$mat, rownames = FALSE)
    } else{
      val.4.pois$mat[i,j] = DT::coerceValue(v, val.4.pois$mat[i,j])
      replaceData(proxy4, val.4.pois$mat, rownames = FALSE)
    }
  })
  
  # Error checking for MAP prior
  errCheck_MAP = function() {
    output$warning.wt = output$warning.alpha = renderText({""})
    
    if(packageVersion("RBesT") < "1.4.0") {return("You must install the latest version of 'RBesT'.")}
    
    if(input$hist_data_option == "Manual") {
      co.data = which_val()$mat
    } else{
      co.data = meta.data()
      if(class(co.data) == "character") return(co.data)
    }
    
    co.data[,2] = as.numeric(co.data[,2])
    co.data[,3] = as.numeric(co.data[,3])
    if(input$endpt == "Poisson") co.data[,4] = as.numeric(co.data[,4])
    
    # Error check historical data
    if(sum(co.data[,1] == "") > 0 | sum(is.na(co.data[,c(2,3)])) > 0) return("Please complete the table of historical trial data.")
    if(sum(co.data[,2] == "") > 0) return("Please complete the table of historical trial data.")
    if(sum(co.data[,3] == "") > 0) return("Please complete the table of historical trial data.")
    if(input$endpt == "Poisson") {
      if(sum(is.na(co.data[,4])) > 0) return("Please complete the table of historical trial data.")
    }
    
    if(length(unique(co.data[,1])) != nrow(co.data)) {
      output$unique_warn = renderText({"Each row should uniquely define one study."})
    } else{
      output$unique_warn = renderText({""})
    }
    
    if(input$hist_data_option == "Manual") {
      if(sum(nchar(co.data[,1]) > 30)) {return("For readability, please keep the names of all studies below 30.")}
    } 
    
    if(sum(co.data[,2] <= 0) > 0) return("Your 2nd column should contain control arm sample sizes, which are positive integers.")  
    if(sum(co.data[,2]%%1 != 0) > 0) return("Your 2nd column should contain control arm sample sizes, which are positive integers.")  
    
    if(sum(!is.numeric(co.data[,2])) > 0) return("Your 2nd column should contain numbers.")
    if(sum(co.data[,3] < 0) > 0 & input$endpt == "Binary") return("Your 3rd column should contain # of events in control group, which are non-negative integers.")  
    if(input$endpt == "Binary" & sum(co.data[,3]%%1 != 0) > 0) return("Your 3rd column should contain control arm sample sizes, which are positive integers.")  
    
    if(sum(!is.numeric(co.data[,3])) > 0) return("Your 3rd column should contain numbers.")
    if(input$endpt == "Binary" & (sum(co.data[,2] < co.data[,3]) > 0)) return("Your number of events (r) cannot exceed your sample size (n).")
    if(input$endpt == "Poisson") {
      if(sum(!is.numeric(co.data[,4])) > 0) return("Your 4th column should contain numbers.")
      if(((sum(co.data[,3] < 0) > 0) | (sum(co.data[,4] <= 0) > 0))) return("Your total counts (y) must be non-negative and your trial durations (t) must be positive.")
    }
    
    if(sum(co.data[,2] >= 100000) > 0) {
      output$samp_size_warn = renderText({"MAP prior is slower to compute for large sample sizes..."})
    } else{
      output$samp_size_warn = renderText({""})
    }
    
    if(input$endpt == "Normal" & (!is.numeric(input$sigma) | input$sigma <= 0)) {return("Reference Scale must be a number greater than 0.")}
    if(input$endpt == "Normal" & input$sigma > 1000000) {return("Are you sure the Reference Scale is that high? Think about rescaling the data.")}
    if(!is.numeric(input$seed) | !input$seed%%1 == 0| input$seed < 1){return("Seed must be a positive integer.")}
    
    
    if(!is.numeric(input$wt) | input$wt < 0 | input$wt > 1){return("Weight for non-informative prior must be between 0 and 1.")}
    if(input$wt > 0.7 & input$wt <= 1){
      output$warning.wt = renderText({
        ("Note: It is not recommended for the non-informative prior's weight to be greater than 0.7.")
      })
    }
    if(input$tau_dist == "HalfNormal" & (!is.numeric(input$tau_arg1_HN) | input$tau_arg1_HN <= 0)) {return(HTML(("<p> Parameters for &tau; prior must be non-negative numbers. </p>")))}
    if(input$tau_dist == "TruncNormal" & (!is.numeric(input$tau_arg1_TN) | !is.numeric(input$tau_arg2_TN) | input$tau_arg1_TN < 0 | input$tau_arg2_TN <= 0)) {return(HTML(("<p> Parameters for &tau; prior must be non-negative numbers. </p>")))}
    if(input$tau_dist == "Uniform" & (!is.numeric(input$tau_arg1_Uni) | !is.numeric(input$tau_arg2_Uni) | (input$tau_arg2_Uni <= input$tau_arg1_Uni))) {return(HTML(("<p> Parameters for &tau; prior must be numeric, with a < b. </p>")))}
    if(input$tau_dist == "Gamma" & (!is.numeric(input$tau_arg1_G) | !is.numeric(input$tau_arg2_G) | input$tau_arg1_G <= 0 | input$tau_arg2_G <= 0)) {return(HTML(("<p> Parameters for &tau; prior must be positive numbers. </p>")))}
    if(input$tau_dist == "InvGamma" & (!is.numeric(input$tau_arg1_IG) | !is.numeric(input$tau_arg2_IG) | input$tau_arg1_IG <= 0 | input$tau_arg2_IG <= 0)) {return(HTML(("<p> Parameters for &tau; prior must be positive numbers. </p>")))}
    if(input$tau_dist == "LogNormal" & (!is.numeric(input$tau_arg1_LN) | !is.numeric(input$tau_arg2_LN) | input$tau_arg1_LN < 0 | input$tau_arg2_LN <= 0)) {return(HTML(("<p> Parameters for &tau; prior must be non-negative numbers. </p>")))}
    if(input$tau_dist == "TruncCauchy" & (!is.numeric(input$tau_arg1_TC) | !is.numeric(input$tau_arg2_TC) | input$tau_arg1_TC < 0 | input$tau_arg2_TC <= 0)) {return(HTML(("<p> Parameters for &tau; prior must be non-negative numbers. </p>")))}
    if(input$tau_dist == "Exp" & (input$tau_arg_exp <= 0 | !is.numeric(input$tau_arg_exp))) {return(HTML(("<p> &lambda; must be a positive number. </p>")))}
    if(input$tau_dist == "Fixed" & (input$tau_fixed <= 0 | !is.numeric(input$tau_fixed))) {return(HTML(("<p> &tau;<sub>0</sub> must be a positive number. </p>")))}
    
    if(input$beta_choice == "Specify") {
      if(!is.numeric(input$beta_mu)) {return(HTML("<p> Prior mean for &beta; must be a number. </p>"))}
      if(!is.numeric(input$beta_sigma) | input$beta_sigma <= 0) {return(HTML("<p> Prior standard deviation for &beta; must be a positive number. </p>"))}
    }
    
    return(NULL)
  }
  
  # Error checking for PoS function
  errCheck_PoS = function(use.hist) {
    output$warning.wt = output$warning.alpha = renderText({""})
    
    error_check = errCheck_MAP()
    if(!is.null(error_check)) return(error_check)
    
    if(!is.numeric(input$wt) | input$wt < 0 | input$wt > 1){return("Weight for non-informative prior must be between 0 and 1.")}
    if(input$wt > 0.7 & input$wt <= 1){
      output$warning.wt = renderText({
        ("Note: It is not recommended for the non-informative prior's weight to be greater than 0.7.")
      })
    }
    
    if(input$samp == "One" & !input$parallel) {
      new.data = which_val1()$mat
    } else if(input$samp == "One" & input$parallel) {
      new.data = which_val2()$mat
      n.newB = new.data[2,2]
      n.new.targetB = new.data[2,4]
      if(input$endpt == "Binary") r.newB = new.data[2,3]
      if(input$endpt == "Normal") m.newB = new.data[2,3]
      if(input$endpt == "Poisson") {
        y.newB = new.data[2,3]
        t.newB = new.data[2,5]
      }
    } else if(input$samp == "Two" & !input$parallel) {
      new.data = which_val3()$mat
      n.ctrl = new.data[2,2]
      n.ctrl.target = new.data[2,4]
      if(input$endpt == "Binary") r.ctrl = new.data[2,3]
      if(input$endpt == "Normal") m.ctrl = new.data[2,3]
      if(input$endpt == "Poisson") {
        y.ctrl = new.data[2,3]
        t.ctrl = new.data[2,5]
      }
    } else{
      new.data = which_val4()$mat
      n.ctrl = new.data[2,2]
      n.newB = new.data[3,2]
      n.ctrlB = new.data[4,2]
      n.ctrl.target = new.data[2,4]
      n.new.targetB = new.data[3,4]
      n.ctrl.targetB = new.data[4,4]
      if(input$endpt == "Binary") {
        r.ctrl = new.data[2,3]
        r.newB = new.data[3,3]
        r.ctrlB = new.data[4,3]
      } else if(input$endpt == "Normal") {
        m.ctrl = new.data[2,3]
        m.newB = new.data[3,3]
        m.ctrlB = new.data[4,3]
      } else{
        y.ctrl = new.data[2,3]
        y.newB = new.data[3,3]
        y.ctrlB = new.data[4,3]
        t.ctrl = new.data[2,5]
        t.newB = new.data[3,5]
        t.ctrlB = new.data[4,5]
      }
      
    }
    if(sum(new.data[,1] == "") > 0 | sum(is.na(new.data[,-1])) > 0) {return("Please complete the table.")}
    n.new = new.data[1,2]
    n.new.target = new.data[1,4]
    if(input$endpt == "Binary") r.new = new.data[1,3]
    if(input$endpt == "Normal") m.new = new.data[1,3]
    if(input$endpt == "Poisson") {
      y.new = new.data[1,3]
      t.new = new.data[1,5]
    }
    
    if(input$samp == "Two") {
      if(!is.numeric(n.ctrl) | !n.ctrl%%1 == 0 | n.ctrl < 1){return("Sample size of control group must be a positive integer.")}
      if(!is.numeric(n.ctrl.target) | !n.ctrl.target%%1 == 0 | n.ctrl.target < 1 | n.ctrl.target < n.ctrl){return("Target number of patients must be a positive integer greater than the sample size.")}
    }
    
    if(input$parallel) {
      if(!is.numeric(n.newB) | !n.newB%%1 == 0 | n.newB < 1){return("Sample size of treatment group must be a positive integer.")}
      if(!is.numeric(n.new.targetB) | !n.new.targetB%%1 == 0 | n.new.targetB < 1 | n.new.targetB < n.newB){return("Target number of patients must be a positive integer greater than the sample size.")}
    }
    
    if(!is.numeric(n.new) | !n.new%%1 == 0 | n.new < 1){return("Sample size of treatment group must be a positive integer.")}
    if(input$endpt == "Binary") {
      if(!is.numeric(r.new) |  !r.new%%1 == 0 | r.new < 0 | r.new > n.new){return("Number of events within treatment group must be a non-negative integer smaller than the sample size.")}
    }
    if(input$endpt == "Normal") {
      if(!is.numeric(m.new)) {return("Sample mean within treatment group must be a number.")}
    } 
    if(input$endpt == "Poisson") {
      if(!is.numeric(t.new) | t.new <= 0) {return("Trial duration must be a positive number.")}
      if(!is.numeric(y.new) | y.new < 0){return("Total count within treatment group must be a non-negative number.")}
      if(input$parallel) {
        if(!is.numeric(t.newB) | t.newB < 0){return("Trial duration must be a positive number.")}
        if(!is.numeric(y.newB) | y.newB < 0){return("Total count within treatment group must be a non-negative number.")}
      }
      if(input$samp == "Two") {
        if(!is.numeric(t.ctrl) | t.ctrl < 0){return("Trial duration must be a positive number.")}
        if(!is.numeric(y.ctrl) | y.ctrl < 0){return("Total count within control group must be a non-negative number.")}
        if(input$parallel) {
          if(!is.numeric(t.ctrlB) | t.ctrlB < 0){return("Trial duration must be a positive number.")}
          if(!is.numeric(y.ctrlB) | y.ctrlB < 0){return("Total count within control group must be a non-negative number.")}
        }  
      }
      
    } 
    if(!is.numeric(n.new.target) | n.new.target%%1 != 0 | n.new.target <= 0 | n.new.target <= n.new){return("Number of target patients must be a positive integer greater than the interim sample size.")}
    
    if(input$endpt == "Binary" & input$samp == "Two") {
      if(!n.ctrl%%1 == 0 | n.ctrl < 1){return("Sample size of control group must be a positive integer.")}
      
      if(!is.numeric(n.ctrl.target) | n.ctrl.target%%1 != 0 | n.ctrl.target <= 0 | n.ctrl.target <= n.ctrl){return("Number of target patients must be a positive integer greater than the interim sample size.")}
      
      if(input$endpt == "Binary") {
        if(!is.numeric(r.ctrl) | !r.ctrl%%1 == 0 | r.ctrl < 0 | r.ctrl > n.ctrl){return("Number of events within control group must be a non-negative integer smaller than the sample size.")}
      } else if(input$endpt == "Normal") {
        if(!is.numeric(m.ctrl) | m.ctrl == ""){return("Sample mean within control group must be a number.")}
      } else{
        if(!is.numeric(y.ctrl) | y.ctrl < 0) {return("Total count within treatment group must be a non-negative number.")}
      }
    }
    if(input$endpt == "Normal" & (!is.numeric(input$sigma) | input$sigma < 0)) {return("Reference Scale must be a number greater than 0.")}
    
    if(!is.numeric(input$seed) | !input$seed%%1 == 0| input$seed < 1){return("Seed must be a positive integer.")}
    
    if(use.hist) {
      if(!is.numeric(input$wt) | input$wt < 0 | input$wt > 1){return("Weight for non-informative prior must be between 0 and 1.")}
      if(input$wt > 0.7 & input$wt <= 1){
        output$warning.wt = renderText({
          ("Note: It is not recommended for the non-informative prior's weight to be greater than 0.7.")
        })
      }
      if(input$tau_dist == "HalfNormal" & (!is.numeric(input$tau_arg1_HN) | input$tau_arg1_HN <= 0)) {return(HTML(("<p> Parameter for &tau; prior must be a positive number. </p>")))}
      if(input$tau_dist == "TruncNormal" & (!is.numeric(input$tau_arg1_TN) | !is.numeric(input$tau_arg2_TN) | input$tau_arg1_TN < 0 | input$tau_arg2_TN <= 0)) {return(HTML(("<p> Parameters for &tau; prior must be non-negative numbers. </p>")))}
      if(input$tau_dist == "Uniform" & (!is.numeric(input$tau_arg1_Uni) | !is.numeric(input$tau_arg2_Uni) | (input$tau_arg2_Uni <= input$tau_arg1_Uni))) {return(HTML(("<p> Parameters for &tau; prior must be numeric, with a < b. </p>")))}
      if(input$tau_dist == "Gamma" & (!is.numeric(input$tau_arg1_G) | !is.numeric(input$tau_arg2_G) | input$tau_arg1_G <= 0 | input$tau_arg2_G <= 0)) {return(HTML(("<p> Parameters for &tau; prior must be positive numbers. </p>")))}
      if(input$tau_dist == "InvGamma" & (!is.numeric(input$tau_arg1_IG) | !is.numeric(input$tau_arg2_IG) | input$tau_arg1_IG <= 0 | input$tau_arg2_IG <= 0)) {return(HTML(("<p> Parameters for &tau; prior must be positive numbers. </p>")))}
      if(input$tau_dist == "LogNormal" & (!is.numeric(input$tau_arg1_LN) | !is.numeric(input$tau_arg2_LN) | input$tau_arg1_LN < 0 | input$tau_arg2_LN <= 0)) {return(HTML(("<p> Parameters for &tau; prior must be non-negative numbers. </p>")))}
      if(input$tau_dist == "TruncCauchy" & (!is.numeric(input$tau_arg1_TC) | !is.numeric(input$tau_arg2_TC) | input$tau_arg1_TC < 0 | input$tau_arg2_TC <= 0)) {return(HTML(("<p> Parameters for &tau; prior must be non-negative numbers. </p>")))}
      if(input$tau_dist == "Exp" & (input$tau_arg_exp <= 0 | !is.numeric(input$tau_arg_exp))) {return(HTML(("<p> &lambda; must be a positive number. </p>")))}
      if(input$tau_dist == "Fixed" & (input$tau_fixed < 0 | !is.numeric(input$tau_fixed))) {return(HTML(("<p> &tau;<sub>0</sub> must be a positive number. </p>")))}
    }
    if(input$endpt == "Binary" & input$samp == "One" & !is.numeric(input$p0) | input$p0 < 0 | input$p0 > 1){return(HTML("<p> p<sub>0</sub> must be between 0 and 1. </p>"))}
    if(input$endpt == "Binary" & input$samp == "Two" & !is.numeric(input$diff0_bin)){return("Value for null hypothesis difference in two-sample trials must be a number.")}
    if(input$endpt == "Normal" & input$samp == "Two" & !is.numeric(input$diff0_norm)){return("Value for null hypothesis difference in two-sample trials must be a number.")}
    if(input$endpt == "Poisson" & input$samp == "Two" & !is.numeric(input$diff0_pois)){return("Value for null hypothesis difference in two-sample trials must be a number.")}
    
    if(!is.numeric(input$pc1) | input$pc1 < 0 | input$pc1 > 1){return("Critical probability threshold must be between 0 and 1.")}
    if(input$pc1 < 0.1){
      output$warning.alpha = renderText({
        ("Are you sure you want a critical probability threshold that low?")
      })
    }
    if(input$ncrit == "Two" & (!is.numeric(input$pc2) | input$pc2 < 0 | input$pc2 > 1)){return("Critical probability thresholds must be between 0 and 1.")}
    if(input$pc2 < 0.1){
      output$warning.alpha2 = renderText({
        ("Are you sure you want a critical probability threshold that low?")
      })
    }
    if(input$endpt == "Poisson" & input$link == "logit" & input$samp == "Two") {return("Logit link for Poisson data won't give a sensible decision rule.")}
    
    
    if(!is.numeric(input$qc2)) {return("2nd null value must be a number.")}
    if(input$endpt == "Binary" & input$samp == "One" & (input$qc2 < 0 | input$qc2 > 1)) {return(HTML("<p> p<sub>02</sub> must be between 0 and 1. </p>"))}
    if(input$endpt == "Binary" & input$samp == "Two" & (input$qc2 < -1 | input$qc2 > 1)) {return(HTML("<p> &Delta;<sub>02</sub> must be between 0 and 1. </p>"))}
    
    # Error check co-data case.
    if(input$parallel) {
      if(!is.numeric(n.newB) | !n.newB%%1 == 0 | n.newB < 1){return("Sample sizes must be a positive integer.")}
      if(input$endpt == "Binary") {
        if(!is.numeric(r.newB) |  !r.newB%%1 == 0 | r.newB < 0 | r.newB > n.newB){return("Number of events within treatment group must be a non-negative integer smaller than the sample size.")}
      }
      if(input$endpt == "Normal") {
        if(!is.numeric(m.newB)) {return("Sample mean within treatment group must be a number.")}
      } 
      if(input$endpt == "Poisson") {
        if(!is.numeric(y.newB) | y.newB < 0){return("Total count within treatment group must be a non-negative number.")}
      } 
      if(!is.numeric(n.new.targetB) | n.new.targetB%%1 != 0 | n.new.targetB <= 0 | n.new.targetB <= n.newB){return("Number of target patients must be a positive integer greater than the interim sample size.")}
      if(input$samp == "Two") {
        if(!is.numeric(n.ctrlB) | !n.ctrlB%%1 == 0 | n.ctrlB < 1){return("Sample sizes must be a positive integer.")}
        if(input$endpt == "Binary") {
          if(!is.numeric(r.ctrlB) |  !r.ctrlB%%1 == 0 | r.ctrlB < 0 | r.ctrlB > n.ctrlB){return("Number of events within control group must be a non-negative integer smaller than the sample size.")}
        }
        if(input$endpt == "Normal") {
          if(!is.numeric(m.ctrlB)) {return("Sample mean within control group must be a number.")}
        } 
        if(input$endpt == "Poisson") {
          if(!is.numeric(y.ctrlB) | y.ctrlB < 0){return("Total count within control group must be a non-negative number.")}
        } 
        if(!is.numeric(n.ctrl.targetB) | n.ctrl.targetB%%1 != 0 | n.ctrl.targetB <= 0 | n.ctrl.targetB <= n.ctrlB){return("Number of target patients must be a positive integer greater than the interim sample size.")}
      }
    }
    
    return(NULL)
  }
  
  # Compute gMAP prior
  get.gMAP = function(co.data, beta.prior, tau.prior, tau.dist) {
    # Error check historical data
    if(sum(co.data[,1] == "") > 0 | sum(is.na(co.data[,c(2,3)])) > 0) return("Please complete the table of historical trial data.")
    if(sum(co.data[,2] == "") > 0) return("Please complete the table of historical trial data.")
    if(sum(co.data[,3] == "") > 0) return("Please complete the table of historical trial data.")
    if(input$endpt == "Poisson") {
      if(sum(is.na(co.data[,4])) > 0) return("Please complete the table of historical trial data.")
    }
    
    if(length(unique(co.data[,1])) != nrow(co.data)) {
      output$unique_warn = renderText({"Each row should uniquely define one study."})
    } else{
      output$unique_warn = renderText({""})
    }
    
    if(sum(co.data[,2] <= 0) > 0) return("Your 2nd column should contain control arm sample sizes, which are positive integers.")  
    if(sum(co.data[,2]%%1 != 0) > 0) return("Your 2nd column should contain control arm sample sizes, which are positive integers.")  
    
    if(sum(!is.numeric(co.data[,2])) > 0) return("Your 2nd column should contain numbers.")
    if(sum(co.data[,3] < 0) > 0 & input$endpt == "Binary") return("Your 3rd column should contain # of events in control group, which are non-negative integers.")  
    if(input$endpt == "Binary" & sum(co.data[,3]%%1 != 0) > 0) return("Your 3rd column should contain control arm sample sizes, which are positive integers.")  
    
    if(sum(!is.numeric(co.data[,3])) > 0) return("Your 3rd column should contain numbers.")
    if(input$endpt == "Binary" & (sum(co.data[,2] < co.data[,3]) > 0)) return("Your number of events (r) cannot exceed your sample size (n).")
    if(input$endpt == "Poisson") {
      if(sum(!is.numeric(co.data[,4])) > 0) return("Your 4th column should contain numbers.")
      if(((sum(co.data[,3] < 0) > 0) | (sum(co.data[,4] <= 0) > 0))) return("Your total counts (y) must be non-negative and your trial durations (t) must be positive.")
    }
    
    if(sum(co.data[,2] >= 100000) > 0) {
      output$samp_size_warn = renderText({"MAP prior is slower to compute for large sample sizes..."})
    } else{
      output$samp_size_warn = renderText({""})
    }
    
    if(input$endpt == "Binary") {
      names(co.data) = c("study", "n", "r")
      base.MAP.mc = gMAP(cbind(r, n-r) ~ 1 | study, co.data, family = binomial, 
                         tau.dist = tau.dist, tau.prior = tau.prior, beta.prior = beta.prior)
    } else if(input$endpt == "Normal") {
      names(co.data) = c("study", "n", "m")
      m.se = input$sigma / sqrt(co.data$n)
      co.data = cbind(co.data, m.se)
      base.MAP.mc = gMAP(cbind(m, m.se) ~ 1 | study, family = gaussian, 
                         data = co.data, 
                         weights = n, beta.prior = beta.prior,
                         tau.dist = tau.dist, tau.prior = tau.prior)
    } else {
      names(co.data) = c("study", "n", "y", "t")
      base.MAP.mc = gMAP(y ~ 1 + offset(log(t*n)) | study, family = poisson, 
                         data = co.data, beta.prior = beta.prior,
                         tau.dist = tau.dist, tau.prior = tau.prior)
    }
    return(base.MAP.mc)
  }
  
  get.params = function() {
    if(input$hist_data_option == "Manual") {
      co.data = which_val()$mat
    } else{
      co.data = meta.data()
    }
    
    co.data[,2] = as.numeric(co.data[,2])
    co.data[,3] = as.numeric(co.data[,3])
    if(input$endpt == "Poisson") co.data[,4] = as.numeric(co.data[,4])
    
    if(input$beta_choice == "Default") {
      if(input$endpt == "Binary") {
        beta.prior = 2 
      } else if(input$endpt == "Normal") {
        beta.prior = 100*input$sigma
      } else{
        beta.prior =  sd(log(co.data[,3] + 0.5 + log(co.data[,2]*co.data[,4])))
      }
    } else{
      beta.prior = cbind(input$beta_mu, input$beta_sigma)
    }
    
    tau.dist = input$tau_dist
    
    if(tau.dist == "HalfNormal") tau.prior = cbind(0, input$tau_arg1_HN)
    if(tau.dist == "TruncNormal") tau.prior = cbind(input$tau_arg1_TN, input$tau_arg2_TN)
    if(tau.dist == "Uniform") tau.prior = cbind(input$tau_arg1_Uni, input$tau_arg2_Uni)
    if(tau.dist == "Gamma") tau.prior = cbind(input$tau_arg1_G, input$tau_arg2_G)
    if(tau.dist == "InvGamma") tau.prior = cbind(input$tau_arg1_IG, input$tau_arg2_IG)
    if(tau.dist == "LogNormal") tau.prior = cbind(input$tau_arg1_LN, input$tau_arg2_LN)
    if(tau.dist == "TruncCauchy") tau.prior = cbind(input$tau_arg1_TC, input$tau_arg2_TC)
    if(tau.dist == "Exp") tau.prior = input$tau_arg_exp
    if(tau.dist == "Fixed") tau.prior = input$tau_fixed
    
    return(list(co.data = co.data, beta.prior = beta.prior, tau.prior = tau.prior, tau.dist = tau.dist))
  }
  
  get.gMAP.mem = memoise(get.gMAP)
  robustify.mem = memoise(robustify)
  
  # Function to make forest plot.
  makeForest = function() {
    
    if(!input$use_hist) return("Be sure to check the box to use historical meta-data.")
    
    error_check = errCheck_MAP()
    if(!is.null(error_check)) return(error_check)
    
    withProgress(message = "Computing MAP prior...", value = 0.5, {
      params = get.params()
      base.MAP.mc = get.gMAP.mem(params$co.data, params$beta.prior, params$tau.prior, params$tau.dist)
      MAP_summ = summary(base.MAP.mc)$theta.pred
      rownames(MAP_summ) = "Posterior summary of MAP prior"
      
      incProgress(amount = 0.25)
      setProgress(message = "Computing parametric approximation...")
      base.MAP = automixfit(base.MAP.mc) 
      MAP.robust = robustify.mem(base.MAP, weight = input$wt)
      ESS = ess(base.MAP, "elir")
      ESS.rob = ess(MAP.robust, "elir")
    }) 
    return(list(base.MAP.mc = base.MAP.mc, MAP_summ = MAP_summ, ESS = ESS, ESS.rob = ESS.rob)) 
  } # close makeForest() function
  
  # Call make forest function triggered by action button
  observeEvent(input$make.forest, {
    output$forest = renderPlot({})
    
    func.output = isolate(makeForest())
    if(class(func.output)[1] %in% c("character", "html")) {
      output$plot_err = renderText({func.output})
    } else{
      output$forest = renderPlot({
        plot(func.output$base.MAP.mc)$forest + theme(legend.position = "right") + 
          labs(title=expression("Posterior medians of "*theta[1]*", ..., "*theta[H]*", "*theta*"*"*" with 95% credible intervals"))
      })
      output$plot_err = renderText({""})
      output$MAP_summ = renderTable({func.output$MAP_summ},
                                    rownames = TRUE, digits = 3)
      output$ESS = renderText({
        paste0("The effective sample size of the MAP prior is ", round(func.output$ESS), ".")
      })
      
      output$ESS.rob = renderText({
        paste0("The effective sample size of the MAP prior with robustification is ", round(func.output$ESS.rob), ".")
      })
    }
  }, ignoreInit = TRUE)
  
  # Function to compute PoS 
  getPoS = function() {
    error_check = errCheck_PoS(input$use_hist)
    if(!is.null(error_check)) return(error_check)
    
    withProgress(message = "Computing MAP prior...", value = 0.5, {
      
      set.seed(input$seed)
      
      if(input$use_hist) {
        params = get.params()
        base.MAP.mc = get.gMAP.mem(params$co.data, params$beta.prior, params$tau.prior, params$tau.dist)
        incProgress(amount = 0.25)
        setProgress(message = "Computing parametric approximation...")
        
        base.MAP = automixfit(base.MAP.mc) 
        
        incProgress(amount = 0.2)
        setProgress(message = "Robustifying prior...")
        
        # Come up with mean for robust component of MAP prior
        # If identity link, can use null hypothesized value
        # If log or logit link, robustify.mem() can't handle negative values
        if(input$wt > 0 & input$wt < 1) {
          if(input$endpt == "Binary") {
            robust.mean = ifelse(input$samp == "One", input$p0, 0.5)
          } else if(input$endpt == "Normal") {
            robust.mean = ifelse(input$samp == "One", input$mu0, input$diff0_norm)
          } else{
            if(input$samp == "One") robust.mean = input$lambda0
          }
          if(input$endpt != "Poisson" | (input$endpt == "Poisson" & input$samp == "One")) {
            MAP.robust = robustify.mem(base.MAP, weight = input$wt, mean = robust.mean)
          } else{
            MAP.robust = robustify.mem(base.MAP, weight = input$wt)
          }
        }
        else if(input$wt == 0) {
          MAP.robust = base.MAP
        } else {
          if(input$endpt == "Binary") {
            MAP.robust = mixbeta(c(1,1,1))
          } else if(input$endpt == "Normal") {
            MAP.robust = mixnorm(c(1, robust.mean, 100))
          } else{
            MAP.robust = mixgamma(c(1, 0.001, 0.001))
          }
        }
      }
      
      if(input$hist_data_option == "Manual") {
        co.data = data.frame(which_val()$mat)
      } else{
        co.data = data.frame(meta.data())
      }
      
      co.data[,2] = as.numeric(co.data[,2])
      co.data[,3] = as.numeric(co.data[,3])
      names(co.data)[1:2] = c("study", "n")
      if(input$endpt == "Binary") names(co.data)[3] = "r"
      if(input$endpt == "Normal") names(co.data)[3] = "m"
      if(input$endpt == "Poisson") {
        co.data[,4] = as.numeric(co.data[,4])
        names(co.data)[3:4] = c("y", "t")
      }
      if(input$beta_choice == "Default") {
        if(input$endpt == "Binary") {
          beta.prior = 2 
        } else if(input$endpt == "Normal") {
          beta.prior = 100*input$sigma
        } else{
          beta.prior =  sd(log(co.data[,3] + 0.5 + log(co.data[,2]*co.data[,4])))
        }
      } else{
        beta.prior = cbind(input$beta_mu, input$beta_sigma)
      }
      
      if(input$tau_dist == "HalfNormal") tau.prior = cbind(0, input$tau_arg1_HN)
      if(input$tau_dist == "TruncNormal") tau.prior = cbind(input$tau_arg1_TN, input$tau_arg2_TN)
      if(input$tau_dist == "Uniform") tau.prior = cbind(input$tau_arg1_Uni, input$tau_arg2_Uni)
      if(input$tau_dist == "Gamma") tau.prior = cbind(input$tau_arg1_G, input$tau_arg2_G)
      if(input$tau_dist == "InvGamma") tau.prior = cbind(input$tau_arg1_IG, input$tau_arg2_IG)
      if(input$tau_dist == "LogNormal") tau.prior = cbind(input$tau_arg1_LN, input$tau_arg2_LN)
      if(input$tau_dist == "TruncCauchy") tau.prior = cbind(input$tau_arg1_TC, input$tau_arg2_TC)
      if(input$tau_dist == "Exp") tau.prior = input$tau_arg_exp
      if(input$tau_dist == "Fixed") tau.prior = input$tau_fixed
      
      
      incProgress(amount = 0.05)
      setProgress(message = "Computing PoS...")
      
      # Derive decision rule.
      samp = input$samp
      if(input$endpt == "Binary") {
        qc1 = ifelse(samp == "One", input$p0, input$diff0_bin)
      } else if(input$endpt == "Normal") {
        qc1 = ifelse(samp == "One", input$mu0, input$diff0_norm)
      } else{
        qc1 = ifelse(samp == "One", input$lambda0, input$diff0_pois)
      }
      
      pc2 = qc2 = NULL
      if(input$ncrit == "Two") {
        if(input$endpt == "Binary") qc2 = input$qc2_bin
        if(input$endpt == "Normal") qc2 = input$qc2_norm
        if(input$endpt == "Poisson") qc2 = input$qc2_pois
        if(samp == "Two") qc2 = input$qc2
        
        pc2 = input$pc2
      }
      
      lower.tail = ifelse(input$sidedness == "Lower", TRUE, FALSE)
      
      link = "identity"
      if(samp == "Two" & input$endpt != "Normal") {
        link = input$link
      }
      
      if(samp == "One") {
        decision = decision1S(pc = c(input$pc1, pc2), qc = c(qc1, qc2), lower.tail = lower.tail)
      } else{
        decision = decision2S(pc = c(input$pc1, pc2), qc = c(qc1, qc2), lower.tail = lower.tail, link = link)
      }
      
      if(TRUE) {
        if(samp == "One" ) {
          new.data = which_val1()$mat
        } else{
          new.data = which_val3()$mat
          n.ctrl = new.data[2,2]
          if(input$endpt == "Binary") r.ctrl = new.data[2,3]
          if(input$endpt == "Normal") m.ctrl = new.data[2,3]
          if(input$endpt == "Poisson") y.ctrl = new.data[2,3]
          n.ctrl.target = new.data[2,4]
        } 
        
        n.new = new.data[1,2]
        n.new.target = new.data[1,4]
        if(input$endpt == "Binary") r.new = new.data[1,3]
        if(input$endpt == "Normal") m.new = new.data[1,3]
        if(input$endpt == "Poisson") {
          y.new = new.data[1,3]
          t.new = new.data[1,5]
        }
      }
      
      if(input$endpt == "Binary") {
        
        treat.prior = mixbeta(c(1, 1, 1))
        
        if(samp == "One") {
          interim = postmix(treat.prior, n = n.new, r = r.new)
          interim.PoS = pos1S(interim, n = n.new.target - n.new, decision = decision)
          if(input$use_hist) {
            interim.combined = postmix(MAP.robust, n = n.new, r = r.new)
          } else{
            interim.combined = interim
          }
          PoS = round(interim.PoS(interim.combined), 3)
        } else {
          interim.trt = postmix(treat.prior, n = n.new, r = r.new)
          if(input$use_hist) {
            interim.ctrl = postmix(MAP.robust, n = n.ctrl, r = r.ctrl)
          } else{
            interim.ctrl = postmix(treat.prior, n = n.ctrl, r = r.ctrl)
          }
          interim.PoS = pos2S(interim.ctrl, interim.trt, n1 = n.ctrl.target - n.ctrl,
                              n2 = n.new.target - n.new, decision = decision)
          PoS = round(interim.PoS(interim.ctrl, interim.trt), 3)
        }
        
        
        
      } else if(input$endpt == "Normal"){
        
        treat.prior = mixnorm(c(1, 0, 100), sigma = input$sigma)
        
        if(samp == "One") {
          interim = postmix(treat.prior, n = n.new, m = m.new)
          interim.PoS = pos1S(interim, n = n.new.target - n.new, decision = decision)
          if(input$use_hist) {
            interim.combined = postmix(MAP.robust, n = n.new, m = m.new)
          } else{
            interim.combined = interim
          }
          PoS = round(interim.PoS(interim.combined), 3)
        } else {
          if(input$use_hist) {
            interim.ctrl = postmix(MAP.robust, n = n.ctrl, m = m.ctrl)
          } else{
            interim.ctrl = postmix(treat.prior, n = n.ctrl, m = m.ctrl)
          }
          interim.trt = postmix(treat.prior, n = n.new, m = m.new)
          interim.PoS = pos2S(interim.ctrl, interim.trt, n1 = n.ctrl.target - n.ctrl,
                              n2 =  n.new.target - n.new, decision = decision)
          PoS = round(interim.PoS(interim.ctrl, interim.trt), 3)
        }
        
      } else { # Poisson
        
        treat.prior = mixgamma(c(1, 0.001, 0.001))
        
        if(input$samp == "One") {
          interim = postmix(treat.prior, n = n.new*t.new, m = y.new/n.new/t.new)
          interim.PoS = pos1S(interim, n = n.new.target - n.new, decision = decision)
          if(input$use_hist) {
            interim.combined = postmix(MAP.robust, n = n.new*t.new, m = y.new/n.new/t.new)
          } else{
            interim.combined = interim
          }
          PoS = round(interim.PoS(interim.combined), 3)
        } else {
          interim.trt = postmix(treat.prior, n = n.new*t.new, m = y.new/n.new/t.new)
          if(input$use_hist) {
            interim.ctrl = postmix(MAP.robust, n = n.ctrl*t.new, m = y.ctrl/n.ctrl/t.new)
          } else{
            interim.ctrl = postmix(treat.prior, n = n.ctrl*t.new, m = y.ctrl/n.ctrl/t.new)
          }
          interim.PoS = pos2S(interim.ctrl, interim.trt, n.ctrl.target - n.ctrl,
                              n.new.target - n.new, decision = decision)
          PoS = round(interim.PoS(interim.ctrl, interim.trt), 3)
        }
      }
      
      return(PoS)
    })
  }
  
  # Function to compute PoS of concurrent trials 
  getPoS2 = function() {
    error_check = errCheck_PoS(input$use_hist)
    if(!is.null(error_check)) return(error_check)
    
    withProgress(message = "Computing MAP prior...", value = 0.5, {
      
      set.seed(input$seed)
      
      if(input$use_hist) {
        
        params = get.params()
        base.MAP.mc = get.gMAP.mem(params$co.data, params$beta.prior, params$tau.prior, params$tau.dist)
        incProgress(amount = 0.25)
        setProgress(message = "Computing parametric approximation...")
        
        base.MAP = automixfit(base.MAP.mc) 
        
        incProgress(amount = 0.2)
        setProgress(message = "Robustifying prior...")
        
        # Come up with mean for robust component of MAP prior
        # If identity link, can use null hypothesized value
        # If log or logit link, robustify.mem() can't handle negative values
        if(input$wt > 0 & input$wt < 1) {
          if(input$endpt == "Binary") {
            robust.mean = ifelse(input$samp == "One", input$p0, 0.5)
          } else if(input$endpt == "Normal") {
            robust.mean = ifelse(input$samp == "One", input$mu0, input$diff0_norm)
          } else{
            if(input$samp == "One") robust.mean = input$lambda0
          }
          if(input$endpt != "Poisson" | (input$endpt == "Poisson" & input$samp == "One")) {
            MAP.robust = robustify.mem(base.MAP, weight = input$wt, mean = robust.mean)
          } else{
            MAP.robust = robustify.mem(base.MAP, weight = input$wt)
          }
        }
        else if(input$wt == 0) {
          MAP.robust = base.MAP
        } else {
          if(input$endpt == "Binary") {
            MAP.robust = mixbeta(c(1,1,1))
          } else if(input$endpt == "Normal") {
            MAP.robust = mixnorm(c(1, robust.mean, 100))
          } else{
            MAP.robust = mixgamma(c(1, 0.001, 0.001))
          }
        }
        
        if(input$hist_data_option == "Manual") {
          co.data = data.frame(which_val()$mat)
        } else{
          co.data = data.frame(meta.data())
        }
        
        co.data[,2] = as.numeric(co.data[,2])
        co.data[,3] = as.numeric(co.data[,3])
        names(co.data)[1:2] = c("study", "n")
        if(input$endpt == "Binary") names(co.data)[3] = "r"
        if(input$endpt == "Normal") names(co.data)[3] = "m"
        if(input$endpt == "Poisson") {
          co.data[,4] = as.numeric(co.data[,4])
          names(co.data)[3:4] = c("y", "t")
        }
        if(input$beta_choice == "Default") {
          if(input$endpt == "Binary") {
            beta.prior = 2 
          } else if(input$endpt == "Normal") {
            beta.prior = 100*input$sigma
          } else{
            beta.prior =  sd(log(co.data[,3] + 0.5 + log(co.data[,2]*co.data[,4])))
          }
        } else{
          beta.prior = cbind(input$beta_mu, input$beta_sigma)
        }
        
        if(input$tau_dist == "HalfNormal") tau.prior = cbind(0, input$tau_arg1_HN)
        if(input$tau_dist == "TruncNormal") tau.prior = cbind(input$tau_arg1_TN, input$tau_arg2_TN)
        if(input$tau_dist == "Uniform") tau.prior = cbind(input$tau_arg1_Uni, input$tau_arg2_Uni)
        if(input$tau_dist == "Gamma") tau.prior = cbind(input$tau_arg1_G, input$tau_arg2_G)
        if(input$tau_dist == "InvGamma") tau.prior = cbind(input$tau_arg1_IG, input$tau_arg2_IG)
        if(input$tau_dist == "LogNormal") tau.prior = cbind(input$tau_arg1_LN, input$tau_arg2_LN)
        if(input$tau_dist == "TruncCauchy") tau.prior = cbind(input$tau_arg1_TC, input$tau_arg2_TC)
        if(input$tau_dist == "Exp") tau.prior = input$tau_arg_exp
        if(input$tau_dist == "Fixed") tau.prior = input$tau_fixed
        
        
      }
      
      incProgress(amount = 0.05)
      setProgress(message = "Computing PoS...")
      
      # Derive decision rule.
      pc2 = qc2 = NULL
      if(input$ncrit == "Two") {
        if(input$endpt == "Binary") qc2 = input$qc2_bin
        if(input$endpt == "Normal") qc2 = input$qc2_norm
        if(input$endpt == "Poisson") qc2 = input$qc2_pois
        if(input$samp == "Two") qc2 = input$qc2
        
        pc2 = input$pc2
      }
      
      samp = input$samp
      if(input$endpt == "Binary") {
        qc1 = ifelse(samp == "One", input$p0, input$diff0_bin)
      } else if(input$endpt == "Normal") {
        qc1 = ifelse(samp == "One", input$mu0, input$diff0_norm)
      } else{
        qc1 = ifelse(samp == "One", input$lambda0, input$diff0_pois)
      }
      
      lower.tail = ifelse(input$sidedness == "Lower", TRUE, FALSE)
      
      link = "identity"
      if(samp == "Two" & input$endpt != "Normal") link = input$link
      
      if(samp == "One") {
        decision = decision1S(pc = c(input$pc1, pc2), qc = c(qc1, qc2), lower.tail = lower.tail)
      } else{
        decision = decision2S(pc = c(input$pc1, pc2), qc = c(qc1, qc2), lower.tail = lower.tail, link = link)
      }
      
      if(samp == "One") {
        new.data = which_val2()$mat
        n.newB = new.data[2,2]
        n.new.targetB = new.data[2,4]
        if(input$endpt == "Binary") r.newB = new.data[2,3]
        if(input$endpt == "Normal") m.newB = new.data[2,3]
        if(input$endpt == "Poisson") {
          y.newB = new.data[2,3]; t.newB = new.data[2,5]
        }
      } else{
        new.data = which_val4()$mat
        n.ctrl = new.data[2,2]
        n.newB = new.data[3,2]
        n.ctrlB = new.data[4,2]
        n.ctrl.target = new.data[2,4]
        n.new.targetB = new.data[3,4]
        n.ctrl.targetB = new.data[4,4]
        if(input$endpt == "Binary") {
          r.ctrl = new.data[2,3]
          r.newB = new.data[3,3]
          r.ctrlB = new.data[4,3]
        } else if(input$endpt == "Normal") {
          m.ctrl = new.data[2,3]
          m.newB = new.data[3,3]
          m.ctrlB = new.data[4,3]
        } else{
          y.ctrl = new.data[2,3]
          y.newB = new.data[3,3]
          y.ctrlB = new.data[4,3]
          n.new.targetB = new.data[3,4];
          t.newB = new.data[3,5]
        }
      }
      
      n.new = new.data[1,2]
      n.new.target = new.data[1,4]
      if(input$endpt == "Binary") r.new = new.data[1,3]
      if(input$endpt == "Normal") m.new = new.data[1,3]
      if(input$endpt == "Poisson") {
        y.new = new.data[1,3]; t.new = new.data[1,5]
      }
      
      
      # Calculate PoS stuff here
      if(input$endpt == "Binary") {
        
        treat.prior = mixbeta(c(1, 1, 1))
        
        if(samp == "One") {
          if(input$use_hist) {
            # Update priors
            interim = postmix(MAP.robust, n = n.new, r = r.new)
            interim.B = postmix(MAP.robust, n = n.newB, r = r.newB)
            
            names(new.data)[1:3] = names(co.data)
            co.data2 = data.frame(rbind(co.data, new.data[,1:3]))
            
            interim.MAP.mc = update(base.MAP.mc, data = co.data2, beta.prior = beta.prior, 
                                    tau.dist = input$tau_dist, tau.prior = tau.prior)  
            
            # Extract MCMC samples of theta_1, ..., theta_H, theta_A, theta_B
            interim.MAP.post = as.matrix(interim.MAP.mc)[,1:nrow(co.data2)]
            
            # Theta on logit scale; back transform to [0,1] with expit
            interim.MAP.post_expit = 1 / (1 + exp(-interim.MAP.post))
            interimA.allcombined = automixfit(interim.MAP.post_expit[,nrow(co.data2)-1], type = "beta")
            interimB.allcombined = automixfit(interim.MAP.post_expit[,nrow(co.data2)], type = "beta")
            
          } else{
            interim = postmix(treat.prior, n = n.new, r = r.new)
            interim.B = postmix(treat.prior, n = n.newB, r = r.newB)
            
            interimA.allcombined = interim
            interimB.allcombined = interim.B
          }
          
          # Compute PoS, Trial A
          interim.PoS = pos1S(prior = interim, n = n.new.target - n.new, decision = decision)
          PoS.A = interim.PoS(interimA.allcombined)
          
          # Compute PoS, Trial B
          interim.PoS.B = pos1S(prior = interim.B, n = n.new.targetB - n.newB, decision = decision)
          PoS.B = interim.PoS.B(interimB.allcombined)
          
          # Probability both trials successful
          interim.oc.A = oc1S(prior = interim, n = n.new.target - n.new, decision = decision)
          interim.oc.B = oc1S(prior = interim.B, n = n.new.targetB - n.newB, decision = decision)
          if(input$use_hist) {
            PoS.both = mean(interim.oc.A(interim.MAP.post_expit[,nrow(co.data2)-1])*interim.oc.B(interim.MAP.post_expit[,nrow(co.data2)]))
          } else{
            PoS.both = PoS.A*PoS.B
          }
          
        } else { # Binary , two-sample
          # Update treatment effect priors.
          interim = postmix(treat.prior, n = n.new, r = r.new)
          interim.B = postmix(treat.prior, n = n.newB, r = r.newB)
          
          if(input$use_hist) {
            # Update control priors
            interim.ctrl = postmix(MAP.robust, n = n.ctrl, r = r.ctrl)
            interim.ctrlB = postmix(MAP.robust, n = n.ctrlB, r = r.ctrlB)
            
            names(new.data)[1:3] = names(co.data)
            
            # Update prior for Trial A with Trial B information
            co.data2 = data.frame(rbind(co.data, new.data[,1:3]))
            
            interim.MAP.mc = update(base.MAP.mc, data = co.data2, beta.prior = beta.prior, 
                                    tau.dist = input$tau_dist, tau.prior = tau.prior, family = binomial)  
            
            # Extract MCMC samples of theta_1, ..., theta_H, theta_B
            interim.MAP.post = as.matrix(interim.MAP.mc)[,1:nrow(co.data2)]
            
            # Theta on logit scale; back transform to [0,1] with expit
            interim.MAP.post_expit = 1 / (1 + exp(-interim.MAP.post))
            interimA.allcombined = automixfit(interim.MAP.post_expit[,nrow(co.data2)-2], type = "beta")
            interimB.allcombined = automixfit(interim.MAP.post_expit[,nrow(co.data2)], type = "beta")
            
          } else{
            interimA.allcombined = postmix(treat.prior, n = n.ctrl, r = r.ctrl)
            interimB.allcombined = postmix(treat.prior, n = n.ctrlB, r = r.ctrlB)
          }
          
          # Compute PoS, Trial A
          interim.PoS = pos2S(interimA.allcombined, interim, n.ctrl.target - n.ctrl, n.new.target - n.new, decision = decision)
          PoS.A = interim.PoS(interimA.allcombined, interim)
          
          
          # Compute PoS, Trial B
          interim.PoS.B = pos2S(interimB.allcombined, interim.B, n.ctrl.targetB - n.ctrlB, n.new.targetB - n.newB,  decision = decision)
          PoS.B = interim.PoS.B(interimB.allcombined, interim.B)
          
          # Probability both trials successful
          interim.oc.A = oc2S(interimA.allcombined, interim, n.ctrl.target - n.ctrl, n.new.target - n.new, decision = decision)
          interim.oc.B = oc2S(interimB.allcombined, interim.B, n.ctrl.targetB - n.ctrlB, n.new.targetB - n.newB, decision = decision)
          
          PoS.A.OC = interim.oc.A(interim.MAP.post_expit[,nrow(co.data2)-2], interim.MAP.post_expit[,nrow(co.data2)-3])
          
          if(is.na(PoS.A)) {
            PoS.A = mean(PoS.A.OC)
          }
          
          PoS.B.OC = interim.oc.B(interim.MAP.post_expit[,nrow(co.data2)], interim.MAP.post_expit[,nrow(co.data2)-1])
          
          if(is.na(PoS.B)) {
            PoS.B = mean(PoS.B.OC)
          }
          
          if(input$use_hist) {
            PoS.both = mean(PoS.A.OC*PoS.B.OC)
          } else{
            PoS.both = PoS.A*PoS.B
          }
        }
      } else if(input$endpt == "Normal"){
        
        treat.prior = mixnorm(c(1, 0, 100), sigma = input$sigma)
        
        if(samp == "One") {
          if(input$use_hist) {
            # Update priors
            interim = postmix(MAP.robust, n = n.new, m = m.new)
            interim.B = postmix(MAP.robust, n = n.newB, m = m.newB)
            
            new.data.trim = new.data[,1:3]
            names(new.data.trim) = names(co.data)
            co.data2 = data.frame(rbind(co.data, new.data.trim))
            m.se = input$sigma / sqrt(co.data2$n)
            co.data2 = cbind(co.data2, m.se)
            names(co.data2) = c("study", "n", "m", "m.se")
            interim.MAP.mc = update(base.MAP.mc, data = co.data2, beta.prior = beta.prior, 
                                    tau.dist = input$tau_dist, tau.prior = tau.prior)
            
            # Extract MCMC samples of theta_1, ..., theta_H, theta_A, theta_B
            interim.MAP.post = as.matrix(interim.MAP.mc)[,1:nrow(co.data2)]
            interimA.allcombined = automixfit(interim.MAP.post[,nrow(co.data2)-1], type = "norm")
            interimB.allcombined = automixfit(interim.MAP.post[,nrow(co.data2)], type = "norm")
            
          } else{
            interim = postmix(treat.prior, n = n.new, m = m.new)
            interim.B = postmix(treat.prior, n = n.newB, m = m.newB)
            
            interimA.allcombined = interim
            interimB.allcombined = interim.B
          }
          
          # Compute PoS, Trial A
          interim.PoS = pos1S(prior = interim, n = n.new.target - n.new, decision = decision)
          PoS.A = interim.PoS(interimA.allcombined)
          
          # Compute PoS, Trial B
          interim.PoS.B = pos1S(prior = interim.B, n = n.new.targetB - n.newB, decision = decision)
          PoS.B = interim.PoS.B(interimB.allcombined)
          
          # Probability both trials successful
          interim.oc.A = oc1S(prior = interim, n = n.new.target - n.new, decision = decision)
          interim.oc.B = oc1S(prior = interim.B, n = n.new.targetB - n.newB, decision = decision)
          if(input$use_hist) {
            PoS.both = mean(interim.oc.A(interim.MAP.post[,nrow(co.data2)-1])*interim.oc.B(interim.MAP.post[,nrow(co.data2)]))
          } else{
            PoS.both = PoS.A*PoS.B
          }
          
        } else { # Normal, two-sample
          interim = postmix(treat.prior, n = n.new, m = m.new)
          interim.B = postmix(treat.prior, n = n.newB, m = m.newB)
          if(input$use_hist) {
            # Update control priors
            interim.ctrl = postmix(MAP.robust, n = n.ctrl, m = m.ctrl)
            interim.ctrlB = postmix(MAP.robust, n = n.ctrlB, m = m.ctrlB)
            
            new.data.trim = new.data[,1:3]
            names(new.data.trim) = names(co.data)
            co.data2 = data.frame(rbind(co.data, new.data.trim))
            m.se = input$sigma / sqrt(co.data2$n)
            co.data2 = cbind(co.data2, m.se)
            names(co.data2) = c("study", "n", "m", "m.se")
            
            interim.MAP.mc = update(base.MAP.mc, data = co.data2, beta.prior = beta.prior, 
                                    tau.dist = input$tau_dist, tau.prior = tau.prior)  
            
            # Extract MCMC samples of theta_1, ..., theta_H, theta_B
            interim.MAP.post = as.matrix(interim.MAP.mc)[,1:nrow(co.data2)]
            
            # Theta on logit scale; back transform to [0,1] with expit
            interimA.allcombined = automixfit(interim.MAP.post[,nrow(co.data2)-2], type = "norm")
            interimB.allcombined = automixfit(interim.MAP.post[,nrow(co.data2)], type = "norm")
            
          } else{
            interimA.allcombined = postmix(treat.prior, n = n.ctrl, m = m.ctrl)
            interimB.allcombined = postmix(treat.prior, n = n.ctrlB, m = m.ctrlB)
          }
          
          # Compute PoS, Trial A
          interim.PoS = pos2S(interimA.allcombined, interim, n.ctrl.target - n.ctrl, n.new.target - n.new, decision = decision, sigma1 = input$sigma, sigma2 = input$sigma)
          PoS.A = interim.PoS(interimA.allcombined, interim)
          
          # Compute PoS, Trial B
          interim.PoS.B = pos2S(interimB.allcombined, interim.B, n.ctrl.targetB - n.ctrlB, n.new.targetB - n.newB, decision = decision, sigma1 = input$sigma, sigma2 = input$sigma)
          PoS.B = interim.PoS.B(interimB.allcombined, interim.B)
          
          # Probability both trials successful
          interim.oc.A = oc2S(interimA.allcombined, interim, n.ctrl.target - n.ctrl, n.new.target - n.new, decision = decision, sigma1 = input$sigma, sigma2 = input$sigma)
          interim.oc.B = oc2S(interimB.allcombined, interim.B, n.ctrl.targetB - n.ctrlB, n.new.targetB - n.newB, decision = decision, sigma1 = input$sigma, sigma2 = input$sigma)
          if(input$use_hist) {
            PoS.both = mean(interim.oc.A(interim.MAP.post[,nrow(co.data2)-2], interim.MAP.post[,nrow(co.data2)-3])*interim.oc.B(interim.MAP.post[,nrow(co.data2)], interim.MAP.post[,nrow(co.data2)-1]))
          } else{
            PoS.both = PoS.A*PoS.B
          }
          
        }
      } else { # Poisson
        
        treat.prior = mixgamma(c(1, 0.001, 0.001))
        
        if(samp == "One") {
          if(input$use_hist) {
            # Update priors
            interim = postmix(MAP.robust, n = n.new*t.new, m = y.new/n.new/t.new)
            interim.B = postmix(MAP.robust, n = n.newB*t.newB, m = y.newB/n.newB/t.newB)
            
            new.data.trim = new.data[,c(1:3, 5)]
            names(new.data.trim) = names(co.data)
            co.data2 = data.frame(rbind(co.data, new.data.trim))
            names(co.data2) = c("study", "n", "y", "t")
            interim.MAP.mc = update(base.MAP.mc, data = co.data2, beta.prior = beta.prior, 
                                    tau.dist = input$tau_dist, tau.prior = tau.prior,
                                    family = poisson)
            
            # Extract MCMC samples of theta_1, ..., theta_H, theta_A, theta_B
            interim.MAP.post = exp(as.matrix(interim.MAP.mc)[,1:nrow(co.data2)])
            interimA.allcombined = automixfit(interim.MAP.post[,nrow(co.data2)-1], type = "gamma")
            interimB.allcombined = automixfit(interim.MAP.post[,nrow(co.data2)], type = "gamma")
            
          } else{ 
            interim = postmix(treat.prior, n = n.new*t.new, m = y.new/n.new/t.new)
            interim.B = postmix(treat.prior, n = n.newB*t.newB, m = y.newB/n.newB/t.newB)
            
            interimA.allcombined = interim
            interimB.allcombined = interim.B
          }
          
          likelihood(interim) = likelihood(interim.B) = likelihood(interimA.allcombined) = likelihood(interimB.allcombined) = "poisson"
          
          # Compute PoS, Trial A
          interim.PoS = pos1S(prior = interimA.allcombined, n = n.new.target - n.new, decision = decision)
          PoS.A = interim.PoS(interimA.allcombined)
          
          # Compute PoS, Trial B
          interim.PoS.B = pos1S(prior = interimB.allcombined, n = n.new.targetB - n.newB, decision = decision)
          PoS.B = interim.PoS.B(interimB.allcombined)
          # Probability both trials successful
          if(input$use_hist) {
            interim.oc.A = oc1S(prior = interim, n = n.new.target - n.new, decision = decision)
            interim.oc.B = oc1S(prior = interim.B, n = n.new.targetB - n.newB, decision = decision)
            PoS.both = mean(interim.oc.A(interim.MAP.post[,nrow(co.data2)-1])*interim.oc.B(interim.MAP.post[,nrow(co.data2)]))
          } else{
            PoS.both = PoS.A*PoS.B
          }
          
        } else { # Two sample Poisson
          interim = postmix(treat.prior, n = n.new*t.new, m = y.new/n.new/t.new)
          interim.B = postmix(treat.prior, n = n.newB*t.newB, m = y.newB/n.newB/t.newB)
          
          if(input$use_hist) {
            
            names(new.data)[c(1:3, 5)] = names(co.data)
            
            # Update prior for Trial A with Trial B information
            co.data2 = data.frame(rbind(co.data, new.data[,c(1:3, 5)]))
            
            interim.MAP.mc = update(base.MAP.mc, data = co.data2, beta.prior = beta.prior, 
                                    tau.dist = input$tau_dist, tau.prior = tau.prior)  
            
            # Extract MCMC samples of theta_1, ..., theta_H, theta_B
            interim.MAP.post = exp(as.matrix(interim.MAP.mc)[,1:nrow(co.data2)])
            
            newMAP.A = automixfit(interim.MAP.post[,nrow(co.data2)-2], type = "gamma")
            newMAP.B = automixfit(interim.MAP.post[,nrow(co.data2)], type = "gamma")
            
            # Update control priors
            likelihood(newMAP.A) = likelihood(newMAP.B) = "poisson"
            
            interimA.allcombined = postmix(newMAP.A, n = n.ctrl*t.new, m = y.ctrl/n.ctrl/t.new)
            interimB.allcombined = postmix(newMAP.B, n = n.ctrlB*t.newB, m = y.ctrlB/n.ctrlB/t.newB)
            
          } else{
            interimA.allcombined = postmix(treat.prior, n = n.ctrl*t.new, m = y.ctrl/n.ctrl/t.new)
            interimB.allcombined = postmix(treat.prior, n = n.ctrlB*t.newB, m = y.ctrlB/n.ctrlB/t.newB)
          }
          
          likelihood(interim) = likelihood(interim.B) = likelihood(interimA.allcombined) = likelihood(interimB.allcombined) = "poisson"
          
          # Compute PoS, Trial A
          interim.PoS = pos2S(interimA.allcombined, interim, n.ctrl.target - n.ctrl, n.new.target - n.new, decision = decision)
          PoS.A = interim.PoS(interimA.allcombined, interim)
          
          # Compute PoS, Trial B
          interim.PoS.B = pos2S(interimB.allcombined, interim.B, n.ctrl.targetB - n.ctrlB, n.new.targetB - n.newB, decision = decision)
          PoS.B = interim.PoS.B(interimB.allcombined, interim.B)
          
          
          # Probability both trials successful
          interim.oc.A = oc2S(interimA.allcombined, interim, n.ctrl.target - n.ctrl, n.new.target - n.new, decision = decision)
          interim.oc.B = oc2S(interimB.allcombined, interim.B, n.ctrl.targetB - n.ctrlB, n.new.targetB - n.newB, decision = decision)
          if(input$use_hist) {
            PoS.both = mean(interim.oc.A(interim.MAP.post[,nrow(co.data2)-2], interim.MAP.post[,nrow(co.data2)-3])*interim.oc.B(interim.MAP.post[,nrow(co.data2)], interim.MAP.post[,nrow(co.data2)-1]))
          } else{
            PoS.both = PoS.A*PoS.B
          }
          
        }
      }
      
      return(list(PoS.A = PoS.A, PoS.B = PoS.B, PoS.both = PoS.both))
    })
  }
  
  # Compute PoS triggered by action button.
  observeEvent(input$compute.pos, {
    if(!input$parallel) {
      func.output = isolate(getPoS())
    } else{
      func.output = isolate(getPoS2())
    }
    
    if(class(func.output)[1] %in% c("html", "character")) {
      string = func.output
    } else if(!input$parallel) {
      string = paste0("The probability of trial success is ", func.output, ".")
    } else{
      string = HTML(paste0("The probability of Trial A success is ",  paste0(round(func.output$PoS.A, 3)), "."), "</br>", 
                    paste0("The probability of Trial B success is ", paste0(round(func.output$PoS.B, 3)), "."), "</br>",
                    paste0("The probability that both trials are successful is ", paste0(round(func.output$PoS.both, 3), ".")))
    }
    
    output$results = renderUI({string})
  }, ignoreInit = TRUE)
  
  URL0 = a("Click here for a walkthrough of this Shiny app (Chrome or Firefox only).", href = "https://github.com/JamesNormington/PoS_Using_RBesT/blob/master/PoS%20App%20Walkthrough.pdf")
  output$link0 = renderUI({
    tagList(URL0)
  }) 
  
  URL = a("Click here for a walkthrough of the methodology and code (Chrome or Firefox only).", href = "https://github.com/JamesNormington/PoS_Using_RBesT/blob/master/PoS%20Code%20and%20Methodology%20Walkthrough.pdf")
  output$link = renderUI({
    tagList(URL)
  }) 
  
  URL2 = a("Click here for Sebastian Weber's 'Probability of Success with Co-Data' vignette.", href = "https://cran.r-project.org/web/packages/RBesT/vignettes/PoS_codata.html")
  output$link2 = renderUI({
    tagList(URL2)
  }) 
  
  URL3 = a("Click here for RBesT documentation on CRAN.", href = "https://cran.r-project.org/web/packages/RBesT/RBesT.pdf")
  output$link3 = renderUI({
    tagList(URL3)
  }) 
  
  URL4 = a("Click here for Sebastian Weber et al.'s RBesT paper on arXiv.", href = "https://arxiv.org/pdf/1907.00603.pdf")
  output$link4 = renderUI({
    tagList(URL4)
  }) 
  
  
} # close server() function

shinyApp(ui, server)