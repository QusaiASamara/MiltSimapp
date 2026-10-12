fluidPage(
  useShinyjs(),
  # Use a more modern theme with custom color palette
  theme = bslib::bs_theme(
    bootswatch = "flatly",
    primary = "#2C3E50",
    secondary = "#95a5a6",
    success = "#18BC9C",
    info = "#3498DB",
    warning = "#F39C12",
    danger = "#E74C3C"
  ),
  
  # Custom CSS for enhanced styling
  tags$head(
    tags$style(HTML("
      .app-header {
        background: linear-gradient(135deg, #2C3E50 0%, #3498DB 100%);
        padding: 1.5rem 0;
        margin-bottom: 2rem;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      .app-title {
        font-weight: 700; 
        letter-spacing: 0.5px;
        text-shadow: 1px 1px 3px rgba(0,0,0,0.2);
      }
      .card {
        border-radius: 8px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.05);
        transition: transform 0.2s, box-shadow 0.2s;
        overflow: hidden;
      }
      .card-body {
      overflow: visible !important;
      }

      .card:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 8px rgba(0,0,0,0.1);
      }
      .card-header {
        border-bottom: none;
        padding: 1rem 1.25rem;
      }
      .btn-primary, .btn-secondary, .btn-success {
        border-radius: 6px;
        font-weight: 500;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        transition: all 0.2s;
      }
      .btn-primary:hover, .btn-secondary:hover, .btn-success:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      .value-box {
        border-radius: 8px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.05);
      }
      .nav-pills .nav-link {
        border-radius: 6px;
        font-weight: 500;
        padding: 0.75rem 1.5rem;
      }
      .nav-pills .nav-link.active {
        background-color: #2C3E50;
      }
      .tab-content {
        padding-top: 1.5rem;
      }
      .form-control, .selectize-input {
        border-radius: 6px;
        border: 1px solid #ddd;
      }
      #covariate_panel {
        background-color: #f8f9fa;
        border-radius: 8px;
        padding: 15px;
        margin-top: 15px;
      }
      .spinner-grow {
        color: #3498DB !important;
      }
      .guide-page {
        max-width: 1240px;
        margin: 0 auto 3rem auto;
      }
      .guide-hero {
        background: linear-gradient(135deg, #f4f8fb 0%, #e8f2f8 100%);
        border: 1px solid #d8e6ef;
        border-radius: 10px;
        padding: 1.5rem 1.75rem;
        margin-bottom: 1.5rem;
      }
      .guide-section {
        margin-top: 2rem;
        margin-bottom: 2rem;
      }
      .guide-section h3 {
        color: #2C3E50;
        border-bottom: 2px solid #e9ecef;
        padding-bottom: 0.5rem;
        margin-bottom: 1rem;
      }
      .guide-callout {
        background: #f8f9fa;
        border-left: 5px solid #3498DB;
        border-radius: 6px;
        padding: 1rem 1.2rem;
        margin: 1rem 0;
      }
      .guide-callout-warning {
        background: #fff8e7;
        border-left-color: #F39C12;
      }
      .guide-equation {
        background: #f6f8fa;
        border: 1px solid #e1e4e8;
        border-radius: 6px;
        font-family: 'Courier New', monospace;
        font-size: 1.02rem;
        padding: 0.8rem 1rem;
        margin: 0.75rem 0;
        overflow-x: auto;
      }
      .guide-step {
        border: 1px solid #e5e9ec;
        border-radius: 8px;
        padding: 1rem;
        height: 100%;
        background: #fff;
      }
      .guide-step-number {
        display: inline-block;
        min-width: 2rem;
        height: 2rem;
        line-height: 2rem;
        text-align: center;
        border-radius: 50%;
        background: #2C3E50;
        color: white;
        font-weight: 700;
        margin-right: 0.5rem;
      }
      .guide-table-wrap {
        overflow-x: auto;
        margin: 1rem 0;
      }
      .guide-table-wrap table {
        min-width: 760px;
      }
      .guide-reference-list li {
        margin-bottom: 0.65rem;
      }
      details.guide-details {
        border: 1px solid #e1e5e8;
        border-radius: 7px;
        padding: 0.85rem 1rem;
        margin: 1rem 0;
        background: #fff;
      }
      details.guide-details summary {
        color: #2C3E50;
        cursor: pointer;
        font-weight: 600;
      }
    "))
  ),
  
  # Application Title with improved styling
  div(
    class = "app-header",
    div(
      class = "container-fluid",
      div(
        class = "row justify-content-center",
        div(
          class = "col-md-10",
          h1("Miltefosine Dose Optimization and Population Analysis", 
             class = "text-center text-white app-title m-0")
        )
      )
    )
  ),
  
  # Main navigation with larger, more distinct tabs
  tabsetPanel(
    id = "main_tabs",
    type = "pills",
    tabPanel(
      div(icon("users"), "Population Simulation"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          div(
            class = "card mb-4",
            div(
              class = "card-header bg-primary text-white",
              h4("Simulation Settings", class = "m-0")
            ),
            div(
              class = "card-body",
              # Population type selector with improved styling
              radioButtons("population_type", "Population Type", 
                           choices = c("Predefined Population" = "predefined", 
                                       "Import Population" = "import"),
                           inline = TRUE),
              hr(class = "my-3"),
              
              # Predefined population settings
              conditionalPanel(
                condition = "input.population_type == 'predefined'",
                selectizeInput("population", "Select Population",
                            choices = c("Pediatric Eastern African Virtual VL Population", 
                                        "WHO Virtual Population",
                                        "Adult Eastern African Virtual VL Population",
                                        "Female Eastern African Virtual VL Population"), 
                            width = "100%"),
                hr(class = "my-3"),
                
                conditionalPanel(
                  condition = "input.population == 'Pediatric Eastern African Virtual VL Population' || input.population == 'WHO Virtual Population'",
                  div(class = "form-group",
                      radioGroupButtons(
                        inputId = "age_unit",
                        label = "Age Unit",
                        choices = c("years", "months"),
                        justified = TRUE,
                        status = "primary",
                        checkIcon = list(yes = icon("check-circle"))
                      )
                  )
                ),
                
                conditionalPanel(
                  condition = "input.population == 'Pediatric Eastern African Virtual VL Population' || input.population == 'WHO Virtual Population'",
                  numericInput("subj_num", "Individuals per Age Unit", 
                               value = 10, min = 1, step = 1, width = "100%"),
                  hr(class = "my-3")
                ),
                
                conditionalPanel(
                  condition = "input.population == 'Pediatric Eastern African Virtual VL Population' || input.population == 'WHO Virtual Population'",
                  div(class = "d-flex align-items-center",
                      materialSwitch("use_seed", "Reproduce Population", 
                                     value = FALSE, status = "primary"),
                  ),
                  
                  conditionalPanel(
                    condition = "input.use_seed == true && (input.population == 'Pediatric Eastern African Virtual VL Population' || input.population == 'WHO Virtual Population')",
                    div(class = "mt-3",
                        numericInput("seed_value", "Seed Value", value = 9119, 
                                     min = 1, step = 1, width = "100%")
                    )
                  )
                ),
                
                div(class = "mt-4",
                    actionButton("show_inputs", "Customize Covariates", 
                                 icon = icon("sliders"),
                                 class = "btn btn-outline-primary btn-block")
                ),
                
                div(
                  id = "covariate_panel",
                  style = "display: none;",
                  div(class = "mt-3 mb-2",
                      h5("Covariate Ranges", style = "color: #2C3E50;")
                  ),
                  
                  fluidRow(
                    column(6, numericInput("min_age", "Min Age (Years)", value = 0)),
                    column(6, numericInput("max_age", "Max Age (Years)", value = 18))
                  ),
                  fluidRow(
                    column(6, numericInput("min_WT", "Min Weight (kg)", value = 0)),
                    column(6, numericInput("max_WT", "Max Weight (kg)", value = 150))
                  ),
                  fluidRow(
                    column(6, numericInput("min_HT", "Min Height (cm)", value = 0)),
                    column(6, numericInput("max_HT", "Max Height (cm)", value = 200))
                  )
                )
              ),
              
              # Import dataset settings
              conditionalPanel(
                condition = "input.population_type == 'import'",
                div(
                  class = "card mb-4",
                  div(
                    class = "card-body",
                    h5("Virtual Population Template", class = "mb-3"),
                    downloadButton("download_template", "Download Template", 
                                   class = "btn-primary w-100",
                                   icon = icon("file-download"))
                  )
                ),
                
                div(
                  class = "card mb-4",
                  div(
                    class = "card-body",
                    h5("Upload Dataset (CSV)", class = "mb-3"),
                    fileInput("upload_csv", label = NULL, 
                              placeholder = "No file selected", 
                              buttonLabel = div(icon("upload"), "Browse"), 
                              accept = c(".csv"),
                              width = "100%")
                  )
                ),
                
                div(
                  class = "card",
                  div(
                    class = "card-body bg-light",
                    h5("Instructions", class = "mb-3"),
                    tags$ol(
                      class = "ps-4",
                      tags$li("Download the template CSV file"),
                      tags$li("Fill in your population data"),
                      tags$li("Upload your completed CSV file"),
                      tags$li("Ensure all required columns are present")
                    )
                  )
                ),
                div(class = "mt-4",
                    actionButton("show_inputs_imp", "Customize Covariates", 
                                 icon = icon("sliders"),
                                 class = "btn btn-outline-primary btn-block")
                ),
                div(
                  id = "covariate_panel_imp",
                  style = "display: none;",
                  div(class = "mt-3 mb-2",
                      h5("Covariate Ranges", style = "color: #2C3E50;")
                  ),
                  
                  fluidRow(
                    column(6, numericInput("min_age_imp", "Min Age (Years)", value = 0)),
                    column(6, numericInput("max_age_imp", "Max Age (Years)", value = 100))
                  ),
                  fluidRow(
                    column(6, numericInput("min_WT_imp", "Min Weight (kg)", value = 0)),
                    column(6, numericInput("max_WT_imp", "Max Weight (kg)", value = 200))
                  ),
                  fluidRow(
                    column(6, numericInput("min_HT_imp", "Min Height (cm)", value = 0)),
                    column(6, numericInput("max_HT_imp", "Max Height (cm)", value = 250))
                  )
                )
                
              ),
              
              # Action buttons
              div(class = "mt-4",
                  actionButton("go_button", "Load Population", 
                               icon = icon("database"),
                               class = "btn btn-primary btn-lg btn-block mb-3"),
                  actionButton("explore", "Explore Demographics", 
                               icon = icon("chart-bar"),
                               class = "btn btn-secondary btn-lg btn-block")
              )
            )
          )
        ),
        
        mainPanel(
          tabsetPanel(
            id = "pop_sim_res",
            tabPanel(
              "Population Simulation",
              value = "simulation",
              h4("Population Simulation Results", class = "text-primary mb-4", style = "color: #0056b3;"),
              withSpinner(verbatimTextOutput("model_loaded"), type = 7),
              hr(),
              withSpinner(plotOutput("virtual_pop", height = "600px"), type = 7),
              hr(),
              fluidRow(
                column(4, valueBoxOutput("id_count", width = NULL)),
                column(8, valueBoxOutput("gender_distribution", width = NULL))
              ),
              hr(),
              withSpinner(plotOutput("age_metric_plot", height = "400px"), type = 7),
            ),
            tabPanel(
              "Demographics Map",
              value = "Demographics",
              h4("Demographics", class = "text-primary"),
              withSpinner(
                leafletOutput("demographics_map", height = "600px"),
                type = 7,
                color = "#0275d8"
              )
            )
          )
        )
      )
    ),
    
    tabPanel(
      div(icon("pills"), "Dose Building"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          div(
            class = "card mb-4",
            div(
              class = "card-header bg-primary text-white",
              h4("Model Selection", class = "m-0")
            ),
            div(
              class = "card-body",
              selectizeInput(
                "model", 
                "Select Model",
                choices = c("L. Verrest (2023)", "Chu, W.-Y. (2024)" ,"Upload Own Model"),
                selected = "L. Verrest (2023)", 
                width = "100%",
                options = list(
                  dropdownParent = "body",
                  openOnFocus = TRUE,
                  maxOptions = 10
                )
              ),
              conditionalPanel(
                condition = "input.model == 'Upload Own Model'",
                actionButton("open_model_upload", "Upload & Configure Model", 
                             class = "btn-primary btn-block mt-2")
              )
            ),
            conditionalPanel(
              condition = "input.model == 'L. Verrest (2023)' || input.model == 'Chu, W.-Y. (2024)'",
              actionButton("sim_toggle", "Simulation Settings")
            ),
            
            conditionalPanel(
              condition = "input.sim_toggle % 2 == 1",  # Toggle open on odd clicks
              div(
                class = "mt-3",
                
                materialSwitch(
                  inputId = "IIV",
                  label = "Include Interindividual Variability (IIV)",
                  value = TRUE,
                  width = "auto"
                ),
                
                materialSwitch(
                  inputId = "RUV",
                  label = "Include Residual Unexplained Variability (RUV)",
                  value = TRUE,
                  width = "auto"
                ),
                
                numericInput(
                  inputId = "End_sim",
                  label = "End of Simulation (days)",
                  value = 60,
                  min = 1
                ),
                
                numericInput(
                  inputId = "delta_sim",
                  label = "Simulation Step Size (hr)",
                  value = 12,
                  min = 0.001
                )
              )
            ),
            uiOutput("current_model_display")
          ),
          
          div(
            class = "card mb-4",
            div(
              class = "card-header bg-primary text-white",
              h4("Dosing Regimen", class = "m-0")
            ),
            div(
              class = "card-body",
              regimen_modal_ui("regimen"),
              hr(class = "my-3"),
              
              numericInput(
                inputId = "weight",
                label = "Maximum weight (kg) for analysis",
                value = 30
              ),
              
              hr(class = "my-3"),
              
              materialSwitch("custom_limit", "Use Custom limits", 
                             value = FALSE, status = "primary"),
              
              conditionalPanel(
                condition = "input.custom_limit == true",
                div(class = "mt-3",
                    numericInput(
                      inputId = "upp_limit",
                      label = "Upper limit of AUC (μg·day/mL)",
                      value = NA
                    ),
                    numericInput(
                      inputId = "lower_limit",
                      label = "Lower limit of T>EC90 (days)",
                      value = NA
                    )
                )
              )
            )
          ),
          
          actionButton("run_model", "Simulate Dosing Strategy", 
                       icon = icon("play"),
                       class = "btn btn-primary btn-lg w-100 mb-4"),
          
          # Conditional panels for different tabs
          conditionalPanel(
            condition = "input.tab_selected == 'Pharmacokinetics'",
            div(
              class = "card",
              div(
                class = "card-header bg-info text-white",
                h4("Regimen Selection", class = "m-0")
              ),
              div(
                class = "card-body",
                selectizeInput("select_sum_plot", "Select regimen:", 
                            choices = NULL, selected = NULL, width = "100%",
                            options = list(
                              dropdownParent = "body",
                              openOnFocus = TRUE
                            ))
              )
            )
          ),
          
          conditionalPanel(
            condition = "input.tab_selected == 'pd'",
            div(
              class = "card",
              div(
                class = "card-header bg-info text-white",
                h4("Regimen Selection", class = "m-0")
              ),
              div(
                class = "card-body",
                selectizeInput("select_hazard_sumplot", "Select regimen:", 
                            choices = NULL, selected = NULL, width = "100%",
                            options = list(
                              dropdownParent = "body",
                              openOnFocus = TRUE))
              )
            )
          )
        ),
        mainPanel(
                 tabsetPanel(
                   id = "tab_selected",
                   
                   # Combined Pharmacokinetics tab with PK profiles
                   tabPanel(
                     title = "Pharmacokinetics",
                     value = "Pharmacokinetics",
                     
                     h3("Pharmacokinetic Analysis", class = "text-primary mb-4"),
                     
                     # Target attainment plot and metrics in first row
                     fluidRow(
                       column(8,
                              div(class = "card shadow-sm mb-4",
                                  div(class = "card-header bg-primary text-white",
                                      h4("Target Attainment Plot", class = "m-0")),
                                  div(class = "card-body",
                                      withSpinner(plotlyOutput("target_attainment_plot"), type = 7)
                                  )
                              )
                       ),
                       column(4,
                              div(class = "h-100 d-flex flex-column justify-content-between",
                                 div(class = "card shadow-sm",
                                      div(class = "card-header bg-info text-white",
                                          h5("AUC Limit", class = "m-0")),
                                      div(class = "card-body",
                                          valueBoxOutput("auc_limit", width = NULL)
                                      )
                                  ),
                                   div(class = "card shadow-sm mb-3",
                                      div(class = "card-header bg-info text-white",
                                          h5("TEC90 Limit", class = "m-0")),
                                      div(class = "card-body",
                                          valueBoxOutput("tec90_limit", width = NULL)
                                      )
                                  )
                              )
                       )
                     ),
                     
                     # Target attainment summary and proposed doses in second row (side by side)
                     fluidRow(
                       column(6,
                              div(class = "card shadow-sm mb-4",
                                  div(class = "card-header bg-primary text-white",
                                      h4("Target Attainment Summary", class = "m-0")),
                                  div(class = "card-body",
                                      withSpinner(uiOutput("combined_output"), type = 7)
                                  )
                              )
                       ),
                       column(6,
                              div(class = "card shadow-sm mb-4",
                                  div(class = "card-header bg-primary text-white",
                                      h4("Proposed Doses", class = "m-0")),
                                  div(class = "card-body",
                                      withSpinner(uiOutput("proposed_doses"), type = 7)
                                  )
                              )
                       )
                     ),
                     
                     # Summary plot in third row
                     fluidRow(
                       column(12, 
                              div(class = "card border-0 shadow-lg mb-4", 
                                  div(class = "card-header bg-primary text-white d-flex justify-content-between align-items-center py-3", 
                                      h4("Target Attainment by Weight", class = "m-0 fw-bold"),
                                      div(class = "d-flex align-items-center", 
                                          helpText(class = "text-white-50 me-2 mb-0", "Select reference regimen:"),
                                          div(style = "width: 250px;", 
                                              selectizeInput("select_sum_plot_ref", label = NULL, choices = NULL, selected = NULL)
                                          )
                                      )
                                  ),
                                  div(class = "plot-container",
                                          withSpinner(
                                            plotOutput("sum_plot"), 
                                            type = 7,
                                            color = "#0d6efd"),
                                          uiOutput("attainment_caption")
                                      )
                                  )
                              )
                     ),
                     
                     fluidRow(
                      column(7,
                              div(class = "card shadow-sm mb-4",
                                  div(class = "card-header bg-primary text-white d-flex justify-content-between align-items-center",
                                      h4("PK Profile Visualization", class = "m-0"),
                                      actionButton("customize_filter", label = div(icon("sliders-h"), "Customize Filter"), 
                                                   class = "btn btn-outline-light btn-sm")
                                  ),
                                  div(class = "card-body",
                                      withSpinner(plotlyOutput("pk_profiles_plot", height = "500px"), type = 7)
                                  )
                              )
                       ),
                       column(5,
                              div(class = "card shadow-sm mb-4 h-100",
                                  div(class = "card-header bg-primary text-white",
                                      h4("PK Statistics", class = "m-0")),
                                  div(class = "card-body",
                                      withSpinner(gt_output("pk_stat"), type = 7)
                                  )
                              )
                       )
                     )
                   ),
                   tabPanel(
                     title = "Pharmacodynamics",
                     value = "pd",
                     column(12,
                            div(class = "card border-0 shadow-lg mb-4",
                                div(class = "card-header bg-primary text-white d-flex justify-content-between align-items-center py-3",
                                    h4("Pharmacodynamic Analysis", class = "m-0 fw-bold"),
                                    helpText(class = "text-white-50 m-0", "Review hazard summary below")
                                ),
                                div(class = "card-body p-4",
                                    div(class = "mb-3",
                                        selectizeInput("select_hazard_sumplot_ref", 
                                                    label = "Select reference regimen:", 
                                                    choices = NULL, 
                                                    selected = NULL,
                                                    width = "100%",
                                                    options = list(
                                                      dropdownParent = "body",
                                                      openOnFocus = TRUE
                                                    ))
                                    ),
                                    tags$style(HTML("
                   #select_hazard_sumplot_ref {
                     height: 45px;
                     font-size: 1.1rem;
                   }
                 ")),
                                    div(class = "plot-container",
                                        withSpinner(
                                          plotOutput("hazard_sumplot", height = "800px", width = "100%"),
                                          type = 7,
                                          color = "#0d6efd"
                                        )
                                    )
                                )
                            )
                     )
                   ),
                   tabPanel(
                     title = "pop PK-PD Model summary",
                     value = "pk_model_summary",
                     div(class = "card shadow-sm",
                         div(class = "card-header bg-primary text-white",
                             h4("Pharmacokinetic Model Parameters", class = "m-0")),
                         div(class = "card-body",
                             withSpinner(gt_output("pk_model_stat"), type = 7)
                         )
                     )
                   )
                 )
               )
             )),

    tabPanel(
      div(icon("book-open"), "User Guide"),
      div(
        class = "guide-page",
        div(
          class = "guide-hero",
          h2("MiltSim User Guide", class = "mt-0"),
          p(
            "MiltSim provides a reproducible workflow for selecting a simulation population, assigning a population pharmacokinetic (PopPK) model, building one or more miltefosine dosing regimens, and comparing pharmacokinetic target attainment."
          ),
          p(
            class = "mb-0",
            "The recommended workflow is: population -> PK model -> reference regimen -> comparator regimen(s) -> simulation -> target-attainment and PK review."
          )
        ),

        div(
          class = "guide-callout guide-callout-warning",
          h4(icon("exclamation-triangle"), " Reference-first rule", class = "mt-0"),
          p(
            class = "mb-0",
            tags$strong("When comparing dosing regimens, always create and save the reference regimen first."),
            " MiltSim uses the first regimen as the benchmark from which default exposure limits and subsequent comparative calculations are derived. All comparator regimens should therefore be added only after the intended reference regimen has been defined."
          )
        ),

        div(
          class = "guide-section",
          h3("1. Select or import a population"),
          p(
            "Open ", tags$strong("Population Simulation"), " and select a predefined population or upload a CSV using the supplied template. Covariate ranges can be restricted before the population is loaded. For simulated populations, the seed can be fixed when reproducibility is required."
          ),
          div(
            class = "guide-table-wrap",
            tags$table(
              class = "table table-striped table-bordered table-sm",
              tags$thead(
                tags$tr(
                  tags$th("Population"),
                  tags$th("How it is generated / source"),
                  tags$th("Recommended use")
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td("Pediatric Eastern African Virtual VL Population"),
                  tags$td(
                    "WHO/CDC growth-chart LMS distributions are adjusted to Eastern African VL anthropometry. The underlying VL demographic data were contributed by MSF (South Sudan and Ethiopia) and DNDi (Ethiopia, Kenya, Sudan and Uganda), as described by Mazariegos Herrera et al."
                  ),
                  tags$td("Pediatric VL simulations intended to represent the Eastern African target population.")
                ),
                tags$tr(
                  tags$td("WHO Virtual Population"),
                  tags$td(
                    "Generated from age- and sex-specific growth-chart LMS parameters with correlated weight and height. The in-silico population construction follows the approach of Wasmann et al. (2021), using WHO child growth standards and the corresponding adolescent growth-chart extension used in the application."
                  ),
                  tags$td("General pediatric simulations or comparison with the VL-adjusted population.")
                ),
                tags$tr(
                  tags$td("Adult Eastern African Virtual VL Population"),
                  tags$td(
                    "Preloaded project population derived from the pooled Eastern African VL demographic source used in this work: MSF (South Sudan and Ethiopia) and DNDi (Ethiopia, Kenya, Sudan and Uganda)."
                  ),
                  tags$td("Analyses extending beyond the pediatric population.")
                ),
                tags$tr(
                  tags$td("Female Eastern African Virtual VL Population"),
                  tags$td(
                    "Preloaded female project population derived from the same pooled Eastern African VL demographic source."
                  ),
                  tags$td("Analyses for which a female-specific population is required.")
                ),
                tags$tr(
                  tags$td("Imported population"),
                  tags$td("User-provided age, sex, height and weight data using the MiltSim CSV template."),
                  tags$td("External or study-specific populations.")
                )
              )
            )
          ),
          p(
            tags$strong("WHO-population generation reference: "),
            tags$a(
              "Wasmann RE et al. Constructing a representative in-silico population for paediatric simulations: Application to HIV-positive African children. Br J Clin Pharmacol. 2021;87:2847-2854.",
              href = "https://pmc.ncbi.nlm.nih.gov/articles/PMC8359354/",
              target = "_blank", rel = "noopener noreferrer"
            )
          ),
          p(
            tags$strong("Eastern African pediatric VL demographic source: "),
            tags$a(
              "Mazariegos Herrera A et al. Weight-band-based simplification of oral allometric miltefosine dosing in paediatric patients with visceral leishmaniasis. J Antimicrob Chemother. 2026;81:dkag014.",
              href = "https://pmc.ncbi.nlm.nih.gov/articles/PMC12828427/",
              target = "_blank", rel = "noopener noreferrer"
            )
          )
        ),

        div(
          class = "guide-section",
          h3("2. Select a pharmacokinetic model"),
          p(
            "In ", tags$strong("Dose Building"), ", select a built-in PopPK model or upload a compatible NONMEM/mrgsolve model. The simulation settings allow interindividual variability (IIV), residual unexplained variability (RUV), simulation duration and output step size to be configured."
          ),
          tags$ul(
            tags$li(
              tags$strong("L. Verrest (2023): "),
              "Eastern African VL miltefosine/paromomycin PopPK model. ",
              tags$a("Source", href = "https://doi.org/10.1093/jac/dkad286", target = "_blank", rel = "noopener noreferrer")
            ),
            tags$li(
              tags$strong("Chu, W.-Y. (2024): "),
              "disease-specific PopPK model incorporating PKDL/VL differences. ",
              tags$a("Source", href = "https://doi.org/10.1093/infdis/jiae413", target = "_blank", rel = "noopener noreferrer")
            ),
            tags$li(
              tags$strong("Upload Own Model: "),
              "use the model upload/configuration workflow when evaluating an external model."
            )
          )
        ),

        div(
          class = "guide-section",
          h3("3. Build the reference regimen and comparator regimens"),
          p(
            "Select ", tags$strong("Add Regimen"), " and define the optimization mode, regimen name, dosing strategy, maintenance schedule and, when relevant, loading dose. Save the reference regimen first, then add all alternatives that should be compared with it."
          ),
          fluidRow(
            column(
              4,
              div(
                class = "guide-step",
                h5(tags$span("1", class = "guide-step-number"), "Regular mode"),
                p("Use for direct PK comparison of complete dosing regimens against the reference exposure limits.")
              )
            ),
            column(
              4,
              div(
                class = "guide-step",
                h5(tags$span("2", class = "guide-step-number"), "Dosage-form optimization"),
                p(
                  "Enter the proposed ", tags$strong("scorable dosage-form strength (mg)"), ". For a candidate formulation, enter the proposed strength (for example, 30 mg). For the Allometric FFM-based reference regimen, leave the field empty (NA)."
                )
              )
            ),
            column(
              4,
              div(
                class = "guide-step",
                h5(tags$span("3", class = "guide-step-number"), "Treatment shortening"),
                p("Use the loading-dose controls to define the initial higher-dose period and the subsequent maintenance schedule when evaluating shortened treatment strategies.")
              )
            )
          )
        ),

        div(
          class = "guide-section",
          h3("4. Dosing strategies"),
          tags$h4("Allometric FFM-based"),
          p(
            "This strategy individualizes the daily miltefosine dose using sex, body weight and height through fat-free mass (FFM). It is the recommended reference strategy for the project analyses. In MiltSim, patients below 30 kg receive an FFM-scaled dose; patients from 30 to <45 kg receive 100 mg/day and patients >=45 kg receive 150 mg/day."
          ),
          div(
            class = "guide-equation",
            HTML("Dose<sub>&lt;30 kg</sub> = round<sub>10 mg</sub>[150 &times; (FFM/53)<sup>0.75</sup>] mg/day")
          ),
          p(
            "The app calculates this algorithm directly. Dorlo et al. originally presented the same FFM-based clinical dosing concept as Table 4. The pediatric portion relevant to the <30 kg calculation is reproduced below for orientation; the full published table should be consulted when a clinical lookup table is required."
          ),
          tags$details(
            class = "guide-details",
            tags$summary("Dorlo et al. (2012), Table 4 - pediatric portion"),
            div(
              class = "guide-table-wrap",
              tags$table(
                class = "table table-bordered table-sm text-center",
                tags$caption(
                  style = "caption-side: top; color: #2C3E50;",
                  "Total daily allometric miltefosine dose (mg) by body weight, sex and indicated height (cm). Blank cells were not tabulated in the source."
                ),
                tags$thead(
                  tags$tr(
                    tags$th("Sex"), tags$th("Weight (kg)"),
                    tags$th("60"), tags$th("70"), tags$th("80"), tags$th("90"), tags$th("100"),
                    tags$th("110"), tags$th("120"), tags$th("130"), tags$th("140"), tags$th("150")
                  )
                ),
                tags$tbody(
                  tags$tr(tags$td("Male"), tags$td("9"),  tags$td("30"), tags$td("40"), tags$td("40"), tags$td("40"), tags$td("40"), tags$td("-"),  tags$td("-"),  tags$td("-"),  tags$td("-"),  tags$td("-")),
                  tags$tr(tags$td("Male"), tags$td("12"), tags$td("40"), tags$td("40"), tags$td("40"), tags$td("50"), tags$td("50"), tags$td("50"), tags$td("-"),  tags$td("-"),  tags$td("-"),  tags$td("-")),
                  tags$tr(tags$td("Male"), tags$td("15"), tags$td("-"),  tags$td("40"), tags$td("50"), tags$td("50"), tags$td("60"), tags$td("60"), tags$td("60"), tags$td("-"),  tags$td("-"),  tags$td("-")),
                  tags$tr(tags$td("Male"), tags$td("20"), tags$td("-"),  tags$td("-"),  tags$td("50"), tags$td("60"), tags$td("60"), tags$td("70"), tags$td("70"), tags$td("70"), tags$td("-"),  tags$td("-")),
                  tags$tr(tags$td("Male"), tags$td("25"), tags$td("-"),  tags$td("-"),  tags$td("-"),  tags$td("60"), tags$td("70"), tags$td("70"), tags$td("80"), tags$td("80"), tags$td("80"), tags$td("-")),
                  tags$tr(tags$td("Female"), tags$td("9"),  tags$td("30"), tags$td("30"), tags$td("30"), tags$td("30"), tags$td("30"), tags$td("-"),  tags$td("-"),  tags$td("-"),  tags$td("-"),  tags$td("-")),
                  tags$tr(tags$td("Female"), tags$td("12"), tags$td("30"), tags$td("30"), tags$td("40"), tags$td("40"), tags$td("40"), tags$td("40"), tags$td("-"),  tags$td("-"),  tags$td("-"),  tags$td("-")),
                  tags$tr(tags$td("Female"), tags$td("15"), tags$td("-"),  tags$td("40"), tags$td("40"), tags$td("40"), tags$td("50"), tags$td("50"), tags$td("50"), tags$td("-"),  tags$td("-"),  tags$td("-")),
                  tags$tr(tags$td("Female"), tags$td("20"), tags$td("-"),  tags$td("-"),  tags$td("50"), tags$td("50"), tags$td("50"), tags$td("60"), tags$td("60"), tags$td("60"), tags$td("-"),  tags$td("-")),
                  tags$tr(tags$td("Female"), tags$td("25"), tags$td("-"),  tags$td("-"),  tags$td("-"),  tags$td("60"), tags$td("60"), tags$td("60"), tags$td("70"), tags$td("70"), tags$td("70"), tags$td("-"))
                )
              )
            ),
            p(
              tags$a(
                "Open the complete published Table 4 and source article",
                href = "https://pmc.ncbi.nlm.nih.gov/articles/PMC3393397/",
                target = "_blank", rel = "noopener noreferrer"
              )
            ),
            p(
              class = "small text-muted",
              "The published table is shown as the dosing reference. Administration frequency in MiltSim is defined by the regimen schedule; the project manuscript used once-daily reference dosing."
            )
          ),

          tags$h4("Allometric WB-based", class = "mt-4"),
          p(
            "This strategy replaces the complex FFM lookup with harmonized pediatric weight bands while aiming to preserve FFM-based exposure. MiltSim implements the unified daily dose from Mazariegos Herrera et al. (2026)."
          ),
          div(
            class = "guide-table-wrap",
            tags$table(
              class = "table table-striped table-bordered table-sm text-center",
              tags$caption(
                style = "caption-side: top; color: #2C3E50;",
                "Table 1. Selected allometric weight-band doses (Mazariegos Herrera et al., 2026)."
              ),
              tags$thead(
                tags$tr(
                  tags$th("Weight band (kg)"),
                  tags$th("14-day selected dose (mg)"),
                  tags$th("28-day selected dose (mg)"),
                  tags$th("Final unified daily dose (mg)")
                )
              ),
              tags$tbody(
                tags$tr(tags$td("<6"), tags$td("20"), tags$td("20"), tags$td("20")),
                tags$tr(tags$td("6.00-9.99"), tags$td("30"), tags$td("30"), tags$td("30")),
                tags$tr(tags$td("10.00-14.99"), tags$td("40"), tags$td("50"), tags$td("50")),
                tags$tr(tags$td("15.00-19.99"), tags$td("60"), tags$td("60"), tags$td("60")),
                tags$tr(tags$td("20.00-24.99"), tags$td("70"), tags$td("70"), tags$td("70")),
                tags$tr(tags$td("25.00-29.99"), tags$td("80"), tags$td("80"), tags$td("80"))
              )
            )
          ),
          p(
            tags$a(
              "Open Mazariegos Herrera et al. (2026), Table 1",
              href = "https://pmc.ncbi.nlm.nih.gov/articles/PMC12828427/",
              target = "_blank", rel = "noopener noreferrer"
            )
          ),

          tags$h4("Conventional (mg/kg)", class = "mt-4"),
          p(
            "For patients below 30 kg, MiltSim applies the conventional 2.5 mg/kg/day dose rounded to the nearest 10 mg. Patients from 30 to <45 kg receive 100 mg/day and patients >=45 kg receive 150 mg/day. This option is useful as a conventional dosing comparator."
          ),

          tags$h4("Allometric Customized WB-based", class = "mt-4"),
          p(
            "Use this strategy to test user-defined weight bands and candidate doses. Specify the weight-band boundaries, the number of candidate doses per band and the candidate dose values. MiltSim simulates each candidate, counts upper- and lower-target violations, and identifies the dose with the lowest total number of target violations within the weight band."
          )
        ),

        div(
          class = "guide-section",
          h3("5. How exposure targets are defined"),
          p(
            "If ", tags$strong("Use Custom limits"), " is not selected, the first saved reference regimen defines the limits. The upper exposure target is the 95th percentile of AUC from treatment initiation to the end of treatment (AUC0-EOT), and the lower efficacy target is the 5th percentile of time above EC90 (T>EC90). The intracellular EC90 used in the project is 10.6 microgram/mL."
          ),
          div(
            class = "guide-equation",
            HTML("Upper target: U<sub>AUC</sub> = Q<sub>0.95</sub>(AUC<sub>0-EOT, reference</sub>)")
          ),
          div(
            class = "guide-equation",
            HTML("Lower target: L<sub>T&gt;EC90</sub> = Q<sub>0.05</sub>(T&gt;EC90<sub>reference</sub>)")
          ),
          p(
            class = "small text-muted",
            "In the manuscript target-development analysis, the reference simulation was repeated 100 times and the median target across replicates was retained to reduce seed-dependent variability. In the interactive app, the default limits are derived from the first saved reference simulation in the active run."
          ),
          p(
            "Select custom limits only when the comparison is intended to use prespecified external targets rather than limits derived from the reference simulation."
          )
        ),

        div(
          class = "guide-section",
          h3("6. Dose selection and PK target attainment"),
          p(
            "For every simulated individual, MiltSim evaluates two exposure violations: AUC0-EOT above the upper target and T>EC90 below the lower target. For a candidate dose in a weight band, let n", tags$sub("upper"), " be the number above the AUC target, n", tags$sub("lower"), " the number below the T>EC90 target and n", tags$sub("total"), " the number evaluated."
          ),
          div(
            class = "guide-equation",
            HTML("I<sub>upper,i</sub> = 1[AUC<sub>0-EOT,i</sub> &gt; U<sub>AUC</sub>] &nbsp;&nbsp;&nbsp; I<sub>lower,i</sub> = 1[T&gt;EC90<sub>i</sub> &lt; L<sub>T&gt;EC90</sub>]")
          ),
          div(
            class = "guide-equation",
            HTML("Score<sub>WB</sub> = (n<sub>upper</sub> + n<sub>lower</sub>) / n<sub>total</sub>")
          ),
          p(
            "Candidate doses are ranked by the combined target-violation score. The preferred dose is the dose that minimizes the total target violations across the relevant weight band(s), giving equal importance to avoiding excessive AUC and insufficient T>EC90."
          ),
          div(
            class = "guide-equation",
            HTML("&Sigma;Score<sub>WB</sub> = &Sigma;<sub>WB</sub> Score<sub>WB</sub> &nbsp;&nbsp;&nbsp; TA<sub>PK</sub>(%) = 100 - &Sigma;Score<sub>WB</sub>(%)")
          ),
          p(
            "The app also reports target-specific attainment directly as the percentage of simulated individuals without an upper- or lower-target violation:"
          ),
          div(
            class = "guide-equation",
            HTML("TA<sub>AUC</sub>(%) = 100 &times; [1 - (&Sigma;n<sub>upper</sub> / &Sigma;n<sub>total</sub>)]")
          ),
          div(
            class = "guide-equation",
            HTML("TA<sub>T&gt;EC90</sub>(%) = 100 &times; [1 - (&Sigma;n<sub>lower</sub> / &Sigma;n<sub>total</sub>)]")
          )
        ),

        div(
          class = "guide-section",
          h3("7. Dosage-form optimization and pill burden"),
          p(
            "For a proposed scored single-strength formulation S (mg), the number of units required for a daily dose D is D/S. For the FFM-based reference formulation, MiltSim treats the currently available 50 mg and 10 mg strengths as the comparator."
          ),
          div(
            class = "guide-equation",
            HTML("Candidate units/day: N<sub>candidate</sub> = D/S")
          ),
          div(
            class = "guide-equation",
            HTML("Reference units/day: N<sub>reference</sub> = floor(D/50) + (D mod 50)/10")
          ),
          tags$h4("Current MiltSim dosage-form score"),
          p(
            "In the current app implementation, the dosage-form summary combines PK target attainment (90% weight) with the relative improvement in pill count (10% weight). This calculation is performed separately for the upper and lower PK targets."
          ),
          div(
            class = "guide-equation",
            HTML("Pill-count improvement (%) = 100 &times; (&Sigma;N<sub>reference</sub> - &Sigma;N<sub>candidate</sub>) / &Sigma;N<sub>reference</sub>")
          ),
          div(
            class = "guide-equation",
            HTML("Score<sub>PC,target</sub> = 0.90 &times; TA<sub>PK,target</sub> + 0.10 &times; Pill-count improvement")
          ),
          tags$h4("Adherence-adjusted methodology used in the manuscript", class = "mt-4"),
          p(
            "The manuscript and Supplementary Results S3 additionally evaluate dosage-form strength by propagating pill burden through an adherence model. The probability that individual i takes a scheduled dose is modeled with an inverse-logistic relationship:"
          ),
          div(
            class = "guide-equation",
            HTML("P(ADH<sub>i</sub>) = logit<sup>-1</sup>(&theta;<sub>0</sub> + &theta;<sub>1</sub>N<sub>i</sub> + &eta;<sub>i</sub>) = 1 / {1 + exp[-(&theta;<sub>0</sub> + &theta;<sub>1</sub>N<sub>i</sub> + &eta;<sub>i</sub>)]}")
          ),
          div(
            class = "guide-equation",
            HTML("&theta;<sub>0</sub> = 3.53; &theta;<sub>1</sub> = -0.34; &eta;<sub>i</sub> ~ N(0, &omega;<sup>2</sup>), with &Omega; = 0.7")
          ),
          div(
            class = "guide-equation",
            HTML("DoseTaken<sub>i,t</sub> ~ Bernoulli[P(ADH<sub>i</sub>)] &isin; {0,1}")
          ),
          p(
            "A value of 1 denotes a taken scheduled dose and 0 a missed dose. Missed doses are removed from the simulated dosing history, the PopPK model is rerun, and AUC0-EOT and T>EC90 are recalculated. The same target-violation framework is then applied to the adherence-adjusted exposure. This is the adherence analysis described in the manuscript/supplementary material."
          )
        ),

        div(
          class = "guide-section",
          h3("8. Run the simulation and interpret the output"),
          tags$ol(
            tags$li("Confirm that the intended population has been loaded and that the desired PK model and variability settings are active."),
            tags$li("Confirm that the reference regimen is the first saved regimen and that its treatment duration and dose schedule are correct."),
            tags$li("Add comparator regimen(s), including any custom weight bands, candidate doses, loading dose, or scored dosage-form strength."),
            tags$li("Leave custom exposure limits disabled to derive the 95th-percentile AUC and 5th-percentile T>EC90 targets from the reference, or enter prespecified limits if required."),
            tags$li("Select Simulate Dosing Strategy."),
            tags$li("Review target attainment, weight-stratified exposure, PK profiles, proposed doses, and PK summary statistics. A regimen should not be selected solely from a population-level score if an individual weight band shows clinically important under- or overexposure.")
          )
        ),

        div(
          class = "guide-section",
          h3("References"),
          tags$ol(
            class = "guide-reference-list",
            tags$li(
              tags$a("Wasmann RE, Svensson EM, Walker AS, Clements MN, Denti P. Constructing a representative in-silico population for paediatric simulations: Application to HIV-positive African children. Br J Clin Pharmacol. 2021;87:2847-2854.", href = "https://doi.org/10.1111/bcp.14694", target = "_blank", rel = "noopener noreferrer")
            ),
            tags$li(
              tags$a("World Health Organization. WHO Child Growth Standards: Length/Height-for-Age, Weight-for-Age, Weight-for-Length, Weight-for-Height and BMI-for-Age: Methods and Development. 2006.", href = "https://www.who.int/publications/i/item/924154693X", target = "_blank", rel = "noopener noreferrer")
            ),
            tags$li(
              tags$a("Dorlo TPC, Huitema ADR, Beijnen JH, de Vries PJ. Optimal Dosing of Miltefosine in Children and Adults with Visceral Leishmaniasis. Antimicrob Agents Chemother. 2012;56:3864-3872.", href = "https://pmc.ncbi.nlm.nih.gov/articles/PMC3393397/", target = "_blank", rel = "noopener noreferrer")
            ),
            tags$li(
              tags$a("Mazariegos Herrera A, Karlsson MO, Svensson EM, Dorlo TPC. Weight-band-based simplification of oral allometric miltefosine dosing in paediatric patients with visceral leishmaniasis. J Antimicrob Chemother. 2026;81:dkag014.", href = "https://pmc.ncbi.nlm.nih.gov/articles/PMC12828427/", target = "_blank", rel = "noopener noreferrer")
            ),
            tags$li(
              tags$a("Verrest L, Roseboom IC, Wasunna M, et al. Population pharmacokinetics of a combination of miltefosine and paromomycin in Eastern African children and adults with visceral leishmaniasis. J Antimicrob Chemother. 2023;78:2702-2714.", href = "https://doi.org/10.1093/jac/dkad286", target = "_blank", rel = "noopener noreferrer")
            ),
            tags$li(
              tags$a("Chu W-Y, Verrest L, Younis BM, et al. Disease-Specific Differences in Pharmacokinetics of Paromomycin and Miltefosine Between Post-Kala-Azar Dermal Leishmaniasis and Visceral Leishmaniasis Patients in Eastern Africa. J Infect Dis. 2024;230:e1375-e1384.", href = "https://doi.org/10.1093/infdis/jiae413", target = "_blank", rel = "noopener noreferrer")
            ),
            tags$li(
              tags$a("Nachega JB, Parienti J-J, Uthman OA, et al. Lower Pill Burden and Once-Daily Antiretroviral Treatment Regimens for HIV Infection: A Meta-Analysis of Randomized Controlled Trials. Clin Infect Dis. 2014;58:1297-1307.", href = "https://doi.org/10.1093/cid/ciu046", target = "_blank", rel = "noopener noreferrer")
            )
          )
        )
      )
    ),

    # Temporarily hidden from the user interface while the sensitivity workflow is under development.
    if (FALSE) tabPanel(
      "Sensitivity Analysis",
      sidebarLayout(
        sidebarPanel(
          div(class = "card shadow-sm p-3 mb-3",
              selectizeInput("model_sens", "Select Model",
                          choices = c("L. Verrest (2023)","Chu, W.-Y. (2024)", "Upload Own Model"),
                          selected = "L. Verrest (2023)", width = "100%"),
              conditionalPanel(
                condition = "input.model_sens == 'Upload Own Model'",
                fileInput("pk_model_file_sens", "Upload OWN Model", accept = c(".cpp"),
                          placeholder = "Please upload a .cpp file")
              )
          ), 
          div(class = "card shadow-sm p-3 mb-3",
              h4("Build Dosing Regimen", class = "text-primary"),
              tags$head(tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css")),
              regimen_modal_ui_sens("regimen_sens"),
              hr(),
              numericInput(
                inputId = "weight_sens",
                label = "Maximum included weight (kg) in the analysis",
                value = 30
              ),
              hr(),
              materialSwitch("custom_limit_sens", "Use Custom limits", value = FALSE),
              conditionalPanel(
                condition = "input.custom_limit_sens == true",
                div(
                  numericInput(
                    inputId = "upp_limit_sens",
                    label = "Upper limit of AUC (ug*day/mL)",
                    value = NA
                  ),
                  numericInput(
                    inputId = "lower_limit_sens",
                    label = "Lower limit of T>EC90 (days)",
                    value = NA
                  )
                )
              )
          ),
          div(class = "card shadow-sm p-3 mb-3",
              h4("Sensitivity Analysis Settings", class = "text-primary mb-3"),
              selectizeInput("param_to_analyze", "Parameter to Analyze",
                          choices = c("CL" = "THETA1", "V" = "THETA2", "KA" = "THETA3", 
                                      "COV F (WEEK)" = "THETA7", "COV F (DDOS)" = "THETA8"),
                          options = list(
                            dropdownParent = "body",
                            openOnFocus = TRUE
                          )),
              numericInput("sensitivity_step", "Step Size (%)", value = 25, min = 1, max = 100, step = 1),
          ),
          actionButton("run_sens", "Run Sensitivity Analysis", 
                       class = "btn btn-success btn-lg w-100 mt-3")
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Sensitivity Results",
              h4("Results", class = "text-primary mb-3"),
              
              # Target Attainment Plot
              div(class = "card shadow-sm mb-4",
                  div(class = "card-header bg-primary text-white",
                      h4("Target Attainment", class = "m-0")),
                  div(class = "card-body p-0",
                      withSpinner(plotlyOutput("target_attainment_plot_sens"), type = 7)
                  )
              ),
              
              # Proposed Doses Table
              div(class = "card shadow-sm mb-4",
                  div(class = "card-header bg-primary text-white",
                      h4("Proposed doses", class = "m-0")),
                  div(class = "card-body",
                      withSpinner(DTOutput("summary_sens"), type = 7)
                  )
              ),
              
              # PK Profile Visualization (moved from separate tab)
              div(class = "card shadow-sm mb-4",
                  div(class = "card-header bg-primary text-white",
                      h4("PK Profile Visualization", class = "m-0")),
                  div(class = "card-body p-0",
                      withSpinner(plotlyOutput("pk_profiles_plot_sens", height = "600px"), type = 7)
                  )
              ),
              
              # PK Summary Table (moved from separate tab)
              div(class = "card shadow-sm",
                  div(class = "card-header bg-primary text-white",
                      h4("Pharmacokinetic Summary", class = "m-0")),
                  div(class = "card-body",
                      withSpinner(gt_output("pk_stat_sens"), type = 7)
                  )
              )
            )
          )
        )
      )
    )
    # tabPanel(
    #   "Cotraception Analysis",
    #   sidebarLayout(
    #     sidebarPanel(
    #       div(class = "card shadow-sm p-3 mb-3",
    #           selectizeInput("model_rep", "Select Model",
    #                          choices = c("L. Verrest (2023)", "Upload Own Model"),
    #                          selected = "L. Verrest (2023)", width = "100%"),
    #           conditionalPanel(
    #             condition = "input.model_rep == 'Upload Own Model'",
    #             fileInput("pk_model_file_rep", "Upload OWN Model", accept = c(".cpp"),
    #                       placeholder = "Please upload a .cpp file")
    #           )
    #       ), 
    #       div(class = "card shadow-sm p-3 mb-3",
    #           h4("Build Dosing Regimen", class = "text-primary"),
    #           tags$head(tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css")),
    #           regimen_modal_ui_rep("regimen_rep"),
    #           hr(),
    #             div(
    #               numericInput(
    #                 inputId = "TSWITCH",
    #                 label = "Contracpetion period (days)",
    #                 value = NA
    #               ),
    #               numericInput(
    #                 inputId = "AUC_target",
    #                 label = "Reproductive Safety Threshold (mg*day/L)",
    #                 value = NA
    #               )
    #             )
    #       ),
    #       actionButton("run_rep", "Run Simulation", 
    #                    class = "btn btn-success btn-lg w-100 mt-3")
    #     ),
    #     
    #     mainPanel(
    #       tabsetPanel(
    #         tabPanel(
    #           "Results",
    #           h4("Results", class = "text-primary mb-3"),
    #           
    #           # Target Attainment Plot
    #           div(class = "card shadow-sm mb-4",
    #               div(class = "card-header bg-primary text-white",
    #                   h4("Target Attainment", class = "m-0")),
    #               div(class = "card-body p-0",
    #                   withSpinner(plotlyOutput("target_attainment_plot_rep"), type = 7)
    #               )
    #           )
    #         )
    #       )
    #     )
    #   )
    # )
  )
)
