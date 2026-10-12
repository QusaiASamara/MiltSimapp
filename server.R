server <- function(input, output, session) {
  

  #################################
  ####    virtual population   ####
  #################################
  

  observeEvent(input$show_inputs, {
    toggle("covariate_panel")
  })
  
  observeEvent(input$show_inputs_imp, {
    toggle("covariate_panel_imp")
  })
  
  
  data <- eventReactive(input$go_button, {
    if (input$population_type == "predefined") {
      shiny::req(input$population)
      
      if (input$use_seed) {
        shiny::validate(
          shiny::need(input$seed_value, "Please enter a seed value.")
        )
      }
      
      # Determine seed value
      seed_value <- if (input$use_seed) {
        as.integer(input$seed_value)
      } else {
        as.integer(sample.int(1e6, 1))
      }
      
      common_params <- list(input$min_age, input$max_age, input$min_WT, input$max_WT, input$min_HT, input$max_HT)
      names(common_params) <- c("minage", "maxage", "minWT", "maxWT", "minHT", "maxHT")
      
      data_list <- switch(input$population,
                          "Pediatric Eastern African Virtual VL Population" = load_East_Africa_pop(
                            subj_num = input$subj_num, seed = seed_value, age_unit = input$age_unit,
                            minage = common_params$minage, maxage = common_params$maxage,
                            minWT = common_params$minWT, maxWT = common_params$maxWT,
                            minHT = common_params$minHT, maxHT = common_params$maxHT
                          ),
                          "WHO Virtual Population" = load_WHO_pop(
                            subj_num = input$subj_num, seed = seed_value, age_unit = input$age_unit,
                            minage = common_params$minage, maxage = common_params$maxage,
                            minWT = common_params$minWT, maxWT = common_params$maxWT,
                            minHT = common_params$minHT, maxHT = common_params$maxHT
                          ),
                          "Eastern African Virtual VL Population" = load_imported_pop(
                            data = "WHO_Data/100boostraped_pop.csv",
                            minage = common_params$minage, maxage = common_params$maxage,
                            minWT = common_params$minWT, maxWT = common_params$maxWT,
                            minHT = common_params$minHT, maxHT = common_params$maxHT
                          ),
                          "Female Eastern African Virtual VL Population" = load_imported_pop(
                            data = "WHO_Data/joint_female.csv",
                            minage = common_params$minage, maxage = common_params$maxage,
                            minWT = common_params$minWT, maxWT = common_params$maxWT,
                            minHT = common_params$minHT, maxHT = common_params$maxHT
                          )
      )
      
      
    } else if (input$population_type == "import") {
      shiny::req(input$upload_csv)
      
      # Read the uploaded file
      file_path <- input$upload_csv$datapath
      
      # Load the imported population data
      data_list <- load_imported_pop(file_path,input$min_age_imp,input$max_age_imp,
                                     input$min_WT_imp,input$max_WT_imp,input$min_HT_imp,
                                     input$max_HT_imp)
      
      return(data_list)
    }
  })
  
  output$model_loaded <- renderText({
    shiny::req(input$go_button)
    
    if (input$use_seed) {
      shiny::validate(
        shiny::need(input$seed_value, "Please enter a seed value.")
      )
    }
    
    "Population is loaded successfully!"
  })
  
  output$download_template <- downloadHandler(
    filename = function() { "population_template.csv" },
    content = function(file) {
      file.copy("Virtual_population_template.csv", file)
    }
  )
  
  output$virtual_pop <- renderPlot({
    shiny::req(input$explore)
    data_list <- data()
    
    ggplot(data_list$WHO_data_HT_WT_FFM_long, 
           aes(x = VAL, fill = GENDER, color = GENDER)) +
      geom_density(alpha = 0.4, linewidth = 0.7) +
      facet_wrap(~METRIC, scales = "free_x", nrow = 1) +
      theme_bw() +
      scale_fill_brewer(palette = "Set1") +
      scale_color_brewer(palette = "Set1") +
      labs(
        title = "Distribution of covariates by Gender",
        subtitle = "Comparison of density distributions across measurement categories",
        x = "Value", 
        y = "Density") +
          theme(
            legend.position = "bottom",
            legend.title = element_text(face = "bold"),
            strip.text = element_text(face = "bold", size = 11),
            plot.title = element_text(face = "bold", size = 14),
            plot.subtitle = element_text(size = 11, color = "gray30"),
            axis.title = element_text(face = "bold"),
            panel.grid.minor = element_blank(),
            panel.spacing = unit(1.5, "lines"))
    
  }) 
  
  output$age_metric_plot <- renderPlot({
    shiny::req(input$explore)
    data_list <- data()
    
    ggplot(data_list$WHO_data_HT_WT_FFM_long, aes(x = AGE, y = VAL, color = GENDER)) +
      geom_point(alpha = 0.1, size = 1.5) +
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), 
                  se = TRUE, level = 0.95) +
      facet_wrap(~METRIC, scales = "free") +
      theme_bw() +
      labs(
        x = "Age (years)",
        y = "Value",
        color = "Gender"
      ) +
      theme(
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 14),  # Larger facet labels
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        plot.margin = margin(t = 10, r = 15, b = 10, l = 10),  # Add some breathing room
        axis.title = element_text(face = "bold"),  # Bold axis titles
        legend.title = element_text(face = "bold")  # Bold legend title
      )
    
  })
  
  output$id_count <- renderValueBox({
    shiny::req(input$explore)
    data_list <- data()
    id_count <- length(unique(data_list$WHO_data_HT_WT_FFM_long$ID))
    valueBox(
      value = id_count,
      subtitle = "Total Individuals count",
      icon = icon("id-badge"))
  })
  
  output$gender_distribution <- renderValueBox({
    shiny::req(input$explore)
    data_list <- data()
    unique_data <- data_list$WHO_data_HT_WT_FFM_long[!duplicated(data_list$WHO_data_HT_WT_FFM_long$ID), ]
    gender_counts <- table(unique_data$GENDER)    
    valueBox(
      value = paste0("Males: ", gender_counts["Boys"], ", Females: ", gender_counts["Girls"]),
      subtitle = "Gender Distribution",
      icon = icon("venus-mars"),
    )
  })
  
  
  
  output$demographics_map <- renderLeaflet({
    shiny::req(
      input$go_button,
      input$population == "Pediatric Eastern African Virtual VL Population",
      input$pop_sim_res == "Demographics"
    )
    
    # Generate demographic map
    create_demographic_map()
  })
  
  
  #################################
  ####    Model building       ####
  #################################
  
  session$onSessionEnded(function() {
    # Use a tryCatch and direct file system operations
    if (!is.null(session$userData$temp_model_dir) && 
        dir.exists(session$userData$temp_model_dir)) {
      tryCatch({
        unlink(session$userData$temp_model_dir, recursive = TRUE, force = TRUE)
      }, error = function(e) {
        # Optional: log the error if needed
        message("Error during session cleanup: ", e$message)
      })
    }
  })
  
  
  observeEvent(input$show_upload_instructions, {
    showModal(modalDialog(
      title = "Model File Upload Instructions",
      includeMarkdown("helper_functions/file_upload_instructions.Rmd"),  # Or you can use the text directly
      footer = modalButton("Close"),
      size = "l"
    ))
  })
  
  
  rv <- reactiveValues(
    current_model = NULL,
    saved_model_code = NULL,
    saved_model = NULL,
    model_name = NULL,
    model_compile_error = NULL,
    temp_model_dir = NULL,
    uploaded_files = NULL
  )
  
  output$current_model_display <- renderUI({
    if (!is.null(rv$model_name)) {
      div(
        class = "current-model-box",
        style = "border: 1px solid #d3d3d3; 
             padding: 10px; 
             margin-bottom: 10px; 
             display: flex; 
             justify-content: space-between; 
             align-items: center;",
        div(
          strong("Current Model: "),
          span(ifelse(is.null(rv$model_name), "Uploaded Model", rv$model_name))
        ),
        actionButton("remove_current_model", 
                     label = "Remove Model", 
                     icon = icon("trash"),
                     class = "btn-danger btn-sm")
      )
    } else {
      NULL
    }
  })
  
  
  
  observeEvent(input$open_model_upload, {
    # Validate model loading state with more informative messages
    if (!is.null(rv$model_name)) {
      showModal(
        modalDialog(
          title = "Model Already Loaded",
          "A model is currently loaded. Please remove the existing model before uploading a new one.",
          footer = modalButton("Close"),
          easyClose = TRUE
        )
      )
    } else {
      showModal(model_UI())
    }
  })
  
  observeEvent(input$remove_current_model, {
    # Comprehensive model reset with detailed logging
    tryCatch({
      # Reset all model-related reactive values
      rv$saved_model <- NULL
      rv$model_name <- NULL
      rv$saved_model_code <- NULL
      rv$current_model <- NULL
      rv$model_compile_error <- NULL
      rv$uploaded_files <- NULL
      
      # Clean up any lingering temporary directories
      if (!is.null(rv$temp_model_dir) && dir.exists(rv$temp_model_dir)) {
        unlink(rv$temp_model_dir, recursive = TRUE)
        rv$temp_model_dir <- NULL
      }
      
      # Reset UI elements
      updateSelectInput(session, "model", selected = "L. Verrest (2023)")
      
      # Enhanced notification
      showNotification(
        "Model and associated files have been completely removed", 
        type = "message", 
        duration = 3
      )
    }, error = function(e) {
      showNotification(
        paste("Error removing model:", e$message), 
        type = "error", 
        duration = 5
      )
    })
  })
  
  observeEvent(input$viewModel, {
    shiny::req(input$pk_model_file)
    shiny::req(input$file_type == "mrgsolve")
    
    if (input$file_type != "mrgsolve") {
      showNotification(
        "Error: Selected file type does not match the uploaded file. Please choose 'NONMEM' for NONMEM files.",
        type = "error",
        duration = 5
      )
      updateSelectInput(session, "file_type", selected = "nonmem")
      return()
    }
    
    tryCatch({
      model_path <- input$pk_model_file$datapath
      
      file_ext <- tools::file_ext(input$pk_model_file$name)
      allowed_extensions <- "cpp"
      
      if (!(tolower(file_ext) %in% allowed_extensions)) {
        showNotification(
          paste("Invalid file type. Please upload a .cpp file for mrgsolve models."),
          type = "error",
          duration = 5
        )
        return()
      }
      
      model_code <- readLines(model_path, warn = FALSE)
      model_code <- paste(model_code, collapse = "\n")
      
      updateAceEditor(session, "modelCodeEditor", value = model_code)
      
      showNotification(
        "Model file successfully loaded",
        type = "message",
        duration = 3
      )
    }, error = function(e) {
      showNotification(
        paste("Error reading model file:", e$message),
        type = "error",
        duration = 5
      )
    })
  })
  
  
  observeEvent(input$convertModel, {
    shiny::req(input$pk_model_file)
    shiny::req(input$file_type == "nonmem")
    
    # Create a temporary directory for this model
    temp_model_dir <- create_temp_model_dir()
    
    # Copy all uploaded files to the temporary directory
    file.copy(
      from = input$pk_model_file$datapath, 
      to = file.path(temp_model_dir, input$pk_model_file$name)
    )
    
    tryCatch({
      # Use the robust conversion function
      converted_file <- convert_nonmem_model(input$pk_model_file, temp_model_dir)
      
      # Read the generated mrgsolve code
      mrgsolve_code <- readLines(converted_file, warn = FALSE)
      
      # Update the code editor with the translated mrgsolve code
      updateAceEditor(session, "modelCodeEditor", 
                      value = paste(mrgsolve_code, collapse = "\n"),
                      mode = "c_cpp")
      
      # Store the temporary directory and files
      rv$temp_model_dir <- temp_model_dir
      rv$uploaded_files <- input$pk_model_file
      
      # Show success notification
      showNotification(
        "NONMEM model successfully translated to mrgsolve!", 
        type = "message", 
        duration = 3
      )
    }, error = function(e) {
      # Enhanced error handling
      showNotification(
        paste("Translation failed:", conditionMessage(e)), 
        type = "error", 
        duration = 6
      )
      
      # Clean up the temporary directory
      unlink(temp_model_dir, recursive = TRUE)
    })
  })
  
  output$downloadModel <- downloadHandler(
    filename = function() {
      paste0("model_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".cpp")
    },
    content = function(file) {
      writeLines(input$modelCodeEditor, file)
    }
  )
  
  observeEvent(input$saveModel, {
    shiny::req(input$modelCodeEditor)
    
    tryCatch({
      # Create a temporary file to save the model
      temp_model_file <- tempfile(fileext = ".cpp")
      writeLines(input$modelCodeEditor, temp_model_file)
      
      
      # Save the model details
      rv$saved_model_code <- temp_model_file
      rv$model_name <- paste0("Translated Model (", 
                              format(Sys.time(), "%Y-%m-%d %H:%M"), ")")
      
      # Update UI
      updateSelectInput(session, "model", selected = "Upload Own Model")
      
      # Show success notification
      showNotification("Model saved successfully", 
                       type = "message", 
                       duration = 3)
      
      # Remove the modal
      removeModal()
    }, error = function(e) {
      showNotification(
        paste("Saving error:", conditionMessage(e)), 
        type = "error", 
        duration = 6
      )
    })
  })
  
  model <- eventReactive(
    eventExpr = {
      if (input$model %in% c("L. Verrest (2023)", "Chu, W.-Y. (2024)")) {
        list(input$model, input$IIV, input$RUV, input$go_button)
      } else if (input$model == "Upload Own Model") {
        list(input$model, rv$saved_model_code, input$go_button)
      }
    },
    valueExpr = {
      # Clear previous error
      rv$model_compile_error <- NULL
      
      tryCatch({
        # Validate input
        if (is.null(input$model) || input$model == "") {
          stop("No model selected. Please select a model.")
        }
        
        # Process based on model type
        if (input$model == "L. Verrest (2023)") {
          
          mod_MF_pk <- load_predefined_model(
            model_name = "L. Verrest (2023)",
            model_path = "helper_functions/Verrest_pk_model.cpp",
            include_iiv = input$IIV,
            include_ruv = input$RUV
          )
          
          rv$model_name <- "L. Verrest (2023)"
          rv$saved_model <- mod_MF_pk
          
        } else if (input$model == "Chu, W.-Y. (2024)") {
          
          mod_MF_pk <- load_predefined_model(
            model_name = "Chu, W.-Y. (2024)",
            model_path = "helper_functions/wendysmodel_VL.cpp",
            include_iiv = input$IIV,
            include_ruv = input$RUV
          )
          
          rv$model_name <- "Chu, W.-Y. (2024)"
          rv$saved_model <- mod_MF_pk
          
        } else if (input$model == "Upload Own Model") {
          
          # Validate uploaded model
          if (is.null(rv$saved_model_code) || rv$saved_model_code == "") {
            stop("No model uploaded. Please upload a model first.")
          }
          
          # Check if file exists
          if (!file.exists(rv$saved_model_code)) {
            stop("Uploaded model file not found. Please re-upload the model.")
          }
          
          own_model <- mread(rv$saved_model_code)
          rv$model_name <- "Uploaded Custom Model"
          rv$saved_model <- own_model
          loadso(own_model)
          
        } else {
          stop(paste("Unknown model type:", input$model))
        }
        
        # Success notification
        showNotification(
          paste("Model", rv$model_name, "loaded successfully"), 
          type = "message", 
          duration = 3
        )
        
        # Return the model
        return(rv$saved_model)
        
      }, error = function(e) {
        # Store compilation error
        error_msg <- conditionMessage(e)
        rv$model_compile_error <- error_msg
        
        # Show error notification
        showNotification(
          paste("Model error:", error_msg), 
          type = "error", 
          duration = 8
        )
        
        # Log error for debugging
        cat("Model compilation error:", error_msg, "\n")
        
        return(NULL)
      })
    }
  )
  
  
  ################################
  ####    dosing strategy     ####
  ################################
  regimen_data <- regimen_server("regimen")
  
  
  combined_regimens <- eventReactive(input$run_model, {
    shiny::req(input$weight)
    # Get all saved regimens
    all_regimens <- regimen_data$regimens()
    
    validate(
      need(!is.null(input$weight), "Please enter a weight value."))
    
    if (is.null(all_regimens) || length(all_regimens) == 0) {
      showNotification("No regimens found. Please add a regimen before running the model.", type = "warning")
      return(NULL)
    }
    regimen_list <- list()
    
    
    first_regimen <- all_regimens[[1]]  
    
    # Extract maintenance dose frequency and interval
    mainta_freq <- first_regimen$maintenance_dose$frequency
    mainta_interval <- first_regimen$maintenance_dose$interval
    utility_score_mode <- first_regimen$optimization_mode
    custom_strength <- first_regimen$custom_dose_strengt
    
    
    ref_data <- create_allom_dataset(
      data = data()$WHO_data_HT_WT_FFM,
      model = model(),
      weight = input$weight,
      seed = 9119,
      use_loading_dose = FALSE,
      fixed_load_dose = NULL,
      load_freq = NULL,
      load_interval = NULL,
      main_freq = mainta_freq,
      main_interval = mainta_interval,
      weight_bands = NULL, 
      type = "ref",
      TSWITCH_val = 0,
      upper = NULL,
      lower = NULL,
      end = input$End_sim,
      delta = input$delta_sim,
      mode = "defulat",
      Utility_score_mode = utility_score_mode,
      dosage = custom_strength)
    
    
    
    
    upper_ci_obs_AUC_ref <- ref_data$upper_ci_obs_AUC
    lower_ci_obs_TOEC90_ref <- ref_data$lower_ci_obs_TOEC90
    # ref_sumplot <- ref_data$sumplot
    
    assign ("upper_ci_obs_AUC", if (!is.na(input$upp_limit) && input$custom_limit) {
      input$upp_limit
    } else {
      upper_ci_obs_AUC_ref  # Use stored reactive value if available
    }, envir = .GlobalEnv)
    
    assign("lower_ci_obs_TOEC90", if (!is.na(input$lower_limit) && input$custom_limit) {
      input$lower_limit
    } else {
      lower_ci_obs_TOEC90_ref  # Use stored reactive value if available
    }, envir = .GlobalEnv)
    # 
    # assign("upper_ci_obs_AUC", upper_ci_obs_AUC, envir = .GlobalEnv)
    # assign("lower_ci_obs_TOEC90", lower_ci_obs_TOEC90, envir = .GlobalEnv)
    # assign("ref_data", ref_sumplot, envir = .GlobalEnv)
    
    
    # Iterate over each saved regimen
    for (regimen_name in names(all_regimens)) {
      
      current_regimen <- all_regimens[[regimen_name]]
      regimen_label <- current_regimen$name  # Ensure this is the correct field
      
      all_regimens <- regimen_data$regimens()
      


      # Extract values for current regimen
      dosing_strategy <- current_regimen$strategy
      use_loading_dose <- !is.null(current_regimen$loading_dose)
      loading_dose_fixed <- current_regimen$loading_dose$fixed_dose
      loading_freq <- ifelse(use_loading_dose, current_regimen$loading_dose$frequency, 0)
      loading_interval <- ifelse(use_loading_dose, current_regimen$loading_dose$interval, 0)
      maint_freq <- current_regimen$maintenance_dose$frequency
      maint_interval <- current_regimen$maintenance_dose$interval
      weight_bands <- current_regimen$loading_dose$weight_bands
      custom_doses <- current_regimen$custom_doses
      utility_score_mode <- current_regimen$optimization_mode
      custom_strength <- current_regimen$custom_dose_strength
      
      
      # Generate regimen based on dosing strategy
      generated_regimen <- switch(dosing_strategy,
                                  "Allometric_FFM" = create_allom_dataset(
                                    data = data()$WHO_data_HT_WT_FFM,
                                    model = model(),
                                    weight = input$weight,
                                    seed = 9119,
                                    use_loading_dose = use_loading_dose,
                                    fixed_load_dose = loading_dose_fixed,
                                    load_freq = loading_freq,
                                    load_interval = loading_interval,
                                    main_freq = maint_freq,
                                    main_interval = maint_interval,
                                    weight_bands = weight_bands,
                                    type = "calc",
                                    TSWITCH_val = 0,
                                    upper = upper_ci_obs_AUC,
                                    lower = lower_ci_obs_TOEC90,
                                    end = input$End_sim,
                                    delta = input$delta_sim,
                                    mode = "defulat",
                                    Utility_score_mode = utility_score_mode,
                                    dosage = custom_strength),
                                  "Conventional" = create_lin_dataset(
                                    data = data()$WHO_data_HT_WT_FFM,
                                    model = model(),
                                    weight = input$weight,
                                    seed = 9119,
                                    use_loading_dose = use_loading_dose,
                                    fixed_load_dose = loading_dose_fixed,
                                    load_freq = loading_freq,
                                    load_interval = loading_interval,
                                    main_freq = maint_freq,
                                    main_interval = maint_interval,
                                    weight_bands = weight_bands,
                                    TSWITCH_val = 0,
                                    upper = upper_ci_obs_AUC,
                                    lower = lower_ci_obs_TOEC90,
                                    end = input$End_sim,
                                    delta = input$delta_sim,
                                    mode = "defulat",
                                    Utility_score_mode = utility_score_mode,
                                    dosage = custom_strength),
                                  "Allometric_WB" = create_allometric_WB_dosing(
                                    data = data()$WHO_data_HT_WT_FFM,
                                    model = model(),
                                    weight = input$weight,
                                    seed = 9119,
                                    use_loading_dose = use_loading_dose,
                                    fixed_load_dose = loading_dose_fixed,
                                    load_freq = loading_freq,
                                    load_interval = loading_interval,
                                    main_freq = maint_freq,
                                    main_interval = maint_interval,
                                    weight_bands = weight_bands,
                                    TSWITCH_val = 0,
                                    upper = upper_ci_obs_AUC,
                                    lower = lower_ci_obs_TOEC90,
                                    end = input$End_sim,
                                    delta = input$delta_sim,
                                    mode = "defulat", 
                                    Utility_score_mode = utility_score_mode,
                                    dosage = custom_strength),
                                  "costum_allometric_WB" = create_costum_allometric_WB_dosing(
                                    data = data()$WHO_data_HT_WT_FFM,
                                    model = model(),
                                    custom_doses = custom_doses,
                                    weight = input$weight,
                                    seed = 9119,
                                    use_loading_dose = use_loading_dose,
                                    fixed_load_dose = loading_dose_fixed,
                                    load_freq = loading_freq,
                                    load_interval = loading_interval,
                                    main_freq = maint_freq,
                                    main_interval = maint_interval,
                                    weight_bands_load = weight_bands,
                                    TSWITCH_val = 0,
                                    upper_limit = upper_ci_obs_AUC,
                                    lower_limit = lower_ci_obs_TOEC90,
                                    end = input$End_sim,
                                    delta = input$delta_sim,
                                    mode = "defulat",
                                    Utility_score_mode = utility_score_mode,
                                    dosage = custom_strength
                                  ))
      
      # Add the generated regimen to the list with its name
      # regimen_list[["Reference"]] <- ref_data
      regimen_list[[regimen_label]] <- generated_regimen
    }
    return(regimen_list)
  })
  
  observeEvent(combined_regimens(), {
    shiny::req(combined_regimens())
    
    updateSelectInput(session,
                      inputId = "select_hazard_sumplot",
                      choices = names(combined_regimens()),
                      selected = names(combined_regimens())[1])
  })
  
  
  observeEvent(combined_regimens(), {
    shiny::req(combined_regimens())
    
    updateSelectInput(session,
                      inputId = "select_sum_plot",
                      choices = names(combined_regimens()),
                      selected = names(combined_regimens())[1])
  })
  
  observeEvent(combined_regimens(), {
    shiny::req(combined_regimens())
    
    updateSelectInput(session,
                      inputId = "select_sum_plot_ref",
                      choices = names(combined_regimens()),
                      selected = names(combined_regimens())[1])
  })
  
  observeEvent(combined_regimens(), {
    shiny::req(combined_regimens())
    
    updateSelectInput(session,
                      inputId = "select_hazard_sumplot_ref",
                      choices = names(combined_regimens()),
                      selected = names(combined_regimens())[1])
  })
  
  #####################################
  ####    target attainment plot   ####
  #####################################
  
  
  output$target_attainment_plot <- renderPlotly({
    shiny::req(input$run_model)
    shiny::req(input$tab_selected == "Pharmacokinetics")
    data_frames <- combined_regimens()
    
    
    AUC_TOEC90_datasets <- lapply(data_frames, function(lst) {
      lst[grep("^AUC_TOEC90_", names(lst))]
    })
    
    renamed_datasets <- rename_datasets_in_list(AUC_TOEC90_datasets)
    
    all_dataframes <- do.call(c, renamed_datasets)
    
    
    combined_df <- purrr::reduce(all_dataframes, full_join, by = c("BINNED_WT", "BIN"))
    
    target_attainment <- combined_df %>%
      gather(TYPE, VALUE, matches("PER_UPPER_|PER_LOWER_"), factor_key = FALSE) %>%
      mutate(
        # Extract regimen name (everything before first underscore "_")
        REGIMEN = sub("_.*", "", TYPE),
        
        # Label for upper/lower limit
        TYPE_LIMIT = if_else(
          str_detect(TYPE, "PER_UPPER_"), 
          "Upper limit of AUC", 
          "Lower limit of T>EC90"
        ),
        
        # Define limit values
        LIMIT = if_else(
          str_detect(TYPE, "PER_UPPER_"),
          paste0("limit =", round(upper_ci_obs_AUC), " ug*day/mL"),
          paste0("limit =", round(lower_ci_obs_TOEC90), " days")
        ),
        
        # Rename TYPE based on regimen
        TYPE = case_when(
          str_detect(TYPE, "Allometric_FFM$") ~ paste(REGIMEN, "Allometric FFM-based"),
          str_detect(TYPE, "conventional$") ~ paste(REGIMEN, "Conventional (mg/kg)"),
          str_detect(TYPE, "Allometric_WB$") ~ paste(REGIMEN, "Allometric WB-based"),
          str_detect(TYPE, "Allometric_WB_custom$") ~ paste(REGIMEN, "Custom Allometric WB-based"),
          TRUE ~ paste(REGIMEN, "Other")
        )
      ) %>%
      # Use row-wise transformation to get the correct DOSE column
      rowwise() %>%
      mutate(
        DOSE = case_when(
          TYPE == paste(REGIMEN, "Allometric FFM-based") & paste0(REGIMEN, "_DOSE_Allometric_FFM") %in% names(pick(everything())) ~ 
            paste0("Dose: ", getElement(pick(everything()), paste0(REGIMEN, "_DOSE_Allometric_FFM")), " mg"),
          
          TYPE == paste(REGIMEN, "Conventional (mg/kg)") & paste0(REGIMEN, "_DOSE_conventional") %in% names(pick(everything())) ~ 
            paste0("Dose: ", getElement(pick(everything()), paste0(REGIMEN, "_DOSE_conventional")), " mg"),
          
          TYPE == paste(REGIMEN, "Allometric WB-based") & paste0(REGIMEN, "_DOSE_Allometric_WB") %in% names(pick(everything())) ~ 
            paste0("Dose: ", getElement(pick(everything()), paste0(REGIMEN, "_DOSE_Allometric_WB")), " mg"),
          
          TYPE == paste(REGIMEN, "Custom Allometric WB-based") & paste0(REGIMEN, "_DOSE_Allometric_WB_custom") %in% names(pick(everything())) ~ 
            paste0("Dose: ", getElement(pick(everything()), paste0(REGIMEN, "_DOSE_Allometric_WB_custom")), " mg"),
          
          TRUE ~ "Other"
        )
      ) %>%
      ungroup()
    
    
    plot <- ggplot(target_attainment, aes(x = BIN, y = VALUE, color = TYPE)) + 
      geom_line(na.rm = TRUE) + 
      geom_point(aes(text = DOSE)) +
      facet_wrap(~TYPE_LIMIT) +
      theme_bw() +  
      labs(x = "Weight (kg)", y = "% of patients within limits") +
      ggtitle("Target achievement of simulated patients' profiles") + 
      theme(legend.position = "bottom", strip.text = element_text(size = 12)) +
      scale_color_discrete(name = "")
    
    ggplotly(plot)
    
  }) 
  
  
  
  output$proposed_doses <- renderUI({
    shiny::req(input$run_model)
    shiny::req(input$select_sum_plot)
    shiny::req(combined_regimens())
    shiny::req(input$tab_selected == "Pharmacokinetics")
    
    data_frames <- combined_regimens()
    
    
    DOSE_datasets <- lapply(data_frames, function(lst) {
      lst[grep("^DOSE", names(lst))]
    })
    
    Dose_sum <- DOSE_datasets[[input$select_sum_plot]][["DOSE_summary"]]
    
    shiny::validate(
      shiny::need(!is.null(Dose_sum), "No dosing information found for the selected regimen.")
    )
    
    # Generate a properly formatted table with kableExtra
    table_html <- knitr::kable(Dose_sum, col.names = c("Weight Band", "Proposed Dose (mg)"), align = c("l", "c")) %>%
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)
    
    HTML(table_html)  # Return as HTML to be displayed
  })
  
  
  
  
  #####################################
  ####        pk profile plot      ####
  #####################################
  
  observeEvent(input$customize_filter, {
    showModal(modalDialog(
      title = "Customize Filter",
      selectInput("filter_type", "Filter by:", 
                  choices = c("Weight" = "WT", "Age" = "AGE", "Clearance" = "CL", "Volume of Distribution" = "V"), 
                  selected = "weight"),
      numericInput("min_val", "Min Value:", value = NA, min = 0, step = NA),
      numericInput("max_val", "Max Value:", value = NA, min = 0, step = NA),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("apply_filter", "Apply Filter")
      )
    ))
  })
  
  # Reactive values to store filter settings
  filter_settings <- reactiveValues(
    filter_type = NULL,
    min_val = NULL,
    max_val = NULL
  )
  
  # Handle the filter application in the server logic
  observeEvent(input$apply_filter, {
    # Validation
    if (is.null(input$filter_type) || input$filter_type == "") {
      showNotification("Please select a filter type.", type = "error")
      return()
    }
    if (input$min_val < 0 || input$max_val <= 0) {
      showNotification("Please enter numeric values greater than 0.", type = "error")
      return()
    }
    if (input$min_val >= input$max_val) {
      showNotification("Min Value should be less than Max Value.", type = "error")
      return()
    }
    
    filter_settings$filter_type <- input$filter_type
    filter_settings$min_val <- input$min_val
    filter_settings$max_val <- input$max_val
    removeModal()
  })
  
  output$pk_profiles_plot <- renderPlotly({
    shiny::req(combined_regimens())
    shiny::req(input$select_sum_plot)
    shiny::req(input$tab_selected == "Pharmacokinetics")
    
    data_frames <- combined_regimens()
    
    pk_profile_data <- lapply(data_frames, function(lst) {
      lst[grep("^pk_model_output", names(lst))]
    })
    
    validate(
      need(pk_profile_data[[input$select_sum_plot]], "No PK profile data found for selected regimen"),
      need(pk_profile_data[[input$select_sum_plot]][["pk_model_output"]], "PK profile data is missing")
    )
    
    pk_profile_data <- pk_profile_data[[input$select_sum_plot]][["pk_model_output"]]
    
    # Apply filter if filter_type is set
    if (!is.null(filter_settings$filter_type)) {
      filter_expr <-switch(filter_settings$filter_type,
                           "WT" = quote(WT),
                           "AGE" = quote(AGE),
                           "CL" = quote(CL),
                           "V" = quote(V),
                           NULL)
      
      pk_profile_data <- pk_profile_data %>%
        filter(!!filter_expr > filter_settings$min_val & !!filter_expr < filter_settings$max_val)
    }
    
    pk_profile_data <- pk_profile_data %>%
      group_by(ID) %>%
      group_by(TIME) %>%
      summarise(
        median = median(CONC_CENT, na.rm = TRUE),
        p5 = quantile(CONC_CENT, 0.05, na.rm = TRUE),
        p95 = quantile(CONC_CENT, 0.95, na.rm = TRUE)) %>%
      mutate(
        TIME = TIME / 24,
        TYPE = names(data_frames)[names(data_frames) == input$select_sum_plot])
    
    plot <- plot_pk_profiles(data = pk_profile_data,
                             log_y = FALSE, 
                             treatment_duration = data_frames[[input$select_sum_plot]][["treatment_duration"]], use_loading_dose = F)
    
    ggplotly(plot)
  }) #%>% bindCache(input$select_sum_plot, combined_regimens(), input$tab_selected == "Pharmacokinetics", filter_settings())
  
  
  output$pk_stat <- render_gt({
    shiny::req(combined_regimens())
    shiny::req(input$select_sum_plot)
    shiny::req(input$tab_selected == "Pharmacokinetics")
    
    data_frames <- combined_regimens()
    
    pk_stat_data <- lapply(data_frames, function(lst) {
      lst[grep("^stat_data", names(lst))]
    })
    validate(
      need(pk_stat_data[[input$select_sum_plot]], "No PK data found for selected regimen"),
      need(pk_stat_data[[input$select_sum_plot]][["stat_data"]], "PK data is missing")
    )
    
    
    
    
    pk_statistic <- pk_stat_data[[input$select_sum_plot]][["stat_data"]] %>% as.data.frame()
    
    column_names <- colnames(pk_statistic)
    
    extracted_numbers <- str_extract(column_names, "\\d+")
    
    # Convert the extracted numbers to numeric
    extracted_numbers <- as.numeric(extracted_numbers)
    
    pk_statistic <- pk_statistic %>%
      rename_with(~ paste0("children (", extracted_numbers [2] , ")"), .cols = starts_with("Children")) %>%
      rename_with(~ paste0("Adults (", extracted_numbers [3], ")"), .cols = starts_with("Adults")) %>%
      rename_with(~ paste0("Total (", extracted_numbers [4], ")"), .cols = starts_with("Total"))
    
    pk_statistic <- pk_statistic %>%
      gt() %>%
      # Add a title and subtitle
      tab_header(
        title = md("**Miltefosine Exposure, Median (IQR)**"),
        subtitle = md(paste0(
          "Summary of exposure parameters for the **",
          input$select_sum_plot,
          "** dosing regimen"
        ))
      ) %>%
      # Add a footnote explaining abbreviations
      tab_footnote(
        footnote = md(paste0(
          "Data are presented as median (IQR). Abbreviations: **AUC0_D14**, ",
          "area under the concentration-time curve from day 0 to day 14; ",
          "**AUC0_EOT**, area under the concentration-time curve until the end of treatment; ",
          "**Cmax**, maximum observed concentration; **T>EC90**, ",
          "time above the EC90 value (10.6 mg/L); ",
          "**TEC90**, time point at which the concentration first reaches the EC90 value; ",
          "**Tmax**, time to reach maximum concentration."
        )),
        locations = cells_title(groups = "subtitle")
      ) %>%
      tab_footnote(
        footnote = "Children: ≤12 years; adults: >12 years.",
        locations = cells_column_labels(columns = starts_with("Children"))
      ) %>%
      tab_footnote(
        footnote = "Children: ≤12 years; adults: >12 years.",
        locations = cells_column_labels(columns = starts_with("Adults"))
      ) %>%
      # Apply custom styling for a professional look
      tab_options(
        table.font.size = "small",          # Set a smaller font size
        heading.align = "center",          # Center-align the title and subtitle
        column_labels.font.size = "medium",# Set medium size for column labels
        table.border.top.color = "black",  # Add a top border for the table
        table.border.bottom.color = "black",
        data_row.padding = px(5),          # Add padding between rows
        row.striping.include_table_body = TRUE, # Add row striping for readability
      ) %>%
      # Add alignment and format
      cols_align(
        align = "center",
        columns = everything()
      ) %>%
      fmt_markdown(columns = everything())
    
    return(pk_statistic)
  })
  
  output$pk_model_stat <- render_gt({
    shiny::req(model)
    
    if (input$model == "L. Verrest (2023)") {
      pk_model_param <- model_parameter_verrest()
      return(pk_model_param)
    } else {
      return(gt::gt(data.frame(Message = "The summary of the selected PK model is not available")))
    }
  })
  
 
  ###########################
  ###   Summary Table     ###
  ###########################
  
  output$combined_output <- renderUI({
    shiny::req(input$run_model)
    shiny::req(combined_regimens())
    shiny::req(input$tab_selected == "Pharmacokinetics")
    
    
    all_regimens <- regimen_data$regimens()
    
    data_frames <- combined_regimens()
    
    Utility_score_mode <-all_regimens[[1]]$optimization_mode
    
    TEC90_datasets <- lapply(data_frames, function(lst) {
      lst[grep("^sumplot", names(lst))]
    })
    
    AUC_TOEC90_datasets <- lapply(data_frames, function(lst) {
      lst[grep("^AUC_TOEC90_", names(lst))]
    })
    
    regimens <- names(data_frames)
    lows <- c()
    upps <- c()
    lows_TEC90 <- c()
    
  if (Utility_score_mode == "regular" || Utility_score_mode == "dosage_form") {
      # ORIGINAL CODE (no dosage optimization)
      for (regimen in regimens) {
        df_current <- AUC_TOEC90_datasets[[regimen]][[1]]
        
        # Calculate percentages (original method)
        low_percent <- round(100 - (sum(df_current[, grep("^COUNT_LOWER", names(df_current))]) / 
                                      sum(df_current[, grep("^COUNT_BIN", names(df_current))]) * 100), 2) 
        upp_percent <- round(100 - (sum(df_current[, grep("^COUNT_UPPER", names(df_current))]) / 
                                      sum(df_current[, grep("^COUNT_BIN", names(df_current))]) * 100), 2)
        
        lows <- c(lows, low_percent)
        upps <- c(upps, upp_percent)
        } 
      }else if (Utility_score_mode == "treatment_shortening"){
        for (regimen in regimens) {
                  # Treatment shortening mode code here
        df_current_TEC90 <- TEC90_datasets[[regimen]][[1]]
        df_current <- AUC_TOEC90_datasets[[regimen]][[1]]
        
        T_EC90_data <- df_current_TEC90 %>% 
          mutate(
            LOWER_TEC90 = ifelse(T_EC90 > 9.175, 1, 0),
          )
        
        basic_summary <- T_EC90_data %>% 
          group_by(BINNED_WT, FLAG) %>% 
          mutate(
            COUNT_Lower_TEC90= sum(LOWER_TEC90), 
            COUNT_BIN = n(),
          ) %>% 
          filter(row_number() == 1) %>% 
          ungroup() %>% 
          mutate(
            PER_LOWER= 100 - ((COUNT_Lower_TEC90) / COUNT_BIN * 100)
          ) %>% 
          arrange(BINNED_WT)
        
        
        upp_percent <- round(100 - (sum(df_current[, grep("^COUNT_UPPER", names(df_current))]) / 
                                      sum(df_current[, grep("^COUNT_BIN", names(df_current))]) * 100), 2)
        
        
        low_percent <- round(100 - (sum(df_current[, grep("^COUNT_LOWER", names(df_current))]) / 
                                      sum(df_current[, grep("^COUNT_BIN", names(df_current))]) * 100), 2) 
        
        low_percent_TEC90 <- round(100 - (sum(basic_summary[, grep("^COUNT_Lower_TEC90", names(basic_summary))]) / 
                                            sum(basic_summary[, grep("^COUNT_BIN", names(basic_summary))]) * 100), 2) 
        
        lows <- c(lows, low_percent)
        upps <- c(upps, upp_percent)
        lows_TEC90 <- c(lows_TEC90, low_percent_TEC90)
        
        }
      }
    
    
    if (Utility_score_mode == "treatment_shortening") {
      result_df <- data.frame(
        regimen_name = regimens,
        lower_limit_TOEC90 = lows,
        upper_limit_AUC = upps,
        lower_limit_TEC90 = lows_TEC90
      ) 
      
      table_html <- result_df %>%
        knitr::kable(
          col.names = c(
            "Regimen Name",
            "Lower Limit of T>EC90 (%)",
            "Upper Limit of AUC (%)",
            "Lower Limit of T≥EC90 ≥3h (%)"),
          align = c('l', 'c', 'c', 'c'),
          digits = 2,
          format = "html"
        ) %>%
        kableExtra::kable_styling(
          bootstrap_options = c("striped", "hover", "condensed"),
          full_width = TRUE,
          position = "center"
        ) %>%
        kableExtra::row_spec(0, bold = TRUE, background = "#f8f9fa") %>%
        kableExtra::column_spec(1, bold = TRUE) %>%
        kableExtra::add_header_above(c(" " = 1, "Target Attainment Metrics" = 3))
      
    } else {
      result_df <- data.frame(
        regimen_name = regimens,
        lower_limit_TEC90 = lows,
        upper_limit_AUC = upps
      ) 
      
      table_html <- result_df %>%
        knitr::kable(
          col.names = c(
            "Regimen Name",
            "Lower Limit of T>EC90 (%)",
            "Upper Limit of AUC (%)"),
          align = c('l', 'c', 'c'),
          digits = 2,
          format = "html"
        ) %>%
        kableExtra::kable_styling(
          bootstrap_options = c("striped", "hover", "condensed"),
          full_width = TRUE,
          position = "center"
        ) %>%
        kableExtra::row_spec(0, bold = TRUE, background = "#f8f9fa") %>%
        kableExtra::column_spec(1, bold = TRUE) %>%
        kableExtra::add_header_above(c(" " = 1, "Target Attainment Metrics" = 2))
    }
    
    # Return as HTML
    HTML(table_html)
  })
  
  
  output$sum_plot <- renderPlot({
    shiny::req(input$run_model)
    shiny::req(input$select_sum_plot)
    shiny::req(input$select_sum_plot_ref)
    shiny::req(combined_regimens())
    shiny::req(input$tab_selected == "Pharmacokinetics")
    

    all_regimens <- regimen_data$regimens()
    
    data_frames <- combined_regimens()
    
    Utility_score_mode <-all_regimens[[1]]$optimization_mode
    
    sumplot_datasets <- lapply(data_frames, function(lst) {
      lst[grep("^sumplot", names(lst))]
    })
    
    # Validate data exists for selected regimen
    shiny::validate(
      shiny::need(!is.null(sumplot_datasets[[input$select_sum_plot]]), 
                  "No data found for the selected regimen."),
      shiny::need(!is.null(sumplot_datasets[[input$select_sum_plot_ref]]), 
                  "No reference data found for the selected regimen.")
    )
    
    
    
    if (Utility_score_mode == "treatment_shortening") {
      plot <- target_attainment_sumplots_treatment_shortening(ref_data = sumplot_datasets[[input$select_sum_plot_ref]][["sumplot"]],
                                                              binned_df = sumplot_datasets[[input$select_sum_plot]][["sumplot"]],
                                                              dosing_strategy = names(data_frames)[names(data_frames) == input$select_sum_plot],
                                                              names(data_frames)[names(data_frames) == input$select_sum_plot_ref])
    }else{
      plot <- target_attainment_sumplots(ref_data = sumplot_datasets[[input$select_sum_plot_ref]][["sumplot"]],
                                       binned_df = sumplot_datasets[[input$select_sum_plot]][["sumplot"]],
                                       dosing_strategy = names(data_frames)[names(data_frames) == input$select_sum_plot],
                                       names(data_frames)[names(data_frames) == input$select_sum_plot_ref])
    
    }
 
    return(plot)
  }) %>% bindCache(input$select_sum_plot,input$select_sum_plot_ref, combined_regimens(),input$tab_selected == "Pharmacokinetics")

  output$attainment_caption <- renderUI({
    shiny::req(input$run_model)
    shiny::req(input$select_sum_plot)
    shiny::req(input$select_sum_plot_ref)
    shiny::req(combined_regimens())
    shiny::req(input$tab_selected == "Pharmacokinetics")
    
    data_frames <- combined_regimens()
    
    caption_text <- HTML(paste0(
      "The yellow shaded area indicates the Tukey's hinges range for <b>",  
      names(data_frames)[names(data_frames) == input$select_sum_plot_ref], 
      "</b>, while the dashed green line represents the target attainment limit.<br>",
      "Box plots show the distribution of AUC and TEC90 values across weight bins for <b>",  
      names(data_frames)[names(data_frames) == input$select_sum_plot], 
      "</b>, excluding outliers.<br>",
      "Numbers above each weight bin indicate the number of individuals in that bin."
    ))
    
    return(caption_text)
  })
  
  output$tec90_limit <- renderValueBox({
    shiny::req(input$run_model)
    shiny::req(combined_regimens())
    valueBox(
      value = paste0(lower_ci_obs_TOEC90, " days"),
      subtitle = "Limit of the lower boundary of T>EC90",
      icon = icon("chart-line"))
  })
  
  output$auc_limit <- renderValueBox({
    shiny::req(input$run_model)
    shiny::req(combined_regimens())
    valueBox(
      value = paste0(upper_ci_obs_AUC, " mg/L*day"),
      subtitle = "Limit of the higher boundary of AUC",
      icon = icon("chart-bar"))
  })

    ###########################
  ###   Pharmacodynamics  ###
  ###########################
 output$hazard_sumplot <- renderPlot({
    shiny::req(input$run_model)
    shiny::req(input$select_hazard_sumplot)
    shiny::req(input$select_hazard_sumplot_ref)
    shiny::req(combined_regimens())
    shiny::req(input$tab_selected == "pd")
    
    
    data_frames <- combined_regimens()
    
    
    sumplot_datasets <- lapply(data_frames, function(lst) {
      lst[grep("^sumplot", names(lst))]
    })
    
    
    # Validate data exists for selected regimen
    shiny::validate(
      shiny::need(!is.null(sumplot_datasets[[input$select_hazard_sumplot]]), 
                  "No data found for the selected regimen."),
      shiny::need(!is.null(sumplot_datasets[[input$select_hazard_sumplot_ref]]), 
                  "No refernce data found for the selected regimen."),
    )
    
    sumplot_datasets[[input$select_hazard_sumplot]][["sumplot"]] <- sumplot_datasets[[input$select_hazard_sumplot]][["sumplot"]] %>%
      rename_with(~ "DOSE", .cols = starts_with("DOSE")) %>%
      mutate(`DOSE (mg)` = as.factor(DOSE))
    
    ref_data <- sumplot_datasets[[input$select_hazard_sumplot_ref]][["sumplot"]]
    
    bounds <- calc_bounds(ref_data, "hazard")
    
    plot <- ggplot(sumplot_datasets[[input$select_hazard_sumplot]][["sumplot"]], 
                   aes(x = BINNED_WT, y = hazard, fill = `DOSE (mg)`)) +
      geom_rect(
        xmin = -Inf, xmax = Inf,
        ymin = bounds[1], ymax = bounds[2],
        fill = "lightyellow", alpha = 0.3
      ) +
      geom_boxplot(alpha = 0.8, outlier.shape = NA) +
      labs(
        title = "Relapse Hazard Ratio",
        subtitle = "Distribution of hazard ratios across weight bands at the end of treatment",
        x = "Weight (kg)",
        y = "Hazard Ratio",
        caption = paste(
          "The yellow shaded area represents the Tukey's hinges range for ", input$select_hazard_sumplot_ref, ".\n",
          "Box plots represent the distribution of relapse hazard ratio across weights for ", input$select_hazard_sumplot, " (excluding outliers)."
        )) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12, color = "gray50"),
        axis.title = element_text(face = "bold", size = 12),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        legend.title = element_text(face = "bold"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_line(color = "gray95"),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
        plot.caption = element_text(hjust = 0)
        
      )
    
    return(plot)
  }) %>% bindCache(input$select_hazard_sumplot,input$select_hazard_sumplot_ref,
                             combined_regimens(),input$tab_selected == "pd")
  
  
  ##############################
  ###  Sensitivity Analysis  ###
  ##############################
  
  
  model_sens <- eventReactive(input$model_sens, {
    shiny::req(input$model_sens)
    input$model_sens
    if (input$model_sens == "L. Verrest (2023)") {
      mod_MF_pk <- mread("helper_functions/Verrest_pk_model.cpp")
      loadso(mod_MF_pk)  
    } else if (input$model_sens == "Upload Own Model") {
      shiny::req(input$pk_model_file_sens)
      model_path <- input$pk_model_file_sens$datapath
      own_model <- mread(model_path)
      loadso(own_model)
    } else {
      NA
    }
  })
  
  regimen_data_sens <- regimen_server_sens("regimen_sens")
  
  combined_regimens_sens <- eventReactive(input$run_sens, {
    shiny::req(input$weight_sens)
    # Get the single saved regimen
    current_regimen <- regimen_data_sens$get_regimen()
    
    validate(
      need(!is.null(input$weight_sens), "Please enter a weight value."))
    
    if (is.null(current_regimen)) {
      showNotification("No regimen found. Please add a regimen before running the model.", type = "warning")
      return(NULL)
    }
    regimen_list <- list()
    
    
    # Extract maintenance dose frequency and interval from the single regimen
    mainta_freq <- current_regimen$maintenance_dose$frequency
    mainta_interval <- current_regimen$maintenance_dose$interval
    
    
    ref_data_sens <- create_allom_dataset(
      data = data()$WHO_data_HT_WT_FFM,
      model = model_sens(),
      weight = input$weight_sens,
      seed = 9119,
      use_loading_dose = FALSE,
      fixed_load_dose = NULL,
      load_freq = NULL,
      load_interval = NULL,
      main_freq = mainta_freq,
      main_interval = mainta_interval,
      weight_bands = NULL, 
      type = "ref", 
      TSWITCH_val = 0,
      upper = NULL,
      lower = NULL,
      end = 200,
      delta = 12,
      mode = "defult")
    
    
    upper_ci_obs_AUC_ref <- ref_data_sens$upper_ci_obs_AUC
    lower_ci_obs_TOEC90_ref <- ref_data_sens$lower_ci_obs_TOEC90
    
    assign("upper_ci_obs_AUC_sens", if (!is.na(input$upp_limit_sens) && input$custom_limit_sens) {
      input$upp_limit_sens
    } else {
      upper_ci_obs_AUC_ref  # Use stored reactive value if available
    }, envir = .GlobalEnv)
    
    assign("lower_ci_obs_TOEC90_sens", if (!is.na(input$lower_limit_sens) && input$custom_limit_sens) {
      input$lower_limit_sens
    } else {
      lower_ci_obs_TOEC90_ref  # Use stored reactive value if available
    }, envir = .GlobalEnv)
    
    
    
    # Process the single regimen
    regimen_label <- current_regimen$name
    
    # Extract values for current regimen
    dosing_strategy <- current_regimen$strategy
    use_loading_dose <- !is.null(current_regimen$loading_dose)
    loading_dose_fixed <- if(use_loading_dose) current_regimen$loading_dose$fixed_dose else NULL
    loading_freq <- if(use_loading_dose) current_regimen$loading_dose$frequency else 0
    loading_interval <- if(use_loading_dose) current_regimen$loading_dose$interval else 0
    maint_freq <- current_regimen$maintenance_dose$frequency
    maint_interval <- current_regimen$maintenance_dose$interval
    weight_bands <- if(use_loading_dose) current_regimen$loading_dose$weight_bands else NULL
    custom_doses <- current_regimen$custom_doses
    
    
    # Generate regimen based on dosing strategy
    generated_regimen <- create_costum_allometric_WB_dosing(
                                  data = data()$WHO_data_HT_WT_FFM,
                                  model = model_sens(),
                                  custom_doses = custom_doses,
                                  weight = input$weight_sens,
                                  seed = 9119,
                                  use_loading_dose = use_loading_dose,
                                  fixed_load_dose = loading_dose_fixed,
                                  load_freq = loading_freq,
                                  load_interval = loading_interval,
                                  main_freq = maint_freq,
                                  main_interval = maint_interval,
                                  weight_bands_load = weight_bands,
                                  TSWITCH_val = 0,
                                  upper_limit = upper_ci_obs_AUC_sens,
                                  lower_limit = lower_ci_obs_TOEC90_sens,
                                  end = 200,
                                  delta = 12,
                                  mode = "defulat")
    

    return(generated_regimen)
  })
  
  
  sens_model <- eventReactive(input$run_sens, {
    shiny::req(combined_regimens_sens())
    shiny::req(input$param_to_analyze)
    shiny::req(input$sensitivity_step)
    
    sens_data <- run_sensitivity_analysis(model_sens(), combined_regimens_sens()$sens_df,
                                          input$param_to_analyze, input$sensitivity_step)
    return(sens_data)
  })
  
  sensitivity_data <- eventReactive(input$run_sens, {
    shiny::req(sens_model())
    
    current_regimen <- regimen_data_sens$get_regimen()
    
    model_data1 <- sens_model()[[paste0(input$param_to_analyze, "_", input$sensitivity_step)]] 
    model_data2 <- sens_model()[[paste0(input$param_to_analyze, "_", "0")]]
    model_data3 <- sens_model()[[paste0(input$param_to_analyze, "_", "-", input$sensitivity_step)]]
    
    if(!is.null(current_regimen$loading_dose)) {
      model_data1 <- model_data1 %>% group_by(ID) %>%
        filter(TIME > 0) %>%
        mutate( AMT = max(AMT))
    } else {
      model_data1 <- model_data1 %>% group_by(ID) %>%
        mutate( AMT = max(AMT))
    }
    
    if(!is.null(current_regimen$loading_dose)) {
      model_data2 <- model_data2 %>% group_by(ID) %>%
        filter(TIME > 0) %>%
        mutate( AMT = max(AMT))
    } else {
      model_data2 <- model_data2 %>% group_by(ID) %>%
        mutate( AMT = max(AMT))
    }
    
    if(!is.null(current_regimen$loading_dose)) {
      model_data3 <- model_data3 %>% group_by(ID) %>%
        filter(TIME > 0) %>%
        mutate( AMT = max(AMT))
    } else {
      model_data3 <- model_data3 %>% group_by(ID) %>%
        mutate( AMT = max(AMT))
    }
    
    
    Allometric_WB_custom_h <- create_bin(model_data1,Time = combined_regimens_sens()$treatment_duration,
                                         input$weight_sens)
    Allometric_WB_custom_n <- create_bin(model_data2, Time = combined_regimens_sens()$treatment_duration,
                                         input$weight_sens)
    Allometric_WB_custom_l <- create_bin(model_data3, Time = combined_regimens_sens()$treatment_duration,
                                         input$weight_sens)
    
    AUC_TOEC90_summary_1 <- AUC_TOEC90_sens(Allometric_WB_custom_h, current_regimen$custom_doses,
                                            upper_ci_obs_AUC_sens, lower_ci_obs_TOEC90_sens)
    AUC_TOEC90_summary_2 <- AUC_TOEC90_sens(Allometric_WB_custom_n, current_regimen$custom_doses,
                                            upper_ci_obs_AUC_sens, lower_ci_obs_TOEC90_sens)
    AUC_TOEC90_summary_3 <- AUC_TOEC90_sens(Allometric_WB_custom_l, current_regimen$custom_doses,
                                            upper_ci_obs_AUC_sens, lower_ci_obs_TOEC90_sens)
    
    DOSE_sum_1 <- AUC_TOEC90_summary_1$DOSE_summary
    DOSE_sum_2 <- AUC_TOEC90_summary_2$DOSE_summary
    DOSE_sum_3 <- AUC_TOEC90_summary_3$DOSE_summary
    
    AUC_TOEC90_summary_1 <- AUC_TOEC90_summary_1$basic_summary
    AUC_TOEC90_summary_2 <- AUC_TOEC90_summary_2$basic_summary
    AUC_TOEC90_summary_3 <- AUC_TOEC90_summary_3$basic_summary 
    
    merged_df <- AUC_TOEC90_summary_1 %>%
      full_join(AUC_TOEC90_summary_2, by = c("BINNED_WT", "BIN", "WT_BAND_Allometric_WB_custom")) %>%
      full_join(AUC_TOEC90_summary_3, by = c("BINNED_WT", "BIN", "WT_BAND_Allometric_WB_custom"))
    
    
    merged_df_dose <- DOSE_sum_1 %>%
      full_join(DOSE_sum_2, by = "WT_BAND_Allometric_WB_custom") %>%
      full_join(DOSE_sum_3, by = "WT_BAND_Allometric_WB_custom")
   
    return(list(merged_df = merged_df, merged_df_dose = merged_df_dose, model_data1 = model_data1, 
                model_data2 = model_data2, model_data3 = model_data3))
  })
  
  output$target_attainment_plot_sens <- renderPlotly({
    shiny::req(input$run_sens)
    target_attainment <- sensitivity_data()$merged_df
    target_attainment %<>%
      gather(TYPE, VALUE, starts_with("PER_UPPER_"), starts_with("PER_LOWER_"), 
             factor_key = FALSE) %>%
      mutate(
        TYPE_LIMIT = if_else(
          startsWith(TYPE, "PER_UPPER_"), 
          "% of patients under upper limit of AUC", 
          "% of patients over lower limit of T>EC90"
        ),
        LIMIT = if_else(
          startsWith(TYPE, "PER_UPPER_"),
          paste0("limit = ", round(upper_ci_obs_AUC_sens), " ug*day/mL"),
          paste0("limit = ", round(lower_ci_obs_TOEC90_sens), " days")
        ),
        TYPE = case_when(
          stringr::str_detect(TYPE, "Allometric_WB_custom_h$") ~ paste0(input$sensitivity_step, "%"),
          stringr::str_detect(TYPE, "Allometric_WB_custom_n$") ~ "no change",
          stringr::str_detect(TYPE, "Allometric_WB_custom_l$") ~ paste0("-", input$sensitivity_step, "%"),
          TRUE ~ "Other"
        ),
        DOSE = case_when(
          stringr::str_detect(TYPE, paste0(input$sensitivity_step, "%")) ~ paste0("Dose: ", DOSE_Allometric_WB_custom_h),
          stringr::str_detect(TYPE, "no change") ~ paste0("Dose: ", DOSE_Allometric_WB_custom_n),
          stringr::str_detect(TYPE, paste0("-", input$sensitivity_step, "%")) ~ paste0("Dose: ", DOSE_Allometric_WB_custom_l),
          TRUE ~ "other"
        )
      )
    
    plot <- ggplot(target_attainment, aes(x = BIN, y = VALUE, color = TYPE)) + 
      geom_line(na.rm = TRUE) + 
      geom_point(aes(text = paste0(DOSE," mg"))) +
      facet_wrap(~TYPE_LIMIT) +
      theme_bw() +  
      labs(x = "Weight in kg", y = "% of patients within limits") +
      ggtitle("Target achievement of simulated patients' profiles") + 
      theme(legend.position = "bottom", strip.text = element_text(size = 12)) +
      scale_color_discrete(name = "")
    
    ggplotly(plot)
  }) 
  
  output$summary_sens <- renderDT({
    shiny::req(input$run_sens)
    target_attainment <- sensitivity_data()$merged_df_dose
    
    target_attainment %>%
      select(WT_BAND_Allometric_WB_custom, DOSE_Allometric_WB_custom_l, DOSE_Allometric_WB_custom_n, 
             DOSE_Allometric_WB_custom_h) %>%
      datatable(rownames = FALSE,
                colnames = c("Weight Band", 
                             paste0("- ", input$sensitivity_step, "%"),
                             "Default",
                             paste0("+ ",input$sensitivity_step, "%")),
                options = list(
                  pageLength = 6,
                  autoWidth = TRUE,
                  dom = 't'
                ))
  })
  
  
  output$pk_profiles_plot_sens <- renderPlotly({
    shiny::req(input$run_sens)
    shiny::req(sensitivity_data())
    
    model_data1 <- sensitivity_data()$model_data1
    model_data2 <- sensitivity_data()$model_data2
    model_data3 <- sensitivity_data()$model_data3
    
    # Summarize and mutate each dataset
    pk_plot_pos <- model_data1 %>%
      group_by(TIME) %>%
      summarise(
        median = median(CONC_CENT, na.rm = TRUE),
        p5 = quantile(CONC_CENT, 0.05, na.rm = TRUE),
        p95 = quantile(CONC_CENT, 0.95, na.rm = TRUE)
      ) %>%
      mutate(TIME = TIME / 24, TYPE = paste0("+", input$sensitivity_step, "%"))
    
    pk_plot_nochange <- model_data2 %>%
      group_by(TIME) %>%
      summarise(
        median = median(CONC_CENT, na.rm = TRUE),
        p5 = quantile(CONC_CENT, 0.05, na.rm = TRUE),
        p95 = quantile(CONC_CENT, 0.95, na.rm = TRUE)
      ) %>%
      mutate(TIME = TIME / 24, TYPE = "Default")
    
    pk_plot_neg <- model_data3 %>%
      group_by(TIME) %>%
      summarise(
        median = median(CONC_CENT, na.rm = TRUE),
        p5 = quantile(CONC_CENT, 0.05, na.rm = TRUE),
        p95 = quantile(CONC_CENT, 0.95, na.rm = TRUE)
      ) %>%
      mutate(TIME = TIME / 24, TYPE = paste0("-", input$sensitivity_step, "%"))
    
    # Combine the datasets
    combined_data <- bind_rows(pk_plot_pos, pk_plot_nochange, pk_plot_neg)
    
    plot <- plot_pk_profiles(combined_data, FALSE,combined_regimens_sens()$treatment_duration,F)
    ggplotly(plot)
    
  })
  
  
  output$pk_stat_sens <- render_gt({
    shiny::req(input$run_sens)
    shiny::req(sensitivity_data())
    sensitivity_data <- sensitivity_data()
    
    
    model_data1 <- pk_summary_sens(sensitivity_data()$model_data1, combined_regimens_sens()$treatment_duration) %>%
      rename("value_pos" = Value)
    model_data2 <- pk_summary_sens(sensitivity_data()$model_data2, combined_regimens_sens()$treatment_duration) %>%
      rename("value_nochange" = Value)
    model_data3 <- pk_summary_sens(sensitivity_data()$model_data3, combined_regimens_sens()$treatment_duration) %>%
      rename("value_neg" = Value)
    
    pk_stat <- list(model_data1,model_data2,model_data3)
    comb_data <- Reduce(function(x, y) merge(x, y,by = c("Metric")), pk_stat)
    
    gt_table <- gt(comb_data) %>%
      tab_header(
        title = " Miltefosine Exposure and Target Attainment",
        subtitle = "Summary of exposure parameters for the sensitivity analysis"
      ) %>%
      cols_label(
        Metric = "",
        value_pos = paste0("+", input$sensitivity_step, " %"),
        value_nochange = "Default",
        value_neg = paste0("-", input$sensitivity_step, " %")
      ) %>%
      tab_footnote(
        footnote = md(paste0(
          "Data are presented as median (IQR). Abbreviations: **AUC0_D14**, ",
          "area under the concentration-time curve from day 0 to day 14; ",
          "**AUC0_EOT**, area under the concentration-time curve until the end of treatment; ",
          "**Cmax**, maximum observed concentration; **T>EC90**, ",
          "time above the EC90 value (10.6 mg/L); ",
          "**TEC90**, time point at which the concentration first reaches the EC90 value; ",
          "**Tmax**, time to reach maximum concentration. +/- ", input$sensitivity_step,
          " denotes sensitivity analysis adjustments")),
        locations = cells_title(groups = "subtitle")
      ) %>%
      tab_options(
        table.font.size = "small",          # Set a smaller font size
        heading.align = "center",          # Center-align the title and subtitle
        column_labels.font.size = "medium",# Set medium size for column labels
        table.border.top.color = "black",  # Add a top border for the table
        table.border.bottom.color = "black",
        data_row.padding = px(5),          # Add padding between rows
        row.striping.include_table_body = TRUE, # Add row striping for readability
      ) %>%
      # Add alignment and format
      cols_align(
        align = "center", 
        columns = everything()
      ) %>%
      fmt_markdown(columns = everything())
    
    return(gt_table)
    
  })
  
  
  
  ################################
  ###  Contraception Analysis  ###
  ################################
  # 
  # model_rep <- eventReactive(input$model_rep, {
  #   shiny::req(input$model_rep)
  #   input$model_rep
  #   if (input$model_rep == "L. Verrest (2023)") {
  #     mod_MF_pk <- mread("helper_functions/Verrest_pk_model.cpp")
  #     loadso(mod_MF_pk)  
  #   } else if (input$model_rep == "Upload Own Model") {
  #     shiny::req(input$pk_model_file_rep)
  #     model_path <- input$pk_model_file_rep$datapath
  #     own_model <- mread(model_path)
  #     loadso(own_model)
  #   } else {
  #     NA
  #   }
  # })
  # 
  # regimen_data_rep <- regimen_server_rep("regimen_rep")
  # 
  # combined_regimens_rep <- eventReactive(input$run_rep, {
  #   # Get the single saved regimen
  #   current_regimen <- regimen_data_rep$regimens()[[1]]
  #   
  #   if (is.null(current_regimen)) {
  #     showNotification("No regimen found. Please add a regimen before running the model.", type = "warning")
  #     return(NULL)
  #   }
  #   regimen_list <- list()
  #   
  #   
  #   # Extract maintenance dose frequency and interval from the single regimen
  #   mainta_freq <- current_regimen$maintenance_dose$frequency
  #   mainta_interval <- current_regimen$maintenance_dose$interval
  #   
  # 
  #   # Process the single regimen
  #   regimen_label <- current_regimen$name
  #   
  #   # Extract values for current regimen
  #   dosing_strategy <- current_regimen$strategy
  #   use_loading_dose <- !is.null(current_regimen$loading_dose)
  #   loading_dose_fixed <- if(use_loading_dose) current_regimen$loading_dose$fixed_dose else NULL
  #   loading_freq <- if(use_loading_dose) current_regimen$loading_dose$frequency else 0
  #   loading_interval <- if(use_loading_dose) current_regimen$loading_dose$interval else 0
  #   maint_freq <- current_regimen$maintenance_dose$frequency
  #   maint_interval <- current_regimen$maintenance_dose$interval
  #   weight_bands <- if(use_loading_dose) current_regimen$loading_dose$weight_bands else NULL
  #   custom_doses <- current_regimen$custom_doses
  #   
  #   
  #   # Generate regimen based on dosing strategy
  #   generated_regimen <- switch(dosing_strategy,
  #                               "Allometric_FFM" = create_allom_dataset(
  #                                 data = data()$WHO_data_HT_WT_FFM,
  #                                 model = model_rep(),
  #                                 weight = 200,
  #                                 seed = 9119,
  #                                 use_loading_dose = use_loading_dose,
  #                                 fixed_load_dose = loading_dose_fixed,
  #                                 load_freq = loading_freq,
  #                                 load_interval = loading_interval,
  #                                 main_freq = maint_freq,
  #                                 main_interval = maint_interval,
  #                                 weight_bands = weight_bands,
  #                                 type = "calc",
  #                                 TSWITCH_val = input$TSWITCH,
  #                                 upper = input$AUC_target,
  #                                 lower = 17,
  #                                 end = 200*24,
  #                                 delta = 24,
  #                                 mode = "rep"),
  #                               "Conventional" = create_lin_dataset(
  #                                 data = data()$WHO_data_HT_WT_FFM,
  #                                 model = model_rep(),
  #                                 weight = 200,
  #                                 seed = 9119,
  #                                 use_loading_dose = use_loading_dose,
  #                                 fixed_load_dose = loading_dose_fixed,
  #                                 load_freq = loading_freq,
  #                                 load_interval = loading_interval,
  #                                 main_freq = maint_freq,
  #                                 main_interval = maint_interval,
  #                                 weight_bands = weight_bands,
  #                                 TSWITCH_val = input$TSWITCH,
  #                                 upper = input$AUC_target,
  #                                 lower = 0,
  #                                 end = 200,
  #                                 delta = 24,
  #                                 mode = "rep"),
  #                               "Allometric_WB" = create_allometric_WB_dosing(
  #                                 data = data()$WHO_data_HT_WT_FFM,
  #                                 model = model_rep(),
  #                                 weight = 200,
  #                                 seed = 9119,
  #                                 use_loading_dose = use_loading_dose,
  #                                 fixed_load_dose = loading_dose_fixed,
  #                                 load_freq = loading_freq,
  #                                 load_interval = loading_interval,
  #                                 main_freq = maint_freq,
  #                                 main_interval = maint_interval,
  #                                 weight_bands = weight_bands,
  #                                 TSWITCH_val = input$TSWITCH,
  #                                 upper = input$AUC_target,
  #                                 lower = 0,
  #                                 end = 200,
  #                                 delta = 24,
  #                                 mode = "rep"),
  #                               "costum_allometric_WB" = create_costum_allometric_WB_dosing(
  #                                 data = data()$WHO_data_HT_WT_FFM,
  #                                 model = model_rep(),
  #                                 custom_doses = custom_doses,
  #                                 weight = 200,
  #                                 seed = 9119,
  #                                 use_loading_dose = use_loading_dose,
  #                                 fixed_load_dose = loading_dose_fixed,
  #                                 load_freq = loading_freq,
  #                                 load_interval = loading_interval,
  #                                 main_freq = maint_freq,
  #                                 main_interval = maint_interval,
  #                                 weight_bands_load = weight_bands,
  #                                 TSWITCH_val = input$TSWITCH,
  #                                 upper_limit = input$AUC_target,
  #                                 lower_limit = 0,
  #                                 end = 200,
  #                                 delta = 24,
  #                                 mode = "rep"
  #                               ))
  #   
  #   return(generated_regimen)
  # })
  # 
  # 
  # output$target_attainment_plot_rep <- renderPlotly({
  #   shiny::req(input$run_rep)
  #   data_frames <- combined_regimens_rep()
  #   
  #   # Extract AUC_TOEC90 datasets from each list element
  #   AUC_TOEC90_dataset <- data_frames[grep("^AUC_TOEC90_", names(data_frames))][[1]]%>%
  #     rename_with(~ "PER_UPPER", starts_with("PER_UPPER")) %>%
  #     rename_with(~ "FLAG", starts_with("FLAG")) %>%
  #     rename_with(~ "DOSE", starts_with("DOSE"))
  #   
  #   # Create the ggplot
  #   p <- ggplot(AUC_TOEC90_dataset, aes(x = BINNED_WT, y = PER_UPPER, group = FLAG)) + 
  #     geom_line(linewidth = 0.8, na.rm = TRUE) + 
  #     geom_point(aes(color = as.factor(DOSE)), size = 3) +
  #     scale_color_discrete(name = "Dose (mg)") +
  #     scale_x_discrete(breaks = c("<30", "40", "50", "60", "65+")) +
  #     labs(
  #       x = "Weight (kg)", 
  #       y = "% of patients within limits",
  #       title = "Target achievement of simulated patients' profiles"
  #     ) + 
  #     theme_bw() +
  #     theme(
  #       legend.position = "bottom",
  #       strip.text = element_text(size = 12),
  #       plot.title = element_text(face = "bold", size = 14),
  #       axis.title = element_text(face = "bold"),
  #       panel.grid.minor = element_blank()
  #     )
  #   
  #   # Display the ggplot
  #   print(p)
  # })
}
