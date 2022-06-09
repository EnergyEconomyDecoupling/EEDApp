# Establishes UI module function
rebound_dashUI <- function(id) {

  ns <- NS(id)

  fluidRow(

    # use_cicerone(),

    # withMathJax(),

################################################################################
# Table properties #
################################################################################

    # # Sets table style - Rebound Dashboard Box, Table content
    # tags$head(tags$style("#rebound_dashboard table {background-color: white; }",
    #                      media="screen",
    #                      type="text/css")),

    # Sets table style - Rebound results table
    tags$head(tags$style("#results_table table {background-color: white; }",
                         media="screen",
                         type="text/css")),

    # Sets table style - Stages table
    tags$head(tags$style("#stages_table table {background-color: white; }",
                         media="screen",
                         type="text/css")),




################################################################################
# Visualisations #
################################################################################


    # # Rebound graph - faceted
    # box(
    #   width = 10,
    #   # height = 450,
    #   title = "Energy",
    #   id = "rebound_graphbox_faceted",
    #   closable = FALSE,
    #   # status = "warning",
    #   solidHeader = FALSE,
    #   collapsible = FALSE,
    #   enable_sidebar = FALSE,
    #   sidebar_width = 25,
    #   sidebar_start_open = FALSE,
    #   sidebar_background = "#FFFFFF",
    #   sidebar_title = "Variables",
    #   sidebar_content = tagList(),
    #   plotlyOutput(outputId = ns("rebound_graph_plotly"))
    # ),



    # Rebound graph - energy
    box(
      width = 12,
      # height = 450,
      title = "Rebound Dashboard",
      id = "rebound_dashboard",
      closable = FALSE,
      solidHeader = FALSE,
      collapsible = FALSE,
      sidebar = boxSidebar(

        icon = tags$b("Options"),

        id = "rebound_options",

        background = "#FFFFFF",

        width = 100,

            fluidRow(
              column(width = 2,
                     selectizeInput(inputId = ns("Example"),
                                    label = "Example Case:",
                                    choices = c(Car = "Car",
                                                Lamp = "Lamp",
                                                None = "None"),
                                    width = "230px",
                                    options = list(dropdownParent = 'body')
                                    ),

                     textInput(inputId = ns("Case"),
                               label = "Case:",
                               value = "Car",
                               width = "230px")
                     ),
              column(width = 2,
                     textInput(inputId = ns("Original"),
                               label = "Original:",
                               value = "Ford Fusion",
                               width = "230px"
                     ),

                     textInput(inputId = ns("Upgrade"),
                               label = "Upgrade:",
                               value = "Ford Fusion Hybrid",
                               width = "230px")
                     ),
              column(width = 2,
                     textInput(inputId = ns("service_unit"),
                               label = "Service Unit:",
                               value = "mile",
                               width = "230px"
                     ),

                     textInput(inputId = ns("energy_engr_unit"),
                               label = "Energy Unit:",
                               value = "gal",
                               width = "230px")
                     ),

              column(width = 2,

                     numericInput(inputId = ns("k"),
                                  label = HTML("Macro Factor (k) [-]:"),
                                  value = 1,
                                  width = "230px"
                     ),

                     numericInput(inputId = ns("p_E_engr_units"),
                                  label = HTML("Price of energy [?]:"),
                                  value = 2.21,
                                  width = "230px")
                     ),

              column(width = 2,
                     numericInput(inputId = ns("MJ_energy_engr_unit"),
                                  label = HTML("MJ/energy_eng_unit:"),
                                  value = 126.62163,
                                  width = "230px"),

                     numericInput(inputId = ns("I_E"),
                                  label = HTML("Economy energy intensity [MJ/$]:"),
                                  value = 3.38933906306065,
                                  width = "230px")
                     )


            ), # Closes fluid row for Base Parameters

            fluidRow(
              column(width = 3,


              numericInput(inputId = ns("e_qs_M"),
                           label = HTML("Income elasticity of energy service consumption [-]:"),
                           value = 1,
                           width = "230px"
                           )

              ),

              column(width = 3,

              numericInput(inputId = ns("e_qo_M"),
                           label = HTML("Income elasticity of other goods consumption [-]:"),
                           value = 1,
                           width = "230px"
                           )
              ),

              column(width = 6,

              numericInput(inputId = ns("e_qs_ps_UC"),
                           label = HTML("Uncompensated Marshallian energy service price elasticity of energy service [-]:"),
                           value = -0.1,
                           width = "230px"
                           )
              )

              ),



            tags$hr(),

            tags$h4("Original device information"),

            # ???
            splitLayout(

              style = "vertical-align: middle;
                       horizontal-align: left;",

              cellArgs = list(style = "padding: 0px"),

              numericInput(inputId = ns("eta_engr_units_orig"),
                           label = HTML("Original energy service efficiency [?]:"),
                           value = 25,
                           width = "150px"),

              numericInput(inputId = ns("t_own_orig"),
                           label = HTML("Original ownership time [years]:"),
                           value = 42,
                           width = "150px"),

              numericInput(inputId = ns("C_dot_md_orig"),
                           label = HTML("Original maintenance and disposal expenditure rate [$/year]:"),
                           value = 2861.13425677328,
                           width = "150px"),

              numericInput(inputId = ns("E_emb_orig"),
                           label = HTML("Original embodied energy [MJ]:"),
                           value = 34000,
                           width = "150px")

            ), # Closes split layout

            splitLayout(

              style = "vertical-align: middle;
                       horizontal-align: left;",

              cellArgs = list(style = "padding: 0px"),

              numericInput(inputId = ns("t_life_orig"),
                           label = HTML("Original lifetime [years]: <br/> "),
                           value = 14,
                           width = "150px"),

              numericInput(inputId = ns("C_cap_orig"),
                           label = HTML("Original net capital expenditure [$]:"),
                           value = 28216.1,
                           width = "150px"),

              numericInput(inputId = ns("q_dot_s_orig"),
                           label = HTML("Original energy service consumption rate [?]:"),
                           value = 14425,
                           width = "150px"),

              numericInput(inputId = ns("M_dot_orig"),
                           label = HTML("Original disposable income rate [$/year]:"),
                           value = 27401.2776930295,
                           width = "150px")

            ), # Closes split layout

            tags$hr(),

            tags$h4("Upgraded device information"),

            splitLayout(

              style = "vertical-align: middle;
                       horizontal-align: left;",

              cellArgs = list(style = "padding: 0px"),

              numericInput(inputId = ns("eta_engr_units_star"),
                           label = HTML("Upgraded energy service efficiency [?]: <br/> "),
                           value = 42,
                           width = "200px"
                           ),

              numericInput(inputId = ns("t_own_star"),
                           label = HTML("Upgraded ownership time [years]: <br/> "),
                           value = 7,
                           width = "200px"
                           ),

              numericInput(inputId = ns("C_dot_md_star"),
                           label = HTML("Upgraded maintenance and disposal <br/>
                                        expenditure rate [$/year]:"),
                           value = 2774.66812079095,
                           width = "200px"
                           ),

              numericInput(inputId = ns("E_emb_star"),
                           label = HTML("Upgraded embodied energy [MJ]: <br/> "),
                           value = 40000,
                           width = "150px"
                           )

              ), # Closes split layout

            splitLayout(

              numericInput(inputId = ns("t_life_star"),
                           label = HTML("Upgraded lifetime [years]: <br/> "),
                           value = 14,
                           width = "150px"
              ),

              numericInput(inputId = ns("C_cap_star"),
                           label = HTML("Upgraded net capital expenditure [$]: <br/> "),
                           value = 27523.4,
                           width = "150px"
                           )
              ) # Closes split layout

      ), # Close sidebar

      fluidRow(
        column(4,
               tags$h4("Energy"),
               align = "center",
               plotOutput(outputId = ns("rebound_graph_energy_i")),
               splitLayout(
                 checkboxInput(inputId = ns("energy_i"),
                               label = "Indexed",
                               value =  FALSE),
                 checkboxInput(inputId = ns("energy_guide"),
                               label = "Guidelines",
                               value = TRUE),
                 cellWidths = c("50%", "50%"),
                 cellArgs = list(style = "padding: 0px",
                                 align = "center")
                 )
               ),
        column(4,
               tags$h4("Expenditure"),
               align = "center",
               plotOutput(outputId = ns("rebound_graph_expenditure_i")),
               splitLayout(
                 checkboxInput(inputId = ns("expenditure_i"),
                               label = "Indexed",
                               value =  FALSE),
                 checkboxInput(inputId = ns("expenditure_guide"),
                               label = "Guidelines",
                               value = TRUE),
                 cellWidths = c("50%", "50%"),
                 cellArgs = list(style = "padding: 0px",
                                 align = "center")
                 )
               ),
        column(4,
               tags$h4("Consumption"),
               align = "center",
               plotOutput(outputId = ns("rebound_graph_preferences_i")),
               splitLayout(
                 checkboxInput(inputId = ns("preferences_guide"),
                               label = "Guidelines",
                               value = TRUE),
                 cellWidths = c("50%", "50%"),
                 cellArgs = list(style = "padding: 0px",
                                 align = "center")
                 )
               )
      ),

      splitLayout(

        style = "vertical-align: middle;
                 horizontal-align: left;",

        cellWidths = c("65%", "35%"),

        cellArgs = list(style = "padding: 0px",
                        align = "center"
                        )#,

        # tableOutput(outputId = ns("stages_table")),
        #
        # tableOutput(outputId = ns("results_table"))

      )

      # plotOutput(outputId = ns("rebound_graph_energy_i"))

    ), # Close box
#
#     # Rebound graph - expenditure
#     box(
#       width = 4,
#       # height = 450,
#       title = "Expenditure",
#       id = "rebound_graphbox_expenditure_i",
#       closable = FALSE,
#       # status = "warning",
#       solidHeader = FALSE,
#       collapsible = FALSE,
#       plotOutput(outputId = ns("rebound_graph_expenditure_i"))
#     ),
#
#     # Rebound graph - preferences
#     box(
#       width = 4,
#       # height = 450,
#       title = "Consumption",
#       id = "rebound_graphbox_preferences_i",
#       closable = FALSE,
#       # status = "warning",
#       solidHeader = FALSE,
#       collapsible = FALSE,
#       enable_sidebar = FALSE,
#       sidebar_width = 25,
#       sidebar_start_open = FALSE,
#       sidebar_background = "#FFFFFF",
#       sidebar_title = "Variables",
#       sidebar_content = tagList(),
#       plotOutput(outputId = ns("rebound_graph_preferences_i"))
#     ),

    # Stages table
    box(
      width = 8,
      height = 500,
      title = "Stages Table",
      id = "stages_table",
      closable = FALSE,
      # status = "warning",
      solidHeader = FALSE,
      collapsible = FALSE,
      enable_sidebar = FALSE,
      tableOutput(outputId = ns("stages_table"))
    ),

    # Rebound results table
    box(
      width = 4,
      height = 500,
      title = "Rebound Results Table",
      id = "results_table",
      closable = FALSE,
      # status = "warning",
      solidHeader = FALSE,
      collapsible = FALSE,
      enable_sidebar = FALSE,
      tableOutput(outputId = ns("results_table"))
    ),



    )

} # Close UI module

# Establishes the server module function
rebound_dash <- function(input, output, session,

                         Example,
                         Case,
                         Original,
                         Upgrade,
                         service_unit,
                         energy_engr_unit,
                         MJ_energy_engr_unit,
                         I_E,
                         k,
                         p_E_engr_units,
                         e_qs_ps_UC,
                         e_qs_M,
                         e_qo_M,
                         eta_engr_units_orig,
                         eta_engr_units_star,
                         q_dot_s_orig,
                         M_dot_orig,
                         C_cap_orig,
                         t_own_orig,
                         C_cap_star,
                         t_own_star,
                         C_dot_md_orig,
                         C_dot_md_star,
                         E_emb_orig,
                         t_life_orig,
                         E_emb_star,
                         t_life_star

                         ) {

################################################################################
# Add tour #
################################################################################


################################################################################
# Update Events #
################################################################################


################################################################################
# Adds Examples #
################################################################################

  observeEvent(input$Example,  {
    req(input$Example)

    if (input$Example == "Car") {

      Case_example <- c("Car")
      Original_example <- c("Ford Fusion")
      Upgrade_example <- c("Ford Fusion Hybrid")
      service_unit_example <- c("mile")
      energy_engr_unit_example <- c("gal")
      MJ_energy_engr_unit_example <- 126.62163
      I_E_example <- 3.38933906306065
      k_example <- 1
      p_E_engr_units_example <- 2.21
      e_qs_ps_UC_example <- -0.1
      e_qs_M_example <- 1
      e_qo_M_example <- 1
      eta_engr_units_orig_example <- 25
      eta_engr_units_star_example <- 42
      q_dot_s_orig_example <- 14425
      M_dot_orig_example <- 27401.2776930295
      C_cap_orig_example <- 28216.1
      t_own_orig_example <- 7
      C_cap_star_example <- 27523.4
      t_own_star_example <- 7
      C_dot_md_orig_example <- 2861.13425677328
      C_dot_md_star_example <- 2774.66812079095
      E_emb_orig_example <- 34000
      t_life_orig_example <- 14
      E_emb_star_example <- 40000
      t_life_star_example <- 14

    }

    if (input$Example == "Lamp") {

      Case_example <- c("Lamp")
      Original_example <- c("Incandescent")
      Upgrade_example <- c("LED")
      service_unit_example <- c("lm-hr")
      energy_engr_unit_example <- c("W-hr")
      MJ_energy_engr_unit_example <- 0.0036
      I_E_example <- 3.38933906306065
      k_example <- 1
      p_E_engr_units_example <- 0.0001355
      e_qs_ps_UC_example <- -0.4
      e_qs_M_example <- 1
      e_qo_M_example <- 1
      eta_engr_units_orig_example <- 8.8333333
      eta_engr_units_star_example <- 81.8
      q_dot_s_orig_example <- 580350
      M_dot_orig_example <- 27401.2776930295
      C_cap_orig_example <- 1.88
      t_own_orig_example <- 1.8
      C_cap_star_example <- 1.21
      t_own_star_example <- 10
      C_dot_md_orig_example <- 0
      C_dot_md_star_example <- 0
      E_emb_orig_example <- 2.2
      t_life_orig_example <- 1.8
      E_emb_star_example <- 6.5
      t_life_star_example <- 10


    }

    if (input$Example == "None") {

      Case_example <- c("")
      Original_example <- c("")
      Upgrade_example <- c("")
      service_unit_example <- c("")
      energy_engr_unit_example <- c("")
      MJ_energy_engr_unit_example <- ""
      I_E_example <- ""
      k_example <- ""
      p_E_engr_units_example <- ""
      e_qs_ps_UC_example <- ""
      e_qs_M_example <- ""
      e_qo_M_example <- ""
      eta_engr_units_orig_example <- ""
      eta_engr_units_star_example <- ""
      q_dot_s_orig_example <- ""
      M_dot_orig_example <- ""
      C_cap_orig_example <- ""
      t_own_orig_example <- ""
      C_cap_star_example <- ""
      t_own_star_example <- ""
      C_dot_md_orig_example <- ""
      C_dot_md_star_example <- ""
      E_emb_orig_example <- ""
      t_life_orig_example <- ""
      E_emb_star_example <- ""
      t_life_star_example <- ""
    }


    # Update input values based on examples
    updateTextInput(session,
                    inputId = "Case",
                    value = Case_example)

    updateTextInput(session,
                    inputId = "Original",
                    value = Original_example)

    updateTextInput(session,
                    inputId = "Upgrade",
                    value = Upgrade_example)

    updateTextInput(session,
                    inputId = "service_unit",
                    value = service_unit_example)

    updateTextInput(session,
                    inputId = "energy_engr_unit",
                    value = energy_engr_unit_example)

    updateNumericInput(session,
                       inputId = "MJ_energy_engr_unit",
                       value = MJ_energy_engr_unit_example)

    updateNumericInput(session,
                       inputId = "I_E",
                       value = I_E_example)

    updateNumericInput(session,
                       inputId = "k",
                       value = k_example)

    updateNumericInput(session,
                       inputId = "p_E_engr_units",
                       value = p_E_engr_units_example)

    updateNumericInput(session,
                       inputId = "e_qs_ps_UC",
                       value = e_qs_ps_UC_example)

    updateNumericInput(session,
                       inputId = "e_qs_M",
                       value = e_qs_M_example)

    updateNumericInput(session,
                       inputId = "e_qo_M",
                       value = e_qo_M_example)

    updateNumericInput(session,
                       inputId = "eta_engr_units_orig",
                       value = eta_engr_units_orig_example)

    updateNumericInput(session,
                       inputId = "eta_engr_units_star",
                       value = eta_engr_units_star_example)

    updateNumericInput(session,
                       inputId = "q_dot_s_orig",
                       value = q_dot_s_orig_example)

    updateNumericInput(session,
                       inputId = "M_dot_orig",
                       value = M_dot_orig_example)

    updateNumericInput(session,
                       inputId = "C_cap_orig",
                       value = C_cap_orig_example)

    updateNumericInput(session,
                       inputId = "t_own_orig",
                       value = t_own_orig_example)

    updateNumericInput(session,
                       inputId = "C_cap_star",
                       value = C_cap_star_example)

    updateNumericInput(session,
                       inputId = "t_own_star",
                       value = t_own_star_example)

    updateNumericInput(session,
                       inputId = "C_dot_md_orig",
                       value = C_dot_md_orig_example)

    updateNumericInput(session,
                       inputId = "C_dot_md_star",
                       value =  C_dot_md_star_example)

    updateNumericInput(session,
                       inputId = "E_emb_orig",
                       value = E_emb_orig_example)

    updateNumericInput(session,
                       inputId = "t_life_orig",
                       value = t_life_orig_example)

    updateNumericInput(session,
                       inputId = "E_emb_star",
                       value = E_emb_star_example)

    updateNumericInput(session,
                       inputId = "t_life_star",
                       value = t_life_star_example)

  })


################################################################################
# Select Data #
################################################################################


  eeu_data_empty <- tibble::tibble(
    Case = as.character(),
    Original = as.character(),
    Upgrade = as.character(),
    service_unit = as.character(),
    energy_engr_unit = as.character(),
    `MJ/energy_engr_unit` = as.double(),
    I_E = as.double(),
    k = as.double(),
    p_E_engr_units = as.double(),
    e_qs_ps_UC = as.double(),
    e_qs_M = as.double(),
    e_qo_M = as.double(),
    eta_engr_units_orig = as.double(),
    eta_engr_units_star = as.double(),
    q_dot_s_orig = as.double(),
    M_dot_orig = as.double(),
    C_cap_orig = as.double(),
    t_own_orig = as.double(),
    C_cap_star = as.double(),
    t_own_star = as.double(),
    C_dot_md_orig = as.double(),
    C_dot_md_star = as.double(),
    E_emb_orig = as.double(),
    t_life_orig = as.double(),
    E_emb_star = as.double(),
    t_life_star = as.double())


  eeu_data_full <- reactive({

    # Data entered by user
    # input$Case, # exclude Case if not nullifying in mutate below
    eeu_data_empty %>%
      tibble::add_row(
        Case = input$Case,
        Original = input$Original,
        Upgrade = input$Upgrade,
        service_unit = input$service_unit,
        energy_engr_unit = input$energy_engr_unit,
        `MJ/energy_engr_unit` = input$MJ_energy_engr_unit,
        I_E = input$I_E,
        k = input$k,
        p_E_engr_units = input$p_E_engr_units,
        e_qs_ps_UC = input$e_qs_ps_UC,
        e_qs_M = input$e_qs_M,
        e_qo_M = input$e_qo_M,
        eta_engr_units_orig = input$eta_engr_units_orig,
        eta_engr_units_star = input$eta_engr_units_star,
        q_dot_s_orig = input$q_dot_s_orig,
        M_dot_orig = input$M_dot_orig,
        C_cap_orig = input$C_cap_orig,
        t_own_orig = input$t_own_orig,
        C_cap_star = input$C_cap_star,
        t_own_star = input$t_own_star,
        C_dot_md_orig = input$C_dot_md_orig,
        C_dot_md_star = input$C_dot_md_star,
        E_emb_orig = input$E_emb_orig,
        t_life_orig = input$t_life_orig,
        E_emb_star = input$E_emb_star,
        t_life_star = input$t_life_star
      ) %>%

      ReboundTools::rebound_analysis()

  })


################################################################################
# Outputs #
################################################################################

  ## Graphs

  # # Plots Rebound plotly graph - Faceted plots
  # output$rebound_graph_plotly <- renderPlotly({
  #
  #
  #
  #   rebound_graph <- eeu_data_full() %>%
  #
  #     dplyr::mutate(Reference = "Placeholder", .before = "Case") %>%
  #
  #     ReboundTools::path_graphs(indexed = TRUE,
  #                               cases = input$Case#,
  #                               # grid_types = NULL
  #                               ) +
  #
  #     ggplot2::facet_wrap(facets = ReboundTools::graph_df_colnames$graph_type_col) +
  #     ggplot2::xlim(0.5, 1.2) +
  #     ggplot2::ylim(0.99, 1.08) +
  #     ggplot2::theme_classic()
  #
  #
  #   rebound_graph_plotly <- rebound_graph %>% plotly::ggplotly()
  #
  #   return(rebound_graph_plotly)
  #
  # })


  # Plots Rebound plotly graph - Energy indexed
  output$rebound_graph_energy_i <- renderPlot({

    if(input$energy_guide == TRUE){

      grid_type <- ReboundTools::graph_types

    } else {

      grid_type <- NULL

    }

    rebound_graph <- eeu_data_full() %>%

      dplyr::mutate(Reference = "Placeholder", .before = "Case") %>%

      ReboundTools::path_graphs(indexed = as.logical(input$energy_i),
                                grid_types = grid_type,
                                graph_types =  ReboundTools::graph_types$energy,
                                cases = input$Case) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.title.x = element_text(size = 16),
                     axis.title.y = element_text(size = 16),
                     axis.text.x = element_text(size = 12),
                     axis.text.y = element_text(size = 12))

    return(rebound_graph)


    # rebound_graph_plotly <- rebound_graph %>%
    #   plotly::ggplotly() %>%
    #   plotly::layout(showLegend = FALSE,
    #                  legend = list(orientation = "h",   # show entries horizontally
    #                                xanchor = "center",  # use center of legend as anchor
    #                                x = 0.5))             # put legend in center of x-axis
    #
    # return(rebound_graph_plotly)

  })


  # Plots Rebound plotly graph - Expenditure indexed
  output$rebound_graph_expenditure_i <- renderPlot({

    if(input$expenditure_guide == TRUE){

      grid_type <- ReboundTools::graph_types

    } else {

      grid_type <- NULL

    }

    rebound_graph <- eeu_data_full() %>%

      dplyr::mutate(Reference = "Placeholder", .before = "Case") %>%

      ReboundTools::path_graphs(indexed = as.logical(input$expenditure_i),
                                grid_types = grid_type,
                                graph_types = ReboundTools::graph_types$expenditure,
                                cases = input$Case) +
      ggplot2::theme_classic() +

      ggplot2::theme(axis.title.x = element_text(size = 16),
                     axis.title.y = element_text(size = 16),
                     axis.text.x = element_text(size = 12),
                     axis.text.y = element_text(size = 12))

    return(rebound_graph)


    # rebound_graph_plotly <- rebound_graph %>%
    #   plotly::ggplotly() %>%
    #   plotly::layout(legend = list(orientation = "h",   # show entries horizontally
    #                                xanchor = "center",  # use center of legend as anchor
    #                                x = 0.5))             # put legend in center of x-axis
    #
    # return(rebound_graph_plotly)

  })


  # Plots Rebound plotly graph - Consumption indexed
  output$rebound_graph_preferences_i <- renderPlot({

    if(input$preferences_guide == TRUE){

      grid_type <- ReboundTools::graph_types

    } else {

      grid_type <- NULL

    }

    rebound_graph <- eeu_data_full() %>%

      dplyr::mutate(Reference = "Placeholder", .before = "Case") %>%

      ReboundTools::path_graphs(indexed = TRUE,
                                grid_types = grid_type,
                                graph_types = ReboundTools::graph_types$consumption,
                                cases = input$Case) +
      ggplot2::theme_classic() +

      ggplot2::theme(axis.title.x = element_text(size = 16),
                     axis.title.y = element_text(size = 16),
                     axis.text.x = element_text(size = 12),
                     axis.text.y = element_text(size = 12))

    return(rebound_graph)


    # rebound_graph_plotly <- rebound_graph %>%
    #   plotly::ggplotly() %>%
    #   plotly::layout(legend = list(orientation = "h",   # show entries horizontally
    #                                xanchor = "center",  # use center of legend as anchor
    #                                x = 0.5))             # put legend in center of x-axis
    #
    # return(rebound_graph_plotly)

  })



  ## Tables

  # Creates rebound results table
  output$results_table <- renderTable({

    rebound_results_table_data <- eeu_data_full() %>%
      dplyr::mutate(
        Case = NULL
      ) %>%
      ReboundTools::rebound_results_table(include_subtotals = FALSE,
                                          escape_latex = TRUE,
                                          label = "tab:results",
                                          digits = 1,
                                          align = "rrr") %>%

      dplyr::mutate(`Rebound term` = stringr::str_replace_all(`Rebound term`, "\\$", "%%")) %>%

      dplyr::mutate(`Rebound effect` = list("Direct emplacement",
                                            "Indirect embodied energy",
                                            "Indirect capital expenditure",
                                            "Indirect maintenance and disposal",
                                            # "Emplacement effect",
                                            "Direct substitution",
                                            "Indirect substitution",
                                            # "Substitution",
                                            "Direct income",
                                            "Indirect income",
                                            # "Income",
                                            "Indirect macro",
                                            # "Sum of direct",
                                            # "Sum of indirect",
                                            "Total"
                                            ), .before = "Rebound term")
      # dplyr::select(-`Rebound term`) %>%
      # magrittr::set_colnames(c("Rebound effect", "Value [%]"))

    rebound_results_table_data


  })

  # Creates rebound stages table
  output$stages_table <- renderTable({

    digs <- matrix(c(rep(1, 7),  # eta_engr_units
                     rep(3, 7),  # eta
                     rep(3, 7),  # p_s
                     rep(0, 7),  # q_dot_s
                     rep(0, 7),  # E_dot_s
                     rep(0, 7),  # E_dot_emb
                     rep(0, 7),  # C_dot_s
                     rep(0, 7),  # C_dot_cap
                     rep(0, 7),  # C_dot_md
                     rep(0, 7),  # C_dot_o
                     rep(0, 7),  # N_dot
                     rep(0, 7)), # M_dot
                   nrow = 12, ncol = 7, byrow = TRUE)

    rebound_stages_table_data <- eeu_data_full() %>%
      dplyr::mutate(
        Case = NULL
      ) %>%
      ReboundTools::stages_table(digits = digs,
                                 escape_latex = TRUE,
                                 latex_vars = ReboundTools::latex_key_analysis_vars,
                                 align = "lrrrrrr") %>%

      magrittr::set_colnames(c("Units", "%%\\circ%% (orig)", "%%*%% (star)", "%%\\wedge%% (hat)", "%%-%% (bar)", "%%\\sim%% (tilde)")) %>%

      dplyr::mutate(var = stringr::str_extract(Units, "\\$(.*?)\\$"), .before = "Units") %>%

      dplyr::mutate(var = stringr::str_replace_all(var, "\\$", "%%")) %>%

      dplyr::mutate(Units = stringr::str_replace_all(Units, "\\$(.*?)\\$\\s", "")) %>%

      dplyr::mutate(Units = stringr::str_remove(Units, "\\\\")) %>%

      dplyr::mutate(`Key analysis variables` = list("Energy Service Efficiency (eng. units)",
                                                    "Energy Service Efficiency",
                                                    "Energy service price",
                                                    "Rate of Energy Service Consumption",
                                                    "Rate of Final Energy Consumption",
                                                    "Rate of Embodied Energy Demand by the Device",
                                                    "Expenditure Rate of Energy Consumption by the Device",
                                                    "Capital expenditure rate of the Device",
                                                    "Maintenance and Disposal Expenditure Rate of the Device",
                                                    "Other Goods Consumption Rate",
                                                    "Freed Cash Rate",
                                                    "Real Income"), .before = "var")


    rebound_stages_table_data
    # return(rebound_stages_table_data)


  })

} # Close server module



