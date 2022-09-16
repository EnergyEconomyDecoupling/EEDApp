# Establishes UI module function
pfuanalysis_dashUI <- function(id) {

  ns <- NS(id)

  fluidRow(

    # use_cicerone(),

    withMathJax(),

        column(width = 10,

               # Energy-GDP Decoupling space box and sidebar
               box(
                 width = 6,
                 # height = 450,
                 title = "Energy-GDP Coupling Space",
                 # id = "exgdp_id_1",
                 closable = FALSE,
                 # status = "warning",
                 solidHeader = FALSE,
                 collapsible = TRUE,
                 enable_sidebar = TRUE,
                 sidebar_width = 25,
                 sidebar_start_open = FALSE,
                 sidebar_background = "#FFFFFF",
                 sidebar_title = "Variables",
                 sidebar_content = tagList(),
                 plotly::plotlyOutput(outputId = ns("exgdp_plot"))

                 ),

               # Energy-Efficiency Rebound Space box and sidebar
               box(
                 width = 6,
                 # height = 450,
                 title = "Energy-Efficiency Rebound Space",
                 # id = "exeta_id_1",
                 closable = FALSE,
                 # status = "warning",
                 solidHeader = FALSE,
                 collapsible = TRUE,
                 enable_sidebar = TRUE,
                 sidebar_width = 50,
                 sidebar_start_open = FALSE,
                 sidebar_background = "#f2f2f2",
                 sidebar_title = "Variables",
                 sidebar_content = tagList(),
                 plotly::plotlyOutput(outputId = ns("exeta_plot"))

                 ),

               # Efficiency-GDP space
               box(
                 width = 6,
                 # height = 450,
                 title = "Efficiency-GDP Coupling Space",
                 # id = "etaagdp_id_1",
                 closable = FALSE,
                 # status = "warning",
                 solidHeader = FALSE,
                 collapsible = TRUE,
                 enable_sidebar = TRUE,
                 sidebar_width = 25,
                 sidebar_start_open = FALSE,
                 sidebar_background = "#FFFFFF",
                 sidebar_title = "Variables",
                 sidebar_content = tagList(),
                 plotlyOutput(outputId = ns("etagdp_plot"))

                 ),

               # Efficiency-GDP space
               box(
                 width = 6,
                 # height = 450,
                 title = "PFU EX GDP Intensity",
                 # id = "etaagdp_id_1",
                 closable = FALSE,
                 # status = "warning",
                 solidHeader = FALSE,
                 collapsible = TRUE,
                 enable_sidebar = TRUE,
                 sidebar_width = 25,
                 sidebar_start_open = FALSE,
                 sidebar_background = "#FFFFFF",
                 sidebar_title = "Variables",
                 sidebar_content = tagList(),
                 plotlyOutput(outputId = ns("pfuex_gdp_plot"))

                 )

               ),

        column(width = 2,

               box(title = "Variables",
                   id = "Variables_ee",
                   # status = "primary",
                   solidHeader = FALSE,
                   closable = FALSE,
                   collapsible = TRUE,
                   width = 12,

                   selectizeInput(inputId = ns("country"),
                                  label = "Country:",
                                  choices = country_options_agg,
                                  options = list(dropdownParent = 'body')),

                   selectizeInput(inputId = ns("iea_andor_mw"),
                                  label = "IEA and/or MW:",
                                  choices = iea.mw_options,
                                  options = list(dropdownParent = 'body')),

                   selectizeInput(inputId = ns("EorX"),
                                  label = "Energy Quantification:",
                                  choices = ex_options,
                                  options = list(dropdownParent = 'body')),

                   selectizeInput(inputId = ns("stage"),
                                  label = "ECC Stage:",
                                  choices = stage_options,
                                  options = list(dropdownParent = 'body')),

                   selectizeInput(inputId = ns("stages"),
                                  label = "Efficiency Stages:",
                                  choices = stages_options,
                                  options = list(dropdownParent = 'body')),

                   selectizeInput(inputId = ns("gross_net"),
                                  label = "Gross or Net:",
                                  choices = gronet_options,
                                  options = list(dropdownParent = 'body')),

                   selectizeInput(inputId = ns("gdpmet"),
                                  label = "GDP Metric:",
                                  choices = gdp_options,
                                  options = list(dropdownParent = 'body')),

                   selectizeInput(inputId = ns("rollavg"),
                                  label = "CAAGR Period:",
                                  choices = caagr_options,
                                  options = list(dropdownParent = 'body')),

                   actionButton(inputId = ns("ee_dash_button"),
                                label = "Help",
                                style = 'margin-top:25px;
                                    margin-left:30px;')

                 )

               )
        )
}

# Establishes the server module function
pfuanalysis_dash <- function(input, output, session,
                             # Inputs
                             EorX,
                             stage,
                             gross_net,
                             country,
                             rollavg,
                             gdpmet,
                             # Outputs
                             ds_plot
                             ) {

################################################################################
# Add tour #
################################################################################

# guide_ee_dash$init()

################################################################################
# Update Events #
################################################################################

  # observeEvent(input$ee_dash_button, {
  #   guide_ee_dash$start()
  # })


################################################################################
# Select Data #
################################################################################

  # Creates reactive data frame for decoupling space plot
  selected_data_ds <- reactive({

    validate(
      need(input$EorX != "", "Please select one Energy quantification"),
      need(input$stage != "", "Please select one Energy Conversion Chain (ECC) stage"),
      need(input$gross_net != "", "Please select one Final demand quantification"),
      need(input$gdpmet != "", "Please select one GDP metric type"),
      need(input$country != "", "Please select atleast one Country"),
      need(input$rollavg != "", "Please select the number of years for average")
    )


    data <- pfuex_econ %>%
      dplyr::filter(Energy.type == input$EorX) %>%
      dplyr::filter(Gross.Net == input$gross_net) %>%
      dplyr::filter(Stage == input$stage) %>%
      dplyr::filter(Country == input$country) %>%
      dplyr::filter(GDP_Metric == input$gdpmet) %>%
      dplyr::filter(IEA.MW == input$iea_andor_mw) %>%
      dplyr::group_by(Country, Method, IEA.MW, Energy.type, Gross.Net, Stage) %>%
      dplyr::mutate(E.dot.CAAGR = calc_roll_caagr(metric = E.dot, period = as.numeric(input$rollavg), direction = "Center") * 100) %>%
      dplyr::mutate(GDP.CAAGR = calc_roll_caagr(metric = GDP, period = as.numeric(input$rollavg), direction = "Center") * 100) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(Year)

    return(data)


  })

  # Creates a dataframe with the selected country, efproduct, and destination
  selected_data_rs <- reactive({
    validate(
      need(input$EorX != "", "Please select one Energy quantification"),
      need(input$stage != "", "Please select one Energy Conversion Chain (ECC) stage"),
      need(input$stages != "", "Please select one efficiency stage"),
      need(input$gross_net != "", "Please select one Final demand quantification"),
      need(input$country != "", "Please select atleast one Country"),
      need(input$rollavg != "", "Please select the number of years for average")
    )



    data <- rebound_space_data_prepped %>%
      dplyr::filter(Energy.type == input$EorX) %>%
      dplyr::filter(Gross.Net == input$gross_net) %>%
      dplyr::filter(Stage == input$stage) %>%
      dplyr::filter(Stages ==input$stages) %>%
      dplyr::filter(Country %in% input$country) %>%
      dplyr::filter(IEA.MW == input$iea_andor_mw) %>%
      dplyr::group_by(Country, Method, IEA.MW, Energy.type, Gross.Net, Stage) %>%
      dplyr::mutate(E.dot.CAAGR = calc_roll_caagr(metric = E.dot, period = as.numeric(input$rollavg), direction = "Center") * 100) %>%
      dplyr::mutate(Eta.CAAGR = calc_roll_caagr(metric = Eta, period = as.numeric(input$rollavg), direction = "Center") * 100) %>%
      dplyr::ungroup()

    return(data)

  })

  # Creates reactive data frame f
  selected_data_etagdp <- reactive({

    data_etagdp <- pfuex_econ %>%
      dplyr::filter(Energy.type == input$EorX) %>%
      dplyr::filter(Stage == input$stage) %>%
      dplyr::filter(Gross.Net == input$gross_net) %>%
      dplyr::filter(Stages == input$stages) %>%
      dplyr::filter(GDP_Metric == input$gdpmet) %>%
      dplyr::filter(Country == input$country) %>%
      dplyr::filter(IEA.MW == input$iea_andor_mw)%>%
      dplyr::mutate(Eta.CAAGR = calc_roll_caagr(metric = Eta,
                                                period = as.numeric(input$rollavg),
                                                direction = "Center") * 100) %>%
      dplyr::mutate(GDP.CAAGR = calc_roll_caagr(metric = GDP,
                                                period = as.numeric(input$rollavg),
                                                direction = "Center") * 100)

    return(data_etagdp)

  })

  # Creates reactive data frame f
  selected_data_pfuex_gdp <- reactive({

    data_pfuex_gdp <- pfuex_econ %>%
      dplyr::filter(Energy.type == input$EorX) %>%
      dplyr::filter(Stage == input$stage) %>%
      dplyr::filter(Gross.Net == input$gross_net) %>%
      dplyr::filter(GDP_Metric == input$gdpmet) %>%
      dplyr::filter(Country == input$country) %>%
      dplyr::filter(IEA.MW == input$iea_andor_mw) %>%
      dplyr::select(-Stages, -Eta, -Eta.i) %>%
      dplyr::distinct()

    return(data_pfuex_gdp)

  })

################################################################################
# Outputs #
################################################################################

################################################################################
# Energy - GDP Decoupling PLot
################################################################################

  output$exgdp_plot <- renderPlotly({

    # Reactively set x-axis and y-axis minimum values
    min_GDP.CAAGR <- min(selected_data_ds()$GDP.CAAGR, na.rm = TRUE)
    min_E.dot.CAAGR <- min(selected_data_ds()$E.dot.CAAGR, na.rm = TRUE)

    # Reactively set x-axis and y-axis maximum values
    max_GDP.CAAGR <- max(selected_data_ds()$GDP.CAAGR, na.rm = TRUE)
    max_E.dot.CAAGR <- max(selected_data_ds()$E.dot.CAAGR, na.rm = TRUE)

    max_values <- c(abs(min_GDP.CAAGR),
                    abs(min_E.dot.CAAGR),
                    abs(max_GDP.CAAGR),
                    abs(max_E.dot.CAAGR))

    max_max <- max(max_values)

      xmin <- max_max * -1.1
      ymin <- max_max * -1.1
      xmax <- max_max * 1.5 * 1.1
      ymax <- max_max * 1.5 * 1.1

    # axis_by <- 2 * round(max_max/10) - 1

    # Hypercoupling position
    hc_y <- ymax
    hc_x <- xmax * 0.5

    # Coupled position
    c_y <- ymax * 0.75
    c_x <- c_y

    # Relative decoupling position
    rd_y <- ymax * 0.35
    rd_x <- xmax * 0.725

    # Decoupled position
    dc_y <- 0
    dc_x <- xmax * 0.75

    # Absolute decoupling position
    ad_y <- ymin - 0.1
    ad_x <- xmax * 0.5

    p <- ggplot2::ggplot(data = selected_data_ds(),
                         mapping = ggplot2::aes(x = GDP.CAAGR, y = E.dot.CAAGR)) +

      ggplot2::geom_point(mapping = ggplot2::aes(colour = Year)) +
      ggplot2::geom_path(size = 0.25,
                         colour = "grey") +

      ggplot2::scale_y_continuous(limits = c(ymin, ymax),
                                  # breaks = seq(ceiling(ymin), ceiling(ymax)), # , by = 1
                                  labels = scales::number_format(accuracy = 1.0)) +

      ggplot2::scale_x_continuous(limits = c(xmin, xmax),
                                  # breaks = seq(ceiling(xmin), ceiling(xmax)), # , by = 1
                                  labels = scales::number_format(accuracy = 1.0)) +

      ggplot2::geom_abline(slope = 1, intercept = 0, colour = "black") +

      ggplot2::geom_hline(data = data.frame(type="A", y=0), mapping=aes(yintercept = y)) +
      ggplot2::geom_vline(data = data.frame(type="A", x=0), mapping=aes(xintercept = x)) +

      ggplot2::geom_segment(data = data.frame(x = 0, y = 0, xend = 0.1, yend = 0.1),
                            mapping = aes(x = x, y = y, xend = xend, yend = yend),
                            colour = "black") +

      ggplot2::geom_segment(data = data.frame(x = 0, y = 0, xend = 0.1, yend = 0),
                            mapping = aes(x = x, y = y, xend = xend, yend = yend),
                            colour = "black") +

      ggplot2::scale_color_gradientn(colors = c("#F0E442", "#D55E00", "#999999")) +

      ggplot2::xlab(paste0(names(gdp_options)[gdp_options == input$gdpmet], " GDP CAAGR [%]")) +
      ggplot2::ylab(paste0(input$stage," Energy CAAGR [%]")) +

      MKHthemes::xy_theme()


    p_plotly <- plotly::ggplotly(p, height = 400, tooltip = c("Year", "GDP.CAAGR", "E.dot.CAAGR")) %>%

      add_annotations(
        x = hc_x,
        y = hc_y,
        text = "Hypercoupling",
        font = list(size = 12),
        opacity = 0.7,
        size = 0.2,
        showarrow = F) %>%

      add_annotations(
        x = c_x,
        y = c_y,
        text = "Coupled",
        font = list(size = 12),
        opacity = 0.7,
        ax = 30,
        ay = 30,
        size = 0.2,
        showarrow = T) %>%

      add_annotations(
        x = rd_x,
        y = rd_y,
        text = "Relative Decoupling",
        font = list(size = 12),
        opacity = 0.7,
        size = 0.2,
        showarrow = F) %>%

      add_annotations(
        x = dc_x,
        y = dc_y,
        text = "Decoupled",
        font = list(size = 12),
        opacity = 0.7,
        size = 0.2,
        showarrow = T) %>%

      add_annotations(
        x = ad_x,
        y = ad_y,
        text = "Absolute Decoupling",
        font = list(size = 12),
        opacity = 0.7,
        size = 0.2,
        showarrow = F)


  })

################################################################################
# Energy - Efficiency Rebound Plot
################################################################################

  output$exeta_plot <- renderPlotly({

    # Reactively set x-axis and y-axis minimum values
    min_Eta.CAAGR <- min(selected_data_rs()$Eta.CAAGR, na.rm = TRUE)
    min_E.dot.CAAGR <- min(selected_data_rs()$E.dot.CAAGR, na.rm = TRUE)

    # Reactively set x-axis and y-axis maximum values
    max_Eta.CAAGR <- max(selected_data_rs()$Eta.CAAGR, na.rm = TRUE)
    max_E.dot.CAAGR <- max(selected_data_rs()$E.dot.CAAGR, na.rm = TRUE)

    max_values <- c(abs(min_Eta.CAAGR),
                    abs(min_E.dot.CAAGR),
                    abs(max_Eta.CAAGR),
                    abs(max_E.dot.CAAGR))

    max_max <- max(max_values)

    xmin <- max_max * -1.1
    ymin <- max_max * -1.1
    xmax <- max_max * 1.5 * 1.1
    ymax <- max_max * 1.5 * 1.1

    # Form coordinates for annotations
    nr_x <- xmax * 0.25
    nr_y <- ymin * 0.85

    pr_x <- xmax * 0.75
    pr_y <- 0 + ymax * 0.05

    b_x <- xmax * 0.25
    b_y <- ymax * 0.65

    # Create ggplot
    p <- ggplot2::ggplot(data = selected_data_rs(), mapping = ggplot2::aes(x = Eta.CAAGR, y = E.dot.CAAGR)) +

      ggplot2::geom_point(mapping = ggplot2::aes(colour = Year)) +

      ggplot2::geom_path(size = 0.25,
                         colour = "grey") +

      ggplot2::scale_y_continuous(limits = c(ymin, ymax),
                                  # breaks = seq(ceiling(ymin), ceiling(ymax), by = 1),
                                  labels = scales::number_format(accuracy = 1.0)) +

      ggplot2::scale_x_continuous(limits = c(xmin, xmax),
                                  # breaks = seq(ceiling(xmin), ceiling(xmax), by = 1),
                                  labels = scales::number_format(accuracy = 1.0)) +

      ggplot2::geom_abline(slope = 1, intercept = 0, colour = "black") +
      ggplot2::geom_abline(slope = -1, intercept = 0, colour = "black") +

      ggplot2::geom_hline(data = data.frame(type="A", y=0), mapping=aes(yintercept = y), linetype="dashed", size = 0.3) +
      ggplot2::geom_vline(data = data.frame(type="A", x=0), mapping=aes(xintercept = x)) +

      ggplot2::geom_segment(data = data.frame(x = 0, y = 0, xend = 0.1, yend = 0.1),
                            mapping = aes(x = x, y = y, xend = xend, yend = yend),
                            colour = "black") +

      ggplot2::geom_segment(data = data.frame(x = 0, y = 0, xend = 0.1, yend = 0),
                            mapping = aes(x = x, y = y, xend = xend, yend = yend),
                            colour = "black") +


      ggplot2::scale_color_gradientn(colors = c("#F0E442", "#D55E00", "#999999")) +

      ggplot2::xlab(paste0(input$stages, " Efficiency CAAGR [%]")) +
      ggplot2::ylab(paste0(input$stage, " Energy CAAGR [%]")) +

      MKHthemes::xy_theme()


    p_plotly <- plotly::ggplotly(p, height = 400, tooltip = c("Year", "Eta_CAAGR", "E.dot_CAAGR")) %>%

      plotly::colorbar(len = 1) %>%

      add_annotations(
        x = nr_x,
        y = nr_y,
        text = "No Rebound",
        font = list(size = 12),
        opacity = 0.7,
        size = 0.2,
        showarrow = F) %>%

      add_annotations(
        x = pr_x,
        y = pr_y,
        text = "Partial Rebound",
        font = list(size = 12),
        opacity = 0.7,
        size = 0.2,
        showarrow = F) %>%

      add_annotations(
        x = b_x,
        y = b_y,
        text = "Backfire",
        font = list(size = 12),
        opacity = 0.7,
        size = 0.2,
        showarrow = F)


  })


################################################################################
# Efficiency - GDP Coupling Plot
################################################################################

  # Plots Efficiency-GDP space
  output$etagdp_plot <- renderPlotly({

    # Reactively set x-axis and y-axis minimum values
    min_Eta.CAAGR <- min(selected_data_etagdp()$Eta.CAAGR, na.rm = TRUE)
    min_GDP.CAAGR <- min(selected_data_etagdp()$GDP.CAAGR, na.rm = TRUE)

    # Reactively set x-axis and y-axis maximum values
    max_Eta.CAAGR <- max(selected_data_etagdp()$Eta.CAAGR, na.rm = TRUE)
    max_GDP.CAAGR <- max(selected_data_etagdp()$GDP.CAAGR, na.rm = TRUE)

    max_values <- c(abs(min_Eta.CAAGR),
                    abs(min_GDP.CAAGR),
                    abs(max_Eta.CAAGR),
                    abs(max_GDP.CAAGR))

    max_max <- max(max_values)

    xmin <- max_max * -1.1
    ymin <- max_max * -1.1
    xmax <- max_max * 1.5 * 1.1
    ymax <- max_max * 1.5 * 1.1


    p <- ggplot2::ggplot(data = selected_data_etagdp()[order(selected_data_etagdp()$Year),],
                         mapping = aes(x = GDP.CAAGR, y = Eta.CAAGR)) +

      ggplot2::geom_point(mapping = aes(colour = Year)) +
      ggplot2::geom_path(size = 0.25,
                         colour = "grey") +

      ggplot2::scale_y_continuous(limits = c(ymin, ymax),
                                  # breaks = seq(ceiling(ymin), ceiling(ymax), by = 1),
                                  labels = scales::number_format(accuracy = 1.0)
      ) +

      ggplot2::scale_x_continuous(limits = c(xmin, xmax),
                                  # breaks = seq(ceiling(xmin), ceiling(xmax), by = 1),
                                  labels = scales::number_format(accuracy = 1.0)
      ) +

      ggplot2::geom_hline(data = data.frame(type="A", y=0), mapping=aes(yintercept = y)) +
      ggplot2::geom_vline(data = data.frame(type="A", x=0), mapping=aes(xintercept = x)) +

      ggplot2::scale_color_gradientn(colors = c("#F0E442", "#D55E00", "#999999")) +

      ggplot2::xlab(paste0(input$gdpmet, " GDP CAAGR [%]")) +
      ggplot2::ylab(paste0(names(gdp_options)[gdp_options == input$gdpmet], " Efficiency CAAGR [%]")) +

      MKHthemes::xy_theme()


    p_plotly <- plotly::ggplotly(p, height = 400, tooltip = c("Year", "Eta.CAAGR", "GDP.CAAGR")) %>%

      plotly::layout(# showlegend = as.logical(input$legend), # showscale
        legend = list(itemclick = TRUE,
                      itemdoubleclick = TRUE))

  })

  ################################################################################
  # PFU EX GDP Intensity Plot
  ################################################################################

  # Plots Efficiency-GDP space
  output$pfuex_gdp_plot <- renderPlotly({

    selected_data_pfuex_gdp <- selected_data_pfuex_gdp()

    p <- ggplot2::ggplot(data = selected_data_pfuex_gdp) +

      ggplot2::geom_line(mapping = ggplot2::aes(x = Year, y = E.dot_intensity, color = Stage)) +

      ggplot2::scale_colour_manual(values = stage.cols) +

      ggplot2::scale_x_continuous(limits = c(1960, 2020), breaks = seq(1960, 2020, by = 10)) +
      ggplot2::scale_y_continuous(limits = c(0, NA)) + # expand = c(0, 0),

      MKHthemes::xy_theme()


    p_plotly <- plotly::ggplotly(p, height = 400, tooltip = c("Year", "E.dot_intensity")) %>%

      plotly::layout(# showlegend = as.logical(input$legend), # showscale
        legend = list(itemclick = TRUE,
                      itemdoubleclick = TRUE))

  })

}



