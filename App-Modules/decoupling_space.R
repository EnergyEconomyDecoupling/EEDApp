# Establishes UI module function
decoupling_spaceUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    tabBox(title = "Energy-Economy Decoupling Space",
           # id = ,
           width = 10,
           height = 950,
           tabPanel(
             title = "Plots",
             plotlyOutput(outputId = ns("ds_plot"))
           )
           # tabPanel( # Add data tab here!
           #
           # )
    ),
    tabBox(width = 2,
           tabPanel(title = "Options",
                    selectizeInput(inputId = ns("EorX"),
                                   label = "Energy Quantification:",
                                   choices = c(Energy = "E", Exergy = "X")
                    ),

                    selectizeInput(inputId = ns("stage"), # Need to change to FUMachine throughout
                                   label = "ECC Stage:",
                                   choices = c(Primary = "Primary",
                                               Final = "Final",
                                               Useful = "Useful")
                                   %>% sort()
                    ),

                    selectizeInput(inputId = ns("gross_net"),
                                   label = "Gross or Net:",
                                   choices = c(Gross = "Gross",
                                               Net = "Net")
                                   %>% sort()
                    ),

                    selectizeInput(inputId = ns("gdpmet"),
                                   label = "GDP Metric:",
                                   choices = c(
                                     `Real` = "rgdpna",
                                     `Expenditure-side` = "rgdpe",
                                     `Output-side` = "rgdpo"
                                   ),
                                   width = "150px",
                                   options = list(dropdownParent = 'body')
                    ),

                    selectizeInput(inputId = ns("country"),
                                   label = "Country:",
                                   choices = country_options,
                                   multiple = TRUE
                                   %>% sort()
                    ),

                    selectizeInput(inputId = ns("rollavg"),
                                   label = "Rolling Average Period:",
                                   choices = c(
                                     "1" = 1,
                                     "3" = 3,
                                     "5" = 5,
                                     "7" = 7
                                   )
                    ),

                    selectizeInput(inputId = ns("facet_scales"),
                                   label = "Facet Scales:",
                                   choices = c(Fixed = "fixed",
                                               Free = "free"),
                                   width = "100%",
                                   options = list(dropdownParent = 'body'))
                    )
           )
  )
}

# Establishes the server module function
decoupling_space <- function(input, output, session,
                             EorX,
                             stage,
                             gross_net,
                             country,
                             rollavg,
                             gdpmet,
                             ds_plot,
                             facet_scales) {

  # Creates a dataframe with the selected country, efproduct, and destination
  selected_data <- reactive({
    validate(
      need(input$EorX != "", "Please select one Energy quantification"),
      need(input$stage != "", "Please select one Energy Conversion Chain (ECC) stage"),
      need(input$gross_net != "", "Please select one Final demand quantification"),
      need(input$gdpmet != "", "Please select one GDP metric type"),
      need(input$country != "", "Please select atleast one Country"),
      need(input$rollavg != "", "Please select the number of years for average")
    )


    data <- pfuex_econ %>%
      dplyr::filter(Energy.type == input$EorX,
                    Gross.Net == input$gross_net,
                    Stage == input$stage,
                    Country %in% input$country,
                    GDP_Metric == input$gdpmet) %>%
      dplyr::group_by(Country, Method, Energy.type, Gross.Net, Stage) %>%
      dplyr::mutate(E.dot.CAAGR = calc_roll_caagr(metric = E.dot, period = as.numeric(input$rollavg), direction = "Center") * 100) %>%
      dplyr::mutate(GDP.CAAGR = calc_roll_caagr(metric = GDP, period = as.numeric(input$rollavg), direction = "Center") * 100) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(Year)

  })


  output$ds_plot <- renderPlotly({

    # Reactively set x-axis and y-axis maximum values
    max_GDP.CAAGR <- max(selected_data()$GDP.CAAGR, na.rm = TRUE)
    max_E.dot.CAAGR <- max(selected_data()$E.dot.CAAGR, na.rm = TRUE)

    if(max_GDP.CAAGR > max_E.dot.CAAGR) {

      xmax <- max_GDP.CAAGR * (1.1)

      ymax <- xmax

    } else {

      xmax <- max_E.dot.CAAGR * (1.1)

      ymax <- xmax

    }

    # Reactively set x-axis and y-axis minimum values
    min_GDP.CAAGR <- min(selected_data()$GDP.CAAGR, na.rm = TRUE)
    min_E.dot.CAAGR <- min(selected_data()$E.dot.CAAGR, na.rm = TRUE)

    # xmin <- min_GDP.CAAGR * (1.1)
    #
    # ymin <- min_EX.CAAGR * (1.1)

    if (min_GDP.CAAGR > 0 | min_E.dot.CAAGR > 0) {

      xmin <- -xmax/4

      ymin <- xmin

    } else if(abs(min_GDP.CAAGR) > abs(min_E.dot.CAAGR)) {

      xmin <- min_GDP.CAAGR * (1.1)

      ymin <- xmin

    } else {

      xmin <- min_E.dot.CAAGR * (1.1)

      ymin <- xmin

    }

    # Hypercoupling position
    hc_y <- ymax
    hc_x <- xmax * 0.5

    # Coupled position
    c_y <- ymax * 0.75
    c_x <- c_y

    # Relative decoupling position
    rd_y <- ymax * 0.35
    rd_x <- xmax * 0.8

    # Decoupled position
    dc_y <- 0
    dc_x <- xmax * 0.75

    # Absolute decoupling position
    ad_y <- ymin - 0.1
    ad_x <- xmax * 0.5

        p <- ggplot2::ggplot(data = selected_data(),
                             # data = selected_data()[order(selected_data()$Year),],
                             mapping = ggplot2::aes(x = GDP.CAAGR, y = E.dot.CAAGR)) +

          ggplot2::geom_point(mapping = ggplot2::aes(colour = Year)) +
          ggplot2::geom_path(size = 0.25,
                             colour = "grey") +

          ggplot2::scale_y_continuous(limits = c(ymin, ymax),
                                      breaks = seq(ceiling(ymin), ceiling(ymax), by = 1),
                                      labels = scales::number_format(accuracy = 1.0)) +

          ggplot2::scale_x_continuous(limits = c(xmin, xmax),
                                      breaks = seq(ceiling(xmin), ceiling(xmax), by = 1),
                                      labels = scales::number_format(accuracy = 1.0)) +

          # ggplot2::scale_y_continuous(limits = c(-6, 12), breaks = seq(-6, 12, by = 1)) +
          # ggplot2::scale_x_continuous(limits = c(-6, 12), breaks = seq(-6, 12, by = 1)) +


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

        ggplot2::xlab("GDP CAAGR [%]") + # [1/year]
        ggplot2::ylab("Final demand CAAGR [%]") + # [1/year]

        ggplot2::facet_wrap(facets = "Country",
                            scales = input$facet_scales,
                            ncol = 2) +

        MKHthemes::xy_theme()


    p_plotly <- plotly::ggplotly(p, height = 850, tooltip = c("Year", "GDP.CAAGR", "E.dot.CAAGR")) %>%
      move_annotations_ds(x_y = -0.05,
                          y_x = -0.05,
                          mar = 80) %>%

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

}
