# Loads required packages
library(tidyverse)

# Establishes UI module function
rebound_spaceUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    tabBox(title = "Rebound Space",
           # id = ,
           width = 9,
           height = 950,
           tabPanel(
             title = "Plots",
             plotlyOutput(outputId = ns("rs_plot"))
           )
           # tabPanel( # Add data tab here!
           #
           # )
    ),
    box(title = "Variables",
        # status = "warning", # sets color
        selectizeInput(inputId = ns("EorX"),
                       label = "Energy Quantification:",
                       choices = c(Energy = "E", Exergy = "X")
        ),
        selectizeInput(inputId = ns("stage"),
                       label = "ECC Stage:",
                       choices = c(Primary = "Primary", Final = "Final", Useful = "Useful")
        ),
        selectizeInput(inputId = ns("stages"),
                       label = "Efficiency Stages:",
                       choices = c(`Primary-Final` = "Primary-Final",
                                   `Primary-Useful` = "Primary-Useful",
                                   `Final-Useful` = "Final-Useful")
        ),
        selectizeInput(inputId = ns("gross_net"),
                       label = "Gross or Net:",
                       choices = c(Gross = "Gross",
                                   Net = "Net")
                       %>% sort(),
                       width = "150px",
                       options = list(dropdownParent = 'body')),
        selectizeInput(inputId = ns("country"),
                       label = "Country:",
                       choices = countries,
                       multiple = TRUE
                       %>% sort()
        ),
        selectizeInput(inputId = ns("rollavg"),
                    label = "Rolling Average Period:",
                    choices = c("1" = 1, "3" = 3, "5" = 5, "7" = 7)
        ),
        solidHeader = TRUE,
        width = 3
    ),
  )
}

# Establishes the server module function
rebound_space <- function(input, output, session,
                          EorX,
                          stage,
                          stages,
                          gross_net,
                          country,
                          rollavg,
                          rs_plot) {

  # Creates a dataframe with the selected country, efproduct, and destination
  selected_data <- reactive({
    validate(
      need(input$EorX != "", "Please select one Energy quantification"),
      need(input$stage != "", "Please select one Energy Conversion Chain (ECC) stage"),
      need(input$stages != "", "Please select one efficiency stage"),
      need(input$gross_net != "", "Please select one Final demand quantification"),
      need(input$country != "", "Please select atleast one Country"),
      need(input$rollavg != "", "Please select the number of years for average")
    )



    rebound_space_data_selected <- rebound_space_data_prepped %>%
      dplyr::filter(Energy.type == input$EorX,
                    Gross.Net == input$gross_net,
                    Stage == input$stage,
                    Stages ==input$stages,
                    Country %in% input$country) %>%
      dplyr::group_by(Country, Method, Energy.type, Gross.Net, Stage) %>%
      dplyr::mutate(E.dot_CAAGR = calc_roll_caagr(metric = E.dot, period = as.numeric(input$rollavg), direction = "Center") * 100) %>%
      dplyr::mutate(Eta_CAAGR = calc_roll_caagr(metric = Eta, period = as.numeric(input$rollavg), direction = "Center") * 100) %>%
      dplyr::ungroup()

    return(rebound_space_data_selected)

  })



  output$rs_plot <- renderPlotly({

    p <- ggplot2::ggplot(data = selected_data(), mapping = ggplot2::aes(x = Eta_CAAGR, y = E.dot_CAAGR)) +

      ggplot2::geom_point(mapping = ggplot2::aes(colour = Year)) + #, frame = Year

      ggplot2::geom_path(size = 0.25,
                         colour = "grey") +

      ggplot2::scale_y_continuous(limits = c(-6, 12), breaks = seq(-6, 12, by = 1)) +
      ggplot2::scale_x_continuous(limits = c(-6, 12), breaks = seq(-6, 12, by = 1)) +

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

    ggplot2::xlab("Efficiency CAAGR [%]") + # [1/year]
    ggplot2::ylab("Energy Consumption CAAGR [%]") + # [1/year]

    ggplot2::facet_wrap(facets = "Country", ncol = 2) +

    MKHthemes::xy_theme()


    p_plotly <- plotly::ggplotly(p, height = 850, tooltip = c("Year", "Eta_CAAGR", "E.dot_CAAGR")) %>%

      # move_annotations_ds(x_y = 0.05,
      #                     y_x = 0.05,
      #                     mar = 80) %>%

      add_annotations(
        x = 3,
        y = -6,
        text = "No Rebound",
        font = list(size = 12),
        size = 0.2,
        showarrow = F) %>%

      add_annotations(
        x = 8,
        y = -1,
        text = "Partial Rebound",
        font = list(size = 12),
        size = 0.2,
        showarrow = F) %>%

      add_annotations(
        x = 4,
        y = 12,
        text = "Backfire",
        font = list(size = 12),
        size = 0.2,
        showarrow = F)


  })


}
