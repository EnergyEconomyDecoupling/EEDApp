intro_reboundUI <- function(id) {
  ns <- NS(id)
        fluidRow(
          box(title = "Introduction",
              width = 12,
              tags$h3("What is energy rebound?"),
              tags$p("'Energy rebound is a phenomenon whereby some expected energy
                     savings fail to materialise after an energy efficiency upgrade
                     (EEU), due to behaviour changes and economic effects'."),
              tags$h3("Description"),
              tags$p("The rebound section of the Energy-Economy Decoupling
                     Organisation app acts as an accompaniment to the",
                     em("Energy Economics"), "paper 'Towards a comprehensive,
                     consumer-sided energy rebound analysis framework'."),
              tags$p("Specifically users may utlise the mathematical frameowrk
                     described in the paper and functions provided in the
                     R package 'ReboundTools' interactively, allowing analysis
                     of energy rebound by entering user-defined device, economic,
                     and elasticity parameters. Alternatively the user can select
                     the two examples used in the paper (a Lamp and Car)."),
              tags$p("Github --> Docker Hub --> Azure CD now working!"),
              tags$p("Adding paragraph to trigger new build of app with updated EEDAppBaseImage")
              ),

          box(
            width = 12,
            height = 500,
            title = "Testing mounted storage",
            id = "mount_storage_test_box",
            closable = FALSE,
            # status = "warning",
            solidHeader = FALSE,
            collapsible = FALSE,
            enable_sidebar = FALSE,
            tags$p("Successfully updated app #1"),
            tags$p("MOUNT PATH:/mnt/.drake"),
            tags$p("Table"),
            tableOutput(outputId = ns("mount_storage_test_table")),
            tags$p("Head Text"),
            verbatimTextOutput(outputId = ns("mount_storage_test_head"))
          )
              )
}

intro_rebound <- function(input, output, session){


  # mount_storage_data_cat <- reactive({
  #
  #   mount_storage_data_cat <- mount_storage_data_cat
  #
  # })


  output$mount_storage_test_table <- renderTable({

    mount_storage_test <- mount_storage_data_cat

    mount_storage_test


  })

  output$mount_storage_test_head <- renderPrint(

    head(mount_storage_data_cat)

  )

}
