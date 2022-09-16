################################################################################
# Server
################################################################################

server <- function(input, output, session) {

  ################################################################################
  # Authenticate user
  ################################################################################

  # call the server part
  # check_credentials returns a function to authenticate users
  res_auth <- shinymanager::secure_server(
    # check_credentials = shinymanager::check_credentials(credentials)
    check_credentials = shinymanager::check_credentials(credentials_path,
                                                        passphrase = "passphrase_wihtout_keyring")
  )

  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })


  ################################################################################
  # Call guides
  ################################################################################

  # Triggers PFU Database guide when childless menuItem is opened
  observeEvent(input$sidebarItemExpanded, {
    req(input$sidebarItemExpanded)
    if (input$sidebarItemExpanded == "pfu_menu") {
      guide_pfu$start()
    }
  })

  # Initialise guide_pfu_dash, but do not start automatically
  guide_pfu_dash$
    init()


  ################################################################################
  # Call server-side module components
  ################################################################################

  ## PFU-Database modules

  # Calls sum_dash_pfu.R module
  callModule(module = sumdashplots,
             id = "dashpfu_id_1")

  # Calls the pfuex_cons.R module
  callModule(module = pfuex_cons,
             id = "pfuex_cons_id_1")

  # Calls the pfuex_eta.R module
  callModule(module = pfuex_eta,
             id = "pfuex_eta_id_1")

  callModule(module = allocplots,
             id = "alloc_id_1",
             comp_alloc_tables_prepped)

  callModule(module = etaplots,
             id = "fu_id_1",
             comp_effic_tables_prepped)

  ## PFU Analysis modules
  callModule(module = pfuanalysis_dash,
             id = "dashpfuanalysis_id_1",
             pfuex_econ,
             rebound_space_data_prepped)

  callModule(module = rebound_space,
             id = "rebound_id_1",
             rebound_space_data_prepped)

  callModule(module = decoupling_space,
             id = "decsp_id_1",
             pfuex_econ)

  ## Rebound modules

  # Calls rebound introduction module
  callModule(module = intro_rebound,
             id = "introreb_id_1")

  # Calls rebound dashboard module
  callModule(module = rebound_dash,
             id = "dashreb_id_1")

  # Calls rebound citation module

}
