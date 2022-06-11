# Establish guide for main mani-menu
guide_main <- Cicerone$
  new(opacity = 0.5,
      padding = 0,
      allow_close = TRUE,
      overlay_click_next = FALSE,
      done_btn_text = "Done",
      close_btn_text = "Close",
      stage_background = "#ffffff",
      next_btn_text = "Next",
      prev_btn_text = "Previous",
      show_btns = TRUE,
      keyboard_control = TRUE,
      id = "guide_main",
      mathjax = FALSE)$
  step(
    el = "[data-value='outline']",
    title = "Outline",
    description = "Click on the Outline tab to view a general description of the 
                  app.",
    is_id = FALSE
  )$
  step(
    el = "[data-value='pfu_menu']",
    title = "PFU Database",
    description = "Click on the PFU Database tab to explore the 
                  Primary-Final-Useful database.",
    is_id = FALSE
  )$
  step(
    el = "[data-value='ee_menu']",
    title = "Energy-Economy",
    description = "Click on the Energy-Economy dashboard tab to explore 
                  energy-economy macroeconomic indicators.",
    is_id = FALSE
  )$
  step(
    el = "[data-value='rebound']",
    title = "Rebound",
    description = "Click on the Rebound tab to explore cutting-edge 
                  energy-economy rebound visualisations associated with 
                  the paper.......",
    is_id = FALSE
  )$
  step(
    el = "[data-value='relatedresources']",
    title = "Related resources",
    description = "Click on the rleated resources tab to view resources related 
    to energyg-economy analysis.",
    is_id = FALSE
  )$
  step(
    el = "[data-value='citation']",
    title = "Citation",
    description = "Click on the citation tab to view details of the people behind
                  the energy-economy decoupling app, and citation information.",
    is_id = FALSE
  )
# Establish guide for PFU-Database
guide_pfu <- Cicerone$
  new(opacity = 0.5,
      padding = 0,
      allow_close = TRUE,
      overlay_click_next = FALSE,
      done_btn_text = "Done",
      close_btn_text = "Close",
      stage_background = "#ffffff",
      next_btn_text = "Next",
      prev_btn_text = "Previous",
      show_btns = TRUE,
      keyboard_control = TRUE,
      id = "guide_pfu",
      mathjax = FALSE)$
  step(
    el = "[data-value='intro_pfu']",
    title = "PFU Database",
    description = "Click on the PFU Database tab to explore the 
                  Primary-Final-Useful database.",
    is_id = FALSE
  )$
  step(
    el = "[data-value='dashboard_pfu']",
    title = "PFU Dashboard",
    description = "Click on the PFU Dashboard tab to view 
                  an overview of the information contained in the database",
    is_id = FALSE
  )$
  step(
    el = "[data-value='sankey']",
    title = "Energy Conversion Chain",
    description = "Click on the Energy Conversion Chain tab to view the energy
                  conversion chain of a selected country at the primary, final, 
                  and uniquely to this database, the useful stage",
    is_id = FALSE
  )$
  step(
    el = "[data-value='allocations']",
    title = "Final-to-useful Allocations",
    description = "Click on the Final-to-useful Allocations tab to explore the 
                  machines, devices, and appliances which consume final energy..",
    is_id = FALSE
  )$
  step(
    el = "[data-value='eta_phi']",
    title = "Final-to-useful Efficiencies",
    description = "Click on the Final-to-useful Efficiencies tab to view the 
                  final-to-useful energy efficiencies, and exergy-to-energy ratios
                  of the machines allocated to final energyh consumption.",
    is_id = FALSE
  )$
  step(
    el = "[data-value='geocov']",
    title = "Geographical coverage",
    description = "Click on the Geographical coverage tab to view information on
                  the coverage of the database",
    is_id = FALSE
  )$
  step(
    el = "[data-value='documentation']",
    title = "Database documentation",
    description = "Click on the Database documentation tab to read the documentation
                  associated with the construction of this database.....",
    is_id = FALSE
  )

# Establish guide for PFU summary dashboard
guide_pfu_dash <- Cicerone$
  new(opacity = 0.5,
      padding = 0,
      allow_close = TRUE,
      overlay_click_next = FALSE,
      done_btn_text = "Done",
      close_btn_text = "Close",
      stage_background = "#ffffff",
      next_btn_text = "Next",
      prev_btn_text = "Previous",
      show_btns = TRUE,
      keyboard_control = TRUE,
      id = "guide_pfu_dash",
      mathjax = FALSE)$
  step(
    el = "Variables_pfu",
    title = "Variables",
    description = "Here are a set of variables linked to each of the dashboard plots"
  )$
  # Cannot refer to id's wrapped in ns() in modules i.e. country below
  # step(
  #   el = "country",
  #   title = "Select a country",
  #   description = "..."
  # )$
  step(
    el = "con_by_prod",
    title = "Consumption by Product",
    description = "Displayed here is the consumption of primary, final, useful energy or exergy 
                  for the selected country by energy product"
  )$
  step(
    el = "con_by_flowsec",
    title = "Consumption by Flow or Sector",
    description = "Displayed here is the consumption of primary, final, useful energy or exergy 
                  for the selected country by sector/destination"
  )$
  step(
    el = "index_data",
    title = "Indexed Data",
    description = "Displayed here is the consumption of primary, final, and useful energy, 
                  along with the associated effciencies...."
  )
