relresourcesUI <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    box(
      width = 12,
      tags$h1("Related Resources"),
      tags$hr(),
      
      tags$h3("End-Use Accounting"),
      
      tags$h4(a("IEA Energy Efficiency Indicators database ", 
                href = "https://www.iea.org/reports/energy-efficiency-indicators")),
      tags$p("The International Energy Agency (IEA) Energy Efficiency Indicators database
             contains information on end-use energy consumption for the residential 
             and transport sectors for a range of countries from 2000-2018."),
      
      tags$h4(a("IIASA Primary, Final and Useful Energy Database", 
                href = "https://iiasa.ac.at/web/home/research/researchPrograms/TransitionstoNewTechnologies/PFUDB.en.html")),
      tags$p("The International Institute for Applied Systems Analysis (IIASA) 
             host a historical Primary, Final and Useful Energy Database (PFUDB) 
             constructed by Dr Simon De Stercke"),
      
      tags$h4(a("Odyssee Database", 
                href = "https://www.indicators.odyssee-mure.eu/energy-efficiency-database.html")),
      tags$p("The Odyssee database contains a collection of end-use energy consumption 
             and intensity data for the Residential, Commercial and public services 
             and transport sectors. It is an EU-funded project and is managed by Enerdata."),
      
      tags$h4(a("JRC Integrated Database of the European Energy System", 
                href = "https://data.jrc.ec.europa.eu/dataset/jrc-10110-10001")),
      tags$p("The European Unions Joint Research Centre (JRC) Integrated Database 
             of the European Energy System (JRC-IDEES) project contains comprehensive
             information on economy-wide end-use energy consumption and efficiency
             across the EU-28 countries from 2000-2015"),
      
      tags$h4(a("LLNL Energy Flow Charts", 
                href = "https://flowcharts.llnl.gov/commodities/energy")),
      tags$p("The Lawerence Livermore National Laboratory (LLNL) Energy Flow Charts
             visualise the flow of energy between the primary, final, and useful
             stages. Charts have been produced yearly since 1978, with post-2010 
             charts available", 
             tags$a(href="https://flowcharts.llnl.gov/commodities/energy", "here,"), 
             "and pre-2010 charts are available", 
             tags$a(href="https://flowcharts.llnl.gov/archive.html", "here.")),
      
      tags$h3("Energy Conversion Chain"),
      
      tags$h4(a("IEA Sankey", 
                href = "https://www.iea.org/sankey/")),
      tags$p("IEA Sankey"),
      
      tags$h3("Exergy"),
      
      tags$h4(a("The Chemical Exergy Calculator", 
                href = "https://exergy-calculator.herokuapp.com/")),
      tags$p("The Chemical Exergy Calculator is a comprehensive database containing 
             chemcial exergy values of various substances and materials. The database was 
             constructed by Charalampos Michalakakis, Jeremy Fouillou, Dr Rick Lupton, 
             Dr Ana Gonzalez Hernandez, and Dr Jonathan M. Cullen")
    ))
}