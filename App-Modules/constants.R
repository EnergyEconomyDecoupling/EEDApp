################################################################################
# Establish other options
################################################################################

# Establish gdp options
gdp_options <- c(`Real` = "rgdpna",
                 `Expenditure-side` = "rgdpe",
                 `Output-side` = "rgdpo")

# Establish IEA, MW, or Both options
iea.mw_options <- c(IEA = "IEA",
                    MW = "MW",
                    Both = "Both")

# Establish Energy quantification options
ex_options <- c(Energy = "E",
                Exergy = "X")

# Establish ECC stage options
stage_options <- c(Primary = "Primary",
                   Final = "Final",
                   Useful = "Useful")

# Establish efficiency stages options
stages_options <- c(`Primary-Useful` = "Primary-Useful",
                    `Primary-Final` = "Primary-Final",
                    `Final-Useful` = "Final-Useful")

# Establish Gross or Net options
gronet_options <- c(`Incl. EIOU (Gross)` = "Gross",
                    `Excl. EIOU (Net)` = "Net")

# Establish compound annual average growth rate (CAAGR) options
caagr_options <- c("11" = 11,
                   "9" = 9,
                   "7" = 7,
                   "5" = 5,
                   "3" = 3,
                   "1" = 1)

exunit_options <- c(ktoe = "ktoe",
                    EJ = "EJ")

################################################################################
# Establish unit conversion factors
################################################################################

ktoe_to_ej <- 0.00004186799999999929
ej_to_ktoe <- 1/ktoe_to_ej
