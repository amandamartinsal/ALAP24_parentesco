#----------------------------------------------------------------------------------------------------
# Functions to Write SOCSIM fertility and mortality input rate files using WPP2024 data
#----------------------------------------------------------------------------------------------------
# Create a sub-folder called "rates" to save the rate files if it does not exist.
ifelse(!dir.exists("rates"), dir.create("rates"), FALSE)
# If the sub-folder name changes, 
# it must be also changed in the functions when opening the output file connection

# Prevent scientific notation
options(scipen=999999)

#----------------------------------------------------------------------------------------------------
#### Write SOCSIM input fertility rates from WPP data (filtered for country and relevant variables) ----

## Need to add start_year, end_year, sex

write_socsim_fertility_rates_WPP <- function(country = country) {

  load(paste0("data/","fertility_",country,".RData"))
  data <- get(paste0("fertility_",country))
  
  # Wrangle data and compute monthly fertility rates
  ASFR <- 
    data %>% 
    select(year, age, fx) %>%
    mutate(Age_up = age + 1, # SOCSIM uses the upper age bound
           Month = 0, 
           fx_mo = fx/12) %>% 
    select(-fx)
    
  # Add rows with rates = 0 for ages 0-15 and 50-101
  ASFR <- 
    ASFR %>% 
    group_by(year) %>% 
    group_split() %>% 
    map_df(~ add_row(.x,
                     year = unique(.x$year), 
                     age = 0, Age_up = 15,  Month = 0, fx_mo = 0.0, 
                     .before = 1)) %>% 
    group_by(year) %>% 
    group_split() %>% 
    map_df(~ add_row(.x, 
                     year = unique(.x$year), 
                     age = 50, Age_up = 101, Month = 0, fx_mo = 0.0, 
                     .after = 36)) %>% 
    ungroup() %>% 
    select(-age)
  
  # Extract the years in the dataset
  years <- ASFR %>% pull(year) %>% unique()
  
  # Row numbers corresponding to sequence of years of age in ASFR
  rows_ageF <- ASFR %>% pull(Age_up) %>% unique() %>% seq_along()

  ## Write the fertility rate files for each year
  
  for(year in years) {
    
    # Find the index of each year of the iteration
    n <- which(year == years)
    n_row <- (n-1)*37 + rows_ageF
    
    # Open an output file connection
    outfilename <- file(paste0("rates/",country,"fert",year), "w") 
    # without ".txt" specification as the original files had no format. 
    
    # Include country and year of the corresponding rates
    cat(c("** Period (Monthly) Age-Specific Fertility Rates for", country, "in", year, "\n"), 
        file = outfilename)
    cat(c("* Retrieved from the World Population Prospects 2024", "\n"), 
        file = outfilename)
    cat(c("* https://population.un.org/wpp/Download/Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Fertility_by_Age1.csv.gz", "\n"), 
        file = outfilename)
    cat(c("* United Nations. Population Division. Department of Economic and Social Affairs", "\n"), 
        file = outfilename)
    cat(c("* Data downloaded on ", format(Sys.time(), format= "%d %b %Y %X %Z"), "\n"), 
        file = outfilename)
    cat(c("** NB: The original WPP annual rates have been converted into monthly rates", "\n"), 
        file = outfilename)
    cat("\n", file = outfilename)
    
    # Print birth rates (single females)
    cat("birth", "1", "F", "single", "0", "\n", file = outfilename)
    for(i in n_row) {
      cat(c(as.matrix(ASFR)[i,-1], "\n"), file = outfilename) }
    cat("\n", file = outfilename)
    
    # Print birth rates (married females)
    cat("birth", "1", "F", "married", "0", "\n", file = outfilename)
    for(i in n_row) {
      cat(c(as.matrix(ASFR)[i,-1],"\n"), file = outfilename) }
    
    close(outfilename)
    
  }
  
}


#----------------------------------------------------------------------------------------------------
#### Write SOCSIM input mortality rates from WPP (filtered for country and relevant variables) ----

write_socsim_mortality_rates_WPP <- function(country = country) {

  load(paste0("data/","mortality_",country,".RData"))
  data <- get(paste0("mortality_",country))
  
  # Wrangle data and compute monthly mortality probabilities
  ASMP <- 
    data %>% 
    select(year, Sex, age, qx) %>%
    mutate(qx_mo = ifelse(age == 100, qx/12, 1-(1-qx)^(1/12)),
           Age_up = age + 1, # SOCSIM uses the upper age bound
           Month = 0) %>% 
    select(c(year, Age_up, Month, Sex, qx_mo)) %>% 
    pivot_wider(names_from = Sex, values_from = qx_mo)
    
  # Extract the years available in WPP
  years <- ASMP %>% pull(year) %>% unique()
  
  # Row numbers corresponding to sequence of years of age in ASMP
  rows_ageM <- ASMP %>% pull(Age_up) %>% unique() %>% seq_along()
  
  ## Write the mortality rate files for each year
  
  for(year in years) {
    
    # Find the index of each year of the iteration
    n <- which(year == years)
    n_row <- (n-1)*101 + rows_ageM
    
    # Open an output file connection
    outfilename <- file(paste0("rates/",country,"mort",year), "w") 
    # without ".txt" specification as the original files had no format. 
    
    # Include country and year of the corresponding probabilities
    cat(c("** Period (Monthly) Age-Specific Probabilities of Death for", country, "in", year, "\n"), 
        file = outfilename)
    cat(c("* Retrieved from the World Population Prospects 2024. Single age life tables up to age 100 Medium variant", "\n"), 
        file = outfilename)
    cat(c("* https://population.un.org/wpp/Download/Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Life_Table_Complete_Medium_Female_1950-2023.csv.gz", "and", "\n"), 
        file = outfilename)
    cat(c("* https://population.un.org/wpp/Download/Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Life_Table_Complete_Medium_Male_1950-2023.csv.gz", "and", "\n"), 
        file = outfilename)
    cat(c("* United Nations. Population Division. Department of Economic and Social Affairs", "\n"), 
        file = outfilename)
    cat(c("* Data downloaded on ", format(Sys.time(), format= "%d %b %Y %X %Z"), "\n"), 
        file = outfilename)
    cat(c("** NB: The original WPP annual probabilities have been converted into monthly probabilities", "\n"), 
        file = outfilename)
    cat(c("** The final age interval is limited to one year [100-101)", "\n"), 
        file = outfilename)
    cat("\n", file = outfilename)
    
    # Print mortality probabilities (single females)
    cat("death", "1", "F", "single", "\n", file = outfilename)
    for(i in n_row) {
      cat(c(as.matrix(ASMP)[i,-c(1,5)], "\n"), file = outfilename) }
    cat("\n", file = outfilename)
    
    # Print mortality probabilities (single males)
    cat("death", "1", "M", "single", "\n", file = outfilename)
    for(i in n_row) {
      cat(c(as.matrix(ASMP)[i,-c(1,4)], "\n"), file = outfilename) } 
    
    close(outfilename)
    
  }
}