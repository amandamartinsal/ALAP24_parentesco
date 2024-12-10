# Function to load and filter UN World Population Prospects data for mortality (px,qx,mx) and fertility (fx)

library(readr)
library(dplyr)
library(tidyr)


UNWPP_data <- function(country, start_year, end_year, sex = c("Female", "Male"), 
                       indicators = c("px", "qx", "mx", "fx"), output_format = c("csv", "RData"), 
                       output_file = "UNWPP_output") {
  # Define file paths
  fertility_file <- "data/WPP2024_Fertility_by_Age1.csv"
  male_lifetable_file <- "data/WPP2024_Life_Table_Complete_Medium_Male_1950-2023.csv"
  female_lifetable_file <- "data/WPP2024_Life_Table_Complete_Medium_Female_1950-2023.csv"
  
  # Check if files exist
  if (!file.exists(fertility_file)) stop("Fertility data file not found: ", fertility_file)
  if (!file.exists(male_lifetable_file)) stop("Male life table data file not found: ", male_lifetable_file)
  if (!file.exists(female_lifetable_file)) stop("Female life table data file not found: ", female_lifetable_file)
  
  # Read the life table data
  WPP2024_male_lifetable <- read_csv(male_lifetable_file, show_col_types = FALSE)
  WPP2024_female_lifetable <- read_csv(female_lifetable_file, show_col_types = FALSE)
  
  # Combine male and female life table data
  WPP2024_lifetable <- bind_rows(WPP2024_female_lifetable, WPP2024_male_lifetable)
  
  # Initialize an empty list to store data frames for each sex
  combined_sex_data <- list()
  
  # Loop over each specified sex
  for (current_sex in sex) {
    # Initialize a list to store selected data for current sex
    selected_data <- list()
    
    # Filter life table data based on specified indicators
    if ("px" %in% indicators) {
      px <- WPP2024_lifetable %>%
        select(Location, Sex, Time, AgeGrpStart, px) %>%
        filter(Location == country, Time >= start_year, Time <= end_year, Sex == current_sex) %>%
        rename(year = Time, age = AgeGrpStart)
      selected_data$px <- px
    }
    
    if ("qx" %in% indicators) {
      qx <- WPP2024_lifetable %>%
        select(Location, Sex, Time, AgeGrpStart, qx) %>%
        filter(Location == country, Time >= start_year, Time <= end_year, Sex == current_sex) %>%
        rename(year = Time, age = AgeGrpStart)
      selected_data$qx <- qx
    }
    
    if ("mx" %in% indicators) {
      mx <- WPP2024_lifetable %>%
        select(Location, Sex, Time, AgeGrpStart, mx) %>%
        filter(Location == country, Time >= start_year, Time <= end_year, Sex == current_sex) %>%
        rename(year = Time, age = AgeGrpStart)
      selected_data$mx <- mx
    }
    
    # Process fertility data if "fx" indicator and sex is Female
    if ("fx" %in% indicators && current_sex == "Female") {
      fertility_data <- read_csv(fertility_file, show_col_types = FALSE)
      asfr <- fertility_data %>%
        select(Location, Time, AgeGrpStart, ASFR) %>%
        mutate(ASFR = ASFR / 1000, Sex = "Female") %>%  # Add Sex column explicitly
        filter(Location == country, Time >= start_year, Time <= end_year) %>%
        rename(year = Time, age = AgeGrpStart, fx = ASFR)
      
      # Join fertility data with other indicators and fill missing values in `fx` with 0
      if ("px" %in% names(selected_data)) {
        asfr <- left_join(selected_data$px, asfr, by = c("Location", "year", "age", "Sex")) %>%
          mutate(fx = replace_na(fx, 0))
      } else {
        asfr <- asfr %>%
          mutate(fx = replace_na(fx, 0))
      }
      
      selected_data$fx <- asfr
    } else if ("fx" %in% indicators && current_sex == "Male") {
      warning("Fertility data (fx) is only available for females. Ignoring fx for males.")
    }
    
    # Combine all selected data for the current sex
    if (length(selected_data) > 0) {
      combined_data_sex <- Reduce(function(x, y) {
        full_join(x, y, by = c("Location", "year", "age", "Sex")) %>%
          mutate(across(ends_with(".x"), ~ coalesce(.x, get(sub(".x$", ".y", cur_column()))))) %>%
          select(-ends_with(".y"))
      }, selected_data)
      
      combined_sex_data[[current_sex]] <- combined_data_sex
    }
  }
  
  # Combine data for all specified sexes into one data frame
  combined_data <- bind_rows(combined_sex_data)
  
  # Save output if specified
  if (output_format == "csv") {
    write.csv(combined_data, file = paste0(output_file, ".csv"), row.names = FALSE)
    message("Data saved as CSV: ", paste0(output_file, ".csv"))
  } else if (output_format == "RData") {
    save(combined_data, file = paste0(output_file, ".RData"))
    message("Data saved as RData: ", paste0(output_file, ".RData"))
  } else {
    stop("Invalid output format. Please specify either 'csv' or 'RData'.")
  }
  
  return(combined_data)
}


# Function to load and filter UN World Population Prospects data for population (N)

UNWPP_pop <-
  function(country_name, start_year, end_year, sex) {
    
    # Read in population data
    WPP2024_pop <- read_csv("data/WPP2024_Population1JanuaryBySingleAgeSex_Medium_1950-2023.csv", show_col_types = FALSE)
    
    # Select the relevant population column based on sex
    if (sex == "Female") {
      wpp <- WPP2024_pop %>% 
        select(age = AgeGrpStart, country = Location, year = Time, pop = PopFemale)
    } else if (sex == "Male") {
      wpp <- WPP2024_pop %>% 
        select(age = AgeGrpStart, country = Location, year = Time, pop = PopMale)
    } else {
      stop("Invalid sex. Please specify 'Male' or 'Female'.") 
    }
    
    # Filter by country and reshape data
    wpp <- wpp %>% 
      filter(country == country_name, year >= start_year, year <= end_year) %>%
      pivot_wider(names_from = year, values_from = pop) %>%
      select(-age, -country) %>%
      as.matrix()
    
    # Add row names for age groups
    row.names(wpp) <- 0:(nrow(wpp) - 1)
    
    return(wpp)
  }
