library(tidyverse)
library(lubridate)

start_date <- function(dob) {
  if (is.na(dob)) {
    return(NA)
  }
  year <- as.numeric(format(dob, format="%Y"))
  school_start_year <- year + 5
  age5 <- paste0(format(dob, format="%d/%m/"), school_start_year) %>% as.Date(format = "%d/%m/%Y")
  # Account for leap year babies
  if (format(dob, format="%d/%m/") == "29/02/") {
    age5 <- paste0("28/02/", school_start_year) %>% as.Date(format = "%d/%m/%Y")
  }
  if (year >= 1945 & year <= 1951) {
    ssd <- paste0("31/12/", school_start_year) %>% as.Date(format = "%d/%m/%Y")
  } else if (year >= 1952 & year <= 1981) {
    ssd <- paste0("28/02/", school_start_year) %>% as.Date(format = "%d/%m/%Y")
  } else if (year >= 1982 & year <= 2002) {
    ssd <- paste0("31/12/", school_start_year) %>% as.Date(format = "%d/%m/%Y")
  } else if (year >= 2003 & year <= 2021) {
    ssd <- paste0("30/06/", school_start_year) %>% as.Date(format = "%d/%m/%Y")
  } else {
    return(NA)
  }
  
  if (abs(age5 - ssd) < abs((age5  %m+% years(1)) - ssd)) {
    result <- (age5 - ssd)
  } else {
    result <- (age5  %m+% years(1)) - ssd
  }
  
  as.numeric(result, units="days")
}

start_date(as.Date(NA, format = "%Y-%m-%d"))

# dobtest <- c("20/05/1994", "26/11/1995") %>% as.Date(format = "%d/%m/%Y")

# print(dobtest %>% map_int(start_date))
