library(tidyverse)
library(lubridate)

start_date <- function(dob) {
  year <- as.numeric(format(dob, format="%Y"))
  school_start_year <- year + 5
  age5 <- paste0(format(dob, format="%d/%m/"), school_start_year) %>% as.Date(format = "%d/%m/%Y")
  if (year >= 1945 & year <= 1951) {
    ssd <- paste0("31/12/", school_start_year) %>% as.Date(format = "%d/%m/%Y")
  } else if (year >= 1952 & year <= 1981) {
    ssd <- paste0("28/02/", school_start_year) %>% as.Date(format = "%d/%m/%Y")
  } else if (year >= 1982 & year <= 2002) {
    ssd <- paste0("31/12/", school_start_year) %>% as.Date(format = "%d/%m/%Y")
  } else if (year >= 2003 & year <= 2021) {
    ssd <- paste0("30/06/", school_start_year) %>% as.Date(format = "%d/%m/%Y")
  }
  
  if (abs(age5 - ssd) < abs((age5  %m+% years(1)) - ssd)) {
    (age5 - ssd)
  } else {
    (age5  %m+% years(1)) - ssd
  }
}

start_date(as.Date("20/05/1994", format = "%d/%m/%Y"))