

# VAR: MONTHLY MEAN VAPOR PRESSURE DEFICIT 
# BASED ON: ERA5
# USING: AVERAGE TEMPERATURE AND DEWPOINT TEMPERATURE


library(tidyverse)
library(stars)
library(furrr)

options(future.fork.enable = T)
plan(multicore)

# import functions
source("https://raw.github.com/carlosdobler/spatial-routines/master/general_tools.R")


# input and output data dir
dir_data <- 
  "gs://clim_data_reg_useast1/era5/monthly_means"

# temporary dir
dir_temp <- 
  "/mnt/pers_disk/tmp"

fs::dir_create(dir_temp)


# dates to process
time_vector <- 
  seq(as_date("1995-01-01"), as_date("2020-12-01"), by = "1 month")

# all input files
ff_tas <- 
  str_glue("gsutil ls {dir_data}/2m_temperature") %>% 
  system(intern = T) %>% 
  str_subset(time_vector %>% 
               year() %>% 
               unique() %>% 
               str_flatten("|")) %>% 
  str_subset(".nc")

ff_dewp <- 
  str_glue("gsutil ls {dir_data}/2m_dewpoint_temperature") %>% 
  system(intern = T) %>% 
  str_subset(time_vector %>% 
               year() %>% 
               unique() %>% 
               str_flatten("|")) %>% 
  str_subset(".nc")


# ARE BOTH LIST OF FILES THE SAME LENGTH?
length(ff_tas) == length(ff_dewp)



for (d in time_vector) {
  
  f <- 
    c(ff_tas %>% 
        str_subset(as.character(d)),
      
      ff_dewp %>% 
        str_subset(as.character(d)))
  
  f %>% 
    walk(~str_glue("gsutil cp {.x} {dir_temp}") %>% 
           system(ignore.stdout = T, ignore.stderr = T))
  
  s <- 
    dir_temp %>% 
    fs::dir_ls() %>% 
    map(read_ncdf, proxy = F) %>% 
    suppressMessages() %>%
    unname() %>% 
    do.call(c, .) %>% 
    adrop()
  
  s_vpd <- 
    s %>% 
    mutate(d2m = d2m %>% units::set_units(degC),
           t2m = t2m %>% units::set_units(degC)) %>% 
    
    units::drop_units() %>% 
    
    mutate(rh = 100 * exp((17.62 * d2m) / (243.12 + d2m)) / exp(17.62 * t2m / (243.12 + t2m))) %>% 
    
    mutate(vpd = 0.6112 * exp((17.62 * t2m) / (243.12 + t2m)) * (1 - rh/100),
           vpd = units::set_units(vpd, kPa)) %>% 
    # based on: https://essd.copernicus.org/articles/14/5573/2022/
    
    select(vpd)
  
  rt_write_nc(s_vpd, 
              str_glue("{dir_temp}/era5_vapor-pressure-deficit_mon_{d}.nc"),
              gatt_name = "source code",
              gatt_val = "lkhfgjhgslkd")
  
  
  
  # add comment to file?
  
  # transfer to bucket
  str_glue("gsutil mv {dir_temp}/era5_vapor-pressure-deficit_mon_{d}.nc {dir_data}/vapor_pressure_deficit")
  
  # delete temp files
  
}



fs::dir_delete(dir_temp)










