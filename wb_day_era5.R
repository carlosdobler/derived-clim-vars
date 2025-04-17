

# VAR: DAILY WETBULB TEMPERATURE 
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
  "gs://clim_data_reg_useast1/era5/daily_aggregates"

# temporary dir
dir_temp <- 
  "/mnt/pers_disk_300/tmp2"

fs::dir_create(dir_temp)


# dates to process
time_vector <- 
  seq(as_date("1970-01-01"), as_date("2020-12-01"), by = "1 month")

# fetch all input file names
ff_tas <- 
  str_glue("{dir_data}/2m_temperature") |> 
  rt_gs_list_files() |>  
  str_subset(time_vector |>  
               year() |> 
               unique() |>  
               str_flatten("|")) %>% 
  str_subset(".nc")

ff_dewp <- 
  str_glue("{dir_data}/2m_dewpoint_temperature") |> 
  rt_gs_list_files() |>  
  str_subset(time_vector %>% 
               year() %>% 
               unique() %>% 
               str_flatten("|")) %>% 
  str_subset(".nc")


# ARE BOTH LIST OF FILE NAMES THE SAME LENGTH?
length(ff_tas) == length(ff_dewp)


# loop through all dates
for (d in as.character(time_vector)) {
  
  print(d)
  
  f <- 
    c(ff_tas %>% 
        str_subset(as.character(d)),
      
      ff_dewp %>% 
        str_subset(as.character(d)))
  
  # copy files to disk
  f <- 
    rt_gs_download_files(f, dir_temp)
  
  
  # read files
  s <- 
    f |> 
    map(read_ncdf, proxy = F) %>% 
    suppressMessages() %>%
    unname() %>% 
    do.call(c, .)
  
  # calculate vpd
  s_wb <- 
    s %>% 
    mutate(d2m = d2m %>% units::set_units(degC),
           t2m = t2m %>% units::set_units(degC)) %>% 
    
    units::drop_units() %>% 
    
    mutate(rh = 100 * exp((17.62 * d2m) / (243.12 + d2m)) / exp(17.62 * t2m / (243.12 + t2m))) %>% 
    
    mutate(wb = t2m * atan(0.151977 * sqrt(rh + 8.313659)) + 
             atan(t2m + rh) - 
             atan(rh - 1.676331) + 
             0.00391838 * (rh)^(3/2) * atan(0.023101 * rh) - 
             4.686035,
  
           wb = units::set_units(wb, degC)) %>% 
    # based on: https://journals.ametsoc.org/view/journals/apme/50/11/jamc-d-11-0143.1.xml
    
    select(wb)
  
  # save result
  f_res <- str_glue("{dir_temp}/era5_wetbulb-temperature_day_{d}.nc")
  
  rt_write_nc(s_wb, 
              f_res,
              calendar = "gregorian",
              gatt_name = "source code",
              gatt_val = "https://github.com/carlosdobler/derived-clim-vars/blob/main/wb_day_era5.R")
  
  # transfer to bucket
  str_glue("gcloud storage mv {f_res} {dir_data}/wetbulb_temperature/") %>% 
    system(ignore.stdout = T, ignore.stderr = T)
  
  # delete temp files
  f |> 
    walk(fs::file_delete)
  
}

# delete temporary dir
fs::dir_delete(dir_temp)

