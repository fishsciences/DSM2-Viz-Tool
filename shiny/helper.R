# package dependencies not listed here

h5_read_attr <- function(data){
  # wrapper around rhdf5::h5readAttributes
  hydro_attr <- lapply(data[["datapath"]], h5readAttributes, "/hydro")
  time_attr <- lapply(data[["datapath"]], h5readAttributes, "/hydro/data/channel flow")
  h5closeAll()
  data.frame(scenario = data[["scenario"]],
             start_date = sapply(time_attr, "[[", "start_time"),
             interval = sapply(time_attr, "[[", "interval"),
             num_channels = sapply(hydro_attr, "[[", "Number of channels"),
             num_intervals = sapply(hydro_attr, "[[", "Number of intervals"))
}

calc_end_date <- function(start_date, num_intervals, interval_vals, interval_units){
  # only accepts min, hour, and day as by argument
  # DSM2 output files don't include end date in metadata (as far as I know)
  total_time <- (num_intervals - 1) * interval_vals
  # translate units to a lubridate function
  funcs <- list("min" = minutes, "hour" = hours, "day" = days)
  # using do.call to unlist without losing date formatting
  end_date <- do.call("c", mapply(function(f, start, tt) start + f(tt), 
                                  funcs[interval_units], start_date, total_time, 
                                  USE.NAMES = FALSE, SIMPLIFY = FALSE))
}

water_year <- function(x){
  # takes a date object and returns the water year (Oct 1st to Sept 30th)
  # https://en.wikipedia.org/wiki/Water_year
  x_lt <- as.POSIXlt(x)
  x_lt$year + 1900L + ifelse(x_lt$mon + 1L >= 10L, 1L, 0L)
}

get_interval_number <- function(start, end, interval_vals, interval_units){
  # find the number of intervals in a date range
  # need plural interval_units for difftime
  interval_units <- paste0(interval_units, "s")
  1L + as.integer(difftime(end, start, units = interval_units))/interval_vals
}

get_date_indexes <- function(start_date, date_range_start, date_range_end, 
                             interval_vals, interval_units) {
  # get index of dates relative to start_date time interval
  # used to index into full HDF5 files when only using a subset of dates
  # need plural interval_units for difftime
  interval_units <- paste0(interval_units, "s")
  diff_drs <- as.integer(difftime(date_range_start, start_date, units = interval_units))
  diff_dre <- as.integer(difftime(date_range_end, start_date, units = interval_units))
  index_start = 1L + diff_drs/interval_vals
  index_end = 1L + diff_dre/interval_vals
  return(seq(index_start, index_end, 1))
}

process_nodes <- function(x){
  x = gsub(pattern = " ", replacement = "", x) # node locations (upstream and downstream) included whitespace 
  x = sapply(x, simple_cap, USE.NAMES = FALSE) # first letter capitalized to match input in ui.R
  return(x)
}

simple_cap <- function(x) {
  # used in process_nodes()
  # ?toupper
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}


h5_read <- function(data, node, channels, dates, name){
  # wrapper around rhdf5::h5read
  # loop through selected files to read selected data in load data step
  # loads only one of two nodes (upstream or downstream); all common channels; and selected date range (input$date_range_load)
  out = list()
  sc = names(dates) # scenarios are stored in list names; dates reflects only scenarios with valid value
  for (i in sc){
    dp = data[["datapath"]][data[["scenario"]] == i]
    out[[i]] = h5read(dp, paste0("/hydro", name),
                      index = list(node[[i]][["Index"]], channels[[i]][["Index"]], dates[[i]][["Index"]]))  # nodes, channels, dates for order of index; NULL indicates load all
  }
  return(out)
}

response_list <- function(response, dates){
  # additional date filtering after loading data (input$date_range_viz)
  out = list() 
  for (i in names(response)){
    out[[i]] = response[[i]][,,dates[[i]][["SubIndex"]], drop = FALSE] # only one dimension for node at this point in the process
  }
  return(out)
}

response_tibble <- function(response, dates, channels, focal_channel){
  # convert response list to tibble for selected focal channel
  # used in plotting time series
  # response, dates, and channels, are all lists with one tibble per scenario (i.e., user uploaded file)
  out = list() 
  for (i in names(response)){
    ci = filter(channels[[i]], Channel == focal_channel)[["Index"]]
    out[[i]] = tibble(Date = dates[[i]][["Date"]],
                      Value = response[[i]][,ci,])
  }
  bind_rows(out, .id = "Scenario")
}

ts_plot <- function(data, y.lab, title, obs.check){
  # plot time series
  if (obs.check == FALSE) return(NULL)
  p <- ggplot(data, aes(x = Date, y = Value, col = Scenario)) +
    geom_line(size = 1, alpha = 0.6) +
    labs(y = y.lab, title = title) +
    scale_colour_brewer(type = "qual", palette = "Set1") +
    theme_minimal() +
    theme.mod
  return(p)
}

calc_summary_stats <- function(array, channel.dim, channels){
  # calculate summary stats for use in comparative analysis
  tibble(channel = channels,
         min = apply(array, channel.dim, min, na.rm = TRUE),
         first.quart = apply(array, channel.dim, quantile, probs = 0.25, na.rm = TRUE),
         median = apply(array, channel.dim, median, na.rm = TRUE),
         mean = apply(array, channel.dim, mean, na.rm = TRUE),
         third.quart = apply(array, channel.dim, quantile, probs = 0.75, na.rm = TRUE),
         max = apply(array, channel.dim, max, na.rm = TRUE),
         prop.neg = apply(array, channel.dim, function(x) sum(x < 0, na.rm = TRUE)/length(x)))
}

proportion_overlap <- function(base, comp, mn, mx){
  # calculate proportion overlap for use in comparative analysis
  # base and comp are equal length vectors
  # mn and mx are min and max values for that channel across all comparisons
  if (length(base) != length(comp)) stop("base and comp are not same length")
  bd = density(base, from = mn, to = mx) # bd = baseline density
  cd = density(comp, from = mn, to = mx) # cd = comparison density
  dis = MESS::auc(bd[["x"]], abs(cd[["y"]] - bd[["y"]]))/(MESS::auc(bd[["x"]], bd[["y"]]) + MESS::auc(cd[["x"]], cd[["y"]])) # dis = 0 is completely overlapping; dis = 1 is no overlap
  return(list(po = 1 - dis, 
              x = bd[["x"]], # base and comp have same x
              y.base = bd[["y"]],
              y.comp = cd[["y"]]))
}

