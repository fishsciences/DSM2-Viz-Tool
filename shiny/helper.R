# package dependencies not listed here

h5_read_attr <- function(data, name, attr, col.name){
  # wrapper around rhdf5::h5readAttributes
  # loop through selected files to extract metadata
  out = c()  # not pre-allocating because not sure about type of attribute (but probably should be)
  for (i in 1:nrow(data)){ # h5readAttributes is not vectorized
    out = c(out, h5readAttributes(data[["datapath"]][i], paste0("/hydro", name))[[attr]])
  }
  out.df = select(data, scenario)
  out.df[[col.name]] = out
  return(out.df)
}

calc_end_date <- function(start_date, num_intervals, by){
  # fragile, inelegant function
  # only accepts min, hour, and day as by argument
  # DSM2 output files don't include end date in metadata (as far as I know)
  start_date = ymd_hms(start_date)
  out = vector(mode = "character", length = length(by))
  for (i in 1:length(by)){
    by_vec = strsplit(by[i], " ", fixed = TRUE)[[1L]]
    total_time = (num_intervals[i] - 1) * as.numeric(by_vec[1])
    if (by_vec[2] == "min")  end_date = start_date[i] + minutes(total_time)
    if (by_vec[2] == "hour") end_date = start_date[i] + hours(total_time)
    if (by_vec[2] == "day")  end_date = start_date[i] + days(total_time)
    end_date = as.character(end_date)
    out[i] = ifelse(nchar(end_date) > 10, end_date, paste(end_date, "00:00:00")) # time of 00:00:00 gets dropped; adding it for consistency
  }
  return(out)
}

water_year <- function(x){
  # takes a date object and returns the water year (Oct 1st to Sept 30th)
  # https://en.wikipedia.org/wiki/Water_year
  as.integer(format(x, "%Y")) + ifelse(as.integer(format(x, "%m")) >= 10L, 1L, 0L)
}

get_interval_number <- function(start, end, by){
  # find the number of intervals in a date range
  gd = get_difftime(start, end, by)
  return(1L + floor(gd[[3]]/gd[[1]]))
}

get_date_indexes <- function(start_date, date_range_start, date_range_end, by) {
  # get index of dates relative to start_date time interval (specified with 'by')
  # used to index into full HDF5 files when only using a subset of dates
  gd_start = get_difftime(start_date, date_range_start, by)
  gd_end = get_difftime(start_date, date_range_end, by)
  index_start = 1L + gd_start[[3]]/gd_start[[1]]
  index_end = 1L + gd_end[[3]]/gd_end[[1]]
  return(seq(index_start, index_end, 1))
}

get_difftime <- function(start, end, by) {
  # get difftime based on interval encoded in HDF5 file (specified with 'by')
  # used in get_interval_number() and get_date_indexes()
  by_vec = strsplit(by, " ", fixed = TRUE)[[1L]]
  by_num = as.integer(by_vec[1])
  by_unit = paste0(by_vec[2], "s")
  difftime_out = as.integer(difftime(end, start, units = by_unit))
  return(list(by_num, by_unit, difftime_out))
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

