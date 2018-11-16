# package dependencies not listed here

calc_summary_stats <- function(arr.2d, time.dim, channels){
  tibble(channel = channels,
         min = apply(arr.2d, time.dim, min, na.rm = TRUE),
         first.quart = apply(arr.2d, time.dim, quantile, probs = 0.25, na.rm = TRUE),
         median = apply(arr.2d, time.dim, median, na.rm = TRUE),
         mean = apply(arr.2d, time.dim, mean, na.rm = TRUE),
         third.quart = apply(arr.2d, time.dim, quantile, probs = 0.75, na.rm = TRUE),
         max = apply(arr.2d, time.dim, max, na.rm = TRUE),
         prop.neg = apply(arr.2d, time.dim, function(x) sum(x < 0, na.rm = TRUE)/length(x)))
}


proportion_overlap <- function(base, comp, mn, mx){
  # base and comp are equal length vectors
  # mn and mx are min and max values for that channel across all comparisons
  if (length(base) != length(comp)) stop("base and comp are not same length")
  # bd = density(base, from = mn, to = mx, bw = "sj") # bd = baseline density
  # cd = density(comp, from = mn, to = mx, bw = "sj") # cd = comparison density
  bd = density(base, from = mn, to = mx) # bd = baseline density
  cd = density(comp, from = mn, to = mx) # cd = comparison density
  dis = MESS::auc(bd$x, abs(cd$y - bd$y))/(MESS::auc(bd$x, bd$y) + MESS::auc(cd$x, cd$y)) # dis = 0 is completely overlapping; dis = 1 is no overlap
  return(list(po = 1 - dis, 
              x = bd$x, # base and comp have same x
              y.base = bd$y,
              y.comp = cd$y))
}

# ?toupper
simple_cap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

process_nodes <- function(x){
  x = gsub(pattern = " ", replacement = "", x) # node locations included whitespace; 
  x = sapply(x, simple_cap, USE.NAMES = FALSE) # first letter capitalized
  return(x)
}

ts_plot <- function(data, y.lab){
  p <- ggplot(data, aes(x = Date, y = Value, col = Scenario)) +
    geom_line(size = 1, alpha = 0.75) +
    labs(y = y.lab) +
    scale_colour_brewer(type = "qual", palette = "Set1") +
    theme_minimal() +
    theme.mod
  return(p)
}

xy_range <- function(data){
  c("x.min" = min(data[["x"]], na.rm = TRUE),
    "x.max" = max(data[["x"]], na.rm = TRUE),
    "y.min" = min(data[["y"]], na.rm = TRUE),
    "y.max" = max(data[["y"]], na.rm = TRUE))
}

h5_read <- function(data, name){
  out = list()
  for (i in 1:nrow(data)){
    out[[data$scenario[i]]] = h5read(data$datapath[i], paste0("/hydro", name))
  }
  return(out)
}

h5_read_attr <- function(data, name, attr){
  out = list()
  for (i in 1:nrow(data)){
    out[[data$scenario[i]]] = h5readAttributes(data$datapath[i], paste0("/hydro", name))[[attr]]
  }
  return(out)
}

