function(input, output, session) {
  
  # Reactive values ----------------------------------------------------------------
  # dlwy = date list water year; drr = date range read; cl = channel list; acc = all common channels
  rv <- reactiveValues(H5 = NULL, H5META = NULL, STAGE = NULL, FLOW = NULL, AREA = NULL, VELOCITY = NULL, DLWY = NULL, DRR = NULL, CL = NULL, ACC = NULL, 
                       # AD = absolute difference; PO = proportion overlap; DN = density; SS = summary stats; B = base scenario; NB = non-base scenarios; DC = deleted channels 
                       AD = NULL, PO = NULL, DN = NULL, SS = NULL, B = NULL, NB = NULL, DC = NULL) 
  
  # Exploratory ----------------------------------------------------------------
  
  # * Metadata tab ----------------------------------------------------------------
  
  # ** select files ----------------------------------------------------------------
  
  volumes <- c(Home = fs::path_home(), getVolumes()())
  shinyFileChoose(input, "h5_files", roots = volumes, filetypes = c("h5"), session = session)
  
  filePaths <- reactive({
    parseFilePaths(volumes, input[["h5_files"]])
  })
  
  observe({
    req(isTRUE(nrow(filePaths()) > 0))
    rv[["H5"]] = mutate(filePaths(), scenario = gsub(pattern = ".h5", replacement = "", x = name))
  })
  
  # ** metadata ----------------------------------------------------------------
  
  h5Metadata <- reactive({
    req(rv[["H5"]])
    # extract metadata; h5_read_attr() is wrapper to h5readAttributes (see helper.R) 
    h5_read_attr(rv[["H5"]]) %>% 
      mutate(interval_vals = as.numeric(gsub('\\D','', interval)),
             interval_units = gsub('\\d','', interval),
             start_date = ymd_hms(start_date),
             end_date = calc_end_date(start_date, num_intervals, 
                                      interval_vals, interval_units))
  })
  #     end_date = as.character(end_date)
  #     out[i] = ifelse(nchar(end_date) > 10, end_date, paste(end_date, "00:00:00")) # time of 00:00:00 gets dropped; adding it for consistency
  output$metadataTable = renderDT({
    h5Metadata() %>% 
      mutate(start_date = as.character(start_date),
             end_date = as.character(end_date)) %>%
      select(Scenario = scenario, `Start Date` = start_date, `End Date` = end_date,
             Interval = interval, Channels = num_channels)},
    style = "bootstrap", rownames = FALSE, caption = "Table 1. Basic metadata for selected HDF5 files.",
    options = list(searching = FALSE, bPaginate = FALSE, info = FALSE, scrollX = TRUE)
  )
  
  # ** dates ----------------------------------------------------------------
  
  datesListRead <- reactive({
    d = h5Metadata()
    out = list()
    for (i in 1:nrow(d)){
      out[[d[["scenario"]][i]]] = tibble(Date = seq(from = d[["start_date"]][i], to = d[["end_date"]][i], 
                                                    by = paste(d[["interval_vals"]][i], d[["interval_units"]][i])),
                                         Index = get_date_indexes(d[["start_date"]][i], d[["start_date"]][i], d[["end_date"]][i],
                                                                  d[["interval_vals"]][i], d[["interval_units"]][i]))
    }
    return(out)
  })
  
  datesListReadSub <- reactive({
    drr = ymd_hms(paste(input[["date_range_read"]], c("00:00:00", "23:59:59")))
    dlr = datesListRead()
    out = list()
    for (i in names(dlr)){
      out[[i]] = filter(dlr[[i]], Date >= drr[1] & Date <= drr[2])
      # After reading data, the original index will be lost; need new index for subsetting data in visualization step 
      out[[i]][["SubIndex"]] = if (nrow(out[[i]]) > 0) 1:length(out[[i]][["Date"]]) else NULL 
    }
    return(out)
  })
  
  # ** intervals ----------------------------------------------------------------
  
  intervals <- reactive({
    d = h5Metadata()
    drr = ymd_hms(paste(input[["date_range_read"]], c("00:00:00", "23:59:59")))
    for (i in 1:nrow(d)){
      first_date = if_else(d[["start_date"]][i] > drr[1], d[["start_date"]][i], drr[1])
      last_date = if_else(d[["end_date"]][i] < drr[2], d[["end_date"]][i], drr[2])
      int_num = get_interval_number(first_date, last_date, d[["interval_vals"]][i], d[["interval_units"]][i])
      d[["num_intervals_in_range"]][i] = ifelse(int_num < 0, 0, int_num)
    }
    return(d)
  })

  intervalSub <- reactive({
    intervals() %>% filter(num_intervals_in_range > 1)
  })
  
  output$intervalTable = renderDT({
    intervals() %>% 
      select(Scenario = scenario, `Total Intervals` = num_intervals, `Intervals in Date Range` = num_intervals_in_range)},
    style = "bootstrap", rownames = FALSE, caption = "Table 2. Number of time intervals in selected HDF5 files.",
    options = list(searching = FALSE, bPaginate = FALSE, info = FALSE, scrollX = TRUE)
  )
  
  # ** dynamic UI ----------------------------------------------------------------
  
  observe({
    cond = !is.null(rv[["H5"]])
    toggle("metadata_msg", condition = !cond)
    toggleState("h5_files", condition = !cond)
    toggle("reset_app", condition = cond)
    toggle("date_range_read", condition = cond)
  })
  
  observe({
    req(rv[["H5"]])
    cond = max(intervals()[["num_intervals_in_range"]]) > 1
    toggle("node_loc", condition = cond)
    toggle("read_data", condition = cond)
    toggle("date_range_read_warn_small", condition = !cond)
  })
  
  observe({
    req(rv[["H5"]])
    cond = max(intervals()[["num_intervals_in_range"]]) > 35040 # threshold based on 1 year of 15-min intervals; b/c warning message advises selecting less than one year
    toggle("date_range_read_warn_large", condition = cond)
  })
  
  observe({
    d = bind_rows(datesListRead(), .id = "Scenario")
    min_date = floor_date(min(d[["Date"]], na.rm = TRUE), unit = "day")
    max_date = floor_date(max(d[["Date"]], na.rm = TRUE), unit = "day")
    updateDateRangeInput(session, "date_range_read",
                         start = min_date, end = max_date, 
                         min = min_date, max = max_date)
  })
  
  # ** channels ----------------------------------------------------------------
  
  channelList <- reactive({
    req(rv[["H5"]])
    out = list()
    for (i in 1:nrow(rv[["H5"]])){
      out[[rv[["H5"]]$scenario[i]]] = tibble(Channel = h5read(rv[["H5"]]$datapath[i], "/hydro/geometry/channel_number"),
                                             Index = 1:length(Channel))
    }
    return(out)
  })
  
  channelTibble <- reactive({
    bind_rows(channelList(), .id = "Scenario") %>% 
      filter(Scenario %in% intervalSub()[["scenario"]]) # exclude scenarios that will be dropped when reading data (b/c not enough data)
  })
  
  channelTibbleWide <- reactive({
    channelTibble() %>% 
      select(-Index) %>% 
      bind_rows(tibble(Scenario = "DefaultMap", Channel = channels)) %>% 
      mutate(Value = 1L) %>% 
      spread(key = Scenario, value = Value, fill = 0L) %>% 
      mutate(Total = rowSums(.) - Channel) # scenario columns are 0 and 1
  })
  
  output$channelTable <- renderDT({
    sc = intervalSub()[["scenario"]]
    ctw = channelTibbleWide() %>% 
      filter(Total < length(sc) + 1) %>% 
      select(c("Channel", "Default Map" = "DefaultMap", sc)) # sorting columns so that Default is always the 2nd column; Channel will always be first because of spread
    ctw}, 
    style = "bootstrap", rownames = FALSE,
    caption = "Table 3. Differences in channels included in default map file and selected HDF5 files with at least 2 time intervals in the date range (see Table 2). 
    1 and 0 indicate that channels are present or not present in a file, respectively. A table with no data indicates that all files have the same channels.",
    options = list(searching = FALSE, bPaginate = FALSE, info = FALSE, scrollX = TRUE)
  )
  
  allCommonChannels <- reactive({
    if (is.null(input[["h5_files"]])) {
      x = channels
    }else{
      x = channelTibbleWide() %>% 
        filter(Total == length(intervalSub()[["scenario"]]) + 1) %>% # filter to keep channels that are found in all scenarios and default map
        pull(Channel)
    }
    return(x) 
  })
  
  # ** nodes ----------------------------------------------------------------
  
  nodeList <- reactive({
    req(rv[["H5"]])
    out = list()
    for (i in 1:nrow(rv[["H5"]])){
      out[[rv[["H5"]]$scenario[i]]] = tibble(NodeLoc = process_nodes(h5read(rv[["H5"]]$datapath[i], "/hydro/geometry/channel_location")),
                                             Index = 1:length(NodeLoc))
    }
    return(out)
  })
  
  # ** read data ----------------------------------------------------------------
  
  observeEvent(input[["read_data"]],{
    rv[["H5META"]] = rv[["STAGE"]] = rv[["FLOW"]] = rv[["AREA"]] = rv[["VELOCITY"]] = rv[["DLRS"]] = rv[["DRR"]] = rv[["CL"]] = rv[["ACC"]] = NULL # clear out previous results
    
    rv[["H5META"]] = h5Metadata()
    rv[["DLRS"]] = datesListReadSub() 
    rv[["DRR"]] = input[["date_range_read"]]
    rv[["ACC"]] = allCommonChannels()
    rv[["CL"]] = lapply(channelList(), filter, Channel %in% rv[["ACC"]])
    nl = lapply(nodeList(), filter, NodeLoc == input[["node_loc"]]) # don't need to store in rv because only two options; once one is selected don't need to track any more
    
    withProgress(message = 'Reading...', value = 0, detail = "Stage",{
      rv[["STAGE"]] = h5_read(rv[["H5"]], nl, rv[["CL"]], rv[["DLRS"]], "/data/channel stage")
      setProgress(2.5/10, detail = "Flow")
      rv[["FLOW"]] = h5_read(rv[["H5"]], nl, rv[["CL"]], rv[["DLRS"]], "/data/channel flow")
      setProgress(5/10, detail = "Area")
      rv[["AREA"]] = h5_read(rv[["H5"]], nl, rv[["CL"]], rv[["DLRS"]], "/data/channel area")
      setProgress(7.5/10, message = "Calculating...", detail = "Velocity")
      rv[["VELOCITY"]] = list()
      for (i in names(rv[["FLOW"]])){
        rv[["VELOCITY"]][[i]] = rv[["FLOW"]][[i]]/rv[["AREA"]][[i]]
      }
      setProgress(9.5/10, message = "Finishing", detail = "")
    })
    h5closeAll()
    updateTabsetPanel(session, "explore_tabs", selected = "Time Series") # change tab after clicking on process output button
  })#/Read Button
  
  # * Time Series tab  ----------------------------------------------------------------
  
  # ** dates  ----------------------------------------------------------------
  
  datesList <- reactive({
    req(rv[["DLRS"]])
    drv = ymd_hms(paste(input[["date_range_viz"]], c("00:00:00", "23:59:59")))
    lapply(rv[["DLRS"]], filter, Date >= drv[1] & Date <= drv[2])
  })
  
  # ** velocity  ----------------------------------------------------------------
  
  velocityList <- reactive({
    req(rv[["VELOCITY"]])
    response_list(rv[["VELOCITY"]], datesList())
  })
  
  # using velocity to test date ranges and number of observations; should be same for flow and stage
  velocityTibble <- reactive({
    req(rv[["VELOCITY"]])
    response_tibble(velocityList(), datesList(), rv[["CL"]], input[["ts_channel"]]) %>% 
      filter(Scenario %in% input[["sel_scenarios"]])
  })
  
  # check that at least two observations for at least one scenario 
  obsCheck <- reactive({
    req(rv[["VELOCITY"]])
    if (nrow(velocityTibble()) > 1){
      scenario_rows = velocityTibble() %>% 
        group_by(Scenario) %>% 
        summarise(N = n())
      out = max(scenario_rows[["N"]]) > 1
    }else{
      out = FALSE  # if 0 or 1 rows in velocityTibble(), then not enough observations and obsCheck() is false
    }
    return(out) 
  })
  
  output$tsVelocityPlot = renderPlot({
    req(rv[["VELOCITY"]])
    ts_plot(velocityTibble(), "Velocity (ft/s)", "Velocity", obsCheck())
  })
  
  # ** flow  ----------------------------------------------------------------
  
  flowList <- reactive({
    req(rv[["FLOW"]])
    response_list(rv[["FLOW"]], datesList())
  })
  
  output$tsFlowPlot = renderPlot({
    req(rv[["FLOW"]])
    d = response_tibble(flowList(), datesList(), rv[["CL"]], input[["ts_channel"]]) %>% 
      filter(Scenario %in% input[["sel_scenarios"]])
    ts_plot(d, "Flow (cfs)", "Flow", obsCheck())
  })
  
  # ** stage  ----------------------------------------------------------------
  
  stageList <- reactive({
    req(rv[["STAGE"]])
    response_list(rv[["STAGE"]], datesList())
  })
  
  output$tsStagePlot = renderPlot({
    req(rv[["STAGE"]])
    d = response_tibble(stageList(), datesList(), rv[["CL"]], input[["ts_channel"]]) %>% 
      filter(Scenario %in% input[["sel_scenarios"]])
    ts_plot(d, "Stage (ft)", "Stage", obsCheck())
  })
  
  # ** dynamic UI ----------------------------------------------------------------
  
  # observe({
  #   if (is.null(rv[["FLOW"]])){
  #     hideTab(inputId = "explore_tabs", target = "Time Series")
  #   }else{
  #     showTab(inputId = "explore_tabs", target = "Time Series")
  #   }
  # })
  
  observe({
    cond = !is.null(rv[["FLOW"]])
    toggle("ts_msg", condition = !cond)
    toggle("ts_plots", condition = cond)
    toggle("ts_channel", condition = cond)
    toggle("map_channel", condition = cond)
    toggle("date_range_viz", condition = cond)
    toggle("base_scenario", condition = cond)
    toggle("sel_scenarios", condition = cond)
  })
  
  intervalCheck <- reactive({
    req(input[["sel_scenarios"]])
    # check if all selected scenarios use the same time interval
    length(unique(filter(rv[["H5META"]], scenario %in% input[["sel_scenarios"]])[["interval_units"]])) == 1
  })
  
  observe({
    req(input[["sel_scenarios"]])
    toggle("date_range_viz_warn", condition = !obsCheck())
    toggle("interval_warn", condition = !intervalCheck())
    toggle("run_comp", condition = length(input[["sel_scenarios"]]) > 1 & obsCheck() & intervalCheck()) 
    toggle("files_warn", condition = length(input[["sel_scenarios"]]) < 2)
  })
  
  observe({
    req(rv[["ACC"]])
    acc = rv[["ACC"]][!(rv[["ACC"]] %in% rv[["DC"]])]
    updatePickerInput(session, "ts_channel", choices = acc, selected = input[["map_channel"]])
  })
  
  observe({
    req(rv[["ACC"]])
    acc = rv[["ACC"]][!(rv[["ACC"]] %in% rv[["DC"]])]
    updatePickerInput(session, "map_channel", choices = acc, selected = input[["ts_channel"]])
  })
  
  observe({
    drr = rv[["DRR"]]
    updateDateRangeInput(session, "date_range_viz",
                         start = drr[1], end = drr[2],
                         min = drr[1], max = drr[2])
  })

  observe({
    sc = sort(names(rv[["FLOW"]]))
    updatePickerInput(session, "sel_scenarios", choices = sc, selected = sc)
  })
  
  observe({
    updatePickerInput(session, "base_scenario", choices = input[["sel_scenarios"]])
  })
  
  
  # Comparative ----------------------------------------------------------------
  
  # * summary statistics  ----------------------------------------------------------------
  
  allLists <- reactive({
    list("flow" = flowList(), "stage" = stageList(), "velocity" = velocityList())
  })
  
  sumStats <- reactive({   # fv = flow velocity
    al = allLists() # al = all lists
    cl = rv[["CL"]]
    out = list()
    for (j in c("flow", "velocity", "stage")){
      for (i in names(al[["flow"]])){
        out[[j]][[i]] = calc_summary_stats(al[[j]][[i]], channel.dim = 2, cl[[i]][["Channel"]]) # 3D array; first dim is nodes; 2nd is channels; 3rd is datetimes
      }
    }
    return(out)
  })
  
  # * run comparative analysis  ----------------------------------------------------------------
  
  observeEvent(input[["run_comp"]],{
    
    rv[["DN"]] = rv[["PO"]] = rv[["AD"]] = rv[["SS"]] = rv[["B"]] = rv[["NB"]] = NULL # clear out previous results
    
    withProgress(message = 'Running analysis...', value = 0,{
      al = allLists()
      ac = rv[["CL"]][[input[["base_scenario"]]]][["Channel"]] # ac = all channels; should be same for all scenarios
      ss = sumStats()
      ss.comb = list() # combine scenarios within each hydro metric (i.e., flow, velocity, stage)
      
      dn = list()  # dn = density
      po = list()  # po = proportion overlap
      ad = list()  # ad = absolute difference
      ad.re = list() # ad.re = absolute difference rescaled to 0-1 across channels and scenarios
      for (j in c("flow", "velocity", "stage")){
        ss.comb[[j]] = bind_rows(ss[[j]], .id = "scenario")
        po.base = al[[j]][[input[["base_scenario"]]]]
        ad.base = ss[[j]][[input[["base_scenario"]]]] %>% arrange(channel)
        dn.comp.list = list()
        po.comp.list = list()
        ad.comp.list = list()
        for (i in nonBase()){
          po.comp = al[[j]][[i]]
          ad.comp = ss[[j]][[i]] %>% arrange(channel)
          ad.comp.list[[i]] = abs(ad.comp[,summ.stats] - ad.base[,summ.stats]) %>% # summ.stats just specifices the summary statistic colums for the comparison (i.e., ignores channel column)
            mutate(channel = ad.comp[["channel"]])
          dn.chan.list = list()
          po.chan = vector(mode = "numeric", length = length(ac))
          for (k in 1:length(ac)){ # using channels in base scenario, but should be same in all scenarios (if filtering was correct)
            msd.chan = filter(ss.comb[[j]], channel == ac[k])
            mn = plyr::round_any(min(msd.chan[["min"]], na.rm = TRUE), accuracy = 0.001, f = floor)
            mx = plyr::round_any(max(msd.chan[["max"]], na.rm = TRUE), accuracy = 0.001, f = ceiling)
            
            po.out = proportion_overlap(po.base[1,k,], po.comp[1,k,], mn, mx)
            
            dn.chan.list[[k]] = bind_rows(tibble(scenario = input[["base_scenario"]], channel = ac[k], x = po.out[["x"]], y = po.out[["y.base"]]),
                                          tibble(scenario = i, channel = ac[k], x = po.out[["x"]], y = po.out[["y.comp"]]))
            
            po.chan[k] = po.out[["po"]]
            
            incProgress(1/(3 * length(nonBase()) * length(ac))) # 3 is for c("flow", "velocity", "stage")
          }
          dn.comp.list[[i]] = bind_rows(dn.chan.list)
          po.comp.list[[i]] = tibble(channel = ac, prop.overlap = po.chan)
        }
        dn[[j]] = bind_rows(dn.comp.list, .id = "comp") %>% 
          mutate(base = input[["base_scenario"]]) # adding base scenario to output for completeness, but not necessary for subsequent calcs (I think)
        po[[j]] = bind_rows(po.comp.list, .id = "comp") %>% 
          mutate(base = input[["base_scenario"]])
        ad[[j]] = bind_rows(ad.comp.list, .id = "comp") %>% 
          mutate(base = input[["base_scenario"]])
        ad.re[[j]] = ad[[j]] %>% 
          mutate_at(.vars = rescale.cols, .funs = rescale)
      }
    })
    rv[["DN"]] = dn
    rv[["PO"]] = po
    rv[["AD"]] = ad.re              
    rv[["SS"]] = ss.comb
    rv[["B"]] = input[["base_scenario"]] # need to stash all input values at time button was clicked
    rv[["NB"]] = nonBase()
    updateNavbarPage(session, "nav_tabs", selected = "Comparative") # change tab after clicking on process output button
  })#/Run Button
  
  # * dynamic UI ----------------------------------------------------------------
  
  nonBase <- reactive({
    # names of scenarios that were not selected as baseline scenario
    req(input[["base_scenario"]])
    input[["sel_scenarios"]][input[["sel_scenarios"]] != input[["base_scenario"]]]
  })
  
  observe({
    updateSelectInput(session, "comp_scenario", choices = rv[["NB"]])
  })
  
  observe({
    cond = !is.null(rv[["PO"]])
    toggle("map_pal", condition = cond)
    toggle("color_range", condition = cond)
    toggle("remove_channels", condition = cond)
    toggle("mainPanel", condition = cond)  
    toggle("plot_info", condition = cond)
  })
  
  observe({ 
    req(rv[["NB"]])                        
    cond = length(rv[["NB"]]) > 1 & input[["summ_stat"]] != ""
    toggle("scale_axes", condition = cond)
    toggle("comp_scenario", condition = cond)
  })
  
  observe({ 
    req(rv[["NB"]]) 
    cond = input[["summ_stat"]] == ""
    toggle("ss_help", condition = cond)
  })
  
  observeEvent(input[["metric"]],{
    ch = summ.stats
    if(input[["metric"]] == "stage") ch = summ.stats[summ.stats != "prop.neg"]
    updateSelectInput(session, "summ_stat", choices = ch, selected = input[["summ_stat"]])
  })
  
  # * filter ----------------------------------------------------------------
  
  poSub <- reactive({
    req(rv[["PO"]], input[["comp_scenario"]])
    po = rv[["PO"]][[input[["metric"]]]]
    return(po[po[["comp"]] == input[["comp_scenario"]] & !(po[["channel"]] %in% rv[["DC"]]),])
  })
  
  poSubChannel <- reactive({
    po = poSub()
    return(po[po[["channel"]] == input[["map_channel"]], ])
  })
  
  adSub <- reactive({
    req(rv[["AD"]], input[["comp_scenario"]], input[["summ_stat"]] != "")
    ad = rv[["AD"]][[input[["metric"]]]]
    ad.sub = ad[ad[["comp"]] == input[["comp_scenario"]] & !(ad[["channel"]] %in% rv[["DC"]]),]
    ad.sub[["value"]] = ad.sub[[input[["summ_stat"]]]]
    return(ad.sub)
  })
  
  adSubChannel <- reactive({
    ad = adSub()
    return(ad[ad[["channel"]] == input[["map_channel"]], ])
  })
  
  ssSub <- reactive({
    req(input[["comp_scenario"]], input[["summ_stat"]] != "")
    ss = rv[["SS"]][[input[["metric"]]]]
    ss[["value"]] = ss[[input[["summ_stat"]]]] # rename summary stat column to value for plotting later
    ss = ss[ss[["channel"]] == input[["map_channel"]] & ss[["scenario"]] %in% c(rv[["B"]], input[["comp_scenario"]]),]
    return(ss)
  })
  
  densityChannel <- reactive({
    dn = rv[["DN"]][[input[["metric"]]]]
    dn = dn[dn[["channel"]] == input[["map_channel"]],]
  })
  
  densitySub <- reactive({ 
    req(input[["comp_scenario"]])
    dn = densityChannel()
    dn = dn[dn[["comp"]] == input[["comp_scenario"]],]
    dn[["scenario"]] = factor(dn[["scenario"]], levels = c(rv[["B"]], input[["comp_scenario"]]))
    return(dn)
  })
  
  densityRange <- reactive({
    # x-axes were scaled the same (for each channel) when calculating density
    if (input[["scale_axes"]] == TRUE){
      df = densityChannel()
    }else{
      df = densitySub()
    }
    y.thresh = max(df[["y"]])/100  # just guessed at threshold value; might need to be adjusted; looked okay in my spot checks
    df = df[df[["y"]] > y.thresh,]
    # out = xy_range(df)
    out = c("x.min" = min(df[["x"]], na.rm = TRUE),
            "x.max" = max(df[["x"]], na.rm = TRUE),
            "y.min" = min(df[["y"]], na.rm = TRUE),
            "y.max" = max(df[["y"]], na.rm = TRUE))
    return(out)
  })
  
  # * density plot ----------------------------------------------------------------
  
  output$densityPlot = renderPlot({
    po = poSubChannel()
    po.lab = tibble(X = Inf, Y = Inf, H = 1.5, V = 2, L = formatC(round(po[["prop.overlap"]], 3), format='f', digits=3)) # create a data.frame with formatted proportion overlap value for plotting in upper right corner of density plot
    
    ad = adSubChannel()
    ad.lab = tibble(X = -Inf, Y = Inf, H = -0.5, V = 2, L = formatC(ad[["value"]], format='f', digits = ifelse(ad[["value"]] < 100, 3, 0), big.mark = ",")) # create a data.frame with formatted absolute difference value for plotting in upper left corner of density plot
    
    df = densitySub()
    dr = densityRange()
    
    p <- ggplot(df) +
      geom_density(aes(x = x, y = y, fill = factor(scenario), col = factor(scenario)), stat = "identity", alpha = 0.6) +
      labs(x = names(x.labs)[x.labs == input[["metric"]]], y = "Density") +
      geom_text(data = ad.lab, aes(x = X, y = Y, hjust = H, vjust = V, label = L), size = 6, col = ifelse(input[["type"]] == "ad", 2, 1)) +
      geom_text(data = po.lab, aes(x = X, y = Y, hjust = H, vjust = V, label = L), size = 6, col = ifelse(input[["type"]] == "ad", 1, 2)) +
      scale_fill_manual(name = "Scenario", values = c("#6a3d9a", "#ff7f00")) +
      scale_colour_manual(name = "Scenario", values = c("#6a3d9a", "#ff7f00"), guide = FALSE) +
      coord_cartesian(xlim = c(dr[["x.min"]], dr[["x.max"]]), ylim = c(0, dr[["y.max"]])) +
      ggtitle(paste("Channel", input[["map_channel"]])) +
      theme_minimal() +
      theme.mod
    if (input[["summ_stat"]] != "prop.neg"){  # doesn't make sense to plot prop.neg/reversal on these distributions
      p = p + geom_vline(data = ssSub(), aes(xintercept = value, col = factor(scenario)), linetype = "dashed", size = 1)
    }
    return(p)
  }, width = 500, height = 280)
  
  # * map data ----------------------------------------------------------------
  # join map data with appropriate value for coloring channels on map
  
  shpSub <- reactive({
    subset(shp, !(channel_nu %in% rv[["DC"]]))
  })
  
  poData <- reactive({
    req(input[["type"]] == "po")
    shp = shpSub()
    cr = input[["color_range"]]
    shp@data = shp@data %>% 
      left_join(select(poSub(), channel_nu = channel, overlap = prop.overlap), by = "channel_nu") %>% 
      mutate(overlap = ifelse(overlap < cr[1], cr[1],
                              ifelse(overlap > cr[2], cr[2], overlap)))
    pal = colorNumeric(
      palette = input[["map_pal"]], 
      domain = cr)
    return(list(shp, pal))
  })
  
  adData <- reactive({
    req(input[["type"]] == "ad")
    shp = shpSub()
    cr = input[["color_range"]]
    shp@data = shp@data %>%
      left_join(select(adSub(), channel_nu = channel, value), by = "channel_nu") %>% 
      mutate(value = ifelse(value < cr[1], cr[1],
                            ifelse(value > cr[2], cr[2], value)))
    pal = colorNumeric(
      palette = input[["map_pal"]],
      domain = cr,
      reverse = TRUE  # for difference, high values indicate greater effect, which is opposite of proportion overlap; reverse the palette to reduce cognitive burden of palette flipping when moving from proportion overlap to absolute difference
    )
    return(list(shp, pal))
  })
  
  # * channel map  ----------------------------------------------------------------
  
  output$Map = renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE)) %>%
      setView(lng = -121.75, lat = 38.05, zoom = 10) 
  })
  
  proxyMap = leafletProxy("Map", session)
  
  observe({
    input[["nav_tabs"]]
    proxyMap %>% 
      addProviderTiles(input[["map_back"]])
  })
  
  observe({
    input[["nav_tabs"]]               # dependency on nav_tabs displays shapefile when tab is clicked (assuming next condition is met)
    if (is.null(rv[["PO"]])){         # display default map until comparative results are generated
      proxyMap %>% 
        clearShapes() %>% 
        addPolylines(data = shp,
                     weight = 6,
                     layerId = ~channel_nu)
    }
  })
  
  # ** draw shapefiles with color scales ----------------------------------------------------------------
  
  # draw proportion overlap map
  observeEvent(poData(),{
    pd = poData()[[1]]
    pal.po <- poData()[[2]]
    delta.map = proxyMap %>%
      clearShapes() %>% 
      addPolylines(data = pd,
                   color = ~pal.po(overlap),
                   weight = 6,
                   layerId = ~channel_nu) %>%
      clearControls() %>% 
      addLegend("bottomright", pal = pal.po, values = pd@data[["overlap"]], title = "Overlap", opacity = 0.8)
    return(delta.map)
  })
  
  # draw absolute difference map
  observeEvent(adData(),{
    ad = adData()[[1]]
    pal.ad = adData()[[2]]
    delta.map = proxyMap %>%
      clearShapes() %>% 
      addPolylines(data = ad,
                   color = ~pal.ad(value),
                   weight = 6,
                   layerId = ~channel_nu) %>%
      clearControls() %>% 
      addLegend("bottomright", pal = pal.ad, values = ad@data[["value"]], title = "Difference", opacity = 0.8)
    return(delta.map)
  })
  
  # * map interactivity  ----------------------------------------------------------------
  # zoom and place map markers for selected channels
  # based on the example here: https://uasnap.shinyapps.io/ex_leaflet/
  
  # highlight selected channel; black chosen to highlight because other choices (e.g., yellow) might be used in map (i.e., hard to see yellow channel highlighted in yellow)
  ap_sel <- function(map, channel){
    addPolylines(map,
                 data = subset(shp, channel_nu == channel),
                 color = "black",
                 weight = 12,
                 opacity = 0.4,
                 layerId = "Selected")
  }
  
  observeEvent(input[["Map_shape_click"]], {
    # update the list of deleted channels (rv[["DC"]]) on map click (when remove_channels is on)
    p <- input[["Map_shape_click"]]
    req(p[["id"]])
    if (input[["remove_channels"]] == TRUE){
      p <- input[["Map_shape_click"]]
      if (is.null(rv[["DC"]])){
        rv[["DC"]] = p[["id"]]
      }else{
        rv[["DC"]] = c(rv[["DC"]], p[["id"]])
      }
    }else{
      # update the location selectInput on map clicks
      if (p[["id"]] != "Selected"){  # 'Selected' layer placed on top of reach layer; don't want to update when 'Selected' layer clicked
        updateSelectInput(session, "map_channel", selected = p[["id"]])
        proxyMap %>% setView(lng = p[["lng"]], lat = p[["lat"]], input[["Map_zoom"]]) %>% ap_sel(p[["id"]])
      } 
    }
  })
  
  # update the map markers and view on location selectInput changes
  observeEvent(input[["map_channel"]], { 
    req(input$Map_zoom) # map zoom is initially zero until set in renderLeaflet
    if (input[["remove_channels"]] == FALSE){
      cll_sub <- filter(cll, channel_nu == input[["map_channel"]])
      if(nrow(cll_sub) == 0){  
        proxyMap %>% removeShape(layerId = "SelectedChannel")
      }else{
        proxyMap %>% setView(lng = cll_sub[["lon"]], lat = cll_sub[["lat"]], input[["Map_zoom"]]) %>% ap_sel(input[["map_channel"]])
      }
    }
  })
  
  # info alerts  ----------------------------------------------------------------
  observeEvent(input[["explore_info"]], {
    if (input[["explore_tabs"]] == "Metadata"){
      sendSweetAlert(
        session = session,
        title = "",             
        text = 
          tags$span(
            tags$p(align="left",
                   "Selecting HDF5 files reads the metadata (but not data) for each selected file and populates the Metadata tab with 
                   tables related to the start and end dates, time intervals, and channels included in the selected files."
            ),
            tags$p(align="left",
                   "Because the HDF5 files might contain large amounts of data, the user can select a date range 
                   (based on the file metadata) to read a subset of the file to reduce the time that DSM2 Viz spends 
                   reading data. The app also requires that the user selects the upstream or downstream node for each 
                   channel. We have created an ", tags$a(href="https://fishsciences.shinyapps.io/dsm2-map/", "interactive map"), " showing DSM2 
                   channels and nodes, including the upstream and downstream node."
            ),
            tags$p(align="left",
                   "Click the 'Read Data' button to read the specified subset of the stage, flow, and channel 
                   area output; velocity is calculated by dividing flow by channel area."
            )
          ),
        type = "info",
        btn_labels = "OK"
      )
    }else{
      sendSweetAlert(
        session = session,
        title = "",             
        text = 
          tags$span(
            tags$p(align="left",
                   "The date range on the Time Series tab is initially set from the date range on the Metadata tab. 
                   Changing the date range allows for zooming in/out on the time series plots. The comparative 
                   analysis is based on the date range selected on the Time Series tab."
            ),
            tags$p(align="left",
                   "Selecting a file for use as a baseline scenario has no effect on the time series plots; 
                   it is only used in the comparative analysis."
            )
          ),
        type = "info",
        btn_labels = "OK"
      )
    }
  })
  
  observeEvent(input[["map_info"]], {
    if (is.null(rv[["PO"]])){
      sendSweetAlert(
        session = session,
        title = "",             
        text = 
          tags$p(align="left",
                 "This is the default channel map. When DSM2 HYDRO output files are selected and read, and the 
                 comparative analysis is run, this map will show the color-scaled results of the comparative analysis."
                 ),
        type = "info",
        btn_labels = "OK"
      )
    }else{
      sendSweetAlert(
        session = session,
        title = "",             
        text = 
          tags$span(
            tags$p(align="left",
                   "Clicking on a channel pans to the selected channel and updates the 'Selected channel' input in 
                   the top left panel."
            ),
            tags$p(align="left",
                   "Overlap is the proportion overlap in the density distributions for the selected comparison. 
                   Proportion overlap ranges from 0 to 1."
            ),
            tags$p(align="left",
                   "Difference is the absolute difference in the selected summary statistic for the selected comparison. 
                   The absolute difference values are rescaled across channels and scenarios to values ranging from 0 to 1; 
                   except for 'Reversal' which naturally ranges from 0 to 1."
            ),
            tags$p(align="left",
                   "By default, map color is scaled from 0 to 1. To adjust the map color range, click on 
                   'Show map tools' in the top left panel."
            ),
            tags$p(align="left",
                   "When the 'Remove channels' switch is on (under 'Show map tools'), clicking on a channel removes 
                   that channel from the map."
            )
          ),
        type = "info",
        btn_labels = "OK"
      )
    }
  })
  
  observeEvent(input[["plot_info"]], {
    sendSweetAlert(
      session = session,
      title = "",          
      text = 
        tags$span(
          tags$p(align="left",
                 "The density plot shows the distribution of velocity/flow/stage values over the selected date range 
                 for the selected comparison." 
          ),
          tags$p(align="left",
                 "The rescaled absolute difference in the selected summary statistic and proportion overlap of 
                 the distributions are shown in the top left and top right corners of the plot, respectively."
          ),
          tags$p(align="left",
                 "The dashed vertical lines show the value of the selected summary statistic. Vertical lines are 
                 not displayed when Reversal is selected as the summary statistic." 
          ),
          tags$p(align="left",
                 "Reversal is the proportion of values that are negative. Reversal only applies to flow and velocity, 
                 not stage."
          ),
          tags$p(align="left",
                 "The minimum and maximum summary statistics do not always align with the minimum and maximum extent 
                 of the distributions because the density estimates are based on a smoothing kernel."
          )
        ),
      type = "info",
      btn_labels = "OK"
    )
  })
  
  session$onSessionEnded(function() {
    h5closeAll()
  })
  
  if (Sys.getenv("WITHIN_ELECTRON") == "1") {
    # needed for windows apps. As soons as the session ends
    # we end the process and terminate the R process
    session$onSessionEnded(function() {
      stopApp()
    })
  }
  
}


