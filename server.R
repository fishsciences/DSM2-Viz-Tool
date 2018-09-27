function(input, output, session) {
  
  # Reactive values ----------------------------------------------------------------
  rv <- reactiveValues(H5 = NULL, AD = NULL, PO = NULL, DN = NULL, SS = NULL, B = NULL, NB = NULL, DC = NULL) # DC = deleted channels 
  
  # Process Output ----------------------------------------------------------------
  
  # * dynamic UI ----------------------------------------------------------------
  
  observe({
    # several UI elements are initially hidden; shown after files uploaded
    cond = !is.null(rv$H5) & isTRUE(nrow(rv$H5) > 1)  # when rv$H5 is null, 2nd condition returns 'logical(0)'; need to wrap in isTRUE to get it to return FALSE
    toggle("sel_scenarios", condition = cond)
    toggle("base_scenario", condition = cond)
    toggle("date_range", condition = cond)
    toggle("nodes", condition = cond)
    toggle("ts_plots", condition = cond)
    toggle("ts_channel", condition = cond)
  })
  
  observe({
    req(rv$H5, isTRUE(nrow(rv$H5) > 1), input$sel_scenarios, input$base_scenario)
    toggle("proc_dsm2", condition = length(input$sel_scenarios) > 1)
  })
  
  observe({
    req(rv$H5)
    toggle("upload_warn", condition = nrow(rv$H5) < 2)
  })
  
  observe({
    sc = rv$H5$scenario
    updatePickerInput(session, "sel_scenarios", choices = sc, selected = sc)
  })
  
  observe({
    updatePickerInput(session, "base_scenario", choices = input$sel_scenarios)
  })
  
  observe({
    ad = do.call("c", datesList()) # ad = all dates; not sure if this is best way to concatenate list of dates, but unlist loses date formatting
    updateDateRangeInput(session, "date_range",
                         start = min(ad), end = max(ad),
                         min = min(ad), max = max(ad))
  })
  
  observe({
    ac = availableChannels()[!(availableChannels() %in% rv$DC)] 
    updatePickerInput(session, "ts_channel", choices = ac, selected = input$map_channel)
  })
  
  observe({
    ac = availableChannels()[!(availableChannels() %in% rv$DC)] 
    updatePickerInput(session, "map_channel", choices = ac, selected = input$ts_channel)
  })
  
  observe({
    req(rv$H5, isTRUE(nrow(rv$H5) > 1), input$sel_scenarios)
    toggle("files_warn", condition = length(input$sel_scenarios) < 2)
  })
  
  # * upload  ----------------------------------------------------------------
  
  observe({
    req(input$h5_files)
    rv$H5 = mutate(input$h5_files, scenario = gsub(pattern = ".h5", replacement = "", x = name))
  })
  
  nonBase <- reactive({
    # names of scenarios that were selected as baseline scenario
    req(input$base_scenario, input$sel_scenarios)
    input$sel_scenarios[input$sel_scenarios != input$base_scenario]
  })
  
  # * extract ----------------------------------------------------------------
  
  selectedScenarios <- reactive({
    req(rv$H5)
    df = filter(rv$H5, scenario %in% input$sel_scenarios) 
    req(nrow(df) > 0) # if user uploads 2nd set of files, then these things get out of sync (presumably) and crash the app; trying to catch that here and stop app until values are available
    return(df)
  })
  
  channelList <- reactive({
    h5_read(selectedScenarios(), "/geometry/channel_number") 
  })
  
  flowList <- reactive({
    h5_read(selectedScenarios(), "/data/channel flow")
  })
  
  areaList <- reactive({
    h5_read(selectedScenarios(), "/data/channel area")
  })
  
  stageList <- reactive({
    h5_read(selectedScenarios(), "/data/channel stage")
  })
  
  velocityList <- reactive({
    out = list()
    for (i in names(flowList())){
      out[[i]] = flowList()[[i]]/areaList()[[i]]
    }
    return(out)
  })
  
  startDateList <- reactive({
    h5_read_attr(selectedScenarios(), "/data/channel flow", "start_time")
  })
  
  timeIntervalList <- reactive({
    h5_read_attr(selectedScenarios(), "/data/channel flow", "interval")
  })
  
  datesList <- reactive({
    til = timeIntervalList()
    sdl = startDateList()
    fl = flowList()
    out = list()
    for (i in names(til)){
      validate(need(til[[i]] %in% c("15min", "1hour"), "Time step interval must be '15min' or '1hour'. Please select different file(s)."))
      by.string = ifelse(til[[i]] == "15min", "15 min", "1 hour")
      out[[i]] = seq(from = ymd_hms(sdl[[i]]), length.out = dim(fl[[i]])[3], by = by.string)
    }
    return(out)
  })
  
  nodeList <- reactive({
    h5_read(selectedScenarios(), "/geometry/channel_location")
  })
  
  availableChannels <- reactive({
    if (is.null(input$h5_files)) {
      x = channels
    }else{
      cl = channelList()
      cl[["shp.chan"]] = channels # add shapefile channels to channel list
      x = sort(Reduce(intersect, cl)) # identify channels that are found in all lists
    }
    return(x) 
  })
  
  # * filter  ----------------------------------------------------------------
  
  fvcd <- reactive({   # fvcd = flow velocity channel dates; named before added other output, i.e., stage, nodes
    fl = flowList()
    vl = velocityList()
    sl = stageList()
    cl = channelList()
    dl = datesList()
    nl = nodeList()
    
    fo = list() # fo = flow out
    vo = list() # vo = velocity out
    so = list() # so = stage out
    co = list() # co = channels out; trying to preserve order of channels in H5 files (perhaps unnecessarily?)
    do = list() # do = dates out
    for (i in names(flowList())){
      ni = which(process_nodes(nl[[i]]) == input$nodes)  # process_nodes formats text to match input$nodes
      ci = which(cl[[i]] %in% availableChannels())
      di = which(dl[[i]] >= input$date_range[1] & dl[[i]] <= input$date_range[2])  # di = date indices
      fo[[i]] = fl[[i]][ni, ci, di]
      vo[[i]] = vl[[i]][ni, ci, di]
      so[[i]] = sl[[i]][ni, ci, di]
      co[[i]] = cl[[i]][ci]
      do[[i]] = dl[[i]][di]
    }
    return(list("flow" = fo, "velocity" = vo, "channels" = co, "stage" = so, "chan.index" = ci, "dates" = do))
  })
  
  # * time series plots  ----------------------------------------------------------------
  
  tsDF <- reactive({
    al = fvcd() # al = all lists; fvcd is a list of lists
    cl = al[["channels"]]
    dl = al[["dates"]]
    
    out = list()
    for (j in c("flow", "velocity", "stage")){
      for (i in names(cl)){
        ci = which(cl[[i]] %in% input$ts_channel)
        out[[j]][[i]] = tibble(Date = dl[[i]],
                               Value = al[[j]][[i]][ci,])
      }
      out[[j]] = bind_rows(out[[j]], .id = "scenario")
    }
    return(out)
  })
  
  output$tsVelocityPlot = renderPlot({
    req(nrow(rv$H5) > 1, nrow(tsDF()$velocity) > 0)
    ts_plot(tsDF()$velocity, "Velocity (ft/s)")
  })
  
  output$tsFlowPlot = renderPlot({
    req(nrow(rv$H5) > 1, nrow(tsDF()$flow) > 0)
    ts_plot(tsDF()$flow, "Flow (cfs)")
  })
  
  output$tsStagePlot = renderPlot({
    req(nrow(rv$H5) > 1, nrow(tsDF()$stage) > 0)
    ts_plot(tsDF()$stage, "Stage (ft)")
  })
  
  # * summary statistics  ----------------------------------------------------------------
  
  sumStats <- reactive({   # fv = flow velocity
    al = fvcd() # al = all lists; fvcd is a list of lists
    cl = al[["channels"]]
    out = list()
    for (j in c("flow", "velocity", "stage")){
      for (i in names(cl)){
        out[[j]][[i]] = calc_summary_stats(al[[j]][[i]], 1, cl[[i]])
      }
    }
    return(out)
  })
  
  # * process  ----------------------------------------------------------------
  
  observeEvent(input$proc_dsm2,{
    
    rv$DN = rv$PO = rv$AD = rv$SS = rv$B = rv$NB = NULL # clear out previous results
    
    withProgress(message = 'Processing DSM2 output...', value = 0, detail = "Initializing...",{
      al = fvcd() # al = all lists; fvcd is a list of lists
      ac = al[["channels"]][[input$base_scenario]] # ac = all channels; should be same for all scenarios
      ss = sumStats()
      ss.comb = list() # combine scenarios within each hydro metric (i.e., flow, velocity)
      
      dn = list()  # dn = density
      po = list()  # po = proportion overlap
      ad = list()  # ad = absolute difference
      ad.re = list() # ad.re = absolute difference rescaled to 0-1 across channels and scenarios
      for (j in c("flow", "velocity", "stage")){
        ss.comb[[j]] = bind_rows(ss[[j]], .id = "scenario")
        po.base = al[[j]][[input$base_scenario]]
        ad.base = ss[[j]][[input$base_scenario]] %>% arrange(channel)
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
            mn = plyr::round_any(min(msd.chan$min, na.rm = TRUE), accuracy = 0.001, f = floor)
            mx = plyr::round_any(max(msd.chan$max, na.rm = TRUE), accuracy = 0.001, f = ceiling)
            
            po.out = proportion_overlap(po.base[k,], po.comp[k,], mn, mx)
            
            dn.chan.list[[k]] = bind_rows(tibble(scenario = input$base_scenario, channel = ac[k], x = po.out$x, y = po.out$y.base),
                                          tibble(scenario = i, channel = ac[k], x = po.out$x, y = po.out$y.comp))
            
            po.chan[k] = po.out$po
            
            incProgress(1/(3 * length(nonBase()) * length(ac)), detail = "Calculating...") # three is for c("flow", "velocity", "stage")
          }
          dn.comp.list[[i]] = bind_rows(dn.chan.list)
          po.comp.list[[i]] = tibble(channel = ac, prop.overlap = po.chan)
        }
        dn[[j]] = bind_rows(dn.comp.list, .id = "comp") %>% 
          mutate(base = input$base_scenario) # adding base scenario to output for completeness, but not necessary for subsequent calcs (I think)
        po[[j]] = bind_rows(po.comp.list, .id = "comp") %>% 
          mutate(base = input$base_scenario)
        ad[[j]] = bind_rows(ad.comp.list, .id = "comp") %>% 
          mutate(base = input$base_scenario)
        ad.re[[j]] = ad[[j]] %>% 
          mutate_at(.vars = rescale.cols, .funs = rescale)
      }
    })
    rv$DN = dn
    rv$PO = po
    rv$AD = ad.re              
    rv$SS = ss.comb
    rv$B = input$base_scenario # need to stash all values at time button was clicked
    rv$NB = nonBase()
    updateNavbarPage(session, "nav_tabs", selected = "Visualize DSM2 Output") # change tab after clicking on process output button
  })#/Run Button
  
  
  # Visualize DSM2 Output ----------------------------------------------------------------
  
  # * dynamic UI ----------------------------------------------------------------
  
  observe({
    updateSelectInput(session, "comp_scenario", choices = rv$NB)
  })
  
  observe({
    cond = !is.null(rv$PO)
    toggle("map_pal", condition = cond)
    toggle("color_range", condition = cond)
    toggle("remove_channels", condition = cond)
    toggle("mainPanel", condition = cond)  
    toggle("plot_info", condition = cond)
  })
  
  observe({ 
    req(rv$NB)                        
    cond = length(rv$NB) > 1 & input$summ_stat != ""
    toggle("scale_axes", condition = cond)
    toggle("comp_scenario", condition = cond)
  })

  observe({ 
    req(rv$NB) 
    cond = input$summ_stat == ""
    toggle("ss_help", condition = cond)
  })
  
  observeEvent(input$metric,{
    ch = summ.stats
    if(input$metric == "stage") ch = summ.stats[summ.stats != "prop.neg"]
    updateSelectInput(session, "summ_stat", choices = ch, selected = input$summ_stat)
  })
  
  # * filter ----------------------------------------------------------------
  
  poSub <- reactive({
    req(rv$PO, input$comp_scenario)
    po = rv$PO[[input$metric]]
    return(po[po[["comp"]] == input$comp_scenario & !(po[["channel"]] %in% rv$DC),])
  })
  
  poSubChannel <- reactive({
    po = poSub()
    return(po[po[["channel"]] == input$map_channel, ])
  })
  
  adSub <- reactive({
    req(rv$AD, input$comp_scenario, input$summ_stat != "")
    ad = rv$AD[[input$metric]]
    ad.sub = ad[ad[["comp"]] == input$comp_scenario & !(ad[["channel"]] %in% rv$DC),]
    ad.sub[["value"]] = ad.sub[[input$summ_stat]]
    return(ad.sub)
  })
  
  adSubChannel <- reactive({
    ad = adSub()
    return(ad[ad[["channel"]] == input$map_channel, ])
  })
  
  ssSub <- reactive({
    req(input$comp_scenario, input$summ_stat != "")
    ss = rv$SS[[input$metric]]
    ss[["value"]] = ss[[input$summ_stat]] # rename summary stat column to value for plotting later
    ss = ss[ss[["channel"]] == input$map_channel & ss[["scenario"]] %in% c(rv$B, input$comp_scenario),]
    return(ss)
  })
  
  densityChannel <- reactive({
    dn = rv$DN[[input$metric]]
    dn = dn[dn$channel == input$map_channel,]
  })
  
  densitySub <- reactive({ 
    req(input$comp_scenario)
    dn = densityChannel()
    dn = dn[dn[["comp"]] == input$comp_scenario,]
    dn[["scenario"]] = factor(dn[["scenario"]], levels = c(rv$B, input$comp_scenario))
    return(dn)
  })
  
  densityRange <- reactive({
    # x-axes were scaled the same (for each channel) when calculating density
    if (input$scale_axes == TRUE){
      df = densityChannel()
    }else{
      df = densitySub()
    }
    y.thresh = max(df$y)/100  # just guessed at threshold value; might need to be adjusted; looked okay in my spot checks
    df = df[df[["y"]] > y.thresh,]
    out = xy_range(df)
    return(out)
  })
  
  # * density plot ----------------------------------------------------------------
  
  output$densityPlot = renderPlot({
    po = poSubChannel()
    po.lab = data_frame(X = Inf, Y = Inf, H = 1.5, V = 2, L = formatC(round(po$prop.overlap, 3), format='f', digits=3)) # create a data.frame with formatted proportion overlap value for plotting in upper right corner of density plot
    
    ad = adSubChannel()
    ad.lab = data_frame(X = -Inf, Y = Inf, H = -0.5, V = 2, L = formatC(ad$value, format='f', digits = ifelse(ad$value < 100, 3, 0), big.mark = ",")) # create a data.frame with formatted absolute difference value for plotting in upper left corner of density plot
    
    df = densitySub()
    dr = densityRange()
    
    p <- ggplot(df) +
      geom_density(aes(x = x, y = y, fill = factor(scenario), col = factor(scenario)), stat = "identity", alpha = 0.6) +
      labs(x = names(x.labs)[x.labs == input$metric], y = "Density") +
      geom_text(data = ad.lab, aes(x = X, y = Y, hjust = H, vjust = V, label = L), size = 6, col = ifelse(input$type == "ad", 2, 1)) +
      geom_text(data = po.lab, aes(x = X, y = Y, hjust = H, vjust = V, label = L), size = 6, col = ifelse(input$type == "ad", 1, 2)) +
      scale_fill_manual(name = "Scenario", values = c("#6a3d9a", "#ff7f00")) +
      scale_colour_manual(name = "Scenario", values = c("#6a3d9a", "#ff7f00"), guide = FALSE) +
      coord_cartesian(xlim = c(dr[["x.min"]], dr[["x.max"]]), ylim = c(0, dr[["y.max"]])) +
      ggtitle(paste("Channel", input$map_channel)) +
      theme_minimal() +
      theme.mod
    if (input$summ_stat != "prop.neg"){  # doesn't make sense to plot prop.neg/reversal on these distributions
      p = p + geom_vline(data = ssSub(), aes(xintercept = value, col = factor(scenario)), linetype = "dashed", size = 1)
    }
    return(p)
  }, width = 500, height = 280)
  
  # * map data ----------------------------------------------------------------
  # join map data with appropriate value for coloring channels on map
  
  shpSub <- reactive({
    subset(shp, !(channel_nu %in% rv$DC))
  })
  
  poData <- reactive({
    req(input$type == "po")
    shp = shpSub()
    cr = input$color_range
    shp@data = shp@data %>% 
      left_join(select(poSub(), channel_nu = channel, overlap = prop.overlap), by = "channel_nu") %>% 
      mutate(overlap = ifelse(overlap < cr[1], cr[1],
                              ifelse(overlap > cr[2], cr[2], overlap)))
    pal = colorNumeric(
      palette = input$map_pal, 
      domain = cr)
    return(list(shp, pal))
  })
  
  adData <- reactive({
    req(input$type == "ad")
    shp = shpSub()
    cr = input$color_range
    shp@data = shp@data %>%
      left_join(select(adSub(), channel_nu = channel, value), by = "channel_nu") %>% 
      mutate(value = ifelse(value < cr[1], cr[1],
                            ifelse(value > cr[2], cr[2], value)))
    pal = colorNumeric(
      palette = input$map_pal,
      domain = cr,
      reverse = TRUE  # for difference, high values indicate greater effect, which is opposite of proportion overlap; reverse the palette to reduce cognitive burden of palette flipping when moving from proportion overlap to absolute difference
    )
    return(list(shp, pal))
  })
  
  # * channel map  ----------------------------------------------------------------
  
  output$Map = renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE, attributionControl = FALSE)) %>%
      setView(lng = -121.75, lat = 38.05, zoom = 10) #%>%
  })
  
  observe({
    input$nav_tabs
    leafletProxy("Map", session) %>% 
      addProviderTiles(input$map_back)
  })
  
  observe({
    input$nav_tabs               # dependency on nav_tabs displays shapefile when tab is clicked (assuming next condition is met)
    if (is.null(rv$PO)){         # display default map until results are generated
      leafletProxy("Map", session) %>%
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
    delta.map = leafletProxy("Map", session) %>%
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
    delta.map = leafletProxy("Map", session) %>%
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
  
  observeEvent(input$Map_shape_click, {
    if (input$remove_channels == TRUE){
      p <- input$Map_shape_click
      if (is.null(rv$DC)){
        rv$DC = p$id
      }else{
        rv$DC = c(rv$DC, p$id)
      }
    }
  })
  
  # highlight selected channel; black chosen to highlight because other choices (e.g., yellow) might be used in map (i.e., hard to see yellow channel highlighted in yellow)
  ap_sel <- function(map, channel){
    addPolylines(map,
                 data = subset(shp, channel_nu == channel),
                 color = "black",
                 weight = 12,
                 opacity = 0.4,
                 layerId = "Selected")
  }
  
  # update the location selectInput on map clicks
  observeEvent(input$Map_shape_click, { 
    if (input$remove_channels == FALSE){
      p <- input$Map_shape_click
      if(!is.null(p$id)){
        if (p$id != "Selected"){  # 'Selected' layer placed on top of reach layer; don't want to update when 'Selected' layer clicked
          if(is.null(input$map_channel) || input$map_channel != p$id){
            updateSelectInput(session, "map_channel", selected = p$id)
          } 
        }else{
          # do nothing; selected channel dropdown should not change b/c want to have plot displayed in corner at all times
        }
      }
    }
  })
  
  # update the map markers and view on map clicks
  observeEvent(input$Map_shape_click, { 
    if (input$remove_channels == FALSE){
      p <- input$Map_shape_click
      proxy <- leafletProxy("Map")
      if(p$id == "Selected"){
        # do nothing
      } else {
        proxy %>% setView(lng = p$lng, lat = p$lat, input$Map_zoom) %>% ap_sel(p$id)
      }
    }
  })
  
  # update the map markers and view on location selectInput changes
  observeEvent(input$map_channel, { 
    if (input$remove_channels == FALSE){
      p <- input$Map_shape_click
      p2 <- filter(cll, channel_nu == input$map_channel)
      proxy <- leafletProxy("Map")
      if(nrow(p2)==0){
        proxy %>% removeShape(layerId = "Selected")
      } else if(length(p$id) && input$map_channel != p$id){  # length(p$id) used as conditional b/c only possible values are 0 or 1
        proxy %>% setView(lng = p2$lon, lat = p2$lat, input$Map_zoom) %>% ap_sel(input$map_channel) 
      } else if(!length(p$id)){
        proxy %>% setView(lng = p2$lon, lat = p2$lat, input$Map_zoom) %>% ap_sel(input$map_channel) 
      }
    }
  })
  
  # info alerts  ----------------------------------------------------------------
  observeEvent(input$proc_info, {
    sendSweetAlert(
      session = session,
      title = "",             # longer paragraphs have long lines b/c returns are interpreted as line breaks (which is useful)
      text = "Click browse to upload at least two H5 files of DSM2 Hydro output for comparison. Example H5 files are available to download.
      
      After uploading the files, the stage, flow, and channel area output are extracted from the H5 files and velocity is calculated by dividing flow by channel area. By default, the velocity time series plot is shown with the option to also show the flow and stage plots.
      
      A subset of the uploaded files can be selected for use in the time series plots and subsequent visualizations. However, it is recommended to only upload two files when trying new files because upload times can be slow.
      
      Selecting a file for use as a baseline scenario has no effect on the time series plots; it is only used in the visualizations shown after processing the DSM2 output.
      
      Click on 'Visualize DSM2 Output' to see the default channel map (i.e., before clicking on 'Process DSM2 Output' button).
      
      The date range is extracted from the earliest and latest date across all uploaded H5 files. Changing the date range allows for zooming in/out on the time series plots. 

      DSM2 requires a warm-up period to move away from the initial conditions. The length of this warm-up period is usually only a few days, but varies from channel to channel. It is most noticeable in channels at the edge of the system (e.g., 1, 410).
      
      After uploading/selecting files, setting the date range, and choosing the upstream or downstream node, click on 'Process DSM2 Output'. When processing is complete, the view will automatically switch to the 'Visualize DSM2 Output' tab.",
      type = "info",
      btn_labels = "OK"
    )
  })
  
  observeEvent(input$map_info, {
    if (is.null(rv$PO)){
      sendSweetAlert(
        session = session,
        title = "",             # longer paragraphs have long lines b/c returns are interpreted as line breaks (which is useful)
        text = "This is the default channel map. When DSM2 Hydro output files are uploaded on the 'Process DSM2 Output' tab, this map will be updated to display only channels found in all uploaded H5 files and the default channel map.
        
        Clicking on a channel pans to the selected channel and updates the 'Selected channel' input in the top left panel.",
        type = "info",
        btn_labels = "OK"
      )
    }else{
      sendSweetAlert(
        session = session,
        title = "",             # longer paragraphs have long lines b/c returns are interpreted as line breaks (which is useful)
        text = "Overlap is the proportion overlap in the density distributions for the selected comparison. Proportion overlap ranges from 0 to 1.
        
        Difference is the absolute difference in the selected summary statistic for the selected comparison. The absolute differences are rescaled across channels and scenarios to values ranging from 0 to 1; except for 'Reversal' which naturally ranges from 0 to 1.
        
        By default, map color is scaled from 0 to 1. To adjust the map color range, click on 'Show map tools' in the top left panel.
        
        When the 'Remove channels' switch is on (under 'Show map tools'), clicking on a channel removes that channel from the map.",
        type = "info",
        btn_labels = "OK"
      )
    }
    
  })
  
  observeEvent(input$plot_info, {
    sendSweetAlert(
      session = session,
      title = "",             # longer paragraphs have long lines b/c returns are interpreted as line breaks (which is useful)
      text = "The density plot shows the distribution of 15-min (or 1-hr) velocity/flow/stage values over the selected date range for the selected comparison. 
      
      The rescaled absolute difference in the selected summary statistic and proportion overlap of the distributions are shown in the top left and top right corners of the plot, respectively.
      
      The dashed vertical lines show the value of the selected summary statistic. Vertical lines are not displayed when 'Reversal' is selected as the summary statistic. 

      Reversal is the proportion of values that are negative. Reversal only applies to flow and velocity, not stage.
      
      The minimum and maximum summary statistics do not always align with the minimum and maximum extent of the distributions because the density estimates are based on a smoothing kernel.",
      type = "info",
      btn_labels = "OK"
    )
  })
  
  
  session$onSessionEnded(function() {
    h5closeAll()   # necessary?
  })
  
}


