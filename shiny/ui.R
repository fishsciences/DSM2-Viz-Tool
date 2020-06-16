navbarPage(
  "DSM2 HYDRO Visualization Tool",
  id = "nav_tabs",
  
  # Exploratory Analysis -----------------------------------------------------
  
  tabPanel(
    "Exploratory",
    useShinyjs(),
    useSweetAlert(),
    add_busy_bar(color = "#919191"),
    sidebarLayout(
      # * sidebar panel -----------------------------------------------------
      sidebarPanel(
        tags$style(HTML("hr {border-top: 1px solid #b5b5b5;}")),
        conditionalPanel(
          condition = 'input.explore_tabs == "Metadata"',
          p(align = "center", 
            hidden(
              actionButton(
                inputId = "reset_app",
                label = "Reset Application",
                icon = icon("redo"),
                onclick = "history.go(0)"
              )
            )
          ),
          fluidRow(
            br(),
            column(2,
                   dropdownButton(
                     actionButton(
                       inputId = "download_1hr",
                       label = "1-hour/70 MB",
                       icon = icon("download"),
                       onclick = "window.open('https://s3-us-west-2.amazonaws.com/datavore/delta-hydrodynamics/ExampleH5Files_1hour.zip', '_blank')"
                     ),
                     actionButton(
                       inputId = "download_15min",
                       label = "15-min/500 MB",
                       icon = icon("download"),
                       onclick = "window.open('https://s3-us-west-2.amazonaws.com/datavore/delta-hydrodynamics/ExampleH5Files_15min.zip', '_blank')"
                     ),
                     helpText("(time interval/file size)"),
                     circle = TRUE, status = "primary",
                     icon = icon("download"), size = "sm", 
                     tooltip = tooltipOptions(title = "Example HDF5 Files")
                   )
            ),
            column(10,
                   shinyFilesButton(
                     'h5_files',
                     label = "Select HDF5 Files", # button label
                     title = "Select HDF5 Files", # widget title
                     icon = icon("folder-open"),
                     multiple = TRUE
                   )
            )
          ),
          br(),    
          hidden(dateRangeInput('date_range_read', label = 'Date range')),
          hidden(radioGroupButtons(inputId = "node_loc", label = "Nodes",
                                   choices = c("Upstream", "Downstream"),
                                   selected = "Upstream", justified = TRUE)),
          hidden(helpText(id = "date_range_read_warn_small",
                          "Please select date range to include at least one scenario with >1 interval (see Table 2).")),
          br(),
          p(align = "center", hidden(actionButton(inputId = "read_data", label = "Read Data", icon = icon("spinner")))),
          hidden(helpText(id = "date_range_read_warn_large",
                          "Warning: Date range includes large number of intervals (see Table 2); reading data will be slow and may run out of memory. 
                          It is recommended to select less than one year of data because differences between scenarios are likely to be swamped by seasonal and annual variation."))
        ),
        conditionalPanel(
          condition = 'input.explore_tabs == "Time Series"',
          hidden(dateRangeInput('date_range_viz', label = 'Date range')),
          hidden(helpText(id = "date_range_viz_warn", "Please select larger date range.")),
          hidden(pickerInput(inputId = "sel_scenarios", label = "Selected scenarios", choices = NULL, multiple = TRUE)),
          hidden(helpText(id = "files_warn", "Please select more than one file for comparative analysis.")),
          hidden(helpText(id = "interval_warn", "Comparative analysis requires that all selected scenarios have the same time interval (see Table 1 on Metadata tab).")),
          hidden(pickerInput(inputId = "base_scenario", label = "Baseline scenario", choices = NULL)),
          br(),
          p(align = "center", hidden(actionButton(inputId = "run_comp", label = "Run Comparative Analysis", icon = icon("spinner"))))
        )
      ),
      # * main panel -----------------------------------------------------
      mainPanel(
        tabsetPanel(
          id = "explore_tabs",
          tabPanel("Metadata",
                   br(),
                   p(id = "metadata_msg", "Select HDF5 files of DSM2 HYDRO output to view metadata. Example HDF5 files are available to download by clicking on the blue download button."),
                   DTOutput("metadataTable"),
                   br(),
                   DTOutput("intervalTable"),
                   br(),
                   DTOutput("channelTable"),
                   br()
          ),
          tabPanel("Time Series",
                   br(),
                   p(id = "ts_msg", "Select and read HDF5 files on the Metadata tab to view time series plots of velocity, flow, and stage."),
                   fluidRow(
                     column(1),
                     column(5,
                            hidden(checkboxGroupButtons(inputId = "ts_plots", label = "Plots shown", choices = c("Velocity", "Flow", "Stage"),
                                                        selected = c("Velocity"),
                                                        checkIcon = list(yes = icon("ok", lib = "glyphicon"))))
                     ),
                     column(3,
                            hidden(pickerInput(inputId = "ts_channel", label = "Selected channel", width = '150px',
                                               choices = NULL, options = list(`live-search` = TRUE, size = 10)))
                     ),
                     column(3)
                   ),
                   # should have noted where I found this code; javascript to check if input$ts_plots is both not null and selected
                   conditionalPanel(condition = "input.ts_plots.indexOf('Velocity') > -1",  
                                    br(),
                                    plotOutput("tsVelocityPlot")),
                   conditionalPanel(condition = "input.ts_plots.indexOf('Flow') > -1",
                                    br(),
                                    plotOutput("tsFlowPlot")),
                   conditionalPanel(condition = "input.ts_plots.indexOf('Stage') > -1",
                                    br(),
                                    plotOutput("tsStagePlot"))
          )
        )
      )
    ),
    
    # * info button -----------------------------------------------------
    absolutePanel(
      top = 73,
      right = 40,
      style = "opacity: 0.95;",
      actionBttn(
        "explore_info",
        "",
        icon("info"),
        style = "material-circle",
        size = "sm"
      )
    )
  ),
  
  # Comparative Analysis -----------------------------------------------------
  
  tabPanel(
    "Comparative",
    tags$style(
      type = "text/css",
      "#Map {height: calc(100vh - 80px) !important;}",
      # fill map to height of container;  https://stackoverflow.com/questions/36469631/how-to-get-leaflet-for-r-use-100-of-shiny-dashboard-height/36471739#36471739
      ".leaflet-map-pane { z-index: auto; }" ,
      "#densityPlot.recalculating { opacity: 1}"   # eliminate blinking effect in densityPlot (which is a shiny feature, not a bug, but I think removing it is useful in this case)
    ),
    
    # map fills the screen and absolutePanels are placed on top
    leafletOutput('Map'),
    
    # * map tools  ----------------------------------------------------------------
    # top left
    absolutePanel(
      top = 80,
      left = 20,
      style = "opacity: 0.8;",
      wellPanel(
        materialSwitch(
          inputId = "map_tools",
          label = "Show map tools",
          right = TRUE,
          status = "primary"
        ),
        conditionalPanel(
          condition = 'input.map_tools',
          selectInput(
            inputId = "map_back",
            width = siw,
            label = "Background map",
            choices = c(
              "WorldGrayCanvas" = "Esri.WorldGrayCanvas",
              "WorldTopoMap" = "Esri.WorldTopoMap",
              "WorldImagery" = "Esri.WorldImagery"
            ),
            selected = "Esri.WorldGrayCanvas"
          ),
          selectInput(
            inputId = "map_pal",
            width = siw,
            label = "Map color palette",
            choices = c(
              "BrBG",
              "PiYG",
              "PRGn",
              "PuOr",
              "RdBu",
              "RdGy",
              "RdYlBu",
              "RdYlGn",
              "Spectral"
            ),
            selected = "RdYlGn"
          ),
          sliderInput(
            inputId = "color_range",
            label = "Map color range",
            min = 0,
            max = 1,
            value = c(0, 1),
            step = 0.01
          ),
          materialSwitch(
            "remove_channels",
            "Remove channels",
            right = TRUE,
            status = "primary"
          )
        ),
        hidden(
          pickerInput(
            inputId = "map_channel",
            width = siw,
            label = "Selected channel",
            choices = NULL,
            options = list(`live-search` = TRUE, size = 5)
          )
        )
      )
    ),
    
    # * main panel ----------------------------------------------------------------
    # top right
    absolutePanel(
      top = 80,
      right = 35,
      width = 235,
      style = "opacity: 0.8;",
      hidden(
        wellPanel(
          id = "mainPanel",
          radioGroupButtons(
            inputId = "metric",
            label = "Response metric",
            choices = c(
              "Velocity" = "velocity",
              "Flow" = "flow",
              "Stage" = "stage"
            ),
            selected = "velocity"
          ),
          selectInput(
            inputId = "summ_stat",
            label = "Summary statistic",
            choices = summ.stats,
            selected = "mean"
          ),
          hidden(helpText(id = "ss_help",  "Please select a summary statistic.")),
          radioGroupButtons(
            inputId = "type",
            label = "Map color variable",
            choices = c("Overlap" = "po", "Difference" = "ad"),
            selected = "po"
          ),
          selectInput(
            inputId = "comp_scenario",
            label = "Comparison scenario",
            choices = NULL
          )
        )
      )
    ),
    
    # * density plot ----------------------------------------------------------------
    absolutePanel(
      bottom = -105,
      left = 20,
      width = 500,
      style = "opacity: 0.95;",
      # trial-and-error to select value for bottom; not sure how that works
      plotOutput("densityPlot")
    ),
    
    # * scale axes
    absolutePanel(
      bottom = 260,
      left = 280,
      style = "opacity: 0.95;",
      hidden(
        checkboxInput("scale_axes", "Scale axes across comparisons", TRUE)
      )
    ),
    
    # * info buttons ----------------------------------------------------------------
    
    absolutePanel(
      top = 85,
      right = 40,
      style = "opacity: 0.95;",
      actionBttn(
        "map_info",
        "",
        icon("info"),
        style = "material-circle",
        size = "sm"
      )
    ),
    absolutePanel(
      bottom = 20,
      left = 480,
      style = "opacity: 0.95;",
      hidden(
        actionBttn(
          "plot_info",
          "",
          icon("info"),
          style = "material-circle",
          size = "sm"
        )
      )
    )
  ),
  # About -----------------------------------------------------
  tabPanel("About", includeMarkdown("About.md"))
  
)
