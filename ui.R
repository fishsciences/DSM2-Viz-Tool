
navbarPage("DSM2 Hydro Visualization Tool",
           id = "nav_tabs",
           tabPanel("Process DSM2 Output",
                    useShinyjs(),
                    useSweetAlert(),
                    tags$head(
                      # Facebook/LinkedIn OpenGraph tags
                      tags$meta(property = "og:title", content = "DSM2 Hydro Visualization Tool"),
                      tags$meta(property = "og:type", content = "website"),
                      tags$meta(property = "og:url", content = "https://fishsciences.shinyapps.io/dsm2-hydro-visualization/"),
                      tags$meta(property = "og:image", content = "AppImage.png"),
                      tags$meta(property = "og:description", content = "An interactive app for visualizing DSM2 Hydro output.")
                    ),
                    sidebarLayout(
                      sidebarPanel(
                        fileInput("h5_files", "Upload H5 files", multiple = TRUE, # multiple upload does not work on older browsers including RSTudio's default viewer
                                  accept = c("application/x-hdf5",".h5")),
                        hidden(h4(align = "center", style = "color:red",  id = "upload_warn", "Please upload more than one file.")),
                        strong(p(align = "center", "Example H5 files (time interval/zip file size)")),
                        fluidRow(
                          column(6,
                                 p(align = "center", actionButton(inputId = "download_1hr", label = "1-hour/70 MB", icon = icon("download"),
                                                                  onclick = "window.open('https://s3-us-west-2.amazonaws.com/datavore/delta-hydrodynamics/ExampleH5Files_1hour.zip', '_blank')"))
                          ),
                          column(6,
                                 p(align = "center", actionButton(inputId = "download_15min", label = "15-min/500 MB", icon = icon("download"),
                                                                  onclick = "window.open('https://s3-us-west-2.amazonaws.com/datavore/delta-hydrodynamics/ExampleH5Files_15min.zip', '_blank')"))
                          )
                        ),
                        
                        
                        br(),
                        hidden(pickerInput(inputId = "sel_scenarios", label = "Select files", choices = NULL, multiple = TRUE)),
                        hidden(pickerInput(inputId = "base_scenario", label = "Select file for baseline scenario", choices = NULL)),
                        hidden(dateRangeInput('date_range', label = 'Date range')),
                        hidden(radioGroupButtons(inputId = "nodes", label = "Nodes",
                                                 choices = c("Upstream", "Downstream"),
                                                 selected = "Upstream", justified = TRUE)),
                        br(),
                        p(align = "center", hidden(actionButton(inputId = "proc_dsm2", label = "Process DSM2 Output", icon = icon("paper-plane")))),
                        hidden(h4(align = "center", style = "color:red",  id = "files_warn", "Please select more than one file."))
                      ),
                      mainPanel(
                        fluidRow(
                          column(1),
                          column(5, 
                                 hidden(checkboxGroupButtons(inputId = "ts_plots", label = "Plots shown", choices = c("Velocity", "Flow", "Area"), 
                                                             selected = c("Velocity", "Flow"),
                                                             checkIcon = list(yes = icon("ok", lib = "glyphicon"))))
                          ),
                          column(3, 
                                 hidden(pickerInput(inputId = "ts_channel", label = "Selected channel", width = '150px',
                                                    choices = NULL, options = list(`live-search` = TRUE, size = 10)))
                          ),
                          column(3)
                        ),
                        conditionalPanel(condition = "input.ts_plots.indexOf('Velocity') > -1",
                                         plotOutput("tsVelocityPlot")),
                        br(),
                        br(),
                        conditionalPanel(condition = "input.ts_plots.indexOf('Flow') > -1",
                                         plotOutput("tsFlowPlot")),
                        br(),
                        br(),
                        conditionalPanel(condition = "input.ts_plots.indexOf('Area') > -1",
                                         plotOutput("tsAreaPlot"))
                      )
                    ),
                    # info button placed with absolutePanel() ----------------------------------------------------------------
                    
                    absolutePanel(top = 90, right = 30, style = "opacity: 0.95;",
                                  actionBttn("proc_info", "", icon("info"), style = "material-circle", size = "md")
                    )
           ),
           tabPanel("Visualize DSM2 Output",
                    tags$style(type = "text/css",
                               "#Map {height: calc(100vh - 80px) !important;}",    # fill map to height of container;  https://stackoverflow.com/questions/36469631/how-to-get-leaflet-for-r-use-100-of-shiny-dashboard-height/36471739#36471739
                               ".leaflet-map-pane { z-index: auto; }" ,
                               "#densityPlot.recalculating { opacity: 1}"   # eliminate blinking effect in densityPlot (which is a shiny feature, not a bug, but I think removing it is useful in this case)
                    ),
                    
                    # map fills the screen and absolutePanels are placed on top
                    # absolutePanel placement chosen by trial-and-error
                    leafletOutput('Map'),
                    
                    # channel panel ----------------------------------------------------------------
                    
                    absolutePanel(top = 80, left = 20, style = "opacity: 0.8;",
                                  wellPanel(
                                    materialSwitch(inputId = "map_tools", label = "Show map tools", right = TRUE, status = "primary"),
                                    conditionalPanel(condition = 'input.map_tools',
                                                     selectInput(inputId = "map_back", width = siw,
                                                                 label = "Background map",
                                                                 choices = c("BlackAndWhite" = "OpenStreetMap.BlackAndWhite",
                                                                             "WorldTopoMap" = "Esri.WorldTopoMap",
                                                                             "WorldImagery" = "Esri.WorldImagery"),
                                                                 selected = "OpenStreetMap.BlackAndWhite"),
                                                     selectInput(inputId = "map_pal", width = siw,
                                                                 label = "Map color palette",
                                                                 choices = c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral"),
                                                                 selected = "RdYlGn"),
                                                     conditionalPanel(condition = 'input.type == "po"',
                                                                      sliderInput(inputId = "po_range", label = "Map color range", min = 0, max = 1, value = c(0, 1), step = 0.01)),
                                                     conditionalPanel(condition = 'input.type == "ad"',
                                                                      sliderInput(inputId = "ad_range", label = "Map color range", min = 0, max = 3, value = c(0, 3), step = 0.1)), 
                                                     materialSwitch("remove_channels", "Remove channels", right = TRUE, status = "primary")
                                    ),
                                    pickerInput(inputId = "map_channel", width = siw, label = "Selected channel",
                                                choices = NULL, options = list(`live-search` = TRUE, size = 5))
                                  )
                    ),
                    
                    # main panel ----------------------------------------------------------------
                    # top right
                    # heavy use of conditional panels to display the correct inputs given choices for other inputs
                    absolutePanel(top = 80, right = 20, width = 220, style = "opacity: 0.8;",
                                  hidden(
                                    wellPanel(
                                      id = "mainPanel",
                                      radioGroupButtons(inputId = "metric", label = "Response metric",
                                                        choices = c("Velocity" = "velocity", "Flow" = "flow"), selected = "velocity"),
                                      selectInput(inputId = "summ_stat", label = "Summary statistic",
                                                  choices = ss.levels, selected = "mean"),
                                      radioGroupButtons(inputId = "type", label = "Map color variable",
                                                        choices = c("Overlap" = "po", "Difference" = "ad"), selected = "po"),
                                      selectInput(inputId = "comp_scenario", label = "Comparison scenario", choices = NULL)
                                    )
                                  )
                    ),
                    
                    # density plot ----------------------------------------------------------------
                    absolutePanel(bottom = -225, height = 542, left = 20, width = 300, style = "opacity: 0.95;", 
                                  plotOutput("densityPlot")
                    ),
                    
                    # small panel that is overlain on plotting panel
                    absolutePanel(bottom = 280, left = 330, style = "opacity: 0.95;",
                                  hidden(checkboxInput("scale_axes", "Scale axes across comparisons", TRUE))
                    ),
                    
                    # info button placed with absolutePanel() ----------------------------------------------------------------
                    
                    absolutePanel(top = 90, right = 30, style = "opacity: 0.95;",
                                  actionBttn("map_info", "", icon("info"), style = "material-circle", size = "md")
                    ),
                    absolutePanel(bottom = 30, left = 502, style = "opacity: 0.95;",
                                  hidden(actionBttn("plot_info", "", icon("info"), style = "material-circle", size = "md"))
                    )
           ),
           tabPanel("About", includeMarkdown("About.md"))
           
)


