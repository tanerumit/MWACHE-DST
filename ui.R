#-------------------------------------------------------------------------------
# Mwache DST - User inteface (UI) script
#-------------------------------------------------------------------------------

#Global script. non-essentials & one-time runs goes here..
source("global.R", local = TRUE)

#### Define side-bar
sidebar <- dashboardSidebar(width = 200, hr(), {
  sidebarMenu(id = "tabs", 
    menuItem("About", tabName = "tab_about", icon = icon("info")),
    menuItem("Vulnerabilty", icon = icon("circle-o"),
      menuSubItem("Climate only", "tab_surface", icon = icon("area-chart")),
      menuSubItem("Multivariate", "tab_parcoord", icon = icon("area-chart"))
    ),
    #menuItem(text="Risks", tabName="tab_risk", icon=icon("circle-o")),
    menuItem("Adaptation",  tabName = "tab_sim", icon = icon("circle-o")),
    tags$style(type = 'text/css', 
      "footer{position: absolute; bottom:2%; left: 5%; padding:5px;}"),
    HTML('<footer> &copy; 2016 HRG </footer>')
  )
})

#### Define body 
body <- dashboardBody(
  useShinyjs(), 
  tags$head(tags$link(rel="stylesheet", type="text/css", href="custom.css")),
  tabItems(
    
  #About TAB +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  tabItem(tabName="tab_about", {
    fluidPage(
      tags$div(style="margin-left:15px;",
        h3(strong("Mwache Dam Interactive Decision Analysis Tool"))
      ),
      column(8, 
        p("The Mwache Dam is one of the flagship water resources development 
          projects in the Coastal Province of Kenya. The new development aims 
          to provide about a total of 80.3 MCM/year (220,000 m3/day) for 
          domestic consumption and irrigation use."),
        p("This work couples the 'decision tree framework' concepts with
          interactive visualization tools to aid the decision making process of 
          the Mwache Dam project"),
        br(),
        h4("Interactive decision-analysis tools (select from the side-bar):"),
        p(strong(em("Vulnerability:")), "Display system performances across the 
          climate domain (climate response maps), and across 
          the multivariate parameter space (parallel coordinates plot)"),
        p(strong(em("Adaptation:")), "Simulate reservoir performance under 
          selected climatic and demographic inputs"),
        #p(strong(em("Risks:")), "Posterior likelihood analysis (in progress)"),
        br(),
        br(),
        h5("For questions, contact to:"),
        h5("M.Umit Taner,", a("tanerumit@gmail.com", href="tanerumit@gmail.com")),
        h5(a("Hydrosystems Research Group", 
          href="https://blogs.umass.edu/hydrosystems/"))
      ),
      column(3, offset = 1,
        img(src="study_site.png", height = "400px", width = "200px")
      )
    )
  }),
    
  #Response surface ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  tabItem(tabName="tab_surface", {
    fluidPage(
      box(title = strong("Settings"), width=4, status="primary",
        fluidRow(tags$div(style="margin-left:15px;",
          bsButton(inputId="SurfaceReset", label="Defaults", 
            icon = icon(name="refresh")))
        ), #fluidrow close
        br(),
        div(id = "SurfaceInputs", 
          wellPanel(
            em(strong(helpText(span("Performance assessment", 
              style="color:blue")))),
            radioButtons(inputId="metric", label="Metric", 
              selected="reliability", inline=T, 
              choices = c("safeyield", "reliability")),
            bsTooltip("metric", 
              "safeyield in MCM, reliability as %",
              placement = "right", options = list(container = "body")),
            radioButtons(inputId="units", label="Evaluation type", 
              selected="disabled", inline=T,
              choices = c("Continuous"="disabled", "binary"="enabled")),
            bsTooltip("units", "Reporting method of the metric",
              placement = "right", options = list(container = "body")),
            sliderInput(inputId = "threshold", label = "Threshold", 
              min = 80, max = 100, step = 1, value = 90, ticks = F),
            bsTooltip("threshold", "performance threshold (for binary evaluation)",
              placement = "right", options = list(container = "body"))
          ), # welpanel close
          wellPanel(
            em(strong(helpText(span("Natural climate variability", 
              style="color:blue")))),
            radioButtons(inputId="dataset", label="Underlying data", 
                choices = c("Princeton","GPCC","CRU"), selected="Princeton",
              inline = T),
            bsTooltip("dataset", 
              "climate data used to generate stochastic realizations",
              placement = "right", options = list(container = "body")),
            h5(strong("Climate realization")),
            checkboxInput("meanTrace", "Mean realization", value = T),
              condition="input.meanTrace == 0",
            bsTooltip("meanTrace", 
              "show results for the mean of all traces or for an individual trace",
              placement = "right", options = list(container = "body")),
            sliderInput("nvar", label = NULL, 
              min=1, max=10, step=1, value=1, ticks=F)
          ), # wellpanel close
          wellPanel(
            em(strong(helpText(span("Non-climatic attributes", 
              style = "color:blue")))),
            
            #conditionalPanel(
            #  condition="input.metric == 'reliability'",
              sliderInput("demand", "Demand increase", 
                  min=50, max=200, step=30, value=80, post="%", ticks = F),
              bsTooltip("demand", 
                "Increase in demand relative to the 2015 level (38 MCM/year)",
                placement = "right", options = list(container = "body")),
            sliderInput("size", "Reservoir size", 
              min=80, max=140, step=10, value=120, post="MCM", ticks = F),
            bsTooltip("size", 
              "Reservoir storage capacity",
              placement = "right", options = list(container = "body"))
          ) # wellpanel close
          ) # div close
        ), #box close
        box(title = strong("Climate response surface"), width=8, 
          align = "left", status="primary",
          plotOutput("SurfacePlot", height = "500px", width = "600px"),
          br(),
          checkboxGroupInput("climInfo", label="Display climate information",
            inline=F,
            c("Historical climate (1960-2000)"=1,
              "CMIP5 projections"=2,
              "CMIP5 confidence level (95%)"=3,
              "CMIP5 confidence level (99%)"=4)),
          bsTooltip("climInfo", 
            "Information to evaluate the subjective likelihood of climate changes",
            placement = "bottom", options = list(container = "body"))
        ) #box close
      ) #fluidpage close
    }), #tabitem close
    
  #Parallel coord plot +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  tabItem(tabName = "tab_parcoord", {
      fluidPage(
        box(title = strong("Settings"), width = 3, solidHeader=F, 
          status="primary",
          wellPanel(
            em(strong(helpText(span("Design features", style = "color:blue")))),
            sliderInput("ParcoordSizeSelect", "Reservoir size", 
              min=80, max=140, step=20, value=120, post="MCM", ticks=F)
          ),
          wellPanel(
            em(strong(helpText(span("Scenario definition", 
              style = "color:blue")))),
            numericInput(inputId = "ParCoordThold", label = "NPV Threshold",
              min = 500, max = 2000, value = 1000),
            selectInput(inputId = "ParCoordTholdType", label = "Threshold type",
              choices=c(">=","<="), selected = ">=", selectize = F)
          ) #wellPanel close
        ), #box close
        box(title = strong("Vulnerability domain"), width = 9, align = "left",
          solidHeader = F, status = "primary",
          parcoordsOutput("ParcoordPlot"),
          valueBoxOutput("range", width=4),
          valueBoxOutput("coverage", width=4),
          valueBoxOutput("density", width=4),
          br(),
          br(),
          #Tooltips
          bsTooltip("coverage", 
            "the percentage of satisfying runs that are described by the selection",
            placement = "bottom", options = list(container = "body")),
          bsTooltip("density", 
            "the percentage of satisfying runs within the selection",
            placement = "bottom", options = list(container = "body")),
          bsTooltip("range", 
            "relative size of the selection",
            placement = "bottom", options = list(container = "body"))
        ) #box close
      ) #fluidpage close
    }),
    
  #Simulation ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  tabItem(tabName = "tab_sim", {
    fluidPage(
      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
        tags$div("Loading...",id="loadmessage")
      ),
      box(width = 4, title = strong("Settings"), status = "primary",
      div(id = "tabSimInputs", status = "primary",
        fluidRow(
          tags$div(style="margin-left:15px;",
            div(style="display:inline-block",
              bsButton(inputId="SimINFO", label="Info", 
                icon=icon(name="info-circle"), size='default')),
            div(style="display:inline-block",
              bsButton(inputId="SimGO", label="Start", 
                icon=icon(name="cog"), size='default')),
            div(style="display:inline-block",
              bsButton(inputId="SimReset", label="Defaults", 
                icon =icon(name="refresh"), size='default'))
          )
        ), # fluidrow close  
        br(),
        wellPanel(
          em(strong(helpText(span("Reservoir capacity", 
            style = "color:blue")))),
          sliderInput(inputId = "Res_size", label = NULL,
            ticks=F, min=40, max=140, step= 5, value=120, post="MCM")
        ),
        wellPanel(
          em(strong(helpText(span("Climate conditions", 
            style = "color:blue")))),
          sliderInput(inputId="input_nvar", "Climate realization",
              min=1, max=10, step=1, value=1, ticks = F),
          sliderInput(inputId="input_temp", "Temperature increase",
              min=0, max=5, step=0.2, value=0, post=HTML("&deg;C"), ticks=F),
          sliderInput(inputId="input_prec", "Precipitation change",
              min=70, max=150, step=5, value=100, post ="%", ticks=F)
        ),
        wellPanel(
          em(strong(helpText(span("Demand levels & priority", 
            style = "color:blue")))),
          fluidRow(
            column(9,uiOutput("SimDemandECO")),
            column(3, uiOutput("SimEcoPrio"))
          ),
          fluidRow(
            column(9,uiOutput("SimDemandDOM")),
            column(3, uiOutput("SimDomPrio"))
          ),
          fluidRow(
            column(9,uiOutput("SimDemandIRR")),
            column(3, uiOutput("SimIrrPrio"))
          ),
          numericInput(inputId = "AllocCoef", "Allocation coefficient",
            min=0, max=1, step=0.1, value=0.8)
        ),
        wellPanel(
          em(strong(helpText(span("Operating policy (advanced)", 
            style = "color:blue")))),
          sliderInput(inputId = "ResBuffer", "Top of buffer pool",
            min=0, max =60, step=5, value=25, post="%", ticks=F),
          numericInput(inputId = "BufferCoef", "Buffer coefficient",
            min=0, max=1, step=0.1, value=0.8)
        ),
        #Tooltips for all inputs 
        bsTooltip("SimDemandECO", "annual environmental demand level",
            placement = "bottom", options = list(container = "body")),
        bsTooltip("input_nvar", 
            "Stochastic realization of the historical climate (1951 - 2000)",
            placement = "bottom", options = list(container = "body")),
        bsTooltip("input_temp", "Long-term temperature change over the historical mean",
            placement = "bottom", options = list(container = "body")),
        bsTooltip("input_prec", "Long-term precipitation change over the historical mean",
            placement = "bottom", options = list(container = "body")),
        bsTooltip("Res_size", "Maximum storage capacity of the reservoir",
            placement = "bottom", options = list(container = "body")),
        bsTooltip("ResBuffer", "Maximum volume of buffer pool",
            placement = "bottom", options = list(container = "body")),
        bsTooltip("ResCons", "Maximum volume of conservation pool",
            placement = "bottom", options = list(container = "body")),
        bsTooltip("BufferCoef",
            "Buffer coefficient to constraint releases from buffer pool",
            placement = "bottom", options = list(container = "body"))
      ) # div close
      ), #box close,
      box(title = strong("Simulation results"), width = 8, solidHeader = F, 
        status = "primary",
        bsButton("MB_table", "View mass balance table"),
        br(),
        br(),
        uiOutput("SimPlotSelect"),
        plotlyOutput("SimPlot", height = "600px"),
        br(),
        uiOutput("SimPerformanceSelect"),
        plotlyOutput("SimPerformance", width = "80%"),
        bsModal(id="MBTable", title="Mass balance", trigger="MB_table",
            DT::dataTableOutput('SimTable'), size = "large"),
        bsModal(id="ModalInfo", title="Water Resources simulation model", 
            trigger="SimINFO", includeHTML("www/model_description.html"),
            size = "large")
      ) # box close
    ) # fluidpage close
  }),
    
  #Risk ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  tabItem(tabName = "tab_risk", {
      fluidPage(
      tabBox(id = "BNDescription", width = 12,
        tabPanel(title = "Bayesian network",
          h5("Bayesian Belief Network for the Mwache system"),
          br()), 
        tabPanel(title="Likelihood information", open="Economic development",
        fluidPage(
          bsCollapse(
            bsCollapsePanel(
              title = "Economic development",
              column(4,
              h5("Subjective likelihood values assigned to low, 
                medium, and high economic development"
              ),
              br(),
              uiOutput("PrDevLow"),
              uiOutput("PrDevMed"),
              uiOutput("PrDevHigh")
              ),
              column(7, offset = 1,
                align = "center",
                plotlyOutput("DevInputPlot", width="80%", height="300px")
              )
            ),
            bsCollapsePanel(
              title = "Future population",
              column(4,
                h5("PDF of the target population for Mwache system in 2034"),
                br(),
                column(6,
                  numericInput("PopMin", "Minimum",
                    value=1.6, min= 0.9, max=2, step=0.1
                  ),
                  numericInput("PopMax", "Maximum",
                    value=2.6, min=2, max=3, step=0.1
                  )
                ),
                column(6,
                  numericInput("PopMean","Mean",
                    value =2.1, min=1.7, max=2.5, step=0.1
                  ),
                  numericInput("PopSD", "S.deviation", value = 0.2,
                    min=0.1, max=0.5, step=0.1
                  )
                )
              ),
              column(
                7,
                offset = 1,
                align = "center",
                plotlyOutput("PopInputPlot", width = "80%", height = "300px")
              )
            ),
            bsCollapsePanel(
              title = "Climate change information",
              column(
                4,
                h5("Mean climate change in 2065-2070 relative to the 
                  1960 - 2000 mean"),
                br(),
                selectInput(
                  "GCMInput",
                  "Climate change information",
                  choices = c("RCP 4.5", "RCP 8.5", "ALL"),
                  selected = "RCP 8.5"
                ),
                selectInput("GCMDist", "Probability distribution",
                  choices = c("Bivariate normal", "Bivariate cauchy",
                    "Kernel density (non-parametric)"
                  ),
                  selected = "Bivariate normal"
                )
              ),
              column(
                7,
                offset = 1,
                align = "center"
                ,
                plotlyOutput("GCMInputPLot", height = "400px", width = "80%")
              )
            ),
            bsCollapsePanel(
              title = "Per capita demand",
              column(4,
                h5("Enter the expected valuate per capita demand (l/p/d)
                  conditional on the level of economic development"),
                uiOutput("PCDMean1"),
                uiOutput("PCDMean2"),
                uiOutput("PCDMean3")
              ),
              column(7,offset = 1, align = "center" ,
                plotlyOutput("PCDInputPlot", height = "300px", width = "80%"))
            ),
            bsCollapsePanel(title = "Sediment flux",
              column(4,
                h5("placeholder")),
              column(7, offset = 1, align = "center")),
            bsCollapsePanel(title = "Price",
              column(4,
                h5("placeholder")),
              column(7, offset = 1, align = "center")),
            bsCollapsePanel(title = "Discount rate",
              column(4,
                h5("placeholder")),
              column(7, offset = 1, align = "center"))
          )
        )), #tabpanel end
        tabPanel(title = "Marginal probs.",
          br(),
          helpText("inputs here"),
          br()),
        tabPanel(title = "Joint prob. & risk metrics",
          br(),
          helpText("this is risk"),
          br())
      ))
    })
  ) #tabitems close
) #dashboard close

####  Define the dashboard 
dashboardPage(
  skin    = "black",
  header  = dashboardHeader(titleWidth = 157, title = "Dashboard"),
  sidebar = sidebar,
  body    = body
  )


#  h5("Parallel coordinates graph: Each point in the", em("n"), "-dimensional 
#  space is represented as a polyline with vertices on the parallel axes; 
#  the position of the vertex on the", em("i"), "-th axis corresponds to 
#  the i-th coordinate of the point.")

