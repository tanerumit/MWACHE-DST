#------------------------------------------------------------------------------#
# Mwache DST - Server-side script
#------------------------------------------------------------------------------#

#Global script. non-essentials & one-time runs goes here..
source("global.R", local = TRUE)

shinyServer(function(input, output, session) {

# RESPONSE SURFACES ------------------------------------------------------------
  
  #### SURFACEPLOT DATA ++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  surfaceData <- reactive({
    
    var <- input$metric
    
    dat <- surface_data %>% rename_(value = var) %>%
      filter(dataset==input$dataset & size==input$size & demand==input$demand)
    
    #Plot for the mean trace
    if(input$meanTrace == 1) {
      dat %<>% filter(nvar == 0)} else {dat %<>% filter(nvar == input$nvar)}
    
    #Threshold based evaluation
    if(input$units == "enabled") {
      var_col <- c("royalblue4", "firebrick2")
      threshold <- ifelse(is.null(input$threshold),90,input$threshold)
      dat <- mutate(dat, 
                    Bins=ifelse(value>=threshold, "Acceptable", "Not acceptable"),
                    Bins=factor(Bins, levels = c("Acceptable", "Not acceptable"),
                                labels = c("Acceptable", "Not acceptable")))
      
      #In absolute units  
    } else {
      if (var == "reliability") {
        
        bin1 <- c(seq(40,90,10),95)
        bin2 <- seq(96,100,1)
        col1 <- colorRampPalette(c("firebrick2", "white"))(length(bin1))
        col2 <- colorRampPalette(c("lightsteelblue1", "royalblue4"))(length(bin2))
        var_col <- c(col1,col2)
        var_bin <- c(bin1, bin2)  
        
      } else {
        
        bin1 <- seq(30,80,10)
        bin2 <- seq(90,130,10)
        col1 <- colorRampPalette(c("firebrick2", "white"))(length(bin1))
        col2 <- colorRampPalette(c("lightsteelblue1", "royalblue4"))(length(bin2))
        var_col <- c(col1,col2)
        var_bin <- c(bin1, bin2) 
        
      }
      
      dat <- mutate(dat, Bins = cut(value,
        breaks = var_bin, dig.lab = 5, include.lowest = T))
    }
    
    list(dat = dat, var_col = var_col)
  }) 
  
  SurfacePlot <- reactive({
    
    df   <- surfaceData()$dat
    varcols <- surfaceData()$var_col
    
    p1 <- ggplot(data = df, aes(x = temp, y = prec)) +
      geom_tile(aes(fill = Bins), color = "gray60") +
      scale_x_continuous(expand=c(0,0), breaks = tick$x) +
      scale_y_continuous(expand=c(0,0), breaks = tick$y,
                         labels = seq(-30, +50, 10)) +
      scale_fill_manual(name = "Range", values = varcols, drop = FALSE)  +
      guides(fill = guide_legend(order = 1, keyheight = 1.5, keywidth = 1.5),
             shape = guide_legend(order = 2, keyheight = 1.5, keywidth = 1.5)) +
      labs(x = label$x, y = label$y)
    
    if(1 %in% input$climInfo) {
      p1 <- p1 + geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
        geom_hline(yintercept = 1, linetype = "dashed", size = 1)
    }
    if(2 %in% input$climInfo) {
      p1 <- p1 + geom_point(aes(x = del_temp, y = del_prec, shape = Scenario),
          size = 2, data = CMIP5, stroke = 1.5) +
        scale_shape_manual(name = "Scenarios", values =c(21,22,23,24))
    }
    if(3 %in% input$climInfo) {
      p1 <- p1 + stat_ellipse(aes(x = del_temp, y = del_prec), data = CMIP5,
        size = 1, linetype = "dotdash", level = 0.95)
    }
    if(4 %in% input$climInfo) {
      p1 <- p1 + stat_ellipse(aes(x = del_temp, y = del_prec), data = CMIP5,
        size = 1, linetype = "dotdash", level = 0.99)
    }

    p1
  })
  #output$SurfacePlot <- renderPlotly({SurfacePlot()})
  output$SurfacePlot <- renderPlot({SurfacePlot()})
  
  #### DYNAMIC METRIC SELECTION BUTTONS +++++++++++++++++++++++++++++++++++++++++
  observeEvent(input$SurfaceReset, {shinyjs::reset("SurfaceInputs")})
  
  observe({
    if (input$metric == "reliability") {
      shinyjs::enable("demand") 
      if (input$units=='enabled') {
        updateSliderInput(session, inputId = "threshold", label = "Threshold",
          min = 80, max = 100, step = 1, value = 90)}
    } else if (input$metric == "safeyield") {
      shinyjs::disable("demand")   
      if (input$units=="disabled") {
        updateSliderInput(session, inputId = "threshold", label = "Threshold",
          min = 40, max = 140, step = 5, value = 1)}
      }
  })

  observe({
    if (input$units=='enabled')
      shinyjs::enable("threshold")
    else 
      shinyjs::disable("threshold")
  })
  
  observe({
    if (input$meanTrace == 0) 
      shinyjs::enable("nvar") 
    else 
      shinyjs::disable("nvar")        
  })

# MULTIVARIATE ANALYSIS --------------------------------------------------------
  parcoord_data <- reactive({
    parc_data %>% 
      filter(size == as.numeric(input$ParcoordSizeSelect)) %>%
      select("realization" = var, 
             "delta Temp" = dtemp,
             "delta Prec" = dprec, 
             "price" = prc, 
             "demand" = dem,
             "sediment" = sed,
             "disc rate" = dsc,
             "NPV" = NPV) 
  })
  
  ####### PARALEL COORDINATES PLOT 
  output$ParcoordPlot <- renderParcoords({

    parcoords(
      data = parcoord_data(), 
      rownames = F,
      brushMode = "1D-axes", 
      brushPredicate = "and",
      reorderable = T, 
      axisDots = NULL,
      composite = NULL,
      margin = list(top = 20, left = 0, bottom = 50, right = 0),
      alpha = 0.60, 
      queue = T, 
      rate = 200,
      color = "steelblue",
      tasks = htmlwidgets::JS("function foo(){this.parcoords.alphaOnBrushed(0.15);}") 
    )
  })
  
  ####### VALUE BOXES 
  MvarInfo <- reactive({
    #browser()
    df <- parcoord_data()
    rows <- if(length(input$ParcoordPlot_brushed_row_names) > 0) {
      as.numeric(input$ParcoordPlot_brushed_row_names)
    } else {as.numeric(rownames(df))}
    
    values <- as.numeric(df[['NPV']])
    
    val_interest <- switch(input$ParCoordTholdType,
                           ">=" = {which(values >= input$ParCoordThold)},
                           "<=" = {which(values <= input$ParCoordThold)})
    
    val_interest_select <- intersect(val_interest, rows)   
    
    cov  <- round(length(val_interest_select) / length(val_interest) * 100)
    den  <- round(length(val_interest_select) / length(rows) * 100)
    size <- round(length(rows)/nrow(df) * 100)
    
    list(range = valueBox(paste0(size,"%"), "selection range", color="blue"),
    coverage = valueBox(paste0(cov,"%"), "scenario coverage", color="olive"),
    density  = valueBox(paste0(den,"%"), "scenario density", color="maroon"))
    
  })

  output$range    <- renderValueBox({MvarInfo()[['range']]})
  output$coverage <- renderValueBox({MvarInfo()[['coverage']]})
  output$density  <- renderValueBox({MvarInfo()[['density']]})

# SIMULATION -------------------------------------------------------------------

   # input <- list()
   # input$input_temp <- 0
   # input$input_prec <- 100
   # input$input_nvar <- 1
   # input$Res_size <- 120
   # input$ResBuffer  <- 30
   # input$BufferCoef <- 1
   # input$demand_dom <- 60
   # input$demand_irr <- 30
   # input$demand_eco <- 10
   # input$AllocCoef <- 0.8
 
  # SIMULATION MODEL +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  sim <- eventReactive(input$SimGO, {
    
    ResBuff <- isolate({input$ResBuffer})/100
    ResCons <- 1 - ResBuff
    
    # Climate forcing & inflow
    Date <- as.Date("2020-01-01") + months(0:599)
    
    clim <- realizations %>% filter(nvar == input$input_nvar) %>%
      mutate(
        Prec = Prec * rep(seq(1, input$input_prec/100, length.out=50), each=12),
        Tavg = Tavg + rep(seq(0, input$input_temp, length.out=50), each=12),
        Tmin = Tmin + rep(seq(0, input$input_temp, length.out=50), each=12),
        Tmax = Tmax + rep(seq(0, input$input_temp, length.out=50), each=12),
        PE = HARGREAVES(Date, Tavg, Tmax - Tmin, lat=3.5),
        Q = ABCD_QEST(abcd_calib, Prec, PE, S_ini=100, G_ini=2) %>%
            as.numeric() * area * 1e-3)

    # #### CALCULATE IRRIGATION DEMAND ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #Calculate ET0 (Penmann - Monteith equation (FAO))
    irrig_cal <- clim %>%
      left_join(crop_pars_et0, by = "Month") %>%
      mutate(
        EPrec = ifelse(Prec > 75, Prec * 0.8 -25, pmax(0, Prec * 0.6 - 10)),
        Wind  = Wind * 0.01157407,
        v_sat = 0.6108 * 2.7183^(17.27*Tavg/(Tavg + 273.3)),
        vap_p = Humid/100 * v_sat,
        ET0 = et0(Tmax=Tmax, Tmin=Tmin, vap_pres=vap_p, sol_rad = Solar,
                  tal = cst(Solar, Date, radians(Latitude)),z = 150, uz = Wind,
                  days  = Date, lat = Latitude) * as.numeric(days_in_month(Month))) %>%
      select(Year, Month, Prec, Tavg, PE, EPrec, ET0)
    
    #Calculate CRW (mm/month)
    irr_demand <- irrig_cal %>%
      left_join(crop_Kc, by = "Month") %>%
      left_join(crop_area, by = c("Season","Crop")) %>%
      mutate(
        Area_ha = ifelse((Year > 2033) & Crop == "Mango", 0, Area_ha)) %>%
      mutate(
        ETc = ET0 * Kc,
        NIR_mm = pmax(ETc - EPrec, 0),
        GIR_mm = NIR_mm / (Eff/100),
        GIR_mcm = GIR_mm * Area_ha / 10^5) %>%
      mutate(Month = as.integer(Month)) %>%
      group_by(Year, Month) %>%
      summarize(Irr_mcm = sum(GIR_mcm)) %$% Irr_mcm
    
    #simulate reservoir operations
    reservoir_sim(
      beg.y     = clim$Year[1],
      beg.m     = clim$Month[1],
      K         = input$Res_size,      
      K_dead    = 20, 
      pool_cons = ResCons,
      pool_buff = ResBuff,
      Q         = clim$Q,
      T_dom     = input$demand_dom * mf$dom,
      T_irr     = input$demand_irr * mf$irr,
      T_eco     = input$demand_eco * mf$eco,
      evap.m    = (clim$PE - clim$Prec) * 1e-3,
      buffer    = input$BufferCoef,
      C_alloc   = input$AllocCoef,
      f_elev   = f_elev, 
      f_vol    = f_vol, 
      f_sarea  = f_sarea)
  })
  
  results <- reactive({
    
    Date <- as.Date("2020-01-01") + months(0:599)
    
    df <- sim()
    df_annual <- sim() %>% 
      group_by(Year) %>% summarize_each(funs(sum)) 
    
    #Calculate reliabilty metric
    rel <- df  %>% summarize(
      Environmental = length(which(R_eco >= 1 * T_eco)) * 100 / n(),
      Domestic      = length(which(R_dom >= 1 * T_dom)) * 100 / n(),
      Irrigation    = length(which(R_irr >= 1 * T_irr)) * 100 / n()) %>% 
      mutate_each(funs(round(.))) %>%
      mutate(Metric = "Reliability") %>%
      gather(key = variable, value = value, -Metric)
    
    crel <- df  %>% summarize(
      Environmental = length(which(R_eco >= 0.6 * T_eco)) * 100 / n(),
      Domestic      = length(which(R_dom >= 0.6 * T_dom)) * 100 / n(),
      Irrigation    = length(which(R_irr >= 0.6 * T_irr)) * 100 / n()) %>% 
      mutate_each(funs(round(.))) %>%
      mutate(Metric = "Critical Reliability") %>%
      gather(key = variable, value = value, -Metric)
    
    metric_df <- bind_rows(rel, crel)

    storage_monthly <- plot_ly(x = Date, y = df$S, name = "Storage level") %>%
      add_trace(x=Date, y=rep(input$Res_size, length(Date)), 
        name="Storage capacity") %>%
      add_trace(x=Date, y=rep(20, length(Date)), name="Inactive storage") %>%
      layout(legend = list(x = 0, y = -0.2),
        xaxis = list(title = "Year"),
        yaxis = list(title = "MCM", range=c(0,input$Res_size + 10)))
    
    ####Choose line settings colors for monthly/annual results plots, used below
    env_tar_line <- list(color = "#80E499", dash = "2px") # environmental target
    env_rel_line <- list(color = "#619757")               # environmental release
    dom_tar_line <- list(color = "#8AC1EB", dash = "2px")
    dom_rel_line <- list(color = "#6D9CBD")
    irr_tar_line <- list(color = "#E3ACE2", dash = "2px")
    irr_rel_line <- list(color = "#B082AC")
    
    
    ####Plot monthly results
    releases_monthly <- plot_ly(x=Date, y=df$T_eco, 
      name="Environmental target", line=env_tar_line) %>%
      add_trace(x=Date, y=df$R_eco, name="Environmental release",
                line=env_rel_line) %>%
      add_trace(x=Date, y=df$T_dom, name="Domestic target",
                line=dom_tar_line) %>%
      add_trace(x=Date, y=df$R_dom, name="Domestic release",
                line=dom_rel_line) %>%
      add_trace(x=Date, y=df$T_irr, name="Irrigation target",
                line=irr_tar_line) %>%
      add_trace(x=Date, y=df$R_irr, name="Irrigation release",
                line=irr_rel_line) %>%
      layout(legend = list(x = 0, y = -0.4),
             xaxis = list(title = "Year"),
             yaxis = list(title = "MCM"))
    
    #### Plot annual results
    releases_annual <- plot_ly(x=2020:2069, y=df_annual$T_eco, 
      name="Environmental target", line=env_tar_line) %>%
      add_trace(x=2020:2069, y=df_annual$R_eco, name="Environmental release",
                line=env_rel_line) %>%
      add_trace(x=2020:2069, y=df_annual$T_dom, name="Domestic target",
                line=dom_tar_line) %>%
      add_trace(x=2020:2069, y=df_annual$R_dom, name="Domestic release",
                line=dom_rel_line) %>%
      add_trace(x=2020:2069, y=df_annual$T_irr, name="Irrigation target",
                line=irr_tar_line) %>%
      add_trace(x=2020:2069, y=df_annual$R_irr, name="Irrigation release",
                line=irr_rel_line) %>%
      layout(legend = list(x = 0, y = -0.4, anchor = "right"),
             xaxis = list(title = "Year"),
             yaxis = list(title = "MCM"))

    #### Plot performance
    reliability <- plot_ly(x = rel$variable, y = rel$value, 
      name = "reliability", type = "bar") %>%
      layout(legend = list(x = 0, y = -0.4, anchor = "right"),
        xaxis = list(title = "Water uses"),
        yaxis = list(title = "%",range=c(0,100)))   

    critical_reliability <- plot_ly(x = crel$variable, y = crel$value, 
      name = "reliability", type = "bar") %>%
      layout(legend = list(x = 0, y = -0.4, anchor = "right"),
        xaxis = list(title = "Water uses"),
        yaxis = list(title = "%",range=c(0,100)))   

    MassBalance <- df %>% 
      mutate_each(funs(round(.,2))) %>% 
      select(Year, Month, Storage=S, Inflow=Q, 
             Domestic=R_dom, Irrigation=R_irr, Environmental=R_eco)
    
    list(releases_monthly = releases_monthly, releases_annual = releases_annual,
      storage_monthly = storage_monthly, MassBalance = MassBalance, 
      reliability = reliability, critical_reliability = critical_reliability)

  })
  
  # SIMULATION BUTTONS +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  observeEvent(input$SimReset, {reset("tabSimInputs")})

  # Demand Levels ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  output$SimDemandECO   <- renderUI({
    sliderInput(inputId = "demand_eco", "Environmental", 
                min = 0, max = 15, step= 1, value = 8, post = "MCM", ticks = F)
  })
  output$SimDemandDOM   <- renderUI({
    sliderInput(inputId = "demand_dom", "Domestic", 
                min = 0, max = 80, step= 1, value = 50, post = "MCM", ticks = F)
  })
  output$SimDemandIRR   <- renderUI({
    sliderInput(inputId = "demand_irr", "Irrigation", 
                min = 0, max = 50, step= 1, value = 20, post = "MCM", ticks = F)
  })
  # Demand Priorities ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  output$SimEcoPrio <- renderUI({
    selectInput(inputId = "SimEcoPrio", label = "", selected = 1, selectize = F,
                choices = users)
  })
  output$SimDomPrio <- renderUI({
    selectInput(inputId = "SimDomPrio", label = "", selected = 2, selectize = F,
                choices = users[!users == input$SimEcoPrio])
  }) 
  output$SimIrrPrio <- renderUI({
    selectInput(inputId = "SimIrrPrio", label = "", selected = 3, selectize = F,
                choices = users[!users %in% c(input$SimEcoPrio, input$SimDomPrio)])
  })

  # SIMULATION RESULTS PLOTTING ++++++++++++++++++++++++++++++++++++++++++++++++
  output$SimTable <- DT::renderDataTable({results()$MassBalance})
  output$SimTableButton <- renderUI({
    bsButton(inputId = "SimTableButton", label="mass balance", size='default')})
  
  output$SimPlotSelect <- renderUI({
    selectInput(inputId = "ListFigures", label="View time-series outputs",
      choices = c("Reservoir storage level" =  "storage_monthly",
        "Releases (monthly)" = "releases_monthly",
        "Releases (annual sums)"  = "releases_annual")) 
  })
  
  output$SimPerformanceSelect <- renderUI({
    selectInput(inputId = "ListFigures2", label="View performance metrics",
      choices = c("Reliability" =  "reliability",
        "Critical reliablity" = "critical_reliability")) 
  })
  
  SimPlot <- reactive({results()[[input$ListFigures]]})
  output$SimPlot <- renderPlotly({SimPlot()})
  
  SimPerformance <- reactive({results()[[input$ListFigures2]]})

#  reactive({print(SimPerformance())})
  output$SimPerformance <- renderPlotly({SimPerformance()})
  
  #output$SimPlotPerformance <- renderPlotly({results()[["Performance"]]})
  
  
# BAYESIAN NETWORK -------------------------------------------------------------  
  
  #LIKELIHOOD INFORMATION INPUTS +++++++++++++++++++++++++++++++++++++++++++++++
  
  ####### Future Development
  DevInputPlot <- reactive({
    plot_ly(x = c("Low", "Med", "High"),
            y = c(input$PrDevLow, input$PrDevMed, input$PrDevHigh),
            name = "Subjective probabilities", type = "bar") %>%
      layout(showlegend = FALSE,
             xaxis = list(title = "States"),
             yaxis = list(title = "Probabilities"))
  })
  output$DevInputPlot <- renderPlotly({DevInputPlot()})
  
  ####### Future population 
  PopInputPlot <- reactive({
    
    x <- qtruncnorm(seq(0,1, length.out = 10000),
                    a=input$PopMin, b=input$PopMax, mean = input$PopMean, sd = input$PopSD)
    y <- dtruncnorm(x,
                    a=input$PopMin, b=input$PopMax, mean = input$PopMean, sd = input$PopSD)
    
    plot_ly(x = x, y = y, hoverinfo = "none") %>%
      layout(showlegend = FALSE,
             xaxis = list(title = "Populaton (Million)"),
             yaxis = list(title = "Frequency"))
    
  })
  output$PopInputPlot <- renderPlotly({PopInputPlot()})
  
  ####### Mean climate change
  GCMInputPLot <- reactive({
    
    if(input$GCMInput == "ALL") {
      gcm_dat <- CMIP5
    } else {
      gcm_dat <- CMIP5 %>% filter(Scenario == input$GCMInput)
    }
    
    gcm_mu <- c(mean(gcm_dat$del_prec), mean(gcm_dat$del_temp))
    gcm_sd <- cov(cbind(gcm_dat[["del_prec"]], gcm_dat[["del_temp"]]))
    
    Prec_seq <- seq(0.4,1.8, length.out = 100)
    Temp_seq <- seq(0,4, length.out = 100)
    df <- expand.grid(Prec_seq, Temp_seq)
    colnames(df) <- c("prec", "temp")
    switch(input$GCMDist,
     "Bivariate normal" = {
       frequency <- dmvc(x = as.matrix(df), mu = gcm_mu, S = gcm_sd)
       frequency <- frequency / sum(frequency)
       plot_ly(x = df$temp, y = df$prec, z = frequency, type = "contour",
               colorscale = "Spectral") %>%
         layout(xaxis = list(title = "Temp. change", showgrid = TRUE),
                yaxis = list(title = "Precip. change", showgrid = TRUE))
     },
     "Bivariate cauchy"= {
       frequency <- dmvnorm(x = as.matrix(df), mean = gcm_mu, sigma = gcm_sd)
       frequency <- frequency / sum(frequency)
       plot_ly(x = df$temp, y = df$prec, z = frequency, type = "contour",
               colorscale = "Spectral") %>%
         layout(xaxis = list(title = "Temp. change", showgrid = TRUE),
                yaxis = list(title = "Precip. change", showgrid = TRUE))
     },
     "Kernel density (non-parametric)" = {
       plot_ly(x = gcm_dat$del_prec, y = gcm_dat$del_temp, type = "histogram2dcontour",
               colorscale = "Spectral") %>%
         layout(xaxis = list(title = "Temp. change", showgrid = TRUE),
                yaxis = list(title = "Precip. change", showgrid = TRUE))
           })
  })
  output$GCMInputPLot <- renderPlotly({GCMInputPLot()})
  
  ####### Per capita demand
  output$PCDMean1 <- renderUI({
    numericInput("PCDMean1", "Expected value given development = 'low'", 
      value=100, min=90, max=130, step=2)
  })
  output$PCDMean2 <- renderUI({
    numericInput("PCDMean2", "Expected value given development = 'medium'", 
      value=110, min=90, max=130, step=2)
  })
  output$PCDMean3 <- renderUI({
    numericInput("PCDMean3","Expected value given development = 'high'", 
      value = 120, min = 90, max = 130, step = 2)
  })
  
  PCDInputPlot <- reactive({
    
    #means <- c(input$PCDMean1, input$PCDMean2, input$PCDMean3)
    means <- c(100,110,120)
    
    x <- lapply(1:3, function(s)
      qnorm(seq(0.001,0.999, length.out = 10000), mean = means[s], sd = means[s]/5)
    )
    
    y <- lapply(1:3, function(s)
      dnorm(x = x[[s]], mean = means[s], sd = means[s]/5)
    )
    
    plot_ly(x = x[[1]], y = y[[1]], name = "Dev='low'",
            inherit = FALSE, hoverinfo = "name") %>%
      add_trace(x = x[[2]], y = y[[2]], name = "Dev='med'",
                hoverinfo = "name") %>%
      add_trace(x = x[[3]], y = y[[3]],name = "Dev='high'",
                hoverinfo = "name") %>%
      layout(showlegend = FALSE,
             xaxis = list(title = "Per capita demand (l/p/d)"),
             yaxis = list(title = "Frequency"))
  })
  output$PCDInputPlot <- renderPlotly({PCDInputPlot()})
  
  ####### Economic development
  output$PrDevLow <- renderUI({            
    numericInput("PrDevLow", "P(Development = Low)", value=0.3, min=0, max=1, step=0.1)
  })
  output$PrDevMed <- renderUI({          
    numericInput("PrDevMed", "P(Development = Medium)", value=0.4, min=0, max=1, step=0.1)
  })
  output$PrDevHigh <- renderUI({
    numericInput("PrDevHigh","P(Development = High)", value =0.3, min = 0, max=1, step=0.1)})
})