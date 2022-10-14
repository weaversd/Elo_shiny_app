library(shiny)

source("global.R")

server <- function(input, output, session) {
  
  output$eloParamsPanel <- renderUI({
    returnObj <- wellPanel(h3("Elo Equation Parameters"), hr(),
              paste0("Starting Elo: ", startingElo), hr(),
              paste0("Elo Denominator: ", EloDenom), hr(),
              numericInput("baseK", "Base K value", 30),
              numericInput("homeAdvantage", "Home Field Advantage", 100),
              numericInput("MOVmultiplier", "Margin of Victory Multiplier", 1),
              numericInput("playoffMultiplier", "Playoff Importance Multiplier", 1.2), hr(),
              h3("Other Factors"),
              numericInput("pctRegression", "Regression % to mean after season", 20),
              numericInput("pctNotToInclude", "% of initial games to leave out of accuracy calculation", 10))
    
    if (input$useOptimizedValues) {
      if (is.null(optimalValues())) {
        returnObj <- "No Optmized Values"
      } else {
        returnObj <- wellPanel(h3("Elo Equation Parameters"), hr(),
                               paste0("Starting Elo: ", startingElo), hr(),
                               paste0("Elo Denominator: ", EloDenom), hr(),
                               numericInput("baseK", "Base K value", optimalValues()$K[1]),
                               numericInput("homeAdvantage", "Home Field Advantage", optimalValues()$HFA[1]),
                               numericInput("MOVmultiplier", "Margin of Victory Multiplier", optimalValues()$MOV[1]),
                               numericInput("playoffMultiplier", "Playoff Importance Multiplier", 1.2), hr(),
                               h3("Other Factors"),
                               numericInput("pctRegression", "Regression % to mean after season", optimalValues()$REG[1]),
                               numericInput("pctNotToInclude", "% of initial games to leave out of accuracy calculation", 10))
      }
    }
    return(returnObj)
  })
  
  games <- reactive({
    if (!is.null(input$games.csv)) {
      import <- read.csv(input$games.csv$datapath)
      import$date <- as.Date(import$date, "%m/%d/%Y")
      import <- import[order(import$date),]
      import$season <- as.character(import$season)
      import <- populate_games(import)
      return(import)
    }
  })
  
  displayGames <- reactive({
    if (!is.null(games())) {
      display <- games()
      display$date <- as.character(display$date)
      return(display)
    }
  })
  
  output$gamesRaw <- renderTable(displayGames())
  
  
  teamNames <- reactive({get_team_list(games())})

  output$teamList <- renderTable({
    validate(
      need(!is.null(games()), "No Data Found. Import Dataset.")
    )
    if (!is.null(games())) {
      teamNames()
    }
  })
  
  output$MOVHistogram <- renderPlotly({
    if (!is.null(games())) {
      create_MOV_histogram(games())
    }
  })
  output$scoreHistogram <- renderPlotly({
    if (!is.null(games())) {
      create_score_histogram(games())
    }
  })
  
  output$homeAwayMOVhistogram <- renderPlotly({
    if (!is.null(games())) {
      create_homeAwayMOV_histogram(games())
    }
  })
  
  output$homeAwayScorehistogram <- renderPlotly({
    if (!is.null(games())) {
      create_homeAwayScore_histogram(games())
    }
  })

  winLossDrawDF <- reactive({
    homeWins <- nrow(games()[games()$winner == games()$homeTeam,])
    awayWins <- nrow(games()[games()$winner == games()$awayTeam,])
    draws <- nrow(games()[games()$winner == "DRAW",])
    homeWinRate <- homeWins / nrow(games())
    awayWinRate <- awayWins / nrow(games())
    tieRate <- draws / nrow(games())
    
    returnDF <- data.frame(homeWinPct = homeWinRate*100,
                           awayWinPct = awayWinRate*100,
                           tiePct = tieRate*100)
    
    return(returnDF)
  })
  
  output$winLossDrawTable <- renderTable(winLossDrawDF())
  
  output$homeAwayWinPct <- renderPlotly({
    if (!is.null(winLossDrawDF())) {
      create_homeAwayScore_chart(winLossDrawDF())
    }
  })
  
  eloTable <- reactive({
    validate(
      need(!is.null(games()), "No Data Found. Import Dataset.")
    )
    table <- fill_elo_historical(games(), input$homeAdvantage, EloDenom,
                        input$MOVmultiplier, MOVinteger,
                        input$baseK, input$playoffMultiplier,
                        startingElo, input$pctRegression,
                        winLossDrawDF()$tiePct[1])
    table$date <- as.character(table$date)
    return(table)
  })
  
  pctCorrect <- reactive({
    calculate_percent_correct(eloTable(), input$pctNotToInclude)
  })
  
  overlapValue <- reactive({
    overlap_subset_value(eloTable(), input$pctNotToInclude)
  })
  
  output$overlapValue <- renderText(paste0(round(overlapValue(), 3), " overlap between home MOV and win%"))
  
  
  output$overlapPlot <- renderPlotly(overlap_plot(eloTable(), input$pctNotToInclude))
  
  output$eloTable <- renderTable(eloTable())
  output$pctCorrect <- renderText(paste0(round(pctCorrect(), 3), "% of games predicted correctly"))
  
  eloPlotDataObj <- reactive(create_plotable_data(eloTable()))

  
  output$eloPlot <- renderPlotly({
    validate(
      need(!is.null(eloTable()), "No data. Cannot view")
    )
    create_elo_plot(eloPlotDataObj()[[1]], eloPlotDataObj()[[2]])})
  
  livePctCorrectDF <- reactive(percent_correct_time_df(eloTable(),
                                                       input$pctNotToInclude))
  output$livePctCorrectPlot <- renderPlotly(plot_live_pct_correct(livePctCorrectDF(),
                                                                  input$pctNotToInclude))
  
  calibrationDF <- reactive({
    validate(
      need(!is.null(eloTable()), "No data. Cannot view")
    )
    create_calibration_DF(eloTable(), input$pctNotToInclude)})
  
  output$calibrationPlot <- renderPlotly(plot_calibration_curve(calibrationDF()))
  output$calibrationResidualPlot <- renderPlotly(plot_calibration_residuals(calibrationDF()))
  output$calibrationScaledResidualPlot <- renderPlotly(plot_scaled_calibration_residuals(calibrationDF()))
  
  output$residualScore <- renderText(paste0("Absolute Residual Score: ", round(return_calibration_error(calibrationDF())[1], 4)))
  output$avgResidualScore <- renderText(paste0("Average Residual Score: ", round(return_calibration_error(calibrationDF())[2], 4)))
  
  optimizationRangeList <- eventReactive(input$optimizeGo, {
    list(HFARange = input$homeAdvantageRange,
         MOVRange = input$MOVmultiplierRange,
         REGRange = input$pctRegressionRange,
         KRange = input$baseKRange)
  })
  
  optimizeObj <- eventReactive(input$optimizeGo, {
    validate(
      need(!is.null(input$baseK), "Starting Elo Parameters Not loaded. Click on 'Elo Parameters' tab first to load defaults, then return here and click 'Optimize'")
    )
    
    progress <- shiny::Progress$new(style = "notification")
    progress$set(message = "Simulating", value = 0)
    on.exit(progress$close())
    
    updateProgress <- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
        value <- value + (progress$getMax() - value) / 100
      }
      progress$set(value = value, detail = detail)
    }
    
    
    optimize_residul_score(RangeList = optimizationRangeList(),
                                        elo_games = eloTable(),
                                        pctToNotInclude = input$pctNotToInclude,
                                        EloDenom = EloDenom,
                                        MOVinteger = MOVinteger,
                                        playoffMult = input$playoffMultiplier,
                                        startingElo = startingElo,
                                        drawRate = winLossDrawDF()$tiePct[1],
                                        iterationN = input$nIterations,
                                        topN = input$topNmedian,
                                        convergeFactor = input$ConvergenceFactor,
                           updateProgress = updateProgress)})
  
  output$residualScorePLot <- renderPlotly(plot_residualScore_vs_Iteration(optimizeObj()))
  output$HFARangePLot <- renderPlotly(plot_optimization_range_HFA(optimizeObj()))
  output$REGRangePLot <- renderPlotly(plot_optimization_range_REG(optimizeObj()))
  output$KRangePLot <- renderPlotly(plot_optimization_range_K(optimizeObj()))
  output$MOVRangePLot <- renderPlotly(plot_optimization_range_MOV(optimizeObj()))
  
  optimalValues <- reactive(optimizeObj()[["optimizedValues"]])
  
  output$optimizedValues <- renderTable(optimalValues())
  
  predictionImport <- reactive({
    if (!is.null(input$futureGames.csv)) {
      validate(
        need(!is.null(games()), "No historical data. Cannot predict.")
      )
      import <- read.csv(input$futureGames.csv$datapath)
      return(import)
    }
  })
  
  eloFutureGames <- reactive({
    validate(
      need(!is.null(predictionImport()), "No data. Cannot predict.")
    )
    fill_elo_scores_prediction(historical_elo = eloTable(),
                                                        future_games = predictionImport(),
                                                        startingElo = startingElo)})
  
  eloFutureGamesPredicted <- reactive({
    fill_winPcts_prediction(eloFutureGames(), 
                            drawRate = winLossDrawDF()$tiePct[1],
                            EloDenom = EloDenom,
                            HFA = input$homeAdvantage)
  })
  
  output$predictedGames <- renderTable({
    validate(
      need(!is.null(eloFutureGamesPredicted()), "No data. Cannot predict.")
    )
    eloFutureGamesPredicted()
  })
  
  output$summarisedPredictions <- renderTable({
    validate(
      need(!is.null(eloFutureGamesPredicted()), "No data. Cannot predict.")
    )
    summarise_predictions(eloFutureGamesPredicted())
  })
  
  
  
  output$downloadNFL2000.2020 <- downloadHandler(
    filename <- function() {
      "nfl_data 2000-2020.csv"
    },
    
    content <- function(file) {
      file.copy("exampleData/nfl_data 2000-2020.csv", file)
    }
  )
  
  output$downloadAUDL2014.2021 <- downloadHandler(
    filename <- function() {
      "AUDL_data 2014-2021.csv"
    },
    
    content <- function(file) {
      file.copy("exampleData/AUDL_data 2014-2021.csv", file)
    }
  )
  
  output$downloadNFL2021 <- downloadHandler(
    filename <- function() {
      "nfl games 2021 empty.csv"
    },
    
    content <- function(file) {
      file.copy("exampleData/nfl_data 2021 noPlayoffs.csv", file)
    }
  )
  
  output$downloadAUDL2022 <- downloadHandler(
    filename <- function() {
      "AUDL_data reg season games 2022.csv"
    },
    
    content <- function(file) {
      file.copy("exampleData/AUDL_data reg season games 2022.csv", file)
    }
  )
  
  
}