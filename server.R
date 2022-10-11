library(shiny)

source("global.R")

server <- function(input, output, session) {
  
  games <- reactive({
    if (!is.null(input$games.csv)) {
      import <- read.csv(input$games.csv$datapath)
      import$date <- as.Date(import$date, "%m/%d/%Y")
      import <- import[order(import$date),]
      import$season <- as.character(import$season)
      import <- populate_games(import)
      write.csv(import, "test.csv", row.names = F)
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
    table <- fill_elo_historical(games(), input$homeAdvantage, input$EloDenom,
                        input$MOVmultiplier, input$MOVinteger,
                        input$baseK, input$playoffMultiplier,
                        input$startingElo, input$pctRegression,
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
  
  percentileScore <- reactive({
    percentileScoreTable()$subsetLivePercentileScore[length(percentileScoreTable()$subsetLivePercentileScore)]
  })
  
  weightedScore <- reactive({
    return_weighted_score(percentileScore(), pctCorrect()/100, weightOfCorrect = input$weightCorrect)
  })
  
  output$overlapValue <- renderText(paste0(round(overlapValue(), 3), " overlap between home MOV and win%"))
  
  output$percentileScore <- renderText(paste0(round(percentileScore(), 3), " percentile score between home MOV and win%"))
  
  output$weightedScore <- renderText(paste0(round(weightedScore()*100, 3), " weighted score (out of 100)"))
  
  output$overlapPlot <- renderPlotly(overlap_plot(eloTable(), input$pctNotToInclude))
  
  output$eloTable <- renderTable(eloTable())
  output$pctCorrect <- renderText(paste0(round(pctCorrect(), 3), "% of games predicted correctly"))
  
  eloPlotDataObj <- reactive(create_plotable_data(eloTable()))

  
  output$eloPlot <- renderPlotly(create_elo_plot(eloPlotDataObj()[[1]], eloPlotDataObj()[[2]]))
  
  livePctCorrectDF <- reactive(percent_correct_time_df(eloTable(),
                                                       input$pctNotToInclude))
  output$livePctCorrectPlot <- renderPlotly(plot_live_pct_correct(livePctCorrectDF(),
                                                                  input$pctNotToInclude))
  
  output$livePercentileScorePlot <- renderPlotly(plot_live_percentile_score(percentileScoreTable(),
                                                                            input$pctNotToInclude))
  
  output$liveCombinedScorePlot <- renderPlotly(plot_live_combined_score(percentileScoreTable(),
                                                                            input$pctNotToInclude))
  
  percentileScoreTable <- reactive(populate_percentile_score(livePctCorrectDF(),
                                                             input$pctNotToInclude,
                                                             input$weightCorrect))
  
  output$percentileScoreDensityPlot <- renderPlotly(plot_percentile_score_density(percentileScoreTable()))
  output$percentileScoreDistributionPlot <- renderPlotly(plot_percentile_score_distribution(percentileScoreTable()))
  
  calibrationDF <- reactive(create_calibration_DF(eloTable(), input$pctNotToInclude))
  
  output$calibrationPlot <- renderPlotly(plot_calibration_curve(calibrationDF()))
  output$calibrationResidualPlot <- renderPlotly(plot_calibration_residuals(calibrationDF()))
  output$calibrationScaledResidualPlot <- renderPlotly(plot_scaled_calibration_residuals(calibrationDF()))
  
  KoptTable <- reactive({
    table <- optimize_K(elo_games = games(),
                        HFARange = input$homeAdvantageRange,
                        EloDenom = input$EloDenom,
                        MOVmultRange = input$MOVmultiplierRange,
                        MOVint = input$MOVinteger,
                        playoffMult = input$playoffMultiplier,
                        startingElo = input$startingElo,
                        regressionMultRange = input$pctRegressionRange,
                        drawRate = winLossDrawDF()$tiePct[1],
                        KRange = input$baseKRange,
                        Kiter = input$baseKIter,
                        pctToNotInclude = input$pctNotToInclude)
  })
  
  output$optKtable <- renderTable(KoptTable())
  
  MOVmultoptTable <- reactive({
    table <- optimize_MOVmult(elo_games = games(),
                        HFARange = input$homeAdvantageRange,
                        EloDenom = input$EloDenom,
                        MOVmultRange = input$MOVmultiplierRange,
                        MOVmultiter = input$MOVmultiplierIter,
                        MOVint = input$MOVinteger,
                        playoffMult = input$playoffMultiplier,
                        startingElo = input$startingElo,
                        regressionMultRange = input$pctRegressionRange,
                        drawRate = winLossDrawDF()$tiePct[1],
                        KRange = input$baseKRange,
                        pctToNotInclude = input$pctNotToInclude)
  })
  
  output$optMOVmultable <- renderTable(MOVmultoptTable())
  
  
  PctRegTable <- reactive({
    table <- optimize_PctReg(elo_games = games(),
                              HFARange = input$homeAdvantageRange,
                              EloDenom = input$EloDenom,
                              MOVmultRange = input$MOVmultiplierRange,
                              MOVint = input$MOVinteger,
                              playoffMult = input$playoffMultiplier,
                              startingElo = input$startingElo,
                              regressionMultRange = input$pctRegressionRange,
                              regressionMultIter = input$pctRegressionIter,
                              drawRate = winLossDrawDF()$tiePct[1],
                              KRange = input$baseKRange,
                              pctToNotInclude = input$pctNotToInclude)
  })
  
  output$optPctRegtable <- renderTable(PctRegTable())
  
  
  HFATable <- reactive({
    table <- optimize_HFA(elo_games = games(),
                             HFARange = input$homeAdvantageRange,
                             HFAiter = input$homeAdvantageIter,
                             EloDenom = input$EloDenom,
                             MOVmultRange = input$MOVmultiplierRange,
                             MOVint = input$MOVinteger,
                             playoffMult = input$playoffMultiplier,
                             startingElo = input$startingElo,
                             regressionMultRange = input$pctRegressionRange,
                             drawRate = winLossDrawDF()$tiePct[1],
                             KRange = input$baseKRange,
                             pctToNotInclude = input$pctNotToInclude)
  })
  
  output$optHFAtable <- renderTable(HFATable())
  
  
  output$optKPlot <- renderPlotly(plot_opt_DF(KoptTable(),
                                              title = "K factor Optimization",
                                              xtitle = "K factor",
                                              color = "hotpink"))
  
  output$optHFAPlot <- renderPlotly(plot_opt_DF(HFATable(),
                                              title = "Home Field Adv Optimization",
                                              xtitle = "Home Field Adv",
                                              color = "blue"))
  
  output$optMOVmultPlot <- renderPlotly(plot_opt_DF(MOVmultoptTable(),
                                                title = "Margin of Victory Optimization",
                                                xtitle = "MOV multiplier",
                                                color = "green4"))
  
  output$optPctRegPlot <- renderPlotly(plot_opt_DF(PctRegTable(),
                                                title = "% Regression between seasons Optimization",
                                                xtitle = "% Regression",
                                                color = "purple"))
}