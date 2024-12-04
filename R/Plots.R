# TODO: make this nice and add to Helpers.R
addTar <- function(data){
  result <- paste0(
    data$riskWindowStart,
    data$riskWindowEnd, 
    data$startAnchor, 
    data$endAnchor
    )
  
  return(result)
}


formatCohortType <- function(
    cohortType
){
  x <- rep('No outcome', length(cohortType))
  x[cohortType == 'Cases'] <- 'outcome'
  
  return(x)
}

plotAgeDistributions <- function(
    ageData,
    riskWindowStart = '1',
    riskWindowEnd = '365', 
    startAnchor = 'cohort start', 
    endAnchor = 'cohort start'
){
  # TODO add input checks
  
  ageData <- ageData %>%
    dplyr::filter(
      .data$riskWindowStart %in% c(NA,riskWindowStart) &
      .data$riskWindowEnd %in% c(NA,riskWindowEnd) &
        .data$startAnchor %in% c(NA,startAnchor) &
        .data$endAnchor %in% c(NA, endAnchor)
      )
  
  if(nrow(ageData) == 0){
    return() # empty plot?
  } 
  # TODO add input checks
  
  # filter to Target and Cases and remove censored
  ageData <- ageData %>% 
    dplyr::filter(.data$sumValue > 0) %>%
    dplyr::filter(.data$cohortType %in% c('Target', 'Cases'))
  
ind <- ageData$cohortType == 'Target'
ageData$averageValue[ind] <- -1*ageData$averageValue[ind] 
ageData$tar <- addTar(ageData)
result <- ggplot2::ggplot(
  data = ageData,
  ggplot2::aes(
    x = .data$averageValue,
    y = .data$covariateName,
    fill = formatCohortType(.data$cohortType)
  )
) +
  ggplot2::geom_col() +
  ggplot2::scale_x_continuous(
    breaks  = c(-1,-0.5, 0, 0.5, 1),
    labels = abs(c(-1,-0.5, 0, 0.5,  1))
  ) +
  ggplot2::facet_grid(
    cols = ggplot2::vars(.data$databaseName)
  ) +
  ggplot2::theme(
    legend.title=ggplot2::element_blank()
    ) +
  ggplot2::labs(
    y = "Variable", 
    x = "Frequency"
  )

return(result)
}


plotSexDistributions <- function(
    sexData,
    riskWindowStart = '1',
    riskWindowEnd = '365', 
    startAnchor = 'cohort start', 
    endAnchor = 'cohort start'
    ){
  # TODO add input checks
  
  sexData <- sexData %>% 
    dplyr::filter(
      .data$riskWindowStart %in% c(NA,riskWindowStart) &
      .data$riskWindowEnd %in% c(NA,riskWindowEnd) &
      .data$startAnchor %in% c(NA,startAnchor) &
      .data$endAnchor %in% c(NA, endAnchor)
  )
  
  if(nrow(sexData) == 0){
    return() # empty plot?
  } 
  
  # filter to Target and Cases and remove censored
  sexData <- sexData %>% 
    dplyr::filter(.data$sumValue > 0) %>%
    dplyr::filter(.data$cohortType %in% c('Target', 'Cases'))
  
  ind <- sexData$cohortType == 'Target'
  sexData$averageValue[ind] <- -1*sexData$averageValue[ind] 
  sexData$tar <- addTar(sexData)
  
  result <- ggplot2::ggplot(
    data = sexData,
    ggplot2::aes(
      x = .data$averageValue,
      y = .data$covariateName,
      fill =  formatCohortType(.data$cohortType)
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::scale_x_continuous(
      breaks  = c(-1,-0.5, 0, 0.5, 1),
      labels = abs(c(-1,-0.5, 0, 0.5,  1))
    ) +
    ggplot2::facet_grid(
      cols = ggplot2::vars(.data$databaseName)
    ) +
    ggplot2::theme(
      legend.title=ggplot2::element_blank()
      ) +
    ggplot2::labs(
      y = "Variable", 
      x = "Frequency"
    )
  
  return(result)
}


plotCmEstimates <- function(
    cmData,
    cmMeta,
    targetName,
    comparatorName,
    selectedAnalysisId
){
  
  fmtHazardRatio <- "%.2f"
  fmtIncidenceRate <- "%.1f"
  incidenceRateMult <- 365.25 * 1000 
  
    
  estimates <- cmData %>%
    dplyr::filter(.data$analysisId == !!selectedAnalysisId) %>%
    dplyr::mutate(
      hr = paste0(
        sprintf(fmtHazardRatio, .data$calibratedRr),
        " (",
        sprintf(fmtHazardRatio, .data$calibratedCi95Lb),
        ", ",
        sprintf(fmtHazardRatio, .data$calibratedCi95Ub),
        ")"),
      eventsTarget = ifelse(.data$targetOutcomes < 1, "<5", as.character(.data$targetOutcomes)),
      eventsComparator = ifelse(.data$comparatorOutcomes < 1, "<5", as.character(.data$comparatorOutcomes)),
      nTarget = prettyNum(.data$targetSubjects, big.mark = ","),
      nComparator = prettyNum(.data$comparatorSubjects, big.mark = ","),
      targetIr = ifelse(
        .data$targetOutcomes < 1,
        paste0("<", as.character(sprintf(fmtIncidenceRate, 5 / .data$targetDays * !!incidenceRateMult))),
        as.character(sprintf(fmtIncidenceRate, .data$targetOutcomes / .data$targetDays * !!incidenceRateMult))),
      comparatorIr = ifelse(
        .data$comparatorOutcomes < 1, 
        paste0("<", as.character(sprintf(fmtIncidenceRate, 5 / .data$comparatorDays * !!incidenceRateMult))),
        as.character(sprintf(fmtIncidenceRate, .data$comparatorOutcomes / .data$comparatorDays * !!incidenceRateMult))),
      mean = .data$calibratedRr,
      lower = .data$calibratedCi95Lb,
      upper = .data$calibratedCi95Ub,
      summary = F
      ) %>%
    dplyr::mutate(
      hr = ifelse(is.na(.data$calibratedRr), "--", .data$hr),
      eventsTarget = ifelse(.data$nTarget == 0, "--", .data$eventsTarget),
      eventsComparator = ifelse(.data$nComparator == 0, "--", .data$eventsComparator),
      targetIr = ifelse(.data$nTarget == 0, "--", .data$targetIr),
      comparatorIr = ifelse(.data$nComparator == 0, "--", .data$comparatorIr)
    ) %>%
    dplyr::arrange(.data$database) %>%
    dplyr::select(
      "database", 
      "nTarget", 
      "nComparator", 
      "eventsTarget", 
      "eventsComparator", 
      "targetIr", 
      "comparatorIr", 
      "hr", 
      "summary", 
      "mean", 
      "upper", 
      "lower"
    )
  
  if (nrow(estimates) <= 0) {
    # No data to plot
    return(NULL)
  }
  
  if (!is.null(cmMeta)) {
    meta <- cmMeta %>%
      dplyr::filter(.data$analysisId == !!selectedAnalysisId) %>%
      dplyr::mutate(
        hr = paste0(
          sprintf(fmtHazardRatio, .data$calibratedRr),
          " (",
          sprintf(fmtHazardRatio, .data$calibratedCi95Lb),
          ", ",
          sprintf(fmtHazardRatio, .data$calibratedCi95Ub),
          ")"),
        mean = .data$calibratedRr,
        upper = .data$calibratedCi95Ub,
        lower = .data$calibratedCi95Lb,
        nTarget = "",
        nComparator = "", 
        eventsTarget = "",
        eventsComparator = "",
        targetIr = "",
        comparatorIr = "",
        summary = T
      ) %>%
      dplyr::mutate(
        hr = ifelse(is.na(.data$calibratedRr), "--", .data$hr),
        database = "Meta Analysis"
      ) %>%
      dplyr::select(
        "database", 
        "nTarget", 
        "nComparator", 
        "eventsTarget", 
        "eventsComparator", 
        "targetIr", 
        "comparatorIr", 
        "hr", 
        "summary", 
        "mean", 
        "upper", 
        "lower"
      )
  }
  
  header <- tibble::tibble(
    database = c("Data", "Source"),
    nTarget = c("Target", "N"),
    nComparator = c("Comparator", "N"),
    eventsTarget = c("Target", "Events"),
    eventsComparator = c("Comparator", "Events"),
    targetIr = c("Target", "IR"),
    comparatorIr = c("Comparator", "IR"),
    hr = c("Hazard Ratio", "(95% CI)"),
    summary = T
  )
  
  plotData <- dplyr::bind_rows(
    header,
    estimates
  )
  
  if (!is.null(cmMeta)) {
    plotData <- dplyr::bind_rows(
      plotData,
      tibble::tibble(calibratedRr = NA_real_),
      meta
    )
  }
  
  dividers <- list(grid::gpar(lty = 1),
                   grid::gpar(lty = 1))
  names(dividers) <- as.character(c(3, nrow(plotData) - 1))
  
  # edit to enable log scale
  if(sum(plotData$lower < 0.01, na.rm = T) > 0){
    plotData$lower[plotData$lower < 0.01] <- 0.01
  }
  if(sum(plotData$upper > 50, na.rm = T) > 0){
    plotData$lower[plotData$lower > 50] <- 50
  }
  
  p <- plotData %>%
    forestplot::forestplot(
      labeltext = c( # .data?
        "database", 
        "nTarget", 
        "nComparator", 
         "eventsTarget", 
        "eventsComparator", 
        "targetIr", 
        "comparatorIr", 
        "hr"
        ),
      is.summary = summary,
      xlog = TRUE,
      boxsize = 0.5,
      hrzl_lines = dividers,
      # align = c("l", "c", "c", "c"),
      txt_gp = forestplot::fpTxtGp(summary = grid::gpar(cex=0.5)),
      colgap = grid::unit(2, "mm"),
      graph.pos = 8,
      title = paste(targetName, "v.", comparatorName))
  return(p)
}

plotSccsEstimates <- function(
    sccsData,
    sccsMeta,
    targetName,
    selectedAnalysisId
) {
  fmtHazardRatio <- "%.2f"
  fmtIncidenceRate <- "%.1f"
  incidenceRateMult <- 365.25 * 1000 
  
  estimates <- sccsData %>%
    dplyr::filter(.data$analysisId == !!selectedAnalysisId) %>%
    dplyr::select(
      "databaseName",
      "exposuresOutcomeSetId",
      "calibratedRr",
      "calibratedCi95Lb",
      "calibratedCi95Ub",
      "calibratedLogRr",
      "calibratedP",
      "outcomeSubjects":"observedDays") %>%
    tidyr::drop_na() %>%
    dplyr::arrange(.data$databaseName) %>%
    dplyr::mutate(db = .data$databaseName) %>%
    dplyr::mutate(
      irr = paste0(
        sprintf(fmtHazardRatio, .data$calibratedRr),
        " (",
        sprintf(fmtHazardRatio, .data$calibratedCi95Lb),
        ", ",
        sprintf(fmtHazardRatio, .data$calibratedCi95Ub),
        ")"),
      cases = prettyNum(.data$outcomeEvents, big.mark = ","),
      yearsObs = format(round(.data$observedDays / 365.25, 1), nsmall = 1, big.mark = ","),
      totalEvents = prettyNum(.data$outcomeEvents, big.mark = ","),
      nExposed = prettyNum(.data$covariateSubjects, big.mark = ","),
      yearsExposed = format(round(.data$covariateDays / 365.25, 1), nsmall = 1, big.mark = ","),
      exposedEvents = prettyNum(.data$covariateOutcomes, big.mark = ","),
      mean = .data$calibratedRr,
      lower = .data$calibratedCi95Lb,
      upper = .data$calibratedCi95Ub,
      summary = F)
  
  if (nrow(estimates) == 0) {
    # No data
    return(NULL)
  }
  
  if (!is.null(sccsMeta)) {
    meta <- sccsMeta %>%
      dplyr::filter(.data$analysisId == !!selectedAnalysisId) %>%
      dplyr::mutate(db = "Meta Analysis") %>% 
      dplyr::select(
        "db",
        "exposuresOutcomeSetId",
        "calibratedRr",
        "calibratedCi95Lb",
        "calibratedCi95Ub",
        "calibratedLogRr",
        "calibratedP",
        "outcomeSubjects":"observedDays") %>%
      tidyr::drop_na() %>%
      dplyr::mutate(
        irr = paste0(
          sprintf(fmtHazardRatio, .data$calibratedRr),
          " (",
          sprintf(fmtHazardRatio, .data$calibratedCi95Lb),
          ", ",
          sprintf(fmtHazardRatio, .data$calibratedCi95Ub),
          ")"),
        cases = "",
        yearsObs = "",
        totalEvents = "",
        nExposed = "",
        yearsExposed = "",
        exposedEvents = "",
        mean = .data$calibratedRr,
        lower = .data$calibratedCi95Lb,
        upper = .data$calibratedCi95Ub,
        summary = TRUE)
  }
  
  header <- tibble::tibble(
    db = c("Data", "Source"),
    cases = c("Total", "Cases"),
    yearsObs = c("Years of", "Observation"),
    totalEvents = c("Total", "Events"),
    nExposed = c("Exposed", "Cases"),
    yearsExposed = c("Exposed", "Years"),
    exposedEvents = c("Exposed", "Events"),
    irr = c("Incidence Rate Ratio", "(95% CI)"),
    summary = TRUE
  )
  
  plotData <- dplyr::bind_rows(
    header,
    estimates
  )
  
  if (!is.null(sccsMeta)) {
    plotData <- dplyr::bind_rows(
      plotData,
      tibble::tibble(calibratedRr = NA_real_),
      meta
    )
  }

  dividers <- list(grid::gpar(lty = 1),
                   grid::gpar(lty = 1))
  names(dividers) <- as.character(c(3, nrow(plotData) - 1))
  
  p <- plotData %>%
    forestplot::forestplot(
      labeltext = c(
        "db", 
        "cases", 
        "yearsObs", 
        "totalEvents", 
        "nExposed", 
        "yearsExposed", 
        "exposedEvents", 
        "irr"
        ),
      is.summary = summary,
      xlog = TRUE,
      boxsize = 0.5,
      hrzl_lines = dividers,
      # align = c("l", "c", "c", "c"),
      colgap = grid::unit(2, "mm"),
      graph.pos = 8,
      title = paste(targetName))
  return(p)
}
