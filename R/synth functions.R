# Function to create synth plots as ggplot objects
# so = synth.res (output from `synth` function)
# dp = dataprep.res (output from `dataprep` function)

gg_synth <- function(dp = NULL, md = NULL, agegrp = "Under-18 birth rates", yr = 1999, post = FALSE, mspe = NULL, mspeOptim = FALSE) {
  require(dplyr)
  require(Synth)
  
  
  if ((is.null(dp)&&is.null(md))|(!is.null(dp)&&!is.null(md)))
    stop("Please enter either dataprep object or model")
  
  if (is.null(md)&&!is.null(dp)) {
    
    # agegrp <- gsub("^.*(\\d{2}).*$","\\1", deparse(substitute(dp)))
    so <- synth(dp)
    synthC <- dp$Y0 %*% so$solution.w
    
    md <- tibble(Year = as.numeric(rownames(dp$Y1)), Treated = dp$Y1[,1], Synthetic = synthC[,1]) %>% 
      gather("Group", "Rate", -1)
    if (is.null(mspe) & mspeOptim){
      mspe <- so$loss.v[1]
    }
  } else {
    
    # agegrp <- gsub("^.*(\\d{2}).*$","\\1", deparse(substitute(md)))
  }
  
  if(post){
    xmax = NA
  } else {
    xmax = yr
    md <- md %>% filter(Year<yr)
  }
  
  if(is.null(mspe) & mspeOptim) {
    stop("Please enter mspe for the synth output, or change mspeOptim to 'FALSE' to calculate from whole pre-intervention period.")
  }
  
  if(!is.null(mspe)){
    mspe <- signif(mspe, 3)
  } else {
    mspe <- pre_MSPE(md)
  }
  
  
  plot <-  ggplot(md, aes(Year, Rate, col = Group)) +  # no linetype change
    geom_line(size = 2) +
    ylab(paste0(agegrp," (per 1,000 women)")) +
    theme_light() +
    theme(legend.title = element_blank(),
          panel.grid.major = element_line(colour = "#e0e0e0"),
          panel.grid.minor = element_blank(),
          panel.border     = element_rect(fill = NA, colour = NA),
          axis.line        = element_line(colour = "grey70", size = rel(1))
    ) +
    scale_colour_manual(name = "Data", 
                        breaks = c("Treated", "Synthetic"),
                        labels = c("England and Wales", "Synthetic"), 
                        values = c("Synthetic" = "#00B5D1", "Treated" = "#951272")) +
    geom_vline(xintercept = 1998.5, linetype = "dotted") +
    scale_x_continuous(limits = c(NA, xmax)) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(subtitle = paste0("Pre-intervention MSPE = ", mspe))
  
  return(plot)
  
}

predvalues_synth <- function(dp, synth_outputs = TRUE, yr = 1999, ...) {
  require(dplyr)
  require(Synth)
  
  so <- synth(dp, ...)
  synthC <- dp$Y0 %*% so$solution.w
  
  if (synth_outputs){
    so_name <- gsub("^[a-z]+_(.*$)", "so_\\1", deparse(substitute(dp)))
    st_name <- gsub("^[a-z]+_(.*$)", "st_\\1", deparse(substitute(dp)))
    print(paste("outputting", so_name, "and", st_name))
    st <- synth.tab(so, dp)
    
    assign(so_name, so, envir = .GlobalEnv)
    assign(st_name, st, envir = .GlobalEnv)
  }
  
  df <- tibble(Year = as.numeric(rownames(dp$Y1)), Treated = dp$Y1[,1], Synthetic = synthC[,1]) %>% 
    gather("Group", "Rate", -1)
  
  return(df)
  
}

printCoefficients <- function(md = NULL, model = NULL){
  
  if ((is.null(model)&&is.null(md))|(!is.null(model)&&!is.null(md)))
    stop("Please enter either lm model or data set ('md = ')")
  
  if (is.null(model)) {
    model <- md %>% filter(Year > 1999) %>% 
      mutate(Time = Year-1998) %>% 
      lm(Rate ~ Time + Group + Time*Group, data = .)
  }
  
  print("Coefficients:")
  coefs <- as_tibble(summary(model)$coefficients) %>%
    mutate(Coefficient = rownames(summary(model)$coefficients),
           Estimate = as.numeric(Estimate),
           SE = as.numeric(`Std. Error`),
           P = as.numeric(`Pr(>|t|)`)) %>% 
    select(Coefficient, Estimate, SE, P)
  
  print(coefs)
  
  print("Confidence intervals:")
  coefs %>% 
    transmute(Coefficient = Coefficient,
              Estimate = Estimate,
              LowerCI = Estimate - 1.96*SE,
              UpperCI = Estimate + 1.96*SE) %>% 
    print()
}

pre_MSPE <- function (md){
  md %>%
    filter(Year < 1999) %>% 
    spread(Group, Rate) %>% 
    mutate(SPE = (Treated - Synthetic)**2) %>% 
    summarise(MSPE = mean(SPE)) %>% 
    pull() %>% 
    signif(3)
}

sPredText <- function(dp) {
  paste("Prediction periods:",
        map(dp$tag$special.predictors, pluck(2)) %>% 
          map(function(x) paste0(min(x), "-", max(x))) %>% 
          str_flatten(collapse = "; ")
  )
}

gg_gaps <- function(md, pl, dp = NULL, mspe_limit = NULL, title = FALSE, subtitle = FALSE) {
  
  
  if(is.null(mspe_limit)) {
    mspe_limit <- pre_MSPE(md)
  }
  
  p <- md %>% 
    spread(Group, Rate) %>% 
    mutate(Gap = Treated - Synthetic) %>% 
    ggplot(aes(Year, Gap)) +
    geom_segment(x = min(md$Year), xend = 2013, y = 0, yend = 0) +
    geom_line(data = pl %>% filter(pre_mspe < 5*mspe_limit), aes(group = Country), col = "grey") +
    geom_line(col = sphsu_cols("Thistle", names = FALSE), size = 2) +
    theme_minimal() +
    theme(panel.grid = element_blank()) +
    geom_vline(xintercept = 1998.5, linetype = "dotted") +
    ylab("Gap = Treated - Synthetic Control")
  
  pl %>% 
    filter(pre_mspe > 5*mspe_limit) %>% 
    pull(Country) %>% 
    unique() %>% 
    paste(collapse = ", ") %>% 
    cat("Countries removed:", .)
  
  if(title){
    p <- p + labs(title = sPredText(dp_u20_sp))
  }
  
  if(subtitle) {
    p <- p + labs(subtitle = paste0("MSPE over optimisation period: ", signif(mspe_limit_u20_sp, 3), " (controls <5*MSPE)"))
  }
  
  p
  
}

rateDiff <- function(md, age = "under 18") {
  
  pop <- synthData %>% filter(Year > 1998,
                              Year< 2014,
                              Country == "England and Wales",
                              grepl(age, .$agegrp, ignore.case = TRUE)) %>% 
    select(Year, sumPops)
  
  df <- md %>% 
    filter(Year > 1998) %>% 
    spread(Group, Rate) %>% 
    mutate(Gap = Treated - Synthetic) %>% 
    right_join(pop, by = "Year") %>% 
    mutate(ab_diff = Gap * sumPops / 1000) %>% 
    summarise(tot_rate = sum(Gap),
              tot_diff = sum(ab_diff))
  
  list <- list(tot_rate = round(df[[1]], 2),
               tot_diff = round(df[[2]], 0) %>% format(big.mark = ','),
               mean_pop = round(mean(pop$sumPops), 0)  %>% format(big.mark = ',')
  )
  return(list)
}


# Initial dataprep and synth ----------------------------------------


synthPrep <- function(data, 
                      grp, 
                      dependent,
                      time.predictors.prior = NULL,
                      time.optimise.ssr = NULL,
                      time.plot = NULL,
                      predictors = NULL,
                      special.predictors = NULL,
                      assign_global = TRUE,
                      ...){
  
  start <- min(data$Year)  
  
  
  ccodes <- data %>% 
    select(Code, Country) %>% 
    arrange(Code) %>% 
    unique()
  
  if(is.null(time.predictors.prior)){
    time.predictors.prior <- start:1998
  }
  
  if(is.null(time.optimise.ssr)){
    time.optimise.ssr <- start:1998
  }
  
  if(is.null(time.plot)){
    time.plot <- start:2013
  }
  
  dp <- dataprep(
    foo = data,
    dependent = dependent,
    unit.variable = "Code",
    unit.names.variable = "Country",
    time.variable = "Year",
    time.predictors.prior = time.predictors.prior,
    time.optimize.ssr = time.optimise.ssr,
    time.plot = time.plot,
    treatment.identifier = ccodes$Code[ccodes$Country =="England and Wales"],
    controls.identifier = ccodes$Code[ccodes$Country !="England and Wales"],
    predictors = predictors,
    special.predictors = special.predictors
  )
  
  so <- synth(dp, ...)
  st <- synth.tab(so, dp)
  
  synthC <- dp$Y0 %*% so$solution.w
  mspe_lim <- so$loss.v[1]
  
  
  md <- tibble(Year = as.numeric(rownames(dp$Y1)), Treated = dp$Y1[,1], Synthetic = synthC[,1]) %>% 
    gather("Group", "Rate", -1)
  
  if (assign_global) {
    assign(paste0("dp_", grp), dp, envir = .GlobalEnv)
    assign(paste0("so_", grp), so, envir = .GlobalEnv)
    assign(paste0("st_", grp), st, envir = .GlobalEnv)
    assign(paste0("md_", grp), md, envir = .GlobalEnv)
    assign(paste0("mspe_limit_", grp), mspe_lim, envir = .GlobalEnv)
  }
  
  return(md)
}


# pre-post mspe ratios ---------------------------------------------------------------------------------------

gg_pre_postMSPE <- function(md, pl){
  
  
  df <- md %>% 
    spread(Group, Rate) %>% 
    mutate(Gap = Treated - Synthetic,
           Country = "England and Wales") %>% 
    select(Year, Country, Gap) %>% 
    bind_rows(pl %>% select(Year, Country, Gap)) %>% 
    mutate(period = ifelse(Year<1999, "pre", "post")) %>% 
    group_by(Country, period) %>% 
    summarise(mspe = mean(Gap**2)) %>% 
    spread(period, mspe) %>% 
    mutate(ratio = post/pre,
           label = ifelse(Country=="England and Wales", paste0("England and Wales; ratio = ", signif(ratio, 3)), NA),
           xintercept = ifelse(Country=="England and Wales", ratio, NA))
  
  df %>% 
    select(Country, ratio) %>% 
    arrange(ratio) %>% 
    ungroup() %>% 
    mutate(rank = row_number()) %>% 
    print()
  
  p <-  ggplot(df, aes(ratio)) +
    geom_histogram(fill = sphsu_cols("Cobalt"), col = "darkgrey", bins = 60) +
    theme_minimal() + 
    theme(panel.grid = element_blank())
  
  ggp <- ggplot_build(p)
  
  ytop <- max(ggp[["data"]][[1]][["count"]])
  
  p <-   p + geom_text(aes(x = xintercept, label = label),vjust = 0, hjust = 0, y = ytop + 0.1, inherit.aes = FALSE) +
    geom_segment(aes(x = xintercept, xend = xintercept), y = 0, yend = ytop, inherit.aes = FALSE) +
    ylim(0, ytop + 0.25)
  
  return(p)
}