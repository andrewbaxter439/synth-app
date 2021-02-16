# Function to create synth plots as ggplot objects
# so = synth.res (output from `synth` function)
# dp = dataprep.res (output from `dataprep` function)

gg_synth <- function(dp = NULL, md = NULL, agegrp = "Under-18 birth rates", yr = 1999, post = FALSE, mspe = NULL, mspeOptim = FALSE) {
  require(dplyr)
  require(SPHSUgraphs)
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
    theme_sphsu_light() +
    ylab(paste0(agegrp," (per 1,000 women)")) +
    theme(legend.title = element_blank(),
          panel.grid.major = element_line(colour = "#e0e0e0"),
          panel.grid.minor = element_blank(),
    ) +
    scale_colour_manual(name = "Data", 
                        breaks = c("Treated", "Synthetic"),
                        labels = c("England and Wales", "Synthetic"), 
                        values = c("Synthetic" = sphsu_cols("Turquoise", names = FALSE), "Treated" = sphsu_cols("Thistle", names = FALSE))) +
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

# generatePlacebos <- function(data,
#                              predictors = NULL,
#                              special.predictors = NULL, 
#                              time.optimize.ssr = 1990:1998,
#                              time.plot = 1985:2013, 
#                              dependent = "rate", 
#                              ...) {
#   require(dplyr)
#   require(Synth)
#   require(tidyr)
#   
#   data <- data.frame(data %>% 
#                        filter(Country != "England and Wales"))
#   
#   ccodes <- data %>% 
#     select(Code, Country) %>% 
#     arrange(Code) %>% 
#     unique()
#   
#   placebos <- data.frame()
#   
#   for(i in 1:nrow(ccodes)){
#     
#     dp <- dataprep(
#       data,
#       predictors = predictors,
#       predictors.op = "mean",
#       special.predictors = special.predictors,
#       time.predictors.prior = time.plot[1]:1998,
#       dependent = dependent,
#       unit.variable = "Code",
#       unit.names.variable = "Country",
#       time.variable = "Year",
#       treatment.identifier = ccodes$Code[i],
#       controls.identifier = ccodes$Code[-i],
#       time.optimize.ssr = time.optimize.ssr,
#       time.plot = time.plot
#     )
#     
#     # md <- predvalues_synth(dp, FALSE, ...) 
#     so <- synth(dp, ...)
#     synthC <- dp$Y0 %*% so$solution.w
#     
#     md <- tibble(Year = as.numeric(rownames(dp$Y1)), Treated = dp$Y1[,1], Synthetic = synthC[,1]) %>% 
#       gather("Group", "Rate", -1)
#     
#     mspe <- pre_MSPE(md)
#     # mspe <- so$loss.v
#     
#     gaps <- md %>%
#       spread(Group, Rate) %>% 
#       mutate(Country = ccodes$Country[i],
#              Gap = Treated - Synthetic,
#              pre_mspe = mspe)
#     
#     placebos <- bind_rows(placebos, gaps)
#   }
#   return(placebos)
# }

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


# interpolateAb <- function(country, data = synth_data_plus_ab){
#   cdata_u20 <- data %>% 
#     filter(Country == country, agegrp == "Under 20")
#   
#   meanProp <- cdata_u20 %>% mutate(prop = Abortions/sumBirths) %>% 
#     filter(prop < mean(prop, na.rm = TRUE) + 2*sd(prop, na.rm = TRUE)) %>% 
#     summarise(mean = mean(prop)) %>% pull()
#   
#   new_dat <- cdata_u20 %>% 
#     filter(is.na(Abortions)) %>%
#     mutate(Abortions = sumBirths * meanProp) %>% 
#     full_join(cdata_u20 %>% filter(!is.na(Abortions)), by = c("Code", "Country", "Year", "Abortions", "agegrp", "sumBirths", "sumPops", "rate", "GDPperCap", "MF_ratio", "MobilePhones", "UrbanPop")) %>% 
#     mutate(totalPregs = Abortions + sumBirths)
#   
#   data %>% 
#     filter(Country == country, agegrp != "Under 20") %>% 
#     bind_rows(new_dat,.) %>% 
#     return()
#   
# }

# testSynthIterations_single <- function(yrs = 1990:1998,
#                                        pred = "pRate",
#                                        data = synthData_u20,
#                                        ccodes = u_20_ccodes,
#                                        n = 5,
#                                        predictors = NULL,
#                                        time.optimise = 1990:1998,
#                                        ...) {
#   
#   require(gtools)
#   require(dplyr)
#   require(Synth)
#   require(stringr)
#   
#   data <- data.frame(data)
#   x <- 1:n
#   
#   combos <- combinations(length(x), length(yrs), x, repeats.allowed = TRUE)
#   
#   i <- 1
#   
#   for (i in 1:nrow(combos)){
#     combos[i,] <- as.numeric(as.factor(as.character(combos[i,])))
#   }
#   
#   combos <- unique(combos)
#   
#   mspes <- c()
#   sps <- c()
#   
#   i <- 1
#   
#   for (i in 1:nrow(combos)) {
#     print(paste0("iteration ", i, "/", nrow(combos)))
#     
#     special.preds <- list(
#       a = list(pred, yrs = yrs[combos[i,] == 1], "mean"),
#       b = list(pred, yrs = yrs[combos[i,] == 2], "mean"),
#       c = list(pred, yrs = yrs[combos[i,] == 3], "mean"),
#       d = list(pred, yrs = yrs[combos[i,] == 4], "mean"),
#       e = list(pred, yrs = yrs[combos[i,] == 5], "mean")
#     )
#     
#     dp <-   dataprep(
#       foo = data.frame(data %>% filter(Year >= yrs[1])),
#       predictors = predictors,
#       special.predictors = special.preds[map_lgl(special.preds, ~ sum(.$yrs) > 0)],
#       time.predictors.prior = yrs,
#       dependent = pred,
#       unit.variable = "Code",
#       unit.names.variable = "Country",
#       time.variable = "Year",
#       treatment.identifier = ccodes$Code[ccodes$Country =="England and Wales"],
#       controls.identifier = ccodes$Code[ccodes$Country !="England and Wales"],
#       time.optimize.ssr = time.optimise,
#       time.plot = yrs
#     )
#     
#     # md <- predvalues_synth(dp, synth_outputs = FALSE, ...)
#     
#     so <- synth(dp, ...)
#     synthC <- dp$Y0 %*% so$solution.w
#     
#     md <- tibble(Year = as.numeric(rownames(dp$Y1)), Treated = dp$Y1[,1], Synthetic = synthC[,1]) %>%
#       gather("Group", "Rate", -1)
#     
#     mspes[i] <- so$loss.v
#     
#     sps[[i]] <-special.preds[map_lgl(special.preds, ~ sum(.$yrs) > 0)]
#     
#     
#     
#   }
#   
#   
#   tb <- tibble(iteration = 1:nrow(combos),
#                pattern = NA,
#                mspe = mspes,
#                sPred = sps,
#                groups = NA)
#   
#   i <- 1
#   for (i in 1:nrow(combos)){
#     tb$pattern[i] <- str_flatten(combos[i,], collapse = ", ")
#     tb$groups[i] <- length(tb$sPred[[i]])
#   }
#   
#   
#   return(tb)
#   
# }

# testSynthIterations <- function(yrs = 1990:1998, 
#                                 pred = "pRate",
#                                 data = synthData_u20, 
#                                 ccodes = u_20_ccodes,
#                                 n = 5,
#                                 predictors = NULL,
#                                 time.optimise = 1990:1998,
#                                 dependent = pred,
#                                 ...) {
#   
#   require(gtools)
#   require(dplyr)
#   require(Synth)
#   require(stringr)
#   require(foreach)
#   require(doParallel)
#   require(tcltk)
#   
#   data <- data.frame(data)
#   x <- 1:n
#   
#   combos <- combinations(length(x), length(yrs), x, repeats.allowed = TRUE)
#   
#   i <- 1
#   
#   for (i in 1:nrow(combos)){
#     combos[i,] <- as.numeric(as.factor(as.character(combos[i,])))
#   }
#   
#   combos <- unique(combos)
#   
#   mspes <- c()
#   sps <- c()
#   w <- c()
#   v <- c()
#   
#   cores <- detectCores() - 1
#   
#   if (cores>8) {cores <- 8}
#   
#   cl <- makeCluster(cores)
#   
#   registerDoParallel(cl)
#   
#   i <- 1
#   tb <- tibble()
#   # for (i in 1:nrow(combos)) {
#   tb <- foreach (i=1:nrow(combos),
#                  .packages = c("Synth", "dplyr", "purrr", "tidyr", "tcltk", "parallel"),
#                  .combine = bind_rows) %dopar% {
#                    
#                    if(!exists("pb")) pb <- tkProgressBar("Iterations", min = 1, max = nrow(combos))
#                    setTkProgressBar(pb, i)
#                    
#                    special.preds <- list(
#                      a = list(pred, yrs = yrs[combos[i,] == 1], "mean"),
#                      b = list(pred, yrs = yrs[combos[i,] == 2], "mean"),
#                      c = list(pred, yrs = yrs[combos[i,] == 3], "mean"),
#                      d = list(pred, yrs = yrs[combos[i,] == 4], "mean"),
#                      e = list(pred, yrs = yrs[combos[i,] == 5], "mean")
#                    )
#                    
#                    dp <- dataprep(
#                      foo = data.frame(data %>% filter(Year >= yrs[1])),
#                      predictors = predictors,
#                      special.predictors = special.preds[map_lgl(special.preds, ~ sum(.$yrs) > 0)],
#                      time.predictors.prior = yrs[1]:1998,
#                      dependent = dependent,
#                      unit.variable = "Code",
#                      unit.names.variable = "Country",
#                      time.variable = "Year",
#                      treatment.identifier = ccodes$Code[ccodes$Country =="England and Wales"],
#                      controls.identifier = ccodes$Code[ccodes$Country !="England and Wales"],
#                      time.optimize.ssr = time.optimise,
#                      time.plot = yrs[1]:2013
#                    )
#                    
#                    # md <- predvalues_synth(dp, synth_outputs = FALSE, ...)
#                    
#                    so <- synth(dp, ...)
#                    st <- synth.tab(so, dp)
#                    
#                    synthC <- dp$Y0 %*% so$solution.w
#                    
#                    md <- tibble(Year = as.numeric(rownames(dp$Y1)), Treated = dp$Y1[,1], Synthetic = synthC[,1]) %>% 
#                      gather("Group", "Rate", -1)
#                    
#                    mspe <- so$loss.v[1,1]
#                    
#                    w <- st$tab.w
#                    v <- tibble(Pred = row.names(st$tab.v), v_weight = as.numeric(st$tab.v))
#                    
#                    sps <- special.preds[map_lgl(special.preds, ~ sum(.$yrs) > 0)]
#                    
#                    gaps <- md %>%
#                      spread(Group, Rate) %>% 
#                      mutate(iteration = i,
#                             groups = length(sps),
#                             Gap = Treated - Synthetic)
#                    
#                    if(i>=(nrow(combos)-8)) {close(pb)}
#                    
#                    tibble(iteration = i, 
#                           pattern = NA,  
#                           mspe, 
#                           sPred = list(sps), 
#                           w_weights = list(w), 
#                           v_weights = list(v),
#                           gaps = list(gaps),
#                           groups = NA)
#                    
#                  }
#   
#   
#   # tb <- tibble(iteration = 1:nrow(combos),
#   #              pattern = NA,
#   #              mspe = mspes,
#   #              sPred = sps,
#   #              groups = NA,
#   #              w_weights = w,
#   #              v_weights = v)
#   # 
#   i <- 1
#   for (i in 1:nrow(combos)){
#     tb$pattern[i] <- str_flatten(combos[i,], collapse = ", ")
#     tb$groups[i] <- length(tb$sPred[[i]])
#   }
#   
#   
#   return(tb)
#   registerDoSEQ()
# }


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


# iterating through year permutations ------------------------------------------------------------------------

# plotIterations <- function(iteration = it_u18_rateSp, post = FALSE) {
#   
#   require(dplyr)
#   require(ggplot2)
#   require(SPHSUgraphs)
#   require(purrr)
#   require(ggpubr)
#   require(tidyr)
#   
#   mspes <- iteration %>% 
#     mutate(groups = as.factor(groups)) %>% 
#     ggplot(aes(mspe, fill = groups)) +
#     geom_histogram(bins = 50) +
#     theme_sphsu_light() +
#     scale_fill_sphsu()
#   
#   gaps <- iteration %>% 
#     select(gaps) %>%
#     map(bind_rows) %>%
#     pluck("gaps") %>% 
#     mutate(groups = as.factor(groups))
#   
#   if (!post) {
#     gaps <- gaps %>% filter(Year<1999)
#   }
#   
#   ggraph <- gaps %>% 
#     ggplot(aes(Year, Gap, group = iteration, col = groups)) + 
#     geom_segment(x = 1985, xend = 2013, y = 0, yend = 0, col = "black") + 
#     geom_line(size = 1, alpha = 0.8) +
#     theme_minimal()+
#     theme(panel.grid = element_blank())+
#     geom_vline(xintercept = 1998.5, linetype = "dashed", col = "grey") +
#     scale_colour_sphsu()
#   
#   ggarrange(mspes, ggraph, ncol = 2, common.legend = TRUE, legend = "right")
#   
# }
# 
# plotIterations <- function(iteration = it_u18_rateSp, labels = FALSE) {
#   
#   require(dplyr)
#   require(ggplot2)
#   require(SPHSUgraphs)
#   require(purrr)
#   require(ggpubr)
#   require(tidyr)
#   
#   # find top countries -----------------------------------------------------------------------------------------
#   
#   weight_labels <- iteration %>% 
#     select(iteration, w_weights, mspe) %>% 
#     unnest(cols = w_weights) %>% 
#     group_by(iteration) %>% 
#     top_n(1,w.weights) %>% 
#     ungroup() %>% 
#     group_by(unit.names) %>% 
#     top_n(1, -mspe) %>% 
#     mutate(weight = paste0(w.weights*100, "%"),
#            label = paste0(unit.names, "\n", weight, ", ", "MSPE = ", round(mspe, 3)))
#   
#   label_pos <- iteration %>%
#     select(gaps) %>%
#     map(bind_rows) %>%
#     pluck("gaps") %>% 
#     filter(Year<=1998) %>%
#     mutate(groups = as.factor(groups)) %>% 
#     inner_join(weight_labels, by = "iteration") %>% 
#     mutate(label = ifelse(Year == 1998, label, NA))
#   
#   # Under-18 special predictors --------------------------------------------------------------------------------
#   
#   
#   mspes <-
#     iteration %>% 
#     mutate(groups = as.factor(groups)) %>% 
#     ggplot(aes(mspe, fill = groups)) +
#     geom_histogram(bins = 100) +
#     theme_sphsu_light() +
#     scale_fill_sphsu() +
#     coord_cartesian(clip = "off")
#   
#   if(labels) {
#     ggp <- ggplot_build(mspes)
#     
#     ytop <- ggp[["data"]][[1]] %>% 
#       group_by(x) %>% 
#       summarise(count = sum(count)) %>% 
#       ungroup() %>% 
#       summarise(max = max(count)) %>% 
#       pull()*2/3
#     
#     
#     mspes <- mspes +
#       geom_text(data = label_pos, aes(x = mspe, y = ytop, label = label),
#                 hjust = 0,
#                 angle = 45,
#                 inherit.aes = FALSE) +
#       theme(plot.margin = unit(c(0,3,0,0), "cm"))
#   }
#   
#   
#   gaps <- iteration %>%
#     select(gaps) %>%
#     map(bind_rows) %>%
#     pluck("gaps") %>% 
#     mutate(groups = as.factor(groups)) %>% 
#     filter(Year<1999) %>% 
#     ggplot(aes(Year, Gap, col = groups, group = iteration)) + 
#     geom_segment(x = min(iteration$gaps[[1]]$Year), xend = 1999, y = 0, yend = 0, col = "black") + 
#     theme_minimal()+
#     theme(panel.grid = element_blank())+
#     geom_vline(xintercept = 1998.5, linetype = "dashed", col = "grey") +
#     scale_colour_sphsu()
#   
#   if(labels){
#     gaps <- gaps +
#       geom_line(size = 1, alpha = 0.2) +
#       geom_line(data = label_pos, alpha = 1, size = 2) +
#       geom_text_repel(data = label_pos %>% filter(Year == 1998), aes(x = 1998, y = Gap, label = unit.names),
#                       hjust = 0,
#                       direction = "y",
#                       nudge_x = 0.75,
#                       xlim = c(NA, 2010),
#                       inherit.aes = FALSE,
#                       na.rm = TRUE) +
#       theme(plot.margin = unit(c(0,4,0,0), "cm")) +
#       coord_cartesian(clip = 'off')
#   } else {
#     gaps <- gaps +
#       geom_line(size = 1, alpha = 0.8)
#   }
#   
#   ggarrange(mspes, gaps, ncol = 2, common.legend = TRUE, legend = "bottom")
#   
# }


# plot mspe ratios -------------------------------------------------------------------------------------------

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


# # iterating through removal of top-weighted countries --------------------------------------------------------
# gg_iterateCountries <- function(itco, jitter = FALSE, n = 10, float_labs = FALSE) {
#   
#   require(purrr)
#   require(ggrepel)
#   require(ggplot2)
#   require(dplyr)
#   
#   if(jitter){
#     height <- itco %>%
#       reduce(bind_rows) %>% 
#       top_n(n, -mspe) %>% 
#       summarise(h = max(mspe)) %>% 
#       pull()/50
#   } else {
#     height <- 0
#   }
#   
#   order_co <- 
#     itco %>% map( ~ select(.x, label)) %>% 
#     reduce(bind_rows) %>% 
#     pull()
#   
#   df <- itco %>% 
#     reduce(bind_rows) %>% 
#     top_n(n, -mspe) %>%
#     # filter(mspe<5*min(mspe)) %>%
#     select(mspe, unit.names, label, gaps) %>% 
#     unnest(gaps) %>% 
#     mutate(label = factor(label, levels = order_co)) %>% 
#     mutate(mgrp = as.numeric(factor(signif(mspe, 2)))) %>%
#     group_by(mgrp) %>% 
#     mutate(mems = n()/24,
#            member = as.numeric(factor(unit.names)),
#            adjust = -(mems+1)/2+member) %>% 
#     ungroup() %>% 
#     mutate(nGap = Gap + 0.005*adjust*max(Gap))
#   
#   plot <- df %>% 
#     ggplot(aes(Year, nGap)) + 
#     geom_line(size = 1, aes(col = label), position = position_jitter(h=height, w = 0)) + 
#     theme_minimal() +
#     theme(panel.grid = element_blank()) +
#     geom_vline(xintercept = 1998.5, linetype = "dashed", col = "grey") +
#     geom_segment(x = min(itco[[1]]$gaps[[1]]$Year), xend = 2013, y = 0, yend = 0, col = "black") +
#     scale_colour_sphsu(name = "Top weighted country") +
#     ylab("Gap = Treated - Synthetic Control")
#   
#   if(float_labs){
#     labs <- df %>% 
#       filter(Year == 2013) %>% 
#       select(Year, nGap, label)
#     plot <-
#       plot +
#       theme(legend.position = "none") +
#       coord_cartesian(clip = "off") +
#       geom_text_repel(data = labs,
#                       aes(x = (Year), y = nGap, label = label),
#                       hjust = 0,
#                       direction = "y",
#                       nudge_x = 0.8,
#                       xlim = c(NA, 2030)
#       ) +
#       theme(plot.margin = unit(c(0,10,0,0), "cm"))
#     
#   }
#   
#   return(plot)
#   
# }
