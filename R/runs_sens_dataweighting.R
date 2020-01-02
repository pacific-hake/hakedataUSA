
runs_sens_base <- function(dirbase, run = FALSE) {
  # 2019.03.01_h_prior_mean_low
  # 2019.03.02_h_fix_high
  # 2019.03.03_sigmaR_fix_low
  # 2019.03.04_sigmaR_fix_high
  # 2019.03.05_M_0.2SD
  # 2019.03.06_M_0.3SD
  # 2019.03.07_age1Survey
  # 2019.03.08_compWeight_HarmonicMean
  # 2019.03.09_compWeight_Francis

  number <- paste(strsplit(strsplit(
    basename(dirbase), "_")[[1]][1], "\\.")[[1]][1:2],
    collapse = ".")
  main <- dirname(dirbase)
  

  # 2019.03.08_compWeight_HarmonicMean
  # 2019.03.09_compWeight_Francis
  aa <- file.path(main, paste0(number, ".08_compWeight_HarmonicMean"))
  bb <- file.path(main, paste0(number, ".09_compWeight_Francis"))
  dir.create(aa, showWarnings = FALSE)
  dir.create(bb, showWarnings = FALSE)
  filesget <- dir(dirbase, pattern = "[aelt].ss\\b|.exe|ter.ss\\b")
  ignore <- sapply(filesget, function(x) {
    file.copy(file.path(dirbase, x), 
    file.path(aa, x), overwrite = TRUE, recursive = FALSE)
    file.copy(file.path(dirbase, x), 
    file.path(bb, x), overwrite = TRUE, recursive = FALSE)
  })
  filename <- dir(aa, pattern = "data", full.names = TRUE)
  lines <- readLines(filename)
  line <- grep("Lbin_method_for_Age_Data", lines)
  temp <- do.call("rbind", strsplit(lines[(line - 2):(line - 1)], "\\s+"))
  temp[, 5:6] <- 0
  lines[(line - 2):(line - 1)] <- apply(temp, 1, paste, collapse = " ")
  writeLines(lines, dir(aa, pattern = "data", full.names = TRUE))
  writeLines(lines, dir(bb, pattern = "data", full.names = TRUE))
  filename <- dir(aa, pattern = "control", full.names = TRUE)
  lines <- readLines(filename)
  line <- grep("# Dirichlet-Multinomial parameters", lines)
  lines <- lines[-(line:(line + 2))]
  lines <- append(lines, 
    c("5 1 0.15", "5 2 0.45"),
    grep("_7=mult_by_generalized_sizecomp", lines))
  writeLines(lines, dir(aa, pattern = "control", full.names = TRUE))
  writeLines(lines, dir(bb, pattern = "control", full.names = TRUE))

  if (run) {
    oldwd <- getwd()
    on.exit(setwd(oldwd), add = TRUE)
    dir <- dir(main, include.dirs = TRUE, pattern = "03\\.0[1-9]", 
      full.names = TRUE)
    for (ii in dir) {
      setwd(ii)
      print(ii);flush.console()
      system("ss3")
    }
    setwd(oldwd)

    for (ii in grep("compWeight", dir)){
      for (iii in 1:4) {
      filename <- dir(dir[ii], pattern = "_control", full.names = TRUE)
      out <- r4ss::SS_output(dir[ii], verbose = FALSE, printstats = FALSE)
      age <- out$Age_comp_Eff_N_tuning_check
      lines <- readLines(filename)
      line <- grep(" #_7=mult_by_generalized_sizecomp", lines)
      if (ii == 8) temp <- out$Age_comp_Eff_N_tuning_check$Recommend_Var_Adj
      if (ii == 9) temp <- r4ss::SS_tune_comps(out, write = FALSE)$New_Var_adj
      lines[c(line + 1, line + 2)] <- apply(cbind(do.call("rbind", 
        strsplit(lines[c(line + 1, line + 2)], "\\s"))[, 1:2],
        temp),
        1, paste, collapse = " ")
      writeLines(lines, filename)
      setwd(dir[ii])
      system("ss3")
      setwd(oldwd)
    }}
    compare <- lapply(dir(main,
      include.dirs = TRUE, 
      pattern = "03\\.0[1-9]|base$",
      #pattern = "03\\..{+}Selec|base$,
      full.names = TRUE),
      r4ss::SS_output, verbose = FALSE, printstats = FALSE)
    r4ss::SSplotComparisons(r4ss::SSsummarize(compare), pdf = TRUE, 
      plotdir = main,
      legendlabels = gsub(".+\\d_", "", dir(main,
        include.dirs = TRUE, 
        pattern = "03\\.0[1-9]|base$")))
        #pattern = "03\\..{+}Selec|base$")))
  }

}
