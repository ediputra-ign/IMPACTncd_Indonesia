source("./global.R")
design <- Design$new("./inputs/sim_design.yaml")

library(data.table)
library(gamlss)
library(gamlss.dist)
library(fst)
library(doParallel)
library(digest)
library(qs2)

# recombine the chunks of large files
# TODO logic to delete these files
if (file.exists("./simulation/large_files_indx.csv")) {
  fl <- fread("./simulation/large_files_indx.csv")$pths
  for (i in 1:length(fl)) {
    if (file.exists(fl[i])) next
    file <- fl[i]
    # recombine the chunks
    if (.Platform$OS.type == "unix") {
      system(paste0("cat ", file, ".chunk?? > ", file, ""))
    } else if (.Platform$OS.type == "windows") {
      # For windows split and cat are from https://unxutils.sourceforge.net/
      shell(paste0("cat ", file, ".chunk?? > ", file, ""))
    } else {
      stop("Operating system is not supported.")
    }
  }
}

# RR ----
# Create a named list of Exposure objects for the files in ./inputs/RR
fl <- list.files(path = "./inputs/RR", pattern = ".csvy$", full.names = TRUE)
# RR <- future_lapply(fl, Exposure$new, design, future.seed = 950480304L)
RR <- vector("list", length(fl))
for (i in seq_along(fl)) {
  print(fl[i])
  RR[[i]] <- Exposure$new(fl[i], design)
}
names(RR) <- sapply(RR, function(x) x$get_name())
# invisible(future_lapply(RR, function(x) {
#     x$gen_stochastic_effect(design, overwrite = TRUE, smooth = FALSE)
# },
# future.seed = 627524136L))
invisible(lapply(RR, function(x) {
  x$gen_stochastic_effect(design, overwrite = TRUE, smooth = FALSE)
}
))
# NOTE smooth cannot be exported to Design for now, because the first time
# this parameter changes we need logic to overwrite unsmoothed files
rm(fl)
#
# Generate diseases ----
diseases <- lapply(design$sim_prm$diseases, function(x) {
  x[["design_"]] <- design
  x[["RR"]] <- RR
  do.call(Disease$new, x)
})
names(diseases) <- sapply(design$sim_prm$diseases, `[[`, "name")

mk_scenario_init2 <- function(scenario_name, diseases_, sp, design_) {
  # scenario_suffix_for_pop <- paste0("_", scenario_name) # TODO get suffix from design
  scenario_suffix_for_pop <- scenario_name
  list(
    "exposures"          = design_$sim_prm$exposures,
    "scenarios"          = design_$sim_prm$scenarios, # to be generated programmatically
    "scenario"           = scenario_name,
    "kismet"             = design_$sim_prm$kismet, # If TRUE random numbers are the same for each scenario.
    "init_year"          = design_$sim_prm$init_year,
    "pids"               = "pid",
    "years"              = "year",
    "ages"               = "age",
    "ageL"               = design_$sim_prm$ageL,
    "all_cause_mrtl"     = paste0("all_cause_mrtl", scenario_suffix_for_pop),
    "cms_score"          = paste0("cms_score", scenario_suffix_for_pop),
    "cms_count"          = paste0("cms_count", scenario_suffix_for_pop),
    "strata_for_outputs" = c("pid", "year", "age", "sex"),
    "diseases"           = lapply(diseases_, function(x) x$to_cpp(sp, design_))
  )
}

# sp <- qs2::qs_read("./simulation/tmp_spfor test.qs")
# l <- mk_scenario_init2("", diseases, sp, design)
# simcpp(sp$pop, l, sp$mc)
# sp2 <- qs2::qs_read("simulation/tmp_spfor testold.qs")
# all.equal(sp$pop, sp2$pop)

# sim <- SynthPop$new(0L, design)
# sim$write_synthpop(1:500)
# sim$delete_synthpop(NULL)
# ll <- sim$gen_synthpop_demog(design)
sp  <- SynthPop$new(1L, design)

# lapply(diseases, function(x) x$harmonise_epi_tables(sp, verbose = TRUE))

# tt <- read_fst("inputs/disease_burden/chd_prvl.fst", as.data.table = T)
# anyNA(tt)
# self <- diseases$nonmodelled$.__enclos_env__$self
# private <- diseases$nonmodelled$.__enclos_env__$private
# self <- diseases$chd$.__enclos_env__$self
# private <- diseases$chd$.__enclos_env__$private
# self <- RR$`Med_DM~t2dm`$.__enclos_env__$self
# private <- RR$`Med_DM~t2dm`$.__enclos_env__$private
# private$fit_beta(c(tt[3500, c(mu, mu_lower, mu_upper)]), verbose = TRUE)
# private$fit_beta(c(tt[3500, c(mu, mu_upper)]), c(0.5, 0.975), verbose = TRUE)
# self <- IMPACTncd$.__enclos_env__$self
# private <- IMPACTncd$.__enclos_env__$private

# tt[is.na(shape1), c("shape1", "shape2") := private$fit_beta_vec(
#     q = list(mu, mu_upper, mu_lower),
#     p = c(0.5, 0.975, 0.025),
#     tolerance = 0.01,
#     verbose = TRUE
# )]
# anyNA(tt)
# write_fst(tt, "inputs/disease_burden/chd_prvl.fst")


lapply(diseases, function(x) {
  print(x)
  x$gen_parf_files(design)
})
