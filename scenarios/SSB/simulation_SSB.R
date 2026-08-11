# https://stackoverflow.com/questions/53622354/how-to-debug-line-by-line-rcpp-generated-code-in-windows
# R -d gdb -e "source('debug.r')"
# break simcpp
# run
source("./global.R")
IMPACTncd <- Simulation$new("scenarios/SSB/sim_design.yaml")

# IMPACTncd$del_parfs()
# IMPACTncd$del_synthpops()

# g <- IMPACTncd$get_causal_structure(print_plot = TRUE)
# g <- IMPACTncd$get_causal_structure(processed = FALSE, print_plot = TRUE, focus = "chd")
# g <- IMPACTncd$get_causal_structure(processed = FALSE, print_plot = TRUE, focus = "BMI", mode = "out", order = Inf)

# plot(igraph::make_ego_graph(g, order = 1, c("chd"), "in")[[1]])

start_runs <- 1
n_runs <- 100L

prop_coverage <- 0.72 # Kantar data showed that packaged beverages contribute to 72% of beverage sales in Indonesia
# Source: https://databoks.katadata.co.id/en/pdb/statistics/4597f3ff527242c/packaged-beverages-a-favorite-consumption-of-indonesian-urbanites?

prop_NW <- 0.46 # based on calculation of products with NW labels (high in sugar)


########## BASELINE SCENARIO
IMPACTncd$
del_logs()$
del_outputs()$
run(mc = start_runs:n_runs, multicore = TRUE, scenario_nam = "sc0") #$
#export_summaries(multicore = TRUE)

######## CONSUMER RESPONSE ONLY ##################################################################################################################################################

######## INCLUDING BMI PATHWAY
################# Scenario 1: Taxation 20% (Consumer response only)
IMPACTncd$update_primary_prevention_scn(
  function(synthpop) {
    #setDT(synthpop$pop)
    # Ensure that .Random.seed exists (it is created when a random number is generated)
    if (!exists(".Random.seed", envir = .GlobalEnv)) {
      runif(1)
    }

    # Save the current random seed state
    saved_seed <- .Random.seed

    # Change the seed (this will change the state of the RNG)
    set.seed(93684 + synthpop$mc_aggr)

    ## Reduction in sugar
    r_new <- runif(1, min = 0, max = 1)

    # Taxation https://cdn.cisdi.org/documents/fnm-IDPolicy-Brief-Elastisitas-Harga-MBDK-2025pdf-1757408493433-fnm.pdf
    # 20% tax ~ 18% reduction: low & medium SES
    # lower = 0.18*0.85 = 0.153; upper = 0.18*1.15 = 0.207
    #
    # 20% tax ~ 17.9% reduction: high SES
    # lower = 0.179*0.85 = 0.152; upper = 0.179*1.15 = 0.206
    #
    # using Betta distribution for percentage, as suggested by Chris:
    # install.packages("rriskDistributions")
    # library(rriskDistributions)
    #
    # Low SES:
    # q <- c(0.153, 0.18, 0.207)   # lower CI, mean, upper CI
    # p <- c(0.025, 0.5, 0.975)     # percentiles: 2.5%, 50%, 97.5%
    #
    # params <- get.beta.par(q = q, p = p)
    # params
    #
    # $par
    # [1]   141.0445 641.5319 #this is alpha, and beta
    elasticity_tax_lses <- qbeta(r_new, 141.0445, 641.5319) * (-1) / 20 # per 1% increase in price

    # High SES:
    # q <- c(0.152, 0.179, 0.206)   # lower CI, mean, upper CI
    # p <- c(0.025, 0.5, 0.975)     # percentiles: 2.5%, 50%, 97.5%
    #
    # params <- get.beta.par(q = q, p = p)
    # params
    #
    # $par
    # [1]   139.6617 639.5582 #this is alpha, and beta
    elasticity_tax_hses <- qbeta(r_new, 139.6617, 639.5582) * (-1) / 20 # per 1% increase in price

    tax_rate <- 20 #20% will be multiplied with the effect above

    synthpop$pop[, sugar_diet := ssb_curr_xps * prop_coverage] #ssb as the number of sugar consumed
    synthpop$pop[
      is.na(sugar_diet) | is.infinite(sugar_diet) | sugar_diet < 0,
      sugar_diet := 0
    ] #positive

    #We will estimate the impact from 2026
    sc_year <- 26L
    synthpop$pop[
      year >= sc_year & ses != "3",
      sugar_tax := sugar_diet * elasticity_tax_lses * tax_rate
    ]
    synthpop$pop[
      year >= sc_year & ses == "3",
      sugar_tax := sugar_diet * elasticity_tax_hses * tax_rate
    ]
    synthpop$pop[
      is.na(sugar_tax) | is.infinite(sugar_tax) | sugar_tax > 0,
      sugar_tax := 0
    ] #negative

    synthpop$pop[year >= sc_year, ssb_curr_xps := ssb_curr_xps + sugar_tax] #sugar is negative

    # This is the BMI pathway:
    # Effect of change in sugar on BMI per 8 oz ~ 227 ml ~ 20 grams of sugar https://doi.org/10.1001/jama.2017.0947
    # 0.10 kg/m2 (95% CI: [0.05, 0.15]) for baseline BMI <25 and 0.23 kg/m2 (95% CI: [0.14, 0.32]) for baseline BMI ≥ 25
    eff_BMI_a <- qnorm(
      r_new,
      mean = 0.1 / 20,
      sd = ((0.15 / 20) - (0.05 / 20)) / 3.92
    )
    eff_BMI_b <- qnorm(
      r_new,
      mean = 0.23 / 20,
      sd = ((0.32 / 20) - (0.14 / 20)) / 3.92
    )

    synthpop$pop[
      year >= sc_year,
      BMI_curr_xps := fifelse(
        BMI_curr_xps < 25,
        BMI_curr_xps + (sugar_tax * eff_BMI_a), #sugar is negative
        BMI_curr_xps + (sugar_tax * eff_BMI_b)
      )
    ]

    synthpop$pop[, c("sugar_diet", "sugar_tax") := NULL]

    # Restore the original seed state
    .Random.seed <- saved_seed
  }
)

IMPACTncd$run(
  start_runs:n_runs,
  multicore = TRUE,
  scenario_nam = "cons_sc1_tax_20"
)
#
# print("SUMMARY EXPORTS!!!")
#
# IMPACTncd$export_summaries(multicore = TRUE)

################# Scenario 2: NW labelling (Consumer response only)
IMPACTncd$update_primary_prevention_scn(
  function(synthpop) {
    #setDT(synthpop$pop)
    # Ensure that .Random.seed exists (it is created when a random number is generated)
    if (!exists(".Random.seed", envir = .GlobalEnv)) {
      runif(1)
    }

    # Save the current random seed state
    saved_seed <- .Random.seed

    # Change the seed (this will change the state of the RNG)
    set.seed(93684 + synthpop$mc_aggr)

    ## Reduction in sugar
    r_new <- runif(1, min = 0, max = 1)

    # NW labelling
    # Croker et al. https://doi.org/10.1111/jhn.12758
    # 0.67 (95% CI: -1.06, -0.28) gr per 100 gr of product weight (no differential effects)
    lbl_eff <- qnorm(
      r_new,
      mean = -0.67 / 100,
      sd = ((1.06 / 100) - (0.28 / 100)) / 3.92
    ) # gram reduction in sugar per 1 ml or 1 gr of SSBs # Assuming 1 gr ~ 1ml for SSBs

    synthpop$pop[, sugar_diet := ssb_curr_xps * prop_coverage] #ssb as the number of sugar consumed
    synthpop$pop[
      is.na(sugar_diet) | is.infinite(sugar_diet) | sugar_diet < 0,
      sugar_diet := 0
    ] #positive

    # We assume that for Indonesia, 1 SSB serving ~ 22.8 gr of sugar diluted in 240 ml of water or 9.5 gr/100 ml
    # Calculating the ml/gr of SSBs consumed based on sugar content
    # 22.8 gr of sugar -> 240, n gr of sugar -> x; so x -> n * 240/22.8
    #We will estimate the impact from 2026
    sc_year <- 26L
    synthpop$pop[
      year >= sc_year,
      sugar_lbl := lbl_eff * (sugar_diet * 240 / 22.8) * prop_NW
    ] # the reduction in sugar, applying prop_NW here as the reduction is in number (gr)
    synthpop$pop[
      is.na(sugar_lbl) | is.infinite(sugar_lbl) | sugar_lbl > 0,
      sugar_lbl := 0
    ] #negative
    # As the sugar_lbl is the number, not proportion, we need to make sure the [magnitude] reduction due to labelling is not higher than the baseline sugar intake
    synthpop$pop[
      abs(sugar_lbl) > sugar_diet * prop_NW,
      sugar_lbl := sugar_diet * prop_NW * (-1)
    ] #sugar diet doesnt include prop_NW

    synthpop$pop[year >= sc_year, ssb_curr_xps := ssb_curr_xps + sugar_lbl] #sugar is negative

    # This is the BMI pathway:
    # Effect of change in sugar on BMI per 8 oz ~ 227 ml ~ 20 grams of sugar https://doi.org/10.1001/jama.2017.0947
    # 0.10 kg/m2 (95% CI: [0.05, 0.15]) for baseline BMI <25 and 0.23 kg/m2 (95% CI: [0.14, 0.32]) for baseline BMI ≥ 25
    eff_BMI_a <- qnorm(
      r_new,
      mean = 0.1 / 20,
      sd = ((0.15 / 20) - (0.05 / 20)) / 3.92
    )
    eff_BMI_b <- qnorm(
      r_new,
      mean = 0.23 / 20,
      sd = ((0.32 / 20) - (0.14 / 20)) / 3.92
    )

    synthpop$pop[
      year >= sc_year,
      BMI_curr_xps := fifelse(
        BMI_curr_xps < 25,
        BMI_curr_xps + (sugar_lbl * eff_BMI_a), #sugar_tax is negative
        BMI_curr_xps + (sugar_lbl * eff_BMI_b)
      )
    ]

    synthpop$pop[, c("sugar_diet", "sugar_lbl") := NULL]

    # Restore the original seed state
    .Random.seed <- saved_seed
  }
)

IMPACTncd$run(start_runs:n_runs, multicore = TRUE, scenario_nam = "cons_sc2_NW")
#
# print("SUMMARY EXPORTS!!!")
#
# IMPACTncd$export_summaries(multicore = TRUE)

################# Scenario 3: 20% Taxation and NW labelling (Consumer response only)
IMPACTncd$update_primary_prevention_scn(
  function(synthpop) {
    #setDT(synthpop$pop)
    # Ensure that .Random.seed exists (it is created when a random number is generated)
    if (!exists(".Random.seed", envir = .GlobalEnv)) {
      runif(1)
    }

    # Save the current random seed state
    saved_seed <- .Random.seed

    # Change the seed (this will change the state of the RNG)
    set.seed(93684 + synthpop$mc_aggr)

    ## Reduction in sugar
    r_new <- runif(1, min = 0, max = 1)

    # Taxation https://cdn.cisdi.org/documents/fnm-IDPolicy-Brief-Elastisitas-Harga-MBDK-2025pdf-1757408493433-fnm.pdf
    # 20% tax ~ 18% reduction: low & medium SES
    # lower = 0.18*0.85 = 0.153; upper = 0.18*1.15 = 0.207
    #
    # 20% tax ~ 17.9% reduction: high SES
    # lower = 0.179*0.85 = 0.152; upper = 0.179*1.15 = 0.206
    #
    # using Betta distribution for percentage, as suggested by Chris:
    # install.packages("rriskDistributions")
    # library(rriskDistributions)
    #
    # Low SES:
    # q <- c(0.153, 0.18, 0.207)   # lower CI, mean, upper CI
    # p <- c(0.025, 0.5, 0.975)     # percentiles: 2.5%, 50%, 97.5%
    #
    # params <- get.beta.par(q = q, p = p)
    # params
    #
    # $par
    # [1]   141.0445 641.5319 #this is alpha, and beta
    elasticity_tax_lses <- qbeta(r_new, 141.0445, 641.5319) * (-1) / 20 # per 1% increase in price

    # High SES:
    # q <- c(0.152, 0.179, 0.206)   # lower CI, mean, upper CI
    # p <- c(0.025, 0.5, 0.975)     # percentiles: 2.5%, 50%, 97.5%
    #
    # params <- get.beta.par(q = q, p = p)
    # params
    #
    # $par
    # [1]   139.6617 639.5582 #this is alpha, and beta
    elasticity_tax_hses <- qbeta(r_new, 139.6617, 639.5582) * (-1) / 20 # per 1% increase in price

    tax_rate <- 20 #20% will be multiplied with the effect above

    # NW labelling
    # Croker et al. https://doi.org/10.1111/jhn.12758
    # 0.67 (95% CI: -1.06, -0.28) gr per 100 gr of product weight (no differential effects)
    lbl_eff <- qnorm(
      r_new,
      mean = -0.67 / 100,
      sd = ((1.06 / 100) - (0.28 / 100)) / 3.92
    ) # gram reduction in sugar per 1 ml or 1 gr of SSBs # Assuming 1 gr ~ 1ml for SSBs

    synthpop$pop[, sugar_diet := ssb_curr_xps * prop_coverage] #ssb as the number of sugar consumed
    synthpop$pop[
      is.na(sugar_diet) | is.infinite(sugar_diet) | sugar_diet < 0,
      sugar_diet := 0
    ] #positive

    #We will estimate the impact from 2026
    # The combined effect can be calculated as
    # 1 – [(1+PRi)*(1+PRj)] PR= percent reduction < 0
    sc_year <- 26L
    synthpop$pop[
      year >= sc_year & ses != "3",
      sugar_tax := sugar_diet * elasticity_tax_lses * tax_rate
    ] #20% tax
    synthpop$pop[
      year >= sc_year & ses == "3",
      sugar_tax := sugar_diet * elasticity_tax_hses * tax_rate
    ] #20% tax
    synthpop$pop[
      is.na(sugar_tax) | is.infinite(sugar_tax) | sugar_tax > 0,
      sugar_tax := 0
    ] #negative

    # For the next policy can be applied in the remaining baseline intake
    # We assume that for Indonesia, 1 SSB serving ~ 22.8 gr of sugar diluted in 240 ml of water or 9.5 gr/100 ml
    # Calculating the ml/gr of SSBs consumed based on sugar content
    # 22.8 gr of sugar -> 240, n gr of sugar -> x; so x -> n * 240/22.8

    synthpop$pop[
      year >= sc_year,
      sugar_lbl := ((sugar_tax + sugar_diet) / sugar_diet) *
        lbl_eff *
        (sugar_diet * 240 / 22.8) *
        prop_NW
    ] # the reduction in sugar, applying prop_NW here as the reduction is in number (gr)
    # As the sugar_lbl is the number, not proportion, we need to make sure the [magnitude] reduction due to labelling is not higher than the baseline sugar intake
    synthpop$pop[
      is.na(sugar_lbl) | is.infinite(sugar_lbl) | sugar_lbl > 0,
      sugar_lbl := 0
    ] #negative
    synthpop$pop[
      abs(sugar_lbl) >
        ((sugar_tax + sugar_diet) / sugar_diet) * sugar_diet * prop_NW,
      sugar_lbl := ((sugar_tax + sugar_diet) / sugar_diet) *
        sugar_diet *
        prop_NW *
        (-1)
    ] #sugar diet here does not include prop_NW

    synthpop$pop[year >= sc_year, sugar_combined := sugar_tax + sugar_lbl]
    synthpop$pop[
      is.na(sugar_combined) | is.infinite(sugar_combined) | sugar_combined > 0,
      sugar_combined := 0
    ] #negative

    synthpop$pop[year >= sc_year, ssb_curr_xps := ssb_curr_xps + sugar_combined] #sugar is negative

    # This is the BMI pathway:
    # Effect of change in sugar on BMI per 8 oz ~ 227 ml ~ 20 grams of sugar https://doi.org/10.1001/jama.2017.0947
    # 0.10 kg/m2 (95% CI: [0.05, 0.15]) for baseline BMI <25 and 0.23 kg/m2 (95% CI: [0.14, 0.32]) for baseline BMI ≥ 25
    eff_BMI_a <- qnorm(
      r_new,
      mean = 0.1 / 20,
      sd = ((0.15 / 20) - (0.05 / 20)) / 3.92
    )
    eff_BMI_b <- qnorm(
      r_new,
      mean = 0.23 / 20,
      sd = ((0.32 / 20) - (0.14 / 20)) / 3.92
    )

    synthpop$pop[
      year >= sc_year,
      BMI_curr_xps := fifelse(
        BMI_curr_xps < 25,
        BMI_curr_xps + (sugar_combined * eff_BMI_a), #sugar_tax is negative
        BMI_curr_xps + (sugar_combined * eff_BMI_b)
      )
    ]

    synthpop$pop[,
      c("sugar_diet", "sugar_tax", "sugar_lbl", "sugar_combined") := NULL
    ]

    # Restore the original seed state
    .Random.seed <- saved_seed
  }
)

IMPACTncd$run(
  start_runs:n_runs,
  multicore = TRUE,
  scenario_nam = "cons_sc3_tax_NW"
)
#
# print("SUMMARY EXPORTS!!!")
#
# IMPACTncd$export_summaries(multicore = TRUE)

print("SUMMARY EXPORTS!!!")
  
IMPACTncd$export_summaries(multicore = TRUE) #delete the previous summaries.
source("scenarios/SSB/process_out.R")