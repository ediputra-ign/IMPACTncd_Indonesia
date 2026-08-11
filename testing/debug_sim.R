source("./global.R")
library(gamlss.dist) # needed for single-core mode
IMPACTncd <- Simulation$new("testing/sim_design_testing.yaml")

# Delete synthpops so they regenerate with the fix
IMPACTncd$del_logs()$del_synthpops()

# Run single-core with proper error traceback
withCallingHandlers(
  IMPACTncd$run(1L, multicore = FALSE, "sc0"),
  error = function(e) {
    message("\n=== ERROR ===")
    message(conditionMessage(e))
    message("\n=== TRACEBACK ===")
    calls <- sys.calls()
    for (i in seq_along(calls)) {
      message(i, ": ", deparse(calls[[i]], width.cutoff = 200)[1])
    }
    message("\n=== DONE ===")
  }
)

message("\n=== SUCCESS - Simulation completed ===")
