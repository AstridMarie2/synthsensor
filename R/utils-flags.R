# Single source of truth for anomaly labels
ANOM_LEVELS <- c("Normal", "Drift", "Spike", "Both", "SpikeCorr")

# Create a fresh factor vector (all Normal)
new_flag_vec <- function(n) {
  factor(rep("Normal", n), levels = ANOM_LEVELS)
}

# Internal: set flags in a range with overlap rules
# - current values are factors with ANOM_LEVELS
# - new_tag is one of ANOM_LEVELS
# Rules:
#   * Drift + (Spike or SpikeCorr) -> Both
#   * Spike + Drift -> Both
#   * SpikeCorr + Drift -> Both
#   * Normal + X -> X
update_flags <- function(flag_vec, idx, new_tag) {
  stopifnot(all(new_tag %in% ANOM_LEVELS))
  cur <- as.character(flag_vec[idx])
  
  if (new_tag == "Drift") {
    cur[cur %in% c("Spike", "SpikeCorr")] <- "Both"
    cur[cur == "Normal"] <- "Drift"
    
  } else if (new_tag %in% c("Spike", "SpikeCorr")) {
    # Any existing Drift becomes Both
    cur[cur == "Drift"] <- "Both"
    # Normal becomes new_tag
    cur[cur == "Normal"] <- new_tag
    # If already Spike or SpikeCorr or Both, keep as-is
  } else if (new_tag == "Both") {
    cur[] <- "Both"  # rarely used directly
  }
  
  flag_vec[idx] <- factor(cur, levels = ANOM_LEVELS)
  flag_vec
}
