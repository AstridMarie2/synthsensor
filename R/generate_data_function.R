#' Generate a labeled two-sensor synthetic dataset
#'
#' @param n Integer length (min 100; clamped).
#' @param add_background Logical; include background signal.
#' @param background_type Character. One of the supported background models.
#' @param poisson_k,poisson_lambda See [generate_background_function()].
#' @param background_rho_rw,randomwalk_sd See [generate_background_function()].
#' @param sine_amplitude,sine_period See [generate_background_function()].
#' @param background_phi,background_rho See [generate_background_function()].
#' @param delayed_sensor Character. One of "None","Sensor1","Sensor2".
#' @param alpha_ema Numeric in (0,1]; EMA coefficient when a sensor is delayed.
#' @param sd1,sd2 Numeric > 0. Noise standard deviations.
#' @param crosscor_noise Numeric between -1 and 1 (inclusive). Correlation between sensor noise terms.
#' @param mean1,mean2 Numeric. Target means added via noise shift.
#' @param n_spikes_corr Integer >= 0. Number of correlated spike windows.
#' @param n_spikes_s1,n_spikes_s2 Integers >= 0. Uncorrelated spikes per sensor.
#' @param max_spike_length Integer >= 1. Max spike window length.
#' @param spike_size Max Spike Magnitude as frac. of mean, minimum 1.
#' @param n_drifts_s1,n_drifts_s2 Integers >= 0. Uncorrelated drift windows per sensor.
#' @param drift_duration Integer vector length 2, c(min, max), min >= 2.
#' @param drift_slope Numeric vector length 2, c(min, max). Linear drift slope range.
#' @return A data frame with columns: `TimeSinceClean`, `Date`, `Sensor1`, `Sensor2`,
#'   `Measurand1`, `Measurand2`, `AnomalyFlag1`, `AnomalyFlag2`, and `Diff`.
#' @export

generate_data_function <- function(
    n,
    add_background = TRUE,
    background_type = c("Poisson Moving Average","Random Walk","Sine Wave","AR(1) Process"),
    poisson_k = 5L,
    poisson_lambda = 3,
    background_rho_rw = 0,
    randomwalk_sd = 0.1,
    sine_amplitude = 1,
    sine_period = 20L,
    background_phi = 0.8,
    background_rho = 0,
    delayed_sensor = c("None","Sensor1","Sensor2"),
    alpha_ema = 0.3,
    sd1 = 0.1, sd2 = 0.1,
    crosscor_noise = 0,
    mean1 = 0, mean2 = 0,
    n_spikes_corr = 0L,
    n_spikes_s1 = 0L, n_spikes_s2 = 0L,
    max_spike_length = 5L,
    spike_size = 1,
    n_drifts_s1 = 0L, n_drifts_s2 = 0L,
    drift_duration = c(20L, 40L),
    drift_slope = c(-0.05, 0.05)
) {
  # ---- validation
  n <- max(as.integer(n), 100L)
  background_type <- match.arg(background_type)
  delayed_sensor  <- match.arg(delayed_sensor)
  stopifnot(sd1 > 0, sd2 > 0)
  stopifnot(crosscor_noise >= -1, crosscor_noise <= 1)
  stopifnot(max_spike_length >= 1, drift_duration[1] >= 2, drift_duration[2] >= drift_duration[1])

  # ---- base df
  df <- data.frame(
    TimeSinceClean = seq_len(n),
    Sensor1 = numeric(n),
    Sensor2 = numeric(n)
  )
  df$Date <- as.POSIXct("2025-01-01 00:00:00", tz = "UTC") + (df$TimeSinceClean - 1) * 3600
  df$AnomalyFlag1 <- new_flag_vec(n)
  df$AnomalyFlag2 <- new_flag_vec(n)

  # ---- background (+ optional EMA delay) & measurands
  if (add_background) {
    bg <- generate_background_function(
      n = n,
      background_type = background_type,
      poisson_k = poisson_k,
      poisson_lambda = poisson_lambda,
      randomwalk_sd = randomwalk_sd,
      background_rho_rw = background_rho_rw,
      sine_amplitude = sine_amplitude,
      sine_period = sine_period,
      background_phi = background_phi,
      background_rho = background_rho
    )
    s1_bg <- bg$sensor1; s2_bg <- bg$sensor2

    if (delayed_sensor == "Sensor1") {
      df$Sensor1 <- lagged_ema(s1_bg, alpha_ema); df$Sensor2 <- s2_bg
    } else if (delayed_sensor == "Sensor2") {
      df$Sensor1 <- s1_bg; df$Sensor2 <- lagged_ema(s2_bg, alpha_ema)
    } else {
      df$Sensor1 <- s1_bg; df$Sensor2 <- s2_bg
    }

    df$Measurand1 <- s1_bg + mean1
    df$Measurand2 <- s2_bg + mean2
  } else {
    df$Measurand1 <- rep(mean1, n)
    df$Measurand2 <- rep(mean2, n)
  }

  # ---- correlated Gaussian noise with specified means
  cov_matrix <- matrix(
    c(sd1^2, crosscor_noise * sd1 * sd2, crosscor_noise * sd1 * sd2, sd2^2),
    nrow = 2
  )
  noise <- MASS::mvrnorm(n = n, mu = c(0, 0), Sigma = cov_matrix)
  # shift to requested means
  noise[,1] <- noise[,1] + (mean1 - mean(noise[,1]))
  noise[,2] <- noise[,2] + (mean2 - mean(noise[,2]))
  df$Sensor1 <- df$Sensor1 + noise[,1]
  df$Sensor2 <- df$Sensor2 + noise[,2]

  # ---- spike amplitude bounds
  min_amp1 <- 2 * sd1; min_amp2 <- 2 * sd2
  max_amp1 <- max(mean1 * spike_size, mean1 * 1)
  max_amp2 <- max(mean2 * spike_size, mean2 * 1)

  draw_uamp <- function(minv, maxv) stats::runif(1, minv, maxv)

  # ---- correlated spikes
  if (n_spikes_corr > 0) {
    starts <- pick_spike_starts(n, n_spikes_corr, max_spike_length, buffer = 100L)
    if (length(starts)) {
      lens <- sample.int(max_spike_length, length(starts), replace = TRUE)
      for (i in seq_along(starts)) {
        idx <- starts[i]; end_idx <- min(idx + lens[i] - 1L, n)
        u <- stats::runif(1)
        amp1 <- min_amp1 + u * (max_amp1 - min_amp1)
        amp2 <- min_amp2 + u * (max_amp2 - min_amp2)
        sgn <- sample(c(-1, 1), 1)
        df$Sensor1[idx:end_idx] <- df$Sensor1[idx:end_idx] + sgn * amp1
        df$Sensor2[idx:end_idx] <- df$Sensor2[idx:end_idx] + sgn * amp2
        df$AnomalyFlag1 <- update_flags(df$AnomalyFlag1, idx:end_idx, "SpikeCorr")
        df$AnomalyFlag2 <- update_flags(df$AnomalyFlag2, idx:end_idx, "SpikeCorr")
      }
    }
  }

  # ---- uncorrelated spikes S1
  if (n_spikes_s1 > 0) {
    starts <- pick_spike_starts(n, n_spikes_s1, max_spike_length, buffer = 100L)
    if (length(starts)) {
      lens <- sample.int(max_spike_length, length(starts), replace = TRUE)
      for (i in seq_along(starts)) {
        idx <- starts[i]; end_idx <- min(idx + lens[i] - 1L, n)
        amp <- draw_uamp(min_amp1, max_amp1)
        sgn <- sample(c(-1, 1), 1)
        df$Sensor1[idx:end_idx] <- df$Sensor1[idx:end_idx] + sgn * amp
        df$AnomalyFlag1 <- update_flags(df$AnomalyFlag1, idx:end_idx, "Spike")
      }
    }
  }

  # ---- uncorrelated spikes S2
  if (n_spikes_s2 > 0) {
    starts <- pick_spike_starts(n, n_spikes_s2, max_spike_length, buffer = 100L)
    if (length(starts)) {
      lens <- sample.int(max_spike_length, length(starts), replace = TRUE)
      for (i in seq_along(starts)) {
        idx <- starts[i]; end_idx <- min(idx + lens[i] - 1L, n)
        amp <- draw_uamp(min_amp2, max_amp2)
        sgn <- sample(c(-1, 1), 1)
        df$Sensor2[idx:end_idx] <- df$Sensor2[idx:end_idx] + sgn * amp
        df$AnomalyFlag2 <- update_flags(df$AnomalyFlag2, idx:end_idx, "Spike")
      }
    }
  }

  # ---- drifts S1
  if (n_drifts_s1 > 0) {
    for (i in seq_len(n_drifts_s1)) {
      duration <- sample.int(drift_duration[2] - drift_duration[1] + 1L, 1L) + drift_duration[1] - 1L
      slope <- stats::runif(1, drift_slope[1], drift_slope[2])
      seg <- pick_segment(n, duration, buffer = 100L); if (is.null(seg)) next
      idx <- seg[1]:seg[2]
      drift <- seq(0, slope, length.out = duration)
      df$Sensor1[idx] <- df$Sensor1[idx] + drift
      df$AnomalyFlag1 <- update_flags(df$AnomalyFlag1, idx, "Drift")
    }
  }

  # ---- drifts S2
  if (n_drifts_s2 > 0) {
    for (i in seq_len(n_drifts_s2)) {
      duration <- sample.int(drift_duration[2] - drift_duration[1] + 1L, 1L) + drift_duration[1] - 1L
      slope <- stats::runif(1, drift_slope[1], drift_slope[2])
      seg <- pick_segment(n, duration, buffer = 100L); if (is.null(seg)) next
      idx <- seg[1]:seg[2]
      drift <- seq(0, slope, length.out = duration)
      df$Sensor2[idx] <- df$Sensor2[idx] + drift
      df$AnomalyFlag2 <- update_flags(df$AnomalyFlag2, idx, "Drift")
    }
  }

  df$Diff <- df$Sensor1 - df$Sensor2
  df
}


