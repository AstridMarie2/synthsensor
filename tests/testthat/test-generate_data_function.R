test_that("shape, columns, factor flags", {
  set.seed(42)
  out <- generate_data_function(
    n = 80, add_background = FALSE, background_type = "Sine Wave",
    sd1 = 0.1, sd2 = 0.2, crosscor_noise = 0, mean1 = 1, mean2 = 2,
    n_spikes_corr = 0, n_spikes_s1 = 0, n_spikes_s2 = 0, max_spike_length = 5,
    n_drifts_s1 = 0, n_drifts_s2 = 0, drift_duration = c(20,30), drift_slope = c(-0.05,0.05),
    delayed_sensor = "None", alpha_ema = 0.3
  )
  expect_equal(nrow(out), 100L)
  expect_true(all(c("Time","Sensor1","Sensor2","Date",
                    "Measurand1","Measurand2","AnomalyFlag1","AnomalyFlag2","Diff") %in% names(out)))
  expect_true(is.factor(out$AnomalyFlag1))
  expect_true(is.factor(out$AnomalyFlag2))
  expect_equal(levels(out$AnomalyFlag1), ANOM_LEVELS)
  expect_true(all(out$AnomalyFlag1 == "Normal"))
})

test_that("measurands without background are constants; Diff ok", {
  set.seed(1)
  out <- generate_data_function(
    n = 200, add_background = FALSE, background_type = "Sine Wave",
    sd1 = 0.1, sd2 = 0.2, crosscor_noise = 0, mean1 = 5, mean2 = 10,
    n_spikes_corr = 0, n_spikes_s1 = 0, n_spikes_s2 = 0, max_spike_length = 5,
    n_drifts_s1 = 0, n_drifts_s2 = 0, drift_duration = c(10,20), drift_slope = c(-0.05,0.05),
    delayed_sensor = "None", alpha_ema = 0.3
  )
  expect_true(all(out$Measurand1 == 5))
  expect_true(all(out$Measurand2 == 10))
  expect_equal(out$Diff, out$Sensor1 - out$Sensor2)
})

test_that("with background: measurands = bg + mean", {
  set.seed(2)
  n <- 300
  out <- generate_data_function(
    n = n, add_background = TRUE, background_type = "Sine Wave",
    sine_amplitude = 2, sine_period = 20,
    sd1 = 0.1, sd2 = 0.1, crosscor_noise = 0, mean1 = 0.5, mean2 = -0.5,
    n_spikes_corr = 0, n_spikes_s1 = 0, n_spikes_s2 = 0, max_spike_length = 5,
    n_drifts_s1 = 0, n_drifts_s2 = 0, drift_duration = c(10,20), drift_slope = c(-0.05,0.05),
    delayed_sensor = "None", alpha_ema = 0.3
  )
  bg <- generate_background_function(
    n = n, background_type = "Sine Wave",
    sine_amplitude = 2, sine_period = 20
  )
  expect_equal(out$Measurand1, bg$sensor1 + 0.5, tolerance = 1e-8)
  expect_equal(out$Measurand2, bg$sensor2 - 0.5, tolerance = 1e-8)
})

test_that("uncorrelated spikes/drifts produce proper labels", {
  set.seed(3)
  out <- generate_data_function(
    n = 400, add_background = TRUE, background_type = "Random Walk",
    background_rho_rw = 0, randomwalk_sd = 0.1,
    sd1 = 0.2, sd2 = 0.2, crosscor_noise = 0, mean1 = 1, mean2 = 1,
    n_spikes_corr = 0, n_spikes_s1 = 3, n_spikes_s2 = 2, max_spike_length = 6,
    n_drifts_s1 = 1, n_drifts_s2 = 1, drift_duration = c(30,40), drift_slope = c(0.05,0.1),
    delayed_sensor = "None", alpha_ema = 0.5
  )
  expect_true(any(out$AnomalyFlag1 %in% c("Spike","Both")))
  expect_true(any(out$AnomalyFlag2 %in% c("Spike","Both")))
  expect_true(any(out$AnomalyFlag1 == "Drift"))
  expect_true(any(out$AnomalyFlag2 == "Drift"))
})

test_that("correlated spikes mark both sensors as SpikeCorr with aligned windows", {
  set.seed(4)
  out <- generate_data_function(
    n = 300, add_background = TRUE, background_type = "AR(1) Process",
    background_phi = 0.8, background_rho = 1,
    sd1 = 0.2, sd2 = 0.2, crosscor_noise = 0, mean1 = 2, mean2 = 2,
    n_spikes_corr = 2, n_spikes_s1 = 0, n_spikes_s2 = 0, max_spike_length = 5,
    n_drifts_s1 = 0, n_drifts_s2 = 0, drift_duration = c(10,20), drift_slope = c(-0.05,0.05),
    delayed_sensor = "None", alpha_ema = 0.4
  )
  idx1 <- which(out$AnomalyFlag1 == "SpikeCorr")
  idx2 <- which(out$AnomalyFlag2 == "SpikeCorr")
  expect_equal(idx1, idx2)
  expect_gt(abs(cor(diff(out$Sensor1[idx1]), diff(out$Sensor2[idx1]))), 0.5)
})

test_that("EMA on Sensor1 reduces variance (heuristic)", {
  set.seed(5)
  out <- generate_data_function(
    n = 200, add_background = TRUE, background_type = "Sine Wave",
    sine_amplitude = 1.5, sine_period = 30,
    sd1 = 0.05, sd2 = 0.05, crosscor_noise = 0, mean1 = 0, mean2 = 0,
    n_spikes_corr = 0, n_spikes_s1 = 0, n_spikes_s2 = 0, max_spike_length = 5,
    n_drifts_s1 = 0, n_drifts_s2 = 0, drift_duration = c(10,20), drift_slope = c(-0.05,0.05),
    delayed_sensor = "Sensor1", alpha_ema = 0.2
  )
  expect_lt(var(out$Sensor1), var(out$Sensor2) + 1e-8)
})
