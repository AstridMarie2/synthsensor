test_that("Poisson Moving Average returns identical series of length n (>=100)", {
  set.seed(1)
  input <- list(
    n = 80L,  # should clamp to 100
    background_type = "Poisson Moving Average",
    poisson_k = 5L,
    poisson_lambda = 3
  )
  out <- generate_background_from_input(input)  
  expect_type(out, "list")
  expect_true(all(c("sensor1","sensor2") %in% names(out)))
  expect_equal(length(out$sensor1), 100L)
  expect_equal(length(out$sensor2), 100L)
  expect_equal(out$sensor1, out$sensor2)
  expect_true(all(out$sensor1 >= 0))  # PMA of Poisson should be non-negative
})

test_that("Random Walk returns two cumulative sums with correlated increments", {
  set.seed(2)
  input <- list(
    n = 300L,
    background_type = "Random Walk",
    background_rho_rw = 0.7,
    randomwalk_sd = 0.5
  )
  out <- generate_background_from_input(input)
  expect_equal(length(out$sensor1), 300L)
  # Check correlation of increments (diff) rather than levels
  inc1 <- diff(out$sensor1)
  inc2 <- diff(out$sensor2)
  expect_true(is.finite(cor(inc1, inc2)))
  expect_gt(cor(inc1, inc2), 0.5)    # close to 0.7, allow slack
})

test_that("Sine Wave has expected amplitude and repeats approximately after period", {
  input <- list(
    n = 200L,
    background_type = "Sine Wave",
    sine_amplitude = 2,
    sine_period = 20L
  )
  out <- generate_background_from_input(input)
  y <- out$sensor1
  expect_equal(max(abs(y)), 2, tolerance = 0.05)
  # value at t and t+period should be close
  expect_equal(y[1:180], y[1:180 + 20], tolerance = 1e-6)
  # and the two sensors are identical
  expect_equal(out$sensor1, out$sensor2)
})

test_that("AR(1) residuals show the targeted innovation correlation", {
  set.seed(3)
  phi <- 0.8
  rho <- 0.4
  input <- list(
    n = 1000L,
    background_type = "AR(1) Process",
    background_phi = phi,
    background_rho = rho
  )
  out <- generate_background_from_input(input)
  s1 <- out$sensor1; s2 <- out$sensor2
  # Reconstruct innovations: e_t = x_t - phi * x_{t-1}
  e1 <- s1[-1] - phi * s1[-length(s1)]
  e2 <- s2[-1] - phi * s2[-length(s2)]
  expect_true(is.finite(cor(e1, e2)))
  expect_gt(cor(e1, e2), rho - 0.1)
  expect_lt(cor(e1, e2), rho + 0.1)
  # AR(1) autocorrelation at lag 1 should be close to phi
  ac1 <- stats::acf(s1, plot = FALSE, lag.max = 1)$acf[2]
  expect_equal(as.numeric(ac1), phi, tolerance = 0.1)
})

test_that("Input validation catches obvious issues when enabled", {
  # These expect_error() checks pass if you add stopifnot guards (see refactor above)
  expect_error(
    generate_background_from_input(list(
      n = 200L, background_type = "Poisson Moving Average",
      poisson_k = 0L, poisson_lambda = 3
    )),
    regexp = "poisson_k"
  )
  expect_error(
    generate_background_from_input(list(
      n = 200L, background_type = "Random Walk",
      background_rho_rw = 1.5, randomwalk_sd = 0.5
    )),
    regexp = "rho"
  )
  expect_error(
    generate_background_from_input(list(
      n = 200L, background_type = "AR(1) Process",
      background_phi = 1.2, background_rho = 0.3
    )),
    regexp = "phi"
  )
})
