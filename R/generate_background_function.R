#' Generate background signals for two sensors
#'
#' @param n Integer length (min 100; clamped).
#' @param background_type Character. One of
#'   "Poisson Moving Average", "Random Walk", "Sine Wave", "AR(1) Process".
#' @param poisson_k Integer >= 1. Window length for the moving average.
#' @param poisson_lambda Numeric > 0. Poisson mean.
#' @param randomwalk_sd Numeric > 0. SD of RW increments.
#' @param background_rho_rw Numeric in [-1, 1]. Correlation between RW increments.
#' @param sine_amplitude Numeric. Amplitude of the sine wave.
#' @param sine_period Integer > 1. Period (in samples).
#' @param background_phi Numeric in (-1, 1). AR(1) coefficient.
#' @param background_rho Numeric in [-1, 1]. Correlation of AR(1) innovations across sensors.
#' @return A list with numeric vectors `sensor1` and `sensor2`, each of length `n`.
#' @export

generate_background_function <- function(
    n,
    background_type,
    poisson_k = NULL,
    poisson_lambda = NULL,
    randomwalk_sd = NULL,
    background_rho_rw = NULL,
    sine_amplitude = NULL,
    sine_period = NULL,
    background_phi = NULL,
    background_rho = NULL
) {
  n <- max(as.integer(n), 100L)

  if (background_type == "Poisson Moving Average") {
    stopifnot(!is.null(poisson_k), poisson_k >= 1, poisson_k == as.integer(poisson_k))
    stopifnot(!is.null(poisson_lambda), poisson_lambda > 0)
    moving <- rpois(n + poisson_k, lambda = poisson_lambda)
    ma <- zoo::rollmean(moving, k = poisson_k, fill = NA)
    ma <- ma[!is.na(ma)]
    ma <- head(ma, n)
    return(list(sensor1 = ma, sensor2 = ma))

  } else if (background_type == "Random Walk") {
    stopifnot(!is.null(background_rho_rw), background_rho_rw >= -1, background_rho_rw <= 1)
    stopifnot(!is.null(randomwalk_sd), randomwalk_sd > 0)
    cov_matrix <- matrix(c(1, background_rho_rw, background_rho_rw, 1), nrow = 2)
    steps <- MASS::mvrnorm(n, mu = c(0, 0), Sigma = cov_matrix) * randomwalk_sd
    sensor1 <- cumsum(steps[, 1])
    sensor2 <- cumsum(steps[, 2])
    return(list(sensor1 = sensor1, sensor2 = sensor2))

  } else if (background_type == "Sine Wave") {
    stopifnot(!is.null(sine_period), sine_period > 1, sine_period == as.integer(sine_period))
    stopifnot(!is.null(sine_amplitude))
    x <- seq_len(n)
    sine_wave <- sine_amplitude * sin(2 * pi * x / sine_period)
    return(list(sensor1 = sine_wave, sensor2 = sine_wave))

  } else if (background_type == "AR(1) Process") {
    stopifnot(!is.null(background_phi), abs(background_phi) < 1)
    stopifnot(!is.null(background_rho), background_rho >= -1, background_rho <= 1)
    cov_matrix <- matrix(c(1, background_rho, background_rho, 1), nrow = 2)
    innovations <- MASS::mvrnorm(n, mu = c(0, 0), Sigma = cov_matrix)
    sensor1 <- numeric(n); sensor2 <- numeric(n)
    sensor1[1] <- rnorm(1, 0, 1 / sqrt(1 - background_phi^2))
    sensor2[1] <- rnorm(1, 0, 1 / sqrt(1 - background_phi^2))
    for (t in 2:n) {
      sensor1[t] <- background_phi * sensor1[t - 1] + innovations[t, 1]
      sensor2[t] <- background_phi * sensor2[t - 1] + innovations[t, 2]
    }
    return(list(sensor1 = sensor1, sensor2 = sensor2))
  }

  stop("Unknown background_type")
}



