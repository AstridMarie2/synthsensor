

lagged_ema <- function(x, alpha) {
  if (alpha == 1) return(x)  # no lag/attenuation
  y <- numeric(length(x))
  y[1] <- x[1]
  for (t in 2:length(x)) {
    y[t] <- (1 - alpha) * y[t - 1] + alpha * x[t]
  }
  y
}
 
# Velg en "sikker" buffer gitt n og duration.
# Bruk ønsket buffer (default 100), men skaler ned hvis dataserien er liten.
safe_buffer <- function(n, duration, desired = 100) {
  # Minst 0, høyst halvparten av det som gjenstår etter å ha plassert duration
  max_buff <- max(0, floor((n - duration) / 2) - 1)
  min(desired, max_buff)
}

# Plukk ett sammenhengende segment [start, end] av gitt length/duration.
# Returnerer c(start, end) eller NULL om det ikke er mulig.
pick_segment <- function(n, duration, buffer = 100) {
  if (n <= 0 || duration <= 0 || n < duration) return(NULL)
  b <- safe_buffer(n, duration, buffer)
  left  <- max(1, b + 1)
  right <- n - duration - b + 1
  if (right < left) {
    # Faller tilbake til ingen buffer hvis det ikke er plass
    left  <- 1
    right <- n - duration + 1
    if (right < left) return(NULL)
  }
  start <- sample.int(right - left + 1, 1) + left - 1
  c(start, start + duration - 1)
}

# Plukk startindekser til spikes gitt maks spike-lengde.
# Returnerer integer(0) hvis det ikke er plass.
pick_spike_starts <- function(n, k, max_len, buffer = 100) {
  if (n <= 0 || max_len <= 0) return(integer(0))
  # Bruk konservativ "min varighet" = 1 for å finne starts
  b <- safe_buffer(n, 1, buffer)
  left  <- max(1, b + 1)
  right <- n - max_len - b
  if (right < left) {
    left  <- 1
    right <- n - max_len
    if (right < left) return(integer(0))
  }
  candidates <- seq.int(left, right)
  if (length(candidates) == 0) return(integer(0))
  sample(candidates, size = min(k, length(candidates)))
}




