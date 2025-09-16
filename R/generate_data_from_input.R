# Wrapper so Shiny server can call with only "input" as argument:
generate_data_from_input <- function(input) {
  generate_data_function(
    n = input$n,
    add_background = input$add_background,
    background_type = input$background_type,
    poisson_k = input$poisson_k,
    poisson_lambda = input$poisson_lambda,
    background_rho_rw = input$background_rho_rw,
    randomwalk_sd = input$randomwalk_sd,
    sine_amplitude = input$sine_amplitude,
    sine_period = input$sine_period,
    background_phi = input$background_phi,
    background_rho = input$background_rho,
    delayed_sensor = input$delayed_sensor,
    alpha_ema = input$alpha_ema,
    sd1 = input$sd1, sd2 = input$sd2,
    crosscor_noise = input$crosscor_noise,
    mean1 = input$mean1, mean2 = input$mean2,
    n_spikes_corr = input$n_spikes_corr,
    n_spikes_s1 = input$n_spikes_s1,
    n_spikes_s2 = input$n_spikes_s2,
    max_spike_length = input$max_spike_length,
    spike_size = input$spike_size,
    n_drifts_s1 = input$n_drifts_s1,
    n_drifts_s2 = input$n_drifts_s2,
    drift_duration = input$drift_duration,
    drift_slope = input$drift_slope
  )
}
