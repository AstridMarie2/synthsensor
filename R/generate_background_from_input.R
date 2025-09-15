# Wrapper so Shiny server can call with only "input" as argument:
generate_background_from_input <- function(input) {
  generate_background_function(
    n = input$n,
    background_type = input$background_type,
    poisson_k = input$poisson_k,
    poisson_lambda = input$poisson_lambda,
    randomwalk_sd = input$randomwalk_sd,
    background_rho_rw = input$background_rho_rw,
    sine_amplitude = input$sine_amplitude,
    sine_period = input$sine_period,
    background_phi = input$background_phi,
    background_rho = input$background_rho
  )
}