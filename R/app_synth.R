library(shiny)
library(MASS)
library(ggplot2)
library(zoo)

# Rev 15.09.25: Made UI more robust. Saved functions separately. Optimized for testthat and JOSS submission
# Rev 11.08.25: Delay and attenuation included
# Rev 08.08.25: Changed slightly wording in UI for crosscor_noise.
# Rev 28.04.25: Ready for deployment
# Rev 25.04.25: Separate tab for selecting and visualizing different background processes.

#' @export
app_synth <- function() {
ui <- navbarPage("Synthetic Sensor Data Generator",
  
                 tabPanel("Background Process",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("background_type", "Choose Background Variation Type:",
                                          choices = c("AR(1) Process", "Poisson Moving Average", "Random Walk", "Sine Wave")),
                              conditionalPanel(
                                condition = "input.background_type == 'AR(1) Process'",
                                sliderInput("background_phi", "Background AR(1) Autocorrelation (ϕ)", min = 0, max = 0.99, value = 0.97, step = 0.01),
                                sliderInput("background_rho", "Background Cross-Sensor Correlation (ρ)", min = -1, max = 1, value = 0.99, step = 0.01)
                              ),
                              conditionalPanel(
                                condition = "input.background_type == 'Poisson Moving Average'",
                                p("The Poisson moving Average has a weak temporal autocorrelation via the smoothing process. There is a strong cross-sensor correlation, unless the standard deviations of the sensors is set high."),
                                numericInput("poisson_lambda", "Poisson Noise Level (λ)", 5, min = 0.1),
                                numericInput("poisson_k", "Moving Average Window (k)", 50, min = 1)
                              ),
                              conditionalPanel(
                                condition = "input.background_type == 'Random Walk'",
                                p("The Random Walk has a strong temporal autocorrelation through the cumulative sum. There is cross-sensor correlation through correlated innovations (step changes)."),
                                numericInput("randomwalk_sd", "Random Walk Step Size (σ)", 0.5, min = 0.01),
                                sliderInput("background_rho_rw", "Background Cross-Sensor Correlation (ρ)", min = -1, max = 1, value = 0.85, step = 0.01)
                              ),
                              conditionalPanel(
                                condition = "input.background_type == 'Sine Wave'",
                                numericInput("sine_amplitude", "Sine Amplitude", 2),
                                numericInput("sine_period", "Sine Period (points)", 200)
                              ),
                              actionButton("generate_background", "Generate Background Preview")
                            ),
                            mainPanel(
                              plotOutput("backgroundPlot"),
                              p("(C) Astrid Marie Skålvik | Revision date 11.08.2025 | . All rights reserved. The author is not responsible for any errors or omissions, or for the results obtained from the use of this information."),
                            ) 
                          ) 
  ), 
  tabPanel("Add sensor errors",
           sidebarLayout(
             sidebarPanel(
               numericInput("n", "Number of Observations (minimum 100)", 1000, min=100),
               checkboxInput("add_background", "Add Background Signal", value = TRUE),
               hr(),
               h4("Sensor delay and attenuation"),
               fluidRow(column(6, selectInput("delayed_sensor", "Choose delayed and attenuated sensor:",choices = c("None","Sensor1", "Sensor2"))),
                        column(6, numericInput("alpha_ema", "Weighing factor Exponential Moving Average", 0.5))),
               hr(),
               h4("Sensor noise and correlation in noise"),
               fluidRow(column(6, numericInput("mean1", "Mean Sensor 1", 10)),
                        column(6, numericInput("sd1", "SD Sensor 1", 0.1))),
               fluidRow(column(6, numericInput("mean2", "Mean Sensor 2", 10)),
                        column(6, numericInput("sd2", "SD Sensor 2", 0.1))),
               sliderInput("crosscor_noise", "Correlation in sensor noise", min = -0.99, max = 0.99, value = 0, step=0.01),
               
               hr(),
               h4("Spike Injection"),
               fluidRow(column(4, numericInput("n_spikes_corr", "Correlated Spikes (Both sensors)", 3)),
                        column(4, numericInput("n_spikes_s1", "Uncorrelated Spikes (Sensor 1)", 2)),
                        column(4, numericInput("n_spikes_s2", "Uncorrelated Spikes (Sensor 2)", 2))),
               fluidRow(column(6, numericInput("max_spike_length", "Max Spike Duration (in time steps)", 6, min = 1)),
                        column(6, numericInput("spike_size", "Max Spike Magnitude (frac. of mean)", 1))),
               hr(),
               h4("Drift Injection"),
               fluidRow(column(6, numericInput("n_drifts_s1", "Uncorrelated Drifts (Sensor 1)", 1)),
                        column(6, numericInput("n_drifts_s2", "Uncorrelated Drifts (Sensor 2)", 1))),
               
               sliderInput("drift_duration", "Drift Duration Range", min = 1, max = 200, value = c(50, 150)),
               sliderInput("drift_slope", "Drift Slope Range", min = -10, max = 10, value = c(0.2, 1)) 
             ), 
             
             mainPanel(
               p("This app allows users to generate realistic synthetic time series data for two correlated sensors. Users can control the mean, standard deviation, and correlation of the base signals, and inject different types of anomalies to simulate fault conditions or data integrity issues."),
               actionButton("generate", "Generate Data"),
               plotOutput("combinedPlot", height = "1200px"),
               downloadButton("downloadData", "Download CSV"),
               p("(C) Astrid Marie Skålvik | Revision date 11.09.2025 | . All rights reserved. The author is not responsible for any errors or omissions, or for the results obtained from the use of this information."),
             ) 
             
           ) 
  
  ) 
  
) 


server <- function(input, output) {
   # ---- BACKGROUND PREVIEW ----
  
  background_preview <- eventReactive(input$generate_background, {
    generate_background_from_input(input)
  }, ignoreInit = TRUE)
  
  output$backgroundPlot <- renderPlot({
    req(background_preview())
    bg <- background_preview()
    plot(bg$sensor1, type = "l", main = "Background signals")
    lines(bg$sensor2)
  })
  
    # ---- GENERATE SENSOR DATA ----
  
  generate_data <- eventReactive(input$generate, {
      generate_data_from_input(input)
  })
  
  # ---- PLOTS ----
  output$backgroundPlot <- renderPlot({
    bg <- generate_background_from_input(input)
    plot(bg$sensor1, type = "l", col = "blue", lwd = 2, ylim = range(c(bg$sensor1, bg$sensor2)),
         main = paste("Background Process:", input$background_type),
         ylab = "Variation", xlab = "Time Steps")
    lines(bg$sensor2, col = "red", lwd = 2)
    legend("topright", legend = c("Sensor 1", "Sensor 2"), col = c("blue", "red"), lty = 1)
  })
  
  
  output$combinedPlot <- renderPlot({
    df <- generate_data()
    
    # Sensor plot
    plotsensor <- ggplot(df, aes(x = Date)) +
      geom_line(aes(y = Sensor1, color = "Sensor1")) +
      geom_line(aes(y = Sensor2, color = "Sensor2")) +
      geom_line(aes(y = Diff, color = "Diff")) +
      labs(title = "Synthetic Sensor Data", y = "Measurement", color = "") +
      scale_color_manual(values = c("Diff" = "darkgreen", "Sensor1" = "blue", "Sensor2" = "black")) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    # Anomaly plot for Sensor 1
    df$AnomalyType1 <- factor(df$AnomalyFlag1, levels = 0:4,
                              labels = c("Normal", "Drift", "Spike", "Both", "Correlated spike"))
    runs1 <- rle(as.character(df$AnomalyType1))
    ends1 <- cumsum(runs1$lengths)
    starts1 <- c(1, head(ends1, -1) + 1)
    anomaly_bands1 <- data.frame(
      start = df$Date[starts1],
      end = df$Date[ends1],
      type = runs1$values
    )
    anomaly_bands1 <- anomaly_bands1[anomaly_bands1$type != "Normal", ]
    
    plotanomaly1 <- ggplot(df, aes(x = Date, y = Sensor1)) +
      geom_line(color = "blue") +
      geom_rect(data = anomaly_bands1, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = type),
                inherit.aes = FALSE, alpha = 0.8) +
      scale_fill_manual(values = c("Drift" = "orange", "Spike" = "red", "Correlated spike" = "darkgreen", "Both" = "purple")) +
      labs(title = "Sensor 1 Anomalies", y = "Sensor 1 Value", fill = "Anomaly Type") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    # Anomaly plot for Sensor 2
    df$AnomalyType2 <- factor(df$AnomalyFlag2, levels = 0:4,
                              labels = c("Normal", "Drift", "Spike", "Both", "Correlated spike"))
    runs2 <- rle(as.character(df$AnomalyType2))
    ends2 <- cumsum(runs2$lengths)
    starts2 <- c(1, head(ends2, -1) + 1)
    anomaly_bands2 <- data.frame(
      start = df$Date[starts2],
      end = df$Date[ends2],
      type = runs2$values
    )
    anomaly_bands2 <- anomaly_bands2[anomaly_bands2$type != "Normal", ]
    
    plotanomaly2 <- ggplot(df, aes(x = Date, y = Sensor2)) +
      geom_line(color = "blue") +
      geom_rect(data = anomaly_bands2, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = type),
                inherit.aes = FALSE, alpha = 0.8) +
      scale_fill_manual(values = c("Drift" = "orange", "Spike" = "red", "Correlated spike" = "darkgreen", "Both" = "purple")) +
      labs(title = "Sensor 2 Anomalies", y = "Sensor 2 Value", fill = "Anomaly Type") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    # Combine
    gridExtra::grid.arrange(plotsensor, plotanomaly1, plotanomaly2, ncol = 1, heights = c(2, 1, 1))
  })
  
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste0("synthetic_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(generate_data_from_input(input), file, row.names = FALSE)
    }
  )
}

shiny::shinyApp(ui, server)

}

