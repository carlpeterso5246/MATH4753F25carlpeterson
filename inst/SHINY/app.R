#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Load required packages
library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Maximum Likelihood Estimation for Univariate Distributions"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Choose a distribution:",
                  choices = c("Normal", "Exponential", "Poisson", "Binomial", "Uniform")),
      
      numericInput("n", "Sample size:", 50, min = 5, max = 1000),
      
      conditionalPanel(
        condition = "input.dist == 'Normal'",
        numericInput("mean", "True mean (μ):", 0),
        numericInput("sd", "True sd (σ):", 1, min = 0.1)
      ),
      
      conditionalPanel(
        condition = "input.dist == 'Exponential'",
        numericInput("rate", "True rate (λ):", 1, min = 0.01)
      ),
      
      conditionalPanel(
        condition = "input.dist == 'Poisson'",
        numericInput("lambda", "True λ:", 3, min = 0.1)
      ),
      
      conditionalPanel(
        condition = "input.dist == 'Binomial'",
        numericInput("size", "Number of trials (n):", 10, min = 1),
        numericInput("prob", "Success probability (p):", 0.5, min = 0, max = 1)
      ),
      
      conditionalPanel(
        condition = "input.dist == 'Uniform'",
        numericInput("minU", "Minimum (a):", 0),
        numericInput("maxU", "Maximum (b):", 1)
      ),
      
      actionButton("generate", "Generate Sample")
    ),
    
    mainPanel(
      plotOutput("likelihoodPlot"),
      verbatimTextOutput("mleText")
    )
  )
)

server <- function(input, output) {
  sample_data <- eventReactive(input$generate, {
    switch(input$dist,
           "Normal" = rnorm(input$n, input$mean, input$sd),
           "Exponential" = rexp(input$n, input$rate),
           "Poisson" = rpois(input$n, input$lambda),
           "Binomial" = rbinom(input$n, input$size, input$prob),
           "Uniform" = runif(input$n, input$minU, input$maxU))
  })
  
  output$likelihoodPlot <- renderPlot({
    x <- sample_data()
    if (is.null(x)) return(NULL)
    
    df <- data.frame(x = x)
    
    # Compute likelihoods for different parameters
    switch(input$dist,
           "Normal" = {
             mu_vals <- seq(min(x) - 2, max(x) + 2, length.out = 200)
             lik <- sapply(mu_vals, function(mu)
               sum(dnorm(x, mean = mu, sd = sd(x), log = TRUE)))
             mle <- mu_vals[which.max(lik)]
             ggplot(data.frame(mu = mu_vals, logL = lik),
                    aes(mu, logL)) +
               geom_line(color = "blue") +
               geom_vline(xintercept = mle, color = "red", linetype = "dashed") +
               labs(title = "Log-Likelihood for Normal Mean (σ fixed)",
                    x = "μ", y = "Log-Likelihood")
           },
           "Exponential" = {
             rate_vals <- seq(0.01, 5, length.out = 200)
             lik <- sapply(rate_vals, function(r)
               sum(dexp(x, rate = r, log = TRUE)))
             mle <- rate_vals[which.max(lik)]
             ggplot(data.frame(rate = rate_vals, logL = lik),
                    aes(rate, logL)) +
               geom_line(color = "blue") +
               geom_vline(xintercept = mle, color = "red", linetype = "dashed") +
               labs(title = "Log-Likelihood for Exponential Rate λ",
                    x = "λ", y = "Log-Likelihood")
           },
           "Poisson" = {
             lambda_vals <- seq(0.1, max(x) + 5, length.out = 200)
             lik <- sapply(lambda_vals, function(l)
               sum(dpois(x, lambda = l, log = TRUE)))
             mle <- lambda_vals[which.max(lik)]
             ggplot(data.frame(lambda = lambda_vals, logL = lik),
                    aes(lambda, logL)) +
               geom_line(color = "blue") +
               geom_vline(xintercept = mle, color = "red", linetype = "dashed") +
               labs(title = "Log-Likelihood for Poisson λ",
                    x = "λ", y = "Log-Likelihood")
           },
           "Binomial" = {
             p_vals <- seq(0, 1, length.out = 200)
             lik <- sapply(p_vals, function(p)
               sum(dbinom(x, size = input$size, prob = p, log = TRUE)))
             mle <- p_vals[which.max(lik)]
             ggplot(data.frame(p = p_vals, logL = lik),
                    aes(p, logL)) +
               geom_line(color = "blue") +
               geom_vline(xintercept = mle, color = "red", linetype = "dashed") +
               labs(title = "Log-Likelihood for Binomial p",
                    x = "p", y = "Log-Likelihood")
           },
           "Uniform" = {
             a_vals <- seq(min(x) - 1, min(x) + 1, length.out = 200)
             lik <- sapply(a_vals, function(a)
               sum(dunif(x, min = a, max = max(x), log = TRUE)))
             mle <- min(x)
             ggplot(data.frame(a = a_vals, logL = lik),
                    aes(a, logL)) +
               geom_line(color = "blue") +
               geom_vline(xintercept = mle, color = "red", linetype = "dashed") +
               labs(title = "Log-Likelihood for Uniform Min (b fixed)",
                    x = "a", y = "Log-Likelihood")
           })
  })
  
  output$mleText <- renderPrint({
    x <- sample_data()
    switch(input$dist,
           "Normal" = cat("MLE for μ =", mean(x), "\nMLE for σ =", sd(x)),
           "Exponential" = cat("MLE for λ =", 1 / mean(x)),
           "Poisson" = cat("MLE for λ =", mean(x)),
           "Binomial" = cat("MLE for p =", mean(x) / input$size),
           "Uniform" = cat("MLE for a =", min(x), "and b =", max(x)))
  })
}

shinyApp(ui, server)
