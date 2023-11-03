library(tidyverse)
library(shiny)
library(bslib)

thematic::thematic_shiny()

shinyApp(
  ui = fluidPage(
    theme = bs_theme(version = 5, preset = "darkly"),
    title = "Beta-binomial app",
    titlePanel("Beta-binomial app"),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        h4("Data"),
        sliderInput("n", "# of flips", min = 1, max = 100, value = 10),
        sliderInput("x", "# of heads", min = 1, max = 100, value = 7),
        h4("Prior"),
        numericInput("a", "# of prior heads", value=5),
        numericInput("b", "# of prior tails", value=5),
        h4("Options"),
        checkboxInput("options", "Show options", value=FALSE),
        conditionalPanel(
          "input.options == true",
          checkboxInput("bw", "Use theme_bw()", value=FALSE),
          checkboxInput("facet", "Use facets", value=FALSE)
        )
      ),
      mainPanel = mainPanel(
        plotOutput("plot"),
        tableOutput("table")
      )
    )
  ),
  server = function(input, output, session) {
    bs_themer()
    
    observe({
      updateSliderInput(session, "x", max = input$n)
    }) |>
      bindEvent(input$n)
    
    d = reactive({
      req(input$a)
      validate(
        need(input$a > 0, "# of prior heads needs to be > 0"),
        need(input$b > 0, "# of prior tails needs to be > 0"),
        need(input$b, "# of prior tails needs to be defined")
      )
    
      tibble(
        p=seq(0, 1, length.out=101)
      ) |>
        mutate(
          prior = dbeta(p, input$a, input$b),
          likelihood = dbinom(input$x, size=input$n, prob = p) %>%
            {. / (sum(.) / n())},
          posterior = dbeta(p, input$a + input$x, input$b + input$n - input$x)
        ) |>
        pivot_longer(
          cols = -p,
          names_to = "distribution",
          values_to = "density"
        ) |>
        mutate(
          distribution = as_factor(distribution)
        )
    })

    output$table = renderTable({
      d() |>
        summarize(
          mean = sum(p*density) / n(),
          median = p[(cumsum(density/n()) >= 0.5)][1],
          q025 = p[(cumsum(density/n()) >= 0.025)][1],
          q975 = p[(cumsum(density/n()) >= 0.975)][1],
          .by = distribution
        )
      
    })
    
    output$plot = renderPlot({
      gg = ggplot(d(), aes(x=p, y=density, color=distribution)) +
        geom_line(linewidth=1.5) +
        geom_ribbon(aes(ymax=density, fill=distribution), ymin=0, alpha=0.5)
      
      if (input$bw) 
        gg = gg + theme_bw()
      
      if (input$facet)
        gg = gg + facet_wrap(~distribution)
      
      gg
    })
  }
)