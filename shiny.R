library(shiny)
library(ggplot2)  
library(rjson)
library(statebins)


states <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", 
            "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "M1", "M2", 
            "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "N1", "N2", "N3", 
            "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", 
            "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", 
            "WV", "WY")

ui <- pageWithSidebar(
  headerPanel('Explore state-level correlations in the 538 model'),
  sidebarPanel(
    selectInput('state_selection', 'Assume Trump victory in...', states),
    p("This tool lets you assume a Trump victory in a given state and see how this conditioning affects the 538 model's predicted probability of a Trump victory in other states."),
    p("The 538 presidential forecast model tries to prevent overconfidence by adding uncertainty to the forecast in several ways. Nate Silver has talked about the use of t distributions instead of normal distributions on his podcast, and there are potentially other ways that uncertatiny is added to the model, though the model itself is closed source."),
    p("Andy Gelman has pointed out that the way this uncertainty is added produces some strange results. Specifically, some states have negative correlation, where a surprising Trump victory in one state makes Biden more likely to win in others."),
    p(" Preventing overconfidence is important, and conservative miscalibration is perhaps better than overconfident miscalibration. That said, exploring where the 538 model starts to break down is an important part of understanding the model and building better ones in the future."),
    p("Links:"),
    a("Gelman's blog post", href="https://statmodeling.stat.columbia.edu/2020/10/24/reverse-engineering-the-problematic-tail-behavior-of-the-fivethirtyeight-presidential-election-forecast/"),
    br(),
    a("Shiny source code", href="https://github.com/ahalterman/538_state_corr"),
    br(),
    p("Note: the Nebraska and Maine districts aren't accounted for correctly. Help is welcome on this.")
  ),
  mainPanel(
    plotOutput('plot1'),
    plotOutput('plot2')
  )
)

server <- function(input, output, session) {
  
  # this code is taken directly from https://statmodeling.stat.columbia.edu/2020/10/24/reverse-engineering-the-problematic-tail-behavior-of-the-fivethirtyeight-presidential-election-forecast/
  sims_538 <- fromJSON(file="simmed-maps.json")
  states <- sims_538$states
  n_sims <- length(sims_538$maps)
  sims <- array(NA, c(n_sims, 59), dimnames=list(NULL, c("", "Trump", "Biden", states)))
  for (i in 1:n_sims){
    sims[i,] <- sims_538$maps[[i]]
  }
  state_sims <- sims[,4:59]
  trump_share <- (state_sims + 1)/2
  biden_wins <- state_sims < 0
  trump_wins <- state_sims > 0
  
  # create a data frame of Trump's unconditional state probabilities
  trump_base <- data.frame(percent = round(colMeans(trump_wins), 2),
                           state = dimnames(trump_wins)[[2]])
  
  
  selected_data <- reactive({
    # subset the data by the user selected state
    selected_state <- input$state_selection
    condition <- trump_wins[, selected_state]
    
    new_trump_pred <- data.frame(pct_trump = round(apply(trump_wins[condition,], 2, mean), 2),
                                 state = dimnames(trump_wins)[[2]])
    # set the selected state to NA for ggplot reasons
    new_trump_pred[new_trump_pred$state == selected_state, "pct_trump"] <- NA
    # calculate the change in predicted probability over the unconditional prediction
    new_trump_pred$change <- new_trump_pred$pct_trump - trump_base$percent
    return(new_trump_pred)
  })
  
  output$plot1 <- renderPlot({
    # make the conditional predicted probability map
    new_trump_pred = selected_data()
    
    ggplot(new_trump_pred, aes(state = state, fill = pct_trump)) +
      geom_statebins(text_color = "white",
                     font_size = 3,
                     legend_title="Percent Trump") +
      scale_fill_continuous(low = "blue", high="red", na.value = "black") +
      theme_statebins(legend_position="right") +
      labs(title = "Conditional predicted probability of a Trump victory",
           subtitle = "Assuming a Trump victory in the black shaded state",
           fill = "Predicted\nprobability\nof Trump\nvictory")
  })
  
  output$plot2 <- renderPlot({
    # make the change in predicted probability map
    new_trump_pred = selected_data()
    
    ggplot(new_trump_pred, aes(state = state, fill = change)) +
      geom_statebins(text_color = "white",
                     font_size = 3,
                     legend_title="Percent Trump") +
      scale_fill_continuous(low = "blue", high="red", na.value = "black") +
      theme_statebins(legend_position="right") +
      labs(title = "Change in predicted probability of a Trump victory",
           subtitle = "Assuming a Trump victory in the black shaded state",
           fill = "Change in\npredicted\nprobability\nof Trump\nvictory")
  })
  
}

shinyApp(ui, server)