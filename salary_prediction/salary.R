# Setup
library(shiny)
library(tidyverse)
library(randomForest)

# Read in model and ata
rf_model <- readRDS('salary_rf.rds')
jobs_skills_matrix <- read_csv('jobs_skills_matrix.csv')

# Set up capture df and input lists
df_cols <- colnames(jobs_skills_matrix)
df_cols_display <- df_cols %>%
  str_remove_all('ed_|loc_') %>%
  str_replace_all('_', ' ') %>%
  str_to_sentence()
degrees <- df_cols[str_which(df_cols, 'ed_')] %>%
  str_remove_all('ed_') %>% str_to_sentence()
locations <- df_cols[str_which(df_cols, 'loc_')] %>%
  str_remove_all('loc_') %>% str_to_sentence()
skills <- df_cols[str_which(
  df_cols,'^((?!ed_|loc_|years|salary|\\.).*)$')] %>%
  str_replace_all('_', ' ') %>%
  str_to_sentence()


# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel('Data Scientist Salary Prediction'),
  
  # Show salary prediction
  mainPanel(
    
    
    # Years of Experience
    sliderInput('experience',
                'Years of Work Experience:',
                min = 1,
                max = 20,
                value = 5
    ),
    
    # Education
    radioButtons('ed',
                 'Highest Level of Education:',
                 choices = degrees,
                 inline = TRUE
    ),
    
    # Location
    radioButtons('loc',
                 'Desired Location:',
                 choices = locations,
                 inline = TRUE
    ),
    
    # Skills
    checkboxGroupInput('skills',
                       'Skills / Knowledge You Possess:',
                       choices = skills,
                       inline = TRUE
    ),
    
    textOutput('prediction')
  )
)

# Define server logic
server <- function(input, output) {

    output$prediction <- renderPrint({
        
        # Create dataframe
        df <- data.frame(matrix(nrow = 1, ncol = length(df_cols)))
        colnames(df) <- df_cols_display
        
        # Capture years of experience
        df['Years exp'] = input$experience
        
        # Capture education
        for (degree in degrees) {
          if (degree %in% input$ed) {
            df[degree] = 1
          } else {df[degree] = 0}
        }
        
        # Capture location
        for (location in locations) {
          if (location %in% input$loc) {
            df[location] = 1
          } else {df[location] = 0}
        }
        
        # Capture skills
        for (skill in skills) {
          if (skill %in% input$skills) {
            df[skill] = 1
          } else {df[skill] = 0}
        }
        
        # Use df with inputs to predict salary and return
        colnames(df) <- df_cols
        result <- paste('Predicted salary: $',
                        (predict(rf_model, df)[[1]] - (sqrt(min(rf_model$mse)) / 2)) %>%
                          scales::comma(),
                        'to',
                        (predict(rf_model, df)[[1]] + (sqrt(min(rf_model$mse)) / 2)) %>%
                          scales::comma()
        )
        return(result)
    
    })
}

# Run the application 
shinyApp(ui = ui, server = server)