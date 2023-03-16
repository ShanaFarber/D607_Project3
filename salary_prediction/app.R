# Setup
library(shiny)
library(tidyverse)
library(randomForest)
# Read in model and ata
rf_model <- readRDS('salary_rf.rds')
jobs_skills_matrix <- read_csv('jobs_skills_matrix.csv')
# Set up capture df and input lists
df_cols <- colnames(jobs_skills_matrix)
degrees <- jobs_skills_matrix %>%
  select(starts_with('ed_')) %>% 
  colnames()
locations <- jobs_skills_matrix %>%
  select(starts_with('loc_')) %>%
  colnames()
skills <- jobs_skills_matrix %>%
  select(!starts_with('ed_') &
         !starts_with('loc_') &
         !contains('years') &
         !contains('salary')) %>%
  colnames()

# Define UI
ui <- fluidPage(

    # Application title
    titlePanel('Data Scientist Salary Prediction'),

    # Sidebar with inputs
    sidebarLayout(
        sidebarPanel(
          
            # Years of Experience
            sliderInput('experience',
                        'Years of Work Experience:',
                        min = 1,
                        max = 20,
                        value = 5),
            
            # Education
            radioButtons('ed',
                         'Highest Level of Education:',
                         choices = degrees
            ),
            
            # Location
            radioButtons('loc',
                         'Desired Location:',
                         choices = locations
            ),
            
            # Skills
            checkboxGroupInput('skills',
                               'Skills / Knowledge You Possess:',
                               choices = skills
            
            )
            
        ),

        # Show salary prediction
        mainPanel(
          textOutput('prediction')
        )
    )
)

# Define server logic
server <- function(input, output) {

    output$prediction <- renderPrint({
        
        # Create dataframe
        df <- data.frame(matrix(nrow = 1, ncol = 23))
        colnames(df) <- df_cols
        
        # Capture years of experience
        df['years_exp'] = input$experience
        
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
        result <- paste('Predicted salary: $',
                        predict(rf_model, df)[[1]] %>% scales::comma())
        return(result)
    
    })
}

# Run the application 
shinyApp(ui = ui, server = server)