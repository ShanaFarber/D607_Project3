# Setup
library(shiny)
library(tidyverse)
# Read in data and define knn function
jobs_skills_matrix <- read.csv('jobs_skills_matrix.csv',row.names = 1)
job_listings <- read.csv('job_listings.csv')
knn <- function(i, distance_matrix, k = 5) {
  neighbors <- data.frame(dist = distance_matrix[i,])
  k_nearest_ids <- arrange(neighbors, dist) %>% 
    slice(1:(k+1)) %>% 
    rownames()
  return(k_nearest_ids)
}
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
  titlePanel('Data Scientist Job Recommendations'),
    
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
    
    # textOutput('matches')
    DT::dataTableOutput('matches')
  
  )
  
)

# Define server logic
server <- function(input, output) {
  
  # output$matches <- renderPrint({
  output$matches <- DT::renderDataTable({
    
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
    
    # Use df with inputs to generate distances and identify k nearest neighbors
    new_matrix <- rbind(jobs_skills_matrix, df)
    new_distances <- as.matrix(dist(new_matrix, method="euclidean"))
    last_row_name <- rownames(tail(new_distances,1))
    match_ids <- knn(last_row_name, new_distances, 5)
    match_ids <- match_ids[match_ids != last_row_name]
    
    output_df <- job_listings[job_listings$job_id %in% match_ids,]
    output_df <- output_df %>% select(-job_description)
    # return(output_df)
    
    DT::datatable(output_df,
                  options = list(dom = 't'),
                  rownames = FALSE
    )
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)