# Setup
library(shiny)
library(tidyverse)

# Read in data and define knn function
jobs_skills_matrix <- read.csv('jobs_skills_matrix.csv', row.names = 1)
job_listings <- read.csv('job_listings_skills_string.csv') %>%
  mutate(job_title = str_to_sentence(job_title))
knn <- function(i, distance_matrix, k = 5) {
  neighbors <- data.frame(dist = distance_matrix[i,])
  k_nearest_ids <- arrange(neighbors, dist) %>% 
    slice(1:(k+1)) %>% 
    rownames()
  return(k_nearest_ids)
}

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
  titlePanel('Data Scientist Job Recommendations'),
  
  # User input and resulting table of matches
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
    df <- data.frame(matrix(nrow = 1, ncol = length(df_cols)  ))
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
    
    # Use df with inputs to generate distances and identify k nearest neighbors
    colnames(df) <- df_cols
    new_matrix <- rbind(jobs_skills_matrix, df)
    new_distances <- as.matrix(dist(new_matrix, method="euclidean"))
    last_row_name <- rownames(tail(new_distances,1))
    match_ids <- knn(last_row_name, new_distances, 5)
    match_ids <- match_ids[match_ids != last_row_name]
    
    output_df <- job_listings[job_listings$job_id %in% match_ids,]
    output_df <- output_df %>% select(-highest_ed,
                                      -years_exp,
                                      -continent,
                                      -country,
                                      -salary_currency)
    
    DT::datatable(output_df,
                  options = list(dom = 't'),
                  rownames = FALSE
    )
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)