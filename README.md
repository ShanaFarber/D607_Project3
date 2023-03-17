# D607_Project3

This github repository is a compilation of the work completed to determine today's most valued data science skills. This work is a collaboration of effort by Waheeb Algabri, Keith Colella, John Cruz, Shoshana Farber, and Kayleah Griffen.

--------------------------------------------------------------------------------------------------------------------------

The work completed to explore today's most valued data science skills can be summarized in five main steps.

1) Collect the data

The data used to conduct this analysis was collected from four jobs boards. Further description of why these jobs boards were chosen (based on respecting robots.txt files and legality) can be found in “./reports/spring23_data607_proj3A.pdf”. After the jobs boards were selected three of them required web-scraping and one could be downloaded directly. The web-scraping was completed in python using jupyter notebooks, these files are titled “1_[data source].ipynb. After the web-scraping and downloading was completed, the output csv files were saved in the “data” folder of our github. 

2) Clean the data and create a skills dictionary

The next step in the process was to clean the data and create a skills dictionary, this was done in “2_create_clean_data_frames.Rmd”. In this step the data from each job board was loaded from the github data folder and synthesized into two dataframes, “job_listings” for all of the job postings and “skills” for the skills dictionary. The output of this step is the two dataframes saved in csv files in a local output directory.

3) Extract the skills from the job postings

At this stage, “3_match_jobs_skills.Rmd” was created to load the output files from step 2 and then create a years of experience column, adjust salary into the same currency, and extract the skills. Our database design is further described in “./reports/spring23_data607_proj3A.pdf”. The dataframes from this step were written to a csv file in a local output directory.

4) Determine the most valued data science skills

Finally, the data saved in the output directory can be read in and explored to answer the question of the most valued data science skills. This analysis was performed in “4_analysis.Rmd”.

5) Create models based on the data

To further the work two models were created in “5_modeling.Rmd” with the output being two shiny applications. One application was designed to predict a salary a candidate could expect based on a candidate's qualifications. The other application designed was a recommender engine to take a candidate's qualifications and recommend jobs that they could apply for that best fit their qualifications. 

--------------------------------------------------------------------------------------------------------------------------

In summary, substantive work was done to answer the question “what are today's most valued data science skills?”. We worked together collaboratively using slack, zoom, github, and google drive to facilitate our teamwork. This work included writing code in python for data collecting and in R for data tidying, transforming, exploring, analyzing and modeling. 
