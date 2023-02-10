# Jowhar-Health-Quality-Indicators
This is interface (dashboard) to a project that looked at health care quality indicators at Jowhar healthcare Somalia
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)

# Define UI for application that draws a histogram
library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)                   # This package helps with data preparation (edit, remove,mutate, etc)
library(shinyWidgets)             
library(shinydashboard)
library(div)
library(shinyjs)
library(shinyauthr)
library(DT)
library(rlang)                   # required package for Rmarkdown
library(r2d3)                    # D3 visualization
library(purrr)                   # requirement packages for Functional Programming Tools
library(stringr)                 # All functions deal with "NA"'s and zero length vectors
library(tidyverse)               # assists with data import, tidying, manipulation, and data visualization.
library(tidytext)
library(htmltools)
library(ggthemes)
library(esquisse)
library(desc)


#library(plyr)                    # The package is used for counting


CSS <- "

P{
 line-height: 1.6; 
  font-family: Helvetica;
  text-align: justify;
  margin: 0;
  font-size: 14px;


}


h1{

 line-height: 1.6; 
  font-family: Helvetica;
  color:green;
  text-align: centre;
  margin: 0;
  font-size: 18px;
  }
  


.Rlogo {
  float: left;
  width: 250px;
  shape-outside: url(https://www.prometheusresearch.com/wp-content/uploads/2019/06/10-Healthcare-Quality-Improvement-Trends-You-Can%E2%80%99t-Ignore.jpg);
  shape-margin: 20px;
  margin-right: 20px;
  margin-bottom: 20px;
  
  }
  
.Hlogo{
  float: right;
  width: 250px;
  shape-outside: url(https://www.gbmc.org/photos/improving-quality-of-care-11-6-15.jpg);
  shape-margin: 20px;
  margin-left: 20px;
  margin-bottom: 20px;
  
  } 

.Jlogo{
  float: left;
  width: 250px;
  shape-outside: url(https://sphweb.bumc.bu.edu/otlt/mph-modules/hpm/americanhealthcare_quality/Quality-Wordle.png);
  shape-margin: 20px;
  margin-right: 20px;
  margin-bottom: 20px;
  
  } 



"





dat <- readxl::read_xlsx("H:\\Denis\\Writing\\Academic Writing\\Hajji\\Health_Quality_Indicators_Analysis\\Copy of Drivers of quality.xlsx", sheet = 'Data_Cleaning')

# Select specific columns to work with
df_t <- dat[ , c('Room_Cleanliness','Room_serenity','time_doc','Respect_courtesy',
  'Doctor listening to the patient','Concerns properly heard and registered','Frequency through which doctors explained things',
  'Staff introducing themselves before treatment','Quick staff response','New medicine administered',
  'Your understanding of the new medicine','Hospital staff describe the sideeffects of the medicine',
  'Family informed of your condition','Doctors following up on whether you received the help needed','Got a feedback on health problems or symptoms',
  'Any mistakes observed at the healthcare','Most common mistake','Patient_safety','Why you chose the healthfacility(first choice)')]

df <- dat[ , c('Location','Gender','Age','Age_Clean','Education','Long-standing_conditions','Room_Cleanliness',
                                 'Room_serenity','time_doc',
                                 'Respect_courtesy','Doctor listening to the patient','Concerns properly heard and registered',
                                 'Frequency through which doctors explained things','Staff introducing themselves before treatment','Staff learning about the patient as a person',
                                 'Quick staff response','Needed help with getting to the bathroom','often times help received',
                                 'New medicine administered','Your understanding of the new medicine','Hospital staff describe the sideeffects of the medicine',
                                 'Family informed of your condition','Destination after being released',
                                 'Doctors following up on whether you received the help needed','Got a feedback on health problems or symptoms',
                                 'Any mistakes observed at the healthcare','Most common mistake',
                                 'Why you chose the healthfacility(first choice)','Patient_safety','Which of the following most affected your choice for the healthcare facility?(2nd Choice)','Which of the following most affected your choice for the healthcare facility?(3rd Choice)',
                                 'Which of the following most affected your choice for the healthcare facility?(4th Choice)')]



df_clean <- dat[ , c('Respect_courtesy',
                     'Doctor listening to the patient','Concerns properly heard and registered','Concerns properly heard and registered',
                     'Frequency through which doctors explained things')]

percentage_gender <- 
  df %>% 
  group_by(Gender) %>% summarise(Percentage=n()/nrow(.))




# plot Gender by Hospital Cleanliness

hospital_cleanliness <- df %>% 
  dplyr::count(Gender, Room_Cleanliness, sort = TRUE)


hospital_cleanliness %>% 
  mutate(Room_Cleanliness = reorder_within(            # reorder functions arranges from the most occuring frequency to the least occuring
    x = Room_Cleanliness,
    by = n,
    within = Gender
  )) %>%
  ggplot(aes(x = Room_Cleanliness, y =n, fill = Gender)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  coord_flip()+
  facet_wrap(~Gender, scales = "free") +
  labs(x = "Cleanliness of the Rooms", y = "Frequency")



# chi square independence test

df %>% 
  select(Gender,Respect_courtesy) %>% 
  table() %>% 
  fisher.test()

df %>% 
  select(Age_Clean,'Doctor listening to the patient') %>% 
  table() %>% 
  fisher.test()


df %>% 
  select(Education,'Frequency through which doctors explained things') %>% 
  table()%>% 
  fisher.test()


df %>% 
  select(Location,Respect_courtesy) %>% 
  table() %>% 
  fisher.test()


df %>% 
  select('Staff introducing themselves before treatment',Respect_courtesy) %>% 
  table() %>% 
  fisher.test()


df %>% 
  select('Why you chose the healthfacility(first choice)',Room_Cleanliness) %>% 
  table() %>% 
  fisher.test()


df %>% 
  select('Why you chose the healthfacility(first choice)',Room_serenity) %>% 
  table()



  



library(ggstatsplot)

test <- fisher.test(table(df$Gender,df$Respect_courtesy))

# Using a graph to view the correlation
ggbarstats(
  df, Gender,Respect_courtesy,
  results.subtitle = FALSE,
  subtitle = paste0(
    "Fisher's exact test", ", p-value = ",
    ifelse(test$p.value < 0.001, "< 0.001", round(test$p.value, 3))
  )
  
)



  










ui <- dashboardPage(skin = "green",
                dashboardHeader(title="Jowhar Health Quality Indicators", titleWidth = 650,
                                    
                                    tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/denis-ochieng-b58810202/" ,icon("linkedin"), "My Profile", target="_blank")),
                                    tags$li(class="dropdown",tags$a(href="https://github.com/Ochieng40" , icon("github"), "Source Code", target="_blank"))),
                
                
                dashboardSidebar(
                  sidebarMenu(id = "sidebar",
                              menuItem("Dataset", tabName = "data", icon = icon("database")),
                              menuItem(text = "Visualization", tabName = "viz", icon = icon("chart-line")),
                              menuItem(text = "Project Map", tabName = "Map", icon = icon("map")))
                  
                  
                  
                  ),
                
                
                  
                
                dashboardBody(
                  
                  tags$head(
                    tags$style(HTML(CSS))
                  ),
                  
                      tabItems(
                        ## First tab item
                        
                        tabItem(tabName = "data",
                                tabBox(id = "t1", width = 12,
                                       tabPanel("About", icon =icon("address-card"),
                                                fluidRow(column(width = 12,
                                                                h1(strong("About the Project"), align = "center"),
                                                                
                                                                img(
                                                  class = "Rlogo",
                                                  src = "https://www.prometheusresearch.com/wp-content/uploads/2019/06/10-Healthcare-Quality-Improvement-Trends-You-Can%E2%80%99t-Ignore.jpg"),
                                                  p("This dissertation aims to understand the factors that affect healthcare quality in the provision of health 
                                                    and nutrition services at Jowhar healthcare facility in Somalia. The dissertation is carried out by Mohammed 
                                                    Hajji a student at Salford University Manchester pursuing Project Management. 
                                                    The dissertation aims to build on previous research quality of healthcare service provision. The researcher
                                                    also aims to use the research findings in improving the quality of healthcare in Jowhar Somalia and other fragile environments.")),
                                                  
                                                  column(width = 12,
                                                         h1(strong("Project Location"), align = "center"),
                                                         
                                                         img(
                                                    class = "Hlogo",
                                                    src = "https://www.gbmc.org/photos/improving-quality-of-care-11-6-15.jpg"),
                                                    p("The healthcare facility under study is located in the Middle Shabelle Region in Jowhar Buullo Makiion. The INTERSOS
                                                      organization mainly funds this hospital. The quality of the hospital has been measured by the bed capacity, the hospital's
                                                      infrastructural size, the number of people the hospital has assisted, and the cases of respiratory and malnutrition treatments
                                                      conducted by the hospital (INTERSOS, 2019). Jowhar is the capital city of Hirshabelle State and also the
                                                      administrative city of Middle Shabelle Region. It is situated about 90 km north of Mogadishu. Jowhar consists of 4 urban villages, namely Horseed,
                                                      Bulosheikh, Kulmis, and Hantiwadag. The first two villages are located on the east side of the town, and the last two are on the west side.
                                                      Jowhar has strategic importance because of its location. It connects Mogadishu to the rest of the central regions through
                                                      the main road that crosses the country. ")
                                                  ),
                                                  
                                                  
                                                  column(width = 12,
                                                         h1(strong("Interest in the Subject"), align = "center"),
                                                         
                                                         img(
                                                           class = "Jlogo",
                                                           src = "https://sphweb.bumc.bu.edu/otlt/mph-modules/hpm/americanhealthcare_quality/Quality-Wordle.png"),
                                                         p("Having worked in the area of monitoring and evaluation, the researcher understands the importance of generating clear and 
                                                           relevant indicators that can be used to guide and improve the performance of a project. The researcher also understands that 
                                                           health is an integral part of the sustainable development goals arising from the governance systems of the United Nations, as 
                                                           it plays a role in the enhancement of citizens’ lives, especially in potentially vulnerable environments such as Somalis. The 
                                                           country currently lacks proper quality healthcare services, and there is a gap in research on healthcare quality, especially in 
                                                           providing health and nutrition services, in such stressed environments. Currently, most organizations in the area measure 
                                                           quality by mainly regarding the healthcare infrastructure. Quality drivers such as the social treatment received by patients, 
                                                           performances of the processes, time spent at the facility and the patient's general satisfaction have often not been critically
                                                           evaluated in previous research. All these factors are what informs the researcher to spend time and resources in this subject area.")
                                                  )
                                                  
                                                  
                                                               
                                                )),
                                      
                                       
                                       tabPanel(title = "Data", icon = icon("table"), dataTableOutput("dataT")),
                                       tabPanel(title = "Structure", icon = icon("table"), verbatimTextOutput("structure"))
                                       
                                       
                                       
                                       
                       )),
                       
                       
                       tabItem(tabName = "viz",
                               tabBox(id = "t2", width = 12,
                                      tabPanel(title = "Descriptive Analysis", icon = icon("table"), h4("tabpanel 4 placeholder")),
                                      tabPanel(title = "Inferential Analysis", icon = icon("table"), h4("tabpanel 5 placeholder"))
                                      ))
                      
                      
                      
                      
                      
                      
                      
                    
                    
                    
))

)


# Define server logic required to draw a histogram
server <- function(input, output, session) {

 # structure of the data
  output$structure <- renderPrint(
    df_t %>% 
      str()
  )
  
 # The data table
output$dataT <- renderDataTable(
    datatable(df,
              options = list(paging = TRUE,    ## paginate the output
                             pageLength = 65,  ## number of rows to output for each page
                             scrollX = TRUE,   ## enable scrolling on X axis
                             scrollY = TRUE,   ## enable scrolling on Y axis
                             autoWidth = TRUE, ## use smart column width handling
                             server = FALSE,   ## use client-side processing  -- If server = TRUE (the default), the browser receives only the displayed data. If server = FALSE the browser receives all the data, which can slow it down if the dataset is large.
                             dom = 'Bfrtip',
                             buttons = c('csv', 'excel'),
                             columnDefs = list(list(targets = '_all', className = 'dt-center'),
                                               list(targets = c(0, 8, 9), visible = TRUE))
              ),
              extensions = 'Buttons',
              selection = 'single', ## enable selection of a single row
              filter = 'bottom',              ## include column filters at the bottom
              rownames = FALSE 
  )
  )
  
  # Demographics
  
  output$loc <- renderPlotly(
    
    df %>% 
     drop_na(Location) %>%  # This function includes the missing values into the table
      ggplot(aes(Location))+    # use fct_infreq to create order   
      geom_bar(fill = "#97B3C6")+
      coord_flip()+
      theme_bw()+
      labs(x = "Location",
           y = NULL,
           title = "Respondent Locations")   
        
        
      )
      
    
    
  
  
  # Histogram for age variable - The histogram will tell us the distribution of the data
  
 output$ag <- renderPlotly(
   df %>% 
    ggplot(aes(Age))+
    geom_histogram(binwidth = 5, fill = "#033E3E")+
     theme_bw()+
     theme_bw()+
     theme(text=element_text(family="Times", face="bold", size=23),
           #panel.grid = element_blank(),   #removes grid line
           plot.title = element_text(hjust = 0.5, size=25))+   #specifies the size and position of the title
     labs(x = "Respondents' age",
          y = "Frequency")+
     ggtitle("Age of Respondents")

 )
 
 
 # Bar graph for education
 
 output$edu <- renderPlotly(
   df %>% 
     drop_na(Education) %>%  # This function includes the missing values into the table
     ggplot(aes(fct_infreq(Education)))+    # use fct_infreq to create order   
     geom_bar(fill = "#033E3E")+
     coord_flip()+
     theme_bw()+
     theme(text=element_text(family="Times", face="bold", size=23),        #Times New Roman, 12pt, Bold
           panel.grid = element_blank(),   #removes grid line
           #axis.title = element_blank(),    # removes the axis title
           panel.border = element_blank(),    #Removes panel border line
           plot.title = element_text(hjust = 0.5, size=28))+   #specifies the size and position of the title
     labs(x = "Education",
          y = "Frequency")+
     ggtitle("Education Level")
     
     
   
   
   
   
 )
 
 output$Gend <- renderPlotly(
   
   df %>%
     ggplot()+
     theme_bw()+
     geom_bar(aes(x = " ", y = percentage_gender, fill = Gender),
              stat = "identity", color = "white")+
     coord_polar("y", start = 0)
   
   
   
   
 )
 
 
 output$Loca <- renderPlotly(
   
   df %>% 
     drop_na(Location) %>%  # This function includes the missing values into the table
     ggplot(aes(fct_infreq(Location)))+    # use fct_infreq to create order   
     geom_bar(fill = "#033E3E")+
     #coord_flip()+
     theme_bw()+
     theme(text=element_text(family="Times", face="bold", size=28),        #Times New Roman, 12pt, Bold
           panel.grid = element_blank(),   #removes grid line
           axis.title = element_blank(),    # removes the axis title
           panel.border = element_blank(),    #Removes panel border line
           plot.title = element_text(hjust = 0.5, size=30))+   #specifies the size and position of the title
     labs(x = "Location",
          y = "Frequency")+
     ggtitle("Location")
   
   
   
 )
 
 output$Clean <- renderPlotly(
   
   df %>% 
     drop_na(Room_Cleanliness) %>%  # This function includes the missing values into the table
     ggplot(aes(fct_infreq(Room_Cleanliness)))+    # use fct_infreq to create order   
     geom_bar(fill = "#033E3E")+
     #coord_flip()+
     theme_bw()+
     theme(text=element_text(family="Times", face="bold", size=30),        #Times New Roman, 12pt, Bold
           #panel.grid = element_blank(),   #removes grid line
           axis.title = element_blank(),    # removes the axis title
           panel.border = element_blank(),    #Removes panel border line
           plot.title = element_text(hjust = 0.5, size=34))+   #specifies the size and position of the title
     labs(x = "Room_Cleanliness",
          y = "Frequency")+
     ggtitle("Room Cleanliness")
   
 )
 
 
 output$Serene <- renderPlotly(
   
   df %>% 
     drop_na(Room_serenity) %>%  # This function includes the missing values into the table
     ggplot(aes(fct_infreq(Room_serenity)))+    # use fct_infreq to create order   
     geom_bar(fill = "#033E3E")+
     #coord_flip()+
     theme_bw()+
     theme(text=element_text(family="Times", face="bold", size=30),        #Times New Roman, 12pt, Bold
           #panel.grid = element_blank(),   #removes grid line
           axis.title = element_blank(),    # removes the axis title
           panel.border = element_blank(),    #Removes panel border line
           plot.title = element_text(hjust = 0.5, size=34))+   #specifies the size and position of the title
     labs(x = "Room_serenity",
          y = "Frequency")+
     ggtitle("Room_Serenity")
   
 )
 
 
 
 output$time_doc <- renderPlotly(
   
   df %>% 
     drop_na(time_doc) %>%  # This function includes the missing values into the table
     ggplot(aes(fct_infreq(time_doc)))+    # use fct_infreq to create order   
     geom_bar(fill = "#033E3E")+
     #coord_flip()+
     theme_bw()+
     theme(text=element_text(family="Times", face="bold", size=30),        #Times New Roman, 12pt, Bold
           panel.grid = element_blank(),   #removes grid line
           axis.title = element_blank(),    # removes the axis title
           panel.border = element_blank(),    #Removes panel border line
           plot.title = element_text(hjust = 0.5, size=34))+   #specifies the size and position of the title
     labs(x = "time_docy",
          y = "Frequency")+
     ggtitle("Time to discuss health problem with doctor")
   
 )
 
 output$Respect_courtesy <- renderPlotly(
   
   df %>% 
     drop_na(Respect_courtesy) %>%  # This function includes the missing values into the table
     ggplot(aes(fct_infreq(Respect_courtesy)))+    # use fct_infreq to create order   
     geom_bar(fill = "#033E3E")+
     #coord_flip()+
     theme_bw()+
     theme(text=element_text(family="Times", face="bold", size=30),        #Times New Roman, 12pt, Bold
           panel.grid = element_blank(),   #removes grid line
          # axis.title = element_blank(),    # removes the axis title
           panel.border = element_blank(),    #Removes panel border line
           plot.title = element_text(hjust = 0.5, size=34))+   #specifies the size and position of the title
     labs(x = NULL,
          y = "Frequency")+
     ggtitle("Treated with courtesy & respect by the doctor")
   
 )
 
 
 

 
 
}

# Run the application 
shinyApp(ui = ui, server = server)
