#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#alle variable in einem Sidebar panel links with scroll bar und Ergebnisse rechts fixed. See Kosten.!! 
#Change color of scroll bar -> sichtbar. 
#https://www.r-bloggers.com/2022/06/scrollbar-for-the-shiny-sidebar/
#put explanation of graphics 
#scroll color 

#change the presented grapha: legend, labels... 


library(shiny)
library(bslib)
library(shinythemes)
library(ggplot2)
library(shinyjs)


# Model of the school garden (see Index.RMD for the explaination and posthoc)
library(decisionSupport)
# We need these functions to run our model ####
# value_varrier function to add variability to values
source("cory/functions/vv.R")

# chance event function to assess the chances 
# mostly for risks
source("cory/functions/chance_event.R")

# discount values for NPV (time value for money)
source("cory/functions/discount.R")

# Model testing ####

# make variables for testing our model (only for construction)
source("cory/functions/make_variables.R")
source("cory/functions/estimate_read_csv.R")


# Define UI for application that draws a histogram
#UI####
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* For Chrome, Safari, and Edge */
      ::-webkit-scrollbar {
        width: 10px;
      }
      ::-webkit-scrollbar-track {
        background: #F1F1F1;
      }
      ::-webkit-scrollbar-thumb {
        background: #2d7547;
      }
      ::-webkit-scrollbar-thumb:hover {
        background: darkred;
      }
      /* For Firefox */
      body {
        scrollbar-width: thin;
        scrollbar-color: #2d7547; /* different colors : red #F1F1F1 */
      }
      
      /* close and open Sidebar */
      #toggleSidebar {
        background-color: #2d7547;  /* dark green Default button color */
        color: white;
      } 
      #toggleSidebar.closed {
        background-color: #636af7;  /* Color when sidebar is hidden */
      }
      
      /* Result as a sticky panel so that It shows when participants are answering */
      .main-panel {
       position: sticky;
      top: 0;
      z-index: 1000;
      background-color: #f4f4f4;
      padding: 15px;
      box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1); /* Optional: Adds shadow to give a floating effect */
    }
    }
    .content {
      margin-right: 60px; /* Adjust to give space for the sticky panel */
    }
    .app-header {
      background-color: #ffffff; /* Choose your color */
      padding: 10px; /* Optional: Adds some padding around the title */
      border-radius: 5px; /* Optional: Rounds the corners */
    }
    .tabPanel {
      background-color: #ffffff; /* Choose your color */
      padding: 10px; /* Optional: Adds some padding around the title */
      border-radius: 5px; /* Optional: Rounds the corners */
    }
    
    "))
  ),
  
  #UI Start####
  # Application title
  titlePanel(
    div(class = "app-header",
        # Logos column
        fluidRow(column(width = 2,
                        align = "right",
                        tags$a(#href = "https://agroreforest.eu/", # Add URL here
                               tags$img(src = "Logo/NIFAM.png", height = "90px"))),
                 # a(href = "https://agroreforest.eu/", target = "_blank",  
                 # img(src = "ReFOREST_logo_horizontal_transparent.png", height = "70px"))),
                 column(width = 7,
                        align = "center",
                        h2(class = "app-title",
                           "Holistic Decision Analysis for NIFAM")),
                 column(width = 3,
                        align = "left",
                        tags$a(href = "https://www.gartenbauwissenschaft.uni-bonn.de/", # Add URL here
                               tags$img(src = "Logo/logo-mix-nobg-en.png", height = "90px"))),
                 
                 windowTitle = "MyPage")
    )),
#theme = bs_theme(version = 5, bootswatch = "minty"), 
  tabsetPanel(               
    # tabPanel("Public", h1("Public School"),
    #          h2("HortiPrimed")),
    # tabPanel("Seedling",h1("Tomatenjungepflanzeproduktion")
    # ),
    
    
    tabPanel("1.Version", h1("1. Version"),
            class= "tabPanel",
                 
             fluidRow(
               column(4, textInput("Date", "Date")),  # Each column takes up 4/12 of the width
               column(4, textInput("Organization", "Your Organization (optional)")),
               column(4, textInput("Name", "Your Name (optional)"))
             ),
             fluidRow(
               sidebarPanel(width = 12,
                            fluidRow( h4( strong('How to Use This NIFAM-Shiny:')),
                                      h5 (HTML("<b>1.</b> Please give your 'input data'")),
                                      h5 (HTML("<b>2.</b> Once you finish, click <b>Save & Download</b>.
                                      You can choose where your file will be saved in your divice.")),
                                      h5 (HTML("<b>3.</b> Send your file to '<b>s@uni-bonn.de</b>'.
                                      If you have any questions, please contact us via email."))
                                      
                            ))),
             
             
             fluidRow(
               
               column(7, class = "content",
                      p(),
                      # 1.Introduction####
                      h4(strong('1. Introduction')),
                      p("Please answer the following questions. Put yourself in the position of a member of the school board of a school in urban Hanoi."),
                      p("You want to take a decision of whether or not to operate a school garden on the school's premises and of whether or not to integrate this school garden into a STEM teaching approach."),
                      p("The graphical model of this decision was jointly developed with a group of experts and the NGO CODAS."),
                      p("The graphical model shows all relevant aspects (variables) that influence the decision and each relevant aspect needs to be evaluated quantitatively by giving ranges of plausible values."),
                      p("Please provide ranges that include the correct value with a likelihood of 90%, just as you have trained it during the NIFAM calibration training."),
                      p("A mathematical model was developed to simulate the outcomes of the decision(s) given an operation period of the school garden of 10 years and given your calibrated expert values for each variable."),
                      p(),
                      actionButton("toggleSidebar", "Hide Sidebar", class = "btn",icon = icon("fa-light fa-eye-slash")), # in server toggle("sidebar)
                      
                      useShinyjs(),  # Initialize shinyjs
                      sidebarPanel(  id = "sidebar",  # Assign an ID to the sidebar
                                     width = 12,
                                   style = "height: 60vh; overflow-y: auto;", #height in % 
                                   
                                   h5(HTML("<b>1-1. Number of years for garden simulation:</b>")),
                                   p("Please set the number of years for which you want the model to simulate the project",
                                      style = "padding-left: 20px;"),
                                   
                                   sliderInput("number_of_years",
                                               NULL,
                                               min = 1,
                                               max = 10,
                                               value = 5,
                                               step = 1),
                                   h5(HTML("<b>1-2. Discounting factor/ base (lending) rate/ prime (lending) rate:</b>")),
                                   p("Give your estimate of how  the base lending rate in Vietnam will develop in the coming years, 
                                     in min % and max %."
                                     ,style = "padding-left: 20px;"),
                                   sliderInput("discount_rate",
                                               NULL,
                                               min = 1,
                                               max = 15,
                                               value = c(2, 8),
                                               step = 1), 
                                   h5(HTML("<b>1-3. Coefficient of variation for our school garden intervention (%):</b>")),
                                   sliderInput("CV_value",
                                               NULL,
                                               min = 1,
                                               max = 100,
                                               value = c(40, 60),
                                               step = 1), 
                                   h5(HTML("<b>1-4. Inflation rate (%):</b>")),
                                   p("Give your estimate of how  the inflation rate in Vietnam will develop in the coming years,
                                     in min % and max %." ,style = "padding-left: 20px;"),
                                   sliderInput("inflation_rate",
                                               NULL,
                                               min = 1,
                                               max = 100,
                                               value = c(3, 15),
                                               step = 1) 
                                 
                      ),
                      
                      # 2. General estimation####
                      h4(strong('2. General estimation of a school garden')),
                      sidebarPanel(width = 12,
                                   style = "height: 60vh; overflow-y: auto;", #height in %
                                   
                                   h5(HTML("<b>2-1. Size of school garden in (m2):</b>")),
                                   p("Please estimate the area size (in m2) a school can allocate to a school garden project."
                                     ,style = "padding-left: 20px;"),
                                   sliderInput("size_of_garden",
                                               NULL,
                                               min = 1,
                                               max = 500,
                                               value = c(5, 100),
                                               step = 1),
                                   h5(HTML("<b>2-2. Cut off value for where the garden becomes more expensive:</b>")),
                                   p("Please estimate at about what size the investment costs for a shcool garden might 
                                     become considerably more expensive because other types of investment might be required 
                                     (e.g. more mechanization or other type of lifestock (cattle rather than small livestock and poultry)",
                                     style = "padding-left: 20px;"),
                                   sliderInput("expensive_garden_size",
                                               NULL,
                                               min = 1,
                                               max = 200,
                                               value = c(80, 100),
                                               step = 1),
                                   h5(HTML("<b>2-3. Percentage more expensive we expect the garden to be if it is beyond the expensive_garden_size:</b>")),
                                   p("Please estimate how the investment costs will increase beyond this area size (increase by ... %)",
                                     style = "padding-left: 20px;"),
                                   sliderInput("cost_increase_expensive_garden_size",
                                               NULL,
                                               min = 0.1,
                                               max = 20,
                                               value = c(0.4, 2.6),
                                               step = 0.1)
                               
                      ),
                      
                      p(),
                      # 3. General estimation of####
                      h3(strong('3. General estimation of a school garden')),
                      sidebarPanel(width = 12,
                                   style = "height: 60vh; overflow-y: auto;", #height in %
                                   
                                   h5(HTML("<b>3-1. Chance of student engagement (%):</b>")),
                                   p("How do you rate the likelihood that students like the school garden project and engage positively with it (in%)?",
                                     style = "padding-left: 20px;"),
                                   sliderInput("if_students_like",
                                               NULL,
                                               min = 1,
                                               max = 100,
                                               value = c(50, 80),
                                               step = 1),
                                   h5(HTML("<b>3-2. Chance of parents support / effectiveness (%):</b>")),
                                   p("How do you rate the likelihood that parents effectively support the garden project (in %)?",
                                     style = "padding-left: 20px;"),
                                   sliderInput("if_parents_like",
                                               NULL,
                                               min = 1,
                                               max = 100,
                                               value = c(50, 80),
                                               step = 1),
                                   h5(HTML("<b>3-3. Chance of community support (%):</b>")),
                                   p("How do you rate the likelihood that the community/ neighborhood supports the garden project (in %)?",
                                     style = "padding-left: 20px;"),
                                   sliderInput("if_community_likes",
                                               NULL,
                                               min = 1,
                                               max = 100,
                                               value = c(50, 80),
                                               step = 1),
                                   h5(HTML("<b>3-4. Chance of effective garden management (%):</b>")),
                                   p("How do you rate the likelihood that the garden can be managed effectively (in %)?",
                                     style = "padding-left: 20px;"),
                                   sliderInput("if_effective_manage",
                                               NULL,
                                               min = 1,
                                               max = 100,
                                               value = c(50, 80),
                                               step = 1),
                                   h5(HTML("<b>3-4. Chance of sufficient yield from garden (%):</b>")),
                                   p("The likelihood to have sufficient supply of yield for school's use?",
                                     style = "padding-left: 20px;"),
                                   sliderInput("if_garden_yield_enough",
                                               NULL,
                                               min = 1,
                                               max = 100,
                                               value = c(50, 80),
                                               step = 1),
                                   h5(HTML("<b>3-5. Chance of healthy food from garden (%):</b>")),
                                   p("How would you rate the likelihood that the garden will produce food that is healthy(er) (in %)?",
                                     style = "padding-left: 20px;"),
                                   sliderInput("if_garden_healthy",
                                               NULL,
                                               min = 1,
                                               max = 100,
                                               value = c(50, 80),
                                               step = 1),
                                   h5(HTML("<b>3-6. Chance of teacher engagement (%):</b>")),
                                   p("How do you rate the likelihood that teachers like the school garden project and engage positively with it (in %)?",
                                     style = "padding-left: 20px;"),
                                   sliderInput("if_teachers_like",
                                               NULL,
                                               min = 1,
                                               max = 100,
                                               value = c(50, 80),
                                               step = 1),
                                   h5(HTML("<b>3-7. Chance of high education quality / effectiveness (%):</b>")),
                                   p("How do you rate the likelihood that the lessons learned on school gardens by kids are effective (in %)?",
                                     style = "padding-left: 20px;"),
                                   sliderInput("if_effective_teaching",
                                               NULL,
                                               min = 1,
                                               max = 100,
                                               value = c(50, 80),
                                               step = 1),
                                   h5(HTML("<b>3-8. Chance of effective training for teachers (%):</b>")),
                                   p("How do you rate the likelihood that teachers will be trained effectively on school gardening and be able to convey 
                                     the messages effectively to the kids (%)?",
                                     style = "padding-left: 20px;"),
                                   sliderInput("if_effective_training",
                                               NULL,
                                               min = 1,
                                               max = 100,
                                               value = c(50, 80),
                                               step = 1),
                                   h5(HTML("<b>3-9. Chance of garden having ecologically valuable green space (%):</b>")),
                                   p("How do you rate the likelihood that the garden will become a place of ecological value, 
                                     a habitat for plants and animals (in %)?",
                                     style = "padding-left: 20px;"),
                                   sliderInput("if_offer_green_space",
                                               NULL,
                                               min = 1,
                                               max = 100,
                                               value = c(50, 80),
                                               step = 1),
                                   h5(HTML("<b>3-10. Chance of garden reducing polution (%):</b>")),
                                   p("How do you rate the likelihood that the existence of the garden will reduce pollution 
                                     as compared to how the space is used now (in %)?",
                                     style = "padding-left: 20px;"),
                                   sliderInput("if_reduce_pollution",
                                               NULL,
                                               min = 1,
                                               max = 100,
                                               value = c(50, 80),
                                               step = 1),
                                   h5(HTML("<b>3-11. Chance of biophysical not damaging (i.e. weather) (%):</b>")),
                                   p("How do you rate the likelihood that the environment will be favorable for 
                                     the garden, i.e. not causing any damage (garden will not be stressed from heat, 
                                     frost, flood, drought) (in %)?",
                                     style = "padding-left: 20px;"),
                                   sliderInput("if_biophysical_good",
                                               NULL,
                                               min = 1,
                                               max = 100,
                                               value = c(50, 80),
                                               step = 1)
                                   
                      ),
                      #4.  The investment costs####
                      h3(strong('4.  The investment costs for setting up a garden')),
                      sidebarPanel(width = 12,
                                   style = "height: 60vh; overflow-y: auto;", #height in %
                                   
                                   h5(HTML("<b>4-1. Costs of design team consultant (million VND):</b>")),
                                   p("Please give your estimate range of costs for planning and designing 
                                     the garden (lay out of plots, planning of crop rotations, etc.) (in million VND).",
                                     style = "padding-left: 20px;"),
                                   sliderInput("garden_designing_costs",
                                               NULL,
                                               min = 1,
                                               max = 50,
                                               value = c(10, 20),
                                               step = 1),
                                   h5(HTML("<b>4-2. Costs of construction for setting up garden (million VND):</b>")),
                                   p("Please give your estimate range of costs for setting up the garden (initial investment) (in million VND).",
                                     style = "padding-left: 20px;"),
                                   sliderInput("construction_cost",
                                               NULL,
                                               min = 1,
                                               max = 200,
                                               value = c(5.5, 50.5),
                                               step = 0.5),
                                   h5(HTML("<b>4-3. Costs of equipment for setting up garden (million VND):</b>")),
                                   p("Please give your estimate range of costs for the equipment that needs to be purchased to operate the garden 
                                     (machinery and tools) (in million VND).",
                                     style = "padding-left: 20px;"),
                                   sliderInput("equipment_cost",
                                               NULL,
                                               min = 1,
                                               max = 50,
                                               value = c(5, 20),
                                               step = 1),
                                   h5(HTML("<b>4-4. Costs of training teachers when setting up garden (million VND):</b>")),
                                   p("Please give your estimate of how much it will cost to initially train the teachers 
                                     (on school gardening and on teaching content around school gardens e.g. healthy nutrition) (in million VND).",
                                     style = "padding-left: 20px;"),
                                   sliderInput("teacher_training_cost",
                                               NULL,
                                               min = 1,
                                               max = 50,
                                               value = c(10, 30),
                                               step = 1),
                                   h5(HTML("<b>4-5. Costs of planning meetings (million VND):</b>")),
                                   p("Please estimate the costs for planning meetings with the school board before setting up the school garden 
                                     (as part if initial investment costs) (in million VND).",
                                     style = "padding-left: 20px;"),
                                   sliderInput("school_board_planning",
                                               NULL,
                                               min = 1,
                                               max = 50,
                                               value = c(3, 15),
                                               step = 1),
                                   h5(HTML("<b>4-6. Equipment for teaching (million VND):</b>")),
                                   p("Please estimate the costs for tools that will be necessary 
                                     for teaching around the garden (magnifying glasses, cup magnifier, charts, 
                                     boards, gardening tools for kids, microscopes, etc.) (in million VND).",
                                     style = "padding-left: 20px;"),
                                   sliderInput("teaching_equipment",
                                               NULL,
                                               min = 1,
                                               max = 50,
                                               value = c(5, 15),
                                               step = 1),
                                   h5(HTML("<b>4-7. Starting compost (million VND):</b>")),
                                   p("Please estimate the costs for starting a compost heap 
                                     (appropriate for the size of the garden) in terms of construction costs and 
                                     starting cultures of efficient microorganisms (in million VND).",
                                     style = "padding-left: 20px;"),
                                   sliderInput("compost_starting",
                                               NULL,
                                               min = 1,
                                               max = 50,
                                               value = c(3, 15),
                                               step = 1),
                                   h5(HTML("<b>4-8. Starting worms for compost (million VND):</b>")),
                                   p("Please estimate the costs of  earth worms to start a vermiculture  (in million VND).",
                                     style = "padding-left: 20px;"),
                                   sliderInput("worm_starting",
                                               NULL,
                                               min = 1,
                                               max = 50,
                                               value = c(3, 10),
                                               step = 1),
                                   h5(HTML("<b>4-9. livestock_establishment_costs:</b>")),
                                   p("Please estimate the costs for the desired set of livestock 
                                     that will be farmed in the garden for manure production and use of crop residues  (in million VND).",
                                     style = "padding-left: 20px;"),
                                   sliderInput("livestock_establishment_costs",
                                               NULL,
                                               min = 1,
                                               max = 100,
                                               value = c(3, 20),
                                               step = 1),
                                   p(),
                                   h3(HTML("<b>4? Donation from families</b>")),
                                   p(),
                                   h5(HTML("<b>4-10. Chance that families donate to establishment (%):</b>")),
                                   p("How do you rate the likelihood that students' families or other donors/ 
                                     sponsors contribute a donation to the investment costs (in %)?",
                                     style = "padding-left: 20px;"),
                                   sliderInput("if_family_pays_establishment",
                                               NULL,
                                               min = 1,
                                               max = 100,
                                               value = c(20, 50),
                                               step = 1),
                                   h5(HTML("<b>4-11. Portion of establishment costs donated by families (%):</b>")),
                                   p("Please rate the amount of investment costs that will be covered out of donations 
                                     from families, donors or sponsors (in million VND).",
                                     style = "padding-left: 20px;"),
                                   sliderInput("establishment_family_portion_paid",
                                               NULL,
                                               min = 1,
                                               max = 100,
                                               value = c(20, 80),
                                               step = 1)
                                   
                      ),
                      #5. The running costs####
                      h3(strong('5. The running costs of school garden')),
                      sidebarPanel(width = 12,
                                   style = "height: 60vh; overflow-y: auto;", #height in %
                                   
                                   h5(HTML("<b>5-1. Annual Labor cost to maintain school garden  (million VND/yr):</b>")),
                                   p("Please estimate the annual labor costs for maintaining the garden  (in million VND/ year).",
                                     style = "padding-left: 20px;"),
                                   sliderInput("maintaining_labor",
                                               NULL,
                                               min = 1,
                                               max = 100,
                                               value = c(20, 40),
                                               step = 1),
                                   h5(HTML("<b>5-2. Additional teacher salary costs (million VND/yr):</b>")),
                                   p("Please estimate the costs of additional salaries for teachers  (in million VND/ year).",
                                     style = "padding-left: 20px;"),
                                   sliderInput("teacher_salary_cost",
                                               NULL,
                                               min = 1,
                                               max = 100,
                                               value = c(20, 30),
                                               step = 1),
                                   h5(HTML("<b>5-3. Teaching equipment / manitaining microscopes etc. (million VND/yr):</b>")),
                                   p("Please estimate the annual costs for the maintenance (and where needed  renewal) of teaching equipment 
                                     (in million VND/ year).",
                                     style = "padding-left: 20px;"),
                                   sliderInput("teaching_equipment_annual",
                                               NULL,
                                               min = 1,
                                               max = 100,
                                               value = c(1, 50),
                                               step = 1),
                                   h5(HTML("<b>5-4. Teaching tools / paper etc. (million VND/yr):</b>")),
                                   p("Please estimate the annual costs for teaching material like paper, etc.  (in million VND/ year).",
                                     style = "padding-left: 20px;"),
                                   sliderInput("teaching_tools",
                                               NULL,
                                               min = 1,
                                               max = 100,
                                               value = c(2, 20),
                                               step = 1),
                                   h5(HTML("<b>5-5. Please estimate the annual costs for seeds and seedlings  (in million VND/ year).:</b>")),
                                   p("Please estimate the annual costs for seeds and seedlings  (in million VND/ year).",
                                     style = "padding-left: 20px;"),
                                   sliderInput("seed_costs",
                                               NULL,
                                               min = 1,
                                               max = 50,
                                               value = c(2, 30),
                                               step = 1),
                                   h5(HTML("<b>5-6. Fertilizer i.e. EM to add to compost (million VND/yr):</b>")),
                                   p("Please estimate the annual costs for fertilizer, i.e. organic matter (if the garden does not produce enough), 
                                     efficient microorganisms to add to compost, earth worms for vermicomposting, if needed, etc.  (in million VND/ year).",
                                     style = "padding-left: 20px;"),
                                   sliderInput("fertilizer",
                                               NULL,
                                               min = 1,
                                               max = 30,
                                               value = c(1, 5.5),
                                               step = 0.5),
                                   h5(HTML("<b>5-7. Integrated Pest Managemernt (IPM) (million VND/yr):</b>")),
                                   p("Please estimate the annual costs for integrated pest management measures  (in million VND/ year).",
                                     style = "padding-left: 20px;"),
                                   sliderInput("plant_protection",
                                               NULL,
                                               min = 1,
                                               max = 30,
                                               value = c(1, 5.5),
                                               step = 0.5),
                                   h5(HTML("<b>5-8. Mainitaining animals (million VND/yr):</b>")),
                                   p("Please estimate the annual costs for maintaining the livestock (feed, veterinary costs, other?, slaughtering?,
                                     repair of shelter and fences, replacement of buckets for feeding?, maintenance of watering devices?) 
                                     (in million VND/ year).",
                                     style = "padding-left: 20px;"),
                                   sliderInput("livestock_maint",
                                               NULL,
                                               min = 1,
                                               max = 50,
                                               value = c(1, 20),
                                               step = 1),
                                   h5(HTML("<b>5-9. Mainitaining teacher training (million VND/yr):</b>")),
                                   p("Please estimate the annual costs of continued teacher training  (in million VND/ year).",
                                     style = "padding-left: 20px;"),
                                   sliderInput("annual_teacher_training",
                                               NULL,
                                               min = 1,
                                               max = 500,
                                               value = c(5, 276),
                                               step = 1)
                      
               ), #close 5. sidebarPanel 
               #6. Canteen####
               h3(strong('6. Canteen')),
               sidebarPanel(width = 12,
                            style = "height: 60vh; overflow-y: auto;", #height in %
                            
                            h5(HTML("<b>6-1. Chance that the school has a canteen (%):</b>")),
                            p("How do you rate the likelihood that the school has a canteen (in %)",
                              style = "padding-left: 20px;"),
                            sliderInput("if_school_has_canteen",
                                        NULL,
                                        min = 1,
                                        max = 100,
                                        value = c(20, 50),
                                        step = 1),
                            h5(HTML("<b>6-2. Canteen savings (million VND/yr):</b>")),
                            p("Please estimate the annual costs that can be saved in 
                              the canteen (if garden produce is relacing the fruits or vegetables purchased 
                              by the canteen), if any  (in million VND/ year).",
                              style = "padding-left: 20px;"),
                            sliderInput("canteen_savings",
                                        NULL,
                                        min = 1,
                                        max = 10,
                                        value = c(1, 5.5),
                                        step = 0.5),
                            h5(HTML("<b>6-3. Sales of garden products (million VND/yr):</b>")),
                            p("Please estimate the annual sales of additional yield (not given to canteen), 
                              if any  (in million VND/ year).",
                              style = "padding-left: 20px;"),
                            sliderInput("sale_of_yield",
                                        NULL,
                                        min = 1,
                                        max = 100,
                                        value = c(10, 30),
                                        step = 1)
                            
               ),
               #7. Potential cost savings####
               h3(strong('7. Potential cost savings from the existence of the school garden.')),
               sidebarPanel(width = 12,
                            style = "height: 60vh; overflow-y: auto;", #height in %
                            
                            h5(HTML("<b>7-1. Savings from extra-cirriclar activities (million VND/year):</b>")),
                            p("Please estimate the savings from extra-curricular activities, if any (in million VND/year)",
                              style = "padding-left: 20px;"),
                            sliderInput("extra_cirricular_savings",
                                        NULL,
                                        min = 1,
                                        max = 200,
                                        value = c(50, 80),
                                        step = 1),
                            h5(HTML("<b>7-2. Savings on formal education costs (no STEM garden) (million VND/year):</b>")),
                            p("Please estimate the savings on formal education costs (if any) in case the garden is set up 
                              without integration into a STEM teaching approach (in million VND/year)",
                              style = "padding-left: 20px;"),
                            sliderInput("formal_edu_savings",
                                        NULL,
                                        min = 1,
                                        max = 20,
                                        value = c(1, 10.5),
                                        step = 0.5),
                            h5(HTML("<b>7-3. formal_edu_savings_STEM:</b>")),
                            p("Please estimate the savings on formal education costs (if any) 
                              in case the garden is set up with integration into a STEM teaching approach (in million VND/year)",
                              style = "padding-left: 20px;"),
                            sliderInput("formal_edu_savings_STEM",
                                        NULL,
                                        min = 1,
                                        max = 500,
                                        value = c(20, 100),
                                        step = 1)

                            
               ),
               #8.Estimation of potential external investment####
               h3(strong('8.Extimate of potential external investment')),
               p("Running a school garden could potentially increase the reputation of the school and as a consequence this 
                 could lead to additional investment from outside the school (e.g. sponsors from local businesses). However, this is not sure."),
               sidebarPanel(width = 12,
                            style = "height: 60vh; overflow-y: auto;", #height in %
                            
                            h5(HTML("<b>8-1. Value of outside investment due to improved reputation no formal STEM (million VND/year):</b>")),
                            p(HTML("Please estimate the value of outside investment due to an improved reputation (if any) in case the garden is 
                              set up <b>without integration into a STEM teaching approach</b> (in million VND/year)"),
                              style = "padding-left: 20px;"),
                            sliderInput("outside_investment_value",
                                        NULL,
                                        min = 0.5,
                                        max = 20,
                                        value = c(1, 10.5),
                                        step = 0.5),
                            h5(HTML("<b>8-2. Value of outside investment due to improved reputation with STEM (million VND/year):</b>")),
                            p(HTML("Please estimate the value of outside investment due to an improved reputation (if any) in case the garden 
                              is set up <b>with integration into a STEM teaching approach</b> (in million VND/year)"),
                              style = "padding-left: 20px;"),
                            sliderInput("outside_investment_value_STEM",
                                        NULL,
                                        min = 0.5,
                                        max = 20,
                                        value = c(1, 10.5),
                                        step = 0.5),
                            h5(HTML("<b>8-3. Increased enrollment and/or tuition due to improved reputation no STEM (million VND/year):</b>")),
                            p(HTML("Please estimate the monetary value of increased enrollment numbers and/or higher tuition fees per child 
                                   because of an improved reputation in the case that the school garden <b>is not integrated</b> with STEM teaching 
                                   (science, technology, engineering and math)  (in million VND/ year)."),
                              style = "padding-left: 20px;"),
                            sliderInput("increased_enrollment_value",
                                        NULL,
                                        min = 0.1,
                                        max = 10,
                                        value = c(0.1, 5.1),
                                        step = 0.1),
                            h5(HTML("<b>8-4. Increased enrollment and/or tuition due to improved reputation with STEM (million VND/year):</b>")),
                            p(HTML("Please estimate the annual monetary value of increased enrollment numbers and/or higher tuition fees per child 
                                   because of an improved reputation in the case that the school garden <b>is integrated</b> with STEM teaching 
                                   (science, technology, engineering and math) (in million VND/ year)."),
                              style = "padding-left: 20px;"),
                            sliderInput("increased_enrollment_value_STEM",
                                        NULL,
                                        min = 1,
                                        max = 200,
                                        value = c(50, 80),
                                        step = 1)
                            
                            
               ),
               #9.Assumption:####
               h3(strong('9.Assumption: The quantity of fruits and vegetables children 
                         eat/snack from the garden is unaffected by the inclusion of a STEM teaching approach.')),
               p("Children having a garden at their school might or might not eat and snack more fruits and vegetables 
                 than children withour a school garden. This could potentially affect their health status, ability to concentrate 
                 and learn as well as to be able to socially engage with their environment. We assume that the quantity children 
                 eat/snack fruits and vegetables from the garden is not affected by whether the garden is included in a STEM teaching approach or not."),
               sidebarPanel(width = 12,
                            style = "height: 60vh; overflow-y: auto;", #height in %
                            
                            h5(HTML("<b>9-1. Healthcare savings for children having more access to safe vegetables from the garden (million VND/year):</b>")),
                            p("Please estimate the annual monetary value of healthcare savings for children having a school garden and eating more fruits and 
                              vegetables  (in million VND/ year).",
                              style = "padding-left: 20px;"),
                            sliderInput("child_veg_health_care_savings",
                                        NULL,
                                        min = 0.1,
                                        max = 10,
                                        value = c(0.1, 5),
                                        step = 0.1),
                            h5(HTML("<b>9-2. School performance value for children having more access to safe vegetables from the garden (million VND/year):</b>")),
                            p("Please estimate the annual monetary value of an improved school performance for children having a school garden and eating more fruits 
                              and vegetables  (in million VND/ year).",
                              style = "padding-left: 20px;"),
                            sliderInput("child_veg_school_performance_value",
                                        NULL,
                                        min = 0.01,
                                        max = 1,
                                        value = c(0.01, 0.2),
                                        step = 0.01),
                            h5(HTML("<b>9-3. Community engagement value for children having more access to safe vegetables from the garden (million VND/year):</b>")),
                            p("Please estimate the annual monetary value of a more active social engagement for children having a school garden and eating more fruits 
                              and vegetables  (in million VND/ year).",
                              style = "padding-left: 20px;"),
                            sliderInput("child_veg_community_engagement_value",
                                        NULL,
                                        min = 0.01,
                                        max = 1,
                                        value = c(0.01, 0.1),
                                        step = 0.01),
                            
                            
               ),
               #10. mental health####
               h3(strong('10. Estimation of mental health from school garden')),
               p("The positive effects of green spaces in cities for mental health 
                 are confirmed my medical resaerch. A school garden might or might not have a positive value on mental health."),
               sidebarPanel(width = 12,
                            style = "height: 40vh; overflow-y: auto;", #height in %
                            
                            h5(HTML("<b>10-1. Mental health value of children having a garden at school (million VND/year):</b>")),
                            p("Please estimate the annual monetary value of having a school garden as green space for the mental 
                              health of children, teachers, school staff and neighbors of the school  (in million VND/ year).",
                              style = "padding-left: 20px;"),
                            sliderInput("garden_mental_health_value",
                                        NULL,
                                        min = 1,
                                        max = 500,
                                        value = c(4, 300),
                                        step = 1)
                            
                            
               ),
               #11. Estimation with and without a STEM####
               h3(strong('11. Estimation with and without a STEM teaching approach')),
               p("Children having a garden at their school might or might not make healthier 
                 food choices than children withour a school garden. This could potentially affect their health status, 
                 ability to concentrate and learn as well as to be able to socially engage with their environment. We assume 
                 that the behavior of making food choices will be affected by  whether the garden is included in a STEM teaching approach or not."),
               sidebarPanel(width = 12,
                            style = "height: 60vh; overflow-y: auto;", #height in %
                            h4(strong("Without a STEM teaching approach")),
                            h5(HTML("<b>11-1. Healthcare savings from children making better choices about food with a passive (no STEM) garden (million VND/year):</b>")),
                            p(HTML("Please estimate the annual monetary value of healthcare savings for children having a school garden and making better food choices 
                              <b>(no STEM)</b> (in million VND/ year)."),
                              style = "padding-left: 20px;"),
                            sliderInput("child_garden_health_care_savings",
                                        NULL,
                                        min = 1,
                                        max = 1000,
                                        value = c(10, 500),
                                        step = 1),
                            h5(HTML("<b>11-2. School performance value for children making better choices about food with a passive (no STEM) garden (million VND/year):</b>")),
                            p(HTML("Please estimate the annual monetary value of an improved school performance for children having a school garden and making better food choices
                                   <b>(no STEM)</b> (in million VND/ year)."),
                              style = "padding-left: 20px;"),
                            sliderInput("child_garden_school_performance_value",
                                        NULL,
                                        min = 1,
                                        max = 500,
                                        value = c(21, 200),
                                        step = 1),
                            h5(HTML("<b>11-3. Community engagement value for children making better choices about food with a passive (no STEM) garden (million VND/year):</b>")),
                            p("Please estimate the annual monetary value of a more active social engagement for children having a school garden and making better food choices
                              <b>(no STEM)</b> (in million VND/ year).",
                              style = "padding-left: 20px;"),
                            sliderInput("child_garden_community_engagement_value",
                                        NULL,
                                        min = 1,
                                        max = 50,
                                        value = c(3, 7),
                                        step = 1),
                            p(),
                            p(),
                            h4(strong("With a STEM teaching approach")),
                            
                            h5(HTML("<b>11-4. Healthcare savings from children making better choices about food with STEM garden (million VND/year):</b>")),
                            p("Please estimate the annual monetary value of healthcare savings for children having a school garden and making better food choices 
                              (in million VND/ year). Here, the school garden is <b>integrated into STEM</b> teaching.",
                              style = "padding-left: 20px;"),
                            sliderInput("child_STEM_health_care_savings",
                                        NULL,
                                        min = 1,
                                        max = 500,
                                        value = c(10, 80),
                                        step = 1),
                            h5(HTML("<b>11-5. School performance value for children making better choices about food with a STEM garden (million VND/year):</b>")),
                            p("Please estimate the annual monetary value of an improved school performance for children having a school garden and making better food 
                              choices (no STEM) (in million VND/ year). Here, the school garden is <b>integrated into STEM</b> teaching.",
                              style = "padding-left: 20px;"),
                            sliderInput("child_STEM_school_performance_value",
                                        NULL,
                                        min = 1,
                                        max = 500,
                                        value = c(2, 100),
                                        step = 1),
                            h5(HTML("<b>11-6. Community engagement value for children making better choices about food with a STEM garden (million VND/year):</b>")),
                            p("Please estimate the annual monetary value of a more active social engagement for children having a school garden and making better food 
                              choices (no STEM) (in million VND/ year). Here, the school garden is <b>integrated into STEM</b> teaching.",
                              style = "padding-left: 20px;"),
                            sliderInput("child_STEM_community_engagement_value",
                                        NULL,
                                        min = 1,
                                        max = 500,
                                        value = c(10, 250),
                                        step = 1)
                            
                            
               ),
               #12. ...####
               h3(strong('12. ... ')),
               sidebarPanel(width = 12,
                            style = "height: 60vh; overflow-y: auto;", #height in %
                            
                            h5(HTML("<b>12-1. Value of green space (million VND/year):</b>")),
                            p("Please estimate the annual  value of having a habitat for plants and animals (million VND/year).",
                              style = "padding-left: 20px;"),
                            sliderInput("green_space_eco_value",
                                        NULL,
                                        min = 1,
                                        max = 50,
                                        value = c(1, 10),
                                        step = 1),
                            h5(HTML("<b>12-2. Value of reduced polution on school garden (million VND/year):</b>")),
                            p("Please estimate the annual value of  reduced pollution (million VND/year).",
                              style = "padding-left: 20px;"),
                            sliderInput("reduce_pollution_value",
                                        NULL,
                                        min = 1,
                                        max = 50,
                                        value = c(1, 3),
                                        step = 1),
                           
                            p(),
                            p(),
                            h4(strong("School events")),
                            h5(HTML("<b>12-3. Value of garden related school events (million VND/year):</b>")),
                            p("Please estimate the value of one (1) garden related school event for the school and community (million VND/event)",
                              style = "padding-left: 20px;"),
                            sliderInput("school_event_value",
                                        NULL,
                                        min = 1,
                                        max = 500,
                                        value = c(10, 200),
                                        step = 1),
                            h5(HTML("<b>12-4. Number of school events per year (days per year):</b>")),
                            p("Please estimate the number of garden related school events per year (days per year)",
                              style = "padding-left: 20px;"),
                            sliderInput("school_event_freq",
                                        NULL,
                                        min = 1,
                                        max = 50,
                                        value = c(3, 10),
                                        step = 1),
                            p(),
                            p(),
                            h4(strong("Land Use")),
                            h5(HTML("<b>12-5. Value of non garden land use, playground etc. (million VND/yr):</b>")),
                            p("Please estimate the annual value of the land as it is used right now (e.g. playground for kids)",
                              style = "padding-left: 20px;"),
                            sliderInput("value_of_non_garden_land_use",
                                        NULL,
                                        min = 1,
                                        max = 100,
                                        value = c(20, 50),
                                        step = 1),
                            h5(HTML("<b>12-6. Cost of non garden land use (million VND/yr):</b>")),
                            p("Please estimate the annual  costs of using the land area as parking lot (e.g. costs for leveling, 
                              fencing, cleaning?) (million VND/year)",
                              style = "padding-left: 20px;"),
                            sliderInput("costs_of_non_garden_land_use",
                                        NULL,
                                        min = 1,
                                        max = 100,
                                        value = c(1, 5),
                                        step = 1),
                            p(),
                            p(),
                            h4(strong("Parking")),
                            h5(HTML("<b>12-7. Chance of including parking on the plot without a garden (%):</b>")),
                            p("How do you rate the likelihood that the land area can generate alternative incomes 
                              e.g. if it is used as a parking lot (in %).",
                              style = "padding-left: 20px;"),
                            sliderInput("if_parking",
                                        NULL,
                                        min = 1,
                                        max = 100,
                                        value = c(10, 80),
                                        step = 1),
                            h5(HTML("<b>12-8. Above table value of parking (million VND/yr):</b>")),
                            p("Please estimate the annual (above table) value of using the land area 
                              as parking lot (million VND/year).",
                              style = "padding-left: 20px;"),
                            sliderInput("parking_value",
                                        NULL,
                                        min = 0.1,
                                        max = 10,
                                        value = c(0.2, 3),
                                        step = 0.1)
                            
                            
               ), # close sidebarPanel 12. 
               #13. variables in Code but not in previous Questionnaires ####
               h3(strong('13. variables in Code but not in previous Questionnaires')),
               p("Do you want to put this in Shiny as well?"),
               sidebarPanel(width = 12,
                            style = "height: 40vh; overflow-y: auto;", #height in %
                            
                            h5(HTML("<b>13-1. Chance of school choosing to integrate animals in garden (%):</b>")),
                            p("explanation",
                              style = "padding-left: 20px;"),
                            sliderInput("if_animals_in_garden",
                                        NULL,
                                        min = 1,
                                        max = 100,
                                        value = c(20, 80),
                                        step = 1),
                            h5(HTML("<b>13-2. Digging a fish pond in the garden (million VND):</b>")),
                            p("explanation",
                              style = "padding-left: 20px;"),
                            sliderInput("fishpond_cost",
                                        NULL,
                                        min = 1,
                                        max = 100,
                                        value = c(7, 10),
                                        step = 1),
                            h5(HTML("<b>13-3. Chance that the garden space is fallow green space (%):</b>")),
                            p("explanation",
                              style = "padding-left: 20px;"),
                            sliderInput("chance_garden_is_fallow_green_space",
                                        NULL,
                                        min = 0.1,
                                        max = 100,
                                        value = c(0.2, 5),
                                        step = 0.1),
                            h5(HTML("<b>13-4. Proportion of value of fallow greenspace compared to garden (%):</b>")),
                            p("explanation",
                              style = "padding-left: 20px;"),
                            sliderInput("fallow_eco_reduction",
                                        NULL,
                                        min = 1,
                                        max = 100,
                                        value = c(50, 80),
                                        step = 1),
                            h5(HTML("<b>13-5. Value of non-garden green space for child health (million VND/year):</b>")),
                            p("explanation",
                              style = "padding-left: 20px;"),
                            sliderInput("green_space_health_value",
                                        NULL,
                                        min = 1,
                                        max = 50,
                                        value = c(1, 10),
                                        step = 1),
                            h5(HTML("<b>13-6. Proportion of value of fallow greenspace for child heatlh compared to garden (%):</b>")),
                            p("explanation",
                              style = "padding-left: 20px;"),
                            sliderInput("fallow_health_reduction",
                                        NULL,
                                        min = 1,
                                        max = 100,
                                        value = c(50, 80),
                                        step = 1),
                            h5(HTML("<b>13-7. Chance that the school has acess to land (%):</b>")),
                            p("explanation",
                              style = "padding-left: 20px;"),
                            sliderInput("land_access",
                                        NULL,
                                        min = 1,
                                        max = 100,
                                        value = c(60, 95),
                                        step = 1),
                            h5(HTML("<b>13-8. Chance that the land at the school is suitable (%):</b>")),
                            p("explanation",
                              style = "padding-left: 20px;"),
                            sliderInput("suitability_of_land_for_garden",
                                        NULL,
                                        min = 1,
                                        max = 100,
                                        value = c(60, 95),
                                        step = 1),
                            h5(HTML("<b>13-9. Chance that beurocratic barriers will inhibit the process (%):</b>")),
                            p("explanation",
                              style = "padding-left: 20px;"),
                            sliderInput("beurocratic_barriers",
                                        NULL,
                                        min = 1,
                                        max = 100,
                                        value = c(1, 50),
                                        step = 1)
               ) # close sidebarPanel 13.
               ), #close left colum
               #Results####
               column(5, class = "main-panel",
                      style = "height: 100vh; overflow-y: auto;",
                      wellPanel(
                      p("It can take a few seconds to show plots with your input data."),
                      h3( strong('Results')),
                      h5(strong("1. The Net Present Value (NPV)")),
                      plotOutput("distPlot1",height = "250px",
                                 width = "95%"),
                      p(strong("Fig.1 Probabilistic outcome distributions from Monte Carlo simulation for baseline and intervention.")),
                      p ("The graph above provides a visual comparison of the outcome distributions
                          for the baseline and intervention options in terms of net present value
                          (NPV in €/ha) over the simulated period. The x-axis represents the NPV,
                          calculated as the sum of discounted cash flows for each simulation year.
                          The y-axis shows the probability of occurrence, indicating the likelihood
                          of different NPV values. A higher value on the y-axis corresponds to a
                         greater probability of the associated NPV on the x-axis."),
                      # h5("2.Title"),
                      # plotOutput("distPlot2",height = "250px",
                      #            width = "95%"),
                      # p("explanation"), 
                      # h5(strong("2. Cash flow")),
                      # plotOutput("distPlot3",height = "250px",
                      #            width = "95%"),
                      # p("Der Cashflow ist eine Reihe von Geldbeträgen, d
                      #     ie entweder negativ (z. B. anfängliche Investitionskosten von 
                      #     Maßnahmen) oder positiv (z. B. zusätzlicher Umsatz, der durch 
                      #     Maßnahmen in einem bestimmten Jahr erzielt wird) über einen 
                      #     bestimmten Zeitraum sind.
                      #     In der Abbildung wird die Unsicherheit 
                      #     des Cashflows durch Quantile um den Median herum dargestellt.")
                      # 
                      
                      )# close wellPanel       
               ) # close the right side column
             ), #close FluidRow
             
             fluidRow(
               sidebarPanel(width = 12,
                            fluidRow( h3( strong('More Idea')),
                                      textAreaInput(width = "100%", 
                                                    height ='100px',
                                                    "Improvement", 
                                                    "Please, give us more idea to improve our model.")),
                            fluidRow(align = "left",
                                     
                               #save & download file####
                               # Single button for saving and downloading
                              h5( strong('Please save and download your file')),      
                              downloadButton('saveDownload', 'Save & Download'))
                            ),#close sidebarPanel
               fluidRow( width = 10, 
                         div(style = "background-image: url('favri_1.jpg'); 
                        background-size: cover; 
                        height: 100vh; 
                        color: white;")),
               fluidRow(column(width = 2,
                               align = "right",
                               tags$a(href = "https://codas.vn/?lang=en",
                                      tags$img(src = "Logo/CODAS.png", height = "70px"))
                               ),
                       column(width = 7,  # Center alignment logic with empty space between logos
                              align = "center",
                              ""
                               ),      
                        column( width = 3,
                                align = "right",
                                tags$a(tags$img(src = "Logo/BMEL.png", height = "90px"))
                               ))

             )# close last fluidRow
             
            
             
) # closed tabPanel
) # closed tabsetPanel

)# UI closed

# Define server logic required to draw a histogram


server <- function(input, output, session) {
  library(decisionSupport)
  library(ggplot2)
  # observeEvent(input$toggleSidebar, {
  #   toggle("sidebar")  # Toggles the visibility of the sidebar
  # })
  # Track sidebar visibility
  sidebar_visible <- reactiveVal(TRUE)
  
  observeEvent(input$toggleSidebar, {
    toggle("sidebar")
    sidebar_visible(!sidebar_visible())
    
    if (sidebar_visible()) {
      updateActionButton(session, "toggleSidebar", label = "Hide Sidebar",icon = icon("fa-light fa-eye-slash"))
      shinyjs::removeClass(selector = "#toggleSidebar", class = "closed")
    } else {
      updateActionButton(session, "toggleSidebar", label = "Show 1. Introduction",icon = icon("fa-regular fa-face-grin-stars"))
      shinyjs::addClass(selector = "#toggleSidebar", class = "closed")
    }
  })
  
  #output ####
  output$distPlot1 <- renderPlot({
  #input table: variables ####  
    input_estimates <- data.frame(variable = c(
      #1. Introduction
      "number_of_years","discount_rate","CV_value", "inflation_rate",
      # 2. Estimation
      "size_of_garden","expensive_garden_size","cost_increase_expensive_garden_size",
      # 3. Estimation if... 
      "if_students_like", "if_parents_like","if_community_likes","if_effective_manage",
      "if_garden_yield_enough","if_garden_healthy", "if_teachers_like", "if_effective_teaching",
      "if_effective_training","if_offer_green_space", "if_reduce_pollution", "if_biophysical_good",
      # 4. Investment
      "equipment_cost", "construction_cost", "garden_designing_costs", "teacher_training_cost",
      "school_board_planning", "teaching_equipment", "compost_starting", "worm_starting",
      "livestock_establishment_costs", "if_family_pays_establishment", "establishment_family_portion_paid", 
      # 5. running cost
      "maintaining_labor", "teacher_salary_cost", "teaching_equipment_annual", "teaching_tools",
      "seed_costs", "fertilizer", "plant_protection", "livestock_maint", 
      "annual_teacher_training",
      # 6. Canteen
      "if_school_has_canteen","canteen_savings", "sale_of_yield",
      # 7. Potential cost
      "extra_cirricular_savings", "formal_edu_savings", "formal_edu_savings_STEM",
      # 8. Esimation of potential investment
      "outside_investment_value", "outside_investment_value_STEM","increased_enrollment_value","increased_enrollment_value_STEM",
      # 9. Assumption: child_veg
      "child_veg_health_care_savings", "child_veg_school_performance_value", "child_veg_community_engagement_value",
      # 10. mental health
      "garden_mental_health_value",
      # 11. with and without STEM
      "child_garden_health_care_savings", "child_garden_school_performance_value", "child_garden_community_engagement_value", "child_STEM_health_care_savings",
      "child_STEM_school_performance_value", "child_STEM_community_engagement_value",
      #12. ...
      "green_space_eco_value", "reduce_pollution_value", "school_event_value", "school_event_freq",
      "value_of_non_garden_land_use", "costs_of_non_garden_land_use","if_parking", "parking_value", 
      # 13. ... 
      "if_animals_in_garden", "fishpond_cost", "chance_garden_is_fallow_green_space", "fallow_eco_reduction",
      "green_space_health_value", "fallow_health_reduction", "land_access", "suitability_of_land_for_garden",
      "beurocratic_barriers"
    ),
      
    lower<- c(
      #1.Intoduction
      min(input$number_of_years), min(input$discount_rate), (min(input$CV_value)/100), min(input$inflation_rate),
      #2. general estimation
      min(input$size_of_garden), min(input$expensive_garden_size), min(input$cost_increase_expensive_garden_size),
      # 3. general estimation
      (min(input$if_students_like)/100), (min(input$if_parents_like)/100), (min(input$if_community_likes)/100), (min(input$if_effective_manage)/100),
      (min(input$if_garden_yield_enough)/100), (min(input$if_garden_healthy)/100), (min(input$if_teachers_like)/100), (min(input$if_effective_teaching)/100),
      (min(input$if_effective_training)/100), (min(input$if_offer_green_space)/100), (min(input$if_reduce_pollution)/100), (min(input$if_biophysical_good)/100),
      #4. investment
      min(input$equipment_cost), min(input$construction_cost), min(input$garden_designing_costs), min(input$teacher_training_cost),
      min(input$school_board_planning), min(input$teaching_equipment), min(input$compost_starting), min(input$worm_starting),
      min(input$livestock_establishment_costs), (min(input$if_family_pays_establishment)/100), (min(input$establishment_family_portion_paid)/100),
      # 5. running costs
      min(input$maintaining_labor), min(input$teacher_salary_cost), min(input$teaching_equipment_annual), min(input$teaching_tools),
      min(input$seed_costs), min(input$fertilizer), min(input$plant_protection), min(input$livestock_maint), 
      min(input$annual_teacher_training),
      # 6. Canteen
      (min(input$if_school_has_canteen)/100), min(input$canteen_savings), min(input$sale_of_yield),
      # 7. potential cost 
      min(input$extra_cirricular_savings), min(input$formal_edu_savings), min(input$formal_edu_savings_STEM),
      #8. Estimation of ... 
      min(input$outside_investment_value), min(input$outside_investment_value_STEM), min(input$increased_enrollment_value), min(input$increased_enrollment_value_STEM),
      # 9. assumption:...
      min(input$child_veg_health_care_savings), min(input$child_veg_school_performance_value), min(input$child_veg_community_engagement_value),
      # 10. mental
      min(input$garden_mental_health_value),
      # 11. estimation STEM
      min(input$child_garden_health_care_savings), min(input$child_garden_school_performance_value), min(input$child_garden_community_engagement_value), min(input$child_STEM_health_care_savings),
      min(input$child_STEM_school_performance_value),  min(input$child_STEM_community_engagement_value),
      # 12. ...
      min(input$green_space_eco_value), min(input$reduce_pollution_value), min(input$school_event_value), min(input$school_event_freq),
      min(input$value_of_non_garden_land_use), min(input$costs_of_non_garden_land_use),(min(input$if_parking)/100), min(input$parking_value), 
      
      # 13. ... 
      (min(input$if_animals_in_garden)/100), min(input$fishpond_cost), (min(input$chance_garden_is_fallow_green_space)/100), (min(input$fallow_eco_reduction)/100),
      min(input$green_space_health_value), (min(input$fallow_health_reduction)/100), (min(input$land_access)/100), (min(input$suitability_of_land_for_garden)/100),
      (min(input$beurocratic_barriers)/100)
    ),
    
    median <- NA,
    upper<- c(
      #1.Intoduction
      max(input$number_of_years), max(input$discount_rate), (max(input$CV_value)/100), max(input$inflation_rate),
      #2. general estimation
      max(input$size_of_garden), max(input$expensive_garden_size), max(input$cost_increase_expensive_garden_size),
      # 3. general estimation
      (max(input$if_students_like)/100), (max(input$if_parents_like)/100), (max(input$if_community_likes)/100), (max(input$if_effective_manage)/100),
      (max(input$if_garden_yield_enough)/100), (max(input$if_garden_healthy)/100), (max(input$if_teachers_like)/100), (max(input$if_effective_teaching)/100),
      (max(input$if_effective_training)/100), (max(input$if_offer_green_space)/100), (max(input$if_reduce_pollution)/100), (max(input$if_biophysical_good)/100),
      #4. investment
      max(input$equipment_cost), max(input$construction_cost), max(input$garden_designing_costs), max(input$teacher_training_cost),
      max(input$school_board_planning), max(input$teaching_equipment), max(input$compost_starting), max(input$worm_starting),
      max(input$livestock_establishment_costs), (max(input$if_family_pays_establishment)/100), (max(input$establishment_family_portion_paid)/100),
      # 5. running costs
      max(input$maintaining_labor), max(input$teacher_salary_cost), max(input$teaching_equipment_annual), max(input$teaching_tools),
      max(input$seed_costs), max(input$fertilizer), max(input$plant_protection), max(input$livestock_maint), 
      max(input$annual_teacher_training),
      # 6. Canteen
      (max(input$if_school_has_canteen)/100), max(input$canteen_savings), max(input$sale_of_yield),
      # 7. potential cost 
      max(input$extra_cirricular_savings), max(input$formal_edu_savings), max(input$formal_edu_savings_STEM),
      #8. Estimation of ... 
      max(input$outside_investment_value), max(input$outside_investment_value_STEM), max(input$increased_enrollment_value), max(input$increased_enrollment_value_STEM),
      # 9. assumption:...
      max(input$child_veg_health_care_savings), max(input$child_veg_school_performance_value), max(input$child_veg_community_engagement_value),
      # 10. mental
      max(input$garden_mental_health_value),
      # 11. estimation STEM
      max(input$child_garden_health_care_savings), max(input$child_garden_school_performance_value), max(input$child_garden_community_engagement_value), max(input$child_STEM_health_care_savings),
      max(input$child_STEM_school_performance_value),  max(input$child_STEM_community_engagement_value),
      # 12. ...
      max(input$green_space_eco_value), max(input$reduce_pollution_value), max(input$school_event_value), max(input$school_event_freq),
      max(input$value_of_non_garden_land_use), max(input$costs_of_non_garden_land_use), (max(input$if_parking)/100), max(input$parking_value), 
      
      # 13. ... 
      (max(input$if_animals_in_garden)/100), max(input$fishpond_cost), (max(input$chance_garden_is_fallow_green_space)/100), (max(input$fallow_eco_reduction)/100),
      max(input$green_space_health_value), (max(input$fallow_health_reduction)/100), (max(input$land_access)/100), (max(input$suitability_of_land_for_garden)/100),
      (max(input$beurocratic_barriers)/100)
    ),
    distribution = c(
      #1.Intoduction
      "const", "posnorm", "tnorm_0_1", "posnorm", 
      #2. general estimation
      "posnorm", "posnorm", "posnorm", 
      # 3. general estimation
      "tnorm_0_1", "tnorm_0_1", "tnorm_0_1", "tnorm_0_1",
      "tnorm_0_1", "tnorm_0_1", "tnorm_0_1", "tnorm_0_1", 
      "tnorm_0_1", "tnorm_0_1", "tnorm_0_1", "tnorm_0_1", 
      #4. investment
      "posnorm", "posnorm", "posnorm", "posnorm",
      "posnorm", "posnorm", "posnorm", "posnorm",
      "posnorm", "tnorm_0_1", "tnorm_0_1",
      # 5. running costs
      "posnorm", "posnorm", "posnorm", "posnorm",
      "posnorm", "posnorm", "posnorm", "posnorm",
      "posnorm",
      # 6. Canteen
      "tnorm_0_1", "posnorm", "posnorm",
      # 7. potential cost 
      "posnorm", "posnorm", "posnorm",
      #8. Estimation of ... 
      "posnorm", "posnorm", "posnorm", "posnorm",
      # 9. assumption:...
      "posnorm", "posnorm", "posnorm", 
      # 10. mental
      "posnorm",
      # 11. estimation STEM
      "posnorm", "posnorm", "posnorm", "posnorm",
      "posnorm", "posnorm",
      # 12. ...
      "posnorm", "posnorm", "posnorm", "posnorm",
      "posnorm", "posnorm", "tnorm_0_1","posnorm",
      # 13. ... 
      "tnorm_0_1", "posnorm","tnorm_0_1", "tnorm_0_1",
      "posnorm", "tnorm_0_1","tnorm_0_1", "tnorm_0_1",
      "tnorm_0_1"),
    
    label = c(
      #1.Intoduction
      "Number of years for garden simulation",
      "Discounting factor",
      "Coefficient of variation for our school garden intervention (%)",
      "Inflation rate (%)",
      #2. general estimation
      "Size of school garden in (m2)",
      "Cut off value for where the garden becomes more expensive (m2)",
      "Percentage more expensive if garden is beyond the expensive_garden_size",
      
      # 3. general estimation
      "Chance of student engagement (%)",
      "Chance of parents support / effectiveness (%)",
      "Chance of community support (%)",
      "Chance of effective garden management (%)",
      "Chance of sufficient yield from garden (%)",
      "Chance of healthy food from garden (%)",
      "Chance of teacher engagement (%)",
      "Chance of high education quality / effectiveness (%)",
      "Chance of effective training for teachers (%)",
      "Chance of garden having ecologically valuable green space (%)",
      "Chance of garden reducing polution (%)",
      "Chance of biophysical not damaging (i.e. weather) (%)",
      
      #4. investment
      "Costs of equipment for setting up garden (million VND)",
      "Costs of construction for setting up garden (million VND)",
      "Costs of design team consultant (million VND)",
      "Costs of training teachers when setting up garden (million VND)",
      "Costs of planning meetings (million VND)",
      "Equipment for teaching (million VND)",
      "Starting compost (million VND)",
      "Starting worms for compost (million VND)",
      "Starting animals in the garden (million VND)",
      "Chance that families donate to establishment (%)",
      "Portion of establishment costs donated by families (%)",
      # 5. running costs
      "Annual Labor cost to maintain school garden (million VND/yr)",
      "Additional teacher salary costs (million VND/yr)",
      "Teaching equipment / maintaining microscopes etc. (million VND/yr)",
      "Teaching tools / paper etc. (million VND/yr)",
      "Seeds and seedlings (million VND/yr)",
      "Fertilizer i.e. EM to add to compost (million VND/yr)",
      "Integrated Pest Management (IPM) (million VND/yr)",
      "Maintaining animals (million VND/yr)",
      "Maintaining teacher training (million VND/yr)",  
      # 6. Canteen
      "Chance that the school has a canteen (%)",
      "Canteen savings (million VND/yr)",
      "Sales of garden products (million VND/yr)",
      
      # 7. potential cost 
      "Savings from extra-curricular activities (million VND/year)",
      "Savings on formal education costs (no STEM garden) (million VND/year)",
      "Savings on STEM formal education costs (million VND/year)",
      
      #8. Estimation of ... 
      "Outside investment value (reputation) garden (million VND/year)",
      "Outside investment value (reputation) STEM (million VND/year)",
      
      "Increased enrollment/tuition income (reputation) garden (million VND/year)",
      "Increased enrollment/tuition income (reputation) STEM (million VND/year)",
      
      # 9. assumption:...
      "Healthcare savings (child) access to garden (million VND/year)",
      "School performance (children) eating garden veg (million VND/year)",
      "Community engagement (children) eating garden veg (million VND/year)",
      
      # 10. mental
      "Mental health value of children/others having a garden at school (million VND/year)",
      
      # 11. estimation STEM
      "Healthcare savings (children) with garden (million VND/year)",
      "School performance value (children) with garden (million VND/year)",
      "Community engagement value (children) with garden (million VND/year)",
      
      "Healthcare savings (children) STEM garden (million VND/year)",
      "School performance value (children) STEM garden (million VND/year)",
      "Community engagement value (children) STEM garden (million VND/year)",
      
      # 12. ...
      "Value of green space (million VND/year)",
      "Value of reduced pollution on school garden (million VND/year)",
      "Value of garden-related school events (million VND/event)",
      "Number of school events per year (days/year)",
      
      "Value of non-garden land use, playground, etc. (million VND/yr)",
      "Cost of non-garden land use (million VND/yr)",
      "Chance of including parking on the plot without a garden (%)",
      "Value of parking (million VND/yr)",
      
      
      # 13. ... 
      "Chance of school choosing to integrate animals in garden (%)",
      "Digging a fish pond in the garden (million VND)",
      
      "Chance that the garden space is fallow green space (%)",
      "Proportion of value of fallow greenspace compared to garden (%)",
      "Value of non-garden green space for child health (million VND/year)",
      "Proportion of value of fallow greenspace for child health compared to garden (%)",
      
      "Chance that the school has access to land (%)",
      "Chance that the land at the school is suitable (%)",
      "Chance that bureaucratic barriers will inhibit the process (%)"
      
    )
    )# close data frame 
      
    input_estimates$lower <- as.numeric(input_estimates$lower) 
    input_estimates$upper <- as.numeric(input_estimates$upper) 
    

    # The model ####
    school_garden_function <- function(x, varnames){
      
      # Costs####
      
      # Establishment costs 
      
      # chance_event options: 
      # family contribution? Will they pay a bit? 
      family_pays_establishment_yes_no <- chance_event(if_family_pays_establishment) 
      # some above the table (mostly under the table)
      
      garden_construction_cost <- if (family_pays_establishment_yes_no == 1) {
        construction_cost * # labor cost (2-3 people/day) + machine cost to setup garden system
          (1-establishment_family_portion_paid) # the family pays a bit
      } else {
        construction_cost = construction_cost
      }
      
      ## Garden establishment ####
      
      garden_establishment_costs <- compost_starting + # getting started with the compost
        worm_starting + # maintaining the compost and breaking down residue
        garden_designing_costs + # garden design costs (hiring a planner) 
        equipment_cost + # this is a high value because we may need a lot of equipment, netting, trellis for plants to climb
        # considering the range, this could be simple or a smart system (full automation)... 
        garden_construction_cost  
      
      # garden establishment cost values based on garden land area 
      # gardens are equally expensive but get more expensive if they are really big
      if (size_of_garden > expensive_garden_size) {
        # of the garden size is large then increase the establishment costs
        # increase by 
        garden_establishment_costs <- garden_establishment_costs * cost_increase_expensive_garden_size
      } else {
        garden_establishment_costs <- garden_establishment_costs 
      }
      
      ## STEM Garden establishment ####
      #costs if with STEM education
      STEM_establishment_costs <- teaching_equipment + # teaching equipment for sciences (science oriented training)
        # includes microscopes and other highly technical education equipment
        # consider 'if else' for aquatic vs. soil vs. rooftop in available space 
        # (not all have soil but all have space)
        teacher_training_cost  # cost for training teacher on gardening
      # this is low because we see it as a benefit partly because of 
      # training for the teachers in STEM and other topics like transdiscipinary and other topics
      # we save time and money on the training, which would otherwise have been spent on other training
      # teacher's cn also save money on other training courses for these topics 
      # that they otherwise would have had to take
      # requires training on 5 or 7 subjects (biology etc.) for 12 days
      
      # cut-off values for number of students
      
      #establishment costs if passive (no STEM) education ####
      establishment_cost_year_one <- school_board_planning + 
        garden_establishment_costs
      #establishment costs if with STEM education ####
      establishment_cost_year_one_STEM <- school_board_planning + 
        garden_establishment_costs + 
        STEM_establishment_costs 
      
      # Maintenance costs ####
      
      # maintenance costs for the garden (with or without STEM)
      garden_maintenance_cost <-
        maintaining_labor + # technical staff etc
        # 2-3 hours per day to manage a garden of this rough size
        seed_costs + # seeds and seedlings each year
        fertilizer + # EM and other helpers for compost
        plant_protection  # IPM for plant protection
      
      # garden maint. cost values based on garden land area 
      # gardens will be equally expensive to maintain (see above)
      if (size_of_garden > expensive_garden_size) {
        # if the garden size is large then increase the establishment costs
        # increase by variable cost_increase_expensive_garden_size
        garden_maintenance_cost <- garden_maintenance_cost * cost_increase_expensive_garden_size
      } else {
        garden_maintenance_cost <- garden_maintenance_cost  
      }
      
      ## maintenance costs if with STEM education
      STEM_maintenance_cost <-
        teacher_salary_cost +  # extra costs for teachers to work on the garden
        annual_teacher_training + # annual teacher training 12 days on 6 subjects
        # low because it is run by the teachers who have already been trained
        teaching_equipment_annual + # reagents, colors, paper, apps
        teaching_tools # children's garden tools, gloves, hoes, basket etc.
      
      # annual maintenance costs if passive (no STEM) education
      maintenance_cost_annual <- garden_maintenance_cost +
        # still need children's garden tools, gloves, hoes, basket etc.
        teaching_tools +
        # annual teacher training just for passive garden activity
        annual_teacher_training * 0.1
      
      ## annual maintenance costs if with STEM education
      maintenance_cost_annual_STEM <- garden_maintenance_cost +
        STEM_maintenance_cost
      
      # Add up all annual maintenance costs garden (no STEM)
      total_cost <- vv(maintenance_cost_annual, 
                       var_CV = CV_value, 
                       n = number_of_years, 
                       relative_trend = inflation_rate) #percentage of increase each year
      
      # Add up all annual maintenance costs garden with STEM
      total_cost_STEM <- vv(maintenance_cost_annual_STEM, 
                            var_CV = CV_value, 
                            n = number_of_years, 
                            relative_trend = inflation_rate) #percentage of increase each year
      
      # Calculate management plus establishment costs in the first year
      total_cost[1] <- establishment_cost_year_one + 
        maintenance_cost_annual #make sure the first is establishment_cost_year_one
      
      # Calculate management plus establishment costs in the first year with STEM
      total_cost_STEM[1] <- establishment_cost_year_one_STEM + 
        maintenance_cost_annual_STEM
      
      # if including animals garden prices change a bit ####
      # Circular garden with animals, trees, plants, fish (Bac Tom option)
      annual_livestock_cost  <- vv(livestock_maint, 
                                   var_CV = CV_value, 
                                   n = number_of_years, 
                                   relative_trend = inflation_rate) #percentage of increase each year
      
      if_animals_included <- chance_event(if_animals_in_garden)
      
      if (if_animals_included == 1){
        # costs of establishing animals in the garden (small birds, rabbits, fish)
        total_cost_STEM[1] <- total_cost_STEM[1] + livestock_establishment_costs + fishpond_cost
        total_cost[1] <- total_cost[1] + livestock_establishment_costs + fishpond_cost
      } else {
        total_cost_STEM = total_cost_STEM
        total_cost = total_cost
      }
      # Risks ####
      
      # These are 'ex-ante' risks, or risks understood when making a decision
      # we use these to multiply the values for the relevant benefits
      # the minimum values are effectively a reduction in the benefits
      # used to multiply benefits (by a number 90% likely)
      # not differentiated by passive and STEM education
      
      garden_function_risk <-  min(if_biophysical_good, 
                                   if_students_like, # damage garden
                                   if_parents_like, #  support
                                   if_community_likes, #damage garden
                                   if_effective_manage) # well managed garden
      
      garden_nutrition_risk <- min(if_students_like, # eat veg/change behavior
                                   if_garden_yield_enough, # goes to hh and school canteen
                                   if_parents_like, #  support and buy garden product
                                   if_garden_healthy, # good food from the garden
                                   if_effective_manage) # well managed garden
      # ex-ante education risks
      education_risk <- min(if_students_like, # pay attention
                            if_teachers_like,# teach effectively
                            if_parents_like,# Allow students to attend
                            if_effective_teaching, # closely related to the next
                            if_effective_training) # but possibly non-correlary
      
      # ex-ante community risks
      community_risk <- min(if_parents_like, #  support and promote
                            if_community_likes, # support and promote
                            if_effective_manage) # well managed garden makes good impression
      
      # ex-ante ecological risks
      ecological_risk <- min(if_offer_green_space, # offer green space
                             if_reduce_pollution) # offer habitat
      
      # Benefits and Risks ####
      
      canteen_yes_no <- chance_event(if_school_has_canteen) 
      # private schools have but others not so much
      # this will change under new decrees and nutrition plans
      
      # parents pay for the canteen food / the school will sell to parents
      if (canteen_yes_no == 1) {
        # sell some and eat rest in canteen 
        harvest_value = vv(canteen_savings + sale_of_yield, CV_value, 
                           number_of_years,
                           #inflation -> percentage of increase each year
                           relative_trend = inflation_rate) * garden_function_risk 
        # account for risk that the garden is not fully functional
      } else {
        # just sell, never eat in the canteen
        # same for no STEM and STEM
        harvest_value = vv(sale_of_yield, CV_value, 
                           number_of_years, 
                           relative_trend = inflation_rate) * garden_function_risk
      }
      
      # here we get a bit abstract but we do not want to leave this out
      # learning is worth something to us 
      # and we want to make sure this is part of the recommendation
      # we use things like savings on after school programs, tutor-ships,
      # hiring more teaching staff, organizing more events for kids
      # we call this 'extra_cirricular_savings'
      
      # education quality is correlated (highly) to the learning and to 
      # other values like outside investment (i.e. parents invest)
      # and increased enrollment by 
      # creating a good impression and gaining reputation
      
      # The savings are also in formal education 
      # the school meets some of the KPI for education with the garden
      # such as local enterprise and local economics 
      # Ministry decree of edu. to benefit local economy (35 sessions of 45 min./yr)
      # private school has more time than this 
      # this will be applied in the STEM case only 
      
      education_savings <- formal_edu_savings #(not much savings here with no STEM)
      
      education_savings_STEM <- formal_edu_savings_STEM + 
        extra_cirricular_savings
      
      #savings on learning
      learning_value <- vv(education_savings, 
                           CV_value, 
                           number_of_years, 
                           relative_trend = inflation_rate) * education_risk
      # Not until the 2nd year is the garden expected to start 'running well'
      # thus providing a learning value
      learning_value[1] <- 0
      #savings on learning with STEM education
      learning_value_STEM <- vv(education_savings_STEM, 
                                CV_value, 
                                number_of_years, 
                                relative_trend = inflation_rate) * education_risk
      
      # The 3rd year is when the STEM education plan will be fully running
      learning_value_STEM[1:2] <- 0
      
      # Reputation ####
      # Reputation for schools, teachers, school board, students…
      # through community building, green running award, 
      # planting trees, environment ecology groups
      # school events in garden connect community, leads to
      # greater access and awareness, positive change in choices around food
      
      # reputation is not important for any older schools
      # only new schools will care about reputation
      # old schools do not need reputation - they already get too many applications they cannot get more students
      # 
      # They do not get investment
      # the school belongs to big companies
      
      #investments from outside
      # i.e. sponsors from local business 
      outside_investment <- vv(outside_investment_value, # related to networking
                               CV_value, 
                               number_of_years, 
                               relative_trend = inflation_rate) * community_risk
      
      outside_investment_STEM <- vv(outside_investment_value_STEM, # related to networking
                                    CV_value, 
                                    number_of_years, 
                                    relative_trend = inflation_rate) * community_risk
      
      # Same for STEM and no STEM
      # the community appreciates the garden 
      # they come to the school and take part in school events
      # the school benefits from the event by selling products
      # maybe products from the garden or increased sales of other school products
      community_value <-  vv(school_event_value * school_event_freq, # i.e. seedlings for sale
                             CV_value, 
                             number_of_years, 
                             relative_trend = inflation_rate) * community_risk
      
      # Increased enrollment ####
      # earnings from increased enrollment without STEM
      increased_enrollment <-  vv(increased_enrollment_value,
                                  CV_value, 
                                  number_of_years, 
                                  relative_trend = inflation_rate) * education_risk
      
      # Increased enrollment with STEM
      increased_enrollment_STEM <-  vv(increased_enrollment_value,
                                       CV_value, 
                                       number_of_years, 
                                       relative_trend = inflation_rate) * education_risk
      
      # It takes time to get a good reputation
      # make the first year (unproductive year) 
      # and the second year (year of gaining reputation) zero
      increased_enrollment[1:2] <- 0 
      # with STEM it takes two years to get the fully functioning garden 
      # and thus to gain reputation
      increased_enrollment_STEM[1:3] <- 0 
      
      # Health related values ####
      # These are critical and extremely important but also somewhat intangible
      # here we determine the value of vegetable access with some proxy values
      child_veg_access <- child_veg_health_care_savings + 
        # access to and consumption of safe food (i.e. vegetables) from the garden
        # can lead to better performance
        child_veg_school_performance_value + 
        # value for children having more access to safe vegetables from the garden
        # as it relates to their engagement with the community
        child_veg_community_engagement_value 
      
      # here we determine the value of healthier choices with some proxy values
      child_healthier_choices <- child_garden_health_care_savings + 
        # children with a garden on campus may do better in school
        # here is the expected effect
        child_garden_school_performance_value + 
        child_garden_community_engagement_value  
      
      # Need to consider these values carefully as they differ between options
      # health benefits from gardens no STEM
      health_value <- child_veg_access + 
        child_healthier_choices  + 
        garden_mental_health_value 
      # can be expanded to include more: children, 
      # but also community, teachers, school staff and neighbors...
      
      health_related_value <-  vv(health_value, 
                                  CV_value, 
                                  number_of_years, 
                                  relative_trend = inflation_rate) * garden_nutrition_risk
      # health benefits from gardens with STEM
      # here we determine the value of healthier choices with some proxy values
      child_healthier_choices_STEM <- child_STEM_health_care_savings + 
        # making better choices about food leads to lower health care costs
        child_STEM_school_performance_value + 
        # for children making better choices about food 
        child_STEM_community_engagement_value  
      # Assuming more formal STEM education time in the garden leads to 
      # better health choices but does not change access (same garden)
      
      health_value_STEM <- child_veg_access + 
        child_healthier_choices_STEM  + 
        garden_mental_health_value
      
      health_related_value_STEM <-  vv(health_value_STEM, 
                                       CV_value, 
                                       number_of_years, 
                                       relative_trend = inflation_rate) * garden_nutrition_risk
      
      # Here we also get abstract, we care about green space and pollution reduction
      
      environmental_value <- 
        green_space_eco_value + # we care about the green space
        # citizens pay more to live close to green spaces
        # cities like Hanoi spend money on planting and maintaining parks and trees
        # - improve mental and physical health
        # - provide opportunities for physical activity and social interaction
        # - public green spaces have been linked to lower crime rates
        
        reduce_pollution_value 
      # i.e. improved air quality/ filter pollutants from the air
      # improving air quality and reducing the risk of respiratory problems
      # especially important in urban areas, where air pollution is high 
      #   - (March 5th 2024) Hanoi worst air pollution in world
      # cities are willing to invest in green areas for pollution 
      #   - (i.e. moss wall in Stuttgart)
      # Hanoi plants and maintains parks and trees (maybe partly for pollution?)
      # attract insects, birds, and other wildlife, contributing to local biodiversity.
      # beneficial microorganisms, promoting soil fertility and plant health.
      # attract butterflies and birds, enhancing the ecological value of the garden.
      # attract pollinators such as bees and butterflies, supporting the pollination of nearby plants 
      # improved air quality by absorbing pollutants and releasing oxygen.
      # provides an outdoor classroom for students to learn about ecosystems, plant life cycles, and the interdependence of living organisms.
      # encourages sustainable practices, such as composting, water conservation, and organic gardening methods.
      # composting systems, reduce organic waste and promoting the recycling of nutrient-rich materials back into the soil.
      # raise awareness about waste reduction and resource conservation.
      
      # some discussion of carbon credit values (not included)
      # garden ecological value - possible fallow land ecologcal value 
      # assuming it could be a green space
      # low chance since land is needed in the city
      
      environmental_value_of_fallow_green_space <- (green_space_eco_value + 
                                                      reduce_pollution_value) * 
        # fallow green space is less valuable than managed garden green space by x amount
        fallow_eco_reduction
      
      # Fallow land also has value to health (no garden health value)
      
      health_value_of_fallow_play_green_space <- (green_space_health_value + 
                                                    reduce_pollution_value) * 
        # fallow space has lower health value than managed garden space by x amount
        fallow_health_reduction
      
      fallow_land <- chance_event(chance = chance_garden_is_fallow_green_space, 
                                  value_if = 1, 
                                  value_if_not = 0)
      
      if(fallow_land == 1){
        environment_related_value <-  vv(environmental_value - 
                                           environmental_value_of_fallow_green_space, 
                                         CV_value, 
                                         number_of_years, 
                                         relative_trend = inflation_rate) * ecological_risk
      }else{
        environment_related_value <-  vv(environmental_value, 
                                         CV_value, 
                                         number_of_years, 
                                         relative_trend = inflation_rate) * ecological_risk
        # overwrite for chance / use as 'No garden' results
        environmental_value_of_fallow_green_space <- 0 
        health_value_of_fallow_play_green_space <- 0
      }
      
      # Add up all benefits ####
      total_benefit <- harvest_value + learning_value + 
        outside_investment + increased_enrollment + 
        # health_related_value + environment_related_value + 
        community_value
      
      # Add up all benefits with STEM ####
      total_benefit_STEM <- harvest_value + learning_value_STEM + 
        outside_investment_STEM + increased_enrollment_STEM + 
        # health_related_value_STEM + environment_related_value + 
        community_value
      
      
      ## Risks for Public Schools ####
      # Access to land 
      # unlikely to have access to land
      # many schools that attended CODAS meetings (follow up to the first workshop) 
      # did not have access to land
      stop_garden_no_land <- chance_event(1-land_access)
      
      # no benefits if public schools meet these challenges
      
      ## If no land then very little or no costs 
      ## just meetings and other small things
      ## also no benefits
      if (stop_garden_no_land == 1) {
        # no benefits from the garden
        total_benefit_public_school <- rep(0, number_of_years)
        total_cost_public_school <- rep(0, number_of_years)
        # no benefits from STEM
        total_benefit_STEM_public_school <- rep(0, number_of_years)
        total_cost_STEM_public_school <- rep(0, number_of_years)
        environment_related_value <- rep(0, number_of_years)
        health_related_value <- rep(0, number_of_years)
        health_related_value_STEM <- rep(0, number_of_years)
      } else {
        # costs and benefits are the same
        total_benefit_public_school <- total_benefit
        total_cost_public_school <- total_cost
        total_benefit_STEM_public_school <- total_benefit_STEM
        total_cost_STEM_public_school <- total_cost_STEM
        environment_related_value <- environment_related_value
        health_related_value <- health_related_value
        health_related_value_STEM <- health_related_value_STEM
      }
      
      # the land they have access to is just cement part of playground
      # or otherwise turns out to be unsuitable (rocks, marsh etc.)
      stop_garden_unsuitable_land <- chance_event(1-suitability_of_land_for_garden)
      
      # many of the schools (especially public schools) can be overwhelmed with bureaucracy
      # CODAS was unable to overcome the bureaucracy hurdles 
      # We (CODAS and NIFAM) were unable to partner with public schools
      stop_garden_beurocratic_barriers <- chance_event(beurocratic_barriers)
      
      ## if land turns out to be unsuitable after some investment 
      ## or bureaucratic barriers permit the use of the garden 
      ## then establishment costs are incurred
      ## but there are no returns 
      if (stop_garden_unsuitable_land == 1 | 
          stop_garden_beurocratic_barriers == 1) {
        # no benefits from the garden
        total_benefit_public_school <- rep(0, number_of_years)
        # costs only in year 1
        total_cost_public_school <- (total_cost[2:number_of_years]<-0)
        # no benefits from STEM
        total_benefit_STEM_public_school <- rep(0, number_of_years)
        # costs only in year 1
        total_cost_STEM_public_school <- (total_cost_STEM[2:number_of_years]<-0)
      } else {
        # costs and benefits are the same
        total_benefit_public_school <- total_benefit
        total_cost_public_school <- total_cost
        total_benefit_STEM_public_school <- total_benefit_STEM
        total_cost_STEM_public_school <- total_cost_STEM
      }
      
      # Final result of the costs and benefits no STEM
      garden_result <- total_benefit - total_cost
      
      # Final result of the costs and benefits STEM
      garden_result_STEM <- total_benefit_STEM - total_cost_STEM
      
      # Final result of the costs and benefits no STEM at public school
      garden_result_public_school <- total_benefit_public_school - total_cost_public_school
      
      # Final result of the costs and benefits STEM at public school
      garden_result_STEM_public_school <- total_benefit_STEM_public_school - total_cost_STEM_public_school
      
      # Alternative use of garden space ####
      ## land-use result = all costs and benefits
      # These are opportunity costs
      # i.e. the value of the next-highest-valued alternative use of the garden space
      
      # the above-board earnings from parking (much will be under the table)
      parking_on_campus <- chance_event(if_parking) # some above the table (mostly under the table)
      
      if (parking_on_campus == 1) {
        non_garden_value <- vv(value_of_non_garden_land_use + parking_value, 
                               # this is a contentious issue with a lot of discussion
                               # keeping a low value and low chance for now
                               CV_value, 
                               number_of_years, 
                               relative_trend = inflation_rate) 
      } else {
        non_garden_value <- vv(value_of_non_garden_land_use, #i.e. cost of other playground
                               CV_value, 
                               number_of_years, 
                               relative_trend = inflation_rate)
      }
      
      
      total_benefit_no <- vv(non_garden_value + # income loss of playground, parking etc.
                               school_board_planning, # time savings for board
                             var_CV = CV_value, 
                             n = number_of_years, 
                             relative_trend = inflation_rate)
      
      total_cost_no <- vv(costs_of_non_garden_land_use, # cleaning playground, managing parking etc.
                          var_CV = CV_value, 
                          n = number_of_years, 
                          relative_trend = inflation_rate)
      
      # subtract costs from benefits
      
      no_garden_result <- total_benefit_no - total_cost_no
      
      # NPV calculations
      # calculate the Net Present Value (NPV) with with the specified discount rate
      # the values include expected inflation so the discount already includes this
      # These represent the expected wins over doing nothing
      
      # NPV no intervention ####
      NPV_no_garden <-
        discount(x = no_garden_result, 
                 discount_rate = discount_rate, 
                 calculate_NPV = TRUE)
      
      # By including `- no_garden_result` we are calculating the expected gains
      
      NPV_garden <-
        discount(x = garden_result - no_garden_result, 
                 discount_rate = discount_rate, 
                 calculate_NPV = TRUE)
      
      NPV_garden_STEM <-
        discount(x = garden_result_STEM - no_garden_result, 
                 discount_rate = discount_rate, 
                 calculate_NPV = TRUE)
      
      NPV_garden_public_school <-
        discount(x = garden_result_public_school - no_garden_result, 
                 discount_rate = discount_rate, 
                 calculate_NPV = TRUE)
      
      NPV_garden_STEM_public_school <-
        discount(x = garden_result_STEM_public_school - no_garden_result, 
                 discount_rate = discount_rate, 
                 calculate_NPV = TRUE)
      
      biodiversity <- sum(environment_related_value) #assume the same ecology value
      health <- sum(health_related_value) 
      health_STEM <- sum(health_related_value_STEM)
      
      ### END of garden model script ####
      
      # Beware, if we do not name our outputs (left-hand side of the equal sign) 
      # in the return section, the variables will be called output_1, _2, etc.
      return(list(NPV_garden = NPV_garden,
                  # comparative results do - do nothing
                  NPV_garden_STEM = NPV_garden_STEM,
                  NPV_garden_public_school = NPV_garden_public_school,
                  NPV_garden_STEM_public_school = NPV_garden_STEM_public_school,
                  biodiversity = biodiversity,
                  health = health,
                  health_STEM = health_STEM,
                  # others
                  total_costs = sum(total_cost),
                  total_costs_STEM = sum(total_cost_STEM),
                  Cashflow_garden = garden_result, 
                  Cashflow_garden_STEM = garden_result_STEM, 
                  Cashflow_garden_public = garden_result_public_school, 
                  Cashflow_garden_STEM_public = garden_result_STEM_public_school))
    }
    
# simulation ####    
    set.seed(42)
    garden_simulation_results <- mcSimulation(
      estimate = as.estimate(input_estimates),
      model_function = school_garden_function,
      numberOfModelRuns = 1e4, #run 10,000 times
      functionSyntax = "plainNames")
    
   plot1 <-  plot_distributions(mcSimulation_object = garden_simulation_results, 
                       vars = c("NPV_garden_public_school", 
                                "NPV_garden_STEM_public_school"),
                       old_names = c("NPV_garden_public_school", "NPV_garden_STEM_public_school"),
                       new_names = c("NPV public school garden", "NPV public school garden with STEM"),
                       method = 'smooth_simple_overlay', 
                       base_size = 7, 
                       x_axis_name = "Comparative NPV outcomes")
    
   plot1
    
    
    
    
  })

# save and download dataframe ####
  Data <- data.frame()
  Results <- reactive({input_data <- (data.frame(variable = c(
    "Date","Organization","Name",
    #1. Introduction
    "number_of_years","discount_rate","CV_value", "inflation_rate",
    # 2. Estimation
    "size_of_garden","expensive_garden_size","cost_increase_expensive_garden_size",
    # 3. Estimation if... 
    "if_students_like", "if_parents_like","if_community_likes","if_effective_manage",
    "if_garden_yield_enough","if_garden_healthy", "if_teachers_like", "if_effective_teaching",
    "if_effective_training","if_offer_green_space", "if_reduce_pollution", "if_biophysical_good",
    # 4. Investment
    "equipment_cost", "construction_cost", "garden_designing_costs", "teacher_training_cost",
    "school_board_planning", "teaching_equipment", "compost_starting", "worm_starting",
    "livestock_establishment_costs", "if_family_pays_establishment", "establishment_family_portion_paid", 
    # 5. running cost
    "maintaining_labor", "teacher_salary_cost", "teaching_equipment_annual", "teaching_tools",
    "seed_costs", "fertilizer", "plant_protection", "livestock_maint", 
    "annual_teacher_training",
    # 6. Canteen
    "if_school_has_canteen","canteen_savings", "sale_of_yield",
    # 7. Potential cost
    "extra_cirricular_savings", "formal_edu_savings", "formal_edu_savings_STEM",
    # 8. Esimation of potential investment
    "outside_investment_value", "outside_investment_value_STEM","increased_enrollment_value","increased_enrollment_value_STEM",
    # 9. Assumption: child_veg
    "child_veg_health_care_savings", "child_veg_school_performance_value", "child_veg_community_engagement_value",
    # 10. mental health
    "garden_mental_health_value",
    # 11. with and without STEM
    "child_garden_health_care_savings", "child_garden_school_performance_value", "child_garden_community_engagement_value", "child_STEM_health_care_savings",
    "child_STEM_school_performance_value", "child_STEM_community_engagement_value",
    #12. ...
    "green_space_eco_value", "reduce_pollution_value", "school_event_value", "school_event_freq",
    "value_of_non_garden_land_use", "costs_of_non_garden_land_use","if_parking", "parking_value", 
    # 13. ... 
    "if_animals_in_garden", "fishpond_cost", "chance_garden_is_fallow_green_space", "fallow_eco_reduction",
    "green_space_health_value", "fallow_health_reduction", "land_access", "suitability_of_land_for_garden",
    "beurocratic_barriers",
    "Improvement"
  ),
  
  lower<- c(
    "","","",
    #1.Intoduction
    min(input$number_of_years), min(input$discount_rate), (min(input$CV_value)/100), min(input$inflation_rate),
    #2. general estimation
    min(input$size_of_garden), min(input$expensive_garden_size), min(input$cost_increase_expensive_garden_size),
    # 3. general estimation
    (min(input$if_students_like)/100), (min(input$if_parents_like)/100), (min(input$if_community_likes)/100), (min(input$if_effective_manage)/100),
    (min(input$if_garden_yield_enough)/100), (min(input$if_garden_healthy)/100), (min(input$if_teachers_like)/100), (min(input$if_effective_teaching)/100),
    (min(input$if_effective_training)/100), (min(input$if_offer_green_space)/100), (min(input$if_reduce_pollution)/100), (min(input$if_biophysical_good)/100),
    #4. investment
    min(input$equipment_cost), min(input$construction_cost), min(input$garden_designing_costs), min(input$teacher_training_cost),
    min(input$school_board_planning), min(input$teaching_equipment), min(input$compost_starting), min(input$worm_starting),
    min(input$livestock_establishment_costs), (min(input$if_family_pays_establishment)/100), (min(input$establishment_family_portion_paid)/100),
    # 5. running costs
    min(input$maintaining_labor), min(input$teacher_salary_cost), min(input$teaching_equipment_annual), min(input$teaching_tools),
    min(input$seed_costs), min(input$fertilizer), min(input$plant_protection), min(input$livestock_maint), 
    min(input$annual_teacher_training),
    # 6. Canteen
    (min(input$if_school_has_canteen)/100), min(input$canteen_savings), min(input$sale_of_yield),
    # 7. potential cost 
    min(input$extra_cirricular_savings), min(input$formal_edu_savings), min(input$formal_edu_savings_STEM),
    #8. Estimation of ... 
    min(input$outside_investment_value), min(input$outside_investment_value_STEM), min(input$increased_enrollment_value), min(input$increased_enrollment_value_STEM),
    # 9. assumption:...
    min(input$child_veg_health_care_savings), min(input$child_veg_school_performance_value), min(input$child_veg_community_engagement_value),
    # 10. mental
    min(input$garden_mental_health_value),
    # 11. estimation STEM
    min(input$child_garden_health_care_savings), min(input$child_garden_school_performance_value), min(input$child_garden_community_engagement_value), min(input$child_STEM_health_care_savings),
    min(input$child_STEM_school_performance_value),  min(input$child_STEM_community_engagement_value),
    # 12. ...
    min(input$green_space_eco_value), min(input$reduce_pollution_value), min(input$school_event_value), min(input$school_event_freq),
    min(input$value_of_non_garden_land_use), min(input$costs_of_non_garden_land_use),(min(input$if_parking)/100), min(input$parking_value), 
    
    # 13. ... 
    (min(input$if_animals_in_garden)/100), min(input$fishpond_cost), (min(input$chance_garden_is_fallow_green_space)/100), (min(input$fallow_eco_reduction)/100),
    min(input$green_space_health_value), (min(input$fallow_health_reduction)/100), (min(input$land_access)/100), (min(input$suitability_of_land_for_garden)/100),
    (min(input$beurocratic_barriers)/100),
    ""),
  
  median <- "",
  upper<- c(
    "","","",
    #1.Intoduction
    max(input$number_of_years), max(input$discount_rate), (max(input$CV_value)/100), max(input$inflation_rate),
    #2. general estimation
    max(input$size_of_garden), max(input$expensive_garden_size), max(input$cost_increase_expensive_garden_size),
    # 3. general estimation
    (max(input$if_students_like)/100), (max(input$if_parents_like)/100), (max(input$if_community_likes)/100), (max(input$if_effective_manage)/100),
    (max(input$if_garden_yield_enough)/100), (max(input$if_garden_healthy)/100), (max(input$if_teachers_like)/100), (max(input$if_effective_teaching)/100),
    (max(input$if_effective_training)/100), (max(input$if_offer_green_space)/100), (max(input$if_reduce_pollution)/100), (max(input$if_biophysical_good)/100),
    #4. investment
    max(input$equipment_cost), max(input$construction_cost), max(input$garden_designing_costs), max(input$teacher_training_cost),
    max(input$school_board_planning), max(input$teaching_equipment), max(input$compost_starting), max(input$worm_starting),
    max(input$livestock_establishment_costs), (max(input$if_family_pays_establishment)/100), (max(input$establishment_family_portion_paid)/100),
    # 5. running costs
    max(input$maintaining_labor), max(input$teacher_salary_cost), max(input$teaching_equipment_annual), max(input$teaching_tools),
    max(input$seed_costs), max(input$fertilizer), max(input$plant_protection), max(input$livestock_maint), 
    max(input$annual_teacher_training),
    # 6. Canteen
    (max(input$if_school_has_canteen)/100), max(input$canteen_savings), max(input$sale_of_yield),
    # 7. potential cost 
    max(input$extra_cirricular_savings), max(input$formal_edu_savings), max(input$formal_edu_savings_STEM),
    #8. Estimation of ... 
    max(input$outside_investment_value), max(input$outside_investment_value_STEM), max(input$increased_enrollment_value), max(input$increased_enrollment_value_STEM),
    # 9. assumption:...
    max(input$child_veg_health_care_savings), max(input$child_veg_school_performance_value), max(input$child_veg_community_engagement_value),
    # 10. mental
    max(input$garden_mental_health_value),
    # 11. estimation STEM
    max(input$child_garden_health_care_savings), max(input$child_garden_school_performance_value), max(input$child_garden_community_engagement_value), max(input$child_STEM_health_care_savings),
    max(input$child_STEM_school_performance_value),  max(input$child_STEM_community_engagement_value),
    # 12. ...
    max(input$green_space_eco_value), max(input$reduce_pollution_value), max(input$school_event_value), max(input$school_event_freq),
    max(input$value_of_non_garden_land_use), max(input$costs_of_non_garden_land_use),(max(input$if_parking)/100), max(input$parking_value), 
    
    # 13. ... 
    (max(input$if_animals_in_garden)/100), max(input$fishpond_cost), (max(input$chance_garden_is_fallow_green_space)/100), (max(input$fallow_eco_reduction)/100),
    max(input$green_space_health_value), (max(input$fallow_health_reduction)/100), (max(input$land_access)/100), (max(input$suitability_of_land_for_garden)/100),
    (max(input$beurocratic_barriers)/100),
    ""
  ),
  distribution = c(
    "","","",
    #1.Intoduction
    "const", "posnorm", "tnorm_0_1", "posnorm", 
    #2. general estimation
    "posnorm", "posnorm", "posnorm", 
    # 3. general estimation
    "tnorm_0_1", "tnorm_0_1", "tnorm_0_1", "tnorm_0_1",
    "tnorm_0_1", "tnorm_0_1", "tnorm_0_1", "tnorm_0_1", 
    "tnorm_0_1", "tnorm_0_1", "tnorm_0_1", "tnorm_0_1", 
    #4. investment
    "posnorm", "posnorm", "posnorm", "posnorm",
    "posnorm", "posnorm", "posnorm", "posnorm",
    "posnorm", "tnorm_0_1", "tnorm_0_1",
    # 5. running costs
    "posnorm", "posnorm", "posnorm", "posnorm",
    "posnorm", "posnorm", "posnorm", "posnorm",
    "posnorm",
    # 6. Canteen
    "tnorm_0_1", "posnorm", "posnorm",
    # 7. potential cost 
    "posnorm", "posnorm", "posnorm",
    #8. Estimation of ... 
    "posnorm", "posnorm", "posnorm", "posnorm",
    # 9. assumption:...
    "posnorm", "posnorm", "posnorm", 
    # 10. mental
    "posnorm",
    # 11. estimation STEM
    "posnorm", "posnorm", "posnorm", "posnorm",
    "posnorm", "posnorm",
    # 12. ...
    "posnorm", "posnorm", "posnorm", "posnorm",
    "posnorm", "posnorm", "tnorm_0_1","posnorm",
    # 13. ... 
    "tnorm_0_1", "posnorm","tnorm_0_1", "tnorm_0_1",
    "posnorm", "tnorm_0_1","tnorm_0_1", "tnorm_0_1",
    "tnorm_0_1",
    ""),
  
  label = c(
    (input$Date), (input$Organization), (input$Name),
    #1.Intoduction
    "Number of years for garden simulation",
    "Discounting factor",
    "Coefficient of variation for our school garden intervention (%)",
    "Inflation rate (%)",
    #2. general estimation
    "Size of school garden in (m2)",
    "Cut off value for where the garden becomes more expensive (m2)",
    "Percentage more expensive if garden is beyond the expensive_garden_size",
    
    # 3. general estimation
    "Chance of student engagement (%)",
    "Chance of parents support / effectiveness (%)",
    "Chance of community support (%)",
    "Chance of effective garden management (%)",
    "Chance of sufficient yield from garden (%)",
    "Chance of healthy food from garden (%)",
    "Chance of teacher engagement (%)",
    "Chance of high education quality / effectiveness (%)",
    "Chance of effective training for teachers (%)",
    "Chance of garden having ecologically valuable green space (%)",
    "Chance of garden reducing polution (%)",
    "Chance of biophysical not damaging (i.e. weather) (%)",
    
    #4. investment
    "Costs of equipment for setting up garden (million VND)",
    "Costs of construction for setting up garden (million VND)",
    "Costs of design team consultant (million VND)",
    "Costs of training teachers when setting up garden (million VND)",
    "Costs of planning meetings (million VND)",
    "Equipment for teaching (million VND)",
    "Starting compost (million VND)",
    "Starting worms for compost (million VND)",
    "Starting animals in the garden (million VND)",
    "Chance that families donate to establishment (%)",
    "Portion of establishment costs donated by families (%)",
    # 5. running costs
    "Annual Labor cost to maintain school garden (million VND/yr)",
    "Additional teacher salary costs (million VND/yr)",
    "Teaching equipment / maintaining microscopes etc. (million VND/yr)",
    "Teaching tools / paper etc. (million VND/yr)",
    "Seeds and seedlings (million VND/yr)",
    "Fertilizer i.e. EM to add to compost (million VND/yr)",
    "Integrated Pest Management (IPM) (million VND/yr)",
    "Maintaining animals (million VND/yr)",
    "Maintaining teacher training (million VND/yr)",  
    # 6. Canteen
    "Chance that the school has a canteen (%)",
    "Canteen savings (million VND/yr)",
    "Sales of garden products (million VND/yr)",
    
    # 7. potential cost 
    "Savings from extra-curricular activities (million VND/year)",
    "Savings on formal education costs (no STEM garden) (million VND/year)",
    "Savings on STEM formal education costs (million VND/year)",
    
    #8. Estimation of ... 
    "Outside investment value (reputation) garden (million VND/year)",
    "Outside investment value (reputation) STEM (million VND/year)",
    
    "Increased enrollment/tuition income (reputation) garden (million VND/year)",
    "Increased enrollment/tuition income (reputation) STEM (million VND/year)",
    
    # 9. assumption:...
    "Healthcare savings (child) access to garden (million VND/year)",
    "School performance (children) eating garden veg (million VND/year)",
    "Community engagement (children) eating garden veg (million VND/year)",
    
    # 10. mental
    "Mental health value of children/others having a garden at school (million VND/year)",
    
    # 11. estimation STEM
    "Healthcare savings (children) with garden (million VND/year)",
    "School performance value (children) with garden (million VND/year)",
    "Community engagement value (children) with garden (million VND/year)",
    
    "Healthcare savings (children) STEM garden (million VND/year)",
    "School performance value (children) STEM garden (million VND/year)",
    "Community engagement value (children) STEM garden (million VND/year)",
    
    # 12. ...
    "Value of green space (million VND/year)",
    "Value of reduced pollution on school garden (million VND/year)",
    "Value of garden-related school events (million VND/event)",
    "Number of school events per year (days/year)",
    
    "Value of non-garden land use, playground, etc. (million VND/yr)",
    "Cost of non-garden land use (million VND/yr)",
    "Chance of including parking on the plot without a garden (%)",
    "Value of parking (million VND/yr)",
    
    
    # 13. ... 
    "Chance of school choosing to integrate animals in garden (%)",
    "Digging a fish pond in the garden (million VND)",
    
    "Chance that the garden space is fallow green space (%)",
    "Proportion of value of fallow greenspace compared to garden (%)",
    "Value of non-garden green space for child health (million VND/year)",
    "Proportion of value of fallow greenspace for child health compared to garden (%)",
    
    "Chance that the school has access to land (%)",
    "Chance that the land at the school is suitable (%)",
    "Chance that bureaucratic barriers will inhibit the process (%)",
    (input$Improvement)
    
  )
  )) #close data frame
  colnames(input_data) <- c("variable", "lower", "median", "upper","distribution","label")
  return(input_data)
  })
  
  
 
  

  #Create a data save and download option ####
  
  output$saveDownload <- downloadHandler(            # Create the download file name
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      
      #Append the row in the dataframe
      Data <<- rbind(Data,Results()) 
      
      #Display the output in the table
      output$table <- renderTable(Data)
      write.csv(Data, file)                   # put Data() into the download file
    })                          
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
