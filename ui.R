# ui.R

shinyUI(pageWithSidebar(
  headerPanel("Ground Commander's Tool for Optimal Economic Growth"),
  
  sidebarPanel(
    helpText("Choose a a country of interest, and input your available Economic Development funds.
             "),
    
    selectInput("country", 
                label = "Country",
                choices = c("Afghanistan","Albania","Armenia","Azerbaijan","Belize","Benin","Bolivia","Bosnia and Herzegovina","Botswana","Bulgaria","Burkina Faso","Burundi","Cambodia","Cameroon","Chad","Chile","Colombia","Congo","Costa Rica","Dominican Republic","Ecuador","El Salvador","Gaza","Georgia","Ghana","Guatemala","Haiti","Honduras","India","Indonesia","Iraq","Israel","Jordan","Kenya","Kosovo","Kyrgyzstan","Lebanon","Liberia","Mali","Mexico","Moldova","Mongolia","Mozambique","Namibia","Nepal","Nicaragua","Nigeria","Pakistan","Palestine","Paraguay","Peru","Philippines","Rwanda","Samoa","Senegal","Sierra Leone","South Africa","South Sudan","Sri Lanka","Tajikistan","Tanzania","Thailand","The Democratic Republic of the Congo","Timor-Leste","Togo","Turkey","Uganda","Ukraine","United States","Vietnam","Yemen","Zambia","Zimbabwe"
                            ),
                selected = "Kenya"),
    numericInput("Funds", "Amount of Funds ($):", 10000),
    helpText(p("The purpose of this tool is to show a ground commander where to invest economic development 
             funds to achieve the best economic growth. The algorithm uses changes in microfinance loan size 
             by economic sector to develop a 'portfolio' recommendation. The recommendation points to the 
             sectors in a given country that tend to lead to better growth."),
             p("This tool (under the 'Compare Your Plan' tab) lets a commander see 
              how their planned allocation stacks up against the recommendation in terms of growth rate."),
             p("One final section is the 'Credit Check' tab where 
             a commander can find the historical default rate of a group of people (by country, gender and sector).")
    )),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Results",
               wellPanel(helpText("THIS TOOL IS IN DRAFT FORM: The information given in the chart 
                                  is growth rate compared to risk of the optimal protfolio and all 
                                  sectors.")),
               plotOutput("growthchart"),wellPanel(helpText("This chart shows of the average loan size over time within the selected country.")),
               plotOutput("sectorbycountryplot"),wellPanel(helpText("The table is the 
                                  recommended allocation (as a percentage,i.e. .75 = 75%) of 
                                  funds available to invest.  This allocation, if followed, will result
                                  in optimal economic growth.")),
               tableOutput("weighttable")),
      tabPanel("Asset Calulator", plotOutput("pie"),tableOutput("amounttable")),
      tabPanel("Compare Your Plan", wellPanel(helpText("Enter your planned allocation to 
                                                       see how it compares to the optimal 
                                                       growth recomendation")),
               numericInput("Agg", "Desired Investment in Agriculture ($):", 2000),
               numericInput("Edu", "Desired Investment in Education ($):", 2000),
               numericInput("Hea", "Desired Investment in Health ($):", 2000),
               numericInput("Ind", "Desired Investment in Industry ($):", 2000),
               numericInput("Ser", "ADesired Investment in Services ($):", 2000),
               plotOutput("pie2"),
               tableOutput("growthtable")
               ),
                                    
        tabPanel("Credit Check",wellPanel(helpText("Select the inputs below to see the 
                                                   average default rate of a particular group")), 
                              selectInput("Gender", 
                              label = "Gender",
                              choices = c("M","F")),
                              
                              selectInput("Sector", 
                              label = "Sector",
                              choices = c("Agriculture","Education","Health","Industry","Services")),
                 plotOutput("bar"), tableOutput("defaulttable")
                 )
               )
      
      #wellPanel(helpText("These Graphs Depict the frequency of loans per sector in a country.  (May take a few minutes to load)")),
      #plotOutput(outputId = "sectorbycountryplot"))
      
    )
  ))