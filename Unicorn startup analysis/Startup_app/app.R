####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Modified from Winston Chang, 
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Concepts about Reactive programming used by Shiny, 
# https://shiny.rstudio.com/articles/reactivity-overview.html

# Load R packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(shinyWidgets)

library(gifski)
library(plotly)

library(gganimate)
library(maps)
library(ggExtra)

load("./../Startup.Rdata")

f_industries  = strsplit(Feamle_startups$Industries, ",")
f_industries = unlist(f_industries)
f_industries = as_tibble(table(f_industries))
female_data = f_industries %>% filter(n > 3)

#india
vec = gsub(",", "", India$Total.Funding..in...)
vec = as.numeric(vec)

India$Total.Funding..in... = vec


valuation = character(length = 125)
for(i in 1:125){
  valuation[i] = strsplit(Feamle_startups$Valuation, " ")[[i]][2]
}
valuation = valuation[2:125]
valuation = substring(valuation,2,5)
valuation = as.numeric(valuation)
#Found_year.1=subset(Found_year, Country != 'India')
data.1 = Found_year
year = character(length = 594)
for(i in 1:594){
  year[i] = strsplit(data.1$Unicorn.Event.Date, ",")[[i]][2]
}

year = as.numeric(year)
data.1 = data.1 %>% mutate(Time_till_unicorn = year - Founded.Year)
data.1 = data.1 %>% mutate(Unicorn_Year = year)
#data.1<-data.1[data.1['Country']!="India"]
data.1<-subset(data.1, Country != 'India')
saal = rep(2013:2022,8)
Growth_rate$year = saal
#code for city wise industry plot
A = Combined_data %>% group_by(City) %>% filter(Industry == "Insurance")%>% summarise(n = n()) %>% filter(n >= 2) %>% arrange(desc(n))
B = Combined_data %>% group_by(City) %>% filter(Industry == "Media & Entertainment")%>% summarise(n = n()) %>% filter(n >= 2) %>% arrange(desc(n))
C = Combined_data %>% group_by(City) %>% filter(Industry == "Industrials")%>% summarise(n = n()) %>% filter(n >= 2) %>% arrange(desc(n))
D = Combined_data %>% group_by(City) %>% filter(Industry == "Healthcare & Life Sciences")%>% summarise(n = n()) %>% filter(n >= 2) %>% arrange(desc(n))
E = Combined_data %>% group_by(City) %>% filter(Industry == "Financial Services")%>% summarise(n = n()) %>% filter(n >= 2) %>% arrange(desc(n))
G = Combined_data %>% group_by(City) %>% filter(Industry == "Enterprise Tech")%>% summarise(n = n()) %>% filter(n >= 3) %>% arrange(desc(n))
H = Combined_data %>% group_by(City) %>% filter(Industry == "Consumer & Retail")%>% summarise(n = n()) %>% filter(n >= 2) %>% arrange(desc(n))


#code for scatter plot
data = Found_year
hyphen_indices <- which(data$Equity.Funding.Raised...M. == "-")
values <- data$Equity.Funding.Raised...M.[data$Equity.Funding.Raised...M. != "-"]
x = as.numeric(values)
y = data$Valuation...B.[-hyphen_indices]

x = c(x, rep(0,10))
y = c(y, rep(0,10))


data = data %>% mutate(Equity = x)
data = data %>% mutate(Valuation = y)

# Define UI
ui<-  navbarPage(
          title = "Unicorn Startups",
          theme = shinytheme("darkly"),  # <--- To use a theme, uncomment this
          tabPanel(
            "Home",
            h3("R Shiny App"),
            br(),
            h1("UNICORNS UNIVERSE"),
            br(),
            h4("MTH208A"),
            h4("Guide: Dootika Vats"),
          ),# mainPanel
          tabPanel(
            "Visualisations",
            
            tags$head(tags$style(
              HTML("hr {border-top: 1px solid #000000;}")
            )),
            
            titlePanel("Visualizing the Data"),
            sidebarLayout(
              sidebarPanel(
                selectInput(
                  "question",
                  h3("View plots:"),
                  choices = list(
                    "Select" = 1,
                    "Number of Unicorns vs Countries" = 2,
                    "GDP growth rate of countries" = 3,
                    "Number of Unicorns in each Industry " = 4,
                    "Industrial Analysis 1" = 5,
                    "Industrial Analysis 2" = 6,
                    "Unicorn Analysis of India" = 7,
                    "Equity raised and Startup Success " = 8,
                    "Female founder Unicorn Analysis"=9
                  )
                ),
                
                conditionalPanel(
                  condition = "input.question == 2",
                  sliderTextInput(
                    "year.2",
                    h4("Year:"),
                    choices = c(2012:2023),
                    selected=2023,
                    animate = animationOptions(loop = TRUE)
                    
                  )
                ),
                #condition 3 is a world map so no input required
                
                conditionalPanel(
                  condition = "input.question==5",
                  checkboxGroupInput(
                    "industry.5",
                    "Industry:",
                    choices =  c("Consumer & Retail","Enterprise Tech","Financial Services","Healthcare & Life Sciences","Industrials","Insuarance","Media & Entertainment"),
                    selected = c("Consumer & Retail","Enterprise Tech","Financial Services","Healthcare & Life Sciences","Industrials","Insuarance","Media & Entertainment")
                  )
                  
                ),
                conditionalPanel(
                  condition = "input.question==6",
                  radioButtons("Industry.6", 
                               label = "Select a single option:", 
                               choices = c("Consumer & Retail","Enterprise Tech","Financial Services","Healthcare & Life Sciences","Industrials","Insuarance","Media & Entertainment"), 
                               selected = "Consumer & Retail")
                ),
                # conditionalPanel(
                #   condition = "input.question==7",
                #   # radioButtons("India.7", 
                #   #              label = "Select a single option:", 
                #   #              choices = c("Valuation Vs Funding","Industry Distribution","Valuation Vs Year_to_Unicorn"), 
                #   #              selected = "Industry Distribution")
                # ),
                #Requires no input
                conditionalPanel(
                  condition = "input.question==8",
                  checkboxGroupInput(
                    "country.8",
                    "Country:",
                    choices =  c("China","France","Germany","India","Israel","South Korea","United Kingdom","United States" ),
                    selected = c("China","France","Germany","India","Israel","South Korea","United Kingdom","United States" )
                  )
                )
                
                # conditionalPanel(
                #   condition="input.question==9"
                # )
              ),
              mainPanel(
                conditionalPanel(condition = "input.question == 2",
                                 plotOutput("UnicornVsCountry")),
                
                conditionalPanel(
                  condition = "input.question == 3",
                  #plotlyOutput("map.3"),
                  plotOutput("GDPlinegraph")
                ),
                
                conditionalPanel(
                  condition = "input.question == 4",
                  plotOutput("Industry_bar"),
                  
                ),
                
                conditionalPanel(condition = "input.question == 5",
                                 #plotOutput("Industry_line"),
                                 plotOutput("Industry_box")),
              
                
                conditionalPanel(condition = "input.question == 6",
                                 plotOutput("Industry_city")
                                 #plotOutput("Industry_invest")
                                 ),
                
                conditionalPanel(
                  condition = "input.question == 7",
                  plotOutput("India_pie"),
                  plotOutput("India_scatter"),
                  plotOutput("India_scat_bar")
                ),
                conditionalPanel(condition = "input.question == 8",
                                 plotOutput("Equity_scatter")),
                conditionalPanel(condition = "input.question == 9",
                                 plotOutput("female_uni_dist"),
                                 plotOutput("female_bar"),
                                 plotOutput("female_box"))
              )
              
            )
          ),
                
          
          tabPanel("About",
                   h3("Credits:"),
                   h5("Rythm Kumar"),
                   h5("Devansh Gupta"),
                   h5("Anoop"),
                   br(),
                   br(),
                   h3("Resource:"),
                   h6("https://www.wikipedia.org/"),
                   h6(
                     "https://www.cbinsights.com/research-unicorn-companies"
                   )
          )
                  
          ) # navbarPage


# Define server function  
server <- function(input, output) {
  
  
  Unicorns_at_time = function(y){
    x = data.1[data.1$Unicorn_Year<y,]
    return(x)
  }
  
  unicorn_data<-reactive({
    req(input$year.2)
    data.2=tibble()
    data.2<-Unicorns_at_time(as.numeric(input$year.2))
    data.2
  })
  #2
  output$UnicornVsCountry<-renderPlot(
    unicorn_data() %>% ggplot(aes(x = Country))+
      geom_bar(fill = "grey", colour = "black")+
      theme_bw()
  )
  output$GDPlinegraph<-renderPlot(
    Growth_rate %>% ggplot(aes(x = year, colour = group))+
      geom_line(aes(y = Growth_rate))+
      geom_line(aes(y = Annual_change), linetype = "dotted")
  )
  #3
  output$map.3<-renderPlot(
    ggplot(world_map_data, aes(x = long, y = lat, group = group, fill = IM_forecast)) +
      geom_polygon() +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      coord_quickmap() +
      labs(title = "World GDP Heatmap", fill = "GDP") +
      theme_void()
  )
  #4
  output$Industry_bar<-renderPlot(
    Combined_data %>% ggplot(aes(y = Industry))+
      geom_bar(colour = 'red', fill = 'pink')+
      labs(x = "Number of Unicorns", y = "Industries", title = "NUMBER OF UNICORNS IN EACH INDUSTRY")+
      theme(axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            plot.title = element_text(size = 18, colour = "brown"),
            axis.title.x = element_text(size = 16, colour = 'blue'),  # Adjust x-axis label size
            axis.title.y = element_text(size = 16, colour = 'blue'))
  )
  # 
  # #5
  
  industry_data.6=reactive({
    data.6=tibble()
    if(input$Industry.6=="Consumer & Retail"){
      data.6=A
    }
    if(input$Industry.6=="Enterprise Tech"){
      data.6=B
    }
    if(input$Industry.6=="Financial Services"){
      data.6=C
    }
    if(input$Industry.6=="Healthcare & Life Sciences"){
      data.6=D
    }
    if(input$Industry.6=="Industrials"){
      data.6=E
    }
    if(input$Industry.6=="Insuarance"){
      data.6=G
    }
    if(input$Industry.6=="Media & Entertainment"){
      data.6=H
    }
    return(data.6)
  }
  
  )
  
  
  output$Industry_city<-renderPlot(
    industry_data.6()%>% ggplot(aes(y = City, x = n))+
      geom_bar(stat = "identity", colour = 'red', fill = 'pink')+
      labs(x = "Number of Unicorns", y = "Cities", title = "NUMBER OF UNICORNS IN EACH CITY")+
      theme(axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            plot.title = element_text(size = 18, colour = "brown"),
            axis.title.x = element_text(size = 16, colour = 'blue'),  # Adjust x-axis label size
            axis.title.y = element_text(size = 16, colour = 'blue'))
    
  )
  
  
  output$Industry_box<-renderPlot(
    ggplot(Combined_data, aes(x = Industry, y = Valuation...B.)) +
      geom_boxplot(fill = c("lightblue","green","Red","pink","orange","grey","yellow"), color = "black",outlier.shape = NA) +
      labs(x = "Industry", y = "Valuation", title = "Boxplot of Valuation by Industry") +
      theme_bw()+ ylim(c(0,7.5))
  )
  # #6
  # output$Industry_invest<-renderPlot(
  #   
  # )
  # #7
  # 
  output$India_pie<-renderPlot(
    India %>% ggplot(aes(x = "", fill = Sector))+geom_bar()+coord_polar(theta = "y")+theme_minimal()+labs(title = 'Industries')
  )
  output$India_scatter<-renderPlot(
    India %>% ggplot(aes(x = Years.To.Unicorn, y = Valuation..In...Bn.))+
      geom_jitter(aes(colour = Current.Status), size = 2)
  )
  output$India_scat_bar<-renderPlot(
    India %>% ggplot(aes(x = Valuation..In...Bn., y = Total.Funding..in...))+
      geom_point(aes(colour = Headquarters, alpha = 0.5))+labs(title = "Valuation vs Funding", x = "Valution in billions", y = "Total Funding")+
      theme_bw()+xlim(c(0,12))+ylim(c(0,5.5e+09))
  )
  
  #8
  output$Equity_scatter<-renderPlot(
    data %>% ggplot(aes(y = Valuation, x = Equity))+
      geom_jitter(aes(colour = Company.Stage))+
      coord_cartesian(xlim = c(0,3000),ylim = c(0,15))+
      theme_minimal()+ 
      labs("Valuation vs Equity raised", x = "Equity Raised in M", y = "Valuation in B")
      #ggMarginal(data,y = Valuation, x = Equity,type = "violin", fill = "transparent")
    #
  )
  # 
  # #9
  # output$female_bar<-renderPlot(
  #   
  # )
  output$female_uni_dist<-renderPlot(
    ggplot(female_data, aes(x = "", y = n, fill = f_industries)) +
      geom_bar(stat = "identity") +
      coord_polar(theta = "y") +
      theme_minimal() +
      labs(title = "Industry Distribution")
  )
  output$female_box<-renderPlot(
    boxplot(valuation, col = "cyan", horizontal = TRUE,xlabs = "Valuation in Billions", main = "Valuation of Startups with females leaders", sub = "Avg: 2.89, Median: 1.5")
  )
  
  output$female_bar<-renderPlot(
    Feamle_startups %>% ggplot(aes(x = Country))+
      geom_bar(aes(fill = 'orange', colour = "red"))+labs(title = 'Country Vs Number of female led startups', y = 'Number of startups')+
      theme_bw()
  )
  # output$txtout <- renderText({
  #   paste( input$txt1, input$txt2, sep = " " )
  # })
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)