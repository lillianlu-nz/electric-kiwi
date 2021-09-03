# SET UP & LOAD DATA ----
library(shiny)
library(shinyMobile)
library(shinyWidgets)
library(ggplot2)
library(tidyverse)
library(scales)
library(lubridate)

# DATA ----

mydata = 
    read_csv("https://raw.githubusercontent.com/lillianlu-nz/electric-kiwi/main/Household%20Device%20Power%20Consumption.csv")


str(mydata)
colnames(mydata)

appliance = mydata %>% 
    mutate(minute_min = `Min units consumption in an hour (kWh)`/60,
           minute_max = `Max units consumption in an hour (kWh)`/60,
           minute_avg = `Average unit consumption in an hour (kWh)`/60,
           wattage_min = as.numeric(substr(`Wattage(min)`,1,nchar(`Wattage(min)`)-1)),
           wattage_max = as.numeric(substr(`Wattage(max)`,1,nchar(`Wattage(max)`)-1)),
           wattage_avg = round((wattage_min+wattage_max)/2,0)) %>% 
    rename(hour_min = `Min units consumption in an hour (kWh)`,
           hour_max = `Max units consumption in an hour (kWh)`,
           hour_avg = `Average unit consumption in an hour (kWh)`,
           Name = Applicance) %>% 
    select(Name, wattage_avg, hour_min, hour_max, hour_avg, minute_min, minute_max, minute_avg) 

# appliance$unit_price = 0.5

namelist = appliance$Name

mydata2 = 
  read_csv("https://raw.githubusercontent.com/lillianlu-nz/electric-kiwi/main/DummyNZ_HourlyElectricity.csv", 
           col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                            Time_of_Day = col_time(format = "%H")))

consumption = mydata2 %>% 
  mutate(Datetime = as.POSIXct(paste(Date, Time_of_Day),
                               format = "%Y-%m-%d %H:%M:%S"),
         Day_of_Week = weekdays(Date),
         Price_by_Minute = Hourly_Cost/60
         ) %>% 
  select(c(1:5,"Datetime","Day_of_Week","Price_by_Minute"))

consumption$Day_of_Week = factor(consumption$Day_of_Week,
                                    levels = c("Monday","Tuesday","Wednesday",
                                              "Thursday","Friday", "Saturday", "Sunday"))

weekdaylist = unique(consumption$Day_of_Week)

unit_price = unique(consumption$Unit_Price)

# UI ----

ui = f7Page(

    options = list(
        theme = c("ios", "md", "auto", "aurora"),
        dark = F,
        iosTranslucentBars = T,
        navbar = list(iosCenterTitle = TRUE,
                      hideOnPageScroll = TRUE),
        toolbar = list(hideOnPageScroll = T),
        pullToRefresh =
            FALSE
    ), 
    
    title = "Electric Kiwi App Demo",

    f7TabLayout(
        
    navbar = f7Navbar(
        title = "Electric Kiwi"
        # ,
        # leftPanel = T
    ),
    
    
    # panels = tagList(
    #     f7Panel(title = "Left Panel", side = "left", theme = "light", "Blabla", effect = "cover"),
    #     f7Panel(title = "Right Panel", side = "right", theme = "dark", "Blabla", effect = "cover")
    # ),

# TAB 1 ----
    
f7Tabs(
animated = FALSE,
swipeable = TRUE,
f7Tab(
    tabName = "News",
    icon = f7Icon("captions_bubble_fill"),
    active = TRUE,

# COMPONENT 1
f7Shadow(
        intensity = 15,
        hover = TRUE,
        f7ExpandableCard(
    title = "💰Energy Saving Tips",
    fullBackground = T,
    image = "https://media.discordapp.net/attachments/882918240265830410/882922091781947412/1.jpg",
    h3("How Much Electricity Different Items in Your House Use?"),
    p("Ever wondered why people go nuts for LED lights? Learn how much electricity items in your home use."),
    h4("Portable Electric Heater"),
    p("Larger portable electric heaters often draw 1500 watts, or 1.5 kW per hour. According to Silicone Valley’s appliance 
      energy use chart, this equates to roughly $0.20 of electricity used per hour, assuming you’re running the heater at full capacity. 
      It’s easy to see how fast this adds up. There’s a reason electricians recommend steering clear of heating your home with electricity.")
    )),

# COMPONENT 2
f7Shadow(
    intensity = 15,
    hover = TRUE,
    pressed = TRUE,
    
    f7ExpandableCard(
        title = "🌱In The Know Iss.6",
        fullBackground = T,
        image =
            "https://media.discordapp.net/attachments/882748529443995761/882857519066804295/EK-_Lightbulb.jpg",
        h3("️15+ Amazing Reasons Why We Should Conserve Energy"),
        h4("1. More jobs being created"),
        p("The energy industry has gone through some major transformations in recent years. 
          As the fossil fuel industry has been working hard to recover completely from the oil crisis since 2015, 
          energy efficiency has become more profitable, providing an interesting opportunity for big companies.")
    )),

# COMPONENT 3
f7Shadow(
    intensity = 15,
    hover = TRUE,
    pressed = TRUE,
    
    f7ExpandableCard(
        title = "🎲Prizes for grabs!",
        fullBackground = T,
        image = "https://media.discordapp.net/attachments/882748529443995761/883150191815974932/EK_DOG.jpg",
        h3("Challenge your power saving knowledge and score to win!"),
        h4("What's in it for you?"),
        p("1 Grand Prize Winner wins a Calligraphy Set worth $2500 and the 5 people with the most entries win a $500 gift voucher.")
    ))
),
            
# TAB 2 ----
f7Tab(
tabName = "Empower",
icon = f7Icon("camera_viewfinder"),
active = FALSE,
# f7Shadow(
#     intensity = 15,
#     hover = TRUE,
f7Shadow(
  intensity = 15,
  hover = TRUE,
  pressed = TRUE,   
    f7Card(
      title = "",
      image = "https://media.discordapp.net/attachments/882748529443995761/883137007205908531/Bottle_Electric.jpg",
      f7SmartSelect(
        inputId = "select_day",
        label = strong("Day of Week:"),
        openIn = "popup",
        choices = list("Monday","Tuesday","Wednesday",
                       "Thursday","Friday", "Saturday", "Sunday", "The Whole Week"),
        selected = "The Whole Week"
      ),
      br(),
      htmlOutput("summarytext"),
      plotOutput("summaryplot")
    )),

br(),

f7Shadow(
  intensity = 15,
  hover = TRUE,
  pressed = TRUE,
    f7Card(
        title = "",
        image = "https://media.discordapp.net/attachments/882748529443995761/883139841678802944/electric-calculatorBlue.jpg", 
        span("This calculator is here to help you easily work out your energy saving plan. Select one appliance from the list 
          below, or use the scan button. Let's do it!", style="font-size:4mm;font-family:Helvetica"),
        br(),
        br(),
        f7SmartSelect(
            inputId = "select_app",
            label = strong("Appliance:"),
            openIn = "popup",
            choices = namelist,
            selected = "Clothes Dryer"
        ),
        
        f7Slider(
            inputId = "select_time", 
            label = "Usage Time (minute):",
            min = 5, max = 120, 
            value = 60), 
        br(),
        
        f7Slider(
          inputId = "select_budget", 
          label = "Daily Budget $:",
          min = 0, max = 10, 
          value = 3), 

        footer = tagList(
            f7Button(label = "Scan it", color = "black", size = "small", 
                     href = "https://www.google.com")
        ),

br(),

f7Swiper(
    # f7Slide(htmlOutput("apptext")),
    f7Slide(plotOutput("weeklyplot")),
    f7Slide(plotOutput("calplot1")),
    f7Slide(plotOutput("calplot2")),
    id = "swipe",
    options = list(speed = 400, spaceBetween = 50, slidesPerView = "auto", centeredSlides
                   = TRUE, pagination = TRUE)))
)

)
)
)
)

# SERVER ----
server <- function(input, output, session) {
    
    weekfilter = reactive({
      
      if(input$select_day == "The Whole Week"){
        consumption
      }
      else{
      consumption %>%
        filter(Day_of_Week == input$select_day)
      }
      
    })  
    
    appliancefilter = reactive({
      
      appliance %>% 
        filter(wattage_avg > appliance$wattage_avg[appliance$Name == input$select_app]) %>% 
        tail(2) %>% 
        rbind(
          appliance %>% 
            filter(Name == input$select_app),
          appliance %>% 
            filter(wattage_avg < appliance$wattage_avg[appliance$Name == input$select_app]) %>% 
            top_n(2)
        )
      
    })
    
    
    # peakhour = reactive({
    #   temp = 
    #   weekfilter() %>% 
    #     group_by(Time_of_Day) %>% 
    #     summarise(peak_hour = max(Hourly_Cost)) %>% 
    #     arrange(desc(peak_hour)) %>% 
    #     top_n(1)
    #   
    #   temp[1]
    # })
    # 
    # peakusage = reactive({
    #   temp1 = 
    #     weekfilter() %>% 
    #     group_by(Time_of_Day) %>% 
    #     summarise(peak_usage = max(Hourly_Usage)) %>% 
    #     arrange(desc(peak_usage)) %>% 
    #     top_n(1)
    #   
    #   temp1[2]
    # })
    
  output$summaryplot <- renderPlot({
    weekfilter() %>% 
      ggplot()+
      geom_line(aes(x=Datetime,y=Hourly_Cost),size=1) +
      geom_point(aes(x=Datetime,y=Hourly_Cost),size=2,color="darkgoldenrod1") +
      theme_minimal() +
      theme(axis.title = element_blank(),
            axis.text = element_text(size=15,color="black",face="bold",family="Helvetica"),
            axis.text.x = element_text(angle=20)) +
      scale_y_continuous(labels = scales::dollar_format())
  })    
  
  
  output$weeklyplot <- renderPlot({
    
    consumption %>% 
      group_by(Day_of_Week) %>% 
      summarise(Daily_Total = sum(Hourly_Cost)) %>% 
      ggplot() +
      geom_col(aes(x=Day_of_Week, y=Daily_Total), fill = "gray") +
      geom_col(aes(x=Day_of_Week, y = appliance$hour_avg[appliance$Name == input$select_app]*0.274/60*input$select_time), 
               fill = "darkgoldenrod1") +
      theme_minimal() +
      theme(axis.title = element_blank(),
            axis.text = element_text(size=15,color="black",face="bold",family="Helvetica"),
            axis.text.x = element_text(angle=20),
            plot.title = element_text(size=15, hjust=0.5, face="bold", 
                                      family="Helvetica", colour='black'),
            plot.caption = element_text(color = "gray36", size =15)) +
      labs(title = "Compared to Your Current Consumption",
           caption = "swipe right ➡️") +
      scale_y_continuous(labels = scales::dollar_format(), n.breaks=10,scales::number_format(accuracy=0.01))
    
  })
  

    output$calplot1 <- renderPlot({
    
    appliance %>% 
        filter(Name == input$select_app) %>% 
        ggplot() +
        geom_col(aes(x=Name, y=input$select_budget),fill="gray") +
        geom_col(aes(x=Name, y= minute_avg*input$select_time*unit_price),fill="darkgoldenrod1") +
        theme_minimal() +
        theme(axis.title = element_blank(),
              axis.text = element_text(size=15,color="black",face="bold",family="Helvetica"),
              plot.title = element_text(size=15, hjust=0.5, face="bold", 
                                        family="Helvetica", colour='black'),
              plot.caption = element_text(color = "gray36", size =15)) +
        labs(title = "Compared to Your Daily Budget",
             caption = "swipe right ➡️") +
        scale_y_continuous(labels = scales::dollar_format(), n.breaks=10,scales::number_format(accuracy=0.01))
        
    })
    
    output$calplot2 <- renderPlot({

      appliancefilter() %>% 
        ggplot() +
        geom_col(aes(x=reorder(Name,-wattage_avg), y=wattage_avg),fill="gray") +
        geom_col(data=appliance %>% 
                   filter(Name == input$select_app), 
                 aes(x=Name, y=wattage_avg),fill="darkgoldenrod1") +
        theme_minimal() +
        theme(axis.title = element_blank(),
              axis.text.x = element_text(angle=10,size=8,color="black",face="bold",family="Helvetica"),
              axis.text = element_text(size=15,color="black",face="bold",family="Helvetica"),
              plot.title = element_text(size=15, hjust=0.5, face="bold", 
                                        family="Helvetica", colour='black'),
              plot.caption = element_text(color = "gray36", size =15)) +
        labs(title = "Compared to Other Appliances",
             caption = "Appliances selected based on wattage ranking.") +
        scale_y_continuous(labels = unit_format(unit = "kWh"))
    })

    
  output$summarytext <- renderText({ 
    
    paste('<span style="font-size: 4mm; font-family:Helvetica"> You have selected <b>', 
          input$select_day,
      '</b>. During this period, you used <b>', 
          round(sum(weekfilter()$Hourly_Usage),2),
          "kWh </b> power, and spent <b> $", 
          round(sum(weekfilter()$Hourly_Cost),2),
          '</b> </span>.')
  })
  
  
  
  output$apptext <- renderText({ 
    
    paste('<span style="font-family:Helvetica"> If you want to use <b>', 
          input$select_app,
          'for',
          input$select_time,
          'minutes </b>, it will cost you <b>', 
          # appliance %>% 
          #   filter(Name == input$select_app) %>% 
          #   select(hour_avg),
          "kWh </b> power, and spent <b> $", 
          round(sum(weekfilter()$Hourly_Cost),2),
          '</b> </span>.')
  })
  
    
    session$onSessionEnded(function(){
        stopApp()
    })
}

shinyApp(ui = ui, server = server)
