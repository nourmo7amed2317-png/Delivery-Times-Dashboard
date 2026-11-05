
#Libraries
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(bslib)
library(fresh)
library(ggplot2)
library(DT)
library(readxl)
library(arules)
library(arulesViz)

#theme
lavender_theme <- bs_theme(
  version = 5,
  primary = "#E0C6FF",  
  secondary = "#D8BFD8",
  base_font = font_google("Oswald"),
  heading_font = font_google("Lobster Two")
)

ui <- fluidPage(
  theme = lavender_theme,  
  
  tags$style(HTML("
    body {
      background-color: #F6F0FF !important;
    }
    .well {
      background-color: #EEE6FF !important;
      border-radius: 10px;
    }
  ")),
  
  titlePanel("Delivery times!🚚"),
  
  
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file","choose a file", multiple = FALSE),
      #note for the user 
      h6("Note: Everything will work when you upload your file
         and press (update view) & (clean & summary buttons)"),
      tags$hr(),
      #K-mean & association controls
      sliderInput("support", "support :", min = 0, max = 0.5, value = 0.25),
      sliderInput("con","confidence :",min=0, max=0.5,value=0.25),
      sliderInput("clu","number of clusters:",min=2, max=4,value=3),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data preview",
                 h4("Cleaning:"),
                 DTOutput("contents"),
                 numericInput("obs","Number of observation to view :", value = 10 , min = 1),
                 # cleaned data preview & summary button
                 actionButton("update" , "Update View" ),
                 checkboxInput("show_summary", "Clean & Show Summary", value = FALSE),
                 conditionalPanel(
                   condition = "input.show_summary == true",
                   tableOutput("view"),
                   verbatimTextOutput("summary")
                 ),
        ),
        tabPanel("Visualization",
                 selectInput("vis","Choose a Plot:", choices = c(
                   "scatter delivery time and prepration time",
                   "Scatterplot(distance &time)",
                   "inverse relationship between speed and delivery time in scatter",
                   "Relationship between Courier experience years and Delivery time",
                   "Connection between Customer rating & Delivery time",
                   "Delivery time Histogram",
                   "Distance_km Histogram",
                   "Preparation time Histogram",
                   "Distance_km Boxplot",
                   "Preparation time Boxplot", 
                   "delivery time min Boxplot",
                   "Delivery time min (weather) Boxplot",
                   "Delivery time min (traffic level) Boxplot",
                   "Delivery time min (vehicle type) Boxplot",
                   "Weather data Barplot",
                   "Traffic level data Barplot",
                   "Relationship between time of day and delivery time/lineplot"
                 )),
                 plotOutput("plot")
        ),
        tabPanel("K-means",
                 h6("Note: 4 is not the ideal number of clusters
                    so it might look a little bit off"),
                 plotOutput("k")
        ),
        tabPanel("Association rules" ,
                 tableOutput("rules")
                 
        ),
      )
    )
  )
)
server <- function(input, output) {
  #upload file
  raw_data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    
    if (ext == "xlsx") {
      read_excel(input$file$datapath)
    } else if (ext == "csv") {
      read.csv(input$file$datapath)
    }
  })
  #clean
  data <- reactive({
    req(raw_data())
    df <- raw_data()
    
    df <- unique(df)
    df[df == ""] <- NA
    df <- na.omit(df)
    
    df$Weather <- as.factor(df$Weather)
    df$Traffic_Level <- as.factor(df$Traffic_Level)
    df$Time_of_Day <- as.factor(df$Time_of_Day)
    df$Vehicle_Type <- as.factor(df$Vehicle_Type)
    
    outlier <- boxplot(df$Delivery_Time_min)$out
    df <- df[!df$Delivery_Time_min %in% outlier, ]
    
    df$Speed <- df$Distance_km / (df$Delivery_Time_min / 60)
    df$Late_Delivery_Flag <- ifelse(df$Speed > 10.5, 1, 0)
    
    df
  })
  # show data preview
  datasetInput <- eventReactive(input$update ,{
    req(input$file)
    data()
  })
  
  
  
  #show summary
  output$view <- renderTable({
    req(datasetInput())
    head(datasetInput(), n = input$obs)  
  })
  
  output$summary <- renderPrint({
    req(datasetInput())
    if (isTRUE(input$show_summary)) {
      summary(datasetInput())
    }
  })
  
  #kmean
  output$k <- renderPlot({
    req(data())
    #3 clusters
    if(input$clu==3){
      #cleaning
      str(data())
      summary(data())
      sum(duplicated(data()))
      data <- unique(data())
      sum(duplicated(data))
      data[data == ""] <- NA
      data <- na.omit(data)
      str(data)
      data$Weather <- as.factor(data$Weather)
      head(data$Weather)
      data$Traffic_Level <- as.factor(data$Traffic_Level)
      head(data$Traffic_Level)
      data$Time_of_Day <- as.factor(data$Time_of_Day)
      head(data$Time_of_Day)
      data$Vehicle_Type <- as.factor(data$Vehicle_Type)
      head(data$Vehicle_Type)
      is.numeric(data$Order_ID)
      boxplot(data$Distance_km, data$Preparation_Time_min, data$Courier_Experience_yrs, data$Delivery_Time_min)
      outlier = boxplot(data$Delivery_Time_min)$out
      outlier
      data = data[-which(data$Delivery_Time_min %in% outlier),]
      boxplot(data$Delivery_Time_min)
      data
      data$Speed <- data$Distance_km / (data$Delivery_Time_min / 60)
      summary(data$Speed)
      aver_speed = 10.5
      data$Late_Delivery_Flag <- ifelse(data$Speed > aver_speed, 1, 0)
      data
      names(data)
      #kmeans
      data$Bad_Weather_Flag <- ifelse(data$Weather %in% c("Rainy", "Stormy", "Foggy"), 1, 0)
      data$slow_vehicle_Flag <- ifelse(data$Vehicle_Type %in% c("Bike"), 1, 0)
      data$Traffic_Score <- ifelse(data$Traffic_Level == "Low", 1,
                                   ifelse(data$Traffic_Level == "Medium", 2,
                                          ifelse(data$Traffic_Level == "High", 3, NA)))
      data$Delivery_Risk_Score <- 
        data$Bad_Weather_Flag +
        data$Traffic_Score +
        data$slow_vehicle_Flag +
        data$Late_Delivery_Flag
      numeric_data <- data[,-c(1,3,4,5,6,11,12,13,14)]
      data_scaled <- as.data.frame(scale(numeric_data))
      
      kmeans_results <- kmeans(data_scaled, centers = 3, nstart = 25)
      
      data$cluster <- kmeans_results$cluster
      
      library(factoextra)
      fviz_cluster(kmeans_results, data = data_scaled)
    } 
    #4 clusters
    else if (input$clu==4){
      #cleaning
      str(data())
      summary(data())
      sum(duplicated(data()))
      data <- unique(data())
      sum(duplicated(data))
      data[data == ""] <- NA
      data <- na.omit(data)
      str(data)
      data$Weather <- as.factor(data$Weather)
      head(data$Weather)
      data$Traffic_Level <- as.factor(data$Traffic_Level)
      head(data$Traffic_Level)
      data$Time_of_Day <- as.factor(data$Time_of_Day)
      head(data$Time_of_Day)
      data$Vehicle_Type <- as.factor(data$Vehicle_Type)
      head(data$Vehicle_Type)
      is.numeric(data$Order_ID)
      boxplot(data$Distance_km, data$Preparation_Time_min, data$Courier_Experience_yrs, data$Delivery_Time_min)
      outlier = boxplot(data$Delivery_Time_min)$out
      outlier
      data = data[-which(data$Delivery_Time_min %in% outlier),]
      boxplot(data$Delivery_Time_min)
      data
      data$Speed <- data$Distance_km / (data$Delivery_Time_min / 60)
      summary(data$Speed)
      aver_speed = 10.5
      data$Late_Delivery_Flag <- ifelse(data$Speed > aver_speed, 1, 0)
      data
      names(data)
      #kmeans
      data$Bad_Weather_Flag <- ifelse(data$Weather %in% c("Rainy", "Stormy", "Foggy"), 1, 0)
      data$slow_vehicle_Flag <- ifelse(data$Vehicle_Type %in% c("Bike"), 1, 0)
      data$Traffic_Score <- ifelse(data$Traffic_Level == "Low", 1,
                                   ifelse(data$Traffic_Level == "Medium", 2,
                                          ifelse(data$Traffic_Level == "High", 3, NA)))
      data$Delivery_Risk_Score <- 
        data$Bad_Weather_Flag +
        data$Traffic_Score +
        data$slow_vehicle_Flag +
        data$Late_Delivery_Flag
      numeric_data <- data[,-c(1,3,4,5,6,11,12,13,14)]
      data_scaled <- as.data.frame(scale(numeric_data))
      
      kmeans_results <- kmeans(data_scaled, centers = 4, nstart = 25)
      
      data$cluster <- kmeans_results$cluster
      
      library(factoextra)
      fviz_cluster(kmeans_results, data = data_scaled)
    }
  })
  
  
  # Visualization
  output$plot <- renderPlot({
    req(data())
    
    if(input$vis == "Scatterplot(distance &time)") {
      plot(x = data()$Distance_km,
           y = data()$Delivery_Time_min,
           main = "Relationship between Distance and Delivery time",
           xlab = "Distance",
           ylab = "Delivery time",
           col="#A76EEE")
      
    } else if(input$vis == "scatter delivery time and prepration time") {
      plot(x = data()$Preparation_Time_min,
           y = data()$Delivery_Time_min,
           main = "Relationship between Preparation time and Delivery time",
           xlab = "Preparation time",
           ylab = "Delivery time",
           col="#A76EEE")
      
    } else if(input$vis == "inverse relationship between speed and delivery time in scatter") {
      plot(x = data()$Speed,
           y = data()$Delivery_Time_min,
           main = "Relationship between Speed and Delivery time",
           xlab = "Speed",
           ylab = "Delivery Time",
           col="#A76EEE")
      
    } else if (input$vis=="Relationship between Courier experience years and Delivery time") {
      Courier <- table(data()$Courier_Experience_yrs)
      barplot(Courier,
              main="Courier Experience Distribution",
              xlab="Experience (years)",
              ylab="Count",
              col="#dbb2db")
      
    } else if (input$vis == "Connection between Customer rating & Delivery time") {
      df <- data()  
      df$customer_rate <- ifelse(df$Late_Delivery_Flag == 1, 1, 5)
      df$total_time <- df$Preparation_Time_min + df$Delivery_Time_min
      boxplot(
        total_time ~ customer_rate,
        data = df,
        main = "Customer Rating vs Total Time",
        xlab = "Customer Rating",
        ylab = "Time (min)",
        col=c("#E0C6FF","#D8BFD8")
      )
    } else if(input$vis=="Distance_km Boxplot"){
      boxplot(data()$Distance_km,
              main="Distance by km",
              xlab="Distance_km",
              col=c("#E0C6FF","#D8BFD8")
      )
      
    } else if(input$vis=="delivery time min Boxplot"){
      boxplot(data()$Delivery_Time_min,
              main="Delivery time (min)",
              xlab="Delivery_Time_min",
              col=c("#E0C6FF","#D8BFD8")
      )
      
    } else if(input$vis=="Preparation time Boxplot"){
      boxplot(data()$Preparation_Time_min,
              main="Preparation time (min)",
              xlab="Preparation_Time_min",
              col=c("#E0C6FF","#D8BFD8")
      )
      
    } else if(input$vis=="Distance_km Histogram"){
      hist(data()$Distance_km,
           main="Distance (km)",
           xlab="Distance_km",
           col="#DDA0DD")
      
    } else if(input$vis=="Delivery time Histogram"){
      hist(data()$Delivery_Time_min,
           main="Delivery time (min)",
           xlab="Delivery_Time_min",
           col="#dbb2db"
      )
      
    } else if(input$vis=="Preparation time Histogram"){
      hist(data()$Preparation_Time_min,
           main="Preparation time (min)",
           xlab="Preparation_Time_min",
           col="#BF94E4")
      
    } else if (input$vis =="Relationship between time of day and delivery time/lineplot"){
      df<-data()
      
      df$Time_of_Day <- factor(
        df$Time_of_Day,
        levels = c("Morning", "Afternoon", "Evening", "Night")
      )
      
      ggplot(df, aes(x = Time_of_Day, y = Delivery_Time_min, group = 1)) +
        stat_summary(fun = mean, geom = "line", size = 1.2, color = "#BF94E4") +
        stat_summary(fun = mean, geom = "point", size = 3, color = "#BF94E4") +
        labs(
          title = "Delivery Time Distribution Throughout the Day",
          x = "Time of Day",
          y = "Delivery Time (min)"
        ) +
        theme_minimal()
    }
    else if (input$vis == "Delivery time min (weather) Boxplot"){
      boxplot(
        Delivery_Time_min ~ Weather ,
        data = data(),
        col = c("pink", "lightblue", "lightgreen", "lightgray","lightyellow"),
        main = "Delivery Time by Weather",
        xlab = "Weather" ,
        ylab = "Delivery Time (min)"
      )
    }
    else if (input$vis == "Delivery time min (traffic level) Boxplot"){
      boxplot(
        Delivery_Time_min ~ Traffic_Level,
        data = data(),
        col = c("lightgreen", "gray", "lightblue"),
        main = "Delivery Time by Traffic Level",
        xlab = "Traffic Level",
        ylab = "Delivery Time (min)")
    }
    else if (input$vis ==  "Delivery time min (vehicle type) Boxplot"){
      boxplot(
        Delivery_Time_min ~ Vehicle_Type,
        data = data(),
        col = c("pink", "skyblue", "gray"),
        main = "Delivery Time by Vehicle Type",
        xlab = "Vehicle Type",
        ylab = "Delivery Time (min)"
      )
    }
    else if (input$vis == "Weather data Barplot" ){
      barplot(
        table(data()$Weather),
        data = data() ,
        col = c("pink", "lightblue", "lightgreen","gray","lightyellow"),
        main = "Number of Orders by Weather Conditions",
        xlab = "Weather Condition",
        ylab = "Number of Orders"
      )
    }
    else if (input$vis =="Traffic level data Barplot"){
      barplot(
        table(data()$Traffic_Level),
        data = data(),
        col = c("pink", "gray", "skyblue"),
        main = "Number of Orders by Traffic Level",
        xlab = "Traffic Level",
        ylab = "Number of Orders"
      )
    }
  })
  
  
  
  # association
  
  
  output$rules <- renderTable({
    req(datasetInput())  
    
    df <- datasetInput()  
    
    
    df$Late_Delivery_flag <- as.factor(df$Late_Delivery_Flag)
    
    
    trans_data <- df[, c("Vehicle_Type", "Traffic_Level", "Weather", "Late_Delivery_flag")]
    
    
    trans <- as(trans_data, "transactions")
    
    
    rules <- apriori(trans,
                     parameter = list(
                       supp = input$support / 100,  
                       conf =  input$con / 100,
                       target = "rules"
                     ))
    
    late_rules <- subset(rules, rhs %in% "Late_Delivery_flag=1")
    late_rules <- sort(late_rules, by="lift", decreasing = TRUE)
    
    
    rules_df <- as(late_rules, "data.frame")
    head(rules_df, 10)  
  })
} 
#run app
shinyApp(ui = ui, server = server)