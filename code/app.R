library(ggplot2)

library(shiny)
library(ggplot2)

library(readr)
df<-read_csv("cleaned_data.csv")


calculation<-function(abdome,chest,hip,thigh,age){
  bodyfat<-18.491+8.286*((abdome-mean(df$ABDOMEN))/sd(df$ABDOMEN))-2.212*((chest-mean(df$CHEST))/sd(df$CHEST))
  -2.331*((hip-mean(df$HIP))/sd(df$HIP))+1.598*((thigh-mean(df$THIGH))/sd(df$THIGH))
  +0.435*((age-mean(df$AGE))/sd(df$AGE))
  return(bodyfat)
}

##############shiny#########################

ui <- fluidPage(
  titlePanel("BodyFat Analysis in your agegroup"),
  sidebarLayout(
    sidebarPanel(
      numericInput("age", "Enter your age:", value = 0),
      numericInput("chest", "Chest(cm):", value = 0),
      numericInput("abdomen", "Abdomen(cm):", value = 0),
      numericInput("hip", "Hip(cm):", value = 0),
      numericInput("thigh", "Thigh(cm):", value = 0),
      actionButton("plotButton", "Generate Plot")
    ),
    mainPanel(
      textOutput("main"),
      textOutput("agegroup"),
      plotOutput("agePlot"),
      textOutput("bodyfat"),
      plotOutput("allplot")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$plotButton, {
    age <- as.numeric(input$age)
    abdome <- as.numeric(input$abdomen)
    chest <- as.numeric(input$chest)
    hip <- as.numeric(input$hip)
    thigh <- as.numeric(input$thigh)
    # agegroup decision
    ageGroup <- ifelse(age <= 40, "young(20-40)", ifelse(age <= 60, "middle-age(41-60)", "elder(>61)"))
   
    # calculation
    BodyFat <- calculation(abdome, chest, hip, thigh, age)
    fitGroup <- ifelse(BodyFat<=15,"athlete",ifelse(BodyFat<=20,"fitness",ifelse(BodyFat<=30,"normal","obsess")))
    
    df_subset <- switch(
      ageGroup,
      "young(20-40)" = df[df$AGE >= 20 & df$AGE <= 40, ],
      "middle-age(41-60)" = df[df$AGE >= 41 & df$AGE <= 60, ],
      "elder(>61)" = df[df$AGE >= 61, ]
    )
    
    lab2<-paste("You are in",fitGroup,"group")
    p<-ggplot(df_subset, aes(x = BODYFAT,fill=bf)) +
      geom_density( color = "gray",alpha = 0.8) +
      labs(
        x = "Body Fat",
        y = "Frequency in each bodyfat group",
        fill="fit group"
      )+
      scale_fill_brewer(palette = "Set2")+
      theme_minimal()+
      geom_vline(xintercept = BodyFat, color = "red", size = 1)+
      geom_text(aes(x = BodyFat,y=0.20, label =lab2 ),
                vjust = -1, hjust = -0.1, color = "red",size=5)
    
    output$agePlot <- renderPlot({
      print(p)
    })
    
    output$bodyfat <- renderText({
      paste("In the overall age population, your body fat is shown as below")
    })
    
    output$agegroup<- renderText({
      paste("Below is the distribution of",ageGroup,"group.")
    })
    
    output$allplot<- renderPlot({
      lab<-paste("Here is your bodyfat:",BodyFat)
      ggplot(df, aes(x = BODYFAT)) +
        geom_density( color = "black",alpha = 0.3) +
        labs(
          x = "Body Fat",
          y = "Frequency"
        )+
        geom_vline(xintercept = BodyFat, color = "red", size = 1)+
        geom_text(aes(x = BodyFat,y=0, label = lab),
                  vjust = -1, hjust = -0.1, color = "red",size=5)+
        theme_minimal()
    })
    
    output$main<-renderText({
      paste("Your bodyfat is predicted be: ", BodyFat)
    })
  })
}

shinyApp(ui, server)


