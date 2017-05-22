# Zadanie 9.2, "lung", Daniel Stasiak

require(shiny)
library(survival)

data(lung)

lung$age.dead <- with(lung, age + floor(time/365))

maxTime <- max(lung$time)
minAge <- min(lung$age)
maxAge <- max(lung$age)
minAgeDead <- min(lung$age.dead)
maxAgeDead <- max(lung$age.dead)
minWtLoss <- min(lung$wt.loss[complete.cases(lung$wt.loss)])
maxWtLoss <- max(lung$wt.loss[complete.cases(lung$wt.loss)])
minCal <- min(lung$meal.cal[complete.cases(lung$meal.cal)])
maxCal <- max(lung$meal.cal[complete.cases(lung$meal.cal)])
minKarnoPh <- min(lung$ph.karno[complete.cases(lung$ph.karno)])
maxKarnoPh <- max(lung$ph.karno[complete.cases(lung$ph.karno)])
minKarnoPat <- min(lung$pat.karno[complete.cases(lung$pat.karno)])
maxKarnoPat <- max(lung$pat.karno[complete.cases(lung$pat.karno)])
minEcog <- min(lung$ph.ecog[complete.cases(lung$ph.ecog)])
maxEcog <- max(lung$ph.ecog[complete.cases(lung$ph.ecog)])

ui <- fluidPage(
  tags$style(type = "text/css",
    ".irs.js-irs-0.irs-with-grid .irs-grid-pol.small {display: none;}",
    ".irs.js-irs-1.irs-with-grid .irs-grid-pol.small {display: none;}",
    ".irs.js-irs-2.irs-with-grid .irs-grid-pol.small {display: none;}"
  ),
  
  div(
    plotOutput(
      "plotGenerated"
    ),
    
    div(
      div(
        sliderInput(
          "phKarno",
          "Skala Karnofsky’ego (ocena lekarza):",
          min=minKarnoPh,
          max=maxKarnoPh,
          step=10,
          value=c(minKarnoPh, maxKarnoPh),
          width="90%"
        ),
        style="padding-left: 3%; float: left; width: 50%; box-sizing: border-box;"
      ),
      
      div(
        sliderInput(
          "patKarno",
          "Skala Karnofsky’ego (ocena pacjenta):",
          min=minKarnoPat,
          max=maxKarnoPat,
          step=10,
          value=c(minKarnoPat, maxKarnoPat),
          width="85%"
        ),
        style="float: left; width: 50%;"
      ),
      
      div(style="width: 100%; clear: both;"),
      style="padding-top: 2%;"
    ),
    
    div(
      sliderInput(
        "ecog",
        "Skala ECOG:",
        min=minEcog,
        max=maxEcog,
        value=c(minEcog, maxEcog),
        width="90%"
      ),
      style="padding-left: 3%; padding-top: 2%; float: left; width: 50%; box-sizing: border-box;"
    ),
    
    div(
      tags$strong("0"),
      "- sprawność prawidłowa",
      tags$br(),
      tags$strong("1"),
      "- obecność objawów choroby",
      tags$br(),
      tags$strong("2"),
      "- zdolność do wykonywania czynności osobistych",
      tags$br(),
      tags$strong("3"),
      "- ograniczona zdolność wykonywania czynności osobistych",
      style="float: left; width: 50%; padding-top: 2%;"
    ),
    
    div(style="width: 100%; clear: both;"),
    style="margin-top: 20px; float: left; width: 60%;"
  ),
  
  div(
    sidebarPanel(
      div(
        checkboxGroupInput(
          "sex",
          "Podział na płeć:",
          c(
            "Kobiety" = "female",
            "Mężczyźni" = "male",
            "Centrum" = "center",
            "Pokaż przedziały ufności" = "conf"
          ),
          "center"
        ),
        style="float: left; width: 50%;"
      ),
      
      div(
        checkboxGroupInput(
          "status",
          "Powód zakończenia badania:",
          c(
            "Śmierć pacjenta" = 2,
            "Inny (cenzorowany)" = 1
          ),
          c(1, 2)
        ),
        style="float: left; width: 50%;"
      ),
      
      div(style="width: 100%; clear: both;"),
      width=12
    ),
    sidebarPanel(
      sliderInput(
        "age",
        "Zakres wiekowy (na początku badania):",
        min=minAge,
        max=maxAge,
        value=c(minAge, maxAge),
        post=" lat"
      ),
      sliderInput(
        "ageDead",
        "Zakres wiekowy (w chwili śmierci):",
        min=minAgeDead,
        max=maxAgeDead,
        value=c(minAgeDead, maxAgeDead),
        post=" lat"
      ),
      sliderInput(
        "wtLoss",
        "Utrata wagi (ostatnie 6 miesięcy):",
        min=minWtLoss,
        max=maxWtLoss,
        value=c(minWtLoss, maxWtLoss),
        post=" kg"
      ),
      sliderInput(
        "cal",
        "Spożywana średnia ilość kalorii:",
        min=minCal,
        max=maxCal,
        value=c(minCal, maxCal),
        post=" cal"
      ),
      width=12
    ),
    style="margin-top: 20px; float: left; width: 40%;"
  ),
  
  div(style="width: 100%; clear: both;")
)

server <- function(input, output) {
  output$plotGenerated <- renderPlot({
    lungDisplay <- lung[lung$status %in% input$status,]
    
    if(input$phKarno[1] != minKarnoPh || input$phKarno[2] != maxKarnoPh) {
      lungDisplay <- subset(lungDisplay, ph.karno >= input$phKarno[1] & ph.karno <= input$phKarno[2])
    }
    
    if(input$patKarno[1] != minKarnoPat || input$patKarno[2] != maxKarnoPat) {
      lungDisplay <- subset(lungDisplay, pat.karno >= input$patKarno[1] & pat.karno <= input$patKarno[2])
    }
    
    if(input$ecog[1] != minEcog || input$ecog[2] != maxEcog) {
      lungDisplay <- subset(lungDisplay, ph.ecog >= input$ecog[1] & ph.ecog <= input$ecog[2])
    }
    
    if(input$ecog[1] != minEcog || input$ecog[2] != maxEcog) {
      lungDisplay <- subset(lungDisplay, ph.ecog >= input$ecog[1] & ph.ecog <= input$ecog[2])
    }
    
    if(input$age[1] != minAge || input$age[2] != maxAge) {
      lungDisplay <- subset(lungDisplay, age >= input$age[1] & age <= input$age[2])
    }
    
    if(input$ageDead[1] != minAgeDead || input$ageDead[2] != maxAgeDead) {
      lungDisplay <- subset(lungDisplay, age.dead >= input$ageDead[1] & age.dead <= input$ageDead[2])
    }
    
    if(input$wtLoss[1] != minWtLoss || input$wtLoss[2] != maxWtLoss) {
      lungDisplay <- subset(lungDisplay, wt.loss >= input$wtLoss[1] & wt.loss <= input$wtLoss[2])
    }
    
    if(input$cal[1] != minCal || input$cal[2] != maxCal) {
      lungDisplay <- subset(lungDisplay, meal.cal >= input$cal[1] & meal.cal <= input$cal[2])
    }
    
    lungDisplay$SurvObj <- with(lungDisplay, Surv(time, status))
    
    plot(
      1,
      type="n",
      xlab="Czas (dni)",
      ylab="Przeżywalność (%)",
      xlim=c(0, maxTime),
      ylim=c(0, 1),
      main="Krzywe przeżywalności dla danych 'lung' z pakietu 'survival'"
    )
    
    if(nrow(lungDisplay) != 0) {
      if("conf" %in% input$sex) {
        confColor0 <- adjustcolor("darkorange", 0.4)
        confColor1 <- adjustcolor("blue", 0.4)
        confColor2 <- adjustcolor("magenta", 0.4)
      } else {
        confColor0 <- rgb(0, 0, 0, alpha=0)
        confColor1 <- rgb(0, 0, 0, alpha=0)
        confColor2 <- rgb(0, 0, 0, alpha=0)
      }
      
      if("center" %in% input$sex) {
        lines(
          survfit(
            SurvObj ~ 1,
            data=lungDisplay,
            conf.type="log-log"
          ),
          col=c("darkorange", confColor0, confColor0)
        )
      }
      
      if("male" %in% input$sex) {
        lines(
          survfit(
            SurvObj ~ sex,
            data=subset(lungDisplay, sex==1),
            conf.type="log-log"
          ),
          col=c("blue", confColor1, confColor1)
        )
      }
      
      if("female" %in% input$sex) {
        lines(
          survfit(
            SurvObj ~ sex,
            data=subset(lungDisplay, sex==2),
            conf.type="log-log"
          ),
          col=c("magenta", confColor2, confColor2)
        )
      }
    }
  })
}

shinyApp(ui, server)
