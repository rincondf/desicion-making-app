library(shinyWidgets)
library(magick)

ui <- fluidPage(
  
  titlePanel(title = div(img(src = "LOGO_color.jpg", height = 100, width = 300), br(),
                         HTML("Decision-making for <em>Phthorimaea absoluta</em> management: Pest tolerance estimation")), windowTitle = "Tolerance"),
  
  fluidRow(
    column(
      
      br(),
      strong("A. Expenses associated with pesticide applications"),
      br(),
      br(),
      
      numericInputIcon(inputId = "jor",
                       label = "Labor wage cost per application ($):",
                       min = 0, 
                       value = "",
                       icon = list(NULL, icon("dollar-sign"))),
      
      numericInputIcon(inputId = "mot",
                       label = "Motorized water pump usage ($):",
                       min = 0,
                       value = "", icon = list(NULL, icon("dollar-sign"))),
      
      numericInputIcon(inputId = "ins",
                       label = "Cost of insecticide per application ($):",
                       min = 0,
                       value = "", icon = list(NULL, icon("dollar-sign"))),
      
      numericInputIcon(inputId = "ag",
                       label = "Cost of water per application ($):",
                       min = 0,
                       value = "", icon = list(NULL, icon("dollar-sign"))),
      
      numericInputIcon(inputId = "adit",
                       label = "Cost fo additives required per application ($):",
                       min = 0,
                       value = "", icon = list(NULL, icon("dollar-sign"))),
      
      width = 3
    ),
    
    column(
      
      br(),
      
      strong("B. Information about the crop"),
      br(),
      br(),
      
      numericInputIcon(inputId = "np",
                       label = "Number of plants in the greenhouse:",
                       min = 50,
                       max = 10000,
                       value = "", icon = list(NULL, icon("fa-regular fa-seedling"))),
      
      numericInputIcon(inputId = "dia",
                       label = "Age of the crop in days after planting:",
                       min = 1,
                       max = 200,
                       value = "",
                       icon = list(NULL, icon("fa-solid fa-calendar-days"))),
      
      numericInputIcon(inputId = "ni", 
                       label = "Cumulative control costs ($):",
                       value = "", min = 0, 
                       icon = list(NULL, icon("dollar-sign"))),
      
      numericInputIcon(inputId = "cos", 
                       label = "Tomato harvested so far (kg):",
                       value = 0, min = 0, 
                       icon = list(NULL, icon("fa-solid fa-basket-shopping"))),
      
      br(),
      br(),
      
      strong("C. Predictions"),
      
      br(),
      br(),
      
      
      numericInputIcon(inputId = "st",
                       label = "Tomato price in the market per kg ($):",
                       min = 0.5,
                       value = "", icon = list(NULL, icon("dollar-sign"))),
      
      radioButtons(inputId = "pt", label = "Your expected yield is:",
                   choices = c("Low (3kg/plant)", "Average (4kg/plant)", "High (5kg/plant)"), selected = "Low (3kg/plant)"
      ),
      
      
      br(),
      
      width = 3
      
    ),
    
    
    column(
      
      br(),
      
      strong("Your tolerance level is:", style = "font-size:20px;"),
      
      br(),
      br(),
      
      htmlOutput(outputId = "result", inline = FALSE, style = "font-size: 35px; font-weight: bold; color: red; text-align: center;
                 border: 4px solid red"),
      
      br(),
      br(),
      
      strong("Now, please memorize your tolerance level and continue to the next module for sampling:", style = "color:red"),
      
      br(),
      br(),
      
      actionButton(inputId='ab1', label="Sampling",
                   icon = icon("fa-solid fa-hand-point-right"), 
                   onclick ="window.open('https://diego-rincon.shinyapps.io/app3_en/', '_blank')"),
      br(),
      br(),
      
      width = 4
    )
  )
)


server <- function(input, output, session) {
  
  
  
  output$result <- renderText({
    
    pta <- function(x) {
      if(x == "Low (3kg/plant)") ss <- 3
      if(x == "Average (4kg/plant)") ss <- 4
      if(x == "High (5kg/plant)") ss <- 5
      ss
    }
    
    inv.ff <- function(y) {
      x1 <- 0.05686059
      x2 <- 0.02517068
      if(y < 0.04070819) res <- (log(x1 - y) - log(x1)) / (-x2)
      else res <- 50
      res
    }
    
    
    inv.fun <- function(y) {
      x1 <- 0.4660213
      x2 <- 3.538212
      if(y < 0.452476318) res <- (log(x1 - y) - log(x1)) / (-x2)
      else res <- 1
      res
    }
    
    si <- function(ag, ins, jor, mot, adit) { 
      sum(c(ag, ins, jor, mot, adit))
    }
    
    thresh <- function(d, pr_p) {
      if(d < 50) resul <- inv.ff(pr_p)
      if(d > 90) resul <- inv.fun(pr_p)
      if((d >= 50) && (d <= 90)) resul <- inv.ff(pr_p) + ((d - 50) * ((inv.fun(pr_p) - inv.ff(pr_p)) / (40)))
      if(resul < 0) resul <- 0
      resul
    }
    
    pr_tl <- function(si, ni, st, pt, np, cos) {
      (si + ni) / (st * ((pt * np) - cos))
    }
    
    if(any(is.na(c(si(input$ag, input$ins, input$jor, input$mot, input$adit), input$ni, input$st, pta(input$pt), input$np, input$cos)))) {
      res0 <- "Incomplete information"
    } else {
      res0 <- (pr_tl(si(input$ag, input$ins, input$jor, input$mot, input$adit), input$ni, input$st, pta(input$pt), input$np, input$cos)) * 100
    }
    
    
    if(is.numeric(res0)) {
      res0 <- round(res0, digits = 1)
    }
    
    res0
    
    
    
    
  })
  
}


shinyApp(ui = ui, server = server)