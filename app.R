library(shinyWidgets)
library(flexdashboard)


ui <- fluidPage(
  
  titlePanel(title = div(img(src = "LOGO_color.jpg", height = 100, width = 300), br(),
                         HTML("Decision-making for <em>Phthorimaea absoluta</em> management: Sampling")), windowTitle = "Sampling"),
  
  
  sidebarLayout(
    
    sidebarPanel(
      
      strong("A. Information about the crop:"),
      
      br(),
      br(),
      
      numericInputIcon(inputId = "pr_p", label = "Tolerance level:", value = "", min = 0.1, max = 100, 
                       icon = list(NULL, icon("fa-regular fa-ban"))),
      numericInputIcon(inputId = "ds", label = "Crop age (in days after planting):", value = "", min = 0, max = 150,
                       icon = list(NULL, icon("fa-solid fa-calendar-days"))),
      numericInputIcon(inputId = "pls", label = "Number of plants in the greenhouse:", value = "", min = 10, max = 20000,
                       icon = list(NULL, icon("fa-regular fa-seedling"))),
      numericInputIcon(inputId = "surc", label = "Number of rows in the crop:", value = "", min = 3, max = 100,
                       icon = list(NULL, icon("fa-solid fa-grip-lines"))),
      actionButton("go", "Go!", icon = icon("play")),
      
    ),
    
    mainPanel(
      
      strong("B. Location of samples"),
      
      br(),
      br(),
      
      actionButton("muest", "Get location", lib = "font-awesome", icon = icon("fa-solid fa-person-walking-arrow-right"), verify_fa = FALSE),
      
      br(),
      br(),
      
      htmlOutput(outputId = "tam_muest", inline = FALSE, 
                 style = "font-weight: bold; font-size: 20px;"),
      
      br(),
      
      htmlOutput(outputId = "resp_mu", inline = TRUE, container = tags$strong),
      
      plotOutput(outputId = "MuesPlot"),
      
      strong("C. Sequential sampling of larvae"),
      
      br(),
      br(),
      
      numericInput(inputId = "counts", label = "Number of larvae in the sample:", value = 0, min = 0),
      actionButton("enter", "Enter", lib = "font-awesome", icon = icon("fa-thin fa-worm"), verify_fa = FALSE),
      br(),
      br(),
      
      htmlOutput(outputId = "ssize", inline = FALSE, 
                 style = "font-weight: bold; font-size: 22px;"),
      br(),
      br(),
      htmlOutput(outputId = "balance", inline = FALSE, 
                 style = "text-align: center; font-weight: bold; font-size: 20px; color: red"),
      br(),
      
      gaugeOutput("gauge"),
      
      strong("Counts log: ", inline = FALSE, style = "font-size: 20px;"),
      
      htmlOutput(outputId = "logs", inline = FALSE, 
                 style = "font-weight: bold; font-size: 20px;"),
      br(),
      br(),
      
      plotOutput(outputId = "distPlot"),
      
      br(),
      br(),
      actionButton("reset", "Restart sampling", icon = icon("refresh"), verify_fa = FALSE),
      br(),
      br(),
      br()
      
      
    )
  )
)

server <- function(input, output, session) {
  
  conteos <- reactiveValues(a = as.numeric(0))
  muestreos <- reactiveValues(a = 0)
  resp <- reactiveValues(a = "Continue sampling", b = NA)
  prop <- reactiveValues(a = as.numeric(0.1))
  ds <- reactiveValues(a = as.numeric(0))
  surc <- reactiveValues(a = as.numeric(20))
  pls <- reactiveValues(a = as.numeric(5000))
  resp_mu <- reactiveValues(a = 0, b = 0)
  
  observeEvent(input$go, {
    validate(
      need(input$pr_p != "", "Provide a valid number")
    )
    validate(
      need(input$ds != "", "Provide a valid number")
    )
    
    pls$a <- input$pls
    surc$a <- input$surc
    
    prop$a <- (input$pr_p) / 100
    ds$a <- input$ds
  })
  
  observeEvent(input$enter, {
    validate(
      need(input$counts != "", "Provide a valid number")
    )
    conteos$a <- c(conteos$a, input$counts)
    muestreos$a <- muestreos$a + 1
    resp$b <- sum(conteos$a) / muestreos$a
    updateNumericInput(session, "counts", value = "")
  })
  
  observeEvent(input$reset, {
    conteos$a <- 0
    muestreos$a <- 0
    resp$a <- "Continue sampling"
    resp$b <- NA
  })
  
  observeEvent(input$muest, {
    
    pl <- pls$a / surc$a
    
    segms <- floor(((pl * 30) / 100) / 4) + 1
    
    resp_mu$a <- sample(seq(1, surc$a), size = 1)
    resp_mu$b <- sample(seq(1, segms), size = 1)
    
  })
  
  output$resp_mu <- renderText({
    
    paste0("Row: ", resp_mu$a)
    
  })
  
  output$MuesPlot <- renderPlot({
    
    if(resp_mu$a == 0) {
      pl <- pls$a / surc$a
      
      par(mar = c(1, 4, 1, 2) + 0.1)
      plot(rep(1, pl), seq(1, pl), xlim = c(0.5, surc$a + 0.5), ylim = c(0, pl + 1), xlab ="", ylab = "", xaxt='n', yaxt='n')
      
      a <- seq(1, surc$a)
      
      for(i in 1: (surc$a - 1)) {
        points(rep(a[i + 1], pl), seq(1, pl))
      }
    } 
    else {
      pl <- pls$a / surc$a
      
      par(mar = c(1, 4, 1, 2) + 0.1)
      plot(rep(1, pl), seq(1, pl), xlim = c(0.5, surc$a + 0.5), ylim = c(0, pl + 1), xlab ="", ylab = "", xaxt='n', yaxt='n')
      
      a <- seq(1, surc$a)
      
      for(i in 1: (surc$a - 1)) {
        points(rep(a[i + 1], pl), seq(1, pl))
      }
      
      as1 <- resp_mu$a - 0.3
      bs1 <- resp_mu$a + 0.3
      
      as2 <- seq(1, pl, 13)[resp_mu$b]
      bs2 <- as2 + 13
      
      
      lines(c(as1, as1, bs1, bs1, as1), c(as2, bs2, bs2, as2, as2), lwd = 2, col = "red")
    }
    
  }
  
  )

  output$distPlot <- renderPlot({
    library("Deriv")
    
    deltamethod.m <- function (g, y, mean, cov, ses = TRUE) {
      cov <- as.matrix(cov)
      n <- length(mean)
      if (!is.list(g)) 
        g <- list(g)
      if ((dim(cov)[1] != n) || (dim(cov)[2] != n)) 
        stop(paste("Covariances should be a ", n, " by ", 
                   n, " matrix"))
      syms <- paste("x", 1:n, sep = "")
      for (i in 1:n) assign(syms[i], mean[i])
      gdashmu <- t(sapply(g, function(form) {
        as.numeric(eval(Deriv(form, syms), envir = list(x1 = mean[1], x2 = mean[2], y = y)))
      }))
      new.covar <- gdashmu %*% cov %*% t(gdashmu)
      if (ses) {
        new.se <- sqrt(diag(new.covar))
        new.se
      }
      else new.covar
    }
    
    esterr1 <- function(x) {
      if(x == 0) x <- .Machine$double.xmin
      if(x < 0.04070819) resp <- deltamethod.m(~ (log(x1 - y) - log(x1)) / (-x2), y = x, mean = c(0.05686059, 0.02517068),
                                               cov = matrix(c(8.870708e-05, -5.064045e-05, -5.064045e-05, 2.976344e-05), 2, 2)) * 1.96
      else resp <- deltamethod.m(~ (log(x1 - y) - log(x1)) / (-x2), y = 0.04070819, mean = c(0.05686059, 0.02517068),
                                 cov = matrix(c(8.870708e-05, -5.064045e-05, -5.064045e-05, 2.976344e-05), 2, 2)) * 1.96
      
      resp
    }
    
    esterr2 <- function(x) {
      if(x == 0) x <- .Machine$double.xmin
      if(x < 0.452476318) resp <- deltamethod.m(~ (log(x1 - y) - log(x1)) / (-x2), y = x, mean = c(0.4660213, 3.538212),
                                                cov = matrix(c(6.478702e-06, -0.0003757342, -3.757342e-04, 0.3129237006), 2, 2)) * 1.96
      else resp <- deltamethod.m(~ (log(x1 - y) - log(x1)) / (-x2), y = 0.452476318, mean = c(0.4660213, 3.538212),
                                 cov = matrix(c(6.478702e-06, -0.0003757342, -3.757342e-04, 0.3129237006), 2, 2)) * 1.96
      resp
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
    
    min.val <- 0
    
    thresh <- function(d, pr_p) {
      if(d <= 50) resul <- inv.ff(pr_p)
      if(d >= 90) resul <- inv.fun(pr_p)
      if((d > 50) && (d < 90)) resul <- inv.ff(pr_p) + ((d - 50) * ((inv.fun(pr_p) - inv.ff(pr_p)) / (40)))
      if(resul < 0) resul <- inv.ff(min.val)
      resul
    }
    
    thresh1 <- function(d, pr_p) {
      if(pr_p < min.val) {
        resul <- c(min.val, esterr1(min.val))
      } else {
        if(d <= 50) resul <- c((inv.ff(pr_p) - esterr1(pr_p)), (inv.ff(pr_p) + esterr1(pr_p)))
        if(d >= 90) resul <- c((inv.fun(pr_p) - esterr2(pr_p)), (inv.fun(pr_p) + esterr2(pr_p)))
        if((d > 50) && (d < 90)) resul <- c(((inv.ff(pr_p) + ((d - 50) * ((inv.fun(pr_p) - inv.ff(pr_p)) / (40)))) - 
                                               (esterr1(pr_p) + ((d - 50) * ((esterr2(pr_p) - esterr1(pr_p)) / (40))))),
                                            ((inv.ff(pr_p) + ((d - 50) * ((inv.fun(pr_p) - inv.ff(pr_p)) / (40)))) + 
                                               (esterr1(pr_p) + ((d - 50) * ((esterr2(pr_p) - esterr1(pr_p)) / (40))))))
      }
      if (resul[1] < 0) resul <- c(.Machine$double.xmin, resul[2])
      resul
    }
    
    trip_mod <- function(y0) {
      r <- 0.1030816
      y0 * exp(-r * 5)
    }
    
    trip_mod1 <- function(d, pr_p) {
      pu <- thresh(d, pr_p)
      a <- trip_mod(pu)
      
      if((a - esterr1(pr_p)) < 0) {
        resul <- c(.Machine$double.xmin, (a + esterr1(pr_p)))
      } else {
        if(d <= 50) resul <- c((a - esterr1(pr_p)), (a + esterr1(pr_p)))
        if(d >= 90) resul <- c((a - esterr2(pr_p)), (a + esterr2(pr_p)))
        if((d > 50) && (d < 90)) resul <- c((a - (esterr1(pr_p) + ((d - 50) * ((esterr2(pr_p) - esterr1(pr_p)) / (40))))),
                                            ((a + (esterr1(pr_p) + ((d - 50) * ((esterr2(pr_p) - esterr1(pr_p)) / (40)))))))
      }
      
      resul
    }
    
    umprin <- thresh1(ds$a, prop$a)
    umprin1 <- thresh(ds$a, prop$a)
    
    k_est1 <- function(me, a, b) {
      if(me < 0.19332) 
        res <- 0.6931779
      else
        res <- (me^2) / ((a * me^(b)) - me)
      
      res
    }
    
    k_um <- k_est1(thresh(ds$a, prop$a), a = exp(0.6043225), b = 1.218041)
    
    low_int_nb <- function(al, be, me0, me1, k_est){
      (log(be / (1 - al))) / (log((me1 * (me0 + k_est)) / (me0 * (me1 + k_est))))
    }
    
    low_um_p <- low_int_nb(al = 0.1, be = 0.1, me0 = umprin[1], me1 = umprin[2], k_est = k_um)
    
    
    hi_int_nb <- function(al, be, me0, me1, k_est){
      (log((1 - be) / (al))) / (log((me1 * (me0 + k_est)) / (me0 * (me1 + k_est))))
    }
    
    hi_um_p <- hi_int_nb(al = 0.1, be = 0.1, me0 = umprin[1], me1 = umprin[2], k_est = k_um)
    
    ll_sl_nb <- function(al, be, me0, me1, k_est){
      (k_est * log((me1 + k_est) / (me0 + k_est))) /
        (log((me1 * (me0 + k_est)) / (me0 * (me1 + k_est))))
    }
    
    pen_p <- ll_sl_nb(al = 0.1, be = 0.1, me0 = umprin[1], me1 = umprin[2], k_est = k_um)
    
    low_seq_c <- function(x){
      pen_p * x + low_um_p
    }
    
    hi_seq_c <- function(x){
      pen_p * x + hi_um_p
    }
    
    
    intersect <- function(l1, l2){
      x <- (l2[1] - l1[1]) / (l1[2] - l2[2])
      y <- l1[1] + l1[2] * x
      return(xy=c(x, y))
    }
    
    k_um_ant <- k_est1(trip_mod(thresh(ds$a, prop$a)), a = exp(0.6043225), b = 1.218041)
    
    low_ant <- low_int_nb(al = 0.1, be = 0.1, me0 = trip_mod1(ds$a, prop$a)[1], me1 = trip_mod1(ds$a, prop$a)[2], k_est = k_um_ant)
    hi_ant <- hi_int_nb(al = 0.1, be = 0.1, me0 = trip_mod1(ds$a, prop$a)[1], me1 = trip_mod1(ds$a, prop$a)[2], k_est = k_um_ant)
    pen_ant <- ll_sl_nb(al = 0.1, be = 0.1, me0 = trip_mod1(ds$a, prop$a)[1], me1 = trip_mod1(ds$a, prop$a)[2], k_est = k_um_ant)
    
    low_seq_ant <- function(x){
      pen_ant * x + low_ant
    }
    
    hi_seq_ant <- function(x){
      pen_ant * x + hi_ant
    }
    
    xlimi <- intersect(c((hi_seq_c(0) * 5), 0), c(low_ant, pen_ant))[1]
    
    if(xlimi > 30) xlimi <- 30
    
    if (muestreos$a == 0) {
      par(mar = c(5, 4, 0, 2) + 0.1)
      plot(100, 100, xlim = c(1, xlimi), ylim = c(0, (hi_seq_c(0) * 5)), xlab = "Number of samples examined", ylab = "Cumulative number of larvae",
           cex.lab = 1.5, yaxt = "n", xaxt = "n", yaxs = "i")
      
      axis(2, at = seq(0, (hi_seq_c(0) * 5), 10), labels = TRUE, cex = 1.5)
      axis(1, at = seq(0, 30, 1), labels = TRUE, cex = 1.5)
      
      abline(a = low_um_p, b = pen_p, lwd = 2)
      abline(a = hi_um_p, b = pen_p, lwd = 2)
      
      
      abline(a = hi_ant, b = pen_ant, lwd = 2)
      abline(a = low_ant, b = pen_ant, lwd = 2)
      
      
      segments(intersect(c(low_ant, pen_ant), c(low_um_p, pen_p))[1], intersect(c(low_ant, pen_ant), c(low_um_p, pen_p))[2],
               intersect(c(hi_ant, pen_ant), c(low_um_p, pen_p))[1], intersect(c(hi_ant, pen_ant), c(low_um_p, pen_p))[2], col = "white", lwd = 3)
      
      segments(0, hi_ant,
               intersect(c(hi_ant, pen_ant), c(low_um_p, pen_p))[1], intersect(c(hi_ant, pen_ant), c(low_um_p, pen_p))[2], col = "white", lwd = 3)
      
      segments((-low_ant / pen_ant), 0,
               intersect(c(low_ant, pen_ant), c(low_um_p, pen_p))[1], intersect(c(low_ant, pen_ant), c(low_um_p, pen_p))[2], col = "white", lwd = 3)
      
    }
    else {
      par(mar = c(5, 4, 0, 2) + 0.1)
      plot(100, 100, xlim = c(1, xlimi), ylim = c(0, (hi_seq_c(0) * 5)), xlab = "Number of samples examined", ylab = "Cumulative number of larvae",
           cex.lab = 1.5, yaxt = "n", xaxt = "n", yaxs = "i")
      
      axis(2, at = seq(0, (hi_seq_c(0) * 5), 10), labels = TRUE, cex = 1.5)
      axis(1, at = seq(0, 30, 1), labels = TRUE, cex = 1.5)
      
      abline(a = low_um_p, b = pen_p, lwd = 2)
      abline(a = hi_um_p, b = pen_p, lwd = 2)
      
      
      abline(a = hi_ant, b = pen_ant, lwd = 2)
      abline(a = low_ant, b = pen_ant, lwd = 2)
      
      
      segments(intersect(c(low_ant, pen_ant), c(low_um_p, pen_p))[1], intersect(c(low_ant, pen_ant), c(low_um_p, pen_p))[2],
               intersect(c(hi_ant, pen_ant), c(low_um_p, pen_p))[1], intersect(c(hi_ant, pen_ant), c(low_um_p, pen_p))[2], col = "white", lwd = 3)
      
      segments(0, hi_ant,
               intersect(c(hi_ant, pen_ant), c(low_um_p, pen_p))[1], intersect(c(hi_ant, pen_ant), c(low_um_p, pen_p))[2], col = "white", lwd = 3)
      
      segments((-low_ant / pen_ant), 0,
               intersect(c(low_ant, pen_ant), c(low_um_p, pen_p))[1], intersect(c(low_ant, pen_ant), c(low_um_p, pen_p))[2], col = "white", lwd = 3)
      
      points(seq(0, muestreos$a), cumsum(conteos$a), lwd = 2, col = c(0, rep("red", (muestreos$a))))
      
      if(muestreos$a > 19) {
        
        resp$b <- sum(conteos$a) / muestreos$a
        
        if(length(which(conteos$a == 0)) >= 17) {
          resp$b <- (2 * umprin1)/3
        }
        
        if (resp$b > ((3 * umprin1)/4)) {
          resp$a <- "Stop! The risk of economic damage is high: For prevention, apply a control measure."
        } else {
          resp$a <- "Stop! The risk of economic damage is medium: A control measure is not recommended for now. Please sample again in 5 days."
        }
        
      }
      
      if((muestreos$a > 14) & (sum(conteos$a) == 0)) {
        resp$a <- "Stop! The risk of economic damage is medium: A control measure is not recommended for now. Please sample again in 5 days."
      }
      
      if (((sum(conteos$a)) > (hi_seq_c(muestreos$a))) | (((sum(conteos$a)) < (low_seq_ant(muestreos$a))) & ((sum(conteos$a)) < (low_seq_c(muestreos$a)))) | 
          ((sum(conteos$a)) < (low_seq_c(muestreos$a)) & (sum(conteos$a)) > (hi_seq_ant(muestreos$a)))) {
        
        if ((sum(conteos$a)) > (hi_seq_c(muestreos$a))) {
          resp$a <- "Stop! Apply a control measure as soon as possible!"
        }
        if ((((sum(conteos$a)) < (low_seq_ant(muestreos$a))) & ((sum(conteos$a)) < (low_seq_c(muestreos$a))))) {
          resp$a <- "Stop! A control measure is not recommended for now. Please, sample again in 10 days."
        }
        if (((sum(conteos$a)) < (low_seq_c(muestreos$a)) & (sum(conteos$a)) > (hi_seq_ant(muestreos$a)))) {
          resp$a <- "Stop! A control measure is not recommended for now. Please, sample again in 5 days."
        }
        
        resp$b <- sum(conteos$a) / muestreos$a
      }
      
      
    }
    
    output$balance <- renderText({
      paste0(resp$a)
      
    })
    
    output$tam_muest <- renderText({
      if(ds$a <= 50) ss <- c("Sampling should be carried out from 3 plants per location.")
      if((ds$a > 50) & (ds$a <= 90)) ss <- c("Sampling should be carried out from 2 plants per location.")
      if(ds$a > 90) ss <- c("Sampling should be carried out from 1 plant per location.")
      ss
    })
    
    output$logs <- renderText({
      paste(conteos$a[-1], collapse = ", ")
      
    })
    
    output$ssize <- renderText({
      paste("Number of samples = ", length(conteos$a[-1]))
    })
    
    output$gauge = renderGauge({
      gauge(round(resp$b, digits = 3), 
            min = 0, 
            max = round(umprin1, digits = 3),
            label = "Level of risk",
            sectors = gaugeSectors(success = c(0, umprin1/2), 
                                   warning = c(umprin1/2, (3 * umprin1)/4),
                                   danger = c((3 * umprin1)/4, umprin1)))
    })
    
  })
}

shinyApp(ui = ui, server = server)