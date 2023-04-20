
library(shiny)
library(scales)
library(forecast)
library(plotrix)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
    
    sidebarLayout(
        
        sidebarPanel(
            
            # tags$hr(),
            # tags$h3("Change the inputs"),
            tags$div(HTML("<span style='margin-top: 25pt; font-size: 18pt'>Specification</span>")),
            p("of an ARMA(p,q):", 
              style = "font-size:14pt; text-align: justify"),
            p("General formula:", 
              style = "font-size:14pt; text-align: justify"),
            htmlOutput("generalformula"),
            #htmlOutput("armamodel"),
    
            tags$div(HTML("<span style='margin-top: 25pt; font-size: 18pt'>Change Parameters</span>")),
            
            
            # Input 1: AR ----
            p("AR Parameters:", 
              style = "font-size:14pt; text-align: justify"),
            sliderInput(inputId = "ar1",
                        label = withMathJax(
                            '\\( \\phi_1\\)'
                        ),
                        min = as.numeric(-0.99),
                        max =  as.numeric(0.99),
                        value = as.numeric(0),
                        # round = FALSE,
                        step = 0.10),
            
            sliderInput(inputId = "ar2",
                        label = withMathJax(
                            '\\( \\phi_2\\)'
                        ),
                        min = as.numeric(-0.99),
                        max =  as.numeric(0.99),
                        value = as.numeric(0),
                        # round = FALSE,
                        step = 0.10),
            
            # Input 2: MA ----
            p("MA Parameters:", 
              style = "font-size:14pt; text-align: justify"),
            sliderInput(inputId = "ma1",
                        label = withMathJax(
                            '\\( \\theta_1\\)'
                        ),
                        min = as.numeric(-0.99),
                        max =  as.numeric(0.99),
                        value = as.numeric(0),
                        # round = FALSE,
                        step = 0.10),
            
            sliderInput(inputId = "ma2",
                        label = withMathJax(
                            '\\( \\theta_2\\)'
                        ),
                        min = as.numeric(-0.99),
                        max =  as.numeric(0.99),
                        value = as.numeric(0),
                        # round = FALSE,
                        step = 0.10),            
            
            tags$div(HTML("<span style='text-decoration: none; font-size: 12pt; margin-top: 25pt'> $$ *\\mu \\text{ is set to 0 in this example}$$ <span> <script>if (window.MathJax) MathJax.Hub.Queue(['Typeset', MathJax.Hub]);</script>"))
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            # hot fix für abstand oben
            tags$div(HTML("<span style='margin-top: 10pt; font-size: 14pt; color: white'>1</span>")),
            # musste ich verschieben, da sonst die formel nicht gut zu sehen war
            htmlOutput("armamodel"),
            fluidRow(
                splitLayout(cellWidths = c("63%", "37%"),
                            plotOutput("tsplot"),
                            plotOutput("rootsplot"))
            ),
            fluidRow(
                splitLayout(cellWidths = c("50%", "50%"), 
                            plotOutput("acfplot"), 
                            plotOutput("pacfplot"))
            ),
        )
    )
)

server <- function(input, output) {
    
    # Simulate ARMA -------

    Arma <- reactive({
        # Simulate one trajectory here
        tt <- 200     # number of relalizations
        set.seed(125) # set seed
        
        
        
        sd <- 1
        ar1 <- input$ar1
        ar2 <- input$ar2
        ma1 <- input$ma1
        ma2 <- input$ma2
        
        y.sim <- NULL
        
        try( y.sim <- arima.sim(n = tt, model = list(ar = c(ar1, ar2), ma = c(ma1, ma2)),
                                rand.gen = function(n, ...) {rnorm(tt, 0, sd)}), silent = TRUE)
        
        if (!is.null(y.sim)) {
            return(y.sim)
        } else {
            return(NULL)
        }
        
        })
    
    
    #..................................................
    
    
    # Plot 1:  ----
    output$tsplot <- renderPlot({
        
        if (!is.null(Arma())) {
            plot(Arma(), xlab = "tt", ylab = "Value",
                 ylim=c(-4,4))
            abline(a = 0, b = 0, col = "red")
            
        } else {
            plot(x = 0:1, # Create empty plot
                 y = 0:1,
                 ann = F,
                 bty = "n",
                 type = "n",
                 xaxt = "n",
                 yaxt = "n")
            text(x = 0.5, # Add text to empty plot
                 y = 0.5,
                 # "This is my first line of text!\nAnother line of text.\n(Created by Base R)", 
                 "Choose a stable ARMA specification \n to display the time series",
                 cex = 2)
        }
    }, height = 350)
    
    #..................................................
    # Plot 2: CDF ----
    output$rootsplot <- renderPlot({
        
        ar1 <- input$ar1
        ar2 <- input$ar2
        
        plot(1/polyroot(c(1,-c(ar1,ar2))),
             col = "red", cex = 1.5, lwd = 3,
             ylim=c(-1.5,1.5),xlim=c(-1.5,1.5),
             xlab = c("Real Part"),
             ylab = c("Imaginary Part"),
             asp=1)
        abline(h=0,lty=2)
        abline(v=0,lty=2)
        draw.circle(x=0,y=0,r=1)
        
    }, height = 350, width = 350)
    
    # Plot 3:  ----
    output$acfplot <- renderPlot({
        
        if (!is.null(Arma())) {
            Acf(Arma(), main="Autocorrelation function")
            
        } else {
            plot(x = 0:1, # Create empty plot
                 y = 0:1,
                 ann = F,
                 bty = "n",
                 type = "n",
                 xaxt = "n",
                 yaxt = "n")
            text(x = 0.5, # Add text to empty plot
                 y = 0.5,
                 # "This is my first line of text!\nAnother line of text.\n(Created by Base R)", 
                 "Choose a stable ARMA specification \n to display the acf",
                 cex = 2)
        }
        
    }, height = 350)
    
    # Plot 3:  ----
    output$pacfplot <- renderPlot({
        
        if (!is.null(Arma())) {
            Pacf(Arma(), main="Partial autocorrelation function")
            
        } else {
            plot(x = 0:1, # Create empty plot
                 y = 0:1,
                 ann = F,
                 bty = "n",
                 type = "n",
                 xaxt = "n",
                 yaxt = "n")
            text(x = 0.5, # Add text to empty plot
                 y = 0.5,
                 # "This is my first line of text!\nAnother line of text.\n(Created by Base R)", 
                 "Choose a stable ARMA specification \n to display the pacf",
                 cex = 2)
        }
        
    }, height = 350)
    
    # Interpretation: p-value ----
    output$armamodel <- renderText({ 
        
        ar1 = input$ar1
        ar2 = input$ar2
        ma1 = input$ma1
        ma2 = input$ma2
        
        if (ar1 == 0 & ar2 == 0 & ma1 == 0 & ma2 == 0) {
            paste0("<span style='text-decoration: none; font-size: 14pt'> 
                   The selection corresponds to an ARMA(0,0) model: 
                   $$\\begin{align*} 
                    y_t &= \\varepsilon_t \\end{align*}$$
                   <span> <script>if (window.MathJax) MathJax.Hub.Queue(['Typeset', MathJax.Hub]);</script>")
            
        } else if (ar1 != 0 & ar2 == 0 & ma1 == 0 & ma2 == 0) {
            paste0("<span style='text-decoration: none; font-size: 14pt'> 
                   The selection corresponds to an AR(1) model: 
                   $$\\begin{align*} 
                    y_t &= (",
                   format(as.numeric(ar1), digits=2), ")y_{t-1} + \\varepsilon_t \\end{align*}$$
                   <span> <script>if (window.MathJax) MathJax.Hub.Queue(['Typeset', MathJax.Hub]);</script>")
            
        } else if (ar2 != 0 & ma1 == 0 & ma2 == 0) {
            paste0("<span style='text-decoration: none; font-size: 14pt'> 
                   The selection corresponds to an AR(2) model: 
                   $$\\begin{align*} 
                    y_t &= (" ,
                   format(as.numeric(ar1), digits=2), ")y_{t-1} + (", 
                   format(as.numeric(ar2), digits=2), ")y_{t-2} + \\varepsilon_t \\end{align*}$$
                   <span> <script>if (window.MathJax) MathJax.Hub.Queue(['Typeset', MathJax.Hub]);</script>")
            
        } else if (ar1 == 0 & ar2 == 0 & ma1 != 0 & ma2 == 0) {
            paste0("<span style='text-decoration: none; font-size: 14pt'> 
                   The selection corresponds to an MA(1) model: 
                   $$\\begin{align*} 
                    y_t &= \\varepsilon_t + (" ,
                   format(as.numeric(ma1, digits=2)), ")\\varepsilon_{t-1}  \\end{align*}$$
                   <span> <script>if (window.MathJax) MathJax.Hub.Queue(['Typeset', MathJax.Hub]);</script>")
            
        } else if (ar1 == 0 & ar2 == 0 & ma2 != 0) {
            paste0("<span style='text-decoration: none; font-size: 14pt'> 
                   The selection corresponds to an MA(2) model: 
                   $$\\begin{align*} 
                    y_t &=  \\varepsilon_t + (" ,
                   format(as.numeric(ma1, digits=2)), ")\\varepsilon_{t-1} + (", 
                   format(as.numeric(ma2, digits=2)), ")\\varepsilon_{t-2}  \\end{align*}$$
                   <span> <script>if (window.MathJax) MathJax.Hub.Queue(['Typeset', MathJax.Hub]);</script>")
            
        } else if (ar1 != 0 & ar2 == 0 & ma1 != 0 & ma2 == 0) {
            paste0("<span style='text-decoration: none; font-size: 14pt'> 
                   The selection corresponds to an ARMA(1,1) model: 
                   $$\\begin{align*} 
                    y_t &= (",
                   format(as.numeric(ar1, digits=2)),")y_{t-1} + \\varepsilon_t + (" ,
                   format(as.numeric(ma1, digits=2)), ")\\varepsilon_{t-1} \\end{align*}$$
                   <span> <script>if (window.MathJax) MathJax.Hub.Queue(['Typeset', MathJax.Hub]);</script>")
            
        } else if (ar2 != 0 & ma1 != 0 & ma2 == 0) {
            paste0("<span style='text-decoration: none; font-size: 14pt'> 
                   The selection corresponds to an ARMA(2,1) model: 
                   $$\\begin{align*} 
                    y_t &= (",
                   format(as.numeric(ar1, digits=2)),") y_{t-1} + (",
                   format(as.numeric(ar2, digits=2)),") y_{t-2} + \\varepsilon_t + (" ,
                   format(as.numeric(ma1, digits=2)), ")\\varepsilon_{t-1} \\end{align*}$$
                   <span> <script>if (window.MathJax) MathJax.Hub.Queue(['Typeset', MathJax.Hub]);</script>")
            
        } else if (ar1 != 0 & ar2 == 0  & ma2 != 0) {
            paste0("<span style='text-decoration: none; font-size: 14pt'> 
                   The selection corresponds to an ARMA(1,2) model: 
                   $$\\begin{align*} 
                    y_t &= (",
                   format(as.numeric(ar1, digits=2)),")y_{t-1} +  \\varepsilon_t + (" ,
                   format(as.numeric(ma1, digits=2)), ")\\varepsilon_{t-1} + (",
                   format(as.numeric(ma2, digits=2)), ")\\varepsilon_{t-2} \\end{align*}$$
                   <span> <script>if (window.MathJax) MathJax.Hub.Queue(['Typeset', MathJax.Hub]);</script>")
            
        } else if (ar1 != 0 & ar2 != 0  & ma1!= 0 & ma2 != 0) {
            paste0("<span style='text-decoration: none; font-size: 14pt'> 
                   The selection corresponds to an ARMA(2,2) model: 
                   $$\\begin{align*} 
                    y_t &= (",
                   format(as.numeric(ar1, digits=2)), ")y_{t-1} + (",
                   format(as.numeric(ar2, digits=2)), ")y_{t-2} + \\varepsilon_t + (" ,
                   format(as.numeric(ma1, digits=2)), ")\\varepsilon_{t-1} + (",
                   format(as.numeric(ma2, digits=2)), ")\\varepsilon_{t-2} \\end{align*}$$
                   <span> <script>if (window.MathJax) MathJax.Hub.Queue(['Typeset', MathJax.Hub]);</script>")
            
        } 
        
        
    })
    
    output$generalformula <- renderText({
        paste0("<span style='text-decoration: none; font-size: 14pt'> 
                   $$\\begin{align*} y_t = \\mu^* &+ \\phi_1y_{t-1} + ... + \\phi_py_{t-p} + \\varepsilon_t
                    \\\\ &+ \\theta_1\\varepsilon_{t-1} + ... + \\theta_q\\varepsilon_{t-q} \\end{align*}$$
                   <span> <script>if (window.MathJax) MathJax.Hub.Queue(['Typeset', MathJax.Hub]);</script>")
        
    })

    
    
}

shinyApp(ui = ui, server = server)