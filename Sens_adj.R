library(ggplot2)
library(shiny)
library(plotly)
library(formattable)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(reshape2)

work_dir <- "~/R_Shiny_proj/Sens_adj/"
setwd(work_dir)
nom_dir <- "./!Device_No-417/"

X <- function(p, C) { 1 / (p * C) }

RII <- function(ra, rb) { ra * rb / (ra + rb) }

Tran1 <- function(p, nominals) {
  nom <- nominals
  return (nom["R1"]) * p / p
}

Tran2 <-  function(p, nominals) {
  nom <- nominals
  return_val <- (2 * nom['R5'] / nom['R3'] /
                   (RII(nom['R7'], X(p, nom['C2'])) + 
                     nom['R8'] + X(p, nom['C1']))) * 
                 RII(nom['R11'], (RII(nom['R10'], nom['R9'] + nom['Rt1']) + 
                                      X(p, nom['C3'])))
  return (return_val)
}
  
Tran3 <- function(p, nominals) {
  nom <- nominals
  t1 <- RII(nom['R14'], nom['R15'] + nom['Rt3']) + X(p, nom['C4'])
  t2 <- nom['R16'] + X(p, nom['C5'])
  return_val <- RII(nom['R17'],
                    RII(
                        (RII(nom['R14'], nom['R15'] + nom['Rt3']) + X(p, nom['C4'])),
                        (nom['R16'] + X(p, nom['C5']))
                        )
                    ) / 
                RII(nom['R12'], nom['R13'] + nom['Rt2'])
  
  return (return_val)
}

Tran4 <- function(p, nominals) {
  nom <- nominals
  Z <- nom["R22"] + X(p, nom["C8"])
  Z1 <- RII(X(p, nom["C9"]), nom["R24"])
  Z2 <- RII(X(p, nom["c12"]), nom["R26"])
  t <- - 1 / (1 + (3 * Z) / Z2 + Z * Z / Z1 / Z2)
  return(t)
}

El_tr_fun <- function(p, noms) {
  func <- function(x) {
    imag = 2 * pi * x
    T1 <- Tran1(complex(real = 0, imaginary = imag), noms)
    T2 <- Tran2(complex(real = 0, imaginary = imag), noms)
    T3 <- Tran3(complex(real = 0, imaginary = imag), noms)
    T4 <- Tran4(complex(real = 0, imaginary = imag), noms) 
    return (T1 * T2 * T3 * T4)
  }  
  sapply(p, func)
}

sidebar_fun <- function(i, update=FALSE) {
  val <- nom_old[as.character(nom_adj$nominals[nom_adj$nominals == i])]
  val <- as.numeric(val)
  if (update == TRUE){
    updateSliderInput(
                inputId = nom_adj$nominals[nom_adj$nominals == i],
                label = nom_adj$nominals[nom_adj$nominals == i],
                min = as.numeric(nom_adj$min[nom_adj$nominals == i]), 
                max = nom_adj$max[nom_adj$nominals == i],
                step = nom_adj$step[nom_adj$nominals == i],
                value = val)    
  } else {
    sliderInput(nom_adj$nominals[nom_adj$nominals == i],
                nom_adj$nominals[nom_adj$nominals == i],
                min = as.numeric(nom_adj$min[nom_adj$nominals == i]), 
                max = nom_adj$max[nom_adj$nominals == i],
                step = nom_adj$step[nom_adj$nominals == i],
                value = val)
  }
}

casc_fun <- function(x) {
    id <- paste(x, '_box', sep='')
    box(width = '800px', 
        actionButton(inputId = x,
                     label = x,
                     width = '100%'),
        wellPanel(id = id,
                  lapply(nom_adj[nom_adj$casc == x, 'nominals'],
                                     sidebar_fun)))
}



sens_path <- paste(nom_dir, "Y_495.dat", sep = "")
nominals_path <- paste(nom_dir, "Y_495.mac", sep = "")

sens <- as.data.frame(
  read.table(sens_path, col.names = c("freq", "ampl", "phase")))

nominals <- as.data.frame(
  read.table(nominals_path, col.names = c("nominal", "value")))

nom_old <- setNames(nominals$value, nominals$nominal)
nom_new <- setNames(nominals$value, nominals$nominal)

nom_adj_path <- paste(nom_dir, "nom_adj.csv", sep = "")
nom_adj <- read.csv(nom_adj_path, header = T)
req_nom <- as.character(nom_adj$nominals)

new_ampl <-  Mod(sens$ampl / El_tr_fun(sens$freq, nom_old) *
                  El_tr_fun(sens$freq,nom_new))

sens_melt <-  cbind(sens, data.frame(new_ampl = new_ampl))
colnames(sens_melt)[c(2, 4)] <- c('Original', 'Adjusted')

sens_melt <- melt(sens_melt, id = c('freq', 'phase'),
                  value.name = 'ampl', variable.name = 'type')




ui <- fluidPage(
  chooseSliderSkin(skin = "Flat", color = "#112446"),
  useShinyjs(),
  # Application title
  titlePanel("Sensor adjustment"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width = 4,
                 lapply(unique(nom_adj$casc), casc_fun) 
    ),
    mainPanel(
      plotlyOutput("distPlot", width = '100%'),
      actionButton("default_nom", "Return default nominals"),
              
              )
    ),
  tags$style("
      .shiny-input-container { 
        margin-bottom: 30px; 
        width: 100%;
        left: 0
      }
      span { 
          margin-left: 2px;
      }
  "),
)


server <- function(input, output) {
  
  hide_box_func <- function(button_name) {
    box_id <- paste(button_name, '_box', sep = '')
    observeEvent(input[[button_name]],
                 {
                   if(input[[button_name]] %% 2 == 0){
                     shinyjs::hide(id = box_id)
                   } else {
                     shinyjs::show(id = box_id)
                   }
                   
                 })
  }
  
  breaks <- c(0.01, 0.1, 1, 10, 100)
  button_id <-  unique(nom_adj$casc)
  box_names <- paste(button_id, '_box', sep = '')
  
  
  # By default all sliders will be hidden 
  lapply(box_names, function(x) {shinyjs::hide(id = x)}) 
  # If press button col with cascade will be reveal
  lapply(button_id, hide_box_func) 
  # Button to return default nominals
  observeEvent(input$default_nom, {lapply(req_nom, sidebar_fun, c(update = TRUE))}) 
  # Characteristic plot
  output$distPlot <- renderPlotly(
    {
      for (i in req_nom) {
        nom_new[i] <- as.numeric(input[[i]])
      }
      
      sens_melt[sens_melt$type == 'Adjusted', 'ampl'] <-  Mod(sens$ampl /El_tr_fun(sens$freq, nom_old) *
                        El_tr_fun(sens$freq,nom_new))
      
      
      plotly_plot <- function() {
        ggplotly(ggplot(sens_melt, aes(x = freq, y = ampl, color = type)) +
                   geom_line() +
                   scale_x_continuous(name = 'Frequency, Hz', 
                                      breaks = breaks,
                                      labels = comma) +
                   scale_y_continuous(name = 'Amplitude, counts') +
                   theme_bw(base_size = 8) ) %>% 
          layout(margin=list(l = 70, b = - 1),
                 xaxis = list(type = 'log',
                              range = c(- 2, 2.5),
                              titlefont = list(size = 15),
                              tickfont = list(size = 12)),
                 yaxis = list(type = 'log',
                              autorange = TRUE,
                              tickfont = list(size = 12),
                              titlefont = list(size = 15))) 
      }

      plotly_plot()
    
    
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)













