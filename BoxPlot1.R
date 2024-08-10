

# Carregar os pacotes necessários
library(ggplot2)
library(plotly)
library(fontawesome)  # Para ícones Font Awesome
library(shiny)                                                                  # Criação de aplicativos web interativos em R.
library(shinydashboard)                                                         # Fornecer layouts e componentes específicos para criar dashboards em Shiny.
library(shinydashboardPlus)                                                     # Extensão do shinydashboard que adiciona recursos extras.
library(shinythemes)
library(shinyWidgets)                                                           # Widgets adicionais e personalizados para aplicativos Shiny.
library(shinycssloaders)                                                        # Indicadores de carregamento e animações CSS para aplicativos Shiny.
library(shinyjs)                                                                # Adição de interações JavaScript em aplicativos Shiny.
library(shinyauthr)
library(shinyLP)
library(shinybusy)
library(shinyBS)
library(htmltools)                                                              # Criação e manipulação de elementos HTML em R.
library(htmlwidgets)                                                            # Criação e manipulação de widgets HTML em R.
library(mathjaxr)
library(rintrojs)



mtcars

# Interface do usuário (UI)
ui <- dashboardPage(skin = "blue",
                    scrollToTop = TRUE,
                    options = list(sidebarExpandOnHover = TRUE),
  dashboardHeader(titleWidth = 320,
    title = tags$div(
      HTML("<h1 style='font-size: 20px; font-weight: bold;'><i class='fa fa-bar-chart' aria-hidden='true'></i> BOXPLOT: Um Guia Prático</h1>"),
      style = "display: flex; align-items: center;"  # Alinha o ícone e o texto verticalmente
    ),

  #===============================================================================#
  # 2.2.1 Redes Sociais ----------------------------------------------------------
  tags$li(class = "dropdown",
          a(href = "https://www.facebook.com/detranPARA",
            class = "fa fa-facebook",
            target = "_blank"
          )
  ),
  tags$li(class = "dropdown",
          a(href = "https://www.instagram.com/detranpa_",
            class = "fa fa-instagram",
            target = "_blank"
          )
  ),
  tags$li(class = "dropdown",
          a(href = "https://twitter.com/DETRAN_PA",
            class = "fa fa-twitter",
            target = "_blank"
          )
  ),
  tags$li(
    class="dropdown",
    tags$a(href="https://github.com/MarioDhiego",
           icon("github"), "Suporte", target = "_blank") 
  )
  ),
#===============================================================================#
  dashboardSidebar(
    sidebarMenu(
      menuItem("Boxplot Clássico", tabName = "boxplots", icon = icon("box")),
      menuItem("ViolinPlot Clássico", tabName = "violinplots", icon = icon("box")),
      menuItem("Boxplot com Jitter", tabName = "boxplot_jitter", icon = icon("box"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "boxplots",
              selectInput(
                inputId = "boxplot_type",
                label = "TIPOS de Boxplot:",
                choices = c("Boxplot Classico", "Boxplot Horizontal"),
                selected = "Boxplot Classico"
              ),
              plotlyOutput("boxplot",
                           height = "600px",
                           width = "800px")
      ),
      tabItem(tabName = "violinplots",
              selectInput(
                inputId = "violinplot_type",
                label = "TIPOS de Violinplot:",
                choices = c("Violinplot Simples", "Violinplot Colorido"),
                selected = "Violinplot Simples"
              ),
              plotlyOutput("violinplot",
                           height = "600px",
                           width = "800px")
      ),
      tabItem(tabName = "boxplot_jitter",
              selectInput(
                inputId = "boxplot_jitter_type",
                label = "Selecione o tipo de Boxplot com Jitter:",
                choices = c("Boxplot com Jitter Simples", "Boxplot com Jitter Colorido"),
                selected = "Boxplot com Jitter Simples"
              ),
              plotlyOutput("boxplot_jitter",
                           height = "600px",
                           width = "800px")
      )
    )
  )
,
footer = dashboardFooter(
  left = tags$b("Mário Dhiego"), 
  right = tags$b("Belém-PA, 2024 v.1")
),
title = "Dashboard"
)



# Servidor (Server)
server <- function(input, output, session) {
  output$boxplot <- renderPlotly({
    plot <- ggplot(mtcars, aes(x = factor(cyl), y = mpg))
    
    if (input$boxplot_type == "Boxplot Classico") {
      plot <- plot + geom_boxplot(aes(fill = factor(cyl)))
      
    } else if (input$boxplot_type == "Boxplot Horizontal") {
      plot <- plot + geom_boxplot(aes(fill = factor(cyl))) + coord_flip()
    }
    
    plot <- plot + labs(x = "Cilindradas", 
                        y = "Milhas por Galão", 
                        title = "Boxplot Clássico") +
      theme_gray()
    
    ggplotly(plot, width = 800, height = 600) # Ajustando largura e altura
  })
  
  output$violinplot <- renderPlotly({
    plot <- ggplot(mtcars, aes(x = factor(cyl), y = mpg))
    
    if (input$violinplot_type == "Violinplot Simples") {
      plot <- plot + geom_violin()
      
    } else if (input$violinplot_type == "Violinplot Colorido") {
      plot <- plot + geom_violin(aes(fill = factor(cyl)), alpha = 0.6)
    }
    
    plot <- plot + labs(x = "Cylinders", 
                        y = "Miles per Gallon", 
                        title = "Violinplot Example")+
      theme_gray()
    
    ggplotly(plot, width = 800, height = 600) # Ajustando largura e altura
  })
  
  output$boxplot_jitter <- renderPlotly({
    plot <- ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
      geom_boxplot(aes(fill = factor(cyl)), alpha = 0.5) + # Boxplot com preenchimento
      geom_jitter(width = 0.2, 
                  size = 3, 
                  alpha = 0.6) # Adiciona o efeito jitter
    
    if (input$boxplot_jitter_type == "Boxplot com Jitter Simples") {
      plot <- plot + geom_jitter(width = 0.2, 
                                 size = 3, 
                                 alpha = 0.6)
      
    } else if (input$boxplot_jitter_type == "Boxplot com Jitter Colorido") {
      plot <- plot + geom_jitter(aes(color = factor(cyl)), 
                                 width = 0.3, 
                                 size = 3, 
                                 alpha = 0.6)
    }
    
    plot <- plot + labs(x = "Cylinders", 
                        y = "Miles per Gallon", 
                        title = "Boxplot with Jitter") +
      theme_gray()
    
    ggplotly(plot, width = 800, height = 600) # Ajustando largura e altura
  })
}

# Executar o aplicativo Shiny
shinyApp(ui = ui, server = server)
