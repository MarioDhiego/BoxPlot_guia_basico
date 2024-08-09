
# Carregar os pacotes necessários
library(shiny)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(viridis)
library(DT)

# Nomes personalizados para as variáveis
custom_var_names <- c(
  mpg = "Miles per Gallon",
  cyl = "Cylinders",
  disp = "Displacement",
  hp = "Horsepower",
  drat = "Driveshaft Ratio",
  wt = "Weight",
  qsec = "Measure of Acceleration",
  vs = "Engine Shape",
  am = "Transmission",
  gear = "Gears",
  carb = "Carburetors"
)

# Interface do usuário (UI)
ui <- dashboardPage(
  dashboardHeader(title = "Gráficos Interativos"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Boxplots", tabName = "boxplots", icon = icon("box")),
      menuItem("Violinplots", tabName = "violinplots", icon = icon("box")),
      menuItem("Boxplot com Jitter", tabName = "boxplot_jitter", icon = icon("box"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "boxplots",
              fluidRow(
                column(3,
                       selectInput(
                         inputId = "boxplot_x",
                         label = "Eixo X:",
                         choices = custom_var_names,
                         selected = "cyl"
                       )
                ),
                column(3,
                       selectInput(
                         inputId = "boxplot_y",
                         label = "Eixo Y:",
                         choices = custom_var_names,
                         selected = "mpg"
                       )
                ),
                column(3,
                       selectInput(
                         inputId = "boxplot_factor",
                         label = "Fator:",
                         choices = custom_var_names,
                         selected = "cyl"
                       )
                ),
                column(3,
                       selectInput(
                         inputId = "boxplot_type",
                         label = "Tipos de Boxplot:",
                         choices = c("Boxplot Classico", "Boxplot Horizontal"),
                         selected = "Boxplot Classico"
                       )
                )
              ),
              plotlyOutput("boxplot", height = "600px")
      ),
      
      tabItem(tabName = "violinplots",
              fluidRow(
                column(3,
                       selectInput(
                         inputId = "violinplot_x",
                         label = "Eixo X:",
                         choices = custom_var_names,
                         selected = "cyl"
                       )
                ),
                column(3,
                       selectInput(
                         inputId = "violinplot_y",
                         label = "Eixo Y:",
                         choices = custom_var_names,
                         selected = "mpg"
                       )
                ),
                column(3,
                       selectInput(
                         inputId = "violinplot_factor",
                         label = "Fator:",
                         choices = custom_var_names,
                         selected = "cyl"
                       )
                ),
                column(3,
                       selectInput(
                         inputId = "violinplot_type",
                         label = "Tipo de Violinplot:",
                         choices = c("Violinplot Classico", 
                                     "Violinplot Horizontal", 
                                     "ViolinPlot com Boxplot"),
                         selected = "Violinplot Classico"
                       )
                )
              ),
              plotlyOutput("violinplot", height = "600px")
      ),
      
      tabItem(tabName = "boxplot_jitter",
              fluidRow(
                column(3,
                       selectInput(
                         inputId = "boxplot_jitter_x",
                         label = "Eixo X:",
                         choices = custom_var_names,
                         selected = "cyl"
                       )
                ),
                column(3,
                       selectInput(
                         inputId = "boxplot_jitter_y",
                         label = "Eixo Y:",
                         choices = custom_var_names,
                         selected = "mpg"
                       )
                ),
                column(3,
                       selectInput(
                         inputId = "boxplot_jitter_factor",
                         label = "Fator",
                         choices = custom_var_names,
                         selected = "cyl"
                       )
                ),
                column(3,
                       selectInput(
                         inputId = "boxplot_jitter_type",
                         label = "Tipo de Boxplot com Jitter:",
                         choices = c("Boxplot com Jitter Classico", 
                                     "Boxplot com Jitter Colorido"),
                         selected = "Boxplot com Jitter Classico"
                       )
                )
              ),
              plotlyOutput("boxplot_jitter", height = "600px")
      )
    )
  )
)

# Servidor (Server)
server <- function(input, output, session) {
  output$boxplot <- renderPlotly({
    plot <- ggplot(mtcars, aes_string(x = names(custom_var_names)[custom_var_names == input$boxplot_x], 
                                      y = names(custom_var_names)[custom_var_names == input$boxplot_y]))
    
    if (input$boxplot_type == "Boxplot Classico") {
      plot <- plot + geom_boxplot(aes(fill = factor(get(names(custom_var_names)[custom_var_names == input$boxplot_factor]), 
                                                    labels = "Fatores")))+
        scale_fill_viridis_d(name = "Fatores")
      
    } else if (input$boxplot_type == "Boxplot Horizontal") {
      plot <- plot + geom_boxplot(aes(fill = factor(get(names(custom_var_names)[custom_var_names == input$boxplot_factor]), 
                                                    labels = "Fatores"))) +
        scale_fill_viridis_d(name = "Fatores") + coord_flip()
    }
    
    plot <- plot + labs(x = input$boxplot_x, y = input$boxplot_y, title = "Gráfico em Boxplot") +
      theme_minimal()
    
    ggplotly(plot) # Converte o ggplot em um gráfico interativo com plotly
  })
  
  output$violinplot <- renderPlotly({
    plot <- ggplot(mtcars, aes_string(x = names(custom_var_names)[custom_var_names == input$violinplot_x], 
                                      y = names(custom_var_names)[custom_var_names == input$violinplot_y]))
    
    if (input$violinplot_type == "Violinplot Classico") {
      plot <- plot + geom_violin(aes(fill = factor(get(names(custom_var_names)[custom_var_names == input$violinplot_factor]), 
                                                   labels = "Fatores")), alpha = 0.6) +
        scale_fill_viridis_d(name = "Fatores")
      
    } else if (input$violinplot_type == "Violinplot Horizontal") {
      plot <- plot + geom_violin(aes(fill = factor(get(names(custom_var_names)[custom_var_names == input$violinplot_factor]), 
                                                   labels = "Fatores")), alpha = 0.6) +
        scale_fill_viridis_d(name = "Fatores") + coord_flip()
      
    } else if (input$violinplot_type == "ViolinPlot com Boxplot") {
      plot <- plot + geom_violin(aes(fill = factor(get(names(custom_var_names)[custom_var_names == input$violinplot_factor]), 
                                                   labels = "Fatores")), alpha = 0.6) +
        geom_boxplot(width = 0.2, color = "black", outlier.shape = NA, alpha = 0.6) +
        scale_fill_viridis_d(name = "Fatores")
    }
    
    plot <- plot + labs(x = input$violinplot_x, y = input$violinplot_y, title = "Gráfico em ViolinPlot") +
      theme_minimal()
    
    ggplotly(plot) # Converte o ggplot em um gráfico interativo com plotly
  })
  
  output$boxplot_jitter <- renderPlotly({
    plot <- ggplot(mtcars, aes_string(x = names(custom_var_names)[custom_var_names == input$boxplot_jitter_x], 
                                      y = names(custom_var_names)[custom_var_names == input$boxplot_jitter_y])) +
      geom_boxplot(aes(fill = factor(get(names(custom_var_names)[custom_var_names == input$boxplot_jitter_factor]), 
                                     labels = "Fatores")), alpha = 0.5) +
      scale_fill_viridis_d(name = "Fatores") +
      geom_jitter(width = 0.2, size = 2, alpha = 0.6)
    
    if (input$boxplot_jitter_type == "Boxplot com Jitter Classico") {
      plot <- plot + geom_jitter(aes(color = factor(get(names(custom_var_names)[custom_var_names == input$boxplot_jitter_factor]), 
                                                    labels = "Fatores")), 
                                 width = 0.2, 
                                 size = 2, 
                                 alpha = 0.6) +
        scale_color_viridis_d(name = "Fatores")
    }
    
    plot <- plot + labs(x = input$boxplot_jitter_x, 
                        y = input$boxplot_jitter_y, 
                        title = "Boxplot com Jitter") +
      theme_minimal()
    
    ggplotly(plot) # Converte o ggplot em um gráfico interativo com plotly
  })
}

# Executar o aplicativo Shiny
shinyApp(ui, server)
