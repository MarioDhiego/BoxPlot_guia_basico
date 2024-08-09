# Carregar os pacotes necessários
library(shiny)
library(ggplot2)
library(plotly)
library(shinydashboard)

mtcars


# Interface do usuário (UI)
ui <- dashboardPage(
  dashboardHeader(title = "Gráficos Interativos"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Boxplot Clássico", tabName = "boxplots", icon = icon("box")),
      menuItem("ViolinPlot Clássico", tabName = "violinplots", icon = icon("violin")),
      menuItem("Boxplot com Jitter", tabName = "boxplot_jitter", icon = icon("dot-circle"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "boxplots",
              selectInput(
                inputId = "boxplot_type",
                label = "Selecione o tipo de Boxplot:",
                choices = c("Boxplot Simples", "Boxplot Colorido", "Boxplot Horizontal"),
                selected = "Boxplot Simples"
              ),
              plotlyOutput("boxplot")
      ),
      
      tabItem(tabName = "violinplots",
              selectInput(
                inputId = "violinplot_type",
                label = "Selecione o tipo de Violinplot:",
                choices = c("Violinplot Simples", "Violinplot Colorido"),
                selected = "Violinplot Simples"
              ),
              hr(),
              h5("Exemplo de Dados: 'mtcars'"),
              h6("mpg: Milhas por galão (milhagem)"),
              h6("cyl: Número de cilindros"),
              plotlyOutput("violinplot")
      ),
      
      tabItem(tabName = "boxplot_jitter",
              selectInput(
                inputId = "boxplot_jitter_type",
                label = "Selecione o tipo de Boxplot com Jitter:",
                choices = c("Boxplot com Jitter Simples", "Boxplot com Jitter Colorido"),
                selected = "Boxplot com Jitter Simples"
              ),
              plotlyOutput("boxplot_jitter")
      )
    )
  )
)

# Servidor (Server)
server <- function(input, output, session) {
  output$boxplot <- renderPlotly({
    plot <- ggplot(mtcars, aes(x = factor(cyl), y = mpg))
    
    if (input$boxplot_type == "Boxplot Simples") {
      plot <- plot + geom_boxplot()
      
    } else if (input$boxplot_type == "Boxplot Colorido") {
      plot <- plot + geom_boxplot(aes(fill = factor(cyl)))
      
    } else if (input$boxplot_type == "Boxplot Horizontal") {
      plot <- plot + geom_boxplot() + coord_flip()
    }
    
    plot <- plot + labs(x = "Número de cilindros", y = "Milhas por galão (mpg)", title = "Boxplot de Exemplo") +
      theme_minimal()
    
    ggplotly(plot) # Converte o ggplot em um gráfico interativo com plotly
  })
  
  output$violinplot <- renderPlotly({
    plot <- ggplot(mtcars, aes(x = factor(cyl), y = mpg))
    
    if (input$violinplot_type == "Violinplot Simples") {
      plot <- plot + geom_violin()
      
    } else if (input$violinplot_type == "Violinplot Colorido") {
      plot <- plot + geom_violin(aes(fill = factor(cyl)), alpha = 0.6)
    }
    
    plot <- plot + labs(x = "Número de cilindros", y = "Milhas por galão (mpg)", title = "Violinplot de Exemplo") +
      theme_minimal()
    
    ggplotly(plot) # Converte o ggplot em um gráfico interativo com plotly
  })
  
  output$boxplot_jitter <- renderPlotly({
    plot <- ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
      geom_boxplot(aes(fill = factor(cyl)), alpha = 0.5) + # Boxplot com preenchimento
      geom_jitter(width = 0.2, size = 2, alpha = 0.6) # Adiciona o efeito jitter
    
    if (input$boxplot_jitter_type == "Boxplot com Jitter Simples") {
      plot <- plot + geom_jitter(width = 0.2, size = 2, alpha = 0.6)
      
    } else if (input$boxplot_jitter_type == "Boxplot com Jitter Colorido") {
      plot <- plot + geom_jitter(aes(color = factor(cyl)), width = 0.2, size = 2, alpha = 0.6)
    }
    
    plot <- plot + labs(x = "Número de cilindros", y = "Milhas por galão (mpg)", title = "Boxplot com Jitter") +
      theme_minimal()
    
    ggplotly(plot) # Converte o ggplot em um gráfico interativo com plotly
  })
}

# Executar o aplicativo Shiny
shinyApp(ui = ui, server = server)


