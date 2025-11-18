library(shiny)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(bslib) #motyw


# ----------------- Przygotowanie danych -----------------
dane_final <- dane_final %>%
  mutate(
    Health_pc = Wydatki_na_medycyne * 1e6 / Population
  )

# Mapa Europy
mapa <- ne_countries(continent = "Europe", returnclass = "sf")
dane_map <- merge(mapa, dane_final, by.x = "name", by.y = "Country")

# Słownik etykiet do ładnych nazw zmiennych
var_labels <- c(
  Smokers        = "Odsetek palących (%)",
  GDP_per_capita = "PKB per capita (USD)",
  Akcyza         = "Udział akcyzy w PKB (%)",
  Health_pc      = "Wydatki med. per capita [Eur]"
)

num_vars <- c("Smokers", "GDP_per_capita", "Akcyza", "Health_pc")

# ----------------- UI -----------------
ui <- fluidPage(
  theme = bs_theme(
    version = 5,        
    bootswatch = "cerulean"  
  ),
  titlePanel("Akcyza na wyroby tytoniowe, palenie i wydatki medyczne w Europie"),
  
  tabsetPanel(
    # --- Zakładka 1: Dane dla kraju ---
    tabPanel("Profil kraju",
             sidebarLayout(
               sidebarPanel(
                 selectInput("country", "Wybierz kraj:",
                             choices = sort(dane_final$Country))
               ),
               mainPanel(
                 h3(textOutput("country_title")),
                 tableOutput("country_table")
               )
             )
    ),
    
    # --- Zakładka 2: Wykres X vs Y ---
    tabPanel("Korelacje",
             sidebarLayout(
               sidebarPanel(
                 selectInput("var_x", "Zmienna X:", choices = num_vars,
                             selected = "Akcyza"),
                 selectInput("var_y", "Zmienna Y:", choices = num_vars,
                             selected = "Smokers")
               ),
               mainPanel(
                 plotOutput("plot_xy"),
                 verbatimTextOutput("cor_xy")
               )
             )
    ),
    
    # --- Zakładka 3: Regresja ---
    tabPanel("Model regresji",
             sidebarLayout(
               sidebarPanel(
                 checkboxInput("use_gdp",   "Uwzględnij PKB per capita", TRUE),
                 checkboxInput("use_akcyza","Uwzględnij akcyzę", TRUE)
               ),
               mainPanel(
                 h4("Model: Odsetek palących ~ (wybrane zmienne)"),
                 verbatimTextOutput("model_summary")
               )
             )
    ),
    
    # --- Zakładka 4: Mapa ---
    tabPanel("Mapa Europy",
             sidebarLayout(
               sidebarPanel(
                 selectInput("map_var", "Zmienna na mapie:",
                             choices = num_vars,
                             selected = "Smokers")
               ),
               mainPanel(
                 plotOutput("map_plot")
               )
             )
    )
  )
)

# ----------------- SERVER -----------------
server <- function(input, output, session) {
  
  # --- Zakładka 1: Profil kraju ---
  output$country_title <- renderText({
    paste("Profil dla kraju:", input$country)
  })
  
  output$country_table <- renderTable({
    dane_final %>%
      filter(Country == input$country) %>%
      transmute(
        `Odsetek palących (%)`    = Smokers,
        `PKB per capita (USD)`    = GDP_per_capita,
        `Akcyza (% PKB)`          = Akcyza,
        `Wydatki med. per capita` = round(Health_pc, 2)
      )
  })
  
  # --- Zakładka 2: Korelacje ---
  output$plot_xy <- renderPlot({
    x_var <- input$var_x
    y_var <- input$var_y
    
    ggplot(dane_final, aes_string(x = x_var, y = y_var)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(
        x = var_labels[x_var],
        y = var_labels[y_var],
        title = paste(var_labels[y_var], "vs", var_labels[x_var])
      ) +
      theme_minimal()
  })
  
  output$cor_xy <- renderPrint({
    x_var <- input$var_x
    y_var <- input$var_y
    
    v1 <- dane_final[[x_var]]
    v2 <- dane_final[[y_var]]
    
    r <- cor(v1, v2, use = "complete.obs")
    cat("Korelacja Pearsona r =", round(r, 3))
  })
  
  # --- Zakładka 3: Model regresji ---
  output$model_summary <- renderPrint({
    
    predictors <- c()
    if (input$use_gdp)    predictors <- c(predictors, "GDP_per_capita")
    if (input$use_akcyza) predictors <- c(predictors, "Akcyza")
    
    if (length(predictors) == 0) {
      cat("Zaznacz co najmniej jedną zmienną objaśniającą.")
      return()
    }
    
    # Budowa formuły
    form <- as.formula(
      paste("Smokers ~", paste(predictors, collapse = " + "))
    )
    
    model <- lm(form, data = dane_final)
    s <- summary(model)
    
    # Własna prezentacja wyników po polsku
    cat("MODEL REGRESJI LINIOWEJ\n")
    cat("---------------------------------------\n")
    cat("Zmienna objaśniana: Odsetek palących (%)\n")
    cat("Zmiennie objaśniające:", paste(predictors, collapse = ", "), "\n\n")
    
    cat("SZACOWANE PARAMETRY:\n")
    
    coefs <- s$coefficients
    rownames(coefs) <- c(
      "Wyraz wolny",
      if ("GDP_per_capita" %in% predictors) "PKB per capita",
      if ("Akcyza" %in% predictors) "Akcyza"
    )
    
    op <- options(scipen = 999)  # wyłączamy notację naukową
    print(round(coefs, 4))
    options(op)                   # przywracamy
    
    cat("\nSTATYSTYKI MODELU:\n")
    cat("R^2:", round(s$r.squared, 4), "\n")
    cat("R^2 skorygowane:", round(s$adj.r.squared, 4), "\n")
    cat("Odchylenie standardowe reszt:", round(s$sigma, 4), "\n")
    cat("Liczba obserwacji:", s$df[1] + s$df[2], "\n")
  })
  
  # --- Zakładka 4: Mapa ---
  output$map_plot <- renderPlot({
    var <- input$map_var
    
    ggplot(dane_map) +
      geom_sf(aes_string(fill = var)) +
      scale_fill_viridis_c() +
      labs(
        title = paste("Mapa Europy:", var_labels[var]),
        fill  = var_labels[var]
      ) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
