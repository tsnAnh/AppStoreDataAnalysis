library(ggplot2)
library(reshape2)
library(GGally)
library(plyr)
library(tidyverse)
library(gridExtra)
library(memisc)
library(dplyr)
library(corrplot)
library(shinydashboard)
library(shiny)

df <- read.csv('./AppleStore.csv')

# Adding a new variable to include size by MB.
df$size_mb <- df$size_bytes / 1024 / 1024
# Seperate game vs general app category
df$is_game <- df$prime_genre == 'Games'
df$is_game <- ifelse(df$is_game == TRUE, "Game", "General App")
# Showing summary about the price, also adding a new variable to seperate paid
# and free apps.
summary(df$price == 0)
df$is_free <- df$price == 0
df$is_free <- ifelse(df$is_free == TRUE, "Free", "Paid")


dfGames <- subset(df, df$is_game == 'Game')
dfGeneral <- subset(df, df$is_game != 'Game')

# Showing top 10 Games based on user rating and total number of ratings
print("Top 10 Games based on user rating and total number of ratings")
head(dfGames[order(dfGames$user_rating, dfGames$rating_count_tot, decreasing = T),
             c("track_name", "rating_count_tot")], n = 10, data = dfGames)

# Showing top 10 General apps based on user rating and total number of ratings
print("Top 10 General apps based on user rating and total number of ratings")
head(dfGeneral[order(dfGeneral$user_rating, dfGeneral$rating_count_tot,
                     decreasing = T), c("track_name", "rating_count_tot",
                                        "prime_genre")], n = 10, data = dfGeneral)
# This results are sorted based on user rating first and total ratings count.

# Showing top 10 apps based on total count of user rating
print("Top 10 apps based on total count of user rating")
head(df[order(df$rating_count_tot, decreasing = T),
        c("track_name", "rating_count_tot")], n = 10, data = df)


ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(
    title = "App Store Data Dashboard"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data Overview", tabName = "overview", icon = icon('grip-horizontal')),
      menuItem('Analytics', tabName = "analytics", icon = icon('chart-line'))
    )
  ),
  dashboardBody(
    tabItems(
      # Dashboard tab content
      tabItem(
        tabName = "dashboard",
        fluidRow(
          box(
            width = 6,
            column(
              width = 12,
              sliderInput('frequency_category_slider_input', label = 'Number of categories: ', min = 1, max = length(unique(df$prime_genre)), value = 5, step = 1),
              selectInput('frequency_category_select_input', "Sorted by:", c('Ascending' = 'asc', 'Descending' = "desc"), selected = 'asc'),
              plotOutput("frequency_categories_plot"),
            )
          ),
          box(
            width = 6,
            column(
              width = 12,
              selectInput('frequency_app_content_user_rating_plot', "Based on column:", c('Content Rating' = 'cont', 'User Rating' = 'user'), selected = 'cont'),
              plotOutput("frequency_app_content_user_rating_plot", height = 500),
            ),
            height = 600,
          ),
        ),
        fluidRow(
          box(
            width = 7,
            column(
              width = 12,
              selectInput(
                "price_rating_relation_input",
                "App Category:",
                c("General App" = "General App", "Games" = "Game"),
                selected = 'general'
              ),
              plotOutput('price_rating_relation_plot'),
            ),
          ),
          box(
            width = 5,
            column(
              width = 12,
              column(12, offset = 3, h4("Correlation betwwen each variables in dataset.")),
              plotOutput('correlation_plot'),
            ),
            height = 500,
          ),
        ),
        fluidRow(
          box(
            width = 4,
            height = 700,
            plotOutput('price_based_on_categories_plot', height = 675)
          ),
          box(
            width = 8,
            height = 700,
            plotOutput('user_rating_based_on_categories', height = 675)
          )
        ),
        fluidRow(
          box(
            plotOutput('rating_lang_count_relation_plot', height = 575),
            height = 600
          ),
          box(
            plotOutput('current_ver_rating_vs_average_rating', height = 575),
            height = 600
          )
        ),
        fluidRow(
          box(
            width = 5,
            column(
              width = 12,
              selectInput(
                'rating_lang_count_select_input',
                'User rating depends on column:',
                choices = c('Number of languages supported' = 'nol', 'Rating count' = 'rc', 'Price' = 'price'),
                selected = 'nol'
              ),
              plotOutput('user_rating_depends_on_app_category_plot', height = 500)
            ),
          ),
          box(
            width = 7,
            plotOutput('rating_size_plot', height = 575),
            height = 600
          )
        ),
        fluidRow(
          box(
            width = 7,
            plotOutput('categories_price_user_rating_plot')
          ),
          box(
            width = 5,
            plotOutput('app_price_based_on_size_plot')
          )
        ),
      ),
      tabItem(
        tabName = "overview",
        plotOutput('overview_plot', height = 900)
      ),
      tabItem(
        tabName = 'analytics',
        fluidRow(
          infoBoxOutput('total_apps_infobox'),
          infoBoxOutput('general_apps_infobox'),
          infoBoxOutput('games_infobox'),
        ),
        fluidRow(
          box(width = 12, dataTableOutput('table_apps'))
        )
      )
    )
  )
)

server <- function(input, output) {
  output$total_apps_infobox <- renderInfoBox({
    infoBox(title = "Total apps", value = nrow(df), color = 'green')
  })


  output$general_apps_infobox <- renderInfoBox({
    infoBox(title = "General apps", value = nrow(filter(df, is_game != 'Game')), icon('shapes'))
  })


  output$games_infobox <- renderInfoBox({
    infoBox(title = "Games", value = nrow(filter(df, is_game == 'Game')), color = 'red', icon = icon('puzzle-piece'))
  })


  f_categories_data <- reactive({
    df %>%
      group_by(prime_genre) %>%
      summarise(count = n_distinct(id)) %>%
      arrange(count) %>%
      map_df(rev) %>%
      top_n(input$frequency_category_slider_input)
  })


  output$frequency_categories_plot <- renderPlot({
    # Showing frequency of apps in our data set sorted by their count.
    if (input$frequency_category_select_input == 'asc') {
      ggplot(aes(x = reorder(prime_genre, count), y = count), data = f_categories_data()) +
        geom_col(fill = 'purple') +
        labs(title = "Frequency of Apps based on the category",
             x = "Categories", y = "Count") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    } else {
      ggplot(aes(x = reorder(prime_genre, -count), y = count), data = f_categories_data()) +
        geom_col(fill = 'purple') +
        labs(title = "Frequency of Apps based on the category",
             x = "Categories", y = "Count") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
  })


  output$frequency_app_content_user_rating_plot <- renderPlot({
    if (input$frequency_app_content_user_rating_plot == 'cont') {
      qplot(x = cont_rating, data = df) +
        labs(title = "Frequency of Apps based on the content rating",
             x = "Content Type", y = "Count")
    } else {
      qplot(x = user_rating, data = subset(df, df$user_rating > 0), binwidth = 1, ) +
        labs(title = "Frequncy of Apps based on the user rating above 0",
             x = "User Rating", y = "Count")
    }
  })


  output$overview_plot <- renderPlot({
    ggpairs(df[, c('price', 'rating_count_tot',
                   'rating_count_ver', 'user_rating', 'user_rating_ver',
                   'size_mb', 'is_game', 'is_free',
                   'lang.num', 'sup_devices.num')]) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })


  output$correlation_plot <- renderPlot({
    M <- cor(select_if(df[, 5:20], is.numeric))
    corrplot(M, method = "circle")
  })


  output$price_rating_relation_plot <- renderPlot({
    ggplot(aes(x = user_rating, y = price), data = df %>%
      filter(is_game == input$price_rating_relation_input) %>%
      subset(price < 50)) +
      geom_point(fill = 'purple') +
      xlim(c(1, 5)) +
      labs(x = "Average User Rating", y = "Price",
           title = "Not strong relation between price of an app and it's mean user rating.")
  })


  output$rating_lang_count_relation_plot <- renderPlot({
    ggplot(aes(x = user_rating, y = lang.num), data = df) +
      geom_line(stat = 'summary', fun.y = mean) +
      labs(x = "Average User Rating", y = "Number of language supported")
  })


  output$user_rating_depends_on_app_category_plot <- renderPlot({
    if (input$rating_lang_count_select_input == 'nol') {
      ggplot(aes(x = prime_genre, y = user_rating, fill = prime_genre), data = df) +
        geom_histogram(stat = 'summary', fun.y = mean) +
        coord_flip() +
        labs(x = "Categories", y = "Average of user rating",
             title = "Does mean user ratings  depend on app category?") +
        facet_wrap(~is_free) +
        theme_minimal()
    } else if (input$rating_lang_count_select_input == 'rc') {
      ggplot(aes(x = prime_genre, y = rating_count_tot, fill = prime_genre), data = df) +
        geom_bar(stat = 'summary', fun.y = mean) +
        coord_flip() +
        labs(x = "Categories", y = "Average of total number of user rating",
             title = "Does mean of total number of user ratings depend on app category?") +
        facet_wrap(~is_free) +
        scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
        theme_minimal()
    } else {
      ggplot(aes(x = prime_genre, y = price, fill = prime_genre), data = df) +
        geom_histogram(stat = 'summary', fun.y = mean) +
        coord_flip() +
        labs(x = "Categories", y = "Average Price",
             title = "Which category is more expensive than other apps?") +
        theme_minimal()
    }
  })


  table_data <- reactive({
    df %>%
      ungroup() %>%
      dplyr::select(-one_of(c('X', 'currency', 'rating_count_total', 'rating_count_version', 'sup_devices.num', 'ipadSc_urls.num', 'lang.num', 'vpp_lic', 'size_bytes')))
  })


  output$table_apps <- renderDataTable({
    table_data()
  })


  output$current_ver_rating_vs_average_rating <- renderPlot({
    ggplot(aes(x = user_rating_ver, y = user_rating), data = df) +
      geom_line(stat = 'summary', fun.y = median) +
      geom_smooth() +
      labs(x = "User rating of recent version", y = "Total average user rating",
           title = "Does the current version is always have more rating than
        the total overall rating?")
  })


  output$rating_size_plot <- renderPlot({
    ggplot(aes(x = user_rating, y = size_mb), data = subset(df, df$user_rating > 0)) +
      geom_histogram(stat = 'summary', fun.y = mean) +
      scale_x_continuous(breaks = seq(1.5, 4.5, .5), limits = c(1, 5))
  })


  output$categories_price_user_rating_plot <- renderPlot({
    ggplot(aes(x = prime_genre, y = price), data = df) +
      geom_line(aes(color = factor(user_rating))) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(y = "Price", x = "Categories")
  })


  output$app_price_based_on_size_plot <- renderPlot({
    ggplot(aes(x = size_mb, y = price), data = subset(df, df$user_rating > 0)) +
      geom_point(aes(color = factor(user_rating))) +
      coord_flip() +
      ylim(limits = c(0, 10)) +
      labs(x = "Size (MB)", y = "Price", title = "App price based on size")
  })


  output$price_based_on_categories_plot <- renderPlot({
    ggplot(aes(x = prime_genre, y = price), data = df) +
      geom_histogram(stat = 'summary', fun.y = mean) +
      coord_flip() +
      labs(x = "Category", y = "Price", title = "Prices based on categories")
  })


  output$user_rating_based_on_categories <- renderPlot({
    ggplot(aes(x = prime_genre, y = user_rating), data = df) +
      geom_histogram(stat = 'summary', fun.y = mean) +
      coord_flip() +
      labs(x = "Category", y = "User Rating", title = "User rating based on categories") +
      facet_wrap(~is_free)
  })
}

shinyApp(ui, server)


# Linear model
linear_model <- lm(user_rating ~ price + prime_genre + cont_rating + size_mb + lang.num, data = df)

mtable(linear_model, m2, m3, m4, m5, m6, sdigits = 3)

xx <- data.frame(prime_genre = "Games",
                 price = 25, cont_rating = "4+", size_mb = 200, lang.num = 20)
xx2 <- data.frame(prime_genre = "Education", price = 20, cont_rating = '12+', size_mb = 2, lang.num=5)
yy <- predict(linear_model, newdata = xx, interval = "prediction")
yy2 <- predict(linear_model, newdata = xx2, interval = 'prediction')
print(yy)
print(yy2)

ggplot(df,aes(user_rating, price)) +
  stat_summary(fun.data=mean_cl_normal) +
  geom_smooth(method='lm')
