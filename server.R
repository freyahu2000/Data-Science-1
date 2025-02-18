#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(plotly)
library(mosaic)
library(ggplot2)
library(purrr)

# dataset
data <- readRDS("Group7_ACS_witmap.RDS")

# Display selected variable
function(input, output) {
  output$data_selected <- renderText({
    variable <- input$var
    paste(variable, ":")
  })
  
  # Display data description 
  output$data_description <- renderText({
    text_Medincome <- "This variable represents the median household income in New Jersey for the year 2021. It is a crucial indicator of the economic status of households, showing the central income level. It derives from table B19013_001"
    text_TotallivingArea <-  "The variable represents the total geographic mobility area in New Jersey for the year 2021. It derives from the table B07401_001."
    text_nativity_percentage <- "Shows the percentage of the population in New Jersey that was foreign-born, capturing the diversity and proportion of the immigrant population. It derives from B15001_002(Nativity)/B15001_001(Total population)"
    text_Under18_percentage <- "This variable represents the percentage of New Jersey's population that is under 18 years of age. It is crucial for understanding the youth demographic. It derives from (B07401_002(Age1-4)+B07401_003(Age5-17))/B07401_001(Total population)"
    text_female_percentage <- "Indicates the proportion of the population in New Jersey identifying as female in 2021, reflecting the gender distribution. It derives from B08406_035(Female population)/B08406_001(Total population)"
    text_marrried_percentage <- "This variable indicates the percentage of the adult population in New Jersey that is married. It encompasses those in both opposite-sex and same-sex marriages and is a key indicator of the marital status distribution. It derives from B06008_003(Married population)/B06008_001(Total Population)"
    text_poverty_percentage <- "The percentage of individuals or families in New Jersey living below the poverty line. It derives from B08522_004(People in poverty)/B08522_001(Total population)"
    text_bachelor_percentage <- "The percentage of adults in New Jersey who have obtained a bachelor's degree. It derives from B15003_023(People who obtain a bachelor degree)/B15003_023(Total population)"
    text_disability_percentage <- "The percentage of the population in New Jersey with a reported disability. It derives from B18140_002(People with disability)/(B18140_002 + B18140_005)(Disability and Non-disability population)"
    text_foodstamp_percentage <- "The percentage of households in New Jersey receiving food stamps. It derives from B22008_002(Households which received a foodstamp)/B22008_001(Total population)"
    temp_text <- switch(input$var, 
                        "Median of Income" = text_Medincome,
                        "Total Living Area" = text_TotallivingArea,
                        "Percentage of Nativity" = text_nativity_percentage,
                        "Percentage of Minors Population" = text_Under18_percentage,
                        "Percentage of Female Population" = text_female_percentage,
                        "Percentage of Married Population" = text_marrried_percentage,
                        "Percentage of Poverty(over 150%) Population" = text_poverty_percentage,
                        "Percentage of Population with Bachelor Degree" = text_bachelor_percentage,
                        "Percentage of Population with Disability" = text_disability_percentage,
                        "Percentage of Population use Foodstamp" = text_foodstamp_percentage)
  })
  
  # histogram using plotly
  output$histogram_plot <- renderPlotly({
    
    temp_data <- data$Medincome
    variable <-"Median of Income"
    
    variable <- input$var
    temp_data <- switch(input$var, 
                        "Median of Income" = data$Medincome,
                        "Total Living Area" = data$TotallivingArea,
                        "Percentage of Nativity" = data$nativity_percentage,
                        "Percentage of Minors Population" = data$Under18_percentage,
                        "Percentage of Female Population" = data$female_percentage,
                        "Percentage of Married Population" = data$marrried_percentage,
                        "Percentage of Poverty(over 150%) Population" = data$poverty_percentage,
                        "Percentage of Population with Bachelor Degree" = data$bachelor_percentage,
                        "Percentage of Population with Disability" = data$disability_percentage,
                        "Percentage of Population use Foodstamp" = data$foodstamp_percentage)
    plot_ly(x = data$NAME, 
            y = temp_data,
            type = "bar") %>%
      layout(title = paste("Histogram of", variable),
             yaxis = list(title = variable))
  })
  
  # summary table
  output$summary_table <- renderTable({
    temp_data <- switch(input$var, 
                        "Median of Income" = data$Medincome,
                        "Total Living Area" = data$TotallivingArea,
                        "Percentage of Nativity" = data$nativity_percentage,
                        "Percentage of Minors Population" = data$Under18_percentage,
                        "Percentage of Female Population" = data$female_percentage,
                        "Percentage of Married Population" = data$marrried_percentage,
                        "Percentage of Poverty(over 150%) Population" = data$poverty_percentage,
                        "Percentage of Population with Bachelor Degree" = data$bachelor_percentage,
                        "Percentage of Population with Disability" = data$disability_percentage,
                        "Percentage of Population use Foodstamp" = data$foodstamp_percentage)
    round(favstats(temp_data), 3)
  })
  
  # interactive map
  output$interactive_map <- renderPlotly({
    fig1 <- plot_ly(data, 
                    split = ~paste("Median income in", NAME, "\n is", Medincome, "dollar."),
                    color = ~Medincome,
                    colors = 'Purples',
                    stroke = I("black"),
                    showlegend = FALSE)
    fig2 <- plot_ly(data, 
                    split = ~paste("Total living area in", NAME, "\n is", TotallivingArea, "square km."),
                    color = ~TotallivingArea,
                    colors = 'Purples',
                    stroke = I("black"),
                    showlegend = FALSE)
    fig3 <- plot_ly(data, 
                    split = ~paste("Nativity percentage in", NAME, "\n is", nativity_percentage, "%."),
                    color = ~nativity_percentage,
                    colors = 'Purples',
                    stroke = I("black"),
                    showlegend = FALSE)
    fig4 <- plot_ly(data, 
                    split = ~paste("Minors percentage in", NAME, "\n is", Under18_percentage, "%."),
                    color = ~Under18_percentage,
                    colors = 'Purples',
                    stroke = I("black"),
                    showlegend = FALSE)
    fig5 <- plot_ly(data, 
                    split = ~paste("Female percentage in", NAME, "\n is", female_percentage, "%."),
                    color = ~female_percentage,
                    colors = 'Purples',
                    stroke = I("black"),
                    showlegend = FALSE)
    fig6 <- plot_ly(data, 
                    split = ~paste("Marrried percentage in", NAME, "\n is", marrried_percentage, "%."),
                    color = ~marrried_percentage,
                    colors = 'Purples',
                    stroke = I("black"),
                    showlegend = FALSE)
    fig7 <- plot_ly(data, 
                    split = ~paste("Poverty percentage in", NAME, "\n is", poverty_percentage, "%."),
                    color = ~poverty_percentage,
                    colors = 'Purples',
                    stroke = I("black"),
                    showlegend = FALSE)
    fig8 <- plot_ly(data, 
                    split = ~paste("Bachelor percentage in", NAME, "\n is", bachelor_percentage, "%."),
                    color = ~bachelor_percentage,
                    colors = 'Purples',
                    stroke = I("black"),
                    showlegend = FALSE)
    fig9 <- plot_ly(data, 
                    split = ~paste("Disability percentage in", NAME, "\n is", disability_percentage, "%."),
                    color = ~disability_percentage,
                    colors = 'Purples',
                    stroke = I("black"),
                    showlegend = FALSE)
    fig10 <- plot_ly(data, 
                     split = ~paste("Foodstamp percentage in", NAME, "\n is", foodstamp_percentage, "%."),
                     color = ~foodstamp_percentage,
                     colors = 'Purples',
                     stroke = I("black"),
                     showlegend = FALSE)
    
    temp_fig <- switch(input$var, 
                       "Median of Income" = fig1,
                       "Total Living Area" = fig2,
                       "Percentage of Nativity" = fig3,
                       "Percentage of Minors Population" = fig4,
                       "Percentage of Female Population" = fig5,
                       "Percentage of Married Population" = fig6,
                       "Percentage of Poverty(over 150%) Population" = fig7,
                       "Percentage of Population with Bachelor Degree" = fig8,
                       "Percentage of Population with Disability" = fig9,
                       "Percentage of Population use Foodstamp" = fig10)
    temp_fig
  })
  
  # Decide number of cluster
  output$cluster_plot <- renderPlot({
    temp_data <- switch(input$var2, 
                        "Median of Income" = data$Medincome,
                        "Total Living Area" = data$TotallivingArea,
                        "Percentage of Nativity" = data$nativity_percentage,
                        "Percentage of Minors Population" = data$Under18_percentage,
                        "Percentage of Female Population" = data$female_percentage,
                        "Percentage of Married Population" = data$marrried_percentage,
                        "Percentage of Poverty(over 150%) Population" = data$poverty_percentage,
                        "Percentage of Population with Bachelor Degree" = data$bachelor_percentage,
                        "Percentage of Population with Disability" = data$disability_percentage,
                        "Percentage of Population use Foodstamp" = data$foodstamp_percentage)
    temp_data <- scale(temp_data)
    ## Function to calculate within-cluster sum of squares for k-means clustering
    set.seed(2234)
    wss <- function(k, data) {
      data <- temp_data
      kmeans(data, k, nstart = 50)$tot.withinss
    }
    ## define k range
    k_values <- 1:15
    ## Actually calculate within-cluster sum of squares
    wss_values <- numeric(length(k_values))
    for (i in k_values) {
      wss_values[i] <- wss(k_values[i])
    }
    ## Store results
    wss_values <- tibble(wss = wss_values, k = k_values)
    ## Plot
    ggplot(wss_values, aes(x = k, y = wss)) +
      geom_point() +
      geom_line()
  })
  
  # Display cluster suggestion
  output$cluster_suggestion <- renderText({
    text_Medincome2 <- "Given that the plot shows that the total within-cluster sum of squares does not change significantly after 3 clusters, it is recommended to set k=3."
    text_TotallivingArea2 <- "Given that the plot shows that the total within-cluster sum of squares does not change significantly after 3 clusters, it is recommended to set k=3."
    text_nativity_percentage2 <- "Given that the plot shows that the total within-cluster sum of squares does not change significantly after 3 clusters, it is recommended to set k=3."
    text_Under18_percentage2 <- "Given that the plot shows that the total within-cluster sum of squares does not change significantly after 3 clusters, it is recommended to set k=3."
    text_female_percentage2 <- "Given that the plot shows that the total within-cluster sum of squares does not change significantly after 2 clusters, it is recommended to set k=2 or k=3."
    text_marrried_percentage2 <- "Given that the plot shows that the total within-cluster sum of squares does not change significantly after 2 clusters, it is recommended to set k=2."
    text_poverty_percentage2 <- "Given that the plot shows that the total within-cluster sum of squares does not change significantly after 3 clusters, it is recommended to set k=3."
    text_bachelor_percentage2 <- "Given that the plot shows that the total within-cluster sum of squares does not change significantly after 3 clusters, it is recommended to set k=3."
    text_disability_percentage2 <- "Given that the plot shows that the total within-cluster sum of squares does not change significantly after 3 clusters, it is recommended to set k=3."
    text_foodstamp_percentage2 <- "Given that the plot shows that the total within-cluster sum of squares does not change significantly after 3 clusters, it is recommended to set k=3."
    temp_text <- switch(input$var2, 
                        "Median of Income" = text_Medincome2,
                        "Total Living Area" = text_TotallivingArea2,
                        "Percentage of Nativity" = text_nativity_percentage2,
                        "Percentage of Minors Population" = text_Under18_percentage2,
                        "Percentage of Female Population" = text_female_percentage2,
                        "Percentage of Married Population" = text_marrried_percentage2,
                        "Percentage of Poverty(over 150%) Population" = text_poverty_percentage2,
                        "Percentage of Population with Bachelor Degree" = text_bachelor_percentage2,
                        "Percentage of Population with Disability" = text_disability_percentage2,
                        "Percentage of Population use Foodstamp" = text_foodstamp_percentage2)
    paste("Cluster Suggestion:", temp_text)
  })
  
  # cluster analysis
  output$cluster_analysis <- renderTable({
    n_k <- 3
    temp_data <- data$Medincome
    
    n_k <- input$k
    temp_data <- switch(input$var2, 
                        "Median of Income" = data$Medincome,
                        "Total Living Area" = data$TotallivingArea,
                        "Percentage of Nativity" = data$nativity_percentage,
                        "Percentage of Minors Population" = data$Under18_percentage,
                        "Percentage of Female Population" = data$female_percentage,
                        "Percentage of Married Population" = data$marrried_percentage,
                        "Percentage of Poverty(over 150%) Population" = data$poverty_percentage,
                        "Percentage of Population with Bachelor Degree" = data$bachelor_percentage,
                        "Percentage of Population with Disability" = data$disability_percentage,
                        "Percentage of Population use Foodstamp" = data$foodstamp_percentage)
    temp_data <- scale(temp_data)
    set.seed(123)
    k_means_results <- kmeans(temp_data, centers = n_k, nstart = 50)
    clusters <- k_means_results$cluster
    name <- gsub(pattern = " Co.*", replacement = "", x = data$NAME)
    temp_d <- cbind(name, clusters)
    temp_d <- data.frame(temp_d)
    colnames(temp_d) <- c("NAME", "clusters")
    
    County <- NULL
    for (i in 1:n_k){
      County[i] <- temp_d %>%
        filter(clusters == i) %>%
        select(NAME) %>%
        unlist() %>%
        as.character()  %>%
        paste(collapse = ', ')
    }
    County <- data.frame(County)
    
    cluster_table <- table(k_means_results$cluster)
    cluster_table <- data.frame(cluster_table)
    cluster_table <- cbind(cluster_table, County)
    colnames(cluster_table) <- c("Cluster", "Freq", "County")
    cluster_table
  })
  
  # interactive cluster map 
  output$cluster_map <- renderPlotly({
    k <- input$k
    temp_data <- switch(input$var2, 
                        "Median of Income" = data$Medincome,
                        "Total Living Area" = data$TotallivingArea,
                        "Percentage of Nativity" = data$nativity_percentage,
                        "Percentage of Minors Population" = data$Under18_percentage,
                        "Percentage of Female Population" = data$female_percentage,
                        "Percentage of Married Population" = data$marrried_percentage,
                        "Percentage of Poverty(over 150%) Population" = data$poverty_percentage,
                        "Percentage of Population with Bachelor Degree" = data$bachelor_percentage,
                        "Percentage of Population with Disability" = data$disability_percentage,
                        "Percentage of Population use Foodstamp" = data$foodstamp_percentage)
    temp_data <- scale(temp_data)
    k_means_results <- kmeans(temp_data, centers = k, nstart = 50)
    
    clusters <- k_means_results$cluster
    temp_data <- cbind(data, clusters)
    plot_ly(temp_data, 
            split = ~paste("Cluster group for", NAME, "\n is", clusters),
            color = ~clusters,
            colors = 'Purples',
            stroke = I("black"),
            showlegend = FALSE)
  })
  
  # display map conclusion
  output$map_conclusion <- renderText({
    text_Medincome2 <- "In our cluster analysis of median income of counties in New Jersey, Cluster 3, with the highest median income, suggests the areas with economic prosperity. Conversely, Cluster 1 with indicates regions with economic challenges, reflected by the lowest median incomes. Cluster 3, with moderate median income, represents areas with diverse economic conditions. This map of clusters highlights significant economic disparities across the state."
    text_TotallivingArea2 <- "In our cluster analysis of total mobility of areas in New Jersey, Cluster 3 has 9 counties, with the largest mobility of areas, suggests the areas with large geographical spread of populations within the state. Cluster 1 with the highest mobility of areas is primarily located in the upper half of the map. Cluster 2, characterized by least mobility of areas, is mainly distributed in the lower half of the map."
    text_nativity_percentage2 <- "In our cluster analysis of percentage of nativity in New Jersey, counties in Cluster 3 might have a broader range of nativity percentages. Cluster 3, with higher nativity percentages, is mainly distributed in the upper half of the map. Cluster 1 has the lowest percentage of nativity."
    text_Under18_percentage2 <- "In our cluster analysis of percentage of minors population in New Jersey, Cluster 3 has 12 counties, with the largest areas, suggesting the areas with moderate percentage of minors population. Cluster 1 has 5 counties, indicating the areas with lowest percentage of minors population, while cluster 2 has 4 counties, indicating the areas with highest percentage of minors population."
    text_female_percentage2 <- "In our cluster analysis of percentage of female population in New Jersey, Cluster 3 has 10 counties, mainly distributed in the upper half of the map, indicating the moderate percentage of female population. Cluster 1 has 6 counties, suggesting the highest percentage of female population, while cluster 2 has 5 counties, indicating the lowest percentage of female population."
    text_marrried_percentage2 <- "In our cluster analysis of percentage of married population in New Jersey, Cluster 2 has 6 counties, mainly distributed in the middle of the map, indicating the moderate percentage of married population. Cluster 1 is mainly distributed in the upper half of the map, indicating the highest percentage of married population in New Jersey, while cluster 3 mainly distributed in the lower half of the map, suggests the lowest percentage of married population."
    text_poverty_percentage2 <- "In our cluster analysis of percentage of poverty population, Cluster 1 has 4 counties, indicating the highest percentage of poverty and it is mainly located in the lower half of the map. Cluster 2 has 9 counties, indicating the moderate percentage of poverty population. Cluster 3 has 8 counties, mainly distributed in the upper half of the map, suggesting the lowest poverty percentage."
    text_bachelor_percentage2 <- "In our cluster analysis of percentage of population with bachelor’s degree, Cluster 3 has the highest percentage population of bachelor’s degree, which mainly distributes in the upper half of the map. Cluster 2 has the lowest percentage population of bachelor’s degree, which mainly distributes in the lower half of the map. And Cluster 1 has the moderate percentage of bachelor’s degree, which is distributed in the middle of the map."
    text_disability_percentage2 <- "In our cluster analysis of percentage of population with disability, Cluster 1 has 3 counties, which has the highest percentage of population with disability and distributes in the lower half of the map. Cluster 3 has 6 counties, which has the lowest percentage of population with disability and distributes in the middle of the map. Cluster 2 contains most counties and has a moderate percentage of population with disability."
    text_foodstamp_percentage2 <- "In our cluster analysis of percentage of population use food stamp, Cluster 1 has 6 counties which suggests the highest percentage of population use a food stamp. Cluster 3 has 8 counties and indicate the area with lowest percentage of population use a food stamp. Cluster 2 has 7 counties mainly distributed in the middle of the map, which indicates the moderate percentage of population uses a foodstamp."
    temp_text <- switch(input$var2, 
                        "Median of Income" = text_Medincome2,
                        "Total Living Area" = text_TotallivingArea2,
                        "Percentage of Nativity" = text_nativity_percentage2,
                        "Percentage of Minors Population" = text_Under18_percentage2,
                        "Percentage of Female Population" = text_female_percentage2,
                        "Percentage of Married Population" = text_marrried_percentage2,
                        "Percentage of Poverty(over 150%) Population" = text_poverty_percentage2,
                        "Percentage of Population with Bachelor Degree" = text_bachelor_percentage2,
                        "Percentage of Population with Disability" = text_disability_percentage2,
                        "Percentage of Population use Foodstamp" = text_foodstamp_percentage2)
    paste("Conclusion:", temp_text)
  })
}