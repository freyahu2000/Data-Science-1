#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(mosaic)
library(ggplot2)


# Define UI for application that draws a histogram
fluidPage(
  
  titlePanel("Group 7 Final Project"),
  helpText("Group 7: Hexiuli Huang, Ziyan Hu"),
  tabsetPanel(
    
    tabPanel("Variables",
             sidebarPanel(
               selectInput("var", 
                           label = "Select Variable", 
                           choices = c("Median of Income",
                                       "Total Living Area",
                                       "Percentage of Nativity",
                                       "Percentage of Minors Population",
                                       "Percentage of Female Population",
                                       "Percentage of Married Population",
                                       "Percentage of Poverty(over 150%) Population",
                                       "Percentage of Population with Bachelor Degree",
                                       "Percentage of Population with Disability",
                                       "Percentage of Population use Foodstamp"),
                           selected = "Median of Income"),
               h3(textOutput("data_selected")),
               textOutput("data_description")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Interactive Map",
                          plotlyOutput("interactive_map")),
                 tabPanel("Data Summary",
                          tableOutput("summary_table"),
                          plotlyOutput("histogram_plot"))
               )
             )
    ),
    
    tabPanel("Unsupervised Learning",
             sidebarPanel(
               selectInput("var2", 
                           label = "Select Variable", 
                           choices = c("Median of Income",
                                       "Total Living Area",
                                       "Percentage of Nativity",
                                       "Percentage of Minors Population",
                                       "Percentage of Female Population",
                                       "Percentage of Married Population",
                                       "Percentage of Poverty(over 150%) Population",
                                       "Percentage of Population with Bachelor Degree",
                                       "Percentage of Population with Disability",
                                       "Percentage of Population use Foodstamp"),
                           selected = "Median of Income"),
               sliderInput("k",
                           label = "Select Number of Clusters:",
                           min = 2, max = 15,
                           value = 3)),
             mainPanel(
               tabsetPanel(
                 tabPanel("Total Within-cluster Sum of Squares VS Numbers of Clusters",
                          plotOutput("cluster_plot"),
                          h5(textOutput("cluster_suggestion"))),
                 tabPanel("Cluster Map",
                          plotlyOutput("cluster_map"),
                          tableOutput("cluster_analysis"),
                          textOutput("map_conclusion"))
               )
             )
    )
  ))
