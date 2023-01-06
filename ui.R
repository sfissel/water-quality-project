library(shiny)
library(ggplot2)
library(plotly)
library(reshape2)
library(gridExtra)
library(grid)
library(cowplot)
library(dplyr)
library(data.table)

water_quality <- read.csv("water_potability_copy.csv")

water_quality <- na.omit(water_quality)
water_quality <- na.omit(water_quality)
water_quality$Potability <- factor(water_quality$Potability)

np <- round(1998/3276 * 100)
p <- round(1278/3276 * 100)
# Subset test data
data <- data.frame(
  category=c("Non-Potable", "Potable"),
  count=c(np, p)
)

# Compute percentages
data$fraction = data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax = cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin = c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$category, ": ", data$count, "%")


fluidPage(
    titlePanel("METRIC DISTRIBUTIONS OF WATER POTABILITY"),
    br(),
    
    sidebarPanel(
      sliderInput("ph", "pH Level (pH):", min = 0, max = 14, value = c(0, 14), 
                  step = 1),
      h6("Recommended Level: 6.5-8.5 pH (neutral on pH scale)"),
      sliderInput("hardness", "Hardness Level (mg/L):", min = 50, max = 360, value = c(50, 360), 
                  step = 10),
      h6("Recommended Level: None. Hardness levels > 150 mg/L are considered hard water. If water is hard, it can reduce efficiency of water treatment."),
      sliderInput("solids", "Solids Level (mg/l):", min = 0, max = 50800, value = c(0, 50800), 
                  step = 1000),
      h6("Recommended Level: 500-1000 mg/l"),
      sliderInput("chloramines", "Chloramine Level (mg/L):", min = 0, max = 14, value = c(0, 14), 
                  step = 1),
      h6("Recommended Level: 4 mg/L or less"),
      sliderInput("sulfates", "Sulfate Level (mg/L):", min = 0, max = 500, value = c(0, 500), 
                  step = 50),
      h6("Recommended Level: 250 mg/L or less"),
      sliderInput("conductivity", "Conductivity Level (μS/cm):", min = 200, max = 800, value = c(200, 800), 
                  step = 100),
      h6("Recommended Level: 400 μS/cm or less"),
      sliderInput("organic_carbon", "Organic Carbon Level (mg/L):", min = 0, max = 30, value = c(0, 30), 
                  step = 1),
      h6("Recommended Level: 2 mg/L or less"),
      sliderInput("trihalomethanes", "Trihalomethanes (ppm):", min = 0, max = 125, value = c(0, 125), 
                  step = 10),
      h6("Recommended Level: 80 ppm or less"),
      sliderInput("turbidity", "Turbidity (NTU):", min = 0, max = 7, value = c(0, 7), 
                  step = 1),
      h6("Recommended Level: 5 NTU or less"),
      width = 4
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Overall", plotOutput("overall"), width = "100%"),
                  tabPanel("pH", plotOutput("ph")),
                  tabPanel("Hardness", plotOutput("hardness")),
                  tabPanel("Solids", plotOutput("solids")),
                  tabPanel("Chloramines", plotOutput("chloramines")),
                  tabPanel("Sulfates", plotOutput("sulfates")),
                  tabPanel("Conductivity", plotOutput("conductivity")),
                  tabPanel("Organic Carbon", plotOutput("organic_carbon")),
                  tabPanel("Trihalomethanes", plotOutput("trihalomethanes")),
                  tabPanel("Turbidity", plotOutput("turbidity")),
      ))
  )
  
