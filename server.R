library(shiny)
library(ggplot2)
library(plotly)
library(reshape2)
library(gridExtra)
library(grid)
library(cowplot)
library(dplyr)
library(data.table)




function(input, output) {
  
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
  
  output$overall <- renderPlot({
    
    ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
      geom_rect() +
      geom_label( x=3.5, aes(y=labelPosition, label=label), size=5) +
      scale_fill_manual(values = c("#E86363", "#63E871")) + 
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
      theme_void() +
      theme(legend.position = "none")
  }, height = 700, width = 900)
  
  output$ph <- renderPlot({
    water_quality <- filter(water_quality,
                            ph >= input$ph[1],
                            ph <= input$ph[2])
    water_quality <- filter(water_quality,
                            Hardness >= input$hardness[1],
                            Hardness <= input$hardness[2])
    water_quality <- filter(water_quality,
                            Solids >= input$solids[1],
                            Solids <= input$solids[2])
    water_quality <- filter(water_quality,
                            Chloramines >= input$chloramines[1],
                            Chloramines <= input$chloramines[2])
    water_quality <- filter(water_quality,
                            Conductivity >= input$conductivity[1],
                            Conductivity <= input$conductivity[2])
    water_quality <- filter(water_quality,
                            Organic_carbon >= input$organic_carbon[1],
                            Organic_carbon <= input$organic_carbon[2])
    water_quality <- filter(water_quality,
                            Trihalomethanes >= input$trihalomethanes[1],
                            Trihalomethanes <= input$trihalomethanes[2])
    water_quality <- filter(water_quality,
                            Turbidity >= input$turbidity[1],
                            Turbidity <= input$turbidity[2])
    
    nf <- layout( matrix(c(1,2), ncol=1))
    par(mar=c(0, 3.1, 1.1, 2.1))
    
    p1_1 <- ggplot(subset(water_quality, Potability %in% c("0")), aes(x=ph)) +
      geom_boxplot(fill="#E86363", alpha=0.7) +
      labs(x = " ") +
      theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.text.x = element_blank(),
            axis.text.y = element_blank(), axis.ticks.x = element_blank(), 
            axis.ticks.y = element_blank())
    
    p1_2 <- ggplot(subset(water_quality, Potability %in% c("1")), aes(x=ph)) +
      geom_boxplot(fill="#63E871", alpha=0.7) +
      labs(x = " ") +
      theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.text.x = element_blank(),
            axis.text.y = element_blank(), axis.ticks.x = element_blank(), 
            axis.ticks.y = element_blank())
    
    par(mar=c(4, 3.1, 1.1, 2.1))
    
    p2 <- ggplot(water_quality, aes(x = ph, color = Potability, fill=Potability)) + 
      geom_histogram(position= "identity", alpha = 0.4, bins=100) +
      scale_color_manual(values=c("#E86363", "#63E871"), guide = "none") +
      scale_fill_manual(values=c("#E86363", "#63E871"), labels = c("Non-Potable", "Potable")) +
      geom_vline(xintercept = 6.5, color = "darkslategrey", linetype = "dashed", size=0.3) + 
      geom_vline(xintercept = 8.5, color = "darkslategrey", linetype = "dashed", size=0.3) +
      labs(x = "pH Level (pH)", y = "Count") + 
      theme(text=element_text(size=15, family = "Helvetica"), legend.position=c(0.85, 0.5)) + 
      annotate("text", x = 5, y = 50, label = "< 6.5 is acidic") +
      annotate("text", x = 10, y = 50, label = "> 8.5 is basic") + 
      annotate("text", x = 7.5, y = 62, label = "recommended", size=3.2, color="darkslategrey") +
      annotate("text", x = 7.5, y = 60, label = "levels", size=3.2, color="darkslategrey")
    
    title <- ggdraw() + draw_label("pH Level Distribution", fontface='bold', size=20)
    plot_grid(title, p1_1, p1_2, p2, ncol=1, align="v", rel_heights = c(0.6, 0.8, 0.8, 6))
  }, height = 800, width = 1000)
  
  output$hardness <- renderPlot({
    water_quality <- filter(water_quality,
                            ph >= input$ph[1],
                            ph <= input$ph[2])
    water_quality <- filter(water_quality,
                            Hardness >= input$hardness[1],
                            Hardness <= input$hardness[2])
    water_quality <- filter(water_quality,
                            Solids >= input$solids[1],
                            Solids <= input$solids[2])
    water_quality <- filter(water_quality,
                            Chloramines >= input$chloramines[1],
                            Chloramines <= input$chloramines[2])
    water_quality <- filter(water_quality,
                            Conductivity >= input$conductivity[1],
                            Conductivity <= input$conductivity[2])
    water_quality <- filter(water_quality,
                            Organic_carbon >= input$organic_carbon[1],
                            Organic_carbon <= input$organic_carbon[2])
    water_quality <- filter(water_quality,
                            Trihalomethanes >= input$trihalomethanes[1],
                            Trihalomethanes <= input$trihalomethanes[2])
    water_quality <- filter(water_quality,
                            Turbidity >= input$turbidity[1],
                            Turbidity <= input$turbidity[2])
    
    nf <- layout( matrix(c(1,2), ncol=1))
    par(mar=c(0, 3.1, 1.1, 2.1))
    
    p1_1 <- ggplot(subset(water_quality, Potability %in% c("0")), aes(x=Hardness)) +
      geom_boxplot(fill="#E86363", alpha=0.7) +
      labs(x = " ") +
      theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.text.x = element_blank(),
            axis.text.y = element_blank(), axis.ticks.x = element_blank(), 
            axis.ticks.y = element_blank())
    
    p1_2 <- ggplot(subset(water_quality, Potability %in% c("1")), aes(x=Hardness)) +
      geom_boxplot(fill="#63E871", alpha=0.7) +
      labs(x = " ") +
      theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.text.x = element_blank(),
            axis.text.y = element_blank(), axis.ticks.x = element_blank(), 
            axis.ticks.y = element_blank())
    
    par(mar=c(4, 3.1, 1.1, 2.1))
    
    p2 <- ggplot(water_quality, aes(x = Hardness, color = Potability, fill=Potability)) + 
      geom_histogram(position= "identity", alpha = 0.4, bins=100) +
      scale_color_manual(values=c("#E86363", "#63E871"), guide = "none") +
      scale_fill_manual(values=c("#E86363", "#63E871"), labels = c("Non-Potable", "Potable")) +
      geom_vline(xintercept = 75, color = "darkslategrey", linetype = "dashed", size=0.3) + 
      geom_vline(xintercept = 150, color = "darkslategrey", linetype = "dashed", size=0.3) + 
      geom_vline(xintercept = 300, color = "darkslategrey", linetype = "dashed", size=0.3) + 
      labs(x = "Hardness Level (mg/L)", y = "Count") + 
      theme(text=element_text(size=15, family = "Helvetica"), legend.position=c(0.85, 0.5)) + 
      annotate("text", x = 75, y = 45, label = "< 75 is soft") +
      annotate("text", x = 110, y = 50, label = "> 75 is moderately hard") +
      annotate("text", x = 170, y = 50, label = "> 150 is hard") +
      annotate("text", x = 305, y = 50, label = "> 300 is very hard")
    
    title <- ggdraw() + draw_label("Hardness Level Distribution", fontface='bold', size=20)
    plot_grid(title, p1_1, p1_2, p2, ncol=1, align="v", rel_heights = c(0.6, 0.8, 0.8, 6))
  }, height = 800, width = 1000)
  
  output$solids <- renderPlot({
    water_quality <- filter(water_quality,
                            ph >= input$ph[1],
                            ph <= input$ph[2])
    water_quality <- filter(water_quality,
                            Hardness >= input$hardness[1],
                            Hardness <= input$hardness[2])
    water_quality <- filter(water_quality,
                            Solids >= input$solids[1],
                            Solids <= input$solids[2])
    water_quality <- filter(water_quality,
                            Chloramines >= input$chloramines[1],
                            Chloramines <= input$chloramines[2])
    water_quality <- filter(water_quality,
                            Conductivity >= input$conductivity[1],
                            Conductivity <= input$conductivity[2])
    water_quality <- filter(water_quality,
                            Organic_carbon >= input$organic_carbon[1],
                            Organic_carbon <= input$organic_carbon[2])
    water_quality <- filter(water_quality,
                            Trihalomethanes >= input$trihalomethanes[1],
                            Trihalomethanes <= input$trihalomethanes[2])
    water_quality <- filter(water_quality,
                            Turbidity >= input$turbidity[1],
                            Turbidity <= input$turbidity[2])
    
    nf <- layout( matrix(c(1,2), ncol=1))
    par(mar=c(0, 3.1, 1.1, 2.1))
    
    p1_1 <- ggplot(subset(water_quality, Potability %in% c("0")), aes(x=Solids)) +
      geom_boxplot(fill="#E86363", alpha=0.7) +
      labs(x = " ") +
      theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.text.x = element_blank(),
            axis.text.y = element_blank(), axis.ticks.x = element_blank(), 
            axis.ticks.y = element_blank())
    
    p1_2 <- ggplot(subset(water_quality, Potability %in% c("1")), aes(x=Solids)) +
      geom_boxplot(fill="#63E871", alpha=0.7) +
      labs(x = " ") +
      theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.text.x = element_blank(),
            axis.text.y = element_blank(), axis.ticks.x = element_blank(), 
            axis.ticks.y = element_blank())
    
    par(mar=c(4, 3.1, 1.1, 2.1))
    
    p2 <- ggplot(water_quality, aes(x = Solids, color = Potability, fill=Potability)) + 
      geom_histogram(position= "identity", alpha = 0.4, bins=100) +
      scale_color_manual(values=c("#E86363", "#63E871"), guide = "none") +
      scale_fill_manual(values=c("#E86363", "#63E871"), labels = c("Non-Potable", "Potable")) +
      geom_vline(xintercept = 500, color = "darkslategrey", linetype = "dashed", size=0.3) + 
      geom_vline(xintercept = 1000, color = "darkslategrey", linetype = "dashed", size=0.3) +
      labs(x = "Solid Level (mg/l)", y = "Count") + 
      theme(text=element_text(size=15, family = "Helvetica"), legend.position=c(0.85, 0.5)) + 
      annotate("text", x = 6000, y = 50, label = "> 1000 is high") + 
      annotate("text", x = 800, y = 62, label = "recommended", size=3.2, color="darkslategrey") +
      annotate("text", x = 800, y = 60, label = "levels", size=3.2, color="darkslategrey")
    
    title <- ggdraw() + draw_label("Solid Level Distribution", fontface='bold', size=20)
    plot_grid(title, p1_1, p1_2, p2, ncol=1, align="v", rel_heights = c(0.6, 0.8, 0.8, 6))
  }, height = 800, width = 1000)
  
  output$chloramines <- renderPlot({
    water_quality <- filter(water_quality,
                            ph >= input$ph[1],
                            ph <= input$ph[2])
    water_quality <- filter(water_quality,
                            Hardness >= input$hardness[1],
                            Hardness <= input$hardness[2])
    water_quality <- filter(water_quality,
                            Solids >= input$solids[1],
                            Solids <= input$solids[2])
    water_quality <- filter(water_quality,
                            Chloramines >= input$chloramines[1],
                            Chloramines <= input$chloramines[2])
    water_quality <- filter(water_quality,
                            Conductivity >= input$conductivity[1],
                            Conductivity <= input$conductivity[2])
    water_quality <- filter(water_quality,
                            Organic_carbon >= input$organic_carbon[1],
                            Organic_carbon <= input$organic_carbon[2])
    water_quality <- filter(water_quality,
                            Trihalomethanes >= input$trihalomethanes[1],
                            Trihalomethanes <= input$trihalomethanes[2])
    water_quality <- filter(water_quality,
                            Turbidity >= input$turbidity[1],
                            Turbidity <= input$turbidity[2])
    
    nf <- layout( matrix(c(1,2), ncol=1))
    par(mar=c(0, 3.1, 1.1, 2.1))
    
    p1_1 <- ggplot(subset(water_quality, Potability %in% c("0")), aes(x=Chloramines)) +
      geom_boxplot(fill="#E86363", alpha=0.7) +
      labs(x = " ") +
      theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.text.x = element_blank(),
            axis.text.y = element_blank(), axis.ticks.x = element_blank(), 
            axis.ticks.y = element_blank())
    
    p1_2 <- ggplot(subset(water_quality, Potability %in% c("1")), aes(x=Chloramines)) +
      geom_boxplot(fill="#63E871", alpha=0.7) +
      labs(x = " ") +
      theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.text.x = element_blank(),
            axis.text.y = element_blank(), axis.ticks.x = element_blank(), 
            axis.ticks.y = element_blank())
    
    par(mar=c(4, 3.1, 1.1, 2.1))
    
    p2 <- ggplot(water_quality, aes(x = Chloramines, color = Potability, fill=Potability)) + 
      geom_histogram(position= "identity", alpha = 0.4, bins=100) +
      scale_color_manual(values=c("#E86363", "#63E871"), guide = "none") +
      scale_fill_manual(values=c("#E86363", "#63E871"), labels = c("Non-Potable", "Potable")) +
      geom_vline(xintercept = 4, color = "darkslategrey", linetype = "dashed", size=0.3) + 
      labs(x = "Chloramine Level (mg/L)", y = "Count") + 
      theme(text=element_text(size=15, family = "Helvetica"), legend.position=c(0.85, 0.5)) + 
      annotate("text", x = 4.8, y = 45, label = "> 4 is high") +
      annotate("text", x = 4, y = 55, label = "recommended", size=3.2, color="darkslategrey") +
      annotate("text", x = 4, y = 53, label = "threshold", size=3.2, color="darkslategrey")
    
    title <- ggdraw() + draw_label("Chloramine Level Distribution", fontface='bold', size=20)
    plot_grid(title, p1_1, p1_2, p2, ncol=1, align="v", rel_heights = c(0.6, 0.8, 0.8, 6))
  }, height = 800, width = 1000)
  
  output$conductivity <- renderPlot({
    water_quality <- filter(water_quality,
                            ph >= input$ph[1],
                            ph <= input$ph[2])
    water_quality <- filter(water_quality,
                            Hardness >= input$hardness[1],
                            Hardness <= input$hardness[2])
    water_quality <- filter(water_quality,
                            Solids >= input$solids[1],
                            Solids <= input$solids[2])
    water_quality <- filter(water_quality,
                            Chloramines >= input$chloramines[1],
                            Chloramines <= input$chloramines[2])
    water_quality <- filter(water_quality,
                            Conductivity >= input$conductivity[1],
                            Conductivity <= input$conductivity[2])
    water_quality <- filter(water_quality,
                            Organic_carbon >= input$organic_carbon[1],
                            Organic_carbon <= input$organic_carbon[2])
    water_quality <- filter(water_quality,
                            Trihalomethanes >= input$trihalomethanes[1],
                            Trihalomethanes <= input$trihalomethanes[2])
    water_quality <- filter(water_quality,
                            Turbidity >= input$turbidity[1],
                            Turbidity <= input$turbidity[2])
    
    nf <- layout( matrix(c(1,2), ncol=1))
    par(mar=c(0, 3.1, 1.1, 2.1))
    
    p1_1 <- ggplot(subset(water_quality, Potability %in% c("0")), aes(x=Conductivity)) +
      geom_boxplot(fill="#E86363", alpha=0.7) +
      labs(x = " ") +
      theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.text.x = element_blank(),
            axis.text.y = element_blank(), axis.ticks.x = element_blank(), 
            axis.ticks.y = element_blank())
    
    p1_2 <- ggplot(subset(water_quality, Potability %in% c("1")), aes(Conductivity)) +
      geom_boxplot(fill="#63E871", alpha=0.7) +
      labs(x = " ") +
      theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.text.x = element_blank(),
            axis.text.y = element_blank(), axis.ticks.x = element_blank(), 
            axis.ticks.y = element_blank())
    
    par(mar=c(4, 3.1, 1.1, 2.1))
    
    p2 <- ggplot(water_quality, aes(x = Conductivity, color = Potability, fill=Potability)) + 
      geom_histogram(position= "identity", alpha = 0.4, bins=100) +
      scale_color_manual(values=c("#E86363", "#63E871"), guide = "none") +
      scale_fill_manual(values=c("#E86363", "#63E871"), labels = c("Non-Potable", "Potable")) +
      geom_vline(xintercept = 400, color = "darkslategrey", linetype = "dashed", size=0.3) + 
      labs(x = "Conductivity Level (μS/cm)", y = "Count") + 
      theme(text=element_text(size=15, family = "Helvetica"), legend.position=c(0.85, 0.5)) + 
      annotate("text", x = 460, y = 42, label = "> 400 is high") +
      annotate("text", x = 400, y = 47, label = "recommended", size=3.2, color="darkslategrey") +
      annotate("text", x = 400, y = 45, label = "threshold", size=3.2, color="darkslategrey")
    
    title <- ggdraw() + draw_label("Conductivity Level Distribution", fontface='bold', size=20)
    plot_grid(title, p1_1, p1_2, p2, ncol=1, align="v", rel_heights = c(0.6, 0.8, 0.8, 6))
  }, height = 800, width = 1000)
  
  output$organic_carbon <- renderPlot({
    water_quality <- filter(water_quality,
                            ph >= input$ph[1],
                            ph <= input$ph[2])
    water_quality <- filter(water_quality,
                            Hardness >= input$hardness[1],
                            Hardness <= input$hardness[2])
    water_quality <- filter(water_quality,
                            Solids >= input$solids[1],
                            Solids <= input$solids[2])
    water_quality <- filter(water_quality,
                            Chloramines >= input$chloramines[1],
                            Chloramines <= input$chloramines[2])
    water_quality <- filter(water_quality,
                            Conductivity >= input$conductivity[1],
                            Conductivity <= input$conductivity[2])
    water_quality <- filter(water_quality,
                            Organic_carbon >= input$organic_carbon[1],
                            Organic_carbon <= input$organic_carbon[2])
    water_quality <- filter(water_quality,
                            Trihalomethanes >= input$trihalomethanes[1],
                            Trihalomethanes <= input$trihalomethanes[2])
    water_quality <- filter(water_quality,
                            Turbidity >= input$turbidity[1],
                            Turbidity <= input$turbidity[2])
    
    nf <- layout( matrix(c(1,2), ncol=1))
    par(mar=c(0, 3.1, 1.1, 2.1))
    
    p1_1 <- ggplot(subset(water_quality, Potability %in% c("0")), aes(x=Organic_carbon)) +
      geom_boxplot(fill="#E86363", alpha=0.7) +
      labs(x = " ") +
      theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.text.x = element_blank(),
            axis.text.y = element_blank(), axis.ticks.x = element_blank(), 
            axis.ticks.y = element_blank())
    
    p1_2 <- ggplot(subset(water_quality, Potability %in% c("1")), aes(x=Organic_carbon)) +
      geom_boxplot(fill="#63E871", alpha=0.7) +
      labs(x = " ") +
      theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.text.x = element_blank(),
            axis.text.y = element_blank(), axis.ticks.x = element_blank(), 
            axis.ticks.y = element_blank())
    
    par(mar=c(4, 3.1, 1.1, 2.1))
    
    p2 <- ggplot(water_quality, aes(x = Organic_carbon, color = Potability, fill=Potability)) + 
      geom_histogram(position= "identity", alpha = 0.4, bins=100) +
      scale_color_manual(values=c("#E86363", "#63E871"), guide = "none") +
      scale_fill_manual(values=c("#E86363", "#63E871"), labels = c("Non-Potable", "Potable")) +
      geom_vline(xintercept = 2, color = "darkslategrey", linetype = "dashed", size=0.3) + 
      labs(x = "Organic Carbon Level (mg/L)", y = "Count") + 
      theme(text=element_text(size=15, family = "Helvetica"), legend.position=c(0.85, 0.5)) + 
      annotate("text", x = 4, y = 35, label = "> 2 is high") + 
      annotate("text", x = 2.5, y = 43, label = "recommended", size=3.2, color="darkslategrey") +
      annotate("text", x = 2.5, y = 41, label = "threshold", size=3.2, color="darkslategrey")
    
    title <- ggdraw() + draw_label("Organic Carbon Level Distribution", fontface='bold', size=20)
    plot_grid(title, p1_1, p1_2, p2, ncol=1, align="v", rel_heights = c(0.6, 0.8, 0.8, 6))
  }, height = 800, width = 1000)
  
  output$trihalomethanes <- renderPlot({
    water_quality <- filter(water_quality,
                            ph >= input$ph[1],
                            ph <= input$ph[2])
    water_quality <- filter(water_quality,
                            Hardness >= input$hardness[1],
                            Hardness <= input$hardness[2])
    water_quality <- filter(water_quality,
                            Solids >= input$solids[1],
                            Solids <= input$solids[2])
    water_quality <- filter(water_quality,
                            Chloramines >= input$chloramines[1],
                            Chloramines <= input$chloramines[2])
    water_quality <- filter(water_quality,
                            Conductivity >= input$conductivity[1],
                            Conductivity <= input$conductivity[2])
    water_quality <- filter(water_quality,
                            Organic_carbon >= input$organic_carbon[1],
                            Organic_carbon <= input$organic_carbon[2])
    water_quality <- filter(water_quality,
                            Trihalomethanes >= input$trihalomethanes[1],
                            Trihalomethanes <= input$trihalomethanes[2])
    water_quality <- filter(water_quality,
                            Turbidity >= input$turbidity[1],
                            Turbidity <= input$turbidity[2])
    
    nf <- layout( matrix(c(1,2), ncol=1))
    par(mar=c(0, 3.1, 1.1, 2.1))
    
    p1_1 <- ggplot(subset(water_quality, Potability %in% c("0")), aes(x=Trihalomethanes)) +
      geom_boxplot(fill="#E86363", alpha=0.7) +
      labs(x = " ") +
      theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.text.x = element_blank(),
            axis.text.y = element_blank(), axis.ticks.x = element_blank(), 
            axis.ticks.y = element_blank())
    
    p1_2 <- ggplot(subset(water_quality, Potability %in% c("1")), aes(x=Trihalomethanes)) +
      geom_boxplot(fill="#63E871", alpha=0.7) +
      labs(x = " ") +
      theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.text.x = element_blank(),
            axis.text.y = element_blank(), axis.ticks.x = element_blank(), 
            axis.ticks.y = element_blank())
    
    par(mar=c(4, 3.1, 1.1, 2.1))
    
    p2 <- ggplot(water_quality, aes(x = Trihalomethanes, color = Potability, fill=Potability)) + 
      geom_histogram(position= "identity", alpha = 0.4, bins=100) +
      scale_color_manual(values=c("#E86363", "#63E871"), guide = "none") +
      scale_fill_manual(values=c("#E86363", "#63E871"), labels = c("Non-Potable", "Potable")) +
      geom_vline(xintercept = 80, color = "darkslategrey", linetype = "dashed", size=0.3) + 
      labs(x = "Trihalomethanes Level (ppm)", y = "Count") + 
      theme(text=element_text(size=15, family = "Helvetica"), legend.position=c(0.85, 0.5)) + 
      annotate("text", x = 90, y = 40, label = "> 80 is high") +
      annotate("text", x = 80, y = 47, label = "recommended", size=3.2, color="darkslategrey") +
      annotate("text", x = 80, y = 45, label = "threshold", size=3.2, color="darkslategrey")
    
    title <- ggdraw() + draw_label("Trihalomethanes Level Distribution", fontface='bold', size=20)
    plot_grid(title, p1_1, p1_2, p2, ncol=1, align="v", rel_heights = c(0.6, 0.8, 0.8, 6))
  }, height = 800, width = 1000)
  
  output$turbidity <- renderPlot({
    water_quality <- filter(water_quality,
                            ph >= input$ph[1],
                            ph <= input$ph[2])
    water_quality <- filter(water_quality,
                            Hardness >= input$hardness[1],
                            Hardness <= input$hardness[2])
    water_quality <- filter(water_quality,
                            Solids >= input$solids[1],
                            Solids <= input$solids[2])
    water_quality <- filter(water_quality,
                            Chloramines >= input$chloramines[1],
                            Chloramines <= input$chloramines[2])
    water_quality <- filter(water_quality,
                            Conductivity >= input$conductivity[1],
                            Conductivity <= input$conductivity[2])
    water_quality <- filter(water_quality,
                            Organic_carbon >= input$organic_carbon[1],
                            Organic_carbon <= input$organic_carbon[2])
    water_quality <- filter(water_quality,
                            Trihalomethanes >= input$trihalomethanes[1],
                            Trihalomethanes <= input$trihalomethanes[2])
    water_quality <- filter(water_quality,
                            Turbidity >= input$turbidity[1],
                            Turbidity <= input$turbidity[2])
    
    nf <- layout( matrix(c(1,2), ncol=1))
    par(mar=c(0, 3.1, 1.1, 2.1))
    
    p1_1 <- ggplot(subset(water_quality, Potability %in% c("0")), aes(x=Turbidity)) +
      geom_boxplot(fill="#E86363", alpha=0.7) +
      labs(x = " ") +
      theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.text.x = element_blank(),
            axis.text.y = element_blank(), axis.ticks.x = element_blank(), 
            axis.ticks.y = element_blank())
    
    p1_2 <- ggplot(subset(water_quality, Potability %in% c("1")), aes(x=Turbidity)) +
      geom_boxplot(fill="#63E871", alpha=0.7) +
      labs(x = " ") +
      theme_bw() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.text.x = element_blank(),
            axis.text.y = element_blank(), axis.ticks.x = element_blank(), 
            axis.ticks.y = element_blank())
    
    par(mar=c(4, 3.1, 1.1, 2.1))
    
    p2 <- ggplot(water_quality, aes(x = Turbidity, color = Potability, fill=Potability)) + 
      geom_histogram(position= "identity", alpha = 0.4, bins=100) +
      scale_color_manual(values=c("#E86363", "#63E871"), guide = "none") +
      scale_fill_manual(values=c("#E86363", "#63E871"), labels = c("Non-Potable", "Potable")) +
      geom_vline(xintercept = 5, color = "darkslategrey", linetype = "dashed", size=0.3) + 
      labs(x = "Turbidity Level (NTU)", y = "Count") + 
      theme(text=element_text(size=15, family = "Helvetica"), legend.position=c(0.85, 0.5)) + 
      annotate("text", x = 5.4, y = 37, label = "> 5 is high") +
      annotate("text", x = 5, y = 45, label = "recommended", size=3.2, color="darkslategrey") +
      annotate("text", x = 5, y = 43, label = "threshold", size=3.2, color="darkslategrey")
    
    title <- ggdraw() + draw_label("Turbidity Level Distribution", fontface='bold', size=20)
    plot_grid(title, p1_1, p1_2, p2, ncol=1, align="v", rel_heights = c(0.6, 0.8, 0.8, 6))
  }, height = 800, width = 1000)
}

