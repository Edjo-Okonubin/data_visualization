###LOAD REQUIRED PACKAGES

library(ggplot2)
library(dplyr)
library(reshape2)
library(zoo)
library(directlabels)

### INGEST DATA
df <- read.csv("C:/Users/EDJO/Documents/Data Science Projects/data_visualization/iphone_sales/data/raw_data/data-orzoM.csv", stringsAsFactors = FALSE)

### Take a peep at the data.
glimpse (df)

### Get quarter year variable into a workable form in R.

df$Year <- grep("Q", unlist(strsplit(as.character(df$Quarter), "\\s")), value = TRUE, invert=TRUE)

df$Q <- grep("Q", unlist(strsplit(as.character(df$Quarter), "\\s")), value = TRUE, invert = FALSE )

df$yrq <-paste(df$Year, df$Q, sep= " ")
df$yrq <- as.yearqtr(df$yrq)

###reshape data into long, tidy format
df2 <- melt(df, id=c("Quarter","yrq","Year", "Q"))
df2$value <- as.numeric(df2$value)

###plot data as in blog post

p <- ggplot(data = df2, aes(x=yrq, y=value, group=variable, color=variable)) +
        geom_line(size = 1.5) +
        scale_x_yearqtr(limits = c(2004, 2015),
                        format = "%Y", breaks=c(2004:2015), expand=c(0.1, 0.01)) +
        scale_color_manual(values = c("red3", "grey", "grey")) +
        scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80)) +
        theme_bw() +
        theme(panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text = element_text(size=15),
              plot.margin = unit(c(1,1,1,1), "lines"),
              legend.position = "none",
              plot.title = element_text(size = 22),
              plot.subtitle = element_text( size = 15),
              panel.border = element_blank()
        ) +
        geom_dl(aes(label=variable),
                method = list(c("last.points"),
                aes(colour = "black"),
                cex = 1.3)
        ) +
        ggtitle("iPhone more successful than all other Apple products", 
                subtitle = "Worldwide sales of selected Apple products in million, by fiscal year, 2000 to 2014") +
        annotate("rect", xmin = 2010, xmax = 2011, ymin = 0, ymax = Inf, fill = "grey", alpha = 0.2) +
        
        annotate("text", x = 2010.5, y = 40, label = "After Apple announced \n the iPhone 4 \n
                 in 2010, \n more iPhones were sold \n than iPods for the first time.", hjust =1,
                 size = 6)

###Save plot as PNG
  ggsave("iPhone_sales.png", p, width=12, height = 5, units = c("in"))

