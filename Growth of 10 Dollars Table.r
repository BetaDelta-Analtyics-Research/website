library(tidyverse)
library(readxl)
library(ggrepel)
library(geomtextpath)

df <- read_xlsx("Growth of 10 Dollars Table.xlsx")

colorp <- c("#006d2c", "#cccccc", "#de2d26")

ggplot(df, aes(date, value)) + 
  geom_textline(label = "$10  a day invested monthly @ 9% annual growth.", 
                linewidth = 1, color = colorp[1], vjust = -0.5, family = "serif", size = 5) +
  geom_point(data = tail(df,1), aes(date, value), size = 4, color = colorp[1], family = "serif") +
  geom_text_repel(data = tail(df,1), aes(label = scales::dollar(round(value,0))), size = 4, color = colorp[1], family = "serif") +
  labs(title = "Change Your Life With Financial Coaching",
       subtitle = "Debt makes life harder. \n \n \n What habit can you change to save $10 a day?",
       caption = "\n Data visaulization by BetaDelta Analytics & Research") + 
  xlab(" \n 40 years starting New Year's 2023.  You can do it.") +
  ylab("Are you ready? \n") +
  theme_classic() +
  theme(plot.title=element_text(size=20),
        plot.caption=element_text(size=8, color="gray"),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),
        text=element_text(family = "serif") #remove y axis ticks
        )