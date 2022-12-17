library(tidyquant)
library(tidyverse)
library(readxl)
library(ggrepel)
library(geomtextpath)
library(gridExtra)

colorp <- c("#252525", "#969696", "#a50f15")

sp500_prices  <- tq_get("^GSPC", get = "stock.prices", from = " 1928-01-01")

sp500_monthly <- sp500_prices %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted, mutate_fun = to.monthly, indexAt = "lastof")

sp500_returns <- tq_get("^GSPC", get  = "stock.prices", from = "1928-01-01", to   = "2022-11-30") %>%
  tq_transmute(adjusted, periodReturn, period = "monthly", col_rename = "sp500.returns")

class(sp500_returns)

sp500_xts <- xts(sp500_returns$sp500.returns, order.by=sp500_returns$date)

sp500_40 <- rollapply(sp500_xts, width = 480, FUN = Return.annualized, fill = 0) 
names(sp500_40) <- "S&P 500"
sp500_40 <- sp500_40[480:length(sp500_40)]


plot1 <- ggplot(sp500_40, aes(x=index(sp500_40), y=sp500_40$`S&P 500`*100)) + 
  geom_line() +
  geom_hline(yintercept=7, linetype=2, color=colorp[3]) +
  geom_text(aes(index(sp500_40)[1],7),label = '7% growth', vjust = -1, hjust=.3, color=colorp[3], family='serif', size=4) +
  geom_hline(yintercept=9, linetype=2, color=colorp[3]) +
  geom_text(aes(index(sp500_40)[1],9),label = '9% growth', vjust = -1, hjust=.3, color=colorp[3], family='serif', size=4) +
  labs(title = "S&P 500 over 40-Years Frequently Achieved 7% Annualized Growth, Broke 9%",
       subtitle = "S&P 500 Rolling 40-Year Returns Annualized",
       caption = "Data visaulization by BetaDelta Analytics & Research
                  \n Source: Yahoo Finance Historical Data: Adjusted Prices for ^GSPC
                  \n R package used for gathering data and annualizing returns: tidyquant") + 
  xlab("") +
  ylab("") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_classic() +
  theme(plot.title=element_text(size=16),
        plot.caption=element_text(size=8, color=colorp[1]),
        axis.title.x=element_text(size=14, hjust = 0),
        axis.title.y=element_blank(), #element_text(size=14),
        axis.text.y = element_text(size=10, hjust = 0), #element_blank(),  #remove y axis labels
        #axis.ticks.y= element_blank(),
        axis.line.y = element_blank(),
        text=element_text(family = "serif") #remove y axis ticks
  )
  

##############################################################################################

df <- read_xlsx("Growth of 10 Dollars Table Seven.xlsx")

df_points <- rbind(head(df,1), df[120,], df[240,], df[360,], tail(df,1))

colorp <- c("#252525", "#969696", "#a50f15")

plot2 <- ggplot(df, aes(date, value)) + 
  geom_textline(label = "$10  a day invested monthly @ 7% annual growth.", 
                linewidth = 1, color = colorp[1], vjust = 1.5, hjust = 0.75, family = "serif", size = 5) +
  geom_point(data = df_points, aes(date, value), size = 4, color = colorp[3]) +
  geom_text_repel(data = df_points, aes(label = scales::dollar(round(value,0))), 
                  size = 4, color = colorp[1], direction="both", vjust = -1.5, hjust = 1.2, point.padding = 10, 
                  family = "serif") + 
  labs(title = "What habit can you change to save $10 a day?",
       subtitle = "Financial coaching can help you change your life.",
       caption = "\n Data visaulization by BetaDelta Analytics & Research") + 
  xlab(" \n 40 years starting New Year's 2023.  You can do it.") +
  ylab("") +
  theme_classic() +
  theme(plot.title=element_text(size=20),
        plot.caption=element_text(size=8, color=colorp[1]),
        axis.title.x=element_text(size=14, hjust = 0),
        axis.title.y=element_blank(), #element_text(size=14),
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),
        axis.line.y = element_blank(),
        text=element_text(family = "serif") #remove y axis ticks
  )

grid.arrange(plot1, plot2, nrow = 2)
