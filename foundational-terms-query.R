library(ngramr)
library(tidyverse)
library(cowplot)
library(wesanderson)

# vector of terms to query 
# (note: labor/labour relations to be queried and aggregated later)
foundational_terms <- c("
                        human resources, 
                        human capital, 
                        people management
                        ")

# case insensitive query
foundational_ng  <- ngrami(foundational_terms, year_start = 1800, smoothing = 0)

# aggregate labor + labour relations
labor_ng <- ngrami("labor relations", year_start = 1800, smoothing = 0)
labour_ng <- ngrami("labour relations", year_start = 1800, smoothing = 0)
labor_agg_ng <- data.frame("Year" = labor_ng$Year,
                           "Phrase" = "labo(u)r relations",
                           "Frequency" = c(labor_ng$Frequency+labour_ng$Frequency),
                           "Corpus" = "eng_2019")
rm(labor_ng, labour_ng)

# add labo(u)r relations ng 
foundational_ng <- rbind(labor_agg_ng, foundational_ng)

# custom color palette
pal <- c(wes_palette("Darjeeling1", 3), "#bf3eff")

# all data plot
p1 <- ggplot(foundational_ng, aes(x=Year, y=Frequency, colour=Phrase)) +
  geom_line(size=1, show.legend = T)+
  scale_color_manual(values = pal)+
  theme_bw()
p1

# zoomed in plot
p2 <- ggplot(foundational_ng, aes(x=Year, y=Frequency, colour=Phrase)) +
  geom_line(size=1, show.legend = T)+
  scale_color_manual(values = pal)+
  coord_cartesian(xlim = c(1951, 2019), 
                  ylim = c(0, 2e-05))+
  theme_bw()
p2

# plot panels + save
leg <- get_legend(p1+theme(legend.position = "bottom"))
prow <- plot_grid(p1+theme(legend.position = "none"), 
                  p2+theme(legend.position = "none"), 
                  nrow = 1, labels = "AUTO")

png("foundational-terms-ngrams.png", res = 300, width = 20, height = 8, units = "cm")
plot_grid(prow, leg, ncol = 1, rel_heights = c(1,0.1))
dev.off()

# inspect peak years for each phrase
foundational_ng %>% 
  group_by(Phrase) %>%
  arrange(Frequency) %>%
  filter(row_number()==n())


