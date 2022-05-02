library(ngramr)
library(tidyverse)
library(cowplot)
library(wesanderson)

# trends associated with each approach to people in the workplace ----
LR_trends <- c(
  "ghost work",
  "crowd work",
  "gig work",
  "non-standard employment",
  "algorithmic management",
  "labor union",
  "labour union",
  "collective bargaining"
   )

HR_trends <- c(
  "workforce ecosystem",
  "flexible workforce ",
  "digital natives",
  "robotic process automation", 
  "work-from-home",
  "work from home",
  "remote work",
  "telework",
  "virtual work",
  "agile work"
  )
  
HC_trends <- c(
  "people analytics",
  "workforce analytics",
  "hr analytics", 
  "upskilling", 
  "reskilling", 
  "online learning platforms",
  "talent marketplaces", 
  "opportunity marketplaces"
  )
  
PM_trends <- c(
  "employee experience", 
  "employee wellbeing", 
  "employee wellness", 
  "mindfulness apps",
  "online mindfulness training", 
  "digital detox", 
  "technostress"
)  

# make list with vectors of trends terms
trends_terms <- list(LR_trends, HC_trends, HR_trends, PM_trends)

# loop through list of vectors to query each term as case insensitive ----
res <- NULL
for (i in 1:4) {
  
  sub_res <- NULL
  for (j in trends_terms[i]) {
    ng <- ngrami(j, year_start = 1951, smoothing = 1)
    sub_res <- rbind(sub_res, ng)
  }
  
  sub_res$ID <- i
  res <- rbind(res, sub_res)
}

# label Approach (i.e., the term bucket)
res$Approach <- with(res,
                     ifelse(ID==1,"labo(u)r relations",
                            ifelse(ID==2, "human capital",
                                   ifelse(ID==3, "human resources", 
                                          "people management"))))
res <- res %>% select(-ID)

# summary results; aggregating across terms in each approach/bucket ----
res_sum <- res %>%
  group_by(Year, Approach, Corpus) %>%
  summarise(Frequency=sum(Frequency)) 

# custom color palette
pal <- c(wes_palette("Darjeeling1", 3), "#bf3eff")

# pure data plot
pt1 <- res_sum %>%
  ggplot(aes(x=Year, y=Frequency, colour=Approach)) +
  geom_line(size=1, show.legend = T)+
  scale_color_manual(values = pal)+
  theme_bw()+
  labs(colour="Approach")

# divide labor relations' frequency by 10 and re-plot
res_sum$Frequency10 <- with(res_sum,                           
                 ifelse(Approach=="labo(u)r relations", Frequency/10, 
                                                           Frequency))

pt2 <- res_sum %>%
  ggplot(aes(x=Year, y=Frequency10, colour=Approach,)) +
  geom_line(size=1, show.legend = T)+
  scale_color_manual(labels = c("human capital", 
                                "human resources", 
                                "labo(u)r relations/10", 
                                "people managament"),
                     values = pal)+
  theme_bw()+
  labs(colour="Approach", y="Frequency")


# generate and save panel
leg <- get_legend(pt1+theme(legend.position = "bottom"))
prow <- plot_grid(pt1+theme(legend.position = "none"), 
                  pt2+theme(legend.position = "none"), 
                  nrow = 1, labels = "AUTO")

png("trend-terms-panel.png", res = 300, width = 18, height = 8, units = "cm")
plot_grid(prow, leg, nrow = 2, rel_heights = c(1, 0.2))
dev.off()

# plot split out results ----

# function for aggregating phrases
aggregate_phrases <- function(regex_phrases, new_label, data = res){
  out <- data %>% 
    filter(str_detect(Phrase, c(regex_phrases))) %>%
    group_by(Year, Approach, Corpus) %>%
    summarise(Frequency=sum(Frequency))
  out$Phrase <- new_label 
  out
}

# aggregate labor+labour union
res_lab_unions <- aggregate_phrases(
  regex_phrases = c("labour union|labor union"), 
  new_label = "labo(u)r union")

# aggregate work(-)from(-)home
res_wfh <- aggregate_phrases(
  regex_phrases = c("work from home|work - from - home"), 
  new_label = "work from home")

# reorder cols to align with aggregated dfs
res <- res[, c(names(res_wfh))]

# swap in aggregated phrase frequencies
res <- res %>%
  filter(Phrase!="labour union" & Phrase!="labor union"&
           Phrase!="work from home" & Phrase!="work - from - home")
res <- rbind(res, res_wfh, res_lab_unions)

## labour relations (plot not included in icis submission)----
pLR <- res %>%
  filter(Approach=="labo(u)r relations") %>%
  ggplot(aes(x=Year, y=Frequency, colour=Phrase,)) +
  geom_line(size=1, show.legend = T)+
  theme_bw()

pLR2_res <- res %>% filter(Approach=="labo(u)r relations")
pLR2_res$Frequency <- with(pLR2_res,
                           ifelse(Phrase!="labo(u)r union", Frequency*100,
                                  Frequency))

pLR2 <- pLR2_res %>%
  filter(Approach=="labo(u)r relations") %>%
  ggplot(aes(x=Year, y=Frequency, colour=Phrase,)) +
  geom_line(size=1, show.legend = T)+
  scale_color_manual(labels = c("algorithmic management x 100",
                                "collective bargain x 100",
                                "crowd work x 100",
                                "ghost work x 100",
                                "gig work x 100",
                                "non - standard employment x 100",
                                "labo(u)r union"),
                     values = (unique(ggplot_build(pLR)$data[[1]]$colour)))+
  theme_bw()

pLR3 <- res %>%
  filter(Approach=="labo(u)r relations") %>%
  filter(Phrase != "labo(u)r union" & Phrase != "collective bargaining") %>%
  ggplot(aes(x=Year, y=Frequency, colour=Phrase,)) +
  geom_line(size=1, show.legend = T)+
  scale_color_manual(values = (unique(ggplot_build(pLR)$data[[1]]$colour))[c(1,3:6)])+
  theme_bw()

leg <- get_legend(pLR+theme(legend.position = "bottom"))
prow <- plot_grid(pLR+theme(legend.position = "none"),
                  pLR3+theme(legend.position = "none"),
                  nrow = 1, labels = "AUTO")

png("LR-terms-ngrams.png", res = 300, width = 18, height = 8, units = "cm")
plot_grid(prow, leg, nrow = 2, rel_heights = c(1, 0.25))
dev.off()

## human capital (plot not included in icis submission) ----
pHC <- res %>%
  filter(Approach=="human capital") %>%
  ggplot(aes(x=Year, y=Frequency, colour=Phrase,)) +
  geom_line(size=1, show.legend = T)+
  theme_bw()

png("HC-terms-ngrams.png", res = 300, width = 14, height = 7, units = "cm")
pHC
dev.off()

## human resources ----

pHR1 <- res %>%
  filter(Approach=="human resources") %>%
  ggplot(aes(x=Year, y=Frequency, colour=Phrase,)) +
  geom_line(size=1, show.legend = T)+
  labs(colour="Trend")+
  theme_bw()

pHR2 <- res %>%
  filter(Approach=="human resources") %>%
  filter(Phrase=="work from home" | Phrase=="remote work" | 
           Phrase=="virtual work" | Phrase=="telework") %>%
  ggplot(aes(x=Year, y=Frequency, colour=Phrase,)) +
  geom_line(size=1, show.legend = T)+
  scale_color_manual(values = (unique(ggplot_build(pHR1)$data[[1]]$colour))[c(4,6:8)])+
  theme_bw()

leg <- get_legend(pHR1+theme(legend.position = "bottom"))
prow <- plot_grid(pHR1+theme(legend.position = "none"),
                  pHR2+theme(legend.position = "none"),
                  nrow = 1, labels = "AUTO")

png("HR-terms-ngrams.png", res = 300, width = 18, height = 7, units = "cm")
plot_grid(prow, leg, nrow = 2, rel_heights = c(1, 0.25))
dev.off()

## people management ----

pPM <- res %>%
  filter(Approach=="people management") %>%
  ggplot(aes(x=Year, y=Frequency, colour=Phrase,)) +
  geom_line(size=1, show.legend = T)+
  labs(colour="Trend")+
  theme_bw()

png("PM-terms-ngrams.png", res = 300, width = 14, height = 7, units = "cm")
pPM
dev.off()


