## Any questions github alrobles mail a.l.robles.fernandez at gmail

library(tidyverse)

#package for evaluate models with pseudo r squared
library(pscl)

# to recover tree from data distance
library(phangorn)



source("boostrap_lm_models.R")
source("get_lm_phy_models.R")
source("get_pR2_from_model_list.R")
source("get_mod_phy_plot.R")

incidence <- readr::read_csv("incidence_sample.csv")


rNames <- incidence[,1] %>% pull()
phydist <- read_csv("phydist_sample.csv") %>%
  as.matrix()

rownames(phydist) <- colnames(phydist)

#recover tree
tree <- phangorn::upgma(phydist)

phyNames <- rownames(phydist) %>%
  enframe(name = "phydist")

incNames <- colnames(incidence) %>%
  enframe(name = "incidence")

selectedNames <- list(phyNames, incNames) %>%
  reduce(inner_join) %>%  pull(value)

phydist <- phydist[selectedNames, selectedNames]

incidence <- incidence[, selectedNames] %>%
  as.matrix(  )

rownames(incidence) <- rNames

# running boostrap (not in parallel)
modelList <- boostrap_lm_models(incidence, log(phydist + 1), 100)

#get pseudo r squared
output_pR2 <- get_pR2_from_model_list(modelList)

# plot boxplot with pr2, values over 0.3 are good

p1_boxplot <- output_pR2 %>%
  filter(modelName %in% c("mod_phy")) %>%
  ggplot() + geom_boxplot(aes(McFadden, modelName )) +
  ylab("Model Name") +
  xlab("Cragg and Uhler's pseudo r-squared") +
  coord_flip() + 
  theme_classic()

# density plot
p1_denplot <- output_pR2 %>%
  filter(modelName %in% c("mod_phy")) %>%
  ggplot() + geom_density(aes(McFadden, fill = modelName), alpha = 0.5) +
  ylab("Model Name") +
  xlab("Cragg and Uhler's pseudo r-squared") +
  theme_classic()


#png("p_combined_vars.png", 640, 640)


p_phy <- get_mod_phy_plot(modelList)


# to plot the model
library(cowplot)
mytheme <- theme_minimal_grid(
  font_size = 18,
  color = "grey70"
)


p_phy <- p_phy + mytheme
p1_boxplot <- p1_boxplot  + mytheme
p1_denplot <- p1_denplot  + mytheme


cowplot::plot_grid(p_phy, p1_denplot) +
  theme_half_open()+
  panel_border() +
  background_grid()

