library(tidyverse)
library(ape)
library(phylolm)
library(pROC)
phy <- ape::rphylo(n = 500,
                   birth = 0.3,
                   death = 0.2,
                   T0 = 600, # total depth in MYr
                   fossils = FALSE,
                   eps = 0.001 )
alpha = 0.1


chr <- phylolm::rbinTrait(n = 1,
                          phy = phy,
                          beta = -(3),
                          alpha = alpha )

chr_count <- chr %>% table()
chr_ratio <- chr_count[2]/chr_count[1]

phyDist <- ape::cophenetic.phylo(phy) %>%
  as.dist(upper = TRUE) %>%
  broom::tidy()
chr_df <- outer(chr, chr) %>%
  as.dist(upper = TRUE) %>%
  broom::tidy() %>%
  rename(class =  distance)
phyDistChr <- inner_join(phyDist, chr_df) %>%
  mutate(label = ifelse(class == 1, "susceptible", "unknown"))
phyDistChr$class %>% table()
samp_size <- phyDistChr %>%
  count(class) %>%
  filter(class == 1) %>% pull(n)
samp_size <- round(samp_size * 0.65)

phyDistChrTrain <- phyDistChr %>%
  group_by(label) %>%
  sample_n(samp_size)
phyDistChrTest <- anti_join(phyDistChr, phyDistChrTrain)

phyDistTrainROC <- roc(class ~ distance, phyDistChrTrain)
phyDistTestROC <- roc(class ~ distance, phyDistChrTest)
plot(phyDistTestROC)
model1 <- phyDistChrTrain %>%
  glm(class ~ distance, data = ., family = stats::binomial(link = "logit"))
model_pseudoR2 <- pscl::pR2(model1) %>% enframe()

simList <- list(
  phyTree = phy,
  chr = chr,
  model = model1,
  dataTrain = phyDistChrTrain,
  dataTest = phyDistChrTest,
  pseudoR = model_pseudoR2,
  rocTrain = phyDistTrainROC,
  rocTest = phyDistTestROC
)


