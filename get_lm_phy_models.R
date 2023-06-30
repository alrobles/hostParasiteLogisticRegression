get_lm_phy_models <- function (incidence, phydist)
{
  if (nrow(incidence) == 1) {
    l <- list(names(incidence[, apply(incidence, 2, function(x) (x ==
                                                                   1))]))
    names(l) <- rownames(incidence)
  }
  else {
    l <- lapply(apply(apply(incidence, 2, function(x) (x ==
                                                         1)), 1, which), names)
  }
  focal <- lapply(l, function(x) sample(x, 1))
  df_phy <- do.call(rbind, lapply(names(focal), function(x) data.frame(colnames(incidence),
                                                                       focal[[x]], phydist[, focal[[x]]], as.numeric(incidence[x,
                                                                       ]), x)))
  colnames(df_phy) <- c("tohost", "fromhost", "phydist", "suscept",
                        "incidence")
  
  mod_phy <- stats::glm(suscept ~ phydist, data = df_phy, family = stats::binomial(link = "logit"))
  modelList <- list(mod_phy)
  names(modelList) <- c("mod_phy")
  return(modelList)
}

