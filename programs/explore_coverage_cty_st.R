library(tidyverse)

indat <- read_csv("R_export/fullIndicAll_periodicReg_irDt_Octfit_span0.4_degree2_analyzeDB_cty.csv")

cty <- indat %>%
  group_by(season, fips) %>%
  summarise(viz = sum(viz, na.rm = TRUE), pop = first(pop)) %>%
  dplyr::mutate(vizn = viz/pop)

stpop <- indat %>%
  distinct(season, fips, pop) %>%
  dplyr::mutate(fips_st = substring(fips, 1, 2)) %>%
  group_by(season, fips_st) %>%
  summarise(pop = sum(pop, na.rm = TRUE))

st <- indat %>%
  dplyr::mutate(fips_st = substring(fips, 1, 2)) %>%
  group_by(season, fips_st) %>%
  summarise(viz = sum(viz, na.rm = TRUE)) %>%
  left_join(stpop, by = c("season", "fips_st")) %>%
  dplyr::mutate(vizn = viz/pop)

cty_mu <- cty %>%
  group_by(season) %>%
  summarise(mu = mean(vizn))
st_mu <- st %>%
  group_by(season) %>%
  summarise(mu = mean(vizn))

ctyplt <- ggplot(cty, aes(x = vizn, y = ..density.., group = season)) +
  geom_histogram(bins = 50) +
  geom_density(alpha = 0.2) +
  geom_vline(data = cty_mu, aes(xintercept = mu),
             color="blue", linetype="dashed", size=1) +
  facet_wrap(~season)
print(ctyplt)
ggsave("graph_outputs/explore_coverage_cty_st/cty_cov_hist.png", ctyplt, width = 5, height = 4)

stplt <- ggplot(st, aes(x = vizn, y = ..density.., group = season)) +
  geom_histogram(bins = 50) +
  geom_density(alpha = 0.2) +
  geom_vline(data = st_mu, aes(xintercept = mu),
             color="blue", linetype="dashed", size=1) +
  facet_wrap(~season)
ggsave("graph_outputs/explore_coverage_cty_st/st_cov_hist.png", stplt, width = 5, height = 4)

