load_all()

load("cache/drivers_orig.RData")
data(drivers)

pg <- drivers %>%
  select(gridid, lon, lat, iso3, pubs_fit) %>%
  group_by(iso3) %>%
  mutate(pubs_new = sum(pubs_fit, na.rm = TRUE)) %>%
  left_join(select(drivers_orig, gridid, pubs_jones))

quickmap(pg, pubs_jones)
quickmap(pg, pubs_new)

pc <- pg %>%
  group_by(iso3) %>%
  summarize(pubs_jones = unique(pubs_jones),
            pubs_new = unique(pubs_new))

write.csv(pc, "~/Desktop/pubs_2016-10-20.csv", row.names = FALSE)


qplot(log(pubs_new), log(pubs_jones), data = pc)


# Country-level model
pmc <- glm(pubs_new ~ pubs_jones, data = pc, family = poisson)
pmc2 <- lm(pubs_new ~ pubs_jones, data = pc)
pmc3 <- lm(log(pubs_new + 1) ~ log(pubs_jones + 1), data = pc)

# Country-level model
pmg <- glm(pubs_new ~ pubs_jones, data = pg, family = poisson)
pmg2 <- glm(pubs_fit ~ pubs_jones, data = pg, family = poisson)


summary(pmc)
1 - pmc$deviance / pmc$null.deviance

summary(pmc2)
1 - pmc2$deviance / pmc2$null.deviance

summary(pmc3)
1 - pmc3$deviance / pmc3$null.deviance

summary(pmg)
1 - pmg$deviance / pmg$null.deviance

summary(pmg2)
1 - pmg$deviance / pmg$null.deviance
