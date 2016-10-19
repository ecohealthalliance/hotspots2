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


qplot(log(pubs_new), log(pubs_jones), data = pc)


# Country-level model
pmc <- glm(pubs_new ~ pubs_jones, data = pc, family = poisson)


# Country-level model
pmg <- glm(pubs_new ~ pubs_jones, data = pg, family = poisson)


summary(pmg)