sum_presences <- function(event_list) {
  to_map <- event_list %>%
    bind_rows() %>%
    filter(presence == 1) %>%
    group_by(gridid) %>%
    summarize(n = n()) %>%
    left_join(select(drivers, gridid, lon, lat))
}
# Usage: quickmap(sum_presences(bsm_events), n)

sum_absences <- function(event_list) {
  to_map <- event_list %>%
    bind_rows() %>%
    filter(presence == 0) %>%
    group_by(gridid) %>%
    summarize(n = n()) %>%
    left_join(select(drivers, gridid, lon, lat))
}
# Usage: quickmap(sum_absences(bsm_events), n)
