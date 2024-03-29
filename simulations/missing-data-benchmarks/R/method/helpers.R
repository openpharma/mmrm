# This is a convenience function assembling the list output by rct_dgp_fun into
# a data.frame. This is because simChef can only return list objects from DGP
# functions.
assemble_df <- function(
    participant,
    visit_num,
    base_bcva,
    strata,
    trt,
    bcva_change,
    participant_as_factor = FALSE,
    visit_num_as_factor = FALSE) {
  if (participant_as_factor) {
    participant <- factor(participant)
  }

  if (visit_num_as_factor) {
    visit_num <- factor(visit_num)
  }

  data.frame(
    "participant" = participant,
    "visit_num" = visit_num,
    "base_bcva" = base_bcva,
    "strata" = strata,
    "trt" = trt,
    "bcva_change" = bcva_change
  )
}
