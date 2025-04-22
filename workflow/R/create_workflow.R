require("tidymodels")

create_workflow <- function(recipe, model_spec, weighted = FALSE) {
  wf <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(model_spec)
  if (weighted){
    wf <- wf %>% add_case_weights(case_weights)
  }
  return (wf)
}
