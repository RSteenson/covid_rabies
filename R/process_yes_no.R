# This script carried out processing of data ready for yes/no mapping

#' _Where countries have multiple responses, treat the whole country as impacted_

#' 7. have financial resources for rabies control/prevention been reduced/diverted elsewhere?
#' 8. was mdv carried out in 2020?
#' 14. have vets experienced higher demand for dog vaccination against rabies?
#' 15. has the pandemic affected animal rabies vaccine production/supply chain?
#' 18. have staff for rabies surveillance been moved elsewhere?
#' 19. has lab capacity been reduced/diverted?
#' 24. has health-seeking behaviour changed?
#' 28. during the pandemic, have dog bites been mentioned in public guidance about circumstances that require immediate medical attention?
#' 30. have rabies awareness campaigns been affected?

rm_multi_response = function(dataframe){
  countries = sort(unique(dataframe$country))
  for(i in 1:length(countries)){
    # Subset rows
    sub_rows = dataframe[which(dataframe$country==countries[i]),]
    # Subset off multiple rows and reduce to one
    if(nrow(sub_rows) >1){
      sub_rows = sub_rows[which(sub_rows[[2]]=="Yes"),]
    }
    # Combine data back together
    if(i==1){
      proc_df = sub_rows
    } else {
      proc_df = rbind(proc_df, sub_rows)
    }
  }
  return(proc_df)
}

#----- 7. Have financial resources for rabies been reduced/diverted? -----------
rabies_budget_diverted = survey_data %>%
  group_by(country, "impacted"=rabies.budget.reduced.diverted) %>%
  tally()

# Use function to remove multiple responses from the same country
rabies_budget_diverted_proc = rm_multi_response(rabies_budget_diverted)

# Add formatting for map plot
rabies_budget_diverted_proc = rabies_budget_diverted_proc %>%
  mutate(col = ifelse(is.na(impacted), "none",
                      ifelse(impacted == "Yes", "cross-sectoral", "empty")),
         shap = ifelse(is.na(impacted), "cross", "circle")) %>%
  dplyr::select(country, col, shap) %>%
  unique()

# Merge into centroid data
budget_divert_centroids = map_centroids %>%
  merge(., rabies_budget_diverted_proc, by.x="CNTRY_TERR", by.y="country", all.x=TRUE) %>%
  filter(!is.na(col))

#----- 8. Was MDV carried out? -------------------------------------------------
mdv_carried_out = mdv_happened_2020 %>%
  mutate(impacted = ifelse(question %in% c("yes.as.planned", "other1"), "No",
                                  ifelse(question %in% c("yes.but.delayed.took.longer", "yes.but.below.target", "no.interrupted.early", "no.not.started"), "Yes", NA))) %>%
  group_by(country, impacted) %>%
  tally()

# Use function to remove multiple responses from the same country
mdv_carried_out_proc = rm_multi_response(mdv_carried_out)

# Add formatting for map plot
mdv_carried_out_proc = mdv_carried_out_proc %>%
  mutate(col = ifelse(is.na(impacted), "none",
                      ifelse(impacted == "Yes", "vet", "empty")),
         shap = ifelse(is.na(impacted), "cross", "circle")) %>%
  dplyr::select(country, col, shap) %>%
  unique()

# Merge into centroid data
mdv_centroids = map_centroids %>%
  merge(., mdv_carried_out_proc, by.x="CNTRY_TERR", by.y="country", all.x=TRUE) %>%
  filter(!is.na(col))

#----- 14. Have vets experienced higher demand for ARV? ------------------------
increased_arv_demand = survey_data %>%
  group_by(country, "impacted"=increased.demand.for.arv.to.vets) %>%
  tally()

# Use function to remove multiple responses from the same country
increased_arv_demand_proc = rm_multi_response(increased_arv_demand)

# Add formatting for map plot
increased_arv_demand_proc = increased_arv_demand_proc %>%
  mutate(col = ifelse(is.na(impacted), "none",
                      ifelse(impacted == "Yes", "vet", "empty")),
         shap = ifelse(is.na(impacted), "cross", "circle")) %>%
  dplyr::select(country, col, shap) %>%
  unique()

# Merge into centroid data
increased_arv_demand_centroids = map_centroids %>%
  merge(., increased_arv_demand_proc, by.x="CNTRY_TERR", by.y="country", all.x=TRUE) %>%
  filter(!is.na(col))

#----- 15. Has the pandemic affected ARV production/supply chain? --------------
arv_supply = impact_on_vaccine %>%
  mutate(impacted = ifelse(question == "none1", "No",
                           ifelse(question=="NA4", NA, "Yes"))) %>%
  group_by(country, impacted) %>%
  tally()

# Use function to remove multiple responses from the same country
arv_supply_proc = rm_multi_response(arv_supply)

# Add formatting for map plot
arv_supply_proc = arv_supply_proc %>%
  mutate(col = ifelse(is.na(impacted), "none",
                      ifelse(impacted == "Yes", "vet", "empty")),
         shap = ifelse(is.na(impacted), "cross", "circle")) %>%
  dplyr::select(country, col, shap) %>%
  unique()

# Merge into centroid data
arv_supply_centroids = map_centroids %>%
  merge(., arv_supply_proc, by.x="CNTRY_TERR", by.y="country", all.x=TRUE) %>%
  filter(!is.na(col))

#----- 18. Have staff for rabies surveillance been reallocated? ----------------
staff_redeployed = survey_data %>%
  group_by(country, "impacted"=staff.redeployed) %>%
  mutate(impacted = ifelse(impacted == 1, "Yes", "No")) %>%
  tally()

# Use function to remove multiple responses from the same country
staff_redeployed_proc = rm_multi_response(staff_redeployed)

# Add formatting for map plot
staff_redeployed_proc = staff_redeployed_proc %>%
  mutate(col = ifelse(is.na(impacted), "none",
                      ifelse(impacted == "Yes", "surveillance", "empty")),
         shap = ifelse(is.na(impacted), "cross", "circle")) %>%
  dplyr::select(country, col, shap) %>%
  unique()

# Merge into centroid data
staff_redeployed_centroids = map_centroids %>%
  merge(., staff_redeployed_proc, by.x="CNTRY_TERR", by.y="country", all.x=TRUE) %>%
  filter(!is.na(col))

#----- 19. Has lab capacity been reduced/diverted? ----------------------------
lab_capacity = survey_data %>%
  group_by(country, "impacted"=lab.capacity.diverted.reduced) %>%
  tally()

# Use function to remove multiple responses from the same country
lab_capacity_proc = rm_multi_response(lab_capacity)

# Add formatting for map plot
lab_capacity_proc = lab_capacity_proc %>%
  mutate(col = ifelse(is.na(impacted), "none",
                      ifelse(impacted == "Yes", "surveillance", "empty")),
         shap = ifelse(is.na(impacted), "cross", "circle")) %>%
  dplyr::select(country, col, shap) %>%
  unique()

# Merge into centroid data
lab_capacity_centroids = map_centroids %>%
  merge(., lab_capacity_proc, by.x="CNTRY_TERR", by.y="country", all.x=TRUE) %>%
  filter(!is.na(col))

#----- 24. Has health seeking behaviour changed? -------------------------------
change_health_seeking = change_in_health_seeking %>%
  mutate(impacted = ifelse(question %in% c("people.still.attending.clinics", "other8"), "No",
                           ifelse(question == "NA8", NA, "Yes"))) %>%
  group_by(country, impacted) %>%
  tally()

# Use function to remove multiple responses from the same country
change_health_seeking_proc = rm_multi_response(change_health_seeking)

# Add formatting for map plot
change_health_seeking_proc = change_health_seeking_proc %>%
  mutate(col = ifelse(is.na(impacted), "none",
                      ifelse(impacted == "Yes", "health", "empty")),
         shap = ifelse(is.na(impacted), "cross", "circle")) %>%
  dplyr::select(country, col, shap) %>%
  unique()

# Merge into centroid data
health_seeking_centroids = map_centroids %>%
  merge(., change_health_seeking_proc, by.x="CNTRY_TERR", by.y="country", all.x=TRUE) %>%
  filter(!is.na(col))

#----- 28. Have dog bites been mentioned in public guidance during pandemic? ----
dog_bite_guidance = survey_data %>%
  group_by(country, "impacted"=dog.bites.in.medical.emergency.guidelines) %>%
  tally()

# Use function to remove multiple responses from the same country
dog_bite_guidance_proc = rm_multi_response(dog_bite_guidance)

# Add formatting for map plot
dog_bite_guidance_proc = dog_bite_guidance_proc %>%
  mutate(col = ifelse(is.na(impacted), "none",
                      ifelse(impacted == "Yes", "health", "empty")),
         shap = ifelse(is.na(impacted), "cross", "circle")) %>%
  dplyr::select(country, col, shap) %>%
  unique()

# Merge into centroid data
dog_bite_guidance_centroids = map_centroids %>%
  merge(., dog_bite_guidance_proc, by.x="CNTRY_TERR", by.y="country", all.x=TRUE) %>%
  filter(!is.na(col))

#----- 30. Were WRD events affected? -------------------------------------------
wrd_impact = survey_data %>%
  mutate(impacted = ifelse(impact.on.wrd.2020.events %in% c("No, many in-person events were moved online", "No, many new online events were organized"), "No",
                           ifelse(impact.on.wrd.2020.events == "Other" & more.on.impact.on.wrd.2020.events %in% c("The WRD event was held online and it was successful. The pandemic, in a way, engendered the attendance of several HCWs since it was online. ",
                                                                                                                 "Still did a campaign and online event",
                                                                                                                 "This was done virtually",
                                                                                                                  "Affected positively. More collaboration, innovative online outreach /enlightenment"), "No", "Yes"))) %>%
  group_by(country, impacted) %>%
  tally()

# Use function to remove multiple responses from the same country
wrd_impact_proc = rm_multi_response(wrd_impact)

# Add formatting for map plot
wrd_impact_proc = wrd_impact_proc %>%
  mutate(col = ifelse(is.na(impacted), "none",
                      ifelse(impacted == "Yes", "cross-sectoral", "empty")),
         shap = ifelse(is.na(impacted), "cross", "circle")) %>%
  dplyr::select(country, col, shap) %>%
  unique()

# Merge into centroid data
wrd_impact_centroids = map_centroids %>%
  merge(., wrd_impact_proc, by.x="CNTRY_TERR", by.y="country", all.x=TRUE) %>%
  filter(!is.na(col))

#----- Combined surveillance staff redeployment & reduced lab capacity ---------

combined_surv = staff_redeployed %>%
  merge(., lab_capacity, by="country") %>%
  group_by(country) %>%
  mutate("impacted" = ifelse(impacted.x=="Yes" | impacted.y=="Yes", "Yes", "No"),
         "n" = max(c(n.x, n.y))) %>%
  dplyr:: select(country, impacted, n)

# Use function to remove multiple responses from the same country
combined_surv_proc = rm_multi_response(combined_surv)

# Add formatting for map plot
combined_surv_proc = combined_surv_proc %>%
  mutate(col = ifelse(is.na(impacted), "none",
                      ifelse(impacted == "Yes", "surveillance", "empty")),
         shap = ifelse(is.na(impacted), "cross", "circle")) %>%
  dplyr::select(country, col, shap) %>%
  unique()

# Merge into centroid data
combined_surv_centroids = map_centroids %>%
  merge(., combined_surv_proc, by.x="CNTRY_TERR", by.y="country", all.x=TRUE) %>%
  filter(!is.na(col))
