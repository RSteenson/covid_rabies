# This script extracts individual tables for each figure from the main spreadsheet

rm(list=ls())

# Load libraries
library(dplyr)
library(tidyr)

# Load functions
source("R/recode_countries.R")

# Load spreadsheet
survey_data <- read.csv("data/Frontiers_data_V4.csv", stringsAsFactors = FALSE)

# Trim whitespace on country names
survey_data$country <- trimws(survey_data$country, which="right")

# Recode names to match between data
survey_data <- recode_countries(survey_data, "country")

# Recode endemic status
survey_data$progress.in.rabies.control..1free.2in.progress.3endemic <- ifelse(survey_data$progress.in.rabies.control..1free.2in.progress.3endemic==1, "Free",
                                                                              ifelse(survey_data$progress.in.rabies.control..1free.2in.progress.3endemic==2, "In-progress", "Endemic"))

#----- Figure 1 ----------------------------------------------------------------

#----- Figure 1a

# Subset data
fig_1a <- survey_data %>%
  dplyr::select(country, "endemic_status"=progress.in.rabies.control..1free.2in.progress.3endemic, interview) %>%
  distinct()

# Fix interview status for countries with multiple rows
countries <- sort(unique(fig_1a$country))
for(i in countries){

  # Get row index
  indx = which(fig_1a$country == i)

  # If there are multiple rows, remove the row with "no" in the interview column
  if(length(indx)>1){
    indx_rm = which(fig_1a$country == i & fig_1a$interview == "No")
    fig_1a <- fig_1a[-indx_rm,]
  }
}

# Save output
write.csv(fig_1a, "output/figure_1a.csv", row.names = FALSE)

#----- Figure 1b

# Subset data
fig_1b <- survey_data %>%
  dplyr::select("endemic_status"=progress.in.rabies.control..1free.2in.progress.3endemic, "work_sector"=work.sector) %>%
  group_by(work_sector, endemic_status) %>%
  summarise(n = n())

# Collect totals
fig_1b_totals <- fig_1b %>%
  group_by(work_sector) %>%
  summarise(ord = sum(n)) %>%
  arrange(desc(ord))

# Rearrange data based on totals
fig_1b <- fig_1b %>%
  arrange(factor(work_sector, levels=fig_1b_totals$work_sector))

# Save output
write.csv(fig_1b, "output/figure_1b.csv", row.names = FALSE)

#----- Figure 2 ----------------------------------------------------------------

#----- Figure 2a

# Subset and process data
fig_2a <- survey_data %>%
  dplyr::select(country, "endemic_status"=progress.in.rabies.control..1free.2in.progress.3endemic,
                "disruption.to.mdv"=country.disruption.to.mdv) %>%
  distinct() %>%
  mutate("result" = ifelse(is.na(disruption.to.mdv), "Not available",
                           ifelse(disruption.to.mdv == "No", "No disruption",
                                  ifelse(disruption.to.mdv == "Yes", "Disruption", NA)))) %>%
  group_by(country, endemic_status, result) %>%
  summarise(n = n())

# Fix interview status for countries with multiple rows
countries <- sort(unique(fig_2a$country))
for(i in countries){

  # Get row index
  indx = which(fig_2a$country == i)

  # If there are multiple rows, keep the row with the highest number of responses
  if(length(indx)>1){
    indx_rm = which(fig_2a$country == i & fig_2a$n == min(fig_2a$n[indx]))
    fig_2a <- fig_2a[-indx_rm,]
  }
}

# CHECK
nrow(fig_2a)
table(fig_2a$endemic_status, fig_2a$result)

# Make final summary calculation
fig_2a <- fig_2a %>%
  group_by(endemic_status, result) %>%
  summarise(n=n())

# Save output
write.csv(fig_2a, "output/figure_2a.csv", row.names = FALSE)

#----- Figure 2b

# Subset and process data
fig_2b <- survey_data %>%
  dplyr::select(country, "endemic_status"=progress.in.rabies.control..1free.2in.progress.3endemic,
                "disruption.to.pep"=country.overall.disruption.to.PEP..people.s.health.seeking.behaviour...PEP.delivery.) %>%
  distinct() %>%
  mutate("result" = ifelse(is.na(disruption.to.pep), "Not available",
                           ifelse(disruption.to.pep == "No", "No disruption",
                                  ifelse(disruption.to.pep == "Yes", "Disruption", NA)))) %>%
  group_by(country, endemic_status, result) %>%
  summarise(n = n())
# View(fig_2b[which(fig_2b$endemic_status=="In-progress"),])

# Fix interview status for countries with multiple rows
countries <- sort(unique(fig_2b$country))
for(i in countries){

  # Get row index
  indx = which(fig_2b$country == i)

  # If there are multiple rows, keep the row with the highest number of responses
  if(length(indx)>1){
    indx_rm = which(fig_2b$country == i & fig_2b$n == min(fig_2b$n[indx]))
    fig_2b <- fig_2b[-indx_rm,]
  }
}

# CHECK
nrow(fig_2b)
table(fig_2b$endemic_status, fig_2b$result)

# Make final summary calculation
fig_2b <- fig_2b %>%
  group_by(endemic_status, result) %>%
  summarise(n=n())

# Save output
write.csv(fig_2b, "output/figure_2b.csv", row.names = FALSE)

#----- Figure 2c

# Subset and process data
fig_2c <- survey_data %>%
  dplyr::select(country, "endemic_status"=progress.in.rabies.control..1free.2in.progress.3endemic,
                "disruption.to.awareness.activities"=country.disruption.to.awareness.activities) %>%
  distinct() %>%
  mutate("result" = ifelse(is.na(disruption.to.awareness.activities), "Not available",
                           ifelse(disruption.to.awareness.activities == "No", "No disruption",
                           ifelse(disruption.to.awareness.activities == "Yes", "Disruption", NA)))) %>%
  group_by(country, endemic_status, result) %>%
  summarise(n = n())

# CHECK
nrow(fig_2c)
table(fig_2c$endemic_status, fig_2c$result)

# Make final summary calculation
fig_2c <- fig_2c %>%
  group_by(endemic_status, result) %>%
  summarise(n=n())

# Save output
write.csv(fig_2c, "output/figure_2c.csv", row.names = FALSE)

#----- Figure 3 ----------------------------------------------------------------

#----- Figure 3a (disruption to MDV)

# Subset and process data
fig_3a <- survey_data %>%
  dplyr::select(country, "endemic_status"=progress.in.rabies.control..1free.2in.progress.3endemic,
                no.staff.available, restrictions.on.staff.movement, no.vaccines.available,
                no.consumables.available, difficult.to.adhere.to.COVID.19.guidelines,
                people.afraid.of.leaving.home.gathering, increased.cost.of.organizing,
                other2) %>%
  gather(., question, response, no.staff.available:increased.cost.of.organizing) %>%
  group_by(country, endemic_status, question) %>%
  summarise(n=sum(response)) %>%
  # Remove all zero response
  filter(n > 0) %>%
  # Make all country results equal
  mutate(n = 1) %>%
  # Recode result names
  mutate(grouped_response = ifelse(question == "restrictions.on.staff.movement", "Movement restrictions", # "Restrictions on staff movement",
                                   ifelse(question == "difficult.to.adhere.to.COVID.19.guidelines", "COVID-19 safety", # "Difficult to adhere to COVID-19 guidelines",
                                          ifelse(question == "people.afraid.of.leaving.home.gathering", "Fears", # "People afraid of leaving home/gathering",
                                                 ifelse(question == "no.vaccines.available", "No/limited vaccines", # "No/limited vaccines available",
                                                        ifelse(question == "no.staff.available", "Staff shortages", # "No staff available",
                                                               ifelse(question == "increased.cost.of.organizing", "Increased costs", # "Increased cost of organizing",
                                                                      ifelse(question == "no.consumables.available", "Lacking consumables", NA)))))))) %>% # "No consumables available",
  group_by(endemic_status, grouped_response) %>%
  summarise(n=sum(n))

# Collect totals
fig_3a_totals <- fig_3a %>%
  group_by(grouped_response) %>%
  summarise(ord = sum(n)) %>%
  arrange(desc(ord))

# Rearrange data based on totals
fig_3a <- fig_3a %>%
  arrange(factor(grouped_response, levels=fig_3a_totals$grouped_response))

# Save output
write.csv(fig_3a, "output/figure_3a.csv", row.names = FALSE)

#----- Figure 3b (disruption to access to PEP)

# Subset and process data
fig_3b <- survey_data %>%
  dplyr::select(country, "endemic_status"=progress.in.rabies.control..1free.2in.progress.3endemic,
                people.have.avoided.clinics.due.to.fear.of.COVID.19, people.cannot.afford.travel,
                people.cannot.reach.clinics.because.of.reduced.public.transport,
                people.have.delayed.going.to.clinics, people.have.relied.more.on.local.remedies,
                people.have.used.toll.free.numbers.telemedicine, people.have.interrupted.PEP) %>%
  gather(., question, response, people.have.avoided.clinics.due.to.fear.of.COVID.19:people.have.interrupted.PEP) %>%
  group_by(country, endemic_status, question) %>%
  summarise(n=sum(response)) %>%
  # Remove all zero response
  filter(n > 0) %>%
  # Make all country results equal
  mutate(n = 1) %>%
  # Recode result names
  mutate(grouped_response = ifelse(question == "people.have.avoided.clinics.due.to.fear.of.COVID.19", "Fears", # "People feared COVID-19 in clinics",
                                 ifelse(question == "people.cannot.afford.travel", "Cost", # "People could not afford travel",
                                        ifelse(question == "people.cannot.reach.clinics.because.of.reduced.public.transport", "No public transport", # "People could not use public transportation",
                                               ifelse(question == "people.have.delayed.going.to.clinics", "Delayed", # "People delayed going to clinics",
                                                      ifelse(question == "people.have.relied.more.on.local.remedies", "Used local healers/remedies", # "People relied on local remedies/healers",
                                                             ifelse(question == "people.have.used.toll.free.numbers.telemedicine", "Telemedicine", # "People used toll-free numbers/telemedicine",
                                                                    ifelse(question == "people.have.interrupted.PEP", "Interrupted schedule", NA)))))))) %>% # "People interrupted the vaccination schedule",
  group_by(endemic_status, grouped_response) %>%
  summarise(n=sum(n))

# Collect totals
fig_3b_totals <- fig_3b %>%
  group_by(grouped_response) %>%
  summarise(ord = sum(n)) %>%
  arrange(desc(ord))

# Rearrange data based on totals
fig_3b <- fig_3b %>%
  arrange(factor(grouped_response, levels=fig_3b_totals$grouped_response))

# Save output
write.csv(fig_3b, "output/figure_3b.csv", row.names = FALSE)

#----- Figure 3c (disruption to delivery of PEP)

# Subset and process data
fig_3c <- survey_data %>%
  mutate(vaccines.out.of.stock = ifelse(vaccines.out.of.stock.because.of.financial.constraints == 1 |
                                          vaccines.out.of.stock.because.of.supply.issues == 1,
                                        1, 0),
         staff.less.diligent = ifelse(staff.less.diligent.due.to.stress == 1 |
                                        staff.less.likely.to.recommend.PEP == 1,
                                      1, 0)) %>%
  dplyr::select(country, "endemic_status"=progress.in.rabies.control..1free.2in.progress.3endemic,
                some.many.clinics.closed.converted, staff.redeployed, staff.reduced.due.to.quarantine.illness,
                staff.less.diligent, follow.up.shots.delayed.cancelled, vaccines.out.of.stock,
                vaccines.available.only.in.the.private.sector) %>%
  gather(., question, response, some.many.clinics.closed.converted:vaccines.available.only.in.the.private.sector) %>%
  group_by(country, endemic_status, question) %>%
  summarise(n=sum(response)) %>%
  # Remove all zero response
  filter(n > 0) %>%
  # Make all country results equal
  mutate(n = 1) %>%
  # Recode result names
  mutate(grouped_response = ifelse(question == "some.many.clinics.closed.converted", "Clinics closed", # "Clinics closed/converted",
                                   ifelse(question == "staff.redeployed", "Staff redeployment", # "Staff redeployed",
                                          ifelse(question == "staff.reduced.due.to.quarantine.illness", "Staff absences", # "Staff reduced due to quarantine/illness",
                                                 ifelse(question == "staff.less.diligent", "Staff stress", # "Staff less diligant due to stress/vaccine shortage",
                                                        ifelse(question == "follow.up.shots.delayed.cancelled", "Follow up visits delayed/cancelled", # "Follow up vaccines delayed/cancelled",
                                                               ifelse(question == "vaccines.out.of.stock", "Vaccine shortages", # "No vaccines available",
                                                                      ifelse(question == "vaccines.available.only.in.the.private.sector", "Vaccines only in private sector", NA)))))))) %>% # "Vaccines available only/mostly \nin the private sector",
  group_by(endemic_status, grouped_response) %>%
  summarise(n=sum(n))

# Collect totals
fig_3c_totals <- fig_3c %>%
  group_by(grouped_response) %>%
  summarise(ord = sum(n)) %>%
  arrange(desc(ord))

# Rearrange data based on totals
fig_3c <- fig_3c %>%
  arrange(factor(grouped_response, levels=fig_3c_totals$grouped_response))

# Save output
write.csv(fig_3c, "output/figure_3c.csv", row.names = FALSE)

#----- Figure 3d (disruption to surveillance)

# Subset and process data
fig_3d <- survey_data %>%
  dplyr::select(country, "endemic_status"=progress.in.rabies.control..1free.2in.progress.3endemic,
                no.staff.available.1, restrictions.on.staff.movement.1,
                no.sample.collection.testing.kit.available,
                no.budget, difficult.to.adhere.to.COVID.19.guidelines.1,
                investigators.not.welcome.in.communities, other7) %>%
  gather(., question, response, no.staff.available.1:other7) %>%
  group_by(country, endemic_status, question) %>%
  summarise(n=sum(response)) %>%
  # Remove all zero response
  filter(n > 0) %>%
  # Make all country results equal
  mutate(n = 1) %>%
  # Recode result names
  mutate(grouped_response = ifelse(question == "no.staff.available.1", "Staff shortages", # "No staff available",
                                   ifelse(question == "restrictions.on.staff.movement.1", "Movement restrictions", # "Restrictions on staff movements",
                                          ifelse(question == "no.sample.collection.testing.kit.available", "No consumables/equipment", # "No sample collection/testing kits available",
                                                 ifelse(question == "no.budget", "No budget", # "No budget",
                                                        ifelse(question == "difficult.to.adhere.to.COVID.19.guidelines.1", "COVID-19 safety", # "Difficult to adhere to COVID-19 guidelines",
                                                               ifelse(question == "investigators.not.welcome.in.communities", "Investigators not welcome", # "Investigators not welcome in communities",
                                                                      ifelse(question == "other7", "Other", NA)))))))) %>%
  group_by(endemic_status, grouped_response) %>%
  summarise(n=sum(n))

# Collect totals
fig_3d_totals <- fig_3d %>%
  group_by(grouped_response) %>%
  summarise(ord = sum(n)) %>%
  arrange(desc(ord))

# Rearrange data based on totals
fig_3d <- fig_3d %>%
  arrange(factor(grouped_response, levels=fig_3d_totals$grouped_response))

# Save output
write.csv(fig_3d, "output/figure_3d.csv", row.names = FALSE)

#----- Figure 4 ----------------------------------------------------------------

#----- Figure 4a (changes in free-roaming dog pops and behaviour)

# Subset and process data
fig_4a <- survey_data %>%
  dplyr::select(country, "endemic_status"=progress.in.rabies.control..1free.2in.progress.3endemic,
                more.free.roaming.dogs, fewer.free.roaming.dogs, dogs.more.aggressive,
                dogs.in.poorer.health) %>%
  gather(., question, response, more.free.roaming.dogs:dogs.in.poorer.health) %>%
  group_by(country, endemic_status, question) %>%
  summarise(n=sum(response)) %>%
  # Remove all zero response
  filter(n > 0) %>%
  # Make all country results equal
  mutate(n = 1) %>%
  group_by(endemic_status, question) %>%
  summarise(n=sum(n))

# Recode question responses
fig_4a$question[which(fig_4a$question=="more.free.roaming.dogs")] <- "More dogs" # "More free-roaming dogs"
fig_4a$question[which(fig_4a$question=="fewer.free.roaming.dogs")] <- "Fewer dogs" # "Fewer free-roaming dogs"
fig_4a$question[which(fig_4a$question=="dogs.more.aggressive")] <- "More aggression" # "Dogs more aggressive"
fig_4a$question[which(fig_4a$question=="dogs.in.poorer.health")] <- "Poorer health" # "Dogs in poorer health"

# Collect totals
fig_4a_totals <- fig_4a %>%
  group_by(question) %>%
  summarise(ord = sum(n)) %>%
  arrange(desc(ord))

# Rearrange data based on totals
fig_4a <- fig_4a %>%
  arrange(factor(question, levels=fig_4a_totals$question))

# Save output
write.csv(fig_4a, "output/figure_4a.csv", row.names = FALSE)

#----- Figure 4b (changes in human-dog interactions)

# Subset and process data
fig_4b <- survey_data %>%
  dplyr::select(country, "endemic_status"=progress.in.rabies.control..1free.2in.progress.3endemic,
                more.people.fed.them, people.complained.asked.for.solutions,
                people.removed.killed.them, local.authority.removed.killed.them,
                more.abandonment.due.to.fear.of.COVID.19,
                more.abandonment.due.to.financial.constraints,
                more.abandonment.due.to.other.reasons,) %>%
  gather(., question, response, more.people.fed.them:more.abandonment.due.to.other.reasons) %>%
  group_by(country, endemic_status, question) %>%
  summarise(n=sum(response)) %>%
  # Remove all zero response
  filter(n > 0) %>%
  # Make all country results equal
  mutate(n = 1) %>%
  group_by(endemic_status, question) %>%
  summarise(n=sum(n))

# Recode question responses
fig_4b$question[which(fig_4b$question=="more.people.fed.them")] <- "More feeding" # "More people fed them"
fig_4b$question[which(fig_4b$question=="people.complained.asked.for.solutions")] <- "Complaints/requests" # "People complained/asked for solutions"
fig_4b$question[which(fig_4b$question=="people.removed.killed.them")] <- "Communities removed/killed dogs" # "People removed/killed them"
fig_4b$question[which(fig_4b$question=="local.authority.removed.killed.them")] <- "Officials removed/killed dogs" # "Official workers removed/killed them"
fig_4b$question[which(fig_4b$question=="more.abandonment.due.to.fear.of.COVID.19")] <- "Abandonment (fear)" # "Abandonment due to fear of COVID-19"
fig_4b$question[which(fig_4b$question=="more.abandonment.due.to.financial.constraints")] <- "Abandonment (cost)" # "Abandonment due to financial constraints"
fig_4b$question[which(fig_4b$question=="more.abandonment.due.to.other.reasons")] <- "Abandonment (other)" # "Abandonment due to other reasons"

# Collect totals
fig_4b_totals <- fig_4b %>%
  group_by(question) %>%
  summarise(ord = sum(n)) %>%
  arrange(desc(ord))

# Rearrange data based on totals
fig_4b <- fig_4b %>%
  arrange(factor(question, levels=fig_4b_totals$question))

# Save output
write.csv(fig_4b, "output/figure_4b.csv", row.names = FALSE)

#----- Figure 4c (changes in media reporting of dogs)

# Subset and process data
fig_4c <- survey_data %>%
  dplyr::select(country, "endemic_status"=progress.in.rabies.control..1free.2in.progress.3endemic,
                attacks.on.dogs, attacks.on.other.animals, attacks.on.humans,
                animal.rabies.cases.deaths, human.exposures.deaths, human.cruelty,
                care) %>%
  gather(., question, response, attacks.on.dogs:care) %>%
  group_by(country, endemic_status, question) %>%
  summarise(n=sum(response)) %>%
  # Remove all zero response
  filter(n > 0) %>%
  # Make all country results equal
  mutate(n = 1) %>%
  group_by(endemic_status, question) %>%
  summarise(n=sum(n))

# Recode question responses
fig_4c$question[which(fig_4c$question=="attacks.on.dogs")] <- "More attacks on dogs" # "More attacks on dogs"
fig_4c$question[which(fig_4c$question=="attacks.on.other.animals")] <- "More attacks on livestock" # "More attacks on other animals"
fig_4c$question[which(fig_4c$question=="attacks.on.humans")] <- "More attacks on people" # "More attacks on humans"
fig_4c$question[which(fig_4c$question=="animal.rabies.cases.deaths")] <- "More animal rabies" # "More animal rabies cases"
fig_4c$question[which(fig_4c$question=="human.exposures.deaths")] <- "More human exposures/deaths" # "More human rabies exposures/deaths"
fig_4c$question[which(fig_4c$question=="human.cruelty")] <- "More cruelty" # "More cases of human cruelty towards dogs"
fig_4c$question[which(fig_4c$question=="care")] <- "More care" # "More cases of human care towards dogs"

# Collect totals
fig_4c_totals <- fig_4c %>%
  group_by(question) %>%
  summarise(ord = sum(n)) %>%
  arrange(desc(ord))

# Rearrange data based on totals
fig_4c <- fig_4c %>%
  arrange(factor(question, levels=fig_4c_totals$question))

# Save output
write.csv(fig_4c, "output/figure_4c.csv", row.names = FALSE)
