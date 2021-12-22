# This script extracts individual tables for each figure from the main spreadsheet

rm(list=ls())

# Load libraries
library(dplyr)
library(tidyr)

# Load functions
source("R/recode_countries.R")

# Load spreadsheet
survey_data <- read.csv("data/Frontiers_data.csv", stringsAsFactors = FALSE)

# Trim whitespace on country names
survey_data$country <- trimws(survey_data$country, which="right")

# Recode names to match between data
survey_data <- recode_countries(survey_data, "country")

# Recode endemic status
survey_data$progress.in.rabies.control..1free.2in.progress.3endemic <- ifelse(survey_data$progress.in.rabies.control..1free.2in.progress.3endemic==1, "Rabies-free",
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

# Save output
write.csv(fig_1b, "output/figure_1b.csv", row.names = FALSE)

#----- Figure 2 ----------------------------------------------------------------

#----- Figure 2a

# Subset and process data
fig_2a <- survey_data %>%
  dplyr::select(country, "endemic_status"=progress.in.rabies.control..1free.2in.progress.3endemic, disruption.to.mdv) %>%
                # yes.as.planned, yes.but.delayed.took.longer, yes.but.below.target,
                # yes.but.interrupted.early, no.not.even.started, NA1
  # gather(., question, response, yes.as.planned:NA1) %>%
  # filter(response == 1) %>%
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
                some.many.clinics.closed.converted, staff.redeployed, staff.reduced.due.to.quarantine.illness,
                staff.less.diligent.due.to.stress, staff.less.likely.to.recommend.PEP, follow.up.shots.delayed.cancelled,
                vaccines.out.of.stock.because.of.financial.constraints, vaccines.out.of.stock.because.of.supply.issues,
                vaccines.available.only.in.the.private.sector, consumables.not.available, RIG.not.available,
                no.changes4, NA9) %>%
  # gather(., question, response, some.many.clinics.closed.converted:NA9) %>%
  # filter(response == 1) %>%
  # mutate("result" = ifelse(question == "no.changes4", "No disruption",
  #                          ifelse(question == "NA9", "Not available", "Disruption"))) %>%
  mutate("result" = ifelse(some.many.clinics.closed.converted == 1 |
                             staff.redeployed == 1 |
                             staff.reduced.due.to.quarantine.illness == 1 |
                             staff.less.diligent.due.to.stress == 1 |
                             staff.less.likely.to.recommend.PEP == 1 |
                             follow.up.shots.delayed.cancelled == 1 |
                             vaccines.out.of.stock.because.of.financial.constraints == 1 |
                             vaccines.out.of.stock.because.of.supply.issues == 1 |
                             vaccines.available.only.in.the.private.sector == 1 |
                             consumables.not.available == 1 |
                             RIG.not.available == 1, "Disruption",
                           ifelse(no.changes4 == 1, "No disruption", "Not available"))) %>%
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
table(fig_2b$result)

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
                disruption.to.awareness.activities) %>%
  mutate("result" = ifelse(is.na(disruption.to.awareness.activities), "Not available",
                           ifelse(disruption.to.awareness.activities == "No", "No disruption",
                           ifelse(disruption.to.awareness.activities == "Yes", "Disruption", NA)))) %>%
  group_by(country, endemic_status, result) %>%
  summarise(n = n())

# CHECK
nrow(fig_2c)
table(fig_2c$result, useNA="always")

# Make final summary calculation
fig_2c <- fig_2c %>%
  group_by(endemic_status, result) %>%
  summarise(n=n())

# Save output
write.csv(fig_2c, "output/figure_2c.csv", row.names = FALSE)
