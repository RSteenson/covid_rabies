rm(list=ls())

# Load libraries
library(dplyr)
library(sf)
library(ggplot2)
library(tidyverse)
devtools::install_github("dcooley/sfheaders")
library(sfheaders)
library(ggpubr)
library(ggnewscale)

options(stringsAsFactors=FALSE,
        dplyr.summarise.inform = FALSE)

# Load data
survey_data = read.csv("data/Frontiers_data.csv")

# Set colour palettes
col_pal = c("1"="#FFDD54", "2"="#5476ff", "3"="#ff8854", "4"="dimgrey")

#----- Produce sample barchart -------------------------------------------------

# Sample barchart: impact on animal vaccine production/supply chains
# Columns:  yes.production.has.considerably.reduced
#           yes.production.has.reduced
#           yes.importation.delayed
#           yes.importation.reduced
#           yes.distribution.affected
#           other4
AV_impacts = survey_data %>%
  dplyr::select(observed.country, progress, yes.production.has.considerably.reduced,
                yes.production.has.reduced, yes.importation.delayed,
                yes.importation.reduced, yes.distribution.affected, other4) %>%
  gather(question, count, yes.production.has.considerably.reduced:other4) %>%
  mutate(question = ifelse(question %in% c("yes.production.has.considerably.reduced", "yes.production.has.reduced"), "Production reduced",
                                  ifelse(question == "yes.importation.delayed", "Importation delayed",
                                         ifelse(question == "yes.importation.reduced", "Imortation reduced",
                                                ifelse(question == "yes.distribution.affected", "Distribution affected", "Other")))))
AV_impacts_responses = AV_impacts %>%
  group_by(question) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count)) %>%
  filter(count>0)
AV_impacts_plot = AV_impacts %>%
  filter(question %in% AV_impacts_responses$question) %>%
  group_by(progress, question) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count))
AV_impacts_plot$progress = factor(AV_impacts_plot$progress, levels=c(1,2,3,4))
AV_impacts_plot$question = factor(AV_impacts_plot$question, levels=unique(AV_impacts_responses$question))
ggplot(data=AV_impacts_plot, aes(x=question, y=count, fill=progress)) +
  geom_col() +
  scale_fill_manual(values=col_pal, labels=c("Rabies free", "Controlled", "Endemic", "Endemic with infrastructural challenges ")) +
  labs(x="", y="Survey responses") +
  theme_classic() +
  theme(legend.title = element_blank(), legend.position = "top",
        axis.text.x = element_text(angle=45, hjust=1))
ggsave("figs/Frontiers/Animal_vaccine_impacts.pdf", height=4, width=7)

#----- Summarise single-choice questions ---------------------------------------

# Country of research application
country_plot = survey_data %>%
  dplyr::select(observed.country, progress) %>%
  group_by(progress, observed.country) %>%
  summarise(count = n()) %>%
  # arrange(progress, desc(count)) %>%
  mutate(question = "Country of reference") %>%
  dplyr::select(question, "options" = observed.country, progress, count) %>%
  spread(key=progress, value=count)

# Work Sector
work_sector_plot = survey_data %>%
  dplyr::select(observed.country, progress, work.sector) %>%
  group_by(progress, work.sector) %>%
  summarise(count = n()) %>%
  # arrange(progress, desc(count)) %>%
  mutate(question = "Work Sector") %>%
  dplyr::select(question, "options" = work.sector, progress, count) %>%
  spread(progress, count)

# Diversion of Rabies funding
fund_diversion_plot = survey_data %>%
  dplyr::select(observed.country, progress, rabies.budget.reduced.diverted) %>%
  group_by(progress, rabies.budget.reduced.diverted) %>%
  summarise(count = n()) %>%
  # arrange(desc(count)) %>%
  mutate(question = "Were funds reduced/diverted?") %>%
  dplyr::select(question, "options" = rabies.budget.reduced.diverted, progress, count) %>%
  spread(progress, count)
fund_diversion_plot$options = factor(fund_diversion_plot$options, levels=c("Yes", "No", NA))
fund_diversion_plot = arrange(fund_diversion_plot, options)

# Have staff for rabies surveillance been moved elsewhere?
surveillance_staff_plot = survey_data %>%
  dplyr::select(observed.country, progress, staff.for.rabies.surveillance.redeployed.assigned.to.other.tasks) %>%
  group_by(progress, staff.for.rabies.surveillance.redeployed.assigned.to.other.tasks) %>%
  summarise(count = n()) %>%
  # arrange(desc(count)) %>%
  mutate(question = "Were surveillance staff moved elsewhere?") %>%
  dplyr::select(question, "options" = staff.for.rabies.surveillance.redeployed.assigned.to.other.tasks, progress, count) %>%
  spread(progress, count)
surveillance_staff_plot$options = factor(surveillance_staff_plot$options, levels=c("Yes", "No", NA))
surveillance_staff_plot = arrange(surveillance_staff_plot, options)

# Have staff for rabies surveillance been moved elsewhere?
lab_capacity_plot = survey_data %>%
  dplyr::select(observed.country, progress, lab.capacity.diverted.reduced) %>%
  group_by(progress, lab.capacity.diverted.reduced) %>%
  summarise(count = n()) %>%
  # arrange(desc(count)) %>%
  mutate(question = "Was lab capacity been reduced/diverted?") %>%
  dplyr::select(question, "options" = lab.capacity.diverted.reduced, progress, count) %>%
  spread(progress, count)
lab_capacity_plot$options = factor(lab_capacity_plot$options, levels=c("Yes", "No", NA))
lab_capacity_plot = arrange(lab_capacity_plot, options)

# Were WRD 2020 events affected?
wrd_events_plot = survey_data %>%
  dplyr::select(observed.country, progress, impact.on.wrd.2020.events) %>%
  group_by(progress, impact.on.wrd.2020.events) %>%
  summarise(count = n()) %>%
  # arrange(desc(count)) %>%
  mutate(question = "Were WRD 2020 events affected?") %>%
  dplyr::select(question, "options" = impact.on.wrd.2020.events, progress, count) %>%
  spread(progress, count)

# Were rabies awareness events affected?
awareness_events_plot = survey_data %>%
  dplyr::select(observed.country, progress, impact.on.awareness.campaigns.for.children) %>%
  group_by(progress, impact.on.awareness.campaigns.for.children) %>%
  summarise(count = n()) %>%
  # arrange(desc(count)) %>%
  mutate(question = "Were rabies awareness events affected?") %>%
  dplyr::select(question, "options" = impact.on.awareness.campaigns.for.children, progress, count) %>%
  spread(progress, count)

# Was the expected 2020/2021 national report on rabies issued?
national_report_plot = survey_data %>%
  dplyr::select(observed.country, progress, X2020.national.report.happened) %>%
  group_by(progress, X2020.national.report.happened) %>%
  summarise(count = n()) %>%
  # arrange(desc(count)) %>%
  mutate(question = "Was national rabies report issued?") %>%
  dplyr::select(question, "options" = X2020.national.report.happened, progress, count) %>%
  spread(progress, count)
national_report_plot$options = factor(national_report_plot$options, levels=c("Yes", "No", NA))
national_report_plot = arrange(national_report_plot, options)

# Was any expected important rabies-related milestone reached?
rabies_milestone_plot = survey_data %>%
  dplyr::select(observed.country, progress, X2020.milestone.happened) %>%
  group_by(progress, X2020.milestone.happened) %>%
  summarise(count = n()) %>%
  # arrange(desc(count)) %>%
  mutate(question = "Was any expected important rabies-related milestone reached?") %>%
  dplyr::select(question, "options" = X2020.milestone.happened, progress, count) %>%
  spread(progress, count)
rabies_milestone_plot$options = factor(rabies_milestone_plot$options, levels=c("Yes", "No", NA))
rabies_milestone_plot = arrange(rabies_milestone_plot, options)

# What was the most disrupted sector?
disrupted_sector_plot = survey_data %>%
  dplyr::select(observed.country, progress, most.disrupted.sector) %>%
  group_by(progress, most.disrupted.sector) %>%
  summarise(count = n()) %>%
  # arrange(desc(count)) %>%
  mutate(question = "What was the most impacred sector?") %>%
  dplyr::select(question, "options" = most.disrupted.sector, progress, count) %>%
  spread(progress, count)

#----- Summarise multi-choice questions ----------------------------------------

# Did any mass dog vaccinations organised by the government/NGOs happen in 2020?
mdv_in_2020_plot = survey_data %>%
  dplyr::select(observed.country, progress, yes.as.planned,
                yes.but.delayed.took.longer, yes.but.below.target,
                yes.but.interrupted.early, no.not.even.started, other1, NA1) %>%
  gather(key="options", value="n", yes.as.planned:NA1) %>%
  group_by(progress, options) %>%
  summarise(count = sum(n)) %>%
  # arrange(progress, desc(count)) %>%
  mutate(question = "Did any MDVs happen in 2020?") %>%
  dplyr::select(question, options, progress, count) %>%
  spread(key=progress, value=count)
mdv_in_2020_plot$options = factor(mdv_in_2020_plot$options,
                                  levels=c("yes.as.planned", "yes.but.delayed.took.longer",
                                           "yes.but.below.target", "yes.but.interrupted.early",
                                           "no.not.even.started", "other1", "NA1"))
mdv_in_2020_plot = arrange(mdv_in_2020_plot, options)

# If mass dog vaccinations were affected, what were the main reasons for disruption?
mdv_disruption_plot = survey_data %>%
  dplyr::select(observed.country, progress, no.staff.available,
                restrictions.on.staff.movement, no.vaccines.available,
                no.consumables.available, difficult.to.adhere.to.COVID.19.guidelines,
                people.afraid.of.leaving.home.gathering, increased.cost.of.organizing,
                other2, NA2) %>%
  gather(key="options", value="n", no.staff.available:NA2) %>%
  group_by(progress, options) %>%
  summarise(count = sum(n)) %>%
  # arrange(progress, desc(count)) %>%
  mutate(question = "What were the main reason for disruption of MDV?") %>%
  dplyr::select(question, options, progress, count) %>%
  spread(key=progress, value=count)
mdv_disruption_plot$options = factor(mdv_disruption_plot$options,
                                  levels=c("no.staff.available", "restrictions.on.staff.movement",
                                           "no.vaccines.available", "no.consumables.available",
                                           "difficult.to.adhere.to.COVID.19.guidelines",
                                           "people.afraid.of.leaving.home.gathering", "increased.cost.of.organizing",
                                           "other2", "NA2"))
mdv_disruption_plot = arrange(mdv_disruption_plot, options)

# If mass dog vaccinations were carried out, what method(s) were used?
mdv_methods_plot = survey_data %>%
  dplyr::select(observed.country, progress, central.static.point, mobile.point,
                catch.vaccinate.release, door.to.door, oral, other3, NA3) %>%
  gather(key="options", value="n", central.static.point:NA3) %>%
  group_by(progress, options) %>%
  summarise(count = sum(n)) %>%
  # arrange(progress, desc(count)) %>%
  mutate(question = "What methods of MDV were used?") %>%
  dplyr::select(question, options, progress, count) %>%
  spread(key=progress, value=count)
mdv_methods_plot$options = factor(mdv_methods_plot$options,
                                     levels=c("central.static.point", "mobile.point",
                                              "catch.vaccinate.release", "door.to.door",
                                              "oral", "other3", "NA3"))
mdv_methods_plot = arrange(mdv_methods_plot, options)

# Has the pandemic affected animal rabies vaccine production/supply chains?
animal_vaccine_plot = survey_data %>%
  dplyr::select(observed.country, progress, yes.production.has.considerably.reduced,
                yes.production.has.reduced, yes.importation.delayed,
                yes.importation.reduced, yes.distribution.affected, other4, NA4) %>%
  gather(key="options", value="n", yes.production.has.considerably.reduced:NA4) %>%
  group_by(progress, options) %>%
  summarise(count = sum(n)) %>%
  # arrange(progress, desc(count)) %>%
  mutate(question = "What methods of MDV were used?") %>%
  dplyr::select(question, options, progress, count) %>%
  spread(key=progress, value=count)
animal_vaccine_plot$options = factor(animal_vaccine_plot$options,
                                  levels=c("yes.production.has.considerably.reduced",
                                           "yes.production.has.reduced", "yes.importation.delayed",
                                           "yes.importation.reduced", "yes.distribution.affected",
                                           "other4", "NA4"))
animal_vaccine_plot = arrange(animal_vaccine_plot, options)

# What (if any) changes have been observed in dog behaviour/populations over the pandemic?
dog_behaviour_plot = survey_data %>%
  dplyr::select(observed.country, progress, more.free.roaming.dogs,
                fewer.free.roaming.dogs, dogs.more.aggressive, dogs.in.poorer.health,
                no.changes1, other5, NA5) %>%
  gather(key="options", value="n", more.free.roaming.dogs:NA5) %>%
  group_by(progress, options) %>%
  summarise(count = sum(n)) %>%
  # arrange(progress, desc(count)) %>%
  mutate(question = "What dog behaviour/population changes have occured?") %>%
  dplyr::select(question, options, progress, count) %>%
  spread(key=progress, value=count)
dog_behaviour_plot$options = factor(dog_behaviour_plot$options,
                                     levels=c("more.free.roaming.dogs", "fewer.free.roaming.dogs",
                                              "dogs.more.aggressive", "dogs.in.poorer.health",
                                              "no.changes1", "other5", "NA5"))
dog_behaviour_plot = arrange(dog_behaviour_plot, options)

# What (if any) changes have been observed in free-roaming dogs over the pandemic?
dog_free_roam_plot = survey_data %>%
  dplyr::select(observed.country, progress, more.people.fed.them,
                people.complained.asked.for.solutions..removal..killing..etc..,
                people.removed.killed.them, local.authority.removed.killed.them,
                more.abandonment.due.to.fear.of.covid, more.abandonment.due.to.financial.constraints,
                more.abandonment.for.other.reasons, no.changes2, other6, NA6) %>%
  gather(key="options", value="n", more.people.fed.them:NA6) %>%
  group_by(progress, options) %>%
  summarise(count = sum(n)) %>%
  # arrange(progress, desc(count)) %>%
  mutate(question = "What changes in interactions with free-roaming dogs have occured?") %>%
  dplyr::select(question, options, progress, count) %>%
  spread(key=progress, value=count)
dog_free_roam_plot$options = factor(dog_free_roam_plot$options,
                                    levels=c("more.people.fed.them",
                                             "people.complained.asked.for.solutions..removal..killing..etc..",
                                             "people.removed.killed.them", "local.authority.removed.killed.them",
                                             "more.abandonment.due.to.fear.of.covid", "more.abandonment.due.to.financial.constraints",
                                             "more.abandonment.for.other.reasons", "no.changes2", "other6", "NA6"))
dog_free_roam_plot = arrange(dog_free_roam_plot, options)

# Has capacity for investigating/reporting cases/exposures/deaths been disrupted?
investigation_disrupted_plot = survey_data %>%
  dplyr::select(observed.country, progress, no.staff.available.1,
                restrictions.on.staff.movement.1, no.sample.collection.testing.kit.available,
                no.budget, difficult.to.adhere.to.COVID.19.guidelines.1,
                investigators.not.welcomed.in.communities, other7, NA7) %>%
  gather(key="options", value="n", no.staff.available.1:NA7) %>%
  group_by(progress, options) %>%
  summarise(count = sum(n)) %>%
  # arrange(progress, desc(count)) %>%
  mutate(question = "For what reasons has investigating/reporting been disrupted?") %>%
  dplyr::select(question, options, progress, count) %>%
  spread(key=progress, value=count)
investigation_disrupted_plot$options = factor(investigation_disrupted_plot$options,
                                    levels=c("no.staff.available.1",
                                             "restrictions.on.staff.movement.1", "no.sample.collection.testing.kit.available",
                                             "no.budget", "difficult.to.adhere.to.COVID.19.guidelines.1",
                                             "investigators.not.welcomed.in.communities", "other7", "NA7"))
investigation_disrupted_plot = arrange(investigation_disrupted_plot, options)

# What changes have been observed in health-seeking behaviour?
health_seeking_plot = survey_data %>%
  dplyr::select(observed.country, progress, people.have.avoided.clinics.due.to.fear.of.COVID.19,
                people.cannot.afford.travel, people.cannot.reach.clinics.because.of.reduced.public.transport,
                people.have.delayed.going.to.clinics, people.have.interrupted.PEP,
                people.have.relied.more.on.local.remedies, people.have.used.toll.free.numbers.telemedicine,
                wound.washing.has.increased, no.changes3, other8, NA8) %>%
  gather(key="options", value="n", people.have.avoided.clinics.due.to.fear.of.COVID.19:NA8) %>%
  group_by(progress, options) %>%
  summarise(count = sum(n)) %>%
  # arrange(progress, desc(count)) %>%
  mutate(question = "What changes have occured in health-seeking behaviour?") %>%
  dplyr::select(question, options, progress, count) %>%
  spread(key=progress, value=count)
health_seeking_plot$options = factor(health_seeking_plot$options,
                                              levels=c("people.have.avoided.clinics.due.to.fear.of.COVID.19",
                                                       "people.cannot.afford.travel", "people.cannot.reach.clinics.because.of.reduced.public.transport",
                                                       "people.have.delayed.going.to.clinics", "people.have.interrupted.PEP",
                                                       "people.have.relied.more.on.local.remedies", "people.have.used.toll.free.numbers.telemedicine",
                                                       "wound.washing.has.increased", "no.changes3", "other8", "NA8"))
health_seeking_plot = arrange(health_seeking_plot, options)

# What changes have been observed in PEP delivery?
pep_delivery_plot = survey_data %>%
  dplyr::select(observed.country, progress, some.many.clinics.closed.converted,
                staff.redeployed, staff.reduced.due.to.quarantine.illness,
                staff.less.diligent.due.to.stress, staff.less.likely.to.recommend.PEP,
                follow.up.shots.delayed.cancelled, vaccines.out.of.stock.because.of.financial.constraints,
                vaccines.out.of.stock.because.of.supply.issues, vaccines.available.only.in.the.private.sector,
                consumables.not.available, RIG.not.available, no.changes4, other9, NA9) %>%
  gather(key="options", value="n", some.many.clinics.closed.converted:NA9) %>%
  group_by(progress, options) %>%
  summarise(count = sum(n)) %>%
  # arrange(progress, desc(count)) %>%
  mutate(question = "What changes have occured in PEP delivery?") %>%
  dplyr::select(question, options, progress, count) %>%
  spread(key=progress, value=count)
pep_delivery_plot$options = factor(pep_delivery_plot$options,
                                     levels=c("some.many.clinics.closed.converted",
                                              "staff.redeployed", "staff.reduced.due.to.quarantine.illness",
                                              "staff.less.diligent.due.to.stress", "staff.less.likely.to.recommend.PEP",
                                              "follow.up.shots.delayed.cancelled", "vaccines.out.of.stock.because.of.financial.constraints",
                                              "vaccines.out.of.stock.because.of.supply.issues", "vaccines.available.only.in.the.private.sector",
                                              "consumables.not.available", "RIG.not.available", "no.changes4", "other9", "NA9"))
pep_delivery_plot = arrange(pep_delivery_plot, options)

# What changes have been observed in media reporting of FRDs or rabies?
media_plot = survey_data %>%
  dplyr::select(observed.country, progress, attacks.on.dogs, attacks.on.other.animals,
                attacks.on.humans, animal.rabies.cases.deaths, human.exposures.deaths,
                human.cruelty, care, no.changes5, other10, NA10) %>%
  gather(key="options", value="n", attacks.on.dogs:NA10) %>%
  group_by(progress, options) %>%
  summarise(count = sum(n)) %>%
  # arrange(progress, desc(count)) %>%
  mutate(question = "What changes have been observed in media reporting of FRDs/rabies?") %>%
  dplyr::select(question, options, progress, count) %>%
  spread(key=progress, value=count)
media_plot$options = factor(media_plot$options,
                                   levels=c("attacks.on.dogs", "attacks.on.other.animals",
                                            "attacks.on.humans", "animal.rabies.cases.deaths", "human.exposures.deaths",
                                            "human.cruelty", "care", "no.changes5", "other10", "NA10"))
media_plot = arrange(media_plot, options)



#----- Bind all data together --------------------------------------------------

question_summaries = bind_rows(
  country_plot,
  work_sector_plot,
  fund_diversion_plot,
  mdv_in_2020_plot,
  mdv_disruption_plot,
  mdv_methods_plot,
  animal_vaccine_plot,
  dog_behaviour_plot,
  dog_free_roam_plot,
  surveillance_staff_plot,
  lab_capacity_plot,
  investigation_disrupted_plot,
  health_seeking_plot,
  pep_delivery_plot,
  wrd_events_plot,
  awareness_events_plot,
  media_plot,
  national_report_plot,
  rabies_milestone_plot,
  disrupted_sector_plot)

# Save output
write.csv(question_summaries, "output/Frontiers/summary_stats.csv", row.names=FALSE)
