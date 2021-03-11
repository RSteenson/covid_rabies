rm(list=ls())

# Load libraries
library(dplyr)
library(sf)
library(ggplot2)

options(stringsAsFactors=FALSE,
        dplyr.summarise.inform = FALSE)

# DATA:
# - The "survey questions" file with each Q and Q type (multi choice, single
#   choice, etc) on a row.
# - The new "survey responses with squeezed Taiwan and Bhutan" file. Sorry for
#   the silly name of this file. As Katie said, here I condensed the 13 responses
#   from Taiwan into 1 and I did the same for the 5 responses from Bhutan.
#   Please consider only the "clean" tab here.
# - The "country data" file, which contains demographic and epi data for all the
#   surveyed countries. As of now, the countries are ordered by the score of
#   their latest SARE. Unfortunately, many countries have no score here because
#   SARE was never done or, in the case of the Western hemisphere, countries
#   there don't use SARE. Katie, I have asked Ryan about this and he said that
#   PAHO was trying to develop a tool for these countries, which considers bat
#   rabies too, but they haven't come up with anything yet.

#' *p.n. 44 and 82 for Q2 Reason for MDV disruption had no answer, edited to be NA in data*
#' *p.n. 36, 72 and 73 for Q3 Method of MDV had no answer, edited to be NA in data*
#' *p.n. 63 for Q9 changes in PEP had no answer, edited to be NA in data*

# Load data
country_data = read.csv("data/Country_data.csv")
survey_data = read.csv("data/Survey_data.csv")

# Load shapefile
map.world <- map_data("world")

# Create colour palette
col_pal = c("Endemic"="firebrick2", "Controlled"="#E69F00", "Not endemic"="#0072B2")

# 1.  A map of countries with responses to the survey (perhaps coloured by those
#     with endemic dog rabies or not - can always use Gavi data on this)
# 2.  Some summary statistics of responses to Y/N questions (basically % Y/N) -
#     but these will need to be adjusted for the denominator (i..e if skipped response is NA)
# 3.  Some barplots of responses to multi choice Qs. e.g. what were reasons for
#     disruption to mass dog vaccinations (there are 10 multi choice Qs like this),
#     possibly stacked barplots for responses from countries that are rabies endemic vs free.

#----- Initial data processing -------------------------------------------------

# Remove Antarctica
map.world <- map.world[-which(map.world$region=="Antarctica"),]

# Remove trailing white space in country names
map.world$region <- trimws(map.world$region, which="right")
survey_data$country <- trimws(survey_data$country, which="right")

# Create dataframe of countries and endemic status
country_df <- survey_data %>%
  dplyr::select(country) %>%
  merge(., country_data, by.x="country", by.y="COUNTRY", all.x=TRUE) %>%
  dplyr::select(country, RABIES.STATUS) %>%
  mutate(RABIES.STATUS = ifelse(is.na(RABIES.STATUS), "Not endemic",
                                ifelse(RABIES.STATUS=="endemic", "Endemic", "Controlled"))) %>%
  unique()
country_df$RABIES.STATUS = factor(country_df$RABIES.STATUS, levels=c("Endemic", "Controlled", "Not endemic"))

# Merge country_df into survey data to capture endemic status
survey_data = merge(survey_data, country_df, by="country")

# Recode shapefile to match data
country_df$country[which(country_df$country=="C??te d'Ivoire")] <- "Ivory Coast"
map.world$region[which(map.world$region=="Guinea-Bissau")] <- "Guinea Bissau"
map.world$region[which(map.world$region=="Swaziland")] <- "Eswatini"
map.world$region[which(map.world$region=="South Africa" & map.world$subregion=="enclave")] <- "Lesotho"
map.world$subregion[which(map.world$region=="South Africa" & map.world$subregion=="enclave")] <- ""
map.world <- map.world[-which(map.world$group==955),] # Remove the original Lesotho, which was double mapped

# CHECK!
sort(unique(country_df$country[-which(country_df$country %in% map.world$region)]))
#' *RS: Need to assign "Global_1", "Global_2", "Global_3" and "Global_4"*

# Merge into world data
map.world.df <- map.world %>%
  merge(., country_df, by.x="region", by.y="country", all.y=T) %>%
  arrange(region, group, order)

#----- Produce map -------------------------------------------------------------

# Plot map
ggplot() +
  geom_polygon(data=map.world, aes(x=long, y=lat, group=group),
               colour="black", fill="grey95", lwd=0.04) +
  geom_polygon(data=map.world.df, aes(x=long, y=lat, group=group, color=RABIES.STATUS, fill=RABIES.STATUS), alpha=0.6) +
  scale_fill_manual(values = col_pal) +
  scale_color_manual(values = col_pal) +
  coord_equal() +
  theme_void()
ggsave("figs/map.pdf", height=5, width=10)

#----- Statistics on multiple choice Q1 ----------------------------------------

# Process data to select multiple choice questions
mdv_happened_2020 = survey_data %>%
  dplyr::select(p.n., RABIES.STATUS,  yes.as.planned, yes.but.delayed.took.longer,
                yes.but.below.target, no.interrupted.early,
                no.not.started, other1, NA1) %>% # more.on.2020.mdv.happened
  gather(question, response, yes.as.planned:NA1) %>%
  arrange(p.n.) %>%
  filter(response == 1)

# Extract number of responses and non-responses, and add to table
mdv_happened_2020$n_surveys = length(unique(mdv_happened_2020$p.n.[which(mdv_happened_2020$question != "NA1")]))
mdv_happened_2020$n_NAs = length(unique(mdv_happened_2020$p.n.[which(mdv_happened_2020$question == "NA1")]))

# Set as factor
mdv_happened_2020$question = factor(mdv_happened_2020$question,
                                    levels=c("yes.as.planned", "yes.but.delayed.took.longer",
                                                  "yes.but.below.target", "no.interrupted.early",
                                                  "no.not.started", "other1", "NA1"))

# Summarise data for statistics
mdv_happened_2020_summary = mdv_happened_2020 %>%
  filter(question != "NA1") %>%
  mutate(q_n = 1) %>%
  group_by(q_n, n_surveys, n_NAs, question) %>%
  tally() %>%
  mutate(p = round((n/n_surveys)*100, digits=1))

# Summarise data for plot
mdv_happened_2020_plot = mdv_happened_2020 %>%
  filter(question != "NA1") %>%
  group_by(n_surveys, n_NAs, RABIES.STATUS, question) %>%
  tally() %>%
  mutate(p = round((n/n_surveys)*100, digits=1))

# Produce barplot
ggplot(data=mdv_happened_2020_plot, aes(x=question, y=p, fill=RABIES.STATUS)) +
  geom_col() +
  labs(x="", y="Percentage of responses", title="In 2020, was MDV carried out?",
       subtitle=paste0("Responded: ", unique(mdv_happened_2020_plot$n_surveys),
                       "; No answer: ", unique(mdv_happened_2020_plot$n_NAs))) +
  scale_y_continuous(limits=c(0, 100)) +
  scale_fill_manual(values=col_pal) +
  theme_classic() +
  theme(legend.position = "top", axis.text.x = element_text(angle=45, hjust=1))
ggsave("figs/q1_mdv_carried_out.pdf", height=7, width=8)

#----- Statistics on multiple choice Q2 ----------------------------------------

# Process data to select multiple choice questions
cause_mdv_interuption = survey_data %>%
  dplyr::select(p.n., RABIES.STATUS, no.staff.available, restrictions.on.staff.movement,
                no.vaccines.available, no.consumables.available,
                difficult.to.adhere.to.covid.guidelines,
                people.afraid.of.leaving.home.gathering,
                increased.cost.of.organizing, other2, NA2) %>% # more.on.reasons.for.mdv.disruption
  gather(question, response, no.staff.available:NA2) %>%
  arrange(p.n.) %>%
  filter(response == 1)

# Extract number of responses and non-responses, and add to table
cause_mdv_interuption$n_surveys = length(unique(cause_mdv_interuption$p.n.[which(cause_mdv_interuption$question != "NA2")]))
cause_mdv_interuption$n_NAs = length(unique(cause_mdv_interuption$p.n.[which(cause_mdv_interuption$question == "NA2")]))

# Set as factor
cause_mdv_interuption$question = factor(cause_mdv_interuption$question,
                                    levels=c("no.staff.available", "restrictions.on.staff.movement",
                                             "no.vaccines.available", "no.consumables.available",
                                             "difficult.to.adhere.to.covid.guidelines",
                                             "people.afraid.of.leaving.home.gathering",
                                             "increased.cost.of.organizing", "other2", "NA2"))

# Summarise data for statistics
cause_mdv_interuption_summary = cause_mdv_interuption %>%
  filter(question != "NA2") %>%
  mutate(q_n = 2) %>%
  group_by(q_n, n_surveys, n_NAs, question) %>%
  tally() %>%
  mutate(p = round((n/n_surveys)*100, digits=1))

# Summarise data for plot
cause_mdv_interuption_plot = cause_mdv_interuption %>%
  filter(question != "NA2") %>%
  group_by(n_surveys, n_NAs, RABIES.STATUS, question) %>%
  tally() %>%
  mutate(p = round((n/n_surveys)*100, digits=1))

# Produce barplot
ggplot(data=cause_mdv_interuption_plot, aes(x=question, y=p, fill=RABIES.STATUS)) +
  geom_col() +
  labs(x="", y="Percentage of responses", title="What were the reasons for MDV being disrupted?",
       subtitle=paste0("Responded: ", unique(cause_mdv_interuption_plot$n_surveys),
                       "; No answer: ", unique(cause_mdv_interuption_plot$n_NAs))) +
  scale_y_continuous(limits=c(0, 100)) +
  scale_fill_manual(values=col_pal) +
  theme_classic() +
  theme(legend.position = "top", axis.text.x = element_text(angle=45, hjust=1))
ggsave("figs/q2_reason_mdv_interupted.pdf", height=7, width=8)

#----- Statistics on multiple choice Q3 ----------------------------------------

# Process data to select multiple choice questions
mdv_method = survey_data %>%
  dplyr::select(p.n., RABIES.STATUS,  central.static.point, mobile.point, catch.vaccinate.release,
                door.to.door, oral, other3, NA3) %>% # more.on.mdv.method
  gather(question, response, central.static.point:NA3) %>%
  arrange(p.n.) %>%
  filter(response == 1)

# Extract number of responses and non-responses, and add to table
mdv_method$n_surveys = length(unique(mdv_method$p.n.[which(mdv_method$question != "NA3")]))
mdv_method$n_NAs = length(unique(mdv_method$p.n.[which(mdv_method$question == "NA3")]))

# Set as factor
mdv_method$question = factor(mdv_method$question,
                                        levels=c("central.static.point", "mobile.point", "catch.vaccinate.release",
                                                 "door.to.door", "oral", "other3", "NA3"))

# Summarise data for statistics
mdv_method_summary = mdv_method %>%
  filter(question != "NA3") %>%
  mutate(q_n = 3) %>%
  group_by(q_n, n_surveys, n_NAs, question) %>%
  tally() %>%
  mutate(p = round((n/n_surveys)*100, digits=1))

# Summarise data for plot
mdv_method_plot = mdv_method %>%
  filter(question != "NA3") %>%
  group_by(n_surveys, n_NAs, RABIES.STATUS, question) %>%
  tally() %>%
  mutate(p = round((n/n_surveys)*100, digits=1))

# Produce barplot
ggplot(data=mdv_method_plot, aes(x=question, y=p, fill=RABIES.STATUS)) +
  geom_col() +
  labs(x="", y="Percentage of responses", title="What was the method of MDV?",
       subtitle=paste0("Responded: ", unique(mdv_method_plot$n_surveys),
                       "; No answer: ", unique(mdv_method_plot$n_NAs))) +
  scale_y_continuous(limits=c(0, 100)) +
  scale_fill_manual(values=col_pal) +
  theme_classic() +
  theme(legend.position = "top", axis.text.x = element_text(angle=45, hjust=1))
ggsave("figs/q3_mdv_method.pdf", height=7, width=8)

#----- Statistics on multiple choice Q4 ----------------------------------------

# Process data to select multiple choice questions
impact_on_vaccine = survey_data %>%
  dplyr::select(p.n., RABIES.STATUS,  yes.production.has.considerably.reduced, yes.production.has.reduced,
                yes.importation.delayed, yes.importation.reduced, yes.distribution.affected,
                none1, other4, NA4) %>% # more.on.impact.on.vaccine.production.supply.chains
  gather(question, response, yes.production.has.considerably.reduced:NA4) %>%
  arrange(p.n.) %>%
  filter(response == 1)

# Extract number of responses and non-responses, and add to table
impact_on_vaccine$n_surveys = length(unique(impact_on_vaccine$p.n.[which(impact_on_vaccine$question != "NA4")]))
impact_on_vaccine$n_NAs = length(unique(impact_on_vaccine$p.n.[which(impact_on_vaccine$question == "NA4")]))

# Set as factor
impact_on_vaccine$question = factor(impact_on_vaccine$question,
                             levels=c("yes.production.has.considerably.reduced", "yes.production.has.reduced",
                                      "yes.importation.delayed", "yes.importation.reduced", "yes.distribution.affected",
                                      "none1", "other4", "NA4"))

# Summarise data for statistics
impact_on_vaccine_summary = impact_on_vaccine %>%
  filter(question != "NA4") %>%
  mutate(q_n = 4) %>%
  group_by(q_n, n_surveys, n_NAs, question) %>%
  tally() %>%
  mutate(p = round((n/n_surveys)*100, digits=1))

# Summarise data for plot
impact_on_vaccine_plot = impact_on_vaccine %>%
  filter(question != "NA4") %>%
  group_by(n_surveys, n_NAs, RABIES.STATUS, question) %>%
  tally() %>%
  mutate(p = round((n/n_surveys)*100, digits=1))

# Produce barplot
ggplot(data=impact_on_vaccine_plot, aes(x=question, y=p, fill=RABIES.STATUS)) +
  geom_col() +
  labs(x="", y="Percentage of responses", title="What has been the impact on vaccine production/supply chains?",
       subtitle=paste0("Responded: ", unique(impact_on_vaccine_plot$n_surveys),
                       "; No answer: ", unique(impact_on_vaccine_plot$n_NAs))) +
  scale_y_continuous(limits=c(0, 100)) +
  scale_fill_manual(values=col_pal) +
  theme_classic() +
  theme(legend.position = "top", axis.text.x = element_text(angle=45, hjust=1))
ggsave("figs/q4_impact_on_vaccine.pdf", height=7, width=8)

#----- Statistics on multiple choice Q5 ----------------------------------------

# Process data to select multiple choice questions
change_in_dog = survey_data %>%
  dplyr::select(p.n., RABIES.STATUS,  more.roaming.dogs, fewer.roaming.dogs, dogs.more.aggressive,
                dogs.in.poorer.health, none2, other5, NA5) %>% # more.on.changes.in.dog.behaviour.populations
  gather(question, response, more.roaming.dogs:NA5) %>%
  arrange(p.n.) %>%
  filter(response == 1)

# Extract number of responses and non-responses, and add to table
change_in_dog$n_surveys = length(unique(change_in_dog$p.n.[which(change_in_dog$question != "NA5")]))
change_in_dog$n_NAs = length(unique(change_in_dog$p.n.[which(change_in_dog$question == "NA5")]))

# Set as factor
change_in_dog$question = factor(change_in_dog$question,
                                    levels=c("more.roaming.dogs", "fewer.roaming.dogs", "dogs.more.aggressive",
                                             "dogs.in.poorer.health", "none2", "other5", "NA5"))

# Summarise data for statistics
change_in_dog_summary = change_in_dog %>%
  filter(question != "NA5") %>%
  mutate(q_n = 5) %>%
  group_by(q_n, n_surveys, n_NAs, question) %>%
  tally() %>%
  mutate(p = round((n/n_surveys)*100, digits=1))

# Summarise data for plot
change_in_dog_plot = change_in_dog %>%
  filter(question != "NA5") %>%
  group_by(n_surveys, n_NAs, RABIES.STATUS, question) %>%
  tally() %>%
  mutate(p = round((n/n_surveys)*100, digits=1))

# Produce barplot
ggplot(data=change_in_dog_plot, aes(x=question, y=p, fill=RABIES.STATUS)) +
  geom_col() +
  labs(x="", y="Percentage of responses", title="What have been the observed changes in dog behaviour/populations?",
       subtitle=paste0("Responded: ", unique(change_in_dog_plot$n_surveys),
                       "; No answer: ", unique(change_in_dog_plot$n_NAs))) +
  scale_y_continuous(limits=c(0, 100)) +
  scale_fill_manual(values=col_pal) +
  theme_classic() +
  theme(legend.position = "top", axis.text.x = element_text(angle=45, hjust=1))
ggsave("figs/q5_change_in_dog_behav_pop.pdf", height=7, width=8)

#----- Statistics on multiple choice Q6 ----------------------------------------

# Process data to select multiple choice questions
change_in_interaction = survey_data %>%
  dplyr::select(p.n., RABIES.STATUS,  more.people.fed.them, people.complained, people.asked.for.removal.kill,
                people.removed.killed.them, official.workers.removed.killed.them,
                more.abandonment.due.to.fear.of.covid, more.abandonment.due.to.financial.constraints,
                more.abandonment.for.other.reasons, none3, other6, NA6) %>% # more.on.changes.in.human.frds.interactions
  gather(question, response, more.people.fed.them:NA6) %>%
  arrange(p.n.) %>%
  filter(response == 1)

# Extract number of responses and non-responses, and add to table
change_in_interaction$n_surveys = length(unique(change_in_interaction$p.n.[which(change_in_interaction$question != "NA6")]))
change_in_interaction$n_NAs = length(unique(change_in_interaction$p.n.[which(change_in_interaction$question == "NA6")]))

# Set as factor
change_in_interaction$question = factor(change_in_interaction$question,
                                levels=c("more.people.fed.them", "people.complained", "people.asked.for.removal.kill",
                                         "people.removed.killed.them", "official.workers.removed.killed.them",
                                         "more.abandonment.due.to.fear.of.covid", "more.abandonment.due.to.financial.constraints",
                                         "more.abandonment.for.other.reasons", "none3", "other6", "NA6"))
# Summarise data for statistics
change_in_interaction_summary = change_in_interaction %>%
  filter(question != "NA6") %>%
  mutate(q_n = 6) %>%
  group_by(q_n, n_surveys, n_NAs, question) %>%
  tally() %>%
  mutate(p = round((n/n_surveys)*100, digits=1))

# Summarise data for plot
change_in_interaction_plot = change_in_interaction %>%
  filter(question != "NA6") %>%
  group_by(n_surveys, n_NAs, RABIES.STATUS, question) %>%
  tally()%>%
  mutate(p = round((n/n_surveys)*100, digits=1))

# Produce barplot
ggplot(data=change_in_interaction_plot, aes(x=question, y=p, fill=RABIES.STATUS)) +
  geom_col() +
  labs(x="", y="Percentage of responses", title="What have been the observed changes in human-frds interactions?",
       subtitle=paste0("Responded: ", unique(change_in_interaction_plot$n_surveys),
                       "; No answer: ", unique(change_in_interaction_plot$n_NAs))) +
  scale_y_continuous(limits=c(0, 100)) +
  scale_fill_manual(values=col_pal) +
  theme_classic() +
  theme(legend.position = "top", axis.text.x = element_text(angle=45, hjust=1))
ggsave("figs/q6_change_in_human_frds_interactions.pdf", height=7, width=8)

#----- Statistics on multiple choice Q7 ----------------------------------------

# Process data to select multiple choice questions
cause_for_investigating_dis = survey_data %>%
  dplyr::select(p.n., RABIES.STATUS,  no.staff.available.1, restrictions.on.staff.movement.1,
                no.sample.collection.testing.kit.available, no.budget,
                difficult.to.adhere.to.covid.guidelines.1, investigators.not.welcomed.in.communities,
                other7, NA7) %>% # more.on.reasons.for.investigating.reporting.disruption
  gather(question, response, no.staff.available.1:NA7) %>%
  arrange(p.n.) %>%
  filter(response == 1)

# Extract number of responses and non-responses, and add to table
cause_for_investigating_dis$n_surveys = length(unique(cause_for_investigating_dis$p.n.[which(cause_for_investigating_dis$question != "NA7")]))
cause_for_investigating_dis$n_NAs = length(unique(cause_for_investigating_dis$p.n.[which(cause_for_investigating_dis$question == "NA7")]))

# Set as factor
cause_for_investigating_dis$question = factor(cause_for_investigating_dis$question,
                                        levels=c("no.staff.available.1", "restrictions.on.staff.movement.1",
                                                 "no.sample.collection.testing.kit.available", "no.budget",
                                                 "difficult.to.adhere.to.covid.guidelines.1", "investigators.not.welcomed.in.communities",
                                                 "other7", "NA7"))
# Summarise data for statistics
cause_for_investigating_dis_summary = cause_for_investigating_dis %>%
  filter(question != "NA7") %>%
  mutate(q_n = 7) %>%
  group_by(q_n, n_surveys, n_NAs, question) %>%
  tally() %>%
  mutate(p = round((n/n_surveys)*100, digits=1))

# Summarise data for plot
cause_for_investigating_dis_plot = cause_for_investigating_dis %>%
  filter(question != "NA7") %>%
  group_by(n_surveys, n_NAs, RABIES.STATUS, question) %>%
  tally() %>%
  mutate(p = round((n/n_surveys)*100, digits=1))

# Produce barplot
ggplot(data=cause_for_investigating_dis_plot, aes(x=question, y=p, fill=RABIES.STATUS)) +
  geom_col() +
  labs(x="", y="Percentage of responses", title="What have been the reasons for investigating/reporting disruption?",
       subtitle=paste0("Responded: ", unique(cause_for_investigating_dis_plot$n_surveys),
                       "; No answer: ", unique(cause_for_investigating_dis_plot$n_NAs))) +
  scale_y_continuous(limits=c(0, 100)) +
  scale_fill_manual(values=col_pal) +
  theme_classic() +
  theme(legend.position = "top", axis.text.x = element_text(angle=45, hjust=1))
ggsave("figs/q7_reason_for_investigating_reporting.pdf", height=7, width=8)

#----- Statistics on multiple choice Q8 ----------------------------------------

# Process data to select multiple choice questions
change_in_health_seeking = survey_data %>%
  dplyr::select(p.n., RABIES.STATUS,  people.still.attending.clinics, people.have.avoided.clinics.due.to.fear.of.covid,
                people.cannot.afford.travel, people.cannot.reach.clinics.because.of.reduced.public.transport,
                people.have.delayed.going.to.clinics, people.have.interrupted.pep,
                people.have.relied.more.on.local.remedies, people.have.called.toll.free.numbers,
                wound.washing.has.increased, other8, NA8) %>% # more.on.changes.in.health.seeking.behaviour
  gather(question, response, people.still.attending.clinics:NA8) %>%
  arrange(p.n.) %>%
  filter(response == 1)

# Extract number of responses and non-responses, and add to table
change_in_health_seeking$n_surveys = length(unique(change_in_health_seeking$p.n.[which(change_in_health_seeking$question != "NA8")]))
change_in_health_seeking$n_NAs = length(unique(change_in_health_seeking$p.n.[which(change_in_health_seeking$question == "NA8")]))

# Set as factor
change_in_health_seeking$question = factor(change_in_health_seeking$question,
                                              levels=c("people.still.attending.clinics", "people.have.avoided.clinics.due.to.fear.of.covid",
                                                       "people.cannot.afford.travel", "people.cannot.reach.clinics.because.of.reduced.public.transport",
                                                       "people.have.delayed.going.to.clinics", "people.have.interrupted.pep",
                                                       "people.have.relied.more.on.local.remedies", "people.have.called.toll.free.numbers",
                                                       "wound.washing.has.increased", "other8", "NA8"))
# Summarise data for statistics
change_in_health_seeking_summary = change_in_health_seeking %>%
  filter(question != "NA8") %>%
  mutate(q_n = 8) %>%
  group_by(q_n, n_surveys, n_NAs, question) %>%
  tally() %>%
  mutate(p = round((n/n_surveys)*100, digits=1))

# Summarise data for plot
change_in_health_seeking_plot = change_in_health_seeking %>%
  filter(question != "NA8") %>%
  group_by(n_surveys, n_NAs, RABIES.STATUS, question) %>%
  tally() %>%
  mutate(p = round((n/n_surveys)*100, digits=1))

# Produce barplot
ggplot(data=change_in_health_seeking_plot, aes(x=question, y=p, fill=RABIES.STATUS)) +
  geom_col() +
  labs(x="", y="Percentage of responses", title="What have been the changes in health seeking behaviour?",
       subtitle=paste0("Responded: ", unique(change_in_health_seeking_plot$n_surveys),
                       "; No answer: ", unique(change_in_health_seeking_plot$n_NAs))) +
  scale_y_continuous(limits=c(0, 100)) +
  scale_fill_manual(values=col_pal) +
  theme_classic() +
  theme(legend.position = "top", axis.text.x = element_text(angle=45, hjust=1))
ggsave("figs/q8_changes_in_health_seeking.pdf", height=7, width=8)

#----- Statistics on multiple choice Q9 ----------------------------------------

# Process data to select multiple choice questions
change_in_pep = survey_data %>%
  dplyr::select(p.n., RABIES.STATUS,  some.many.arc.closed.converted, staff.redeployed, staff.reduced.due.to.quarantine,
                staff.less.diligent.due.to.stress, staff.less.likely.to.recommend.pep,
                follow.up.shots.delayed.cancelled, vaccines.out.of.stock.because.of.financial.constraints,
                vaccines.out.of.stock.due.to.supply.issues, vaccines.available.only.in.the.private.sector,
                consumables.not.available, rig.not.available, other9, NA9) %>% # more.on.changes.in.pep
  gather(question, response, some.many.arc.closed.converted:NA9) %>%
  arrange(p.n.) %>%
  filter(response == 1)

# Extract number of responses and non-responses, and add to table
change_in_pep$n_surveys = length(unique(change_in_pep$p.n.[which(change_in_pep$question != "NA9")]))
change_in_pep$n_NAs = length(unique(change_in_pep$p.n.[which(change_in_pep$question == "NA9")]))

# Set as factor
change_in_pep$question = factor(change_in_pep$question,
                                           levels=c("some.many.arc.closed.converted", "staff.redeployed", "staff.reduced.due.to.quarantine",
                                                    "staff.less.diligent.due.to.stress", "staff.less.likely.to.recommend.pep",
                                                    "follow.up.shots.delayed.cancelled", "vaccines.out.of.stock.because.of.financial.constraints",
                                                    "vaccines.out.of.stock.due.to.supply.issues", "vaccines.available.only.in.the.private.sector",
                                                    "consumables.not.available", "rig.not.available", "other9", "NA9"))
# Summarise data for statistics
change_in_pep_summary = change_in_pep %>%
  filter(question != "NA9") %>%
  mutate(q_n = 9) %>%
  group_by(q_n, n_surveys, n_NAs, question) %>%
  tally() %>%
  mutate(p = round((n/n_surveys)*100, digits=1))

# Summarise data for plot
change_in_pep_plot = change_in_pep %>%
  filter(question != "NA9") %>%
  group_by(n_surveys, n_NAs, RABIES.STATUS, question) %>%
  tally() %>%
  mutate(p = round((n/n_surveys)*100, digits=1))

# Produce barplot
ggplot(data=change_in_pep_plot, aes(x=question, y=p, fill=RABIES.STATUS)) +
  geom_col() +
  labs(x="", y="Percentage of responses", title="What have been the changes in PEP?",
       subtitle=paste0("Responded: ", unique(change_in_pep_plot$n_surveys),
                       "; No answer: ", unique(change_in_pep_plot$n_NAs))) +
  scale_y_continuous(limits=c(0, 100)) +
  scale_fill_manual(values=col_pal) +
  theme_classic() +
  theme(legend.position = "top", axis.text.x = element_text(angle=45, hjust=1))
ggsave("figs/q9_changes_in_pep.pdf", height=7, width=8)

#----- Statistics on multiple choice Q10 ---------------------------------------

# Process data to select multiple choice questions
change_in_frds_media = survey_data %>%
  dplyr::select(p.n., RABIES.STATUS, attacks.on.dogs, attacks.on.other.animals, attacks.on.humans,
                animal.rabies.cases.deaths, human.exposures.deaths, human.cruelty,
                care, none4, other10, NA10) %>% # more.on.changes.in.frds.in.the.media
  gather(question, response, attacks.on.dogs:NA10) %>%
  arrange(p.n.) %>%
  filter(response == 1)

# Extract number of responses and non-responses, and add to table
change_in_frds_media$n_surveys = length(unique(change_in_frds_media$p.n.[which(change_in_frds_media$question != "NA10")]))
change_in_frds_media$n_NAs = length(unique(change_in_frds_media$p.n.[which(change_in_frds_media$question == "NA10")]))

# Set as factor
change_in_frds_media$question = factor(change_in_frds_media$question,
                                levels=c("attacks.on.dogs", "attacks.on.other.animals", "attacks.on.humans",
                                         "animal.rabies.cases.deaths", "human.exposures.deaths", "human.cruelty",
                                         "care", "none4", "other10", "NA10"))
# Summarise data for statistics
change_in_frds_media_summary = change_in_frds_media %>%
  filter(question != "NA10") %>%
  mutate(q_n = 10) %>%
  group_by(q_n, n_surveys, n_NAs, question) %>%
  tally() %>%
  mutate(p = round((n/n_surveys)*100, digits=1))

# Summarise data for plot
change_in_frds_media_plot = change_in_frds_media %>%
  filter(question != "NA10") %>%
  group_by(n_surveys, n_NAs, RABIES.STATUS, question) %>%
  tally() %>%
  mutate(p = round((n/n_surveys)*100, digits=1))

# Produce barplot
ggplot(data=change_in_frds_media_plot, aes(x=question, y=p, fill=RABIES.STATUS)) +
  geom_col() +
  labs(x="", y="Percentage of responses", title="What have been the changes in frds in the media?",
       subtitle=paste0("Responded: ", unique(change_in_frds_media_plot$n_surveys),
                       "; No answer: ", unique(change_in_frds_media_plot$n_NAs))) +
  scale_y_continuous(limits=c(0, 100)) +
  scale_fill_manual(values=col_pal) +
  theme_classic() +
  theme(legend.position = "top", axis.text.x = element_text(angle=45, hjust=1))
ggsave("figs/q10_changes_in_frds_media.pdf", height=7, width=8)

#----- Finish processing summary statistics ------------------------------------

# Merge question summaries together
question_summaries = rbind(mdv_happened_2020_summary,
                           cause_mdv_interuption_summary,
                           mdv_method_summary,
                           impact_on_vaccine_summary,
                           change_in_dog_summary,
                           change_in_interaction_summary,
                           cause_for_investigating_dis_summary,
                           change_in_health_seeking_summary,
                           change_in_pep_summary,
                           change_in_frds_media_summary)

# Save output
write.csv(question_summaries, "output/summary_stats.csv", row.names=FALSE)
