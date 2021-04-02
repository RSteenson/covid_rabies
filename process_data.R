rm(list=ls())

# Load libraries
library(dplyr)
library(sf)
library(ggplot2)
library(tidyverse)
devtools::install_github("dcooley/sfheaders")
library(sfheaders)
library(ggpubr)

options(stringsAsFactors=FALSE,
        dplyr.summarise.inform = FALSE)

# Load functions
source("R/point_map.R")

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
country_data = read.csv("data/country_data_V2.csv")
survey_data = read.csv("data/Survey_data.csv")

# Load shapefile
map.world <- map_data("world")

# Transform map to shapefile
# map.world_sf <- sfheaders::sf_polygon(obj = map.world, x = "long", y = "lat", polygon_id = "group")
# map.world_sf = map.world %>%
#   dplyr::select(group, region) %>%
#   unique() %>%
#   merge(map.world_sf, ., by="group") %>%
#   group_by(region) %>%
#   summarise()
#
# leaflet() %>%
#   addPolygons(data = map.world_sf, label=map.world_sf$region, fillColor = "white", weight=1,
#               highlightOptions = list(fillColor="blue"))

# Create colour and alpha palette
col_pal = c("Endemic"="red2", "Controlled"="#ff6666", "Not endemic"="grey35", "Global"="dimgrey")
alph_pal = c("positive"=1, "negative"=0.1)

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

# Recode names to match between data
country_data$COUNTRY[which(country_data$COUNTRY=="Brunei Darussalam")] <- "Brunei"
country_data$COUNTRY[which(country_data$COUNTRY=="Guinea-Bissau")] <- "Guinea Bissau"
country_data$COUNTRY[which(country_data$COUNTRY=="INAia")] <- "India"
country_data$COUNTRY[which(country_data$COUNTRY=="INAonesia")] <- "Indonesia"
country_data$COUNTRY[which(country_data$COUNTRY=="Lao People's Democratic Republic")] <- "Laos"
country_data$COUNTRY[which(country_data$COUNTRY=="Taiwan Province of China")] <- "Taiwan"
country_data$COUNTRY[which(country_data$COUNTRY=="Tanzania, United Republic of ")] <- "Tanzania"
country_data$COUNTRY[which(country_data$COUNTRY=="ThailaNA")] <- "Thailand"
country_data$COUNTRY[which(country_data$COUNTRY=="UgaNAa")] <- "Uganda"
country_data$COUNTRY[which(country_data$COUNTRY=="United States of America")] <- "USA"

# Recode countries to read as "endemic" or "controlled"
# country_data$RABIES.STATUS[which(country_data$COUNTRY=="Argentina")] <- "endemic"
# country_data$RABIES.STATUS[which(country_data$COUNTRY=="Brazil")] <- "endemic"
# country_data$RABIES.STATUS[which(country_data$COUNTRY=="Peru")] <- "endemic"
# country_data$RABIES.STATUS[which(country_data$COUNTRY=="Haiti")] <- "endemic"
# country_data$RABIES.STATUS[which(country_data$COUNTRY=="Israel")] <- "controlled"
# country_data$RABIES.STATUS[which(country_data$COUNTRY=="Kuwait")] <- "controlled"
# country_data$RABIES.STATUS[which(country_data$COUNTRY=="Saudi Arabia")] <- "controlled"

# Create dataframe of countries and endemic status
country_df <- survey_data %>%
  dplyr::select(country) %>%
  merge(., country_data, by.x="country", by.y="COUNTRY", all.x=TRUE) %>%
  dplyr::select(country, FINAL.CANINE.RABIES.STATUS) %>%
  mutate(FINAL.CANINE.RABIES.STATUS = ifelse(FINAL.CANINE.RABIES.STATUS=="endemic", "Endemic",
                                             ifelse(FINAL.CANINE.RABIES.STATUS=="controlled", "Controlled", "Not endemic"))) %>%
  unique()

# Merge country_df into survey data to capture endemic status
survey_data = merge(survey_data, country_df, by="country")

# Change global country names/Endemic status to "Global"
survey_data$country[grepl("Global", survey_data$country)] <- "Global"
survey_data$FINAL.CANINE.RABIES.STATUS[which(survey_data$country=="Global")] <- "Global"

# Remove global records
country_df <- country_df[-which(country_df$country %in% c("Global_1", "Global_2", "Global_3", "Global_4")),]

# Set factor level
country_df$FINAL.CANINE.RABIES.STATUS = factor(country_df$FINAL.CANINE.RABIES.STATUS, levels=c("Endemic", "Controlled", "Not endemic"))

# Recode shapefile to match data
country_df$country[grepl("d'Ivoire", country_df$country)] <- "Ivory Coast"
map.world$region[which(map.world$region=="Guinea-Bissau")] <- "Guinea Bissau"
map.world$region[which(map.world$region=="Swaziland")] <- "Eswatini"
map.world$region[which(map.world$region=="South Africa" & map.world$subregion=="enclave")] <- "Lesotho"
map.world$subregion[which(map.world$region=="South Africa" & map.world$subregion=="enclave")] <- ""
map.world <- map.world[-which(map.world$group==955),] # Remove the original Lesotho, which was double mapped

# CHECK!
sort(unique(country_df$country[-which(country_df$country %in% map.world$region)]))

# Merge into world data
map.world.df <- map.world %>%
  merge(., country_df, by.x="region", by.y="country", all.y=T) %>%
  arrange(region, group, order)

#----- Produce map -------------------------------------------------------------

# Plot map
ggplot() +
  geom_polygon(data=map.world, aes(x=long, y=lat, group=group),
               fill="grey75", lwd=0.04) + # colour="black",
  geom_polygon(data=map.world.df, aes(x=long, y=lat, group=group), fill="white") +
  geom_polygon(data=map.world.df, aes(x=long, y=lat, group=group, color=FINAL.CANINE.RABIES.STATUS,
                                      fill=FINAL.CANINE.RABIES.STATUS), alpha=0.8) +
  scale_fill_manual(name="Rabies Status", values = col_pal) +
  scale_color_manual(name="Rabies Status", values = col_pal) +
  coord_equal() +
  theme_void() +
  theme(legend.position = "top")
ggsave("figs/paper/map.pdf", height=8, width=14)

#----- Produce initial stats ---------------------------------------------------

source("R/produce_initial_stats.R")

#----- Produce multi-panel barplot ---------------------------------------------

# Set colours for different sectors
sector_cols = c("vet"="#4080ff", "health"="#ffbf40", "surveillance"="#74e539")

# Barplot for reason mdv interupted
cause_mdv_interuption_bp = cause_mdv_interuption %>%
  mutate(plot_groups = ifelse(question %in% c("no.staff.available", "restrictions.on.staff.movement"), "Staff restrictions",
                              ifelse(question %in% c("no.vaccines.available", "no.consumables.available"), "Supply issues",
                                     ifelse(question %in% c("difficult.to.adhere.to.covid.guidelines", "people.afraid.of.leaving.home.gathering"), "Covid-based restrictions",
                                            ifelse(question == "increased.cost.of.organizing", "Budget issues",
                                                   ifelse(question == "other2", "Other", NA)))))) %>%
  group_by(p.n., n_surveys, n_NAs, plot_groups) %>%
  summarise(n=length(unique(p.n.))) %>% # tally()
  group_by(n_surveys, n_NAs, plot_groups) %>%
  summarise(n = sum(n)) %>%
  mutate(p=(n/n_surveys)*100,
         type="vet") %>%
  filter(!is.na(plot_groups))
cause_mdv_interuption_bp$plot_groups <- factor(cause_mdv_interuption_bp$plot_groups,
                                               levels=c("Staff restrictions", "Supply issues",
                                                        "Covid-based restrictions", "Budget issues",
                                                        "Other"))
cause_mdv_interuption_bp$type <- factor(cause_mdv_interuption_bp$type,
                                        levels=c("vet", "health", "surveillance"))
bp_1 = ggplot(data=cause_mdv_interuption_bp, aes(x=plot_groups, y=p, fill=type)) +
  geom_col() +
  labs(x="", y="Percentage of respondents") +
  scale_y_continuous(limits=c(0, 100)) +
  scale_fill_manual(values=sector_cols, drop=FALSE) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=12), legend.title = element_blank()) # legend.position = "none",
bp_1

# Barplot for disrutption to investigations
cause_for_investigating_dis_bp = cause_for_investigating_dis %>%
  mutate(plot_groups = ifelse(question %in% c("no.staff.available.1", "restrictions.on.staff.movement.1"), "Staff restrictions",
                              ifelse(question %in% c("no.sample.collection.testing.kit.available"), "Supply issues",
                                     ifelse(question %in% c("difficult.to.adhere.to.covid.guidelines.1", "investigators.not.welcomed.in.communities"), "Covid-based restrictions",
                                            ifelse(question == "no.budget", "Budget issues",
                                                   ifelse(question == "other7", "Other", NA)))))) %>%
  group_by(p.n., n_surveys, n_NAs, plot_groups) %>%
  summarise(n=length(unique(p.n.))) %>% # tally()
  group_by(n_surveys, n_NAs, plot_groups) %>%
  summarise(n = sum(n)) %>%
  mutate(p=(n/n_surveys)*100,
         type="surveillance") %>%
  filter(!is.na(plot_groups))
cause_for_investigating_dis_bp$plot_groups <- factor(cause_for_investigating_dis_bp$plot_groups,
                                                     levels=c("Staff restrictions", "Supply issues",
                                                              "Covid-based restrictions", "Budget issues",
                                                              "Other"))
bp_2 = ggplot(data=cause_for_investigating_dis_bp, aes(x=plot_groups, y=p, fill=type)) +
  geom_col() +
  labs(x="", y="Percentage of respondents") +
  scale_y_continuous(limits=c(0, 100)) +
  scale_fill_manual(values=sector_cols) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=12)) # legend.position = "none",
bp_2

# Barplot for changes in health seeking behaviour
change_in_health_seeking_bp = change_in_health_seeking %>%
  filter(question != "people.still.attending.clinics") %>%
  mutate(plot_groups = ifelse(question %in% c("people.have.called.toll.free.numbers", "people.have.relied.more.on.local.remedies", "wound.washing.has.increased"), "Reliance on \nalternative healthcare",
                              ifelse(question %in% c("people.have.avoided.clinics.due.to.fear.of.covid", "people.have.delayed.going.to.clinics", "people.have.interrupted.pep"), "Avoidence of clinics",
                                     ifelse(question %in% c("people.cannot.reach.clinics.because.of.reduced.public.transport", "people.cannot.afford.travel"), "Transport issues",
                                            ifelse(question == "other8", "Other", NA))))) %>%
  group_by(p.n., n_surveys, n_NAs, plot_groups) %>%
  summarise(n=length(unique(p.n.))) %>% # tally()
  group_by(n_surveys, n_NAs, plot_groups) %>%
  summarise(n = sum(n)) %>%
  mutate(p=(n/n_surveys)*100,
         type="health") %>%
  filter(!is.na(plot_groups))
change_in_health_seeking_bp$plot_groups <- factor(change_in_health_seeking_bp$plot_groups,
                                                     levels=c("Avoidence of clinics", "Reliance on \nalternative healthcare",
                                                              "Transport issues", "Other"))
bp_3 = ggplot(data=change_in_health_seeking_bp, aes(x=plot_groups, y=p, fill=type)) +
  geom_col() +
  labs(x="", y="Percentage of respondents") +
  scale_y_continuous(limits=c(0, 100)) +
  scale_fill_manual(values=sector_cols) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=12)) # legend.position = "none",
bp_3

# Barplot for disruption to PEP
change_in_pep_bp = change_in_pep %>%
  filter(question != "people.still.attending.clinics") %>%
  mutate(plot_groups = ifelse(question %in% c("staff.redeployed", "staff.reduced.due.to.quarantine"), "Staff restrictions",
                              ifelse(question %in% c("staff.less.likely.to.recommend.pep", "follow.up.shots.delayed.cancelled"), "Change in PEP delivery",
                                     ifelse(question %in% c("vaccines.out.of.stock.due.to.supply.issues", "vaccines.available.only.in.the.private.sector", "consumables.not.available", "rig.not.available"), "Supply issues",
                                            ifelse(question %in% c("vaccines.out.of.stock.because.of.financial.constraints"), "Budget issues",
                                                   ifelse(question == "other9", "Other", NA)))))) %>%
  group_by(p.n., n_surveys, n_NAs, plot_groups) %>%
  summarise(n=length(unique(p.n.))) %>% # tally()
  group_by(n_surveys, n_NAs, plot_groups) %>%
  summarise(n = sum(n)) %>%
  mutate(p=(n/n_surveys)*100,
         type="health") %>%
  filter(!is.na(plot_groups))
change_in_pep_bp$plot_groups <- factor(change_in_pep_bp$plot_groups,
                                       levels=c("Staff restrictions", "Change in PEP delivery",
                                                "Supply issues", "Budget issues", "Other"))
bp_4 = ggplot(data=change_in_pep_bp, aes(x=plot_groups, y=p, fill=type)) +
  geom_col() +
  labs(x="", y="Percentage of respondents") +
  scale_y_continuous(limits=c(0, 100)) +
  scale_fill_manual(values=sector_cols) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=12)) # legend.position = "none",
bp_4

# Create and save multi-panel plot
multi_plot = ggarrange(bp_1, bp_2, bp_3, bp_4, ncol=2, nrow=2, common.legend = TRUE, align="hv",
          labels=c("A", "B", "C", "D"))
annotate_figure(multi_plot,
                bottom = text_grob(label="A: Reasons for MDV disruption | B: Reasons for surveillance disruption
                                   C: Observed changes in health seeking behaviour | D: Reasons for PEP disruption",
                                   hjust = 1, x = 1, face = "italic", size = 10))
ggsave("figs/paper/multi_panel_barplot.pdf", height=10, width=8)

#----- Produce maps with yes/no responses --------------------------------------

# Transform world map to sf object
map_sf = st_as_sf(map.world.df, coords=c("long", "lat"))

# Extract country centroids
map_centroids = map_sf %>%
   group_by(region) %>%
  summarise() %>%
  st_centroid()

# Create base map
base_map = ggplot() +
  geom_polygon(data=map.world, aes(x=long, y=lat, group=group),
               fill="grey75", lwd=0.04) +
  geom_polygon(data=map.world.df, aes(x=long, y=lat, group=group),
               fill="grey50", color="grey35", lwd=0.04) +
  coord_sf() +
  theme_void()

# Plot map to check centroid location
base_map + geom_sf(data=map_centroids)

# Set map point colours and shapes
# col_pal = c("circle"="black", "cross"="red")
# fill_pal = c(sector_cols, "fill"="black", "empty"="white", "none"="red")
# shap_pal = c("circle"=21, "cross"=4)

# Run script to complete processing
source("R/process_yes_no.R")

# Produce maps, and save as individual files
budget_divert_map = point_map(dataframe=budget_divert_centroids,
                              map_title="Was the budget for rabies \nprevention/control reduced/diverted?")
ggsave("figs/paper/individual_point_maps/map_budget_divert.pdf", width=10, height=6)
mdv_map = point_map(dataframe=mdv_centroids,
                    map_title="Was MDV carried out in 2020?")
ggsave("figs/paper/individual_point_maps/map_mdv_happened.pdf", width=10, height=6)
arv_demand_map = point_map(dataframe=increased_arv_demand_centroids,
                    map_title="Have vets experienced higher demand for ARV?")
ggsave("figs/paper/individual_point_maps/map_vet_arv_demand.pdf", width=10, height=6)
arv_supply_map = point_map(dataframe=arv_supply_centroids,
                           map_title="Has ARV production/supply been affected?")
ggsave("figs/paper/individual_point_maps/map_vet_arv_supply.pdf", width=10, height=6)
staff_redeployed_map = point_map(dataframe=staff_redeployed_centroids,
                           map_title="Were surveillance staff redeployed?")
ggsave("figs/paper/individual_point_maps/map_surv_staff_redeployed.pdf", width=10, height=6)
lab_capacity_map = point_map(dataframe=lab_capacity_centroids,
                                 map_title="Was lab capacity reduced/diverted?")
ggsave("figs/paper/individual_point_maps/map_lab_capacity.pdf", width=10, height=6)
health_seeking_map = point_map(dataframe=health_seeking_centroids,
                             map_title="Were there changes in health seeking behaviour?")
ggsave("figs/paper/individual_point_maps/map_health_seeking.pdf", width=10, height=6)
dog_bite_guidance_map = point_map(dataframe=dog_bite_guidance_centroids,
                               map_title="Have dog bites been mentioned in \npublic guidance during the pandemic?")
ggsave("figs/paper/individual_point_maps/map_bite_guidance.pdf", width=10, height=6)
wrd_impact_map = point_map(dataframe=wrd_impact_centroids,
                           map_title="Were WRD events impacted in 2020?")
ggsave("figs/paper/individual_point_maps/map_wrd_events.pdf", width=10, height=6)

# Produce combined map (panelled currently)
ggarrange(budget_divert_map, mdv_map, arv_demand_map,
          arv_supply_map, staff_redeployed_map, lab_capacity_map,
          health_seeking_map, dog_bite_guidance_map, wrd_impact_map,
          ncol=3, nrow=3, common.legend = TRUE)
ggsave("figs/paper/combined_map.pdf", width=15, height=9)
