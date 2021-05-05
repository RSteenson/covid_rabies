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

# Load functions
source("R/recode_countries.R")
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
map.world = read_sf("data/WHO Map boundaries/MapTemplate_detailed_2013/Shapefiles/detailed_2013.shp")

# Create colour and alpha palette
# col_pal_1 = c("Endemic"="red2", "Controlled"="#ff6666", "Not endemic"="grey35", "Global"="dimgrey")
col_pal_2 = c("Endemic"="firebrick2", "Controlled"="#E69F00", "Not endemic"="dimgrey", "Global"="dimgrey")
alph_pal = c("positive"=1, "negative"=0.1)

#----- Initial data processing -------------------------------------------------

# Remove Antarctica
# map.world <- map.world[-which(map.world$region=="Antarctica"),]

# Remove trailing white space in country names
survey_data$country <- trimws(survey_data$country, which="right")

# Set Taiwan as endemic as must be classed as part of Chine
country_data$FINAL.CANINE.RABIES.STATUS[which(country_data$COUNTRY=="Taiwan Province of China")] <- "endemic"

# Recode names to match between data
survey_data <- recode_countries(survey_data, "country")
country_data <- recode_countries(country_data, "COUNTRY")

# Check if any names don't match data
survey_data$country[-which(survey_data$country %in% map.world$CNTRY_TERR)]
country_data$COUNTRY[-which(country_data$COUNTRY %in% map.world$CNTRY_TERR)]

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

# CHECK!
sort(unique(country_df$country[-which(country_df$country %in% map.world$CNTRY_TERR)]))

# Merge into world data
map.world.df <- map.world %>%
  merge(., country_df, by.x="CNTRY_TERR", by.y="country", all.y=T)

#----- Produce map -------------------------------------------------------------

# Plot map
# ggplot() +
#   geom_sf(data=map.world, fill="grey75", lwd=0.04, color=NA) + # colour="black",
#   geom_sf(data=map.world.df, fill="white") +
#   geom_sf(data=map.world.df, aes(color=FINAL.CANINE.RABIES.STATUS, fill=FINAL.CANINE.RABIES.STATUS), alpha=0.7) +
#   scale_fill_manual(name="Canine Rabies Status", values = col_pal_1) +
#   scale_color_manual(name="Canine Rabies Status", values = col_pal_1) +
#   coord_sf() +
#   theme_void() +
#   theme(legend.position = "top")
# ggsave("figs/paper/map_red.pdf", height=8, width=14)
status = ggplot() +
  geom_sf(data=map.world, fill="grey75", lwd=0.04, color=NA) + # colour="black",
  geom_sf(data=map.world.df, fill="white") +
  geom_sf(data=map.world.df, aes(color=FINAL.CANINE.RABIES.STATUS, fill=FINAL.CANINE.RABIES.STATUS), alpha=0.7) +
  scale_fill_manual(name="Canine Rabies Status", values = col_pal_2, guide=guide_legend(order=1)) +
  scale_color_manual(name="Canine Rabies Status", values = col_pal_2, guide=guide_legend(order=1)) +
  coord_sf() +
  theme_void() +
  theme(legend.position = "top")
status
# ggsave("figs/paper/map_endemic_status.pdf", height=8, width=14)

#----- Produce initial stats ---------------------------------------------------

source("R/produce_initial_stats.R")

#----- Produce multi-panel barplot ---------------------------------------------

# Set colours for different sectors
sector_cols = c("vet"="#4080ff", "health"="#ffbf40", "surveillance"="#74e539")
sector_labels = c("Vet", "Health", "Surveillance")

# Barplot for reason mdv interupted
cause_mdv_interuption_bp = cause_mdv_interuption %>%
  mutate(plot_groups = ifelse(question %in% c("no.staff.available", "restrictions.on.staff.movement"), "Movement \nrestrictions",
                              ifelse(question %in% c("no.vaccines.available", "no.consumables.available"), "Supply \nissues",
                                     ifelse(question %in% c("difficult.to.adhere.to.covid.guidelines", "people.afraid.of.leaving.home.gathering"), "COVID safety \nmeasures",
                                            ifelse(question == "increased.cost.of.organizing", "Budget \nconstraints",
                                                   ifelse(question == "other2", "Other", NA)))))) %>%
  group_by(p.n., n_surveys, n_NAs, plot_groups) %>%
  summarise(n=length(unique(p.n.))) %>% # tally()
  group_by(n_surveys, n_NAs, plot_groups) %>%
  summarise(n = sum(n)) %>%
  mutate(p=(n/n_surveys)*100,
         type="vet") %>%
  filter(!is.na(plot_groups)) %>%
  arrange(desc(p))
cause_mdv_interuption_bp$plot_groups <- factor(cause_mdv_interuption_bp$plot_groups, levels=cause_mdv_interuption_bp$plot_groups)
# cause_mdv_interuption_bp$plot_groups <- factor(cause_mdv_interuption_bp$plot_groups,
#                                                levels=c("Staff restrictions", "Supply issues",
#                                                         "Covid-based restrictions", "Budget issues",
#                                                         "Other"))
cause_mdv_interuption_bp$type <- factor(cause_mdv_interuption_bp$type,
                                        levels=c("vet", "health", "surveillance"))
bp_1 = ggplot(data=cause_mdv_interuption_bp, aes(x=plot_groups, y=p, fill=type)) +
  geom_col() +
  labs(x="", y="Percentage of respondents") +
  scale_y_continuous(limits=c(0, 100)) +
  scale_fill_manual(name="Sector: ", values=sector_cols, labels=sector_labels, drop=FALSE) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=12)) #, legend.title = element_blank()) # legend.position = "none",
bp_1

# Barplot for disrutption to investigations
cause_for_investigating_dis_bp = cause_for_investigating_dis %>%
  mutate(plot_groups = ifelse(question %in% c("no.staff.available.1", "restrictions.on.staff.movement.1"), "Movement \nrestrictions",
                              ifelse(question %in% c("no.sample.collection.testing.kit.available"), "Supply \nissues",
                                     ifelse(question %in% c("difficult.to.adhere.to.covid.guidelines.1", "investigators.not.welcomed.in.communities"), "COVID safety \nmeasures",
                                            ifelse(question == "no.budget", "Budget \nconstraints",
                                                   ifelse(question == "other7", "Other", NA)))))) %>%
  group_by(p.n., n_surveys, n_NAs, plot_groups) %>%
  summarise(n=length(unique(p.n.))) %>% # tally()
  group_by(n_surveys, n_NAs, plot_groups) %>%
  summarise(n = sum(n)) %>%
  mutate(p=(n/n_surveys)*100,
         type="surveillance") %>%
  filter(!is.na(plot_groups)) %>%
  arrange(desc(p))
cause_for_investigating_dis_bp$plot_groups <- factor(cause_for_investigating_dis_bp$plot_groups, levels=cause_for_investigating_dis_bp$plot_groups)
# cause_for_investigating_dis_bp$plot_groups <- factor(cause_for_investigating_dis_bp$plot_groups,
#                                                      levels=c("Staff restrictions", "Supply issues",
#                                                               "Covid-based restrictions", "Budget issues",
#                                                               "Other"))
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
  mutate(plot_groups = ifelse(question %in% c("people.have.called.toll.free.numbers", "people.have.relied.more.on.local.remedies", "wound.washing.has.increased"), "Alternative \nmedicine",
                              ifelse(question %in% c("people.have.avoided.clinics.due.to.fear.of.covid", "people.have.delayed.going.to.clinics", "people.have.interrupted.pep"), "Avoided \nclinics",
                                     ifelse(question %in% c("people.cannot.reach.clinics.because.of.reduced.public.transport"), "Transport \nissues",
                                            ifelse(question %in% c("people.cannot.afford.travel"), "Costs",
                                                   ifelse(question == "other8", "Other", NA)))))) %>%
  group_by(p.n., n_surveys, n_NAs, plot_groups) %>%
  summarise(n=length(unique(p.n.))) %>% # tally()
  group_by(n_surveys, n_NAs, plot_groups) %>%
  summarise(n = sum(n)) %>%
  mutate(p=(n/n_surveys)*100,
         type="health") %>%
  filter(!is.na(plot_groups)) %>%
  arrange(desc(p))
change_in_health_seeking_bp$plot_groups <- factor(change_in_health_seeking_bp$plot_groups, levels=change_in_health_seeking_bp$plot_groups)
# change_in_health_seeking_bp$plot_groups <- factor(change_in_health_seeking_bp$plot_groups,
#                                                      levels=c("Avoidance of clinics", "Reliance on \nalternative healthcare",
#                                                               "Transport issues", "Budget issues", "Other"))
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
  mutate(plot_groups = ifelse(question %in% c("staff.redeployed", "staff.reduced.due.to.quarantine"), "Movement \nrestrictions",
                              ifelse(question %in% c("staff.less.likely.to.recommend.pep", "follow.up.shots.delayed.cancelled"), "Change in PEP \nadministration",
                                     ifelse(question %in% c("vaccines.out.of.stock.due.to.supply.issues", "vaccines.available.only.in.the.private.sector", "consumables.not.available", "rig.not.available"), "Supply \nissues",
                                            ifelse(question %in% c("vaccines.out.of.stock.because.of.financial.constraints"), "Budget \nconstraints",
                                                   ifelse(question == "other9", "Other", NA)))))) %>%
  group_by(p.n., n_surveys, n_NAs, plot_groups) %>%
  summarise(n=length(unique(p.n.))) %>% # tally()
  group_by(n_surveys, n_NAs, plot_groups) %>%
  summarise(n = sum(n)) %>%
  mutate(p=(n/n_surveys)*100,
         type="health") %>%
  filter(!is.na(plot_groups)) %>%
  arrange(desc(p))
change_in_pep_bp$plot_groups <- factor(change_in_pep_bp$plot_groups, levels=change_in_pep_bp$plot_groups)
# change_in_pep_bp$plot_groups <- factor(change_in_pep_bp$plot_groups,
#                                        levels=c("Staff restrictions", "Change in PEP delivery",
#                                                 "Supply issues", "Budget issues", "Other"))
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
  group_by(CNTRY_TERR) %>%
  summarise() %>%
  st_centroid()

# Set countries that had an in-depth interview conducted
interview_countries = c("Haiti", "India", "Bangladesh", "Bhutan", "Nepal", "Pakistan",
                        "Philippines", "Indonesia", "Sri Lanka", "Afghanistan", "Yemen",
                        "Mali", "C??te d'Ivoire", "Gabon", "United Republic of Tanzania",
                        "Kenya", "Uganda", "South Sudan", "Malawi")
int_count_centroids = map_centroids %>%
  filter(map_centroids$CNTRY_TERR %in% interview_countries)

# Save endemic status map
status = status +
  geom_sf(data=int_count_centroids, size=2, shape=24, stroke=0.9, fill=NA)
status
ggsave("figs/paper/map_endemic_status.pdf", height=8, width=14)

# Create base map
base_map = ggplot() +
  geom_sf(data=map.world, #, aes(x=long, y=lat, group=group),
               fill="grey75", color=NA, lwd=0.04) +
  geom_sf(data=map.world.df, #, aes(x=long, y=lat, group=group),
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
                              map_title="Rabies budget reduced/diverted")
# ggsave("figs/paper/individual_point_maps/map_budget_divert.pdf", width=10, height=6)
mdv_map = point_map(dataframe=mdv_centroids,
                    map_title="Dog vaccination campaigns disrupted")
# ggsave("figs/paper/individual_point_maps/map_mdv_happened.pdf", width=10, height=6)
# arv_demand_map = point_map(dataframe=increased_arv_demand_centroids,
#                     map_title="Vets experienced higher demand for ARV")
# ggsave("figs/paper/individual_point_maps/map_vet_arv_demand.pdf", width=10, height=6)
arv_supply_map = point_map(dataframe=arv_supply_centroids,
                           map_title="Dog vaccine production/supply affected")
# ggsave("figs/paper/individual_point_maps/map_vet_arv_supply.pdf", width=10, height=6)
staff_redeployed_map = point_map(dataframe=staff_redeployed_centroids,
                           map_title="Surveillance staff redeployed")
# ggsave("figs/paper/individual_point_maps/map_surv_staff_redeployed.pdf", width=10, height=6)
# lab_capacity_map = point_map(dataframe=lab_capacity_centroids,
#                                  map_title="Lab capacity reduced/diverted")
# ggsave("figs/paper/individual_point_maps/map_lab_capacity.pdf", width=10, height=6)
health_seeking_map = point_map(dataframe=health_seeking_centroids,
                             map_title="Health seeking behaviour affected")
# ggsave("figs/paper/individual_point_maps/map_health_seeking.pdf", width=10, height=6)
# dog_bite_guidance_map = point_map(dataframe=dog_bite_guidance_centroids,
#                                map_title="Dog bites mentioned in public guidance?")
# ggsave("figs/paper/individual_point_maps/map_bite_guidance.pdf", width=10, height=6)
# wrd_impact_map = point_map(dataframe=wrd_impact_centroids,
#                            map_title="WRD events impacted")
# ggsave("figs/paper/individual_point_maps/map_wrd_events.pdf", width=10, height=6)

# Produce combined map (panelled currently)
# ggarrange(budget_divert_map, mdv_map, arv_demand_map,
#           arv_supply_map, staff_redeployed_map, lab_capacity_map,
#           health_seeking_map, dog_bite_guidance_map, wrd_impact_map,
#           ncol=3, nrow=3, common.legend = TRUE)
# ggsave("figs/paper/combined_map_V1.pdf", width=15, height=9)

# Produce reduced combined map
# combined_surv_map = point_map(dataframe=combined_surv_centroids,
#                            map_title="Surveillance staff redeployed, or lab capacity reduced")
# ggarrange(budget_divert_map, mdv_map, arv_supply_map, combined_surv_map,
#           health_seeking_map,
#           ncol=2, nrow=3, common.legend = TRUE)
# ggsave("figs/paper/combined_map_V2.pdf", width=15, height=9)

# Produce reduced combined map
ggarrange(budget_divert_map, mdv_map, arv_supply_map, staff_redeployed_map, health_seeking_map,
          ncol=2, nrow=3, common.legend = TRUE)
ggsave("figs/paper/combined_map_1.pdf", width=14, height=9)

# Combine map with endemic status
point_dummy <- health_seeking_centroids[1:4,]
point_dummy$col <- c("Cross-sectoral", "Health", "Surveillance", "Vet")
new_fill_pal = c(col_pal_2, "Cross-sectoral"="black", "Health"="#ffbf40", "Surveillance"="#74e539", "Vet"="#4080ff")
status_edit = status +
  new_scale_fill() +
  # geom_sf(data=g1, aes(fill=FINAL.CANINE.RABIES.STATUS), color=NA, alpha=0) +
  geom_sf(data=point_dummy, aes(fill=col), stroke=0.7, alpha=0, size=2) +
  scale_fill_manual(name="        Sector", values = new_fill_pal, drop=FALSE,
                    guide=guide_legend(order=2, override.aes=list(alpha=1, shape=21, size=4))) +
  ggtitle("Rabies Endemic Status")
status_edit
ggarrange(status_edit, budget_divert_map, mdv_map, arv_supply_map, staff_redeployed_map,
          health_seeking_map, ncol=2, nrow=3, common.legend = TRUE)
ggsave("figs/paper/combined_map_2.pdf", width=14, height=9)
