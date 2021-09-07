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

#----- Analyse basic data ------------------------------------------------------

# Country of research application
country_plot = survey_data %>%
  dplyr::select(observed.country, progress) %>%
  group_by(progress, observed.country) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
country_plot$progress = factor(country_plot$progress, levels=c(1,2,3,4))
ggplot(data=country_plot, aes(x=observed.country, y=count, fill=progress)) +
  geom_col() +
  scale_fill_manual(values=col_pal, labels=c("Rabies free", "Controlled", "Endemic", "Endemic with infrastructural challenges ")) +
  labs(x="", y="Survey responses") +
  theme_classic() +
  theme(legend.title = element_blank(), legend.position = "top",
        axis.text.x = element_text(angle=45, hjust=1))

# Work Sector
work_sector_responses = survey_data %>%
  dplyr::select(observed.country, work.sector) %>%
  group_by(work.sector) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
work_sector_plot = survey_data %>%
  dplyr::select(observed.country, progress, work.sector) %>%
  group_by(progress, work.sector) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
work_sector_plot$progress = factor(work_sector_plot$progress, levels=c(1,2,3,4))
work_sector_plot$work.sector = factor(work_sector_plot$work.sector, levels=unique(work_sector_responses$work.sector))
ggplot(data=work_sector_plot, aes(x=work.sector, y=count, fill=progress)) +
  geom_col() +
  scale_fill_manual(values=col_pal, labels=c("Rabies free", "Controlled", "Endemic", "Endemic with infrastructural challenges ")) +
  labs(x="", y="Survey responses") +
  theme_classic() +
  theme(legend.title = element_blank(), legend.position = "top",
        axis.text.x = element_text(angle=45, hjust=1))

# Divrsion of Rabies funding
fund_diversion_responses = survey_data %>%
  dplyr::select(rabies.budget.reduced.diverted) %>%
  group_by(rabies.budget.reduced.diverted) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
fund_diversion_plot = survey_data %>%
  dplyr::select(observed.country, progress, rabies.budget.reduced.diverted) %>%
  group_by(progress, rabies.budget.reduced.diverted) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
fund_diversion_plot$progress = factor(fund_diversion_plot$progress, levels=c(1,2,3,4))
fund_diversion_plot$rabies.budget.reduced.diverted = factor(fund_diversion_plot$rabies.budget.reduced.diverted, levels=unique(fund_diversion_responses$rabies.budget.reduced.diverted))
ggplot(data=fund_diversion_plot, aes(x=rabies.budget.reduced.diverted, y=count, fill=progress)) +
  geom_col() +
  scale_fill_manual(values=col_pal, labels=c("Rabies free", "Controlled", "Endemic", "Endemic with infrastructural challenges ")) +
  labs(x="", y="Survey responses") +
  theme_classic() +
  theme(legend.title = element_blank(), legend.position = "top",
        axis.text.x = element_text(angle=45, hjust=1))
