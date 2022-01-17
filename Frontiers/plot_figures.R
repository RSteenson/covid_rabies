rm(list=ls())

# Load libraries
library(dplyr)
library(sf)
library(ggplot2)
library(ggpubr)

# Set global options
options(stringsAsFactors = FALSE)

# Load functions
source("R/recode_countries.R")

# Load data
fig_1a <- read.csv("output/figure_1a.csv")
fig_1b <- read.csv("output/figure_1b.csv")
fig_2a <- read.csv("output/figure_2a.csv")
fig_2b <- read.csv("output/figure_2b.csv")
fig_2c <- read.csv("output/figure_2c.csv")
fig_3a <- read.csv("output/figure_3a.csv")
fig_3b <- read.csv("output/figure_3b.csv")
fig_3c <- read.csv("output/figure_3c.csv")
fig_3d <- read.csv("output/figure_3d.csv")
fig_4a <- read.csv("output/figure_4a.csv")
fig_4b <- read.csv("output/figure_4b.csv")
fig_4c <- read.csv("output/figure_4c.csv")

#######################################################################
# Fix names:
# fig_1b$work_sector <- gsub("National policy making/government", "National government", fig_1b$work_sector)
# fig_1b$work_sector <- gsub("Vaccine industry/logistics", "Industry", fig_1b$work_sector)
# CONFIRM LOCAL GOVT (only in rabies-free settings - are they really not AH or PH or Lab?)?
# CONFIRM programme management (only in endemic/ in-progress settings - are they really not AH or PH or Lab?)?
# Regional organization (if this is PAHO or a subbranch of WHO I would, like for Asia pacific, I'd def put at Int. Organization)
# What is other - can it really not be categorized?
#
# Note that the diamonds to highlight countries is now lost too!
# CAN THE COLOUR PALATE USE "Free" NOT "Rabies-free"

#######################################################################

# Load shapefile
world_shp <- read_sf("data/WHO Map boundaries/MapTemplate_detailed_2013/Shapefiles/detailed_2013.shp")

# Set colour palettes

col_pal <- c("Endemic"="#f44336", "In-progress"="#f28b30", "Free"="#03658c")

#----- Produce figure 1 --------------------------------------------------------

#----- Process data for map

# Fix geometry
world_shp_fixed <- st_make_valid(world_shp)

# Merge data into shapefile for plotting
world_shp_data <- merge(world_shp_fixed, fig_1a, by.x="CNTRY_TERR", by.y="country", all.y=T)

# Extract centroids for each country
country_centroids <- st_centroid(world_shp_fixed)

# Merge data into centroids and subset for interviewed countries
country_centroids <- country_centroids %>%
  merge(., fig_1a, by.x="CNTRY_TERR", by.y="country", all.y=T) %>%
  filter(interview == "Yes")

#----- Produce panel 1a

fig_1a_plot = ggplot() +
  geom_sf(data=world_shp, fill="grey75", lwd=0.04, color="white") +
  # geom_sf(data=world_shp, fill="white") +
  geom_sf(data=world_shp_data, aes(fill=endemic_status), color="white", lwd=0.04, alpha=0.8) +
  scale_fill_manual(name="Dog-mediated rabies:", values = col_pal, labels=c("Endemic   ", "In-progress   ", "Free")) +
  # scale_color_manual(name="Status of countries \nwith survey responses", values = col_pal, guide=guide_legend(order=1)) +
  geom_sf(data=country_centroids, fill="black", shape=1, size=2.5, stroke=0.8) +
  coord_sf() +
  theme_void() +
  theme(legend.position = "top", legend.title = element_text(size=10),
        legend.text = element_text(size=10))
fig_1a_plot

#----- Process data for barplot

# Set factors for bar order
fig_1b$work_sector <- factor(fig_1b$work_sector, levels=unique(fig_1b$work_sector))

# Set factors for colour order
fig_1b$endemic_status <- factor(fig_1b$endemic_status, levels=c("Endemic", "In-progress", "Free"))

#----- Produce panel 1b

fig_1b_plot = ggplot(data=fig_1b, aes(x=work_sector, y=n, fill=endemic_status)) +
  geom_col(alpha=0.8) + # colour="white",
  scale_fill_manual(name="Status of countries \nwith survey responses", values = col_pal, guide=guide_legend(order=1)) +
  labs(x="Work Sector", y="Respondents") +
  scale_x_discrete(limits=rev) +
  scale_y_continuous(limits=c(0,30)) +
  coord_flip() +
  theme_classic() + theme(legend.position = "none")
fig_1b_plot

# Combine plots and save output
ggarrange(fig_1a_plot, fig_1b_plot, ncol=1, heights=c(1.5,1), labels=c("A", "B"), common.legend = TRUE)
ggsave("figs/Figure_1.pdf", height=14, width=18, units = "cm")

#----- Produce figure 2 --------------------------------------------------------

#----- Process data for barplot

# Set factors for colour order
fig_2a$endemic_status <- factor(fig_2a$endemic_status, levels=c("Endemic", "In-progress", "Free"))
fig_2b$endemic_status <- factor(fig_2b$endemic_status, levels=c("Endemic", "In-progress", "Free"))
fig_2c$endemic_status <- factor(fig_2c$endemic_status, levels=c("Endemic", "In-progress", "Free"))

#----- Produce panel 2a

fig_2a_plot = ggplot(data=fig_2a, aes(x=result, y=n, fill=endemic_status)) +
  geom_col(alpha=0.8)+
  scale_fill_manual(name="Dog-mediated \n rabies status:",
                    values = col_pal, guide=guide_legend(order=1)) +
  labs(title="Dog vaccination", x="", y="") +
  scale_y_continuous(limits=c(0,40)) +
  theme_classic() +
  theme(axis.title.x = element_blank(), legend.title = element_text(size=6),
        legend.text = element_text(size=6), axis.title=element_text(size=7),
        title = element_text(size = 7), axis.text = element_text(size=6))
fig_2a_plot

#----- Produce panel 2b

fig_2b_plot = ggplot(data=fig_2b, aes(x=result, y=n, fill=endemic_status)) +
  geom_col(alpha=0.8) +
  scale_fill_manual(name="Dog-mediated rabies status:", values = col_pal, guide=guide_legend(order=1)) +
  labs(title="Post-exposure prophylaxis", x="", y="Respondents") +
  scale_y_continuous(limits=c(0,40)) +
  theme_classic() +
  theme(axis.title.x = element_blank(), legend.title = element_text(size=6),
        legend.text = element_text(size=6), axis.title=element_text(size=7),
        title = element_text(size = 7), axis.text = element_text(size=6))
fig_2b_plot

#----- Produce panel 2c

fig_2c_plot = ggplot(data=fig_2c, aes(x=result, y=n, fill=endemic_status)) +
  geom_col(alpha=0.8) +
  scale_fill_manual(name="Dog-mediated rabies status:", values = col_pal, guide=guide_legend(order=1)) +
  labs(title="Awareness activities", x="", y="") +
  scale_y_continuous(limits=c(0,40)) +
  theme_classic() +
  theme(axis.title.x = element_blank(), legend.title = element_text(size=6),
        legend.text = element_text(size=6), axis.title=element_text(size=7),
        title = element_text(size = 7), axis.text = element_text(size=6))
fig_2c_plot

#----- Combine and save

# Combine plots and save output
ggarrange(fig_2a_plot, fig_2b_plot, fig_2c_plot, ncol=1, common.legend = TRUE,
          labels=c("A", "B", "C"))
ggsave("figs/Figure_2.pdf", height=16, width=8, units="cm")

#----- Produce figure 3 --------------------------------------------------------

#----- Process data for barplot

# Set factors for bar order
fig_3a$grouped_response <- factor(fig_3a$grouped_response, levels=unique(fig_3a$grouped_response))
fig_3b$grouped_response <- factor(fig_3b$grouped_response, levels=unique(fig_3b$grouped_response))
fig_3c$grouped_response <- factor(fig_3c$grouped_response, levels=unique(fig_3c$grouped_response))
fig_3d$grouped_response <- factor(fig_3d$grouped_response, levels=unique(fig_3d$grouped_response))

# Set factors for colour order
fig_3a$endemic_status <- factor(fig_3a$endemic_status, levels=c("Endemic", "In-progress", "Free"))
fig_3b$endemic_status <- factor(fig_3b$endemic_status, levels=c("Endemic", "In-progress", "Free"))
fig_3c$endemic_status <- factor(fig_3c$endemic_status, levels=c("Endemic", "In-progress", "Free"))
fig_3d$endemic_status <- factor(fig_3d$endemic_status, levels=c("Endemic", "In-progress", "Free"))

#----- Produce panel 3a

fig_3a_plot = ggplot(data=fig_3a, aes(x=grouped_response, y=n, fill=endemic_status)) +
  geom_col(alpha=0.8, width=0.9)+
  scale_fill_manual(name="Dog-mediated rabies status:", values = col_pal, guide=guide_legend(order=1)) +
  labs(title="Dog vaccination", y="Number of countries\n") +
  scale_y_continuous(limits=c(0,30)) +
  theme_classic() +
  scale_x_discrete(limits=rev) +
  coord_flip() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title.position = "plot", plot.title = element_text(hjust=0.1, size = 10))
fig_3a_plot

#----- Produce panel 3b

fig_3b_plot = ggplot(data=fig_3b, aes(x=grouped_response, y=n, fill=endemic_status)) +
  geom_col(alpha=0.8)+
  scale_fill_manual(name="Dog-mediated rabies status:", values = col_pal, guide=guide_legend(order=1)) +
  labs(title="Health seeking", y="Number of countries\n") +
  scale_y_continuous(limits=c(0,30)) +
  theme_classic() +
  scale_x_discrete(limits=rev) +
  coord_flip() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title.position = "plot", plot.title = element_text(hjust=0.1, size = 10))
fig_3b_plot

#----- Produce panel 3c

fig_3c_plot = ggplot(data=fig_3c, aes(x=grouped_response, y=n, fill=endemic_status)) +
  geom_col(alpha=0.8)+
  scale_fill_manual(name="Dog-mediated rabies status:", values = col_pal, guide=guide_legend(order=1)) +
  labs(title="Post-exposure prophylaxis", y="Number of countries\n") +
  scale_y_continuous(limits=c(0,30)) +
  theme_classic() +
  scale_x_discrete(limits=rev) +
  coord_flip() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title.position = "plot", plot.title = element_text(hjust=0.15, size = 10))

#----- Produce panel 3d

fig_3d_plot = ggplot(data=fig_3d, aes(x=grouped_response, y=n, fill=endemic_status)) +
  geom_col(alpha=0.8)+
  scale_fill_manual(name="Dog-mediated rabies status:", values = col_pal, guide=guide_legend(order=1)) +
  labs(title="Surveillance", y="Number of countries\n") +
  scale_y_continuous(limits=c(0,30)) +
  theme_classic() +
  scale_x_discrete(limits=rev) +
  coord_flip() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title.position = "plot", plot.title = element_text(hjust=0.1, size = 10))
fig_3d_plot

#----- Combine and save

# Combine plots and save output
ggarrange(fig_3a_plot, fig_3b_plot, fig_3c_plot, fig_3d_plot,
          ncol=2, nrow=2, common.legend = TRUE,
          labels=c("A", "B", "C", "D"), align="hv")
ggsave("figs/Figure_3.pdf", height=12, width=18, units = "cm")

#----- Figure 4 ----------------------------------------------------------------

#----- Process data for barplot

# Set factor for bar order
fig_4a$question <- factor(fig_4a$question, levels=unique(fig_4a$question))
fig_4b$question <- factor(fig_4b$question, levels=unique(fig_4b$question))
fig_4c$question <- factor(fig_4c$question, levels=unique(fig_4c$question))

# Set factors for colour order
fig_4a$endemic_status <- factor(fig_4a$endemic_status, levels=c("Endemic", "In-progress", "Free"))
fig_4b$endemic_status <- factor(fig_4b$endemic_status, levels=c("Endemic", "In-progress", "Free"))
fig_4c$endemic_status <- factor(fig_4c$endemic_status, levels=c("Endemic", "In-progress", "Free"))

#----- Produce panel 4a

fig_4a_plot = ggplot(data=fig_4a, aes(x=question, y=n, fill=endemic_status)) +
  geom_col(alpha=0.8, width=0.9)+
  scale_fill_manual(name="Dog-mediated \nrabies status:", values = col_pal, guide=guide_legend(order=1)) +
  labs(title="Free-roaming dogs", y="") +
  scale_y_continuous(limits=c(0,30)) +
  theme_classic() +
  scale_x_discrete(limits=rev) +
  coord_flip() +
  theme(axis.title.y = element_blank(),
      legend.title = element_text(size=6),
      legend.text = element_text(size=6), axis.title=element_text(size=7),
      title = element_text(size = 7), axis.text = element_text(size=6),
      plot.title.position = "plot", plot.title = element_text(hjust=0.1))
fig_4a_plot

#----- Produce panel 4b

fig_4b_plot = ggplot(data=fig_4b, aes(x=question, y=n, fill=endemic_status)) +
  geom_col(alpha=0.8, width=0.9)+
  scale_fill_manual(name="Dog-mediated rabies status:", values = col_pal, guide=guide_legend(order=1)) +
  labs(title="Human-dog interactions", y="") +
  scale_y_continuous(limits=c(0,30)) +
  theme_classic() +
  scale_x_discrete(limits=rev) +
  coord_flip() +
  theme(axis.title.y = element_blank(),
      legend.title = element_text(size=6),
      legend.text = element_text(size=6), axis.title=element_text(size=7),
      title = element_text(size = 7), axis.text = element_text(size=6),
      plot.title.position = "plot", plot.title = element_text(hjust=0.1))
fig_4b_plot

#----- Produce panel 4c

fig_4c_plot = ggplot(data=fig_4c, aes(x=question, y=n, fill=endemic_status)) +
  geom_col(alpha=0.8, width=0.9)+
  scale_fill_manual(name="Dog-mediated rabies status:", values = col_pal, guide=guide_legend(order=1)) +
  labs(title="Media reporting of dogs", y="Countries\n") +
  scale_y_continuous(limits=c(0,30)) +
  theme_classic() +
  scale_x_discrete(limits=rev) +
  coord_flip() +
  theme(axis.title.y = element_blank(),
        legend.title = element_text(size=6),
        legend.text = element_text(size=6), axis.title=element_text(size=7),
        title = element_text(size = 7), axis.text = element_text(size=6),
        plot.title.position = "plot", plot.title = element_text(hjust=0.1))
fig_4c_plot

#----- Combine and save

# Combine plots and save output
ggarrange(fig_4a_plot, fig_4b_plot, fig_4c_plot, ncol=1, common.legend = TRUE,
          labels=c("A", "B", "C"), align="v", heights=c(0.7,1,1))
ggsave("figs/Figure_4.pdf", height=12, width=8, units="cm")

