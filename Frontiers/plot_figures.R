rm(list=ls())

# Load libraries
library(dplyr)
library(sf)
library(ggplot2)
library(ggpubr)

# Load functions
source("R/recode_countries.R")

# Load data
fig_1a <- read.csv("output/figure_1a.csv", stringsAsFactors = FALSE)
fig_1b <- read.csv("output/figure_1b.csv", stringsAsFactors = FALSE)
fig_2a <- read.csv("output/figure_2a.csv", stringsAsFactors = FALSE)
fig_2b <- read.csv("output/figure_2b.csv", stringsAsFactors = FALSE)
fig_2c <- read.csv("output/figure_2c.csv", stringsAsFactors = FALSE)
fig_3a <- read.csv("output/figure_3a.csv", stringsAsFactors = FALSE)
fig_3b <- read.csv("output/figure_3b.csv", stringsAsFactors = FALSE)
fig_3c <- read.csv("output/figure_3c.csv", stringsAsFactors = FALSE)
fig_3d <- read.csv("output/figure_3d.csv", stringsAsFactors = FALSE)
fig_4a <- read.csv("output/figure_4a.csv", stringsAsFactors = FALSE)
fig_4b <- read.csv("output/figure_4b.csv", stringsAsFactors = FALSE)
fig_4c <- read.csv("output/figure_4c.csv", stringsAsFactors = FALSE)

# Load shapefile
world_shp <- read_sf("data/WHO Map boundaries/MapTemplate_detailed_2013/Shapefiles/detailed_2013.shp")

# Set colour palettes
col_pal <- c("Endemic"="#f44336", "In-progress"="#f28b30", "Rabies-free"="#03658c")

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
  scale_fill_manual(name="Status of countries \nwith survey responses   ", values = col_pal, labels=c("Endemic   ", "In-progress   ", "Rabies-free")) +
  # scale_color_manual(name="Status of countries \nwith survey responses", values = col_pal, guide=guide_legend(order=1)) +
  geom_sf(data=country_centroids, fill="black", shape=1, size=2.5, stroke=0.8) +
  coord_sf() +
  theme_void() +
  theme(legend.position = "top", legend.title = element_text(size=12),
        legend.text = element_text(size=11))
fig_1a_plot

#----- Process data for barplot

fig_1b$work_sector <- factor(fig_1b$work_sector, levels=unique(fig_1b$work_sector))

#----- Produce panel 1b

fig_1b_plot = ggplot(data=fig_1b, aes(x=work_sector, y=n, fill=endemic_status)) +
  geom_col(colour="white", alpha=0.8) +
  scale_fill_manual(name="Status of countries \nwith survey responses", values = col_pal, guide=guide_legend(order=1)) +
  labs(x="Work Sector", y="Number of respondents") +
  scale_x_discrete(limits=rev) +
  coord_flip() +
  theme_classic()
fig_1b_plot

# Combine plots and save output
ggarrange(fig_1a_plot, fig_1b_plot, ncol=1, heights=c(2,1), labels=c("A", "B"), common.legend = TRUE)
ggsave("figs/Figure_1.pdf", height=7, width=10)

#----- Produce figure 2 --------------------------------------------------------

#----- Produce panel 2a

fig_2a_plot = ggplot(data=fig_2a, aes(x=result, y=n, fill=endemic_status)) +
  geom_col(alpha=0.8)+
  scale_fill_manual(name="Status of countries \nwith survey responses", values = col_pal, guide=guide_legend(order=1)) +
  labs(title="Impact on mass dog vaccination", x="Extent of disruption", y="Number of respondents\n") +
  scale_y_continuous(limits=c(0,40)) +
  theme_classic() +
  theme(axis.title.x = element_blank())
fig_2a_plot

#----- Produce panel 2b

fig_2b_plot = ggplot(data=fig_2b, aes(x=result, y=n, fill=endemic_status)) +
  geom_col(alpha=0.8) +
  scale_fill_manual(name="Status of countries \nwith survey responses", values = col_pal, guide=guide_legend(order=1)) +
  labs(title="Impact on access to post-exposure prophylaxis", x="Extent of disruption", y="Number of respondents\n") +
  scale_y_continuous(limits=c(0,40)) +
  theme_classic() +
  theme(axis.title.x = element_blank())
fig_2b_plot

#----- Produce panel 2c

fig_2c_plot = ggplot(data=fig_2c, aes(x=result, y=n, fill=endemic_status)) +
  geom_col(alpha=0.8) +
  scale_fill_manual(name="Status of countries \nwith survey responses", values = col_pal, guide=guide_legend(order=1)) +
  labs(title="Impact on children's awareness activities", x="Extent of disruption", y="Number of respondents\n") +
  scale_y_continuous(limits=c(0,40)) +
  theme_classic()
fig_2c_plot

#----- Combine and save

# Combine plots and save output
ggarrange(fig_2a_plot, fig_2b_plot, fig_2c_plot, ncol=1, common.legend = TRUE,
          labels=c("A", "B", "C"))
ggsave("figs/Figure_2.pdf", height=10, width=10)

#----- Produce figure 3 --------------------------------------------------------

#----- Process data for barplot

fig_3a$question <- factor(fig_3a$question, levels=unique(fig_3a$question))
fig_3b$question <- factor(fig_3b$question, levels=unique(fig_3b$question))
fig_3c$question <- factor(fig_3c$question, levels=unique(fig_3c$question))
fig_3d$question <- factor(fig_3d$question, levels=unique(fig_3d$question))

#----- Produce panel 3a

fig_3a_plot = ggplot(data=fig_3a, aes(x=question, y=n, fill=endemic_status)) +
  geom_col(alpha=0.8, width=0.9)+
  scale_fill_manual(name="Status of countries \nwith survey responses", values = col_pal, guide=guide_legend(order=1)) +
  labs(title="Reasons for disruption to mass dog vaccination", y="Number of respondents\n") +
  scale_y_continuous(limits=c(0,50)) +
  theme_classic() +
  scale_x_discrete(limits=rev) +
  coord_flip() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title.position = "plot", plot.title = element_text(hjust=0.1))
fig_3a_plot

#----- Produce panel 3b

fig_3b_plot = ggplot(data=fig_3b, aes(x=question, y=n, fill=endemic_status)) +
  geom_col(alpha=0.8)+
  scale_fill_manual(name="Status of countries \nwith survey responses", values = col_pal, guide=guide_legend(order=1)) +
  labs(title="Reasons for disruption to access to post-exposure prophylaxis", y="Number of respondents\n") +
  scale_y_continuous(limits=c(0,50)) +
  theme_classic() +
  scale_x_discrete(limits=rev) +
  coord_flip() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title.position = "plot", plot.title = element_text(hjust=0.15))
fig_3b_plot

#----- Produce panel 3c

fig_3c_plot = ggplot(data=fig_3c, aes(x=question, y=n, fill=endemic_status)) +
  geom_col(alpha=0.8)+
  scale_fill_manual(name="Status of countries \nwith survey responses", values = col_pal, guide=guide_legend(order=1)) +
  labs(title="Reasons for disruption to delivery of post-exposure prophyaxis", y="Number of respondents\n") +
  scale_y_continuous(limits=c(0,50)) +
  theme_classic() +
  scale_x_discrete(limits=rev) +
  coord_flip() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title.position = "plot", plot.title = element_text(hjust=0.15))
fig_3c_plot

#----- Produce panel 3d

fig_3d_plot = ggplot(data=fig_3d, aes(x=question, y=n, fill=endemic_status)) +
  geom_col(alpha=0.8)+
  scale_fill_manual(name="Status of countries \nwith survey responses", values = col_pal, guide=guide_legend(order=1)) +
  labs(title="Reasons for disruption to surveillance", y="Number of respondents\n") +
  scale_y_continuous(limits=c(0,50)) +
  theme_classic() +
  scale_x_discrete(limits=rev) +
  coord_flip() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title.position = "plot", plot.title = element_text(hjust=0.1))
fig_3d_plot

#----- Combine and save

# Combine plots and save output
ggarrange(fig_3a_plot, fig_3b_plot, fig_3c_plot, fig_3d_plot,
          ncol=2, nrow=2, common.legend = TRUE,
          labels=c("A", "B", "C", "D"), align="hv")
ggsave("figs/Figure_3.pdf", height=10, width=15)

#----- Figure 4 ----------------------------------------------------------------

#----- Process data for barplot

fig_4a$question <- factor(fig_4a$question, levels=unique(fig_4a$question))
fig_4b$question <- factor(fig_4b$question, levels=unique(fig_4b$question))
fig_4c$question <- factor(fig_4c$question, levels=unique(fig_4c$question))

#----- Produce panel 4a

fig_4a_plot = ggplot(data=fig_4a, aes(x=question, y=n, fill=endemic_status)) +
  geom_col(alpha=0.8, width=0.9)+
  scale_fill_manual(name="Status of countries \nwith survey responses", values = col_pal, guide=guide_legend(order=1)) +
  labs(title="Changes in free-roaming dog populations and behaviour", y="Number of respondents\n") +
  scale_y_continuous(limits=c(0,35)) +
  theme_classic() +
  scale_x_discrete(limits=rev) +
  coord_flip() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title.position = "plot", plot.title = element_text(hjust=0.1))
fig_4a_plot

#----- Produce panel 4b

fig_4b_plot = ggplot(data=fig_4b, aes(x=question, y=n, fill=endemic_status)) +
  geom_col(alpha=0.8, width=0.9)+
  scale_fill_manual(name="Status of countries \nwith survey responses", values = col_pal, guide=guide_legend(order=1)) +
  labs(title="Changes in human-dog interactions", y="Number of respondents\n") +
  scale_y_continuous(limits=c(0,35)) +
  theme_classic() +
  scale_x_discrete(limits=rev) +
  coord_flip() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title.position = "plot", plot.title = element_text(hjust=0.08))
fig_4b_plot

#----- Produce panel 4c

fig_4c_plot = ggplot(data=fig_4c, aes(x=question, y=n, fill=endemic_status)) +
  geom_col(alpha=0.8, width=0.9)+
  scale_fill_manual(name="Status of countries \nwith survey responses", values = col_pal, guide=guide_legend(order=1)) +
  labs(title="Changes in media reporting of dogs", y="Number of respondents\n") +
  scale_y_continuous(limits=c(0,35)) +
  theme_classic() +
  scale_x_discrete(limits=rev) +
  coord_flip() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title.position = "plot", plot.title = element_text(hjust=0.08))
fig_4c_plot

#----- Combine and save

# Combine plots and save output
ggarrange(fig_4a_plot, fig_4b_plot, fig_4c_plot, ncol=1, common.legend = TRUE,
          labels=c("A", "B", "C"), align="v")
ggsave("figs/Figure_4.pdf", height=12, width=8)
