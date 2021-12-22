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

# Load shapefile
world_shp <- read_sf("data/WHO Map boundaries/MapTemplate_detailed_2013/Shapefiles/detailed_2013.shp")

# Recode name to match between data
world_shp$CNTRY_TERR[which(world_shp$CNTRY_TERR == "C??te d'Ivoire")] = "Cote d'Ivoire"
world_shp$CNTRY_TERR[which(world_shp$CNTRY_TERR == "C??te d'Ivoire")] = "Cote d'Ivoire"

# Set colour palettes
col_pal <- c("Endemic"="#f44336", "In-progress"="#f28b30", "Rabies-free"="#03658c")

#----- Produce figure 1 --------------------------------------------------------

#----- Process data for map

# Merge data into shapefile
world_shp_data <- merge(world_shp, fig_1a, by.x="CNTRY_TERR", by.y="country", all.y=T)

# Set as factor
# world_shp_data$endemic_status <- factor(as.character(world_shp_data$endemic_status),
#                                         levels=c("1", "2", "3"))

#----- Produce panel 1a

fig_1a_plot = ggplot() +
  geom_sf(data=world_shp, fill="grey75", lwd=0.04, color="white") +
  # geom_sf(data=world_shp, fill="white") +
  geom_sf(data=world_shp_data, aes(fill=endemic_status), color="white", alpha=0.8) +
  scale_fill_manual(name="Status of countries \nwith survey responses   ", values = col_pal, labels=c("Endemic   ", "In-progress   ", "Rabies-free")) +
  # scale_color_manual(name="Status of countries \nwith survey responses", values = col_pal, guide=guide_legend(order=1)) +
  coord_sf() +
  theme_void() +
  theme(legend.position = "top", legend.title = element_text(size=12),
        legend.text = element_text(size=11))
fig_1a_plot

#----- Produce panel 1b

fig_1b_plot = ggplot(data=fig_1b, aes(x=work_sector, y=n, fill=endemic_status)) +
  geom_col(colour="white") +
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
  geom_col() +
  scale_fill_manual(name="Status of countries \nwith survey responses", values = col_pal, guide=guide_legend(order=1)) +
  labs(title="Impact on mass dog vaccination", x="Extent of disruption", y="Number of respondents\n") +
  scale_y_continuous(limits=c(0,40)) +
  theme_classic()
fig_2a_plot

#----- Produce panel 2b

fig_2b_plot = ggplot(data=fig_2b, aes(x=result, y=n, fill=endemic_status)) +
  geom_col() +
  scale_fill_manual(name="Status of countries \nwith survey responses", values = col_pal, guide=guide_legend(order=1)) +
  labs(title="Impact on access to post-exposure prophylaxis", x="Extent of disruption", y="Number of respondents\n") +
  scale_y_continuous(limits=c(0,40)) +
  theme_classic()
fig_2b_plot

#----- Produce panel 2c

fig_2c_plot = ggplot(data=fig_2c, aes(x=result, y=n, fill=endemic_status)) +
  geom_col() +
  scale_fill_manual(name="Status of countries \nwith survey responses", values = col_pal, guide=guide_legend(order=1)) +
  labs(title="Impact on children's awareness activities", x="Extent of disruption", y="Number of respondents\n") +
  scale_y_continuous(limits=c(0,40)) +
  theme_classic()
fig_2c_plot

# Combine plots and save output
ggarrange(fig_2a_plot, fig_2b_plot, fig_2c_plot, ncol=1, common.legend = TRUE,
          labels=c("A", "B", "C"))
          #labels=c("A: Impact on mass dog vaccination",
          #         "B: Impact on access to post-exposure prophylaxis",
          #         "C: Impact on children's awareness activities"))
ggsave("figs/Figure_2.pdf", height=10, width=10)
