point_map = function(map_bg = base_map, dataframe, map_title){

  # Remove "none"
  dataframe = dataframe[which(dataframe$col != "none"),]

  # Set column as factor
  dataframe$col <- factor(dataframe$col,
                          levels=c("vet", "health", "surveillance", "cross-sectoral", "empty")) #, "none"))

  # Set map point colours and shapes
  col_pal = c("circle"="black", "cross"="red")
  fill_pal = c(sector_cols, "cross-sectoral"="black", "empty"="white") #, "none"="red")
  shap_pal = c("circle"=21, "cross"=4)

  # Update base map appearence
  basic_map <- map_bg +
    scale_fill_manual(values=fill_pal, breaks=c("vet", "health", "surveillance", "cross-sectoral"),
                      drop=FALSE, guide=guide_legend(override.aes=list(shape=21))) +
    scale_color_manual(values=col_pal, guide="none") +
    scale_shape_manual(values=shap_pal, guide="none") +
    theme(legend.title = element_blank())

  # Add points to map
  basic_map + geom_sf(data=dataframe,
                      aes(fill=col, color=shap, shape=shap),
                      stroke=0.7, size=3) +
          ggtitle(map_title)
  }