point_map = function(map_bg = base_map, dataframe, map_title){

  # Update base map appearence
  basic_map <- map_bg +
    scale_color_manual(values=col_pal) +
    scale_fill_manual(values=fill_pal) +
    scale_shape_manual(values=shap_pal) +
    theme(legend.position = "none")

  # Add points to map
  print(basic_map + geom_sf(data=dataframe,
                      aes(color=shap, fill=col, shape=shap),
                      stroke=1, size=2) +
          ggtitle(map_title))
}