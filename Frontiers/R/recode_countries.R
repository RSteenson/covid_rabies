recode_countries = function(dataframe, country_col){

  dataframe[[country_col]][which(dataframe[[country_col]]=="Tanzania")] <- "United Republic of Tanzania"
  dataframe[[country_col]][which(dataframe[[country_col]]=="USA")] <- "United States of America"
  # dataframe[[country_col]][which(dataframe[[country_col]]=="Taiwan")] <- "China"
  # dataframe[[country_col]][which(dataframe[[country_col]]=="Taiwan Province of China")] <- "China"
  dataframe[[country_col]][which(dataframe[[country_col]]=="Laos")] <- "Lao People's Democratic Republic"
  dataframe[[country_col]][which(dataframe[[country_col]]=="Guinea Bissau")] <- "Guinea-Bissau"
  dataframe[[country_col]][which(dataframe[[country_col]]=="Brunei")] <- "Brunei Darussalam"
  dataframe[[country_col]][which(dataframe[[country_col]]=="Eswatini")] <- "Swaziland"

  return(dataframe)
}