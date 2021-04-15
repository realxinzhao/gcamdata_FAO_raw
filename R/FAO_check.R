#region/country definition downloaded from FAOSTAT
FAO_ctry <- readr::read_csv("data_raw/FAOSTAT_data_4-14-2021.csv") %>%
  mutate(Country = iconv(Country, to = 'ASCII//TRANSLIT'),
         Country = gsub("\\'", "", Country))

#FAO regions with country code > 350 are aggregated regions, including China (Taiwan, Hongkong, Macau, & mainland)
FAO_ctry_Agg <- FAO_ctry %>% filter(`Country Code` > 350) %>% pull(Country)
unique(FAO_ctry$Country)

AGLU_ctry <- readr::read_csv("input/GCAMv5.3/aglu/AGLU_ctry.csv", comment = "#")
AGLU_Ctry_Unique <-distinct(AGLU_ctry,FAO_country,.keep_all = TRUE)


Ctry_FAO_replace <- data.frame(
  countries = c("Eswatini", "Yemen Ar Rp", "Yemen Dem",
                "Unspecified Area", "Johnston Island",
                "French Southern Territories", "French Guyana",
                "Pitcairn", "Saint Helena, Ascension and Tristan da Cunha",
                "South Georgia and the South Sandwich Islands",
                "Sint Maarten (Dutch part)",
                "United Kingdom of Great Britain and Northern Ireland",
                "North Macedonia"),
  FAO_country = c(rep("ctry_rm",11),
                  #"Sint Maarten (Dutch Part)",
                  "United Kingdom",
                  "The former Yugoslav Republic of Macedonia")
  )

#Maping FAO countries to AGLU_ctry and remove unimportant small regions
FAO_ctry_remap <- function(.df, .colname = "countries"){
  .df %>%
    rename("countries" = .colname) %>%
    filter(! countries %in% FAO_ctry_Agg) %>%
    mutate(countries = iconv(countries, to = 'ASCII//TRANSLIT'),
           countries = gsub("\\'", "", countries)
           ) %>%
    left_join(Ctry_FAO_replace, by = "countries") %>%
    filter(FAO_country != "ctry_rm" | is.na(FAO_country)) %>%
    mutate(countries = if_else(is.na(FAO_country) == F, FAO_country, countries)) %>%
    select(-FAO_country) %>%
    rename_(.dots=setNames(list("countries"), .colname))
}




