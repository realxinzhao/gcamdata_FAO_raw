
#region/country & itemdefinition downloaded from FAOSTAT
#http://www.fao.org/faostat/en/#definitions
FAO_ctry <- readr::read_csv("data_raw/FAOSTAT_data_region_4-14-2021.csv") %>%
  mutate(Country = iconv(Country, to = 'ASCII//TRANSLIT'),
         Country = gsub("\\'", "", Country))
FAO_item <- readr::read_csv("data_raw/FAOSTAT_data_item_4-16-2021.csv") %>%
  select(FAO_domain_code = "Domain Code", "Domain",
         `item codes` = "Item Code", item = "Item")


#FAO regions with country code > 350 are aggregated regions, including China (Taiwan, Hongkong, Macau, & mainland)
FAO_ctry_Agg <- FAO_ctry %>% filter(`Country Code` > 350) %>% pull(Country)
unique(FAO_ctry$Country)

#current GCAM mappings
AGLU_ctry <- readr::read_csv("input/GCAMv5.3/aglu/AGLU_ctry.csv", comment = "#")
AGLU_Ctry_Unique <-distinct(AGLU_ctry,FAO_country,.keep_all = TRUE)

GCAM_FAO_item <- readr::read_csv("input/GCAMv5.3/aglu/FAO/FAO_ag_items_PRODSTAT.csv", comment = "#")

# setdiff(
#   unique(GCAM_FAO_item$item),
#   lookup(.lookupvalue = "QC",.lookup_df = FAO_item,
#          .lookup_col = "FAO_domain_code",.target_col = "item") %>% pull()
# )
# setdiff(
#   lookup(.lookupvalue = "QC",.lookup_df = FAO_item,
#          .lookup_col = "FAO_domain_code",.target_col = "item") %>% pull(),
#   unique(GCAM_FAO_item$item)
#
# )



Ctry_FAO_replace <- data.frame(
  countries = c("Eswatini", "Yemen Ar Rp", "Yemen Dem",
                "Unspecified Area", "Johnston Island",
                "French Southern Territories", "French Guyana",
                "Pitcairn", "Saint Helena, Ascension and Tristan da Cunha",
                "South Georgia and the South Sandwich Islands",
                "Sint Maarten (Dutch part)",
                "Saint-Martin (French part)",
                "United Kingdom of Great Britain and Northern Ireland",
                "North Macedonia"),
  FAO_country = c(rep("ctry_rm",12),
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


#Draw information from a dataset
FAO_tbl_summary <- function(.tbl, col.cnty = "countries", col.item = "item"){
  assert_that(is.character(.tbl))

  get(.tbl) %>% gcamdata::gather_years() -> .tbl1
  list(
    name = .tbl,
    ncountry = length(unique(.tbl1[col.cnty])%>% pull()),
    nitem = length(unique(.tbl1[col.item])%>% pull()),
    nyear = length(unique(.tbl1$year)),
    start_year = min(unique(.tbl1$year)),
    end_year = max(unique(.tbl1$year)) ,
    NA_perc = paste0(round(.tbl1 %>%
                             summarise(sum(is.na(value))/n()) %>%
                             as.numeric() *100 , 1), "%")
  )
}

#************************************
#Create a summary table including datasets update information

# need a list of df being loaded
update_summary <- function(data_map, replace_out = F){
  assert_that(is.data.frame(data_map))
  assert_that(is.logical(replace_out))

  lapply(data_map %>% pull(name), function(df){
    if (df == "FAO_BilateralTrade") {
      data.frame(t(sapply(FAO_tbl_summary(df, "Reporter.Countries", "Item") %>% unlist(),c)))
    } else {
      data.frame(t(sapply(FAO_tbl_summary(df) %>% unlist(),c)))
    }
  }) %>% bind_rows() -> data_summary

  data_summary %>%
    left_join(data_map %>%
                left_join(FAOsearch() %>% select(datasetcode, "FAO_update_date" = dateupdate),
                          by = c( "FAO_domain_code" = "datasetcode")),
              by = "name") %>% mutate(log_date = Sys.Date()) -> log_summary
  if (isTRUE(replace_out)) {
    readr::write_csv(log_summary,"output/log_summary.csv")
  }
  return(log_summary)
}












