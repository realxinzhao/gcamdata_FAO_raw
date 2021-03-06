library(dplyr)
library(tidyr)
library(assertthat)
source("R/fn.faostat.R")
source("R/FAO_Check.R")


#fao_metadata <- FAOsearch()
data_folder <- "data_raw"
out_dir <- "output"
download <- F
dir.create(data_folder)
dir.create("output")

#Check log first
readr::read_csv("output/log_summary.csv") -> log_summary

#data needed in gcam aglu/FAO & corresponding FAO domain
GCAM_FAO <- list("FAO_GDP_deflators",
                 "FAO_ag_an_ProducerPrice",
                 "FAO_BilateralTrade",
                 "FAO_For_Exp_m3_USD_FORESTAT",
                 "FAO_For_Imp_m3_FORESTAT",
                 "FAO_For_Prod_m3_FORESTAT",
                 "FAO_For_Exp_m3_FORESTAT")
FAO_domain_code = c("PD", "PP", "TM", rep("FO",4))

data_map <- data.frame(name = GCAM_FAO %>% unlist(), FAO_domain_code)



#************************************
#*PD:  FAO_GDP_deflators
code = "PD"
get_faostat_bulk(code = code, data_folder = data_folder, download = download) -> PD
readr::read_csv("data_raw/GDP_deflator_Taiwan.csv",comment = "#") -> GDP_deflator_Taiwan

FAO_GDP_deflators <- PD %>%
  filter(item == "GDP Deflator", grepl("US\\$", element) ) %>%
  select(countries = area,`country codes` = `area code`,
         item, element, year, value) %>%
  bind_rows(GDP_deflator_Taiwan %>%
              mutate(countries = "China, Taiwan Province of",
                     `country codes` = 214,
                     item = "GDP Deflator",
                     element = "Value US$, 2015 prices",
                     value = round(100 * value / value[year == 2015], 2)) ) %>%
  spread(year, value) %>%
  FAO_ctry_remap()

col_type = paste0("cicc", paste0(rep("n",FAO_tbl_summary("FAO_GDP_deflators")["nyear"]), collapse = ""))

fn <- "FAO_GDP_Deflators.csv"
fqfn <- paste0("output/", fn)
suppressWarnings(file.remove(fqfn))
cmnts <- c(
  paste0("File: ", fn),
  "Title: FAO GDP deflators by country (2015 = 100)",
  "Units: Unitless",
  paste0("Source: FAOSTAT (domain:", code ," FAO.udpate:",FAOsearch(code = code)$dateupdate,")", " & Taiwan Statistics (access:4-12-2021)"),
  paste0("Date of last update: ", Sys.Date()),
  paste0("Column types: ",col_type) ,
  "----------"
)
cat(paste("#", cmnts), file = fqfn, sep = "\n", append = TRUE)
readr::write_csv(FAO_GDP_deflators, fqfn, append = TRUE, col_names = TRUE, na = "")


#************************************
#*PP:  FAO_ag_an_ProducerPrice
code = "PP"
get_faostat_bulk(code = code, data_folder = data_folder, download = download) -> PP

unique(PP$element_code)

FAO_ag_an_ProducerPrice <- PP %>%
  filter(grepl("USD/tonne|Index", element)) %>%
  select(countries = area,
         `country codes` = `area code`,
         item, `item codes` = `item code`,
         element,
         year, value) %>%
  spread(element, value) %>%
  left_join(
    PP %>% filter(grepl("USD/tonne|Index", element)) %>%
      select(countries = area,
             `country codes` = `area code`,
             item, `item codes` = `item code`,
             element,
             year, value) %>%
      spread(element, value) %>%
      rename(pp_base = `Producer Price (USD/tonne)`,
             pp_baseindex = `Producer Price Index (2014-2016 = 100)`) %>%
      filter(!is.na(pp_base)) %>%
      group_by(countries, `country codes`, item) %>%
      filter(year == last(year)) %>% within(rm(year))
  ) %>% mutate(
    `Producer Price (USD/tonne)` = if_else(is.na(`Producer Price (USD/tonne)`),
                                           pp_base* `Producer Price Index (2014-2016 = 100)` /pp_baseindex,
                                           `Producer Price (USD/tonne)`)
  )  %>%
  transmute(
    countries, `country codes`, item, `item codes`,
           element = "Producer Price (USD/tonne)",
          `element codes` = 5532,
           year, value = `Producer Price (USD/tonne)`
  ) %>% spread(year, value) %>%
  FAO_ctry_remap()

str(FAO_ag_an_ProducerPrice)

col_type = paste0("cicc", paste0(rep("n",FAO_tbl_summary("FAO_ag_an_ProducerPrice")["nyear"]), collapse = ""))

fn <- "FAO_ag_an_ProducerPrice.csv"
fqfn <- gzfile(paste0("output/", fn, ".gz"))

cmnts <- c(
  paste0("File: ", fn, ".gz"),
  "Title: FAO producer prices of primary agricultural and animal commodities by country.item.year",
  "Units: USD per tonne",
  paste0("Source: FAOSTAT (domain:", code ," FAO.udpate:",FAOsearch(code = code)$dateupdate,")"),
  paste0("Date of last update: ", Sys.Date()),
  paste0("Column types: ",col_type) ,
  "----------"
)
cat(paste("#", cmnts), file = fqfn, sep = "\n", append = TRUE)
readr::write_csv(FAO_ag_an_ProducerPrice,fqfn, append = TRUE, col_names = TRUE,na = "")


#************************************
#*TM: FAO_BilateralTrade
code = "TM"
get_faostat_bulk(code = code, data_folder = data_folder, download = download) -> TM

FAO_BilateralTrade <- TM %>%
  filter(element %in% c("Import Quantity", "Export Quantity"),
              year >= 2008, year <= 2017)  %>%
  select(Reporter.Country.Code = `reporter country code`,
             Reporter.Countries = `reporter countries`,
             Partner.Country.Code = `partner country code`,
             Partner.Countries = `partner countries`,
             Item.Code = `item code`,
             Item = item,
             Element.Code = `element code`,
             Element = element,
             Unit = unit,
             year, value) %>%
  spread(year, value) %>%
  FAO_ctry_remap("Reporter.Countries") %>%
  FAO_ctry_remap("Partner.Countries")

nyears = length(unique(TM$year)[unique(TM$year)>= 2008 & unique(TM$year)<= 2017])
col_type = paste0("icicicicc", paste0(rep("n",nyears), collapse = ""))

fn <- "FAO_BilateralTrade.csv"
fqfn <- gzfile(paste0("output/", fn, ".gz"))


cmnts <- c(
  paste0("File: ", fn, ".gz"),
  "Title: FAO bilateral trade matrix by commodity and year",
  "Units: tonne",
  paste0("Source: FAOSTAT (domain:", code ," FAO.udpate:",FAOsearch(code = code)$dateupdate,")"),
  paste0("Date of last update: ", Sys.Date()),
  paste0("Column types: ",col_type) ,
  "----------"
)
cat(paste("#", cmnts), file = fqfn, sep = "\n", append = TRUE)
readr::write_csv(FAO_BilateralTrade,fqfn, append = TRUE, col_names = TRUE,na = "")





#************************************
#*Forest economics
#*FO:  FAO_For_Prod_m3_FORESTAT
code = "FO"
get_faostat_bulk(code = code, data_folder = data_folder, download = download) -> FO


unique(FO$element)

FAO_For_Prod_m3_FORESTAT <- FO %>%
  filter(element == "Production", item == "Roundwood") %>%
  select(countries = area,`country codes` = `area code`,
         item, `item codes` = `item code`, element, `element codes` = `element code`,
         year, value) %>%
  spread(year, value) %>%
  FAO_ctry_remap()

nyears = length(FAO_For_Prod_m3_FORESTAT) - 6
col_type = paste0("cicici", paste0(rep("n",nyears), collapse = ""))

fn <- "FAO_For_Prod_m3_FORESTAT.csv"
fqfn <- paste0("output/", fn)
suppressWarnings(file.remove(fqfn))
cmnts <- c(
  paste0("File: ", fn),
  "Title: FAO forestry production (roundwood total) by country.year",
  "Units: m3",
  paste0("Source: FAOSTAT (domain:", code ," FAO.udpate:",FAOsearch(code = code)$dateupdate,")"),
  paste0("Date of last update: ", Sys.Date()),
  paste0("Column types: ",col_type) ,
  "----------"
)
cat(paste("#", cmnts), file = fqfn, sep = "\n", append = TRUE)
readr::write_csv(FAO_For_Prod_m3_FORESTAT, fqfn, append = TRUE, col_names = TRUE, na = "")

#************************************
#*FO:  FAO_For_Imp_m3_FORESTAT

FAO_For_Imp_m3_FORESTAT <- FO %>%
  filter(element == "Import Quantity", item == "Roundwood") %>%
  select(countries = area,`country codes` = `area code`,
         item, `item codes` = `item code`, element, `element codes` = `element code`,
         year, value) %>%
  spread(year, value) %>%
  FAO_ctry_remap()

col_type = paste0("cicici", paste0(rep("n",FAO_tbl_summary("FAO_For_Imp_m3_FORESTAT")["nyear"]), collapse = ""))

fn <- "FAO_For_Imp_m3_FORESTAT.csv"
fqfn <- paste0("output/", fn)
suppressWarnings(file.remove(fqfn))
cmnts <- c(
  paste0("File: ", fn),
  "Title: FAO forestry imports (roundwood total) by country.year",
  "Units: m3",
  paste0("Source: FAOSTAT (domain:", code ," FAO.udpate:",FAOsearch(code = code)$dateupdate,")"),
  paste0("Date of last update: ", Sys.Date()),
  paste0("Column types: ",col_type) ,
  "----------"
)
cat(paste("#", cmnts), file = fqfn, sep = "\n", append = TRUE)
readr::write_csv(FAO_For_Imp_m3_FORESTAT, fqfn, append = TRUE, col_names = TRUE, na = "")


#************************************
#*FO:  FAO_For_Exp_m3_USD_FORESTAT
code = "FO"
get_faostat_bulk(code = code, data_folder = data_folder, download = download) -> FO

FO %>%
  filter(element %in% c("Export Value", "Export Quantity"),
         item %in% c("Roundwood"))%>%
  mutate(element = paste0(element, " (", unit, ")") ) %>%
  select(-unit) %>%
  select(countries = area,
        `country codes` = `area code`,
        item,
        `item codes` = `item code`,
        element ,
        `element codes` = `element code`,
        year, value) %>%
  as_tibble() %>%
  spread(year, value) %>%
  FAO_ctry_remap()->
  FAO_For_Exp_m3_USD_FORESTAT


col_type = paste0("cicici", paste0(rep("n",FAO_tbl_summary("FAO_For_Exp_m3_USD_FORESTAT")["nyear"]), collapse = ""))

fn <- "FAO_For_Exp_m3_USD_FORESTAT.csv"
fqfn <- paste0("output/", fn)
suppressWarnings(file.remove(fqfn))
cmnts <- c(
  paste0("File: ", fn),
  "Title: FAO forests export qunatity and export value by country.year",
  "Units: Export Quantity in m3; Export Value in 1000 US$",
  paste0("Source: FAOSTAT (domain:", code ," FAO.udpate:",FAOsearch(code = code)$dateupdate,")"),
  paste0("Date of last update: ", Sys.Date()),
  paste0("Column types: ",col_type) ,
  "----------"
)
cat(paste("#", cmnts), file = fqfn, sep = "\n", append = TRUE)
readr::write_csv(FAO_For_Exp_m3_USD_FORESTAT,fqfn, append = TRUE, col_names = TRUE,na = "")


#************************************
#*FO:  FAO_For_Exp_m3_FORESTAT
code = "FO"
FAO_For_Exp_m3_FORESTAT <- FAO_For_Exp_m3_USD_FORESTAT %>%
  filter(element == "Export Quantity (m3)")

col_type = paste0("cicici", paste0(rep("n",FAO_tbl_summary("FAO_For_Exp_m3_FORESTAT")["nyear"]), collapse = ""))

fn <- "FAO_For_Exp_m3_FORESTAT.csv"
fqfn <- paste0("output/", fn)
suppressWarnings(file.remove(fqfn))
cmnts <- c(
  paste0("File: ", fn),
  "Title: FAO forests export qunatity by country.year",
  "Units: Export Quantity in m3",
  paste0("Source: FAOSTAT (domain:", code ," FAO.udpate:",FAOsearch(code = code)$dateupdate,")"),
  paste0("Date of last update: ", Sys.Date()),
  paste0("Column types: ",col_type) ,
  "----------"
)
cat(paste("#", cmnts), file = fqfn, sep = "\n", append = TRUE)
readr::write_csv(FAO_For_Exp_m3_FORESTAT,fqfn, append = TRUE, col_names = TRUE,na = "")

#************************************
#Check mapping
lapply(list(FAO_GDP_deflators$countries,
            FAO_ag_an_ProducerPrice$countries,
            FAO_BilateralTrade$Reporter.Countries,
            FAO_BilateralTrade$Partner.Countries,
            FAO_For_Exp_m3_USD_FORESTAT$countries,
            FAO_For_Imp_m3_FORESTAT$countries,
            FAO_For_Prod_m3_FORESTAT$countries,
            FAO_For_Exp_m3_FORESTAT$countries), function(cnty){
              setdiff(
                unique(cnty),
                unique(AGLU_Ctry_Unique$FAO_country)
              )
            })

#************************************
#update summary
update_summary(data_map, replace_out = F)



