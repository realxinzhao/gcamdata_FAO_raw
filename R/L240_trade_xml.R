#rm(list = ls())
library(gcamdata)
source("R/L240_trade_info.R") # info in constant.R or functions in gcamdata
library(dplyr)
library(tidyr)
library(assertthat)
library(tibble)

#Objective: generate Agtrade.xml

get_data <- function(data.dir, fn){
  assign(fn,
         readr::read_csv(paste0(file.path(data.dir, fn), ".csv"),
                         comment = "#"), envir = .GlobalEnv )
  }

lapply(
  gsub("aglu/", "",
       c(FILE = "aglu/A_agRegionalSector",
         FILE = "aglu/A_agRegionalSubsector",
         FILE = "aglu/A_agRegionalTechnology",
         FILE = "aglu/A_agTradedSector",
         FILE = "aglu/A_agTradedSubsector",
         FILE = "aglu/A_agTradedTechnology") ),
  FUN = function(file){
    get_data("input/GCAMv5.3/aglu", file)})


lapply(
  gsub("common/", "",
       c(FILE = "common/GCAM_region_names",
         FILE = "common/iso_GCAM_regID") ),
  FUN = function(file){
    get_data("input/GCAMv5.3/common", file)})

c("L109.ag_ALL_Mt_R_C_Y",
  "L109.an_ALL_Mt_R_C_Y",
  "L110.For_ALL_bm3_R_Y",
  "L100.FAO_For_Exp_m3",
  "L1091.GrossTrade_Mt_R_C_Y") -> gcamdata_outfiles
lapply(X = gcamdata_outfiles, FUN = function(file){
  get_data("input/GCAMv5.3/gcamdata_output", file)})


#################################
# traded sector/data processing code from module_aglu_L240.ag_trade

# 0: Bind crops, livestock, and forest for prod and netexp
L109.ag_an_for_ALL_Mt_R_C_Y <- L109.ag_ALL_Mt_R_C_Y %>%
  select(GCAM_region_ID, GCAM_commodity, year, Prod_Mt, NetExp_Mt) %>%
  bind_rows(L109.an_ALL_Mt_R_C_Y %>%
              select(GCAM_region_ID, GCAM_commodity, year, Prod_Mt, NetExp_Mt)) %>%
  bind_rows(L110.For_ALL_bm3_R_Y %>%
              filter(GCAM_commodity %in% aglu.TRADED_FORESTS) %>%
              select(GCAM_region_ID, GCAM_commodity, year,
                     Prod_Mt = Prod_bm3, NetExp_Mt = NetExp_bm3)) #note that physical unit for forest data is bm3

# Adding forest trade data in L1091.GrossTrade_Mt_R_C_Y. Note that bilateral trade data are not used for now.
# FAO does not provide primary roundwood bilateral trade data. We use export data to back calculate gross trade.
# replace_na here only affect Taiwan, which we did not have trade data.
L1091.GrossTrade_Mt_R_C_Y <- L1091.GrossTrade_Mt_R_C_Y %>%
  bind_rows(L110.For_ALL_bm3_R_Y %>%
              left_join(
                L100.FAO_For_Exp_m3 %>%
                  mutate(GCAM_region_ID = left_join_error_no_match(L100.FAO_For_Exp_m3, iso_GCAM_regID, by = c("iso"))[['GCAM_region_ID']],
                         GCAM_commodity = "Forest",                   # add the forest commodity label
                         value = CONV_M3_BM3 * value,                 # convert the value units from m3 to bm3, had to add this constant to constants.R
                         flow = "GrossExp") %>%
                  select(GCAM_region_ID, GCAM_commodity, flow, year, value) %>%
                  group_by(GCAM_region_ID, GCAM_commodity, flow, year) %>%
                  summarise(value = sum(value)) %>%
                  ungroup() %>%
                  spread(flow, value),
                by = c("GCAM_region_ID", "GCAM_commodity", "year")) %>%
              replace_na(list(GrossExp = 0)) %>%
              filter(GCAM_commodity %in% aglu.TRADED_FORESTS) %>%
              mutate(GrossImp_Mt = ifelse(GrossExp - NetExp_bm3 > 0, GrossExp - NetExp_bm3, 0),
                     GrossExp_Mt = ifelse(GrossExp - NetExp_bm3 > 0, GrossExp, NetExp_bm3)) %>%
              select(names(L1091.GrossTrade_Mt_R_C_Y)) )

# 1. TRADED SECTOR / SUBSECTOR / TECHNOLOGY")
# L240.Supplysector_tra: generic supplysector info for traded ag commodities
# By convention, traded commodity information is contained within the USA region (could be within any)
A_agTradedSector$region <- gcam.USA_REGION

# L240.Supplysector_tra: generic supplysector info for traded ag commodities
L240.Supplysector_tra <- mutate(A_agTradedSector, logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
  select(c(LEVEL2_DATA_NAMES[["Supplysector"]], "logit.type"))

# L240.SectorUseTrialMarket_tra: Create solved markets for the traded sectors
L240.SectorUseTrialMarket_tra <- select(A_agTradedSector, region, supplysector) %>%
  mutate(use.trial.market = 1)

# L240.SubsectorAll_tra: generic subsector info for traded ag commodities
# Traded commodities have the region set to USA and the subsector gets the region name pre-pended
L240.SubsectorAll_tra <- write_to_all_regions(A_agTradedSubsector,
                                              c(LEVEL2_DATA_NAMES[["SubsectorAll"]], "logit.type"),
                                              filter(GCAM_region_names, !region %in% aglu.NO_AGLU_REGIONS),
                                              has_traded = TRUE)

# Base technology-level table for several tables to be written out")
A_agTradedTechnology_R_Y <- repeat_add_columns(A_agTradedTechnology,
                                               tibble(year = MODEL_YEARS)) %>%
  repeat_add_columns(filter(GCAM_region_names, !region %in% aglu.NO_AGLU_REGIONS)) %>%
  mutate(subsector = paste(region, subsector, sep = " "),
         technology = subsector,
         market.name = region,
         region = gcam.USA_REGION)

# L240.TechShrwt_tra: Share-weights of traded technologies
L240.TechShrwt_tra <- select(A_agTradedTechnology_R_Y, LEVEL2_DATA_NAMES[["TechShrwt"]])

# L240.TechCost_tra: Costs of traded technologies
L240.TechCost_tra <- A_agTradedTechnology_R_Y %>%
  mutate(minicam.non.energy.input = "trade costs") %>%
  select(LEVEL2_DATA_NAMES[["TechCost"]])

# L240.TechCoef_tra: Coefficient and market name of traded technologies
L240.TechCoef_tra <- select(A_agTradedTechnology_R_Y, LEVEL2_DATA_NAMES[["TechCoef"]])

# L240.Production_tra: Output (gross exports) of traded technologies
L240.GrossExports_Mt_R_C_Y <- left_join_error_no_match(L1091.GrossTrade_Mt_R_C_Y,
                                                       GCAM_region_names,
                                                       by = "GCAM_region_ID") %>%
  select(region, GCAM_commodity, year, GrossExp_Mt)

L240.Production_tra <- filter(A_agTradedTechnology_R_Y, year %in% MODEL_BASE_YEARS) %>%
  left_join_error_no_match(L240.GrossExports_Mt_R_C_Y,
                           by = c(market.name = "region", minicam.energy.input = "GCAM_commodity", "year")) %>%
  rename(calOutputValue = GrossExp_Mt) %>%
  mutate(share.weight.year = year,
         subs.share.weight = if_else(calOutputValue > 0, 1, 0),
         tech.share.weight = subs.share.weight) %>%
  select(LEVEL2_DATA_NAMES[["Production"]])


############
# Base technology-level table for several tables to be written out")
A_agRegionalTechnology_R_Y <- repeat_add_columns(A_agRegionalTechnology,
                                                 tibble(year = MODEL_YEARS)) %>%
  repeat_add_columns(filter(GCAM_region_names["region"], !region %in% aglu.NO_AGLU_REGIONS)) %>%
  mutate(market.name = if_else(market.name == "regional", region, market.name))
L240.TechCoef_reg <- select(A_agRegionalTechnology_R_Y, LEVEL2_DATA_NAMES[["TechCoef"]])

#********
xml_dir <- "output/xml"; dir.create(xml_dir, showWarnings = FALSE, recursive = TRUE)

create_xml(file.path(xml_dir, "ag_trade1.xml")) %>%
  add_logit_tables_xml(L240.Supplysector_tra, "Supplysector") %>%
  add_xml_data(L240.SectorUseTrialMarket_tra, "SectorUseTrialMarket") %>%
  add_logit_tables_xml(L240.SubsectorAll_tra, "SubsectorAll", base_logit_header = "SubsectorLogit") %>%
  add_xml_data(L240.TechShrwt_tra, "TechShrwt") %>%
  add_xml_data(L240.TechCost_tra, "TechCost") %>%
  add_xml_data(L240.TechCoef_tra, "TechCoef") %>%
  add_xml_data(L240.Production_tra, "Production") %>%
  add_xml_data(L240.TechCoef_reg, "TechCoef") ->
  ag_trade.xml
gcamdata::run_xml_conversion(ag_trade.xml)

#*****************************
aglu.TRADED_CROPS.Bilateral0 <- tolower(c("Corn", "Rice", "Wheat", "OilCrop", "OtherGrain", "PalmFruit"))
aglu.TRADED_CROPS.Bilateral <-paste0("traded ", aglu.TRADED_CROPS.Bilateral0)
aglu.TRADED_CROPS.Bilateral1 <-paste0("imported ", aglu.TRADED_CROPS.Bilateral0)

#Change top Armington nest to regional markets
L240.TechCoef_reg %>% filter(subsector %in% aglu.TRADED_CROPS.Bilateral1) %>%
  mutate(market.name = region) %>% bind_rows(
    L240.TechCoef_reg %>% filter(!subsector %in% aglu.TRADED_CROPS.Bilateral1)
  ) -> L240.TechCoef_reg1


# repeat adding column to all regions for bilateral trade
Addtradedmarkets <- function(.tb){
  assign(paste0(.tb,"1"),
         get(.tb) %>%
           filter(!supplysector %in% aglu.TRADED_CROPS.Bilateral) %>%
           bind_rows(
             get(.tb) %>%
               filter(supplysector %in% aglu.TRADED_CROPS.Bilateral) %>%
               select(-region) %>%
               repeat_add_columns(filter(GCAM_region_names, !region %in% aglu.NO_AGLU_REGIONS) %>%
                                    select(region)) ),
         envir = parent.frame() )
  }

for (tb in c("L240.Supplysector_tra",
             "L240.SectorUseTrialMarket_tra",
             "L240.SubsectorAll_tra",
             "L240.TechShrwt_tra",
             "L240.TechCost_tra",
             "L240.TechCoef_tra")) {
  Addtradedmarkets(tb)
}

L240.TechCoef_tra1

Bilateral_export_share %>% group_by(GCAM_commodity, reporter) %>% summarise(exportshare = sum(exportshare))

lapply(unique(Bilateral_export_share$reporter), function(importor){
  L240.Production_tra %>% filter(supplysector %in% aglu.TRADED_CROPS.Bilateral) %>%
    mutate(region = importor) %>%
    left_join(Bilateral_export_share %>%
                mutate(supplysector = paste0("traded ", tolower(GCAM_commodity)),
                       subsector = paste(reporter, supplysector)) %>%
                filter(partner == importor) %>%
                select(-GCAM_commodity, -reporter),
              by = c("region" = "partner", "supplysector", "subsector")) %>%
    mutate(calOutputValue = exportshare  * calOutputValue)
  }) %>% bind_rows() %>%
  bind_rows(
    L240.Production_tra %>%
      filter(!supplysector %in% aglu.TRADED_CROPS.Bilateral) ) ->
  L240.Production_tra1

################

FAO_ag_items_TRADE <- readr::read_csv("input/GCAMv5.3/aglu/FAO/FAO_ag_items_TRADE.csv", comment = "#")

FAO_BilateralTrade %>%
  select(Reporter.Countries, Partner.Countries, Item.Code, Item, Element, as.character(aglu.TRADE_CAL_YEARS)) %>%
    filter(Element %in% c( "Import Quantity", "Export Quantity")) %>%
    gather_years(na.rm = TRUE) ->
  FAO_BilateralTrade


# 1: Filter and prepare the bi-lateral trade flow volume data by country and FAO commodity

# Filter to traded GCAM commodities
# left_join because many of the trade commodities don't map to GCAM commodities (e.g., silk worm cocoons)
L1091.BiTrade_t_ctry_item <- left_join(FAO_BilateralTrade, select(FAO_ag_items_TRADE, item.code, bitrade_commod, GCAM_commodity),
                                       by = c(Item.Code = "item.code")) %>%
  filter(!is.na(GCAM_commodity),
         GCAM_commodity %in% c(aglu.TRADED_CROPS, aglu.TRADED_MEATS)) %>%
  # Join the reporter and partner countries. 10/17/2017 this does produce some missing iso codes for partner
  # countries but they're all tiny. Many  (e.g., "Unspecified Area") do not have 3-digit iso codes anyway. They are
  # dropped by drop_na().
  # Note also - this uses left_join_keep_first_only for countries like the USSR with multiple associated present-day
  # iso codes. We wouldn't want to repeat the trade data by each post-dissolution country, and since none of these
  # actually exist during the time frame for which gross trade is being assessed, there's no benefit to downscaling.
  left_join_keep_first_only(select(AGLU_ctry, FAO_country, iso),
                            by = c( Reporter.Countries = "FAO_country")) %>%
  rename(iso.reporter = iso) %>%
  left_join_keep_first_only(select(AGLU_ctry, FAO_country, iso),
                            by = c( Partner.Countries = "FAO_country")) %>%
  rename(iso.partner = iso) %>%
  drop_na(iso.partner, iso.reporter)

#2. Re-balancing bilateral trade data
# The bilateral trade data are not symmetrical - some countries are partner countries but not reporter countries
# (e.g. Vietnam does not report trading with others, but appears as a partner countries for a number of others). In
# those missing cases, we use what's already available in the bilateral trade data, and simply flip reporter/partner
# countries, as well as export/import to get a symmetrical dataset.

# First, we find those missing cases by comparing the export and import lists
# The export list - reporter countries, partner countries, commodities, and year
exp.list <- L1091.BiTrade_t_ctry_item %>%
  filter(Element == "Export Quantity") %>%
  select(Reporter.Countries, Partner.Countries, bitrade_commod, year) %>%
  distinct()
# The import list - reporter countries, partner countries, commodities, and year
imp.list <- L1091.BiTrade_t_ctry_item %>%
  filter(Element == "Import Quantity") %>%
  select(Reporter.Countries, Partner.Countries, bitrade_commod, year) %>%
  distinct()

# The two lists should have the same number of observations -
# Partner countries in the export list should be reporter countries in the import list, and vice versa

# Find the partner countries that are missing to report import
L1091.BiTrade_t_ctry_item_missing_exp <- exp.list %>%
  anti_join(imp.list, by = c("bitrade_commod", "year",
                             "Reporter.Countries" = "Partner.Countries",
                             "Partner.Countries" = "Reporter.Countries")) %>%
  mutate(Element = "Export Quantity")
# Find the partner countries that are missing to report export
L1091.BiTrade_t_ctry_item_missing_imp <- imp.list %>%
  anti_join(exp.list, by = c("bitrade_commod", "year",
                             "Reporter.Countries" = "Partner.Countries",
                             "Partner.Countries" = "Reporter.Countries")) %>%
  mutate(Element = "Import Quantity")

# Filter those asymmetric observations in the original data, and flip reporter/partner, export/import
L1091.BiTrade_t_ctry_item_full <- L1091.BiTrade_t_ctry_item_missing_exp %>%
  bind_rows(L1091.BiTrade_t_ctry_item_missing_imp) %>%
  inner_join(L1091.BiTrade_t_ctry_item,
             by = c("Reporter.Countries", "Partner.Countries", "Element", "bitrade_commod", "year")) %>%
  mutate(Element = if_else(Element == "Import Quantity", "Export Quantity", "Import Quantity")) %>%
  select(Reporter.Countries = Partner.Countries, Partner.Countries = Reporter.Countries,
         iso.reporter = iso.partner, iso.partner = iso.reporter,
         Item.Code, Item, Element, bitrade_commod, GCAM_commodity, year, value) %>%
  bind_rows(L1091.BiTrade_t_ctry_item)

# 3. Deriving extra-regional trade flows by GCAM region and traded commodity, filtering out within-region trade

# Method: filter only the crops considered, join in the reporting region and partner region (from reporting and partner
# country), filter out where reporting and partner region are the same, convert units, aggregate by GCAM region and
# commodity, and calculate the net trade, and take an unweighted average of the years.
# Explanation: In multi-country GCAM regions, within-region trade is excluded.
iso_mapping_partner <- select(iso_GCAM_regID, iso, GCAM_region_ID) %>%
  rename(iso.partner = iso,
         GCAMreg.partner = GCAM_region_ID)
L1091.XregTrade_Mt_R_C <- L1091.BiTrade_t_ctry_item_full %>%
  filter(GCAM_commodity %in% c(aglu.TRADED_CROPS, aglu.TRADED_MEATS)) %>%
  left_join_error_no_match(select(iso_GCAM_regID, iso, GCAM_region_ID),
                           by = c("iso.reporter" = "iso")) %>%
  left_join_error_no_match(iso_mapping_partner,
                           by = "iso.partner") %>%
  filter(GCAM_region_ID != GCAMreg.partner) %>%
  mutate(value = value * CONV_T_MT,
         var = tolower(sub(" Quantity", "", Element))) %>%
  group_by(GCAM_region_ID, GCAMreg.partner, GCAM_commodity, var, year) %>%
  summarise(value = sum(value)) %>%
  group_by(GCAM_region_ID, GCAMreg.partner, GCAM_commodity, var) %>%
  summarise(value = mean(value)) %>%
  ungroup() %>%
  complete(GCAM_region_ID = unique(GCAM_region_ID),
           GCAMreg.partner = unique(GCAMreg.partner),
           GCAM_commodity = unique(GCAM_commodity),
           var = unique(var)) %>%
  replace_na(list(value = 0)) %>%
  spread(var, value) %>%
  mutate(net_trade = export - import) %>% left_join(GCAM_region_names) %>%
  left_join(GCAM_region_names %>% select(GCAMreg.partner = GCAM_region_ID, partner = region))

L1091.XregTrade_Mt_R_C %>%
  filter(tolower(GCAM_commodity) %in% aglu.TRADED_CROPS.Bilateral0) %>%
  group_by(region, GCAM_commodity) %>%
  mutate(exportshare = export / (sum(export))) %>% ungroup() %>%
  select(reporter = region, partner, GCAM_commodity, exportshare) ->
  Bilateral_export_share



# ===================================================

# Produce outputs
xml_dir <- "output/xml"; dir.create(xml_dir, showWarnings = FALSE, recursive = TRUE)

create_xml(file.path(xml_dir, "ag_trade_bi.xml")) %>%
  add_logit_tables_xml(L240.Supplysector_tra1, "Supplysector") %>%
  add_xml_data(L240.SectorUseTrialMarket_tra1, "SectorUseTrialMarket") %>%
  add_logit_tables_xml(L240.SubsectorAll_tra1, "SubsectorAll", base_logit_header = "SubsectorLogit") %>%
  add_xml_data(L240.TechShrwt_tra1, "TechShrwt") %>%
  add_xml_data(L240.TechCost_tra1, "TechCost") %>%
  add_xml_data(L240.TechCoef_tra1, "TechCoef") %>%
  add_xml_data(L240.Production_tra1, "Production") %>%
  add_xml_data(L240.TechCoef_reg1, "TechCoef") ->
  ag_trade.xml

ag_trade.xml %>%
  gcamdata::run_xml_conversion()
