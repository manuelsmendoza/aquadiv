get_production <- function() {
  # Fetch production data from FAO repo
  data_url <- "https://www.fao.org/fishery/static/Data/GlobalProduction_2023.1.1.zip"

  if (HEAD(data_url)$status_code == 200) {
    download.file(url = data_url, destfile = file.path(tempdir(), "fao_prod.zip"))
  } else {
    stop("Data missed")
  }

  # Load the data
  prod_tbl <- read_csv(file = unzip(zipfile = file.path(tempdir(), "fao_prod.zip"), files = "Global_production_quantity.csv", exdir = tempdir())) %>%
    select(COUNTRY.UN_CODE, SPECIES.ALPHA_3_CODE, PRODUCTION_SOURCE_DET.CODE, PERIOD, VALUE) %>%
    rename("CTCODE" = COUNTRY.UN_CODE, "SPCODE" = SPECIES.ALPHA_3_CODE, "SOURCE" = PRODUCTION_SOURCE_DET.CODE, "YEAR" = PERIOD, "QUANT" = VALUE) %>%
    mutate(SOURCE = case_when(SOURCE == "CAPTURE" ~ str_to_title(SOURCE), TRUE ~ "Aquaculture")) %>%
    group_by(CTCODE, SPCODE, SOURCE, YEAR) %>%
    summarise(QUANT = sum(QUANT)) %>%
    group_by() %>%
    inner_join(read_csv(file = unzip(zipfile = file.path(tempdir(), "fao_prod.zip"), files = "CL_FI_COUNTRY_GROUPS.csv", exdir = tempdir())) %>%
                 select(UN_Code, Name_En, GeoRegion_Group_En) %>%
                 rename("CTCODE" = UN_Code, "CTNAME" = Name_En, "CTREGION" = GeoRegion_Group_En)) %>%
    inner_join(read_csv(file = unzip(zipfile = file.path(tempdir(), "fao_prod.zip"), files = "CL_FI_SPECIES_GROUPS.csv", exdir = tempdir())) %>%
                 select(`3A_Code`, Scientific_Name, ISSCAAP_Group_En) %>%
                 rename("SPCODE" = `3A_Code`, "SPNAME" = Scientific_Name, "SPGROUP" = ISSCAAP_Group_En)) %>%
    select(CTNAME, CTREGION, SPNAME, SPGROUP, SOURCE, YEAR, QUANT)

  # Split data by source
  return(list("aquaculture" = prod_tbl %>% filter(SOURCE == "Aquaculture") %>% select(!SOURCE),
              "capture"     = prod_tbl %>% filter(SOURCE == "Capture")     %>% select(!SOURCE)))
}
