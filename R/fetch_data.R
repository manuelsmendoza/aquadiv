#' Download seafood production from FAOSTAT
#'
#'\code{get_production} download the seafood production dataset from the FAOSTAT repository
#'and summaries the production by country and source (aquaculture or capture)
#'
#'@return A list containing the production from aquaculture industry and the captures from wild stocks
#'@export
#'@importFrom magrittr %>%
get_production <- function() {
  # Fetch production data from FAO repo
  data_url <- "https://www.fao.org/fishery/static/Data/GlobalProduction_2023.1.1.zip"

  if (httr::HEAD(data_url)$status_code == 200) {
    download.file(url = data_url, destfile = file.path(tempdir(), "fao_prod.zip"))
  } else {
    stop("Data missed")
  }

  # Load the data
  prod_tbl <- readr::read_csv(file = unzip(zipfile = file.path(tempdir(), "fao_prod.zip"), files = "Global_production_quantity.csv", exdir = tempdir())) %>%
    dplyr::select(COUNTRY.UN_CODE, SPECIES.ALPHA_3_CODE, PRODUCTION_SOURCE_DET.CODE, PERIOD, VALUE) %>%
    dplyr::rename("CTCODE" = COUNTRY.UN_CODE, "SPCODE" = SPECIES.ALPHA_3_CODE, "SOURCE" = PRODUCTION_SOURCE_DET.CODE, "YEAR" = PERIOD, "QUANT" = VALUE) %>%
    dplyr::mutate(SOURCE = dplyr::case_when(SOURCE == "CAPTURE" ~ stringr::str_to_title(SOURCE), TRUE ~ "Aquaculture")) %>%
    dplyr::group_by(CTCODE, SPCODE, SOURCE, YEAR) %>%
    dplyr::summarise(QUANT = sum(QUANT)) %>%
    dplyr::group_by() %>%
    dplyr::inner_join(readr::read_csv(file = unzip(zipfile = file.path(tempdir(), "fao_prod.zip"), files = "CL_FI_COUNTRY_GROUPS.csv", exdir = tempdir())) %>%
                        dplyr::select(UN_Code, Name_En, GeoRegion_Group_En) %>%
                        dplyr::rename("CTCODE" = UN_Code, "CTNAME" = Name_En, "CTREGION" = GeoRegion_Group_En)) %>%
    dplyr::inner_join(readr::read_csv(file = unzip(zipfile = file.path(tempdir(), "fao_prod.zip"), files = "CL_FI_SPECIES_GROUPS.csv", exdir = tempdir())) %>%
                        dplyr::select(`3A_Code`, Scientific_Name, ISSCAAP_Group_En) %>%
                        dplyr::rename("SPCODE" = `3A_Code`, "SPNAME" = Scientific_Name, "SPGROUP" = ISSCAAP_Group_En)) %>%
    dplyr::select(CTNAME, CTREGION, SPNAME, SPGROUP, SOURCE, YEAR, QUANT)

  # Split data by source
  return(list("aquaculture" = prod_tbl %>% dplyr::filter(SOURCE == "Aquaculture") %>% dplyr::select(!SOURCE),
              "capture"     = prod_tbl %>% dplyr::filter(SOURCE == "Capture")     %>% dplyr::select(!SOURCE)))
}



#' Get the complete taxonomic classification
#'
#' \code{get_taxonomy} wraps the taxonomic information from NCBI Taxonomy database
#'
#' @return The complete taxonomic classification from a scientific name.
#' @export
get_taxonomy <- function(scname) {
  # Prepare the dataset
  if (!file.exists(file.path(tempdir(), "accessionTaxa.sql"))) {
    taxonomizr::getNamesAndNodes(outDir = tempdir())
    taxonomizr::getAccession2taxid(outDir = tempdir())
    taxonomizr::read.names.sql(file.path(tempdir(), "names.dmp"), file.path(tempdir(), "accessionTaxa.sql"))
    taxonomizr::read.nodes.sql(file.path(tempdir(), "nodes.dmp"), file.path(tempdir(), "accessionTaxa.sql"))
    taxonomizr::read.accession2taxid(list.files(tempdir(), paste(tempdir(), "accession2taxid.gz$", sep = "/")), file.path(tempdir(), "accessionTaxa.sql"))
  }

  # Get NCBI Taxonomy ID (NCBI:txid)
  txid <- taxonomizr::getId(taxa = scname, sqlFile = file.path(tempdir(), "accessionTaxa.sql"), onlyScientific = TRUE)
  txid <- stringr::str_replace(string = txid, pattern = ",.*", replacement = "")

  # Translate txid to taxonomy classification
  taxo <- taxonomizr::getTaxonomy(ids = txid, sqlFile = file.path(tempdir(), "accessionTaxa.sql"))
  taxo <- tibble::as_tibble(taxo)

  # Return the taxonomic information
  return(list("query" = tibble::tibble("name" = scname, "id" = txid), "classification" = taxo))
}
