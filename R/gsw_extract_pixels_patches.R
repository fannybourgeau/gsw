#' Extract change_raster pixels in polygon and summarise the results
#' @param polygon the sf polygon in which to extract values
#' @param change_raster the GSW change intensity raster
#' @return a summary table by type of change ("neg","neutral", or "pos")
#' @export
extract_values_in_polygon <- function(polygon,
                                      raster_change) {
  sf::sf_use_s2(FALSE)
  pol=vect(polygon)
  tib_values <- tibble::tibble(values=terra::extract(raster_change,pol)[,2]-100) %>% 
    dplyr::mutate(type=dplyr::case_when(values>=-10 & values <=10~"neutral",
                                        values>10~"pos",
                                        values<(-10)~"neg")) %>% 
    dplyr::mutate(type=factor(type,levels=c("neg","neutral","pos")))
  tib_values_summary=tib_values %>% 
    dplyr::filter(!is.na(type)) %>% 
    dplyr::group_by(type) %>% 
    dplyr::summarise(ntype=dplyr::n(),
                     mutype=mean(values),
                     sdtype=sd(values))
  return(tib_values_summary)
}

#' Extract 1000 pixels of GSW change intensity per zone x reach polygon for a city
#' @param thisCityCode citycode
#' @param study_areas the sf polygons describing reach and zone per city
#' @param dir_images the directory containing GSW images
#' @return a table with gsw_pixels results for the city
#' @export
gsw_extract_pixels_patches=function(thisCityCode,study_areas,dir_images="data-raw/images"){
  dir_images="data-raw/images"
  table_polygones=study_areas %>%
    dplyr::filter(CityCode==thisCityCode) %>% 
    dplyr::mutate(area_tot=purrr::map_dbl(geometry,~as.numeric(st_area(.x))))
  # # Get change and entropy rasters
  raster_change <- terra::rast(glue::glue("{dir_images}/change_{thisCityCode}.tif"))
  raster_labels <- terra::rast(glue::glue("{dir_images}/labels_{thisCityCode}.tif"))
  mask_raster <- raster_change >= 0 & raster_change <= 200
  polygons <- terra::as.polygons(raster_labels*mask_raster, values=TRUE) %>%
    sf::st_as_sf()
  colnames(polygons)[1]="label"
  labelprops=readr::read_csv(glue::glue("{dir_images}/labelprops_{thisCityCode}.csv"))
  polygons=polygons %>% 
    dplyr::left_join(labelprops,by="label")
  
  sf::sf_use_s2(FALSE)
  intersection_to_zonereach=sf::st_intersection(polygons,table_polygones) %>% 
    dplyr::select(CityCode,reach,zone,label,num_pixels,eccentricity,solidity) %>% 
    dplyr::filter(label!=0) 
  
  
  result=intersection_to_zonereach %>%
    dplyr::mutate(data=purrr::map(.x=geometry,
                                  .f=extract_values_in_polygon,
                                  raster_change=raster_change)) %>%  
    tidyr::unnest(cols=c(data))%>% 
    tidyr::pivot_wider(names_from=type,values_from=c(ntype,mutype,sdtype),values_fill=0)
  return(result)
}

#' get gsw_patches results for the city
#' @param thisCityCode citycode
#' @param conn connection
#' @return a table with gsw_patches results for the city
#' @export
gsw_get_patches=function(thisCityCode,conn){
  conn=glourbi::connect_to_glourb()
  sql <- "SELECT * FROM gsw_patches WHERE citycode LIKE ?thisCityCode"
  query <- sqlInterpolate(conn, sql, thisCityCode = thisCityCode)
  result <- sf::st_read(dsn = conn, query = query) 
  return(result)
}

#' get gsw_patches_summary results for the city
#' @param thisCityCode citycode
#' @param conn connection
#' @return a table with gsw_patches_summary results for the city
#' @export
gsw_summarise_patches=function(thisCityCode,conn){
  
  sf::sf_use_s2(FALSE)
  result=gsw_get_patches(thisCityCode,conn) %>%
    dplyr::mutate(sizepatch=dplyr::case_when(num_pixels<=20~"1_small",
                                             num_pixels>20 & num_pixels<max(num_pixels, na.rm=TRUE)~"2_medium",
                                             num_pixels==max(num_pixels,na.rm=TRUE)~"3_largest")) %>% 
    dplyr::group_by(sizepatch,reach,zone) %>%
    tidyr::nest() %>%
    dplyr::mutate(geometry=purrr::map(data,~st_union(.x$geometry))) %>%
    dplyr::mutate(data=purrr::map(data,~sf::st_drop_geometry(.x) %>% 
                                  summarise(npatches=dplyr::n(),
                                            ntype_pos=sum(ntype_pos),
                                            ntype_neutral=sum(ntype_neutral),
                                            ntype_neg=sum(ntype_neg),
                                            mutype_pos=mean(mutype_pos, na.rm=T),
                                            mutype_neutral=mean(mutype_neutral, na.rm=T),
                                            mutype_neg=mean(mutype_neg, na.rm=T),
                                            eccentricity=mean(eccentricity, na.rm=T),
                                            solidity=mean(solidity, na.rm=T)))) %>% 
    tidyr::unnest(cols=c(data)) %>% 
    tidyr::unnest(cols=c(geometry)) %>%
    dplyr::ungroup() %>% 
    sf::st_as_sf() %>% 
    
    dplyr::mutate(area=purrr::map_dbl(geometry,~as.numeric(sf::st_area(.x))),
                  perimeter=purrr::map_dbl(geometry,~sf::st_perimeter(.x))) 
    return(result)
}

