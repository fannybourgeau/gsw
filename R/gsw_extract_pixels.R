#' Extract 1000 pixels of GSW change intensity per zone x reach polygon for a city
#' @param thisCityCode citycode
#' @param dir_images the directory containing GSW images
#' @return a table with gsw_pixels results for the city
#' @export
gsw_extract_pixels=function(thisCityCode,dir_images="data-raw/images"){
  table_polygones=study_areas %>%
    dplyr::filter(CityCode==thisCityCode) %>%
    dplyr::mutate(npol=1:dplyr::n()) %>%
    dplyr::group_by(npol,CityCode,reach,zone) %>%
    tidyr::nest()
  # Get change raster
  raster_image_1 <- terra::rast(glue::glue("{dir_images}/change_{thisCityCode}.tif"))
 
  # Define mask based of values of raster_image_1
  mask_raster <- raster_image_1 >= 0 & raster_image_1 <= 200

  my_mask_polygons <- sf::st_as_sf(as.polygons(mask_raster, dissolve = TRUE))[2,]
  intersect_polygon=function(polygon,
                             mask_polygons=my_mask_polygons){

    sf::sf_use_s2(FALSE)
    polygon_intersection <- sf::st_intersection(polygon, mask_polygons) %>%
      sf::st_collection_extract("POLYGON")
    return(polygon_intersection)
  }
  extract_values_in_polygon <- function(polygon_intersection,
                                        raster_image_1,
                                        n=1000) {

    sf::sf_use_s2(FALSE)
    pol=vect(polygon_intersection)
    values_1 <- terra::extract(raster_image_1, pol)
    points_df=tibble::tibble(change = values_1[, 2])
    return(points_df)
  }
  result=table_polygones %>%
    mutate(intersect=purrr::map(.x=data,
                                .f=intersect_polygon,
                                mask_polygons=my_mask_polygons))%>%
    mutate(area_pol=purrr::map_dbl(data,~as.numeric(st_area(.x))),
           area_intersect=purrr::map_dbl(intersect,~sum(as.numeric(st_area(.x))))) %>%
    mutate(data=purrr::map(.x=intersect,
                           .f=extract_values_in_polygon,
                           raster_image_1=raster_image_1)) %>%
    mutate(n=purrr::map_dbl(data,nrow)) %>%
    filter(n>0) %>%
    mutate(data=purrr::map(data,sample_n,size=1000,replace=TRUE)) %>%
    tidyr::unnest(cols=c("data")) %>%
    mutate(citycode=CityCode,
           area_pol=area_pol/(1000^2),
           area_intersect=area_intersect/(1000^2)) %>%
    ungroup() %>%
    select(citycode,reach,zone,n,change,entropy, area_pol, area_intersect)
  return(result)
}
