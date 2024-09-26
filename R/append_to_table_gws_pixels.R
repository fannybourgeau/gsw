append_to_table_gws_pixels <- function(CityCode,
                                conn){
  # Check if table exist
  if(!DBI::dbExistsTable(conn,"gws_areas")){
    query="CREATE TABLE public.gws_areas (
     citycode VARCHAR(255),
     reach VARCHAR(255),
     zone VARCHAR(255),
     n INTEGER,
     change INTEGER,
     entropy double precision,
     area_pol double precision,
     area_intersect double precision
    );"
    DBI::dbExecute(conn,query)
  }
  if(DBI::dbExistsTable(conn, "gws_areas")){
    # Check if city has already been added
    query = glue::glue("SELECT * FROM public.gws_areas WHERE citycode = '{CityCode}';")
    result= DBI::dbGetQuery(conn, query)
    if(nrow(result)>0){return()}
  }
  result=extract_city_GWS(CityCode)
  DBI::dbWriteTable(conn,name="gws_areas", value=result, append=TRUE)
  return(glue::glue("Results have been added to DB table for city {CityCode}"))
}
