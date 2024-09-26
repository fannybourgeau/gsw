#' Append results of gsw_extract_pixels run on CityCode to the table gsw_pixels in sandbox database
#' #' @param CityCode citycode
#' @param conn a connection to the sandbox database
#' @return a message
#' @export
append_to_table_gsw_pixels <- function(CityCode,
                                conn){
  # Check if table exist
  if(!DBI::dbExistsTable(conn,"gsw_pixels")){
    query="CREATE TABLE public.gsw_pixels (
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
  if(DBI::dbExistsTable(conn, "gsw_pixels")){
    # Check if city has already been added
    query = glue::glue("SELECT * FROM public.gsw_pixels WHERE citycode = '{CityCode}';")
    result= DBI::dbGetQuery(conn, query)
    if(nrow(result)>0){return()}
  }
  result=gsw_extract_pixels(CityCode)
  DBI::dbWriteTable(conn,name="gsw_pixels", value=result, append=TRUE)
  return(glue::glue("Results have been added to DB table for city {CityCode}"))
}
