#' Append results of gsw_summarise run on CityCode to the table gsw_summary in sandbox database
#' @param CityCode citycode
#' @param conn a connection to the sandbox database
#' @return a message
#' @export
append_to_table_gsw_summary <- function(CityCode,
                                        conn){
  # Check if table exist
  if(!DBI::dbExistsTable(conn,"gsw_summary")){
    query="CREATE TABLE public.gsw_summary (
     citycode VARCHAR(255),
     reach VARCHAR(255),
     zone VARCHAR(255),
     area_total double precision,
     area_water double precision,
     cat_change double precision,
     cat_entropy double precision,
     dir_change VARCHAR(255),
     type_change VARCHAR(255),
     type VARCHAR(255),
     n INTEGER,
     propwater double precision
    );"
    DBI::dbExecute(conn,query)
  }

  if(DBI::dbExistsTable(conn, "gsw_summary")){
    # Check if city has already been added
    query = glue::glue("SELECT * FROM public.gsw_summary WHERE citycode = '{CityCode}';")
    result= DBI::dbGetQuery(conn, query)
    if(nrow(result)>0){return()}
  }
  result=gsw_summarise(CityCode,average_entropy=my_average_entropy, conn=conn)
  DBI::dbWriteTable(conn,name="gsw_summary", value=result, append=TRUE)
  return(glue::glue("Added data to table gsw_summary for city {CityCode}"))
}
