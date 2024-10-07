#' Append sf table to database
#' @param table the sf table to write
#' @param table_name the table name as displayed in the DSB
#' @param CityCode citycode
#' @param conn a connection to the sandbox database
#' @return a message
#' @export
append_to_table <- function(table,
                            table_name,
                            CityCode,
                            conn){
  # Check if table exist
  if(DBI::dbExistsTable(conn, table_name)){
    # Check if city has already been added
    query = glue::glue("SELECT * FROM public.{table_name} WHERE citycode = '{CityCode}';")
    result= DBI::dbGetQuery(conn, query)
    if(nrow(result)>0){return()}
  }
  if("sf" %in% class(table)){
    sf::st_write(table,
                 dsn=conn,
                 layer=table_name,
                 append=TRUE,
                 quiet=TRUE)
  }else{
    DBI::dbWriteTable(conn=conn,
                      name=table_name,
                      value=table,
                      append=TRUE)
  }
  return(glue::glue("Added data to table {table_name} for city {CityCode}"))
}
