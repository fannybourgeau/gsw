#' Extract 1000 pixels of GSW change intensity per zone x reach polygon for a city
#' @param city city name
#' @param conn connection to sandbox database
#' @return a table with gsw_summary results for the city
#' @export
#' @examples
#' conn=glourbi::connection_to_sandbox()
#' gsw_summary("Lyon",conn)
gsw_summary=function(city,conn){
  thisCityCode=glourbi::all_cities %>%
    dplyr::filter(Urban.Aggl==city) %>%
    dplyr::pull(ID)
  result=DBI::dbReadTable(conn,"gsw_summary") %>%
    dplyr::filter(citycode==thisCityCode) %>% 
    dplyr::mutate(city=city) %>% 
    dplyr::mutate(reach=factor(reach,levels=c("upstream","city","downstream"))) %>% 
    dplyr::group_by(reach,zone) %>% 
    dplyr::mutate(main=n==max(n)) %>% 
    dplyr::ungroup()
  return(result)
}

#' Plot gsw_summary
#' @param gsw_summary_result result of gsw_summary(city)
#' @return a table with gsw_pixels results for the city
#' @export
gsw_summary_plot=function(gws_summary_result){
  result_by_reach_zone=gws_summary_result %>%
    dplyr::group_by(reach,zone) %>%
    dplyr::summarise(propwater=unique(propwater))
  plot=ggplot2::ggplot(gws_summary_result,
                       ggplot2::aes(x=dir_change,y=forcats::fct_reorder(type_change,cat_entropy)))+
    ggplot2::geom_point(ggplot2::aes(size=n*100,col=as.factor(dir_change)))+
    ggplot2::geom_point(data=gws_summary_result %>% dplyr::filter(main==TRUE),
                        stroke=1,col="black",shape=21, ggplot2::aes(size=n*100))+
    ggplot2::geom_rect(data=result_by_reach_zone,
                       ggplot2::aes(x=4,y=1,xmin=4,xmax=4.5, ymin=1,ymax=3),
                       fill="white",stat="identity",color="black")+
    ggplot2::geom_rect(data=result_by_reach_zone ,
                       ggplot2::aes(x=4,y=1,xmin=4,xmax=4.5, ymin=1,ymax=1+2*propwater),
                       fill="lightblue",stat="identity",color="black")+
    ggplot2::scale_size(range = c(0, 30))+
    ggplot2::theme(legend.position="none") +
    ggplot2::xlab("")+ggplot2::ylab("")+ggplot2::labs(title=unique(gws_summary_result$city)) +
    ggplot2::facet_grid(rows=ggplot2::vars(reach),cols=ggplot2::vars(zone))    +
    ggplot2::scale_color_manual(values=c("red","darkgrey","green"))
  return(plot)
}
