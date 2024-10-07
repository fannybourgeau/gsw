#' Summarise results from the 1000 pixels randomly extracted for each zone x reach of a particular city
#' @param thisCityCode citycode
#' @param average_entropy the average entropy (above which entropy is considered high i.e. change is considered diffuse).
#' @return a message
#' @export
gsw_summarise=function(thisCityCode, average_entropy,conn){
  result_pix=glourbi::get_city_sf("gsw_pixels",
                                  thisCityCode,
                                  conn=conn) %>%
    as_tibble()
  result=result_pix %>%
    group_by(reach,zone,area_pol,area_intersect) %>%
    tidyr::nest() %>%
    mutate(data=purrr::map(data,sample_n,size=1000))
  # For each reach x zone simplify and summarise data:
  simplify_gsw_data=function(gsw_data){
    result=gsw_data %>%
      mutate(cat_change=case_when(change>100~1,
                                  change<100~-1,
                                  TRUE~0),
             cat_entropy=as.numeric(entropy>average_entropy)) %>%
      mutate(cat_entropy=case_when(cat_change==0~0.5, TRUE~cat_entropy)) %>%
      group_by(citycode,cat_change,cat_entropy) %>%
      summarise(n=n(),.groups="drop") %>%
      group_by(citycode) %>%
      mutate(main=n==max(n)) %>%
      ungroup() %>%
      mutate(dir_change=case_when(cat_change==-1~"Negative",
                                  cat_change==0~"Neutral",
                                  cat_change==1~"Positive"),
             type_change=case_when(cat_entropy==0~"Localized",
                                   cat_entropy==0.5~"Neutral",
                                   cat_entropy==1~"Diffuse"))
    return(result)
  }
  result=result %>%
    mutate(data=purrr::map(data,simplify_gsw_data)) %>%
    tidyr::unnest(cols=c("data")) %>%
    mutate(type=purrr::map2_chr(dir_change,type_change,~paste(.x,.y,sep="-"))) %>%
    mutate(reach=factor(reach, levels=c("upstream","city","downstream")),
           zone=factor(zone,levels=c("corridor","plain"))) %>%
    mutate(propwater=area_intersect/area_pol) %>%
    select(citycode,
           reach,
           zone,
           area_total=area_pol,
           area_water=area_intersect,
           cat_change,
           cat_entropy,
           dir_change,
           type_change,
           type,
           n,
           propwater)
  return(result)
}
