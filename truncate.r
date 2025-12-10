    
truncate <- function(delta, occ){
  df <- dplyr::inner_join(
      delta,
      occ_exp, 
      by = "path"
    ) 
  
  df_y0 <- df %>% 
    collapse::fsubset(delta <= stop) %>% 
    dplyr::group_by(path) %>% 
    dplyr::slice_min(time, with_ties = F) %>% 
    collapse::fselect(y0_lm = current_mark, path) %>% 
    collapse::fungroup()
    
  df_full <- df %>% 
    collapse::fsubset(delta <= stop) %>% 
    collapse::fmutate(
      time = dplyr::if_else(stop <= delta + 1/12, delta, time),
      exposure = stop - time
    ) %>% 
    dplyr::inner_join(df_y0,by = "path")
}
