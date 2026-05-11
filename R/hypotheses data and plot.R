extract_dir_target <- function(h3_cos_sim, dir_name) {
  
  dplyr::bind_rows(
    lapply(names(h3_cos_sim), function(g) {
      
      dir_obj <- h3_cos_sim[[g]][[dir_name]]
      
      dplyr::bind_rows(
        lapply(dir_obj, function(x) {
          
          x |>
            dplyr::mutate(
              group = g,
              dimension = dir_name,
              month = target
            )
          
        })
      )
      
    })
  )
  
}