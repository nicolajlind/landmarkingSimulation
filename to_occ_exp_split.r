to_occ_exp_split <- function(events, dt = 1/12, eps = 1e-9) {
  ev <- events %>%
    dplyr::arrange(path, time)
  
  build_path <- function(df) {
    path_id <- unique(df$path)
    t0 <- unique(df$t0)
    tn <- unique(df$tn)
    y0 <- unique(df$y0)
    
    times <- sort(unique(c(t0, df$time[df$time >= t0 & df$time <= tn], tn)))
    mark_at <- rep(NA_integer_, length(times))
    if (nrow(df)) mark_at[match(df$time, times)] <- df$mark
    
    starts <- head(times, -1)
    stops  <- tail(times, -1)
    
    cur <- integer(length(starts))
    cur[1] <- y0
    if (length(starts) > 1) {
      cur[-1] <- head(mark_at, -1)[-1]
    }
    
    tibble::tibble(
      path         = path_id,
      time         = starts,
      stop         = stops,
      t0           = t0,
      y0           = y0,
      current_mark = cur,
      next_mark    = tail(mark_at, -1)
    ) %>% 
      dplyr::filter(stop > time + eps)
  }
  
  paths <- ev %>%
    dplyr::group_by(path) %>%
    dplyr::group_split() %>%
    purrr::map_dfr(build_path)
  
  split_one <- function(path, start, stop, t0, y0, current_mark, next_mark) {
    brks <- seq(from = start, to = stop, by = dt)
    if (tail(brks, 1) < stop - 1e-12) brks <- c(brks, stop)
    starts <- head(brks, -1)
    stops  <- tail(brks, -1)
    last_slice <- seq_along(starts) == length(starts)
    tibble::tibble(
      path, 
      time = starts, 
      stop = stops,
      exposure = stops - starts,
      t0 = t0, 
      y0 = y0,
      current_mark, next_mark,
      last_slice
    )
  }
  
  out <- purrr::pmap_dfr(
    paths[, c("path","time","stop","t0","y0","current_mark","next_mark")],
    ~ split_one(..1, ..2, ..3, ..4, ..5, ..6, ..7)
  )
  
  if (!nrow(out)) return(out)
  
  marks <- sort(unique(stats::na.omit(events$mark)))
  for (m in marks) {
    out[[paste0("occ_", m)]] <- as.integer(
      out$last_slice &
        !is.na(out$next_mark) &
        (out$next_mark == m) &
        (out$next_mark != out$current_mark)
    )
  }
  
  out %>%
    dplyr::select(
      path, time, stop, exposure, t0, y0, current_mark, next_mark,
      dplyr::starts_with("occ_")
    ) %>%
    dplyr::arrange(path, time)
}