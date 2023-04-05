my_label_parsed <- function (variable, value) {
  if (variable == "impact") {
    return(as.character(value))
  } else {
    llply(as.character(value), function(x) parse(text = x))    
  }
}


data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- plyr::rename(data_sum, c("mean" = varname))
 return(data_sum)
}


read_data_in = function(eggInclude = TRUE, path = 'Results_files') {

  Result_path = path

    # eggData: -----------------
  eggData = data.table::fread(file.path(Result_path, 'ResultFile.sh.pcod.EggStage.EggStage.csv'), skip = 1)
  stageData = eggData
  
  if(nrow(stageData) > 0) {
  stageData$time = lubridate::ymd_hms(stageData$time)
  stageData$time_id = as.numeric(stageData$time)
  # Order dataframe by time_id:
  stageData = stageData[order(stageData$time_id), ]
  stageData$n_stage = 1
  # Check certain columns are numeric: (because of possible '?')
  stageData$SL = as.numeric(stageData$SL)
  stageData$DW = as.numeric(stageData$DW)
  stageData$stmsta = as.numeric(stageData$stmsta)
  stageData$psurvival = as.numeric(stageData$psurvival)
  stageData$grSL = as.numeric(stageData$grSL)
  stageData$grDW = as.numeric(stageData$grDW)

  eggData = stageData

  }

  # Calculate number of deaths per time step: 
  # Dead individuals have numbers = 0

  # YSLData: -----------------
  YSLData = data.table::fread(file.path(Result_path, 'ResultFile.sh.pcod.YSLStage.YSLStage.csv'), skip = 1)
  stageData = YSLData

  if(nrow(stageData) > 0) {
  stageData$time = lubridate::ymd_hms(stageData$time)
  stageData$time_id = as.numeric(stageData$time)
  # Order dataframe by time_id:
  stageData = stageData[order(stageData$time_id), ]
  stageData$n_stage = 2
  # Check certain columns are numeric: (because of possible '?')
  stageData$SL = as.numeric(stageData$SL)
  stageData$DW = as.numeric(stageData$DW)
  stageData$stmsta = as.numeric(stageData$stmsta)
  stageData$psurvival = as.numeric(stageData$psurvival)
  stageData$grSL = as.numeric(stageData$grSL)
  stageData$grDW = as.numeric(stageData$grDW)

  YSLData = stageData
  }

  # FLDData: -----------------
  FLDData = data.table::fread(file.path(Result_path, 'ResultFile.sh.pcod.FDLStage.FDLStage.csv'), skip = 1)
  stageData = FLDData
  
  if(nrow(stageData) > 0) {
  stageData$time = lubridate::ymd_hms(stageData$time)
  stageData$time_id = as.numeric(stageData$time)
  # Order dataframe by time_id:
  stageData = stageData[order(stageData$time_id), ]
  stageData$n_stage = 3
  # Check certain columns are numeric: (because of possible '?')
  stageData$SL = as.numeric(stageData$SL)
  stageData$DW = as.numeric(stageData$DW)
  stageData$stmsta = as.numeric(stageData$stmsta)
  stageData$psurvival = as.numeric(stageData$psurvival)
  stageData$grSL = as.numeric(stageData$grSL)
  stageData$grDW = as.numeric(stageData$grDW)

  FLDData = stageData
  }

  # FLDpfData: -----------------
  FLDpfData = data.table::fread(file.path(Result_path, 'ResultFile.sh.pcod.FDLpfStage.FDLpfStage.csv'), skip = 1)
  stageData = FLDpfData
  
  if(nrow(stageData) > 0) {
  stageData$time = lubridate::ymd_hms(stageData$time)
  stageData$time_id = as.numeric(stageData$time)
  # Order dataframe by time_id:
  stageData = stageData[order(stageData$time_id), ]
  stageData$n_stage = 4
  # Check certain columns are numeric: (because of possible '?')
  stageData$SL = as.numeric(stageData$SL)
  stageData$DW = as.numeric(stageData$DW)
  stageData$stmsta = as.numeric(stageData$stmsta)
  stageData$psurvival = as.numeric(stageData$psurvival)
  stageData$grSL = as.numeric(stageData$grSL)
  stageData$grDW = as.numeric(stageData$grDW)

  FLDpfData = stageData
  }

  # EpijuvData: -----------------
  EpijuvData = data.table::fread(file.path(Result_path, 'ResultFile.sh.pcod.EpijuvStage.EpijuvStage.csv'), skip = 1)
  stageData = EpijuvData
  
  if(nrow(stageData) > 0) {
  stageData$time = lubridate::ymd_hms(stageData$time)
  stageData$time_id = as.numeric(stageData$time)
  # Order dataframe by time_id:
  stageData = stageData[order(stageData$time_id), ]
  stageData$n_stage = 5
  # Check certain columns are numeric: (because of possible '?')
  stageData$SL = as.numeric(stageData$SL)
  stageData$DW = as.numeric(stageData$DW)
  stageData$stmsta = as.numeric(stageData$stmsta)
  stageData$psurvival = as.numeric(stageData$psurvival)
  stageData$grSL = as.numeric(stageData$grSL)
  stageData$grDW = as.numeric(stageData$grDW)

  EpijuvData = stageData
  } 


  # BenthicJuvData: -----------------
  # BenthicJuvData = data.table::fread(file.path(Result_path, 'ResultFile.sh.pcod.BenthicJuvStage.BenthicJuvStage.csv'), skip = 1)
  # stageData = BenthicJuvData

  # if(nrow(stageData) > 0) {
  # stageData$time = ymd_hms(stageData$time)
  # stageData$time_id = as.numeric(stageData$time)
  # # Order dataframe by time_id:
  # stageData = stageData[order(stageData$time_id), ]
  # stageData$n_stage = 6
  # # Check certain columns are numeric: (because of possible '?')
  # stageData$SL = as.numeric(stageData$SL)
  # stageData$DW = as.numeric(stageData$DW)
  # stageData$stmsta = as.numeric(stageData$stmsta)
  # stageData$psurvival = as.numeric(stageData$psurvival)
  # stageData$grSL = as.numeric(stageData$grSL)
  # stageData$grDW = as.numeric(stageData$grDW)
  
  # BenthicJuvData = stageData
  # }

  # Merge datasets:
  if(eggInclude) {
    Data_List = list(eggData, YSLData, FLDData, FLDpfData, EpijuvData)
    allData = dplyr::bind_rows(Data_List, .id = "column_label")
    allData$typeName = factor(allData$typeName, levels = c('Eggs', 'YSL', 'FDL', 'FDLpf', 'Epijuv'))
  } else {
    Data_List = list(YSLData, FLDData, FLDpfData, EpijuvData)
    allData = dplyr::bind_rows(Data_List, .id = "column_label")
    allData$typeName = factor(allData$typeName, levels = c('YSL', 'FDL', 'FDLpf', 'Epijuv'))
  }
  allData$sizePoint = 1
  allData$vertPos = -1*allData$vertPos
  allData$time2 = paste0(substring(text = allData$time, first = 1, last = 13), 'h')
  allData$year = lubridate::year(allData$time)
  allData$id2 = paste0(allData$year, '-', allData$id)

  return(allData)

}


explore_plot_3D = function(data){

  require(plotly)
  if('Eggs' %in% allData$typeName) {
    ini_col = 1
  } else {
    ini_col = 2
  }
  colPal = rev(RColorBrewer::brewer.pal(n = 5, name = 'Set1'))[ini_col:5]

# Make plot:
fig = plot_ly(x = lon-360, 
              y = lat, 
              z = bathy2)
fig = fig %>% add_surface(showscale = FALSE,
                          colorscale = list(c(0, 1), c("black",
                                                       "white")),
                          contours = list(
                            x = list(highlight = FALSE),
                            y = list(highlight = FALSE),
                            z = list(highlight = FALSE)
                          ),
                          hoverinfo = "none")
fig = fig %>% add_markers(data = data,
                          x = ~horizPos1,
                          y = ~horizPos2,
                          z = ~vertPos,
                          size = I(80),
                          color = ~typeName,
                          alpha = 0.95,
                          text = ~time2,
                          hovertemplate = paste(
                            "<b>%{text}</b><br>",
                            "lon: %{x:.0}<br>",
                            "lat: %{y:.0}<br>",
                            "depth: %{z:.0}<br>")
                          )
fig = fig %>% add_trace(data = data,
                        x = ~horizPos1,
                        y = ~horizPos2,
                        z = ~vertPos,
                        size = I(2),
                        type = 'scatter3d',
                        color = ~typeName,
                        mode = 'lines',
                        hoverinfo = "none",
                        showlegend = FALSE)
fig = fig %>% layout(
  scene = list(
    xaxis = list(title = "longitude", showspikes=FALSE),
    yaxis = list(title = "latitude", showspikes=FALSE),
    zaxis = list(title = "Depth", showspikes=FALSE,
                 range = c(-500, 10))
  ))

return(fig)

}

explore_plot_2D = function(data){

  require(RColorBrewer)

  theme_set(theme_bw())
  
  iniDate = min(as.Date(data$time))
  endDate = max(as.Date(data$time))
  
  if('Eggs' %in% allData$typeName) {
    ini_col = 1
  } else {
    ini_col = 2
  }
  colPal = rev(RColorBrewer::brewer.pal(n = 5, name = 'Set1'))[ini_col:5]

  idlen = length(unique(data$id))

  if(idlen == 1){
    titleLab = paste0('ID: ', unique(data$id))
  } else {
    titleLab = 'multiple IDs' 
  }
  
  p1 = ggplot() +
    geom_contour(data = newBathy, aes(x = lon, y = lat, z = value), bins = 3, 
                 colour = "gray80") +
    geom_point(data = data, aes(horizPos1, horizPos2, color = typeName), size = 0.5) +
    theme(legend.position = c(0.92, 0.2)) +
    geom_polygon(data = ak, aes(long, lat, group = group), 
                 fill = 8, color="black") +
    coord_cartesian(xlim = c(-180, -155), ylim = c(52, 63)) +
    ylab(label = 'latitude') +
    xlab(label = 'longitude') +
    ggtitle(label = titleLab, 
            subtitle = paste0('From: ', iniDate, ' to: ', endDate)) +
    scale_color_manual(values = colPal)+ 
    guides(size = 'none') +
    guides(color = guide_legend(title = "Stage"))

  return(p1)
            
}


explore_animation_2D = function(data, save = TRUE) {

  if('Eggs' %in% allData$typeName) {
    ini_col = 1
  } else {
    ini_col = 2
  }
  colPal = rev(RColorBrewer::brewer.pal(n = 5, name = 'Set1'))[ini_col:5]

  idlen = length(unique(data$id))

  if(idlen == 1){
    saveLab = paste0('ID_', unique(data$id))
  } else {
    saveLab = 'multiple_IDs' 
  }

  p1 = ggplot() +
    geom_contour(data = newBathy, aes(x = lon, y = lat, z = value), bins = 3, 
                 colour = "gray80") +
    geom_point(data = data, aes(horizPos1, horizPos2, color = typeName, size = sizePoint)) +
    theme(legend.position = c(0.92, 0.25)) +
    geom_polygon(data = ak, aes(long, lat, group = group), 
                 fill = 8, color="black") +
    coord_cartesian(xlim = c(-180, -155), ylim = c(52, 63)) +
    ylab(label = 'latitude') +
    xlab(label = 'longitude') +
    scale_color_manual(values = colPal) + 
    guides(size = FALSE) +
    guides(color = guide_legend(title = "Stage"))

  panimate = p1 + transition_time(time) +
                labs(title = "Time: {frame_time}") +
                shadow_wake(wake_length = 0.05, alpha = FALSE)

  if(save) anim_save(filename = paste0('animation_', saveLab, '.gif'), animation = panimate)

}

explore_settlement_2D = function(data){

  datatmp = data[data$typeName == 'BenthicJuv', ]

  theme_set(theme_bw())
  
  p1 = ggplot() +
    geom_contour(data = newBathy, aes(x = lon, y = lat, z = value), bins = 3, 
                 colour = "gray80") +
    geom_point(data = datatmp, aes(horizPos1, horizPos2), size = 2, color = 'red') +
    geom_polygon(data = ak, aes(long, lat, group = group), 
                 fill = 8, color="black") +
    coord_cartesian(xlim = c(-180, -155), ylim = c(52, 63)) +
    ylab(label = 'latitude') +
    xlab(label = 'longitude') +
    ggtitle(label = 'Settlement locations (BenthicJuv)')

  return(p1)
            
}


plot_initial_locations = function(initData) {

  shift_value_1 <- 0
  shift_value_2 <- 360

  map_world_df <- map_data('world', wrap=c(shift_value_1, shift_value_2)) %>%
    dplyr::filter(region != "Antarctica")

  country_shapes <-  geom_polygon(data = map_world_df, 
                                  aes(x=long, y = lat, group = group),
                                  fill = "gainsboro",
                                  color = "gainsboro",
                                  size = 0.15)

  # change longitude values:
  initData$horizPos1 = ifelse(test = initData$horizPos1 > 0, yes = initData$horizPos1 - 360, no = initData$horizPos1)
  initData$horizPos1 = initData$horizPos1 + 360

  p1 = ggplot() + 
  country_shapes +
  geom_contour(data = bathy1, aes(x = lon, y = lat, z = bathy), 
               breaks = c(-50, -100, -200),
               colour = "gray80", size = 0.25) +
  geom_contour(data = bathy2, aes(x = lon, y = lat, z = bathy), 										   
               breaks = c(-50, -100, -200),
               colour = "gray80", size = 0.25) +
  geom_point(data = initData, aes(x = horizPos1, y = horizPos2), color = 'black', size = 0.5) +
  coord_cartesian(xlim = c(170, 205), ylim = c(51, 65)) +
  scale_x_continuous(breaks = c(175, 185, 195, 205), 
                     labels = c('175\u00B0E', '175\u00B0W', '165\u00B0W', '155\u00B0W')) +
  scale_y_continuous(breaks = c(52, 56, 60, 64), 
                     labels = c('52\u00B0N', '56\u00B0N', '60\u00B0N', '64\u00B0N')) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"))

  return(p1)

}


plot_final_locations = function(plot_data) {

  shift_value_1 <- 0
  shift_value_2 <- 360

  map_world_df <- map_data('world', wrap=c(shift_value_1, shift_value_2)) %>%
    dplyr::filter(region != "Antarctica")

  country_shapes <-  geom_polygon(data = map_world_df, 
                                  aes(x=long, y = lat, group = group),
                                  fill = "gainsboro",
                                  color = "gainsboro",
                                  size = 0.15)

  # change longitude values:
  plot_data$horizPos1 = ifelse(test = plot_data$horizPos1 > 0, yes = plot_data$horizPos1 - 360, no = plot_data$horizPos1)
  plot_data$horizPos1 = plot_data$horizPos1 + 360

  p1 = ggplot() + 
        country_shapes +
  geom_contour(data = bathy1, aes(x = lon, y = lat, z = bathy), 
               breaks = c(-50, -100, -200),
               colour = "gray80", size = 0.25) +
  geom_contour(data = bathy2, aes(x = lon, y = lat, z = bathy),                        
               breaks = c(-50, -100, -200),
               colour = "gray80", size = 0.25) +
  geom_point(data = plot_data, aes(x = horizPos1, y = horizPos2), color = 'black') +
  coord_cartesian(xlim = c(170, 205), ylim = c(51, 65)) +
  scale_x_continuous(breaks = c(175, 185, 195, 205), 
                     labels = c('175\u00B0E', '175\u00B0W', '165\u00B0W', '155\u00B0W')) +
  scale_y_continuous(breaks = c(52, 56, 60, 64), 
                     labels = c('52\u00B0N', '56\u00B0N', '60\u00B0N', '64\u00B0N')) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black")) +
  facet_wrap(~year, ncol = 4)

  return(p1)

}


plot_var_time = function(data, var_name = 'DW', var_label = 'dry weight (mg)', n_days = NULL, leg_pos = c(0.15, 0.8)) {

  if('Eggs' %in% allData$typeName) {
    ini_col = 1
  } else {
    ini_col = 2
  }
  colPal = rev(RColorBrewer::brewer.pal(n = 5, name = 'Set1'))[ini_col:5]

  if(!is.null(n_days)) {
    data = data[which(data$time <= min(data$time) + n_days*3600*24), ]
  }

  # DW over time
  p3 = ggplot(data = data, aes_string(x = 'time', y = var_name, group = 'id')) +
          geom_line(aes(color = typeName)) +
          theme_bw() +
          ylab(var_label) +
          xlab('Time') +
          guides(color = guide_legend(title = "Stage")) +
          scale_color_manual(values = colPal) +
          scale_x_datetime(labels = date_format("%b"), date_breaks = '1 month') +
          theme(legend.position = leg_pos)

  return(p3)

}



plot_var_age = function(data, age_name = 'ageYSL', var_name = 'DW', var_label = 'dry weight (mg)', n_days = NULL, leg_pos = c(0.15, 0.8)) {

  if('Eggs' %in% allData$typeName) {
    ini_col = 1
  } else {
    ini_col = 2
  }
  colPal = rev(RColorBrewer::brewer.pal(n = 5, name = 'Set1'))[ini_col:5]

  if(!is.null(n_days)) {
    data = data[which(data[,age_name] <= n_days), ]
  }

  # DW over time
  p3 = ggplot(data = data, aes_string(x = age_name, y = var_name, group = 'id')) +
          geom_line(aes(color = typeName)) +
          theme_bw() +
          ylab(var_label) +
          xlab('Days from hatching') +
          guides(color = guide_legend(title = "Stage")) +
          scale_color_manual(values = colPal)+ 
          theme(legend.position = leg_pos)

  return(p3)

}

"cgi" <-
function(x = long, y = lat, z = NA, w = NA, modproj = NA, mlong = NA, 
  mlat = NA, col = 1, plot = FALSE)
{
  miss <- function(x){
    length(x) == 1 && is.na(x)
  }

  if(miss(z))
    z <- rep(1, length(x))
  if(miss(w))
    w <- rep(1, length(x))
  sel <- !is.na(x * y * z * w)
  x <- x[sel]
  y <- y[sel]
  z <- z[sel]
  w <- w[sel]
  if(length(x[!is.na(x)]) > 0) {
    if(!miss(modproj)) {
      bid <- dg2nm(x = x, y = y, modproj = modproj, mlong = mlong, mlat = mlat)
      x <- bid$x
      y <- bid$y
    }
    # Center of gravity coordinates
    xg <- sum(x * z * w)/sum(z * w)
    yg <- sum(y * z * w)/sum(z * w)
    
    # Inertia
    dx <- x - xg
    dy <- y - yg
    d <- sqrt(dx^2 + dy^2)
    inert <- sum(z * w * (d^2))/sum(z * w)
    I <- inert  
    
    # Weigthed PCA 
    if(!is.na(I)) {
      M11 <- sum(dx^2 * z * w)
      M22 <- sum(dy^2 * z * w)
      M21 <- sum(dx * dy * z * w)
      M12 <- M21
      M <- matrix(c(M11, M12, M21, M22), byrow = T, ncol = 2)
      x1 <- eigen(M)$vectors[1, 1]
      y1 <- eigen(M)$vectors[2, 1]
      x2 <- eigen(M)$vectors[1, 2]
      y2 <- eigen(M)$vectors[2, 2]
      r1 <- eigen(M)$values[1]/(eigen(M)$values[1] + eigen(M)$values[2])
        
      # Principal axis coordinates
      e1 <- (y1/x1)^2
      sx1 <- x1/abs(x1)
      sy1 <- y1/abs(y1)
      sx2 <- x2/abs(x2)
      sy2 <- y2/abs(y2)
      xa <- xg + sx1 * sqrt((r1 * inert)/(1 + e1))
      ya <- yg + sy1 * sqrt((r1 * inert)/(1 + (1/e1)))
      xb <- 2 * xg - xa
      yb <- 2 * yg - ya
      xc <- xg + sx2 * sqrt(((1 - r1) * inert)/(1 + (1/e1)))
      yc <- yg + sy2 * sqrt(((1 - r1) * inert)/(1 + e1))
      xd <- 2 * xg - xc
      yd <- 2 * yg - yc
      Imax <- r1*inert 
      Imin <- (1-r1)*inert
      Iso <- sqrt(Imin/Imax)
    }
    else {
      xa <- NA
      ya <- NA
      xb <- NA
      yb <- NA
      xc <- NA
      yc <- NA
      xd <- NA
      yd <- NA
      Imax <- NA
      Imin <- NA
      Iso <- NA
    }
    if(!miss(modproj)) {
      bid <- nm2dg(x = c(xg, xa, xb, xc, xd), y = c(yg, ya, yb, yc, yd), 
        modproj = modproj, mlong = mlong, mlat = mlat)
      res <- list(xcg = bid$x[1], ycg = bid$y[1], I = I, Imax = Imax, 
        Imin = Imin, Iso = Iso, xaxe1 = bid$x[2:3], yaxe1 = bid$y[2:3], 
        xaxe2 = bid$x[4:5], yaxe2 = bid$y[4:5])
    }
    else res <- list(xcg = xg, ycg = yg, I = I, Imax = Imax, Imin = Imin, 
      Iso = Iso, xaxe1 = c(xa, xb), yaxe1 = c(ya, yb), xaxe2 = c(xc, xd), 
      yaxe2 = c(yc, yd))
    if(plot == T) {
      segments(res$xaxe1[1], res$yaxe1[1], res$xaxe1[2], res$yaxe1[2], col = col, lwd = 2)
      segments(res$xaxe2[1], res$yaxe2[1], res$xaxe2[2], res$yaxe2[2], col = col, lwd = 2)
    }
  }
  else {
    res <- list(xcg = NA, ycg = NA, I = NA, Imax = NA, 
      Imin = NA, Iso = NA, xaxe1 = NA, yaxe1 = NA, xaxe2 = NA, yaxe2 = NA)
  }
  res
}


plot_direction = function(plot_data, init_dat) {

  shift_value_1 <- 0
  shift_value_2 <- 360

  map_world_df <- map_data('world', wrap=c(shift_value_1, shift_value_2)) %>%
    dplyr::filter(region != "Antarctica")

  country_shapes <-  geom_polygon(data = map_world_df, 
                                  aes(x=long, y = lat, group = group),
                                  fill = "gainsboro",
                                  color = "gainsboro",
                                  size = 0.15)

nYears = length(unique(plot_data$year))
myColPal = colorRamps::blue2red(n = nYears)

  g2 = ggplot() + 
  country_shapes +
  geom_contour(data = bathy1, aes(x = lon, y = lat, z = bathy),
               breaks = c(-50, -100, -200),
               colour = "gray80", size = 0.25, alpha = 0.5) +
  geom_contour(data = bathy2, aes(x = lon, y = lat, z = bathy),
               breaks = c(-50, -100, -200),
               colour = "gray80", size = 0.25, alpha = 0.3) +
  geom_segment(data = plot_data, aes(x = init_dat$cgx, y = init_dat$cgy, xend = cgx, 
                                     yend = cgy, color = factor(year)),
               arrow = arrow(length = unit(0.1, "cm")))  +
  coord_cartesian(xlim = c(183, 200), ylim = c(55, 60)) +
  scale_x_continuous(breaks = c(175, 185, 195, 205), 
                     labels = c('175\u00B0E', '175\u00B0W', '165\u00B0W', '155\u00B0W')) +
  scale_y_continuous(breaks = c(52, 56, 60, 64), 
                     labels = c('52\u00B0N', '56\u00B0N', '60\u00B0N', '64\u00B0N')) +
  scale_colour_manual(values = myColPal, name = NULL) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"),
        legend.position = c(0.8, 0.3),
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.25, 'cm'),
        #legend.direction="horizontal",
        legend.background = element_rect(fill = "transparent")) 

  return(g2)

}


plot_cg = function(plot_data, init_dat, mainPal) {

  shift_value_1 <- 0
  shift_value_2 <- 360

  map_world_df <- map_data('world', wrap=c(shift_value_1, shift_value_2)) %>%
    dplyr::filter(region != "Antarctica")

  country_shapes <-  geom_polygon(data = map_world_df, 
                                  aes(x=long, y = lat, group = group),
                                  fill = "gainsboro",
                                  color = "gainsboro",
                                  size = 0.15)

  g2 = ggplot() + 
  country_shapes +
  geom_contour(data = bathy1, aes(x = lon, y = lat, z = bathy),
               breaks = c(-50, -100, -200),
               colour = "gray80", size = 0.25, alpha = 0.5) +
  geom_contour(data = bathy2, aes(x = lon, y = lat, z = bathy),
               breaks = c(-50, -100, -200),
               colour = "gray80", size = 0.25, alpha = 0.3) +
  geom_point(data = plot_data, aes(x = cgx, y = cgy, colour = scenario), shape = 3, alpha = 0.25)  +
  geom_point(data = init_dat, aes(x = cgx, y = cgy), colour = 'black', shape = 17)  +
  coord_cartesian(xlim = c(170, 205), ylim = c(51, 65)) +  
  scale_x_continuous(breaks = c(175, 185, 195, 205), 
                     labels = c('175\u00B0E', '175\u00B0W', '165\u00B0W', '155\u00B0W')) +
  scale_y_continuous(breaks = c(52, 56, 60, 64), 
                     labels = c('52\u00B0N', '56\u00B0N', '60\u00B0N', '64\u00B0N')) +
  scale_color_brewer(palette = mainPal) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"),
        legend.position = 'none') 

  return(g2)

}

plot_cg2 = function(plot_data, mainPal) {

  shift_value_1 <- 0
  shift_value_2 <- 360

  map_world_df <- map_data('world', wrap=c(shift_value_1, shift_value_2)) %>%
    dplyr::filter(region != "Antarctica")

  country_shapes <-  geom_polygon(data = map_world_df, 
                                  aes(x=long, y = lat, group = group),
                                  fill = "gainsboro",
                                  color = "gainsboro",
                                  size = 0.15)

  g2 = ggplot() + 
  country_shapes +
  geom_contour(data = bathy1, aes(x = lon, y = lat, z = bathy),
               breaks = c(-50, -100, -200),
               colour = "gray80", size = 0.25, alpha = 0.5) +
  geom_contour(data = bathy2, aes(x = lon, y = lat, z = bathy),
               breaks = c(-50, -100, -200),
               colour = "gray80", size = 0.25, alpha = 0.3) +
  geom_segment(data = plot_data, aes(x = xa, y = ya, xend = xa1, yend = ya1, colour = decade))  +
  geom_segment(data = plot_data, aes(x = xb, y = yb, xend = xb1, yend = yb1, colour = decade))  +
  coord_cartesian(xlim = c(170, 205), ylim = c(51, 65)) +  
  scale_x_continuous(breaks = c(175, 185, 195, 205), 
                     labels = c('175\u00B0E', '175\u00B0W', '165\u00B0W', '155\u00B0W')) +
  scale_y_continuous(breaks = c(52, 56, 60, 64), 
                     labels = c('52\u00B0N', '56\u00B0N', '60\u00B0N', '64\u00B0N')) +
  scale_color_brewer(palette = mainPal) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"),
        legend.position = 'none') 

  return(g2)

}



plot_map_var = function(plot_data, legTitle = 'Slope', palFun, mainTitle = '', colLimits = NULL, ...) {

  shift_value_1 <- 0
  shift_value_2 <- 360

  map_world_df <- map_data('world', wrap=c(shift_value_1, shift_value_2)) %>%
    dplyr::filter(region != "Antarctica")

  country_shapes <-  geom_polygon(data = map_world_df, 
                                  aes(x=long, y = lat, group = group),
                                  fill = "gainsboro",
                                  color = "gainsboro",
                                  size = 0.15)

  g2 = ggplot() + 
  country_shapes +
  geom_contour(data = bathy1, aes(x = lon, y = lat, z = bathy),
               breaks = c(-50, -100, -200),
               colour = "gray80", size = 0.25, alpha = 0.5) +
  geom_contour(data = bathy2, aes(x = lon, y = lat, z = bathy),
               breaks = c(-50, -100, -200),
               colour = "gray80", size = 0.25, alpha = 0.3) +
  geom_point(data = plot_data, aes(x = lon, y = lat, colour = var), size = 0.75)  +
  coord_cartesian(xlim = c(174, 200), ylim = c(53, 62)) +  
  scale_x_continuous(breaks = c(175, 185, 195, 205), 
                     labels = c('175\u00B0E', '175\u00B0W', '165\u00B0W', '155\u00B0W')) +
  scale_y_continuous(breaks = c(52, 56, 60, 64), 
                     labels = c('52\u00B0N', '56\u00B0N', '60\u00B0N', '64\u00B0N')) +
  #scale_colour_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  #scale_colour_gradientn(colours = colorspace::diverge_hcl(7)) +
  #scale_colour_distiller(palette = "Spectral", limits = colLimits) +
  scico::scale_colour_scico(palette = "vik", limits = colLimits) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_text(size = 8),
        legend.position = c(0.13,0.38)) +
  guides(color = guide_colourbar(title = legTitle, barwidth = 0.75)) +
  annotate(geom = 'text', label = mainTitle, x = -Inf, y = Inf, hjust = -1, vjust = 1.5, ...)

  return(g2)

}


plot_map_var2 = function(plot_data, legTitle = 'Slope', palFun, mainTitle = '', ...) {

  shift_value_1 <- 0
  shift_value_2 <- 360

  map_world_df <- map_data('world', wrap=c(shift_value_1, shift_value_2)) %>%
    dplyr::filter(region != "Antarctica")

  country_shapes <-  geom_polygon(data = map_world_df, 
                                  aes(x=long, y = lat, group = group),
                                  fill = "gainsboro",
                                  color = "gainsboro",
                                  size = 0.15)

  g2 = ggplot() + 
  country_shapes +
  geom_contour(data = bathy1, aes(x = lon, y = lat, z = bathy),
               breaks = c(-50, -100, -200),
               colour = "gray80", size = 0.25, alpha = 0.5) +
  geom_contour(data = bathy2, aes(x = lon, y = lat, z = bathy),
               breaks = c(-50, -100, -200),
               colour = "gray80", size = 0.25, alpha = 0.3) +
  geom_point(data = plot_data, aes(x = lon, y = lat, colour = var), size = 0.75)  +
  coord_cartesian(xlim = c(174, 200), ylim = c(53, 62)) +  
  scale_x_continuous(breaks = c(175, 185, 195, 205), 
                     labels = c('175\u00B0E', '175\u00B0W', '165\u00B0W', '155\u00B0W')) +
  scale_y_continuous(breaks = c(52, 56, 60, 64), 
                     labels = c('52\u00B0N', '56\u00B0N', '60\u00B0N', '64\u00B0N')) +
  scale_color_gradientn(colours = rev(brewer.pal(9,'RdYlBu')), ...) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"),
        legend.background = element_rect(fill = "transparent"),
        legend.position = c(0.13,0.38)) +
  guides(color = guide_colourbar(title = legTitle, barwidth = 0.75)) +
  annotate(geom = 'text', label = mainTitle, x = -Inf, y = Inf, hjust = 0, vjust = 1)

  return(g2)

}


plot_map_2d_density = function(plot_data, palFun, nCol = 3, ...) {

  shift_value_1 <- 0
  shift_value_2 <- 360

  map_world_df <- map_data('world', wrap=c(shift_value_1, shift_value_2)) %>%
    dplyr::filter(region != "Antarctica")

  country_shapes <-  geom_polygon(data = map_world_df, 
                                  aes(x=long, y = lat, group = group),
                                  fill = "gainsboro",
                                  color = "gainsboro",
                                  size = 0.15)

  g2 = ggplot() + 
  country_shapes +
  geom_contour(data = bathy1, aes(x = lon, y = lat, z = bathy),
               breaks = c(-50, -100, -200),
               colour = "gray80", size = 0.25, alpha = 0.5) +
  geom_contour(data = bathy2, aes(x = lon, y = lat, z = bathy),
               breaks = c(-50, -100, -200),
               colour = "gray80", size = 0.25, alpha = 0.3) +
  stat_density_2d(aes(x = horizPos1, y = horizPos2, fill = stat(level)), alpha = .25,
                 geom = "polygon", data = plot_data,
                        n = 250 ,
                        bins = 20, show.legend = FALSE) +
  coord_cartesian(xlim = c(170, 205), ylim = c(51, 65)) +  
  scale_x_continuous(breaks = c(175, 185, 195, 205), 
                     labels = c('175\u00B0E', '175\u00B0W', '165\u00B0W', '155\u00B0W')) +
  scale_y_continuous(breaks = c(52, 56, 60, 64), 
                     labels = c('52\u00B0N', '56\u00B0N', '60\u00B0N', '64\u00B0N')) +
  scale_fill_distiller(palette= "Spectral", direction=-1) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"),
        legend.position = 'none',
        strip.background = element_blank()) +
  facet_wrap(~decade, ncol = nCol)

  return(g2)

}

plot_map_2d_density2 = function(plot_data, palFun, nCol = 3, ...) {

  shift_value_1 <- 0
  shift_value_2 <- 360

  map_world_df <- map_data('world', wrap=c(shift_value_1, shift_value_2)) %>%
    dplyr::filter(region != "Antarctica")

  country_shapes <-  geom_polygon(data = map_world_df, 
                                  aes(x=long, y = lat, group = group),
                                  fill = "gainsboro",
                                  color = "gainsboro",
                                  size = 0.15)

  g2 = ggplot() + 
  country_shapes +
  geom_contour(data = bathy1, aes(x = lon, y = lat, z = bathy),
               breaks = c(-50, -100, -200),
               colour = "gray80", size = 0.25, alpha = 0.5) +
  geom_contour(data = bathy2, aes(x = lon, y = lat, z = bathy),
               breaks = c(-50, -100, -200),
               colour = "gray80", size = 0.25, alpha = 0.3) +
  stat_density_2d(aes(x = horizPos1, y = horizPos2, fill = stat(level)), alpha = .25,
                 geom = "polygon", data = plot_data,
                        n = 250 ,
                        bins = 20, show.legend = FALSE) +
  coord_cartesian(xlim = c(170, 205), ylim = c(51, 65)) +  
  scale_x_continuous(breaks = c(175, 185, 195, 205), 
                     labels = c('', '175\u00B0W', '', '155\u00B0W')) +
  scale_y_continuous(breaks = c(52, 56, 60, 64), 
                     labels = c('52\u00B0N', '56\u00B0N', '60\u00B0N', '64\u00B0N')) +
  scale_fill_distiller(palette= "Spectral", direction=-1) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"),
        legend.position = 'none',
        strip.background = element_blank()) +
  facet_wrap(~decade, ncol = nCol)

  return(g2)

}


plot_map_2d_density3 = function(plot_data, init_locs, ...) {

  shift_value_1 <- 0
  shift_value_2 <- 360

  map_world_df <- map_data('world', wrap=c(shift_value_1, shift_value_2)) %>%
    dplyr::filter(region != "Antarctica")

  country_shapes <-  geom_polygon(data = map_world_df, 
                                  aes(x=long, y = lat, group = group),
                                  fill = "gainsboro",
                                  color = "gainsboro",
                                  size = 0.15)

  g2 = ggplot() + 
  country_shapes +
  geom_contour(data = bathy1, aes(x = lon, y = lat, z = bathy),
               breaks = c(-50, -100, -200),
               colour = "gray80", size = 0.25, alpha = 0.5) +
  geom_contour(data = bathy2, aes(x = lon, y = lat, z = bathy),
               breaks = c(-50, -100, -200),
               colour = "gray80", size = 0.25, alpha = 0.3) +
  stat_density_2d(aes(x = horizPos1, y = horizPos2, fill = stat(level)), alpha = .25,
                 geom = "polygon", data = plot_data,
                        n = 250 ,
                        bins = 20, show.legend = FALSE) +
  # geom_point(data = init_locs, aes(x = horizPos1, y = horizPos2), color = 'black', size = 0.5, alpha = 0.1) +
  coord_cartesian(xlim = c(170, 205), ylim = c(51, 65)) +  
  scale_x_continuous(breaks = c(175, 185, 195, 205), 
                     labels = c('175\u00B0E', '175\u00B0W', '165\u00B0W', '155\u00B0W')) +
  scale_y_continuous(breaks = c(52, 56, 60, 64), 
                     labels = c('52\u00B0N', '56\u00B0N', '60\u00B0N', '64\u00B0N')) +
  scale_fill_distiller(palette= "Spectral", direction=-1) +
  xlab(NULL) +
  ylab(NULL) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"),
        legend.position = 'none')

  return(g2)

}


plot_trajectory = function(plot_data) {

  shift_value_1 <- 0
  shift_value_2 <- 360

  map_world_df <- map_data('world', wrap=c(shift_value_1, shift_value_2)) %>%
    dplyr::filter(region != "Antarctica")

  country_shapes <-  geom_polygon(data = map_world_df, 
                                  aes(x=long, y = lat, group = group),
                                  fill = "gainsboro",
                                  color = "gainsboro",
                                  size = 0.15)

  p2 = ggplot() + 
                    country_shapes +
                    geom_contour(data = bathy1, aes(x = lon, y = lat, z = bathy),
                                 breaks = c(-50, -100, -200),
                                 colour = "gray80", size = 0.25, alpha = 0.5) +
                    geom_contour(data = bathy2, aes(x = lon, y = lat, z = bathy),
                                 breaks = c(-50, -100, -200),
                                 colour = "gray80", size = 0.25, alpha = 0.5) +
                    geom_path(data = plot_data, aes(x = horizPos1, y = horizPos2, group = factor(id)), 
                              color = 'black', alpha = 0.3) +
                    coord_cartesian(xlim = c(170, 205), ylim = c(51, 65)) +  
                    scale_x_continuous(breaks = c(175, 185, 195, 205), 
                                       labels = c('', '175\u00B0W', '', '155\u00B0W')) +
                    scale_y_continuous(breaks = c(52, 56, 60, 64), 
                                       labels = c('52\u00B0N', '56\u00B0N', '60\u00B0N', '64\u00B0N')) +
                    xlab(NULL) +
                    ylab(NULL) +
                    theme_bw() +
                    annotate('text', x = 205, y =63, label= unique(plot_data$year)[1], hjust=1, size = 3) +
                    theme(panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
                          axis.text = element_text(size = 6)) 
    
    return(p2)

}