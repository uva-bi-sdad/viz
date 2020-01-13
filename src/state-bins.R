#Data is found here: 
#https://www.nsf.gov/statistics/state-indicators/downloads

#Statebins Sources
#https://rud.is/b/2017/11/18/statebins-reimagined/
#https://rdrr.io/github/hrbrmstr/statebins/man/statebins.html
#https://rdrr.io/cran/statebins/src/R/statebins.R
#https://github.com/hrbrmstr/statebins/blob/master/R/geom-otile.R
#https://github.com/hrbrmstr/statebins

##### Set Up #####
#FIRST!INSTALL!NEW!VERSION!#
#install.packages("statebins", repos = "https://cinc.rud.is")
library(readxl)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(statebins)

##### Palette #####
theme_Palette<-c("#1B3766", "#02ABD6", "#6DD4DB", "#A9D5A5", "#F17E1D")
wheel <- function(col, radius = 1, ...)
pie(rep(1, length(col)), col=col, radius=radius)
wheel(theme_Palette)



##### Read In Data #####
comp.math <- read_excel("/home/sm9dv/viz/data/viz/original/comp-math.xlsx", skip = 3)
comp.math <- comp.math[-c(53, 55:61), ]

comp.math.18 <- comp.math %>% select("State", "2018...17", "2018...51") %>% filter(!(State == "United States"))

colnames(comp.math.18)[colnames(comp.math.18)== "2018...17"] <- "count.18"
colnames(comp.math.18)[colnames(comp.math.18)== "2018...51"] <- "count.18.per"
comp.math.se <- read_excel("/home/sm9dv/viz/data/viz/original/comp-math-se.xlsx", skip = 3)
comp.math.se <- comp.math.se[-c(53, 55:61), ]


comp.math.se.18 <- comp.math.se %>% select("State", "2018...17", "2018...51") %>% filter(!(State == "United States"))

colnames(comp.math.se.18)[colnames(comp.math.se.18)== "2018...17"] <- "se.18"
colnames(comp.math.se.18)[colnames(comp.math.se.18)== "2018...51"] <- "se.18.per"


comp.math.18 <- merge(comp.math.18, comp.math.se.18, by = "State")

##### STATE BINS BEFORE ADJUSTMENTS #####
## Test Percentages
statebins(comp.math.18, 
          state_col = "State", 
          value_col = "count.18.per",
          name = "Computer and Mathematical Scientists Percentage", 
          dark_label = "black", 
          light_label = "white", 
          na_label = "black",
          font_size = 4, 
          round = TRUE,
          ggplot2_scale_function = scale_fill_gradient,
          low = "white", 
          high = "#1B3766"
          ) +
  theme_statebins(legend_position="bottom") +
  labs(title = "Computer and Mathematical Scientists as a \nPercentage of All Occupations by State") 

## Test Percent Error 
statebins(comp.math.18, 
          state_col = "State", 
          value_col = "se.18.per",
          name = "Computer and Mathematical Scientists Percent Error", 
          dark_label = "black", 
          light_label = "white", 
          na_label = "black",
          font_size = 4, 
          round = TRUE,
          radius = ,
          ggplot2_scale_function = scale_fill_gradient,
          low = "white", 
          high = "#1B3766"
) +
  theme_statebins(legend_position="bottom") +
  labs(title = "Computer and Mathematical Scientists as a \nPercent Error of All Occupations by State") 


##### STATE BINS CODE #####

## State Validation Function
validate_states <- function(state_data, state_col, merge.x) {
  
  good_states <- state_data[,state_col] %in% state_coords[,merge.x]
  if (any(!good_states)) {
    invalid <- state_data[,state_col][which(!good_states)]
    state_data <- state_data[which(good_states),]
    warning("Found invalid state values: ", invalid)
  }
  
  dups <- duplicated(state_data[,state_col])
  if (any(dups)) {
    state_data <- state_data[which(!dups),]
    warning("Removing duplicate state rows")
  }
  
  return(state_data)
  
}

## State Coordinates
state_coords <- structure(list(abbrev = c("AL", "AK", "AZ", "AR", "CA", "CO",
                                          "CT", "DC", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS",
                                          "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE",
                                          "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA",
                                          "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY",
                                          "PR", "VI", "NYC"),
                               state = c("Alabama", "Alaska", "Arizona", "Arkansas",
                                         "California", "Colorado", "Connecticut", "District of Columbia",
                                         "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois",
                                         "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine",
                                         "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi",
                                         "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
                                         "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
                                         "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island",
                                         "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah",
                                         "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming",
                                         "Puerto Rico", "Virgin Islands", "New York City"),
                               col = c(8L, 1L, 3L, 6L, 2L, 4L, 11L, 10L, 11L, 10L,
                                       9L, 1L, 3L, 7L, 7L, 6L, 5L, 7L, 6L, 12L, 10L, 11L, 8L, 6L, 7L,
                                       6L, 4L, 5L, 3L, 12L, 10L, 4L, 10L, 8L, 5L, 8L, 5L, 2L, 9L, 12L,
                                       9L, 5L, 7L, 5L, 3L, 11L, 9L, 2L, 8L, 7L, 4L, 12L, 12L, 12),
                               row = c(7L, 7L,
                                       6L, 6L, 5L, 5L, 4L, 6L, 5L, 8L, 7L, 8L, 3L, 3L, 4L, 4L, 6L, 5L,
                                       7L, 1L, 5L, 3L, 3L, 3L, 7L, 5L, 3L, 5L, 4L, 2L, 4L, 6L, 3L, 6L,
                                       3L, 4L, 7L, 4L, 4L, 4L, 6L, 4L, 6L, 8L, 5L, 2L, 5L, 3L, 5L, 2L, 4L,
                                       8L, 7L, 3L)),
                          .Names = c("abbrev", "state", "col", "row"),
                          class = "data.frame", row.names = c(NA, -54L))

state_coords <- state_coords[order(state_coords$state),]

b_state_coords <- state_coords
colnames(b_state_coords) <- c("abbrev", "state", "x", "y")
b_state_coords$y <- -b_state_coords$y

## SB Invert for Converting Text Color For Dark/Light Bins
.sb_invert <- function(hex_color, dark_color="black", light_color="white",
                       na_color="white") {
  
  hex_color <- gsub("#", "", hex_color)
  
  R <- suppressWarnings(as.integer(paste("0x", substr(hex_color,1,2), sep="")))
  G <- suppressWarnings(as.integer(paste("0x", substr(hex_color,3,4), sep="")))
  B <- suppressWarnings(as.integer(paste("0x", substr(hex_color,5,6), sep="")))
  
  YIQ <- ((R*299) + (G*587) + (B*114)) / 1000
  
  return(
    ifelse(is.na(YIQ), na_color,
           ifelse(
             YIQ >= 128, dark_color, light_color)
    )
  )
}

## State Validation
validate_states <- function(state_data, state_col, merge.x, ignore_dups=FALSE) {
  
  good_states <- state_data[,state_col] %in% state_coords[,merge.x]
  if (any(!good_states)) {
    invalid <- state_data[,state_col][which(!good_states)]
    state_data <- state_data[which(good_states),]
    warning("Found invalid state values: ", invalid)
  }
  
  if (!ignore_dups) {
    dups <- duplicated(state_data[,state_col])
    if (any(dups)) {
      state_data <- state_data[which(!dups),]
      warning("Removing duplicate state rows")
    }
  }
  
  return(state_data)
  
}

## Function that Chooses a over b unless a is Null
"%||%" <- function(a, b) { if (!is.null(a)) a else b }

## Defines .pt
.pt <- 2.84527559055118

# Grid Graphical Object
ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

#Not sure what this if for but it is necessary
geom_rrect <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       radius = grid::unit(6, "pt"),
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRrect,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      radius = radius,
      na.rm = na.rm,
      ...
    )
  )
}

GeomRrect <- ggplot2::ggproto("GeomRrect", ggplot2::Geom,
                              
                              default_aes = ggplot2::aes(
                                colour = NA, fill = "grey35", size = 0.5, linetype = 1, alpha = NA
                              ),
                              
                              required_aes = c("xmin", "xmax", "ymin", "ymax"),
                              
                              draw_panel = function(self, data, panel_params, coord,
                                                    radius = grid::unit(6, "pt")) {
                                
                                coords <- coord$transform(data, panel_params)
                                
                                lapply(1:length(coords$xmin), function(i) {
                                  
                                  grid::roundrectGrob(
                                    coords$xmin[i], coords$ymax[i],
                                    width = (coords$xmax[i] - coords$xmin[i]),
                                    height = (coords$ymax[i] - coords$ymin)[i],
                                    r = radius,
                                    default.units = "native",
                                    just = c("left", "top"),
                                    gp = grid::gpar(
                                      col = coords$colour[i],
                                      fill = alpha(coords$fill[i], coords$alpha[i]),
                                      lwd = coords$size[i] * .pt,
                                      lty = coords$linetype[i],
                                      lineend = "butt"
                                    )
                                  )
                                  
                                }) -> gl
                                
                                grobs <- do.call(grid::gList, gl)
                                
                                ggname("geom_rrect", grid::grobTree(children = grobs))
                                
                              },
                              
                              draw_key = ggplot2::draw_key_polygon
                              
)

#Not sure what this is for but it is necessary
geom_rtile <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       radius = 6,
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRtile,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      radius = radius,
      na.rm = na.rm,
      ...
    )
  )
}

GeomRtile <- ggplot2::ggproto("GeomRtile", GeomRrect,
                              
                              extra_params = c("na.rm", "width", "height"),
                              
                              setup_data = function(data, params) {
                                data$width <- data$width %||% params$width %||% ggplot2::resolution(data$x, FALSE)
                                data$height <- data$height %||% params$height %||% ggplot2::resolution(data$y, FALSE)
                                
                                transform(data,
                                          xmin = x - width / 2,  xmax = x + width / 2,  width = NULL,
                                          ymin = y - height / 2, ymax = y + height / 2, height = NULL
                                )
                              },
                              
                              default_aes = ggplot2::aes(
                                fill = "grey20", colour = NA, size = 0.1, linetype = 1, alpha = NA
                              ),
                              
                              required_aes = c("x", "y"),
                              
                              draw_key = ggplot2::draw_key_polygon
                              
)



statebins_test <- function(state_data,
                                        state_col="state", value_col="value",
                                        dark_label="black", light_label="white",
                                        na_label="white", font_size=3,
                                        state_border_col="white", state_border_size=2,
                                        round=FALSE, radius=grid::unit(6, "pt"),
                                        ggplot2_scale_function=ggplot2::scale_fill_distiller,
                                        ...) {
  
  state_data <- data.frame(state_data, stringsAsFactors=FALSE)
  
  if (max(nchar(state_data[,state_col])) <= 3) {
    merge.x <- "abbrev"
  } else {
    merge.x <- "state"
  }
  
  state_data <- validate_states(state_data, state_col, merge.x)
  
  st.dat <- merge(state_coords, state_data, by.x=merge.x, by.y=state_col, all.y=TRUE,
                  sort=TRUE)
  
  gg <- ggplot()
  
  if (round) {
    gg <- gg + geom_rtile(data = st.dat, radius = radius,
                          aes_string(x = "col", y = "row", fill = value_col),
                          color = state_border_col, size = state_border_size)
  } else {
    gg <- gg + geom_tile(data = st.dat,
                         aes_string(x = "col", y = "row", fill = value_col),
                         color = state_border_col, size = state_border_size)
  }
  
  gg <- gg + scale_y_reverse()
  gg <- gg + ggplot2_scale_function(...)
  gg <- gg + coord_equal()
  gg <- gg + labs(x = NULL, y = NULL)
  
  gb <- ggplot2::ggplot_build(gg)
  
  gg <- gg + geom_text(data = st.dat,
                       aes_string(x = "col", y = "row", label = "abbrev"),
                       angle = 0,
                       color = .sb_invert(gb$data[[1]]$fill, dark_label, light_label, na_label),
                       size = font_size)
  
  gg
  
}



statebins_test(comp.math.18, 
          state_col = "State", 
          value_col = "count.18.per",
          name = "Computer and Mathematical Scientists Percent Error", 
          dark_label = "black", 
          light_label = "white", 
          na_label = "black",
          font_size = 4, 
          round = TRUE,
          ggplot2_scale_function = scale_fill_gradient,
          low = "white", 
          high = "#1B3766"
)  +
  theme_statebins(legend_position="bottom") +
  labs(title = "Computer and Mathematical Scientists as a \nPercent Error of All Occupations by State") 


# Look at the function

#parameters
state_data = comp.math.18
state_col="State"
value_col="count.18.per"
var_col = "se.18.per"
dark_label="black"
light_label="white"
na_label="white"
font_size=4
state_border_col="white" 
state_border_size=2
round=TRUE
radius = 4
ggplot2_scale_function=ggplot2::scale_fill_distiller

state_data <- data.frame(state_data, stringsAsFactors=FALSE)

#labels if state names are full nare or abbreviations
if (max(nchar(state_data[,state_col])) <= 3) {
  merge.x <- "abbrev"
} else {
  merge.x <- "state"
}


## State Validation Function
validate_states <- function(state_data, state_col, merge.x) {
  
  good_states <- state_data[,state_col] %in% state_coords[,merge.x]
  if (any(!good_states)) {
    invalid <- state_data[,state_col][which(!good_states)]
    state_data <- state_data[which(good_states),]
    warning("Found invalid state values: ", invalid)
  }
  
  dups <- duplicated(state_data[,state_col])
  if (any(dups)) {
    state_data <- state_data[which(!dups),]
    warning("Removing duplicate state rows")
  }
  
  return(state_data)
  
}

state_coords <- structure(list(abbrev = c("AL", "AK", "AZ", "AR", "CA", "CO",
                                          "CT", "DC", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS",
                                          "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE",
                                          "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA",
                                          "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY",
                                          "PR", "VI", "NYC"),
                               state = c("Alabama", "Alaska", "Arizona", "Arkansas",
                                         "California", "Colorado", "Connecticut", "District of Columbia",
                                         "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois",
                                         "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine",
                                         "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi",
                                         "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
                                         "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
                                         "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island",
                                         "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah",
                                         "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming",
                                         "Puerto Rico", "Virgin Islands", "New York City"),
                               col = c(8L, 1L, 3L, 6L, 2L, 4L, 11L, 10L, 11L, 10L,
                                       9L, 1L, 3L, 7L, 7L, 6L, 5L, 7L, 6L, 12L, 10L, 11L, 8L, 6L, 7L,
                                       6L, 4L, 5L, 3L, 12L, 10L, 4L, 10L, 8L, 5L, 8L, 5L, 2L, 9L, 12L,
                                       9L, 5L, 7L, 5L, 3L, 11L, 9L, 2L, 8L, 7L, 4L, 12L, 12L, 12),
                               row = c(7L, 7L,
                                       6L, 6L, 5L, 5L, 4L, 6L, 5L, 8L, 7L, 8L, 3L, 3L, 4L, 4L, 6L, 5L,
                                       7L, 1L, 5L, 3L, 3L, 3L, 7L, 5L, 3L, 5L, 4L, 2L, 4L, 6L, 3L, 6L,
                                       3L, 4L, 7L, 4L, 4L, 4L, 6L, 4L, 6L, 8L, 5L, 2L, 5L, 3L, 5L, 2L, 4L,
                                       8L, 7L, 3L)),
                          .Names = c("abbrev", "state", "col", "row"),
                          class = "data.frame", row.names = c(NA, -54L))

state_coords <- state_coords[order(state_coords$state),]


#Makes sure that state names are valid
state_data <- validate_states(state_data, state_col, merge.x)



#adds coordinates to data
st.dat <- merge(state_coords, state_data, by.x=merge.x, by.y=state_col, all.y=TRUE,
                  sort=TRUE)
#  
gg <- ggplot()

GeomRrect <- ggplot2::ggproto("GeomRrect", ggplot2::Geom,
                              
                              default_aes = ggplot2::aes(
                                colour = NA, fill = "grey35", size = 0.5, linetype = 1, alpha = NA
                              ),
                              
                              required_aes = c("xmin", "xmax", "ymin", "ymax"),
                              
                              draw_panel = function(self, data, panel_params, coord,
                                                    radius = grid::unit(6, "pt")) {
                                
                                coords <- coord$transform(data, panel_params)
                                
                                lapply(1:length(coords$xmin), function(i) {
                                  
                                  grid::roundrectGrob(
                                    coords$xmin[i], coords$ymax[i],
                                    width = (coords$xmax[i] - coords$xmin[i]),
                                    height = (coords$ymax[i] - coords$ymin)[i],
                                    r = radius,
                                    default.units = "native",
                                    just = c("left", "top"),
                                    gp = grid::gpar(
                                      col = coords$colour[i],
                                      fill = alpha(coords$fill[i], coords$alpha[i]),
                                      lwd = coords$size[i] * .pt,
                                      lty = coords$linetype[i],
                                      lineend = "butt"
                                    )
                                  )
                                  
                                }) -> gl
                                
                                grobs <- do.call(grid::gList, gl)
                                
                                ggname("geom_rrect", grid::grobTree(children = grobs))
                                
                              },
                              
                              draw_key = ggplot2::draw_key_polygon
                              
)


geom_rrect <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       radius = grid::unit(6, "pt"),
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRrect,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      radius = radius,
      na.rm = na.rm,
      ...
    )
  )
}

GeomRtile <- ggplot2::ggproto("GeomRtile", GeomRrect,
                              
                              extra_params = c("na.rm", "width", "height"),
                              
                              setup_data = function(data, params) {
                                data$width <- data$width %||% params$width %||% ggplot2::resolution(data$x, FALSE)
                                data$height <- data$height %||% params$height %||% ggplot2::resolution(data$y, FALSE)
                                
                                transform(data,
                                          xmin = x - width / 2,  xmax = x + width / 2,  width = NULL,
                                          ymin = y - height / 2, ymax = y + height / 2, height = NULL
                                )
                              },
                              
                              default_aes = ggplot2::aes(
                                fill = "grey20", colour = NA, size = 0.1, linetype = 1, alpha = NA
                              ),
                              
                              required_aes = c("x", "y"),
                              
                              draw_key = ggplot2::draw_key_polygon
                              
)



#Not sure what this is for but it is necessary
geom_rtile <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       radius = 6,
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRtile,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      radius = radius,
      na.rm = na.rm,
      ...
    )
  )
}


#distiguishes between defined and undefined radius and assigns the gg object
  if (round) {
    gg <- gg + geom_rtile(data = st.dat, radius = radius,
                          aes_string(x = "col", y = "row", fill = value_col),
                          color = state_border_col, size = state_border_size)
  } else {
    gg <- gg + geom_tile(data = st.dat,
                         aes_string(x = "col", y = "row", fill = value_col),
                         color = state_border_col, size = state_border_size)
  }
  

#revers y scale
  gg <- gg + scale_y_reverse()
  #insert ggplot scale
  gg <- gg + ggplot2_scale_function(...)
  
  gg <- gg + coord_equal()
  gg <- gg + labs(x = NULL, y = NULL)
  
  gb <- ggplot2::ggplot_build(gg)
  
  gg <- gg + geom_text(data = st.dat,
                       aes_string(x = "col", y = "row", label = "abbrev"),
                       angle = 0,
                       color = .sb_invert(gb$data[[1]]$fill, dark_label, light_label, na_label),
                       size = font_size)
  
  gg

  

  