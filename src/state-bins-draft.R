library(readxl)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(statebins)

comp.math <- read_excel("visualization/comp-math.xlsx", skip = 3)
comp.math <- comp.math[-c(53, 55:61), ]

comp.math.18 <- comp.math %>% select("State", "2018", `2018__2`) %>% filter(!(State == "United States"))

colnames(comp.math.18)[colnames(comp.math.18)== "2018"] <- "count.18"
colnames(comp.math.18)[colnames(comp.math.18)== "2018__2"] <- "count.18.per"
comp.math.se <- read_excel("visualization/comp-math-se.xlsx", skip = 3)
comp.math.se <- comp.math.se[-c(53, 55:61), ]


comp.math.se.18 <- comp.math.se %>% select("State", "2018", `2018__2`) %>% filter(!(State == "United States"))

colnames(comp.math.se.18)[colnames(comp.math.se.18)== "2018"] <- "se.18"
colnames(comp.math.se.18)[colnames(comp.math.se.18)== "2018__2"] <- "se.18.per"


comp.math.18 <- merge(comp.math.18, comp.math.se.18, by = "State")



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
radius = grid::unit(6, "pt")
ggplot2_scale_function=ggplot2::scale_fill_distiller

varsize <- "0.2063717"

state_data <- data.frame(state_data, stringsAsFactors=FALSE)

#labels if state names are full nare or abbreviations
if (max(nchar(state_data[,state_col])) <= 3) {
  merge.x <- "abbrev"
} else {
  merge.x <- "state"
}
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


## Defines .pt
.pt <- 2.84527559055118

# Grid Graphical Object
ggname <- function(prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
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


## Function that Chooses a over b unless a is Null
"%||%" <- function(a, b) { if (!is.null(a)) a else b }

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


# circle radius is NOT TO SCALE AT ALL
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

#THIS IS WHERE TO INSERT VARIANCE THINGS

gg <- gg + geom_point(data = st.dat, aes(x = st.dat$col, y = st.dat$row), size = st.dat$se.18.per*100, alpha = .5, color = "red")


gg <- gg + scale_y_reverse()
gg <- gg + ggplot2_scale_function()
gg <- gg + coord_equal()
gg <- gg + labs(x = NULL, y = NULL)



gb <- ggplot2::ggplot_build(gg)

gg <- gg + geom_text(data = st.dat,
                     aes_string(x = "col", y = "row", label = "abbrev"),
                     angle = 0,
                     color = .sb_invert(gb$data[[1]]$fill, dark_label, light_label, na_label),
                     size = font_size)

gg + 
  theme_statebins(legend_position="bottom") +
  labs(title = "Computer and Mathematical Scientists as a \nPercent Error of All Occupations by State")





# Error Squred (understates the SE)

gg <- ggplot()

if (round) {
  gg <- gg + geom_rtile(data = st.dat, radius = radius,
                        aes_string(x = "col", y = "row", fill = value_col),
                        color = state_border_col, size = state_border_size) + 
    geom_tile(data = st.dat, aes(x = col, y = row,height = se.18.per, width = se.18.per), fill = "red", color = "red", alpha =0.8) + 
    scale_y_reverse()
} else {
  gg <- gg + geom_tile(data = st.dat,
                       aes_string(x = "col", y = "row", fill = value_col),
                       color = state_border_col, size = state_border_size) + 
    geom_tile(data = st.dat, aes(x = col, y = row, height = se.18.per, width = se.18.per), fill = "red", color = "red", alpha =0.8) + 
    scale_y_reverse()
}


gg <- gg + ggplot2_scale_function( direction = 1, "Proportion of Computer and Mathematical Scientists")
gg <- gg + coord_equal()
gg <- gg + labs(x = NULL, y = NULL)

gb <- ggplot2::ggplot_build(gg)

gg <- gg + geom_text(data = st.dat,
                     aes_string(x = "col", y = "row", label = "abbrev"),
                     angle = 0,
                     color = .sb_invert(gb$data[[1]]$fill, dark_label, light_label, na_label),
                     size = font_size)

gg + 
  theme_statebins(legend_position="bottom") +
  labs(title = "Computer and Mathematical Scientists as a \nProportion of All Occupations by State", legend = "Proportion of Computer and Mathematical Scientists", caption = "The red squares indicate standard error. Each side of the box equals percent error. \nEx) Washington DC SE = .30, each side of the box equals .30.")



# SQUARE ROOT (Overstates SE)
gg <- ggplot()

if (round) {
  gg <- gg + geom_rtile(data = st.dat, radius = radius,
                        aes_string(x = "col", y = "row", fill = value_col),
                        color = state_border_col, size = state_border_size) + 
    geom_tile(data = st.dat, aes(x = col, y = row,height = sqrt(se.18.per), width = sqrt(se.18.per)), fill = "red", color = "red", alpha =0.8) + 
    scale_y_reverse()
} else {
  gg <- gg + geom_tile(data = st.dat,
                       aes_string(x = "col", y = "row", fill = value_col),
                       color = state_border_col, size = state_border_size) + 
    geom_tile(data = st.dat, aes(x = col, y = row, height = sqrt(se.18.per), width = sqrt(se.18.per)), fill = "red", color = "red", alpha =0.8) + 
    scale_y_reverse()
}


gg <- gg + ggplot2_scale_function( direction = 1, "Proportion of Computer and Mathematical Scientists")
gg <- gg + coord_equal()
gg <- gg + labs(x = NULL, y = NULL)

gb <- ggplot2::ggplot_build(gg)

gg <- gg + geom_text(data = st.dat,
                     aes_string(x = "col", y = "row", label = "abbrev"),
                     angle = 0,
                     color = .sb_invert(gb$data[[1]]$fill, dark_label, light_label, na_label),
                     size = font_size)

gg + 
  theme_statebins(legend_position="bottom") +
  labs(title = "Computer and Mathematical Scientists as a \nProportion of All Occupations by State", legend = "Proportion of Computer and Mathematical Scientists", caption = "The red squares indicate standard error. The area of the box idicate standard error. \nEx) Washington DC SE = .30, each side of the box equals .55, area = 0.3.")



#RECTANGE 

gg <- ggplot()

if (round) {
  gg <- gg + geom_rtile(data = st.dat, radius = radius,
                        aes_string(x = "col", y = "row", fill = value_col),
                        color = state_border_col, size = state_border_size) + 
    geom_tile(data = st.dat, aes(x = col, y = row, height = se.18.per, width = 1), fill = "red", color = "red", alpha =0.8) + 
    scale_y_reverse()
} else {
  gg <- gg + geom_tile(data = st.dat,
                       aes_string(x = "col", y = "row", fill = value_col),
                       color = state_border_col, size = state_border_size) + 
    geom_tile(data = st.dat, aes(x = col, y = row, height = se.18.per, width = 1), fill = "red", color = "red", alpha =0.8) + 
    scale_y_reverse()
}


gg <- gg + ggplot2_scale_function()
gg <- gg + coord_equal()
gg <- gg + labs(x = NULL, y = NULL)

gb <- ggplot2::ggplot_build(gg)

gg <- gg + geom_text(data = st.dat,
                     aes_string(x = "col", y = "row", label = "abbrev"),
                     angle = 0,
                     color = .sb_invert(gb$data[[1]]$fill, dark_label, light_label, na_label),
                     size = font_size)

gg + 
  theme_statebins(legend_position="bottom") +
  labs(title = "Computer and Mathematical Scientists as a \nPercent Error of All Occupations by State")



#RECTANGLE SHIFT

gg <- ggplot()

if (round) {
  gg <- gg + geom_rtile(data = st.dat, radius = radius,
                        aes_string(x = "col", y = "row", fill = value_col),
                        color = state_border_col, size = state_border_size) + 
    geom_tile(data = st.dat, aes(x = col, y = row-1+0.5+0.5*se.18.per, height = se.18.per, width = 1), fill = "red", color = "red", alpha =0.8) + 
    scale_y_reverse()
} else {
  gg <- gg + geom_tile(data = st.dat,
                       aes_string(x = "col", y = "row", fill = value_col),
                       color = state_border_col, size = state_border_size) + 
    geom_tile(data = st.dat, aes(x = col, y = row-1+0.5+0.5*se.18.per, height = se.18.per, width = 1), fill = "red", color = "red", alpha =0.8) + 
    scale_y_reverse()
}


gg <- gg + ggplot2_scale_function()
gg <- gg + coord_equal()
gg <- gg + labs(x = NULL, y = NULL)

gb <- ggplot2::ggplot_build(gg)

gg <- gg + geom_text(data = st.dat,
                     aes_string(x = "col", y = "row", label = "abbrev"),
                     angle = 0,
                     color = .sb_invert(gb$data[[1]]$fill, dark_label, light_label, na_label),
                     size = font_size)

gg + 
  theme_statebins(legend_position="bottom") +
  labs(title = "Computer and Mathematical Scientists as a \nPercent Error of All Occupations by State")





# Plotting Red Squares under border
# will need to figure out dots at corners of squares for rounded statebins

gg <- ggplot()

if (round) {
  gg <- gg + geom_rtile(data = st.dat, radius = radius,
                        aes_string(x = "col", y = "row", fill = value_col),
                        color = state_border_col, size = state_border_size) +
    geom_tile(data = st.dat, aes(x = col, y = row-1+0.5+0.5*se.18.per, height = se.18.per, width = 1), fill = "red", color = "white", alpha =0.5) +
    geom_rtile(data = st.dat, radius = radius,
               aes_string(x = "col", y = "row", fill = value_col),
               color = state_border_col, size = state_border_size, alpha = 0)  +
    scale_y_reverse()
} else {
  gg <- gg + geom_tile(data = st.dat,
                       aes_string(x = "col", y = "row", fill = value_col),
                       color = state_border_col, size = state_border_size) + 
    geom_tile(data = st.dat, aes(x = col, y = row-1+0.5+0.5*se.18.per, height = se.18.per, width = 1), fill = "red", color = "red", alpha =0.5) + 
    geom_tile(data = st.dat, aes(x = col, y = row-1+0.5+0.5*se.18.per, height = se.18.per, width = 1), fill = "red", color = "red", alpha =0.8)  + geom_tile(data = st.dat, aes_string(x = "col", y = "row", fill = value_col), color = state_border_col, size = state_border_size, alpha = 0) +
    scale_y_reverse()
}


gg <- gg + ggplot2_scale_function(direction = 1, "Proportion of Computer and Mathematical Scientists")
gg <- gg + coord_equal()
gg <- gg + labs(x = NULL, y = NULL)

gb <- ggplot2::ggplot_build(gg)

gg <- gg + geom_text(data = st.dat,
                     aes_string(x = "col", y = "row", label = "abbrev"),
                     angle = 0,
                     color = .sb_invert(gb$data[[1]]$fill, dark_label, light_label, na_label),
                     size = font_size)

gg + 
  theme_statebins(legend_position="bottom") +
  labs(title = "Computer and Mathematical Scientists as a \nProportion of All Occupations by State", caption = "Area of red box represents stand error. \nEx) Washington DC SE% = 0.3, height = 0.3, width = 1, area = 0.3")





############MATH THINGS

# if we use sqrt(se.18.per), then area = .552 *.552 = 0.305, so area of red box is .305 of the area of teh state bin (1)
ggplot()+
  geom_tile(data = st.dat, aes(x = col, y = row, height = sqrt(se.18.per), width = sqrt(se.18.per)), fill = "red", color = "red", alpha =0.8)+
  geom_tile(data = st.dat, aes(x = col, y = row, height = 1, width = 1), fill = "yellow", color = "red", alpha =0.4) +
  coord_equal()


# if we use se.18.per, then area = .305*.305 =0.09 
ggplot()+
  geom_tile(data = st.dat, aes(x = col, y = row, height = se.18.per, width = se.18.per), fill = "red", color = "red", alpha =0.8)+
  geom_tile(data = st.dat, aes(x = col, y = row, height = 1, width = 1),  fill = "yellow", color = "red", alpha =0.4) +
  coord_equal()



#rectangle instead of square
ggplot()+
  geom_tile(data = st.dat, aes(x =col, y = row, height = se.18.per, width = 1), fill = "red", color = "red", alpha =0.8)+
  geom_tile(data = st.dat, aes(x = col, y = row, height = 1, width = 1), fill = "yellow", color = "red", alpha =0.4) +
  coord_equal()




#rectangle shifted downwards
ggplot()+
  geom_tile(data = st.dat, aes(x =col, y = row-1+0.5+0.5*se.18.per, height = se.18.per, width = 1), fill = "red", color = "red", alpha =0.8)+
  geom_tile(data = st.dat, aes(x = col, y = row, height = 1, width = 1), fill = "yellow", color = "red", alpha =0.4) +
  coord_equal()









