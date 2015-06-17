# R-Colors
Color tools for R

##colorRange 
Function interpolates colors between user inputed colors for use with continuous data

*    percent: data values scaled from 0 to 1
*    colours: colors to interpolate between
*    trans: transparancy of colors

##nColor 
Function provides equally spaced colors on color wheel

*    number: number of colors
*    base.col: base color to use as origin
*    trans: transparency
*    distinct: order colors to maximize distinction between colors

##bivarColor 
Function provides coloring based on two contious variables instead of just 1 (colorRange) uses linear models to interpolate

*  v1: value between 0 and 1
*  v2: value between 0 and 1 
*  cols: colors for plot provided in a square
    *  c(top row left to right,middle row left to right,bottom row left to right)
    *  four colors minimum c(topleft,topright,bottomleft, bottomright)
*  trans: transparency

###bivarSplineColor 
Continous bivariate color scheme using a a spline surface to interpolate colors

*  v1: value between 0 and 1
*  v2: value between 0 and 1 
*  cols: colors for plot provided in a square
    *  c(top row left to right,middle row left to right,bottom row left to right)
    *  four colors minimum c(topleft,topright,bottomleft, bottomright)
*  trans: transparency
