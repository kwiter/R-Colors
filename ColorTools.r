
#function interpolates colors for continous 0:1 data
colorRange <- function(percent = .5,colours = c('#AF2F03','white','#027A40'),trans = 1){
  #percent: data values scaled from 0 to 1
  #colours: colors to interpolate between
  #trans: transparancy of colors
  
  #cols = rgb2hsv(col2rgb(colours))
  cols = col2rgb(colours)/256
  ncolours = length(colours)
  whr = percent*(ncol(cols)-1)+1
  whr.lo = floor(whr)
  whr.hi = ceiling(whr)
  return(
      rgb( #hsv
      (ncolours - 1)*(percent - (whr.lo-1)/(ncolours - 1)) * (cols[1,whr.hi] - cols[1,whr.lo]) + cols[1,whr.lo],
      (ncolours - 1)*(percent - (whr.lo-1)/(ncolours - 1)) * (cols[2,whr.hi] - cols[2,whr.lo]) + cols[2,whr.lo],
      (ncolours - 1)*(percent - (whr.lo-1)/(ncolours - 1)) * (cols[3,whr.hi] - cols[3,whr.lo]) + cols[3,whr.lo],
      trans
      )
  )
}

#function provides equally spaced colors on color wheel
nColor <- function(number = 2,base.col = '#AF2F03',trans = 1,distinct = FALSE){
  #number: number of colors
  #base.col: base color to use as origin
  #trans: transparency
  #distinct: order colors to maximize distinction between colors
  
  b.col = rgb2hsv(col2rgb(base.col))
  spread = 1/number
  cols = matrix(NA,3,number)
  cols[,1] = b.col
  if(number > 1){
    for(i in 2:number){
      cols[1,i] = b.col[1]  + (i-1)/number
      cols[2:3,i] = b.col[2:3]
    }
  }
  cols[1,cols[1,] > 1] = cols[1,cols[1,] > 1] - 1
  order = 1:number
  
  if(number > 2 & distinct == TRUE){
    if(number %% 2 == 0){
      order = c()
      one = 1:(number/2)
      two = (number/2+1):number
      for(i in 1:length(one)){
        order = c(order, one[i] , two[i])
      }
    }else{
      order = 1
      one = 2:(floor(number/2)+1)
      two = (floor(number/2)+2):number  
      for(i in 1:length(one)){
        order = c(order, two[i] , one[i])
      }
    }
  }
  
  
  hsv(cols[1,order],cols[2,order],cols[3,order],trans)
  
}

#continous bivariate color scheme using linear models interpolate colors
bivarColor = function(v1 = .5,v2 = .5, cols = c("#64acbe","#627f8c","#574249",
                                                "#b0d5df","#ad9ea5","#985356",
                                                "#e8e8e8","#e4acac","#c85a5a")){
 
  red = col2rgb(cols)[1,]/256
  green = col2rgb(cols)[2,]/256
  blu = col2rgb(cols)[3,]/256
  x = rep(seq(0,1,length = sqrt(length(cols))),sqrt(length(cols)))
  y = rep(seq(1,0,length = sqrt(length(cols))),each = sqrt(length(cols)))
  rgb(predict(lm(red   ~ x + y),data.frame(x = v1,y = v2)),
      predict(lm(green ~ x + y),data.frame(x = v1,y = v2)),
      predict(lm(blu   ~ x + y),data.frame(x = v1,y = v2)))

}

#continous bivariate color scheme using a a spline surface to interpolate colors
bivarSplineColor = function(v1 = .5,v2 = .5,cols = c("#64acbe","#627f8c","#574249",
                                                     "#b0d5df","#ad9ea5","#985356",
                                                     "#e8e8e8","#e4acac","#c85a5a")){
  require(akima)
  #fuzWHICH() needs in mckFunctions or helper.github
  red = col2rgb(cols)[1,]/256
  green = col2rgb(cols)[2,]/256
  blue = col2rgb(cols)[3,]/256
  x = rep(seq(0,1,length = sqrt(length(cols))),sqrt(length(cols)))
  y = rep(seq(1,0,length = sqrt(length(cols))),each = sqrt(length(cols)))
  surface = interp(x= y,y= x,z = red)
  xs = fuzWHICH(v1,surface$y)
  ys = fuzWHICH(v2,surface$x)
  red = surface$z[cbind(ys,xs)]
  surface = interp(x= y,y= x,z = green)
  xs = fuzWHICH(v1,surface$y)
  ys = fuzWHICH(v2,surface$x)
  green = surface$z[cbind(ys,xs)]
  surface = interp(x= y,y= x,z = blue)
  xs = fuzWHICH(v1,surface$y)
  ys = fuzWHICH(v2,surface$x)
  blue = surface$z[cbind(ys,xs)]
  rgb(red,green,blue)
  #rgb(predict(lm(red   ~ x + y),data.frame(x = v1,y = v2)),
  #    predict(lm(green ~ x + y),data.frame(x = v1,y = v2)),
  #    predict(lm(blue   ~ x + y),data.frame(x = v1,y = v2)))
  
}
