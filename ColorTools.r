
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
