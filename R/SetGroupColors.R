#' @title Set colors for each group.
#' @noRd
#' @param nGroup The number of groups
#' @return A vector of \code{nGroup} colors.
#' @importFrom grDevices colorRampPalette rgb
#'
SetGroupColors <- function ( nGroup ) {
  colors <- c(
    rgb(255/255,0/255,0/255),#1
    rgb(160/255,90/255,120/255),
    # rgb(159/255,89/255,89/255),
    rgb(245/255,190/255,187/255), 
    rgb(240/255,163/255,51/255), #2
    rgb(165/255,195/255,0/255), #3
    rgb(0/255,160/255,80/255),#4
    rgb(162/255,230/255,182/255),
    rgb(185/255,185/255,235/255), #6
    # grey(0.7), #8
    rgb(168/255,102/255,255/255),
    rgb(65/255,0/255,110/255), 
    rgb(95/255,125/255,180/255),
    # rgb(120/255,235/255,255/255), #5
    rgb(170/255,210/255,255/255),
    rgb(65/255,180/255,255/255), #5
    rgb(0/255,0/255,255/255) #7
    # rgb(250/255,120/255,250/255), #9
  )
  # delOrder <- c(11,2,12,3,7,9,10,13,4,8,6)
  delOrder <- c(11,2,12,3,10,9,8,7,5,6,13,4)
  if ( length(colors) > nGroup ) {
    if ( nGroup < length(colors) - length(delOrder) ) {
      colors <- colorRampPalette( colors = colors )( nGroup )
    } else {
      colors <- colors[ -delOrder[ seq_len( length(colors) - nGroup ) ] ]
    }
  } else if ( length(colors) < nGroup ) {
    # colors <- scales::hue_pal()(nGroup)
    colors <- colorRampPalette( c("#FF0000", "#F0D333", "#A5EE80", "#41B4FF", "#0000FF") )(nGroup)
    # colors <- rainbow( nGroup, end = 2/3 )
  }
  # plot(seq_along(colors),pch = 16, col = colors)
  colors <- colors[length(colors):1]
  return (colors)
}
