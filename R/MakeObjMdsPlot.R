#' @title Make an object MDS plot
#' @description Make a plot of 2-dimensional classical metric multidimensional scaling of the 
#' data based on the pairwise distance matrix or a scatterplot of 2-dimensional Euclidean data.
#' @param distmat An \eqn{n}-by-\eqn{n} symmetric matrix holding the pairwise distances between observations.
#' @param data An \eqn{n}-by-2 matrix or data frame holding the data; 
#' This is only applicable for 2-dimensional Euclidean data and overrides \code{distmat} if there is a proper input.
#' @param color_by A vector of length \eqn{n} holding the variable according to which 
#' colors are assigned to each point. If missing, all points will be in black.
#' @param nGroup Number of groups for optional grouping according to \code{color_by}. 
#' If given, the sample is divided into \code{nGroup} groups according to quantiles of \code{color_by}, 
#' which should be numeric in this case; the \eqn{i}-th group contains the 
#' points with the corresponding value in \code{color_by} falling in 
#' \eqn{( q_{1-i/\code{nGroup}}, q_{1-(i-1)/\code{nGroup}} ] }, 
#' except for \eqn{i=\code{nGroup}}, for which the corresponding interval is \eqn{[ q_{0}, q_{1/\code{nGroup}} ]}. 
#' Here, \eqn{q_{\alpha}} is the \eqn{\alpha}-quantile of \code{color_by}, given by \code{quantile(color_by,alpha)}. 
#' @param shape_by A vector of length \eqn{n} holding the variable according to which 
#' shapes are assigned to each point. Default: \code{NULL}.
#' @param shape Shape of points when \code{shape_by} is \code{NULL}. Default: 3. 
#' See \code{\link[ggplot2]{geom_point}} for details.
#' @param id A vector of length \eqn{n} holding the label of each point. 
#' Default: \code{rownames(data)} if \code{data} is given and 
#' \code{rownames(distmat)} otherwise.
#' @param id_size Size of the text labels of points. Default: 3.
#' @param xlab,ylab The labels of x-axis and y-axis. Default: \code{'Coordinate 1'} and \code{'Coordinate 2'}.
#' @param colorlab The label of the color variable when \code{color_by} is given. Default: \code{NULL}.
#' @param shapelab The label of the shape variable when \code{shape_by} is given. Default: \code{NULL}.
#' @param use_default_colors Logical, whether to use default colors when \code{color_by} is given. Default: \code{TRUE}.
#' @return A ggplot object.
#' @examples 
#' n <- 100
#' p <- 2
#' set.seed(1)
#' data <- matrix( rnorm( n * p ), ncol = p )
#' res <- GetTpRank( data = data )
#' pl <- MakeObjMdsPlot( data = data, color_by = res$rank, nGroup = 10 )
#' @importFrom stats cmdscale quantile
#' @import ggplot2
#' @importFrom ggrepel geom_text_repel
#' @seealso \code{\link{GetTpRank}}, \code{\link{MakePrflMdsPlot}}
#' @export
#' 
MakeObjMdsPlot <- function (
    distmat, data, color_by, nGroup, shape_by = NULL, shape = 3, id, id_size = 3,
    xlab = 'Coordinate 1', ylab = 'Coordinate 2', colorlab = NULL, shapelab = NULL,
    use_default_colors = TRUE
) {
  use_data <- !missing(data)
  if ( use_data ) {
    if ( ncol(data) != 2 ) {
      stop ( "The number of columns in the input data should be but is not 2.")
    }
    n <- nrow( data )
  } else {
    CheckDistmat( distmat )
    n <- nrow( distmat )
    if ( abs( n - ncol(distmat) ) > 0 ) {
      stop ( "The input distmat is not a square matrix." )
    }
  }
  
  missing_color_by <- missing(color_by)
  if ( !missing_color_by ) {
    
    if ( length(color_by) != n ) {
      stop ( "The length of the input color_by is not equal to the number of observations in distmat or data.")
    }
    if ( missing(nGroup) ) {
      work_color_by <- color_by
    } else {
      if ( !is.numeric(color_by) ) {
        stop ( "The input color_by is not but should be numeric with a given nGroup." )
      }
      cutoffs <- quantile( color_by, seq( 0, 1, length.out = nGroup + 1 ) )
      work_color_by <- findInterval( x = color_by, vec = cutoffs, all.inside = TRUE, left.open = TRUE )
      work_color_by <- factor( nGroup + 1 - work_color_by )
    }
  } 
  if ( is.null(shape_by) ) {
    missing_shape_by <- TRUE
  } else {
    missing_shape_by <- FALSE
    
    if ( length(shape_by) != n ) {
      stop ( "The length of shape_by is not equal to the number of observations in distmat or data.")
    }
  }
  
  if ( missing(id) ) {
    
    if ( use_data ) {
      id <- rownames( data )
    } else {
      id <- rownames( distmat )
    }
    
  } else {
    if ( length(id) != n ) {
      stop ( "The length of id is not equal to the number of rows/columns in distmat or data.")
    }
  }
  missing_id <- is.null(id)
  
  if ( use_data ) {
    df <- data
  } else {
    df <- stats::cmdscale( distmat, k = 2 )
  }
  df <- data.frame(df)
  names(df) <- c( 'x', 'y' )
  if ( !missing_color_by ) {
    df$color_by <- work_color_by
  }
  if ( !missing_id ) {
    df$id <- id
  }
  pl <- ggplot( data = df, aes( x = x, y = y ) )
  
  if ( missing_shape_by ) {
    
    if ( missing_color_by ) {
      pl <- pl + geom_point( shape = shape ) +
        labs( x = xlab, y = ylab )
    } else {
      pl <- pl + geom_point( aes( color = color_by ), shape = shape ) +
        labs( x = xlab, y = ylab, color = colorlab )
    }
    
  } else {
    
    if ( missing_color_by ) {
      pl <- pl + geom_point( aes( shape = shape_by ) ) +
        labs( x = xlab, y = ylab, shape = shapelab )
    } else {
      pl <- pl + geom_point( aes( color = color_by, shape = shape_by ) ) +
        labs( x = xlab, y = ylab, color = colorlab, shape = shapelab )
    }
    
  }
  
  if ( !missing_id ) {
    
    if ( missing_color_by ) {
      pl <- pl +
        geom_text_repel( aes( label = id ),
                         size = id_size, show.legend = FALSE )
    } else {
      pl <- pl +
        geom_text_repel( aes( label = id, color = color_by ),
                         size = id_size, show.legend = FALSE )
    }
    
  }
  
  if ( use_default_colors & !missing_color_by ) {
    if ( is.numeric( df$color_by ) ) {
      pl <- pl +
        scale_color_gradientn( colors = c("indianred2","grey80","royalblue2") )
    } else {
      pl <- pl +
        scale_color_manual( values = SetGroupColors( length( unique( df$color_by ) ) ) )
    }
  }
  
  pl + SetTheme()
}