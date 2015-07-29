#' Build polynomial predictor variables
#' 
#' Takes a data frame, name of predictor variable, and polynomial order. Creates polynomial-transformed predictor variables, adds them to the data frame and returns the result. Original data frame is unchanged, remember to assign the result.
#' 
#' @param df data frame, should not contain any variables called "predictor"
#' @param predictor string name of predictor variable
#' @param poly.order integer order of polynomial to be created
#' @param orthogonal logical value indicating whether polynomial should be orthogonal (default) or natural (aka raw)
#' @param draw.poly logical value indicating whether to create a graph showing tranformed polynomial predictor values, defaults to TRUE
#' @return Returns a data frame containing the original data and at least two new columns: "predictor".Index, and a column for each order of the polynomial-transformed predictor
#' @examples
#' WordLearnEx.gca <- code.poly(df=WordLearnEx, predictor="Block", poly.order=2)
#' Az.gca <- code.poly(df=Az, predictor="Time", poly.order=2, orthogonal=FALSE)
#' @section Contributors:
#' Originally written by Matt Winn \url{http://www.mattwinn.com/tools/R_add_polynomials_to_df.html} and revised by Dan Mirman to handle various corner cases.
code_poly <- function(df=NULL, predictor=NULL, poly.order=NULL, orthogonal=TRUE, draw.poly=TRUE){
  if (draw.poly){
    require(ggplot2)
    require(reshape2)
  }
  # Codes raw or orthogonal polynomial transformations of a predictor variable
  # be sure to not have an actual variable named "predictor" in your data.frame
  # ultimately adds 2 or more columns, including:
  # (predictor).Index, and a column for each order of the polynomials 
  #
  # Written by Matt Winn: http://www.mattwinn.com/tools/R_add_polynomials_to_df.html
  # Revised by Dan Mirman (11/3/2014): 
  #   - call to poly now uses predictor.index and 1:max instead of unique() to deal with out-of-order time bins
  #   - combined indexing and alignment with original data, mainly to avoid problems when poly.order==1
  # Revised by Matt Winn (2/12/2015)
  #   - uses `[` instead of `$` to use variable name to extract column
  #   - computes polynomial on unique(sort(predictor.vector))
  #       rather than 1:max(predictor.vector) 
  #       to accomodate non-integer predictor (e.g. time) levels
  #       (such as those produced by a Tobii 60-Hz eye tracker)
  #   - Accomodates missing/unevenly-spaced time bins 
  #       by indexing each sorted unique time bin and using the index to extract
  #       the polynomial value
  # Revised by Matt Winn (3/19/2015)
  #   - uses dplyr::collect to handle columns in tbl_df objects
  #     (of the type created when using dplyr),
  #     from which you cannot extract pure vectors using `[` or `select`
  #     it looks like this: . %>% collect %>% .[[predictor]] 
  # Revised by Matt Winn (7/29/2015)
  #   - no longer uses dplyr;
  #   - simply uses `[[` to extract predictor (time) column
  #===========================================================#
  # convert choice for orthogonal into choice for raw
  raw <- (orthogonal-1)^2
  
  # Make sure that the declared predictor is actually present in the data.frame
  if (!predictor %in% names(df)){
    stop(paste0(predictor, " is not a variable in your data frame. Check spelling and try again"))
  }
  
  # Extract the vector to be used as the predictor
  predictor.vector <- df[[predictor]]
  
  # Create index of predictor (e.g. numbered time bins)
  # The index of the time bin will be used later as an index to call the time sample.
  predictor.indices <- as.numeric(as.factor(predictor.vector))
  
  df$temp.predictor.index <- predictor.indices
  
  # Create n-order order polynomials (orthogonal if not raw)
  predictor.polynomial <- poly(x = unique(sort(predictor.vector)), 
                               degree = poly.order, raw=raw)
  
  # Use predictor index as index to align 
  #   polynomial-transformed predictor values with original dataset
  #   (as many as called for by the polynomial order).
  df[, paste("poly", 1:poly.order, sep="")] <- 
    predictor.polynomial[predictor.indices, 1:poly.order]
  
  # Draw a plot of the polynomial transformations, if desired
  if (draw.poly == TRUE){
    # extract the polynomials from the df
    df.poly <- unique(df[c(predictor, paste("poly", 1:poly.order, sep=""))])
    
    # Melt from wide to long format
    df.poly.melt <- melt(df.poly, id.vars=predictor)
    
    # Make level names intuitive for plot
    #  don't bother with anything above 6th order. 
    levels(df.poly.melt$variable)[levels(df.poly.melt$variable)=="poly1"] <- "Linear"
    levels(df.poly.melt$variable)[levels(df.poly.melt$variable)=="poly2"] <- "Quadratic"
    levels(df.poly.melt$variable)[levels(df.poly.melt$variable)=="poly3"] <- "Cubic"
    levels(df.poly.melt$variable)[levels(df.poly.melt$variable)=="poly4"] <- "Quartic"
    levels(df.poly.melt$variable)[levels(df.poly.melt$variable)=="poly5"] <- "Quintic"
    levels(df.poly.melt$variable)[levels(df.poly.melt$variable)=="poly6"] <- "Sextic"
    
    # Change some column names for the output
    colnames(df.poly.melt)[colnames(df.poly.melt) == "variable"] <- "Order"
    
    poly.plot <- ggplot(df.poly.melt, aes(y=value, color=Order))+
      aes_string(x=predictor)+
      geom_line()+
      xlab(paste0(predictor, "\n (raw predictor)"))+
      ylab("Transformed polynomial value")+
      scale_color_brewer(palette="Set1")+
      theme_bw()
    
    print(poly.plot)
  }
  
  # Restore correct column names
  colnames(df)[colnames(df) == "temp.predictor.index"] <- paste0(predictor,".Index")
  
  # Return the original data.frame with polynomial columns 
  # that can be used as model predictors
  return(df)
}
