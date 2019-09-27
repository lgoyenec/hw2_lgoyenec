# -------------------------------------------------------------------
# Based on: https://github.com/rstudio/shinydashboard/issues/25
# -------------------------------------------------------------------

convertMenuItem = function(mi,tabName) {
  
  mi$children[[1]]$attribs['data-toggle'] ="tab"
  mi$children[[1]]$attribs['data-value']  = tabName
  
  if(length(mi$attribs$class) > 0 && mi$attribs$class == "treeview") {
    mi$attribs$class = NULL
  }
  
  mi
  
}

# -------------------------------------------------------------------