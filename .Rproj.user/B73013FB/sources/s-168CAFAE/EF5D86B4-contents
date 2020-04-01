#
# label_encode.R
# Stroke-CPM
#
# Created by Tosin-Dairo on 28/03/2020
# MIT License
#

## @knitr encode
encode.fit_transform<-function(df, plug_missing=TRUE){
  
  list_of_levels=list()  #empty list   
  
  #loop through the columns
  for (i in 1: ncol(df))
  {
    
    #only   
    if (is.character(df[,i]) ||  is.factor(df[,i]) ){
      
      #deal with missing
      if(plug_missing){
        
        #if factor
        if (is.factor(df[,i])){
          df[,i] = factor(df[,i], levels=c(levels(df[,i]), 'MISSING'))
          df[,i][is.na(df[,i])] = 'MISSING' 
          
          
        }else{   #if character
          
          df[,i][is.na(df[,i])] = 'MISSING' 
          
        }
      }#end missing IF
      
      levels<-unique(df[,i]) #distinct levels
      list_of_levels[[colnames(df)[i]]] <- levels #set list with name of the columns to the levels
      df[,i] <- as.numeric(factor(df[,i], levels = levels))
      
    }#end if character/factor IF
    
    
  }#end loop
  
  return (list(list_of_levels,df)) #return the list of levels and the new DF
  
}#end of function


## @knitr decode
encode.transform<-function(df,list_of_levels,plug_missing=TRUE)
{
  #loop through the columns
  for (i in 1: ncol(df))
  {
    
    #only   
    if (is.character(df[,i]) ||  is.factor(df[,i]) ){
      
      
      #deal with missing
      if(plug_missing){
        
        #if factor
        if (is.factor(df[,i])){
          df[,i] = factor(df[,i], levels=c(levels(df[,i]), 'MISSING'))
          df[,i][is.na(df[,i])] = 'MISSING' 
          
          
        }else{   #if character
          
          df[,i][is.na(df[,i])] = 'MISSING' 
          
        }
      }#end missing IF
      
      levels=list_of_levels[[colnames(df)[i]]]
      
      if (! is.null(levels)){
        df[,i] <- as.numeric(factor(df[,i], levels = levels))
      }
      
    }# character or factor
    
  }#end of loop
  
  return(df)
  
}#end of function
