library('move')
library('foreach')

rFunction <- function(data, selName=NULL, startVar=NULL, endVar=NULL, trackVar="trackId")
{
  Sys.setenv(tz="UTC") 
  
  if (is.null(selName) | (selName %in% namesIndiv(data))==FALSE) 
  {
    logger.info("Your property name (e.g. 'nest') is not set or does not exist in the data. This will lead to an error.")
    result <- NULL
  } else
  {
    ix <- which(namesIndiv(data)==selName)
    selT <- data[[ix]]
    data <- data[[-ix]]
   
    data.split <- move::split(data)
    
    data_filter <- foreach(datai = data.split) %do% {
      id <- namesIndiv(datai)
      logger.info(id)
      
      minT <- min(timestamps(datai))
      maxT <- max(timestamps(datai))
      
      if (is.null(startVar) | (startVar %in% names(datai))==FALSE)
      {
        time1 <- minT
        logger.info(paste("You have not provided a starting time variable name or the provided names does not exist in the data. Therefore, each track is filtered by its minimum timestamp."))
      } else 
      {
        if (any(selT@data[,trackVar]==id)) time1 <- as.POSIXct(selT@data[which(selT@data[,trackVar]==id),startVar]) else time1 <- NA
      }
      
      if (is.null(endVar) | (endVar %in% names(datai))==FALSE)
      {
        timeN <- maxT
        logger.info(paste("You have not provided an end time variable name or the provided names does not exist in the data. Therefore, each track is filtered by its maximum timestamp."))
      } else 
      {
        if (any(selT@data[,trackVar]==id)) timeN <- as.POSIXct(selT@data[which(selT@data[,trackVar]==id),endVar]) else timeN <- NA  
      }
      
      if (is.na(time1) | is.na(timeN))
      {
        
        dataif <- NULL
        logger.info (paste0("There are no start/stop data avaiable for the track",id,". Therefore, all locations of this track are removed from the result."))
        
      } else
      {
        if (time1>timeN)
        {
          time10 <- time1
          timeN0 <- timeN
          time1 <- timeN0
          timeN <- time10
          logger.info("Warning! Your start timestamp is after your end timestamp. We assume that they were switched and filter the data accordingly.")
        }
        
        if (minT>timeN | maxT<time1)
        {
          logger.info("Warning! None of your data lie in the requested time range. All locations of this track are removed from the result.")
          dataif <- NULL
        } else
        {
          dataif <- datai[timestamps(datai)>=time1 & timestamps(datai)<=timeN,]
          logger.info(paste("Filtering successful. It found",length(dataif),"positions of",length(datai),"locations for the track",id,"."))
        }
      }

      dataif
    }
    names(data_filter) <- selT@data[,trackVar]
    
    data_filter.nozero <- data_filter[unlist(lapply(data_filter, length) > 0)]
    result <- moveStack(data_filter.nozero,forceTz="UTC")

  }

  return(result)
}
  
