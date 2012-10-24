library(RCurl)
library(RJSONIO)

#tap into the facebook site, set limit, and give url 
Result <- function(Facebook){
    fb.base<-paste("http://graph.facebook.com/search?q=",Facebook,"&limit=10",sep="") #change &limit= to whatever you want to retieve the desired number of observations
    fb.url<-getURL(fb.base)
    fb.parse<-fromJSON(fb.url)
    return(fb.parse)
      
    }
  
FBookkeyword<-function(x){
        #takes keyword from function
        fbkeyword <- x
        fbresult <- Result(fbkeyword)    
        fbdata <- fbresult$data
        fbdata.length <- length(fbdata)
        fbid = facebookers = fbmessage = fbpic = fblink = fbname = fbcaption = 
        fbdescription = fbicon = fbtype = fbcreated = fbupdated = fblikecount = 0
        #retrieve all variables from facebook
        for (i in 1:fbdata.length){
            fbid[i] <- fbdata[[i]]$id
            facebookers[i] <- fbdata[[i]]$from[[1]]
            fbmessage[i] <- ifelse(is.null(fbdata[[i]]$message),"No message",fbdata[[i]]$message)
            fbpic[i] <- ifelse(is.null(fbdata[[i]]$picture),"No picture found",fbdata[[i]]$picture)
            fblink[i] <- ifelse(is.null(fbdata[[i]]$link),"No link found",fbdata[[i]]$link)
            fbname[i] <- ifelse(is.null(fbdata[[i]]$name),"No name",fbdata[[i]]$name)
            fbcaption[i] <- ifelse(is.null(fbdata[[i]]$caption),"No caption",fbdata[[i]]$caption)
            fbdescription[i] <- ifelse(is.null(fbdata[[i]]$description),"No description found",fbdata[[i]]$description)
            fbicon[i] <- ifelse(is.null(fbdata[[i]]$icon),"No icon found",fbdata[[i]]$icon)
            fbtype[i] <- ifelse(is.null(fbdata[[i]]$type),"Unknown",fbdata[[i]]$type)
            fbcreated[i] <- ifelse(is.null(fbdata[[i]]$created_time),"Unknown",fbdata[[i]]$created_time)
            fbupdated[i] <- ifelse(is.null(fbdata[[i]]$updated_time),"Unknown",fbdata[[i]]$updated_time)
            fblikecount[i] <- ifelse(is.null(fbdata[[i]]$likes$count),0,fbdata[[i]]$likes$count)
     
            #for(j in 1:fblikecount[i]){ fblikename[i] <- ifelse(is.null(fbdata[[i]]$likes$data[[j]][[1]]),"0",fbdata[[i]]$likes$data[[j]][[1]]) }
        }
        #build all above variables into one data frame
        y<-as.data.frame(cbind(fbid, facebookers, fbmessage, fbpic, fblink, fbname, fbcaption, fbdescription, fbicon, fbtype, fbcreated, fbupdated, fblikecount, keyword=x))
    #adjust time zone to USA eastern time zone
    y$fbupdated<-format(as.POSIXct(strptime(as.character(y$fbupdated), "%Y-%m-%dT%H:%M:%S+0000"), tz="GMT"), tz="America/New_York",usetz=TRUE)
    y$fbcreated<-format(as.POSIXct(strptime(as.character(y$fbcreated), "%Y-%m-%dT%H:%M:%S+0000"), tz="GMT"), tz="America/New_York",usetz=TRUE)
    return(y)
}
  
                 
