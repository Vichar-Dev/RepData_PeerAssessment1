unzipFile <- "activity.csv"
downloadFile <- "activity.zip"

library(scales)

stepCountAnalysis <- function(){
    ## Check if dataset file has already been unzipped
    if(!file.exists(unzipFile)){
        
        ## now Unzip file and extract dataset
        unzip(downloadFile)
        
    } else {
        ## Dataset file exist already
    }
    
    
    StepCountData <- read.csv(unzipFile)
    
    StepCountDataNotNA<-StepCountData[complete.cases(StepCountData),]
    
    StepsPerDay <- aggregate(StepCountDataNotNA$steps,by=list(StepCountDataNotNA$date),FUN="sum")
    
    names(StepsPerDay)<-c("interval","steps")
    
    plot1<- ggplot(StepsPerDay,aes(x=interval, y=steps))+
        geom_bar(stat="identity")+
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90))+
        xlab("Date")+
        ylab("Step Count")+
        ggtitle("Steps Per Day")+
        geom_hline(yintercept = mean(StepsPerDay$steps), color="blue")+
        geom_hline(yintercept = median(StepsPerDay$steps), color="red")
    
    print(plot1)
    
    print(mean(StepsPerDay$steps))
    
    print(median(StepsPerDay$steps))
    
    AverageStepsPerInterval <- aggregate(StepCountDataNotNA$steps,by=list(StepCountDataNotNA$interval),FUN="mean")
    
    names(AverageStepsPerInterval)<-c("interval","steps")
    
    AverageStepsPerInterval$steps<-round(AverageStepsPerInterval$steps)
    
    plot2 <- ggplot(AverageStepsPerInterval, aes(x=interval, y=steps)) + 
        geom_line() + 
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90))+
        xlab("Time of Day") + 
        ggtitle("Steps Per 5 min Interval")+
        ylab("Average Step Count")+
        scale_x_continuous(breaks = 
                    round(seq(min(AverageStepsPerInterval$interval), 
                    max(AverageStepsPerInterval$interval), by = 60),1))

    print(plot2)  
    
    print(AverageStepsPerInterval$interval[which.max(AverageStepsPerInterval$steps)])
    
    print(nrow(StepCountData)-nrow(StepCountDataNotNA))
    
    StepCountDataNA <- StepCountData[!complete.cases(StepCountData),]
    
    StepCountDataNA<-merge(x=StepCountDataNA, y=AverageStepsPerInterval,by=c("interval"),all.x = TRUE)
                    
    StepCountDataNA <- StepCountDataNA[,c("interval","date","steps.y")]       
    
    names(StepCountDataNA)[3]<-"steps"
    
    StepCountDataComplete <- rbind(StepCountDataNotNA,StepCountDataNA)
    
    StepsPerDayComplete <- aggregate(StepCountDataComplete$steps,by=list(StepCountDataComplete$date),FUN="sum")
    
    names(StepsPerDayComplete)<-c("interval","steps")
    
    plot3<- ggplot(StepsPerDayComplete,aes(x=interval, y=steps))+
        geom_bar(stat="identity")+
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90))+
        xlab("Date")+
        ylab("Step Count")+
        ggtitle("Steps Per Day")+
        geom_hline(yintercept = mean(StepsPerDayComplete$steps), color="blue")+
        geom_hline(yintercept = median(StepsPerDayComplete$steps), color="red")
    
    print(plot3)
    
    print(mean(StepsPerDayComplete$steps))
    
    print(median(StepsPerDayComplete$steps))
    
    StepCountDataComplete$wd<-weekdays(as.Date(StepCountDataComplete$date))
    
    StepCountDataComplete$we = factor(ifelse(StepCountDataComplete$wd %in% c("Saturday", 
         "Sunday"), "weekend", "weekday"))
    
    AverageStepsPerInterval <- aggregate(StepCountDataComplete$steps,
         by=list(StepCountDataComplete$interval,StepCountDataComplete$we),FUN="mean")
    
    names(AverageStepsPerInterval)<-c("interval","we","steps")
    
    AverageStepsPerInterval$steps <- round(AverageStepsPerInterval$steps)
    
    plot4 <- ggplot(AverageStepsPerInterval, aes(x=interval, y=steps)) + 
        geom_line(color="blue") + 
        theme_bw()+
        theme(axis.text.x = element_text(angle = 90))+
        facet_grid(AverageStepsPerInterval$we~.)+
        xlab("Time of Day") + 
        ggtitle("Steps Per 5 min Interval")+
        ylab("Average Step Count")
    
    print(plot4)
}