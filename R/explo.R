## entered vs target
source('./analytics.R')


plot(B$pat_TARGET_TREATMENT,B$pat_ENTERED_TREATMENT) ; abline(0,1)


ggplot(B,aes(x=pat_TARGET_TREATMENT,y=pat_ENTERED_TREATMENT,
                  color='PI tier')) + 
  geom_point() 

ggplot(B,aes(x=pat_TARGET_TREATMENT,y=pat_ENTERED_TREATMENT,
             color=Spec)) + 
  geom_jitter(alpha=0.4,size=3,width = 0.2, height = 0.2) +
  xlab('Target patients number') + 
  ylab('Patients entered')


## result
DF = B_Opht_ag[order(B_Opht_ag$Score,decreasing = TRUE)[1:20],]
DF = data.frame(Name=factor(paste(DF$PI_FORENAME,DF$PI_SURNAME)),
                Score=DF$Score , Performance=DF$Performance)
ggplot(DF,aes(x=reorder(Name,Score),y=Score,fill=Performance)) + 
  geom_bar(position = "dodge",stat="identity") + 
  coord_flip() + xlab('')

