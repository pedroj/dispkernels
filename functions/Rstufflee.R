#DECLUSTERING DATA USING THE RUNS METHOD#

#separation interva s=1#
cluster1<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (2):length(set))
    {
        if(set[i-1]>threshold & set[i]<=threshold)
        {
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=2#
cluster2<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (3):length(set))
    {
        if(set[i-2]>threshold
            & set[i-1]<=threshold & set[i]<=threshold)
        {
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=3#
cluster3<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (4):length(set))
    {
        if(set[i-3]>threshold
            & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
        {
            
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }
    }
    
}
    z
}

#separation interval s=4#
cluster4<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (5):length(set))
    {
        if(set[i-4]>threshold
            & set[i-3]<=threshold
            & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
        {
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=5#
cluster5<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (6):length(set))
    {
        if(set[i-5]>threshold
            & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
        {
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=6#
cluster6<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (7):length(set))
    {
        if(set[i-6]>threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
        {
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=7#
cluster7<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (8):length(set))
    {
        if(set[i-7]>threshold
            & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
        {
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=8#
cluster8<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (9):length(set))
    {
        if(set[i-8]>threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
        {
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=9#
cluster9<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (10):length(set))
    {
        if(set[i-9]>threshold
            & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
        {
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=10#
cluster10<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (11):length(set))
    {
        if(set[i-10]>threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
        {
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=11#

cluster11<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (12):length(set))
    {
        if(set[i-11]>threshold
            & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
        {
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=12#
cluster12<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (13):length(set))
    {
        if(set[i-12]>threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
        {
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=13#
cluster13<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (14):length(set))
    {
        if(set[i-13]>threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
        {
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=14#
cluster14<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (15):length(set))
    {
        if(set[i-14]>threshold & set[i-13]<=threshold
            & set[i-12]<=threshold & set[i-11]<=threshold 
            & set[i-10]<=threshold & set[i-9]<=threshold 
            & set[i-8]<=threshold & set[i-7]<=threshold 
            & set[i-6]<=threshold & set[i-5]<=threshold 
            & set[i-4]<=threshold & set[i-3]<=threshold 
            & set[i-2]<=threshold & set[i-1]<=threshold 
            & set[i]<=threshold)
        {
            x<-max(set[j:i])
            
            
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=15#
cluster15<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (16):length(set))
    {
        if(set[i-15]>threshold & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
        {
            
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=16#
cluster16<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (17):length(set))
    {
        if(set[i-16]>threshold & set[i-15]<=threshold
            & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
        {
            x<-max(set[j:i])
            
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=17#
cluster17<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (18):length(set))
    {
        if(set[i-17]>threshold & set[i-16]<=threshold
            & set[i-15]<=threshold
            & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
        {
            x<-max(set[j:i])
            
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=18#
cluster18<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (19):length(set))
    {
        if(set[i-18]>threshold & set[i-17]<=threshold
            & set[i-16]<=threshold
            & set[i-15]<=threshold
            & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=19#
cluster19<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (20):length(set))
    {
        if(set[i-19]>threshold & set[i-18]<=threshold
            & set[i-17]<=threshold
            & set[i-16]<=threshold
            & set[i-15]<=threshold
            & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=20#
cluster20<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (21):length(set))
    {
        if(set[i-20]>threshold & set[i-19]<=threshold
            & set[i-18]<=threshold
            & set[i-17]<=threshold
            & set[i-16]<=threshold
            & set[i-15]<=threshold
            & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=21#
cluster21<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (22):length(set))
    {
        if(set[i-21]>threshold & set[i-20]<=threshold
            & set[i-19]<=threshold
            & set[i-18]<=threshold
            & set[i-17]<=threshold
            & set[i-16]<=threshold
            & set[i-15]<=threshold
            & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=22#
cluster22<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (23):length(set))
    {
        if(set[i-22]>threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold
            & set[i-18]<=threshold
            & set[i-17]<=threshold
            & set[i-16]<=threshold
            & set[i-15]<=threshold
            & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=23#
cluster23<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (24):length(set))
    {
        if(set[i-23]>threshold
            & set[i-22]<=threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold
            & set[i-18]<=threshold
            & set[i-17]<=threshold
            & set[i-16]<=threshold
            & set[i-15]<=threshold
            & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=24#
cluster24<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (25):length(set))
    {
        if(set[i-24]>threshold
            & set[i-23]<=threshold
            & set[i-22]<=threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold
            & set[i-18]<=threshold
            & set[i-17]<=threshold
            & set[i-16]<=threshold
            & set[i-15]<=threshold
            & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=25#
cluster25<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (26):length(set))
    {
        if(set[i-25]>threshold
            & set[i-24]<=threshold
            & set[i-23]<=threshold
            & set[i-22]<=threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold
            & set[i-18]<=threshold
            & set[i-17]<=threshold
            & set[i-16]<=threshold
            & set[i-15]<=threshold
            & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}


#separation interval s=26#
cluster26<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (27):length(set))
    {
        if(set[i-26]>threshold
            & set[i-25]<=threshold
            & set[i-24]<=threshold
            & set[i-23]<=threshold
            & set[i-22]<=threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold
            & set[i-18]<=threshold
            & set[i-17]<=threshold
            & set[i-16]<=threshold
            & set[i-15]<=threshold
            & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=27#
cluster27<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (28):length(set))
    {
        if(set[i-27]>threshold
            & set[i-26]<=threshold
            & set[i-25]<=threshold
            & set[i-24]<=threshold
            & set[i-23]<=threshold
            & set[i-22]<=threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold
            & set[i-18]<=threshold
            & set[i-17]<=threshold
            & set[i-16]<=threshold
            & set[i-15]<=threshold
            & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=28#
cluster28<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (29):length(set))
    {
        if(set[i-28]>threshold
            & set[i-27]<=threshold
            & set[i-26]<=threshold
            & set[i-25]<=threshold
            & set[i-24]<=threshold
            & set[i-23]<=threshold
            & set[i-22]<=threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold
            & set[i-18]<=threshold
            & set[i-17]<=threshold
            & set[i-16]<=threshold
            & set[i-15]<=threshold
            & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=29#
cluster29<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (30):length(set))
    {
        if(set[i-29]>threshold
            & set[i-28]<=threshold
            & set[i-27]<=threshold
            & set[i-26]<=threshold
            & set[i-25]<=threshold
            & set[i-24]<=threshold
            & set[i-23]<=threshold
            & set[i-22]<=threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold
            & set[i-18]<=threshold
            & set[i-17]<=threshold
            & set[i-16]<=threshold
            & set[i-15]<=threshold
            & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}




#separation interval s=30#
cluster30<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (31):length(set))
    {
        if(set[i-30]>threshold
            & set[i-29]<=threshold & set[i-28]<=threshold
            & set[i-27]<=threshold & set[i-26]<=threshold
            & set[i-25]<=threshold & set[i-24]<=threshold
            & set[i-23]<=threshold & set[i-22]<=threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold & set[i-18]<=threshold
            & set[i-17]<=threshold & set[i-16]<=threshold
            & set[i-15]<=threshold & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=31#
cluster31<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (32):length(set))
    {
        if(set[i-31]>threshold
            & set[i-30]<=threshold
            & set[i-29]<=threshold & set[i-28]<=threshold
            & set[i-27]<=threshold & set[i-26]<=threshold
            & set[i-25]<=threshold & set[i-24]<=threshold
            & set[i-23]<=threshold & set[i-22]<=threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold & set[i-18]<=threshold
            & set[i-17]<=threshold & set[i-16]<=threshold
            & set[i-15]<=threshold & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=32#
cluster32<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (33):length(set))
    {
        if(set[i-32]>threshold
            & set[i-31]<=threshold
            & set[i-30]<=threshold
            & set[i-29]<=threshold & set[i-28]<=threshold
            & set[i-27]<=threshold & set[i-26]<=threshold
            & set[i-25]<=threshold & set[i-24]<=threshold
            & set[i-23]<=threshold & set[i-22]<=threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold & set[i-18]<=threshold
            & set[i-17]<=threshold & set[i-16]<=threshold
            & set[i-15]<=threshold & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=33#
cluster33<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (34):length(set))
    {
        if(set[i-33]>threshold
            & set[i-32]<=threshold
            & set[i-31]<=threshold
            & set[i-30]<=threshold
            & set[i-29]<=threshold & set[i-28]<=threshold
            & set[i-27]<=threshold & set[i-26]<=threshold
            & set[i-25]<=threshold & set[i-24]<=threshold
            & set[i-23]<=threshold & set[i-22]<=threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold & set[i-18]<=threshold
            & set[i-17]<=threshold & set[i-16]<=threshold
            & set[i-15]<=threshold & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=34#
cluster34<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (35):length(set))
    {
        if(set[i-34]>threshold
            & set[i-33]<=threshold
            & set[i-32]<=threshold
            & set[i-31]<=threshold
            & set[i-30]<=threshold
            & set[i-29]<=threshold & set[i-28]<=threshold
            & set[i-27]<=threshold & set[i-26]<=threshold
            & set[i-25]<=threshold & set[i-24]<=threshold
            & set[i-23]<=threshold & set[i-22]<=threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold & set[i-18]<=threshold
            & set[i-17]<=threshold & set[i-16]<=threshold
            & set[i-15]<=threshold & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=35#
cluster35<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (36):length(set))
    {
        if(set[i-35]>threshold
            & set[i-34]<=threshold
            & set[i-33]<=threshold
            & set[i-32]<=threshold
            & set[i-31]<=threshold
            & set[i-30]<=threshold
            & set[i-29]<=threshold & set[i-28]<=threshold
            & set[i-27]<=threshold & set[i-26]<=threshold
            & set[i-25]<=threshold & set[i-24]<=threshold
            & set[i-23]<=threshold & set[i-22]<=threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold & set[i-18]<=threshold
            & set[i-17]<=threshold & set[i-16]<=threshold
            & set[i-15]<=threshold & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=36#
cluster36a<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (37):length(set))
    {
        if(set[i-36]>threshold
            & set[i-35]<=threshold
            & set[i-34]<=threshold
            & set[i-33]<=threshold
            & set[i-32]<=threshold
            & set[i-31]<=threshold
            & set[i-30]<=threshold
            & set[i-29]<=threshold & set[i-28]<=threshold
            & set[i-27]<=threshold & set[i-26]<=threshold
            & set[i-25]<=threshold & set[i-24]<=threshold
            & set[i-23]<=threshold & set[i-22]<=threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold & set[i-18]<=threshold
            & set[i-17]<=threshold & set[i-16]<=threshold
            & set[i-15]<=threshold & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=36#
cluster36<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (37):length(set))
    {
        if(set[i-36]>threshold
            & set[i-35]<=threshold
            & set[i-34]<=threshold
            & set[i-33]<=threshold
            & set[i-32]<=threshold
            & set[i-31]<=threshold
            & set[i-30]<=threshold
            & set[i-29]<=threshold & set[i-28]<=threshold
            & set[i-27]<=threshold & set[i-26]<=threshold
            & set[i-25]<=threshold & set[i-24]<=threshold
            & set[i-23]<=threshold & set[i-22]<=threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold & set[i-18]<=threshold
            & set[i-17]<=threshold & set[i-16]<=threshold
            & set[i-15]<=threshold & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=37#
cluster37<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (38):length(set))
    {
        if(set[i-37]>threshold
            & set[i-36]<=threshold
            & set[i-35]<=threshold
            & set[i-34]<=threshold
            & set[i-33]<=threshold
            & set[i-32]<=threshold
            & set[i-31]<=threshold
            & set[i-30]<=threshold
            & set[i-29]<=threshold & set[i-28]<=threshold
            & set[i-27]<=threshold & set[i-26]<=threshold
            & set[i-25]<=threshold & set[i-24]<=threshold
            & set[i-23]<=threshold & set[i-22]<=threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold & set[i-18]<=threshold
            & set[i-17]<=threshold & set[i-16]<=threshold
            & set[i-15]<=threshold & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=38#
cluster38<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (39):length(set))
    {
        if(set[i-38]>threshold
            & set[i-37]<=threshold
            & set[i-36]<=threshold
            & set[i-35]<=threshold
            & set[i-34]<=threshold
            & set[i-33]<=threshold
            & set[i-32]<=threshold
            & set[i-31]<=threshold
            & set[i-30]<=threshold
            & set[i-29]<=threshold & set[i-28]<=threshold
            & set[i-27]<=threshold & set[i-26]<=threshold
            & set[i-25]<=threshold & set[i-24]<=threshold
            & set[i-23]<=threshold & set[i-22]<=threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold & set[i-18]<=threshold
            & set[i-17]<=threshold & set[i-16]<=threshold
            & set[i-15]<=threshold & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=39#
cluster39<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (40):length(set))
    {
        if(set[i-39]>threshold
            & set[i-38]<=threshold
            & set[i-37]<=threshold
            & set[i-36]<=threshold
            & set[i-35]<=threshold
            & set[i-34]<=threshold
            & set[i-33]<=threshold
            & set[i-32]<=threshold
            & set[i-31]<=threshold
            & set[i-30]<=threshold
            & set[i-29]<=threshold & set[i-28]<=threshold
            & set[i-27]<=threshold & set[i-26]<=threshold
            & set[i-25]<=threshold & set[i-24]<=threshold
            & set[i-23]<=threshold & set[i-22]<=threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold & set[i-18]<=threshold
            & set[i-17]<=threshold & set[i-16]<=threshold
            & set[i-15]<=threshold & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=40#
cluster40<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (41):length(set))
    {
        if(set[i-40]>threshold
            & set[i-39]<=threshold
            & set[i-38]<=threshold
            & set[i-37]<=threshold
            & set[i-36]<=threshold
            & set[i-35]<=threshold
            & set[i-34]<=threshold
            & set[i-33]<=threshold
            & set[i-32]<=threshold
            & set[i-31]<=threshold
            & set[i-30]<=threshold
            & set[i-29]<=threshold & set[i-28]<=threshold
            & set[i-27]<=threshold & set[i-26]<=threshold
            & set[i-25]<=threshold & set[i-24]<=threshold
            & set[i-23]<=threshold & set[i-22]<=threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold & set[i-18]<=threshold
            & set[i-17]<=threshold & set[i-16]<=threshold
            & set[i-15]<=threshold & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=41#
cluster41<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (42):length(set))
    {
        if(set[i-41]>threshold
            & set[i-40]<=threshold
            & set[i-39]<=threshold
            & set[i-38]<=threshold
            & set[i-37]<=threshold
            & set[i-36]<=threshold
            & set[i-35]<=threshold
            & set[i-34]<=threshold
            & set[i-33]<=threshold
            & set[i-32]<=threshold
            & set[i-31]<=threshold
            & set[i-30]<=threshold
            & set[i-29]<=threshold & set[i-28]<=threshold
            & set[i-27]<=threshold & set[i-26]<=threshold
            & set[i-25]<=threshold & set[i-24]<=threshold
            & set[i-23]<=threshold & set[i-22]<=threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold & set[i-18]<=threshold
            & set[i-17]<=threshold & set[i-16]<=threshold
            & set[i-15]<=threshold & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=42#
cluster42<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (43):length(set))
    {
        if(set[i-42]>threshold
            & set[i-41]<=threshold
            & set[i-40]<=threshold
            & set[i-39]<=threshold
            & set[i-38]<=threshold
            & set[i-37]<=threshold
            & set[i-36]<=threshold
            & set[i-35]<=threshold
            & set[i-34]<=threshold
            & set[i-33]<=threshold
            & set[i-32]<=threshold
            & set[i-31]<=threshold
            & set[i-30]<=threshold
            & set[i-29]<=threshold & set[i-28]<=threshold
            & set[i-27]<=threshold & set[i-26]<=threshold
            & set[i-25]<=threshold & set[i-24]<=threshold
            & set[i-23]<=threshold & set[i-22]<=threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold & set[i-18]<=threshold
            & set[i-17]<=threshold & set[i-16]<=threshold
            & set[i-15]<=threshold & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=43#
cluster43<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (44):length(set))
    {
        if(set[i-43]>threshold
            & set[i-42]<=threshold
            & set[i-41]<=threshold
            & set[i-40]<=threshold
            & set[i-39]<=threshold
            & set[i-38]<=threshold
            & set[i-37]<=threshold
            & set[i-36]<=threshold
            & set[i-35]<=threshold
            & set[i-34]<=threshold
            & set[i-33]<=threshold
            & set[i-32]<=threshold
            & set[i-31]<=threshold
            & set[i-30]<=threshold
            & set[i-29]<=threshold & set[i-28]<=threshold
            & set[i-27]<=threshold & set[i-26]<=threshold
            & set[i-25]<=threshold & set[i-24]<=threshold
            & set[i-23]<=threshold & set[i-22]<=threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold & set[i-18]<=threshold
            & set[i-17]<=threshold & set[i-16]<=threshold
            & set[i-15]<=threshold & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=44#
cluster44<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (45):length(set))
    {
        if(set[i-44]>threshold
            & set[i-43]<=threshold
            & set[i-42]<=threshold
            & set[i-41]<=threshold
            & set[i-40]<=threshold
            & set[i-39]<=threshold
            & set[i-38]<=threshold
            & set[i-37]<=threshold
            & set[i-36]<=threshold
            & set[i-35]<=threshold
            & set[i-34]<=threshold
            & set[i-33]<=threshold
            & set[i-32]<=threshold
            & set[i-31]<=threshold
            & set[i-30]<=threshold
            & set[i-29]<=threshold & set[i-28]<=threshold
            & set[i-27]<=threshold & set[i-26]<=threshold
            & set[i-25]<=threshold & set[i-24]<=threshold
            & set[i-23]<=threshold & set[i-22]<=threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold & set[i-18]<=threshold
            & set[i-17]<=threshold & set[i-16]<=threshold
            & set[i-15]<=threshold & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=45#
cluster45<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (46):length(set))
    {
        if(set[i-45]>threshold
            & set[i-44]<=threshold
            & set[i-43]<=threshold
            & set[i-42]<=threshold
            & set[i-41]<=threshold
            & set[i-40]<=threshold
            & set[i-39]<=threshold
            & set[i-38]<=threshold
            & set[i-37]<=threshold
            & set[i-36]<=threshold
            & set[i-35]<=threshold
            & set[i-34]<=threshold
            & set[i-33]<=threshold
            & set[i-32]<=threshold
            & set[i-31]<=threshold
            & set[i-30]<=threshold
            & set[i-29]<=threshold & set[i-28]<=threshold
            & set[i-27]<=threshold & set[i-26]<=threshold
            & set[i-25]<=threshold & set[i-24]<=threshold
            & set[i-23]<=threshold & set[i-22]<=threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold & set[i-18]<=threshold
            & set[i-17]<=threshold & set[i-16]<=threshold
            & set[i-15]<=threshold & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=46#
cluster46<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (47):length(set))
    {
        if(set[i-46]>threshold
            & set[i-45]<=threshold
            & set[i-44]<=threshold
            & set[i-43]<=threshold
            & set[i-42]<=threshold
            & set[i-41]<=threshold
            & set[i-40]<=threshold
            & set[i-39]<=threshold
            & set[i-38]<=threshold
            & set[i-37]<=threshold
            & set[i-36]<=threshold
            & set[i-35]<=threshold
            & set[i-34]<=threshold
            & set[i-33]<=threshold
            & set[i-32]<=threshold
            & set[i-31]<=threshold
            & set[i-30]<=threshold
            & set[i-29]<=threshold & set[i-28]<=threshold
            & set[i-27]<=threshold & set[i-26]<=threshold
            & set[i-25]<=threshold & set[i-24]<=threshold
            & set[i-23]<=threshold & set[i-22]<=threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold & set[i-18]<=threshold
            & set[i-17]<=threshold & set[i-16]<=threshold
            & set[i-15]<=threshold & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=47#
cluster47<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (48):length(set))
    {
        if(set[i-47]>threshold
            & set[i-46]<=threshold
            & set[i-45]<=threshold
            & set[i-44]<=threshold
            & set[i-43]<=threshold
            & set[i-42]<=threshold
            & set[i-41]<=threshold
            & set[i-40]<=threshold
            & set[i-39]<=threshold
            & set[i-38]<=threshold
            & set[i-37]<=threshold
            & set[i-36]<=threshold
            & set[i-35]<=threshold
            & set[i-34]<=threshold
            & set[i-33]<=threshold
            & set[i-32]<=threshold
            & set[i-31]<=threshold
            & set[i-30]<=threshold
            & set[i-29]<=threshold & set[i-28]<=threshold
            & set[i-27]<=threshold & set[i-26]<=threshold
            & set[i-25]<=threshold & set[i-24]<=threshold
            & set[i-23]<=threshold & set[i-22]<=threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold & set[i-18]<=threshold
            & set[i-17]<=threshold & set[i-16]<=threshold
            & set[i-15]<=threshold & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=48#
cluster48<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (49):length(set))
    {
        if(set[i-48]>threshold
            & set[i-47]<=threshold
            & set[i-46]<=threshold
            & set[i-45]<=threshold
            & set[i-44]<=threshold
            & set[i-43]<=threshold
            & set[i-42]<=threshold
            & set[i-41]<=threshold
            & set[i-40]<=threshold
            & set[i-39]<=threshold
            & set[i-38]<=threshold
            & set[i-37]<=threshold
            & set[i-36]<=threshold
            & set[i-35]<=threshold
            & set[i-34]<=threshold
            & set[i-33]<=threshold
            & set[i-32]<=threshold
            & set[i-31]<=threshold
            & set[i-30]<=threshold
            & set[i-29]<=threshold & set[i-28]<=threshold
            & set[i-27]<=threshold & set[i-26]<=threshold
            & set[i-25]<=threshold & set[i-24]<=threshold
            & set[i-23]<=threshold & set[i-22]<=threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold & set[i-18]<=threshold
            & set[i-17]<=threshold & set[i-16]<=threshold
            & set[i-15]<=threshold & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=49#
cluster49<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (50):length(set))
    {
        if(set[i-49]>threshold
            & set[i-48]<=threshold
            & set[i-47]<=threshold
            & set[i-46]<=threshold
            & set[i-45]<=threshold
            & set[i-44]<=threshold
            & set[i-43]<=threshold
            & set[i-42]<=threshold
            & set[i-41]<=threshold
            & set[i-40]<=threshold
            & set[i-39]<=threshold
            & set[i-38]<=threshold
            & set[i-37]<=threshold
            & set[i-36]<=threshold
            & set[i-35]<=threshold
            & set[i-34]<=threshold
            & set[i-33]<=threshold
            & set[i-32]<=threshold
            & set[i-31]<=threshold
            & set[i-30]<=threshold
            & set[i-29]<=threshold & set[i-28]<=threshold
            & set[i-27]<=threshold & set[i-26]<=threshold
            & set[i-25]<=threshold & set[i-24]<=threshold
            & set[i-23]<=threshold & set[i-22]<=threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold & set[i-18]<=threshold
            & set[i-17]<=threshold & set[i-16]<=threshold
            & set[i-15]<=threshold & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=50#
cluster50<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (51):length(set))
    {
        if(set[i-50]>threshold
            & set[i-49]<=threshold
            & set[i-48]<=threshold
            & set[i-47]<=threshold
            & set[i-46]<=threshold
            & set[i-45]<=threshold
            & set[i-44]<=threshold
            & set[i-43]<=threshold
            & set[i-42]<=threshold
            & set[i-41]<=threshold
            & set[i-40]<=threshold
            & set[i-39]<=threshold
            & set[i-38]<=threshold
            & set[i-37]<=threshold
            & set[i-36]<=threshold
            & set[i-35]<=threshold
            & set[i-34]<=threshold
            & set[i-33]<=threshold
            & set[i-32]<=threshold
            & set[i-31]<=threshold
            & set[i-30]<=threshold
            & set[i-29]<=threshold & set[i-28]<=threshold
            & set[i-27]<=threshold & set[i-26]<=threshold
            & set[i-25]<=threshold & set[i-24]<=threshold
            & set[i-23]<=threshold & set[i-22]<=threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold & set[i-18]<=threshold
            & set[i-17]<=threshold & set[i-16]<=threshold
            & set[i-15]<=threshold & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=20#
cluster20<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (21):length(set))
    {
        if(set[i-20]>threshold
            & set[i-19]<=threshold & set[i-18]<=threshold
            & set[i-17]<=threshold & set[i-16]<=threshold
            & set[i-15]<=threshold & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold & set[i]<=threshold)
        {
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=60#
cluster60<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (61):length(set))
    {
        if(set[i-60]>threshold
            & set[i-59]<=threshold & set[i-58]<=threshold
            & set[i-57]<=threshold & set[i-56]<=threshold
            & set[i-55]<=threshold & set[i-54]<=threshold
            & set[i-53]<=threshold & set[i-52]<=threshold
            & set[i-51]<=threshold & set[i-50]<=threshold
            & set[i-49]<=threshold & set[i-48]<=threshold
            & set[i-47]<=threshold & set[i-46]<=threshold
            & set[i-45]<=threshold & set[i-44]<=threshold
            & set[i-43]<=threshold & set[i-42]<=threshold
            & set[i-41]<=threshold & set[i-40]<=threshold
            & set[i-39]<=threshold & set[i-38]<=threshold
            & set[i-37]<=threshold & set[i-36]<=threshold
            & set[i-35]<=threshold & set[i-34]<=threshold
            & set[i-33]<=threshold & set[i-32]<=threshold
            & set[i-31]<=threshold
            & set[i-30]<=threshold
            & set[i-29]<=threshold & set[i-28]<=threshold
            & set[i-27]<=threshold & set[i-26]<=threshold
            & set[i-25]<=threshold & set[i-24]<=threshold
            & set[i-23]<=threshold & set[i-22]<=threshold
            & set[i-21]<=threshold & set[i-20]<=threshold
            & set[i-19]<=threshold & set[i-18]<=threshold
            & set[i-17]<=threshold & set[i-16]<=threshold
            & set[i-15]<=threshold & set[i-14]<=threshold
            & set[i-13]<=threshold & set[i-12]<=threshold
            & set[i-11]<=threshold & set[i-10]<=threshold
            & set[i-9]<=threshold & set[i-8]<=threshold
            & set[i-7]<=threshold & set[i-6]<=threshold
            & set[i-5]<=threshold & set[i-4]<=threshold
            & set[i-3]<=threshold & set[i-2]<=threshold
            & set[i-1]<=threshold
            & set[i]<=threshold)
{
            x<-max(set[j:i])
            ifelse(i !=length(set), j<-i+1, NA)
            z<-as.numeric(c(z,x))
        }}}
    z
}

#separation interval s=100#
cluster100<-function(set,threshold)
{
    x<-list()
    z<-list()
    j<-1
{
    for(i in (101):length(set))
    {if(set[i-100]>threshold
        & set[i-99]<=threshold & set[i-98]<=threshold
        & set[i-97]<=threshold & set[i-96]<=threshold
        & set[i-95]<=threshold & set[i-94]<=threshold
        & set[i-93]<=threshold & set[i-92]<=threshold
        & set[i-91]<=threshold & set[i-90]<=threshold
        & set[i-89]<=threshold & set[i-88]<=threshold
        & set[i-87]<=threshold & set[i-86]<=threshold
        & set[i-85]<=threshold & set[i-84]<=threshold
        & set[i-83]<=threshold & set[i-82]<=threshold
        & set[i-81]<=threshold & set[i-80]<=threshold
        & set[i-79]<=threshold & set[i-78]<=threshold
        & set[i-77]<=threshold & set[i-76]<=threshold
        & set[i-75]<=threshold & set[i-74]<=threshold
        & set[i-73]<=threshold & set[i-72]<=threshold
        & set[i-71]<=threshold
        & set[i-70]<=threshold
        & set[i-69]<=threshold & set[i-68]<=threshold
        & set[i-67]<=threshold & set[i-66]<=threshold
        & set[i-65]<=threshold & set[i-64]<=threshold
        & set[i-63]<=threshold & set[i-62]<=threshold
        & set[i-61]<=threshold & set[i-60]<=threshold
        & set[i-59]<=threshold & set[i-58]<=threshold
        & set[i-57]<=threshold & set[i-56]<=threshold
        & set[i-55]<=threshold & set[i-54]<=threshold
        & set[i-53]<=threshold & set[i-52]<=threshold
        & set[i-51]<=threshold & set[i-50]<=threshold
        & set[i-49]<=threshold & set[i-48]<=threshold
        & set[i-47]<=threshold & set[i-46]<=threshold
        & set[i-45]<=threshold & set[i-44]<=threshold
        & set[i-43]<=threshold & set[i-42]<=threshold
        & set[i-41]<=threshold & set[i-40]<=threshold
        & set[i-39]<=threshold & set[i-38]<=threshold
        & set[i-37]<=threshold & set[i-36]<=threshold
        & set[i-35]<=threshold & set[i-34]<=threshold
        & set[i-33]<=threshold & set[i-32]<=threshold
        & set[i-31]<=threshold
        & set[i-30]<=threshold
        & set[i-29]<=threshold & set[i-28]<=threshold
        & set[i-27]<=threshold & set[i-26]<=threshold
        & set[i-25]<=threshold & set[i-24]<=threshold
        & set[i-23]<=threshold & set[i-22]<=threshold
        & set[i-21]<=threshold & set[i-20]<=threshold
        & set[i-19]<=threshold & set[i-18]<=threshold
        & set[i-17]<=threshold & set[i-16]<=threshold
        & set[i-15]<=threshold & set[i-14]<=threshold
        & set[i-13]<=threshold & set[i-12]<=threshold
        & set[i-11]<=threshold & set[i-10]<=threshold
        & set[i-9]<=threshold & set[i-8]<=threshold
        & set[i-7]<=threshold & set[i-6]<=threshold
        & set[i-5]<=threshold & set[i-4]<=threshold
        & set[i-3]<=threshold & set[i-2]<=threshold
        & set[i-1]<=threshold
        & set[i]<=threshold)
{
        x<-max(set[j:i])
        ifelse(i !=length(set), j<-i+1, NA)
        z<-as.numeric(c(z,x))      
    }}}
    z
}

#GEV RETURN LEVEL CALCULATION
ret.level.gev<-function(mu,sigma,xi,period)
{
    ret.level<-mu-(sigma/xi)*(1-(-log(1-(1/period)))**(-xi))
    ret.level
}



#GPD RETURN LEVEL CALCULATION
ret.level.gpd<-function(sigma,xi,lambda,thresh,period)
{
    ret.level<-thresh+(sigma/xi)*(((period*lambda)^(xi))-1)
    ret.level
}

#GEV STANDARD ERRORS FOR Q
gev.ret<-function(data, period)
{
    lee<-gev.fit(data)
    
    V<-matrix(ncol=3,nrow=3)
    V[1,1]<-lee$cov[1,1]
    V[2,2]<-lee$cov[2,2]
    V[3,3]<-lee$cov[3,3]
    V[1,2]<-lee$cov[1,2]
    V[1,3]<-lee$cov[1,3]
    V[2,1]<-lee$cov[2,1]
    V[2,3]<-lee$cov[2,3]
    V[3,1]<-lee$cov[3,1]
    V[3,2]<-lee$cov[3,2]
    
    yp<--log(1-(1/period))
    
    diff1<-1
    diff2<--((lee$mle[3])**(-1))*(1-(yp**(-lee$mle[3])))
    diff3<-((lee$mle[2])*((lee$mle[3])**(-2))*(1-((yp)**(-lee$mle[3]))))-((lee$mle[2])*((lee$mle[3])**(-1))*((yp)**(-(lee$mle[3])))*log(yp))
    
    del.t<-matrix(ncol=3,nrow=1)
    del.t[1,1]<-diff1
    del.t[1,2]<-diff2
    del.t[1,3]<-diff3
    
    del<-matrix(ncol=1,nrow=3)
    del[1,1]<-diff1
    del[2,1]<-diff2
    del[3,1]<-diff3
    
    A<-matrix(ncol=3,nrow=1)
    A[1,1]<-del.t[1,1]*V[1,1]
    A[1,2]<-(del.t[1,2]*V[2,2])+(del.t[1,3]*V[3,2])
    A[1,3]<-(del.t[1,2]*V[2,3])+(del.t[1,3]*V[3,3])
    
    ret.var<-(A[1,1]*diff1) + (A[1,2]*diff2) + (A[1,3]*diff3)
    ret.se<-sqrt(ret.var)
    ret.level<-(lee$mle[1])-((lee$mle[2])/(lee$mle[3]))*(1-(-log(1-(1/period)))**(-(lee$mle[3])))
    
    cat(" ret.level= ",ret.level,"\n","ret.se= ",ret.se)
}

#GPD STANDARD ERRORS FOR Q)
gpd.ret<-function(data,threshold,period)
{
    lee<-gpd.fit(data,threshold)
    lambda<-length(data[data>threshold])/length(data)
    
    V<-matrix(ncol=3,nrow=3)
    V[1,1]<-lambda*(1-lambda)/length(data)
    V[1,2]<-0
    V[1,3]<-0
    V[2,1]<-0
    V[3,1]<-0
    V[2,2]<-(lee$cov[1,1])
    V[2,3]<-lee$cov[1,2]
    V[3,2]<-lee$cov[2,1]
    V[3,3]<-(lee$cov[2,2])
    
    diff1<-(lee$mle[1])*period^((lee$mle[2]))*lambda^((lee$mle[2])-1)
    diff2<-((lee$mle[2])^(-1))*(((period*lambda)^((lee$mle[2])))-1)
    diff3<--((lee$mle[1]))*(lee$mle[2])^(-2)*(((period*lambda)^((lee$mle[2])))-1) + (lee$mle[1])*((lee$mle[2])^(-1))*((period*lambda)^((lee$mle[2])))*log(period*lambda)
    
    
    del.t<-matrix(ncol=3,nrow=1)
    del.t[1,1]<-diff1
    del.t[1,2]<-diff2
    del.t[1,3]<-diff3
    
    del<-matrix(ncol=1,nrow=3)
    del[1,1]<-diff1
    del[2,1]<-diff2
    del[3,1]<-diff3
    
    A<-matrix(ncol=3,nrow=1)
    A[1,1]<-del.t[1,1]*V[1,1]
    A[1,2]<-(del.t[1,2]*V[2,2])+(del.t[1,3]*V[3,2])
    A[1,3]<-(del.t[1,2]*V[2,3])+(del.t[1,3]*V[3,3])
    
    ret.var<-(A[1,1]*diff1) + (A[1,2]*diff2) + (A[1,3]*diff3)
    ret.se<-sqrt(ret.var)
    ret.level<-threshold+((lee$mle[1])/(lee$mle[2]))*(((period*lambda)^((lee$mle[2])))-1)
    cat(" ret.level= ",ret.level,"\n","ret.se= ",ret.se)
}

#GEV - BAYES
gev.bayes<-function(n,dataset,mustart,sigmastart,xistart,errmu,errlogsigma,errxi,sdmu,sdlogsigma,sdxi)
{
    k<-length(dataset)
    
    mu<-mustart
    sigma<-sigmastart
    xi<-xistart
    eta<-log(sigma)
    
    canmu<-vector("numeric")
    caneta<-vector("numeric")
    canxi<-vector("numeric")
    
    canmu[1]<-mu
    caneta[1]<-eta
    canxi[1]<-xi
    
    w<-vector("numeric")
    x<-vector("numeric")
    y<-vector("numeric")
    
    aprobmu<-vector("numeric")
    aprobeta<-vector("numeric")
    aprobxi<-vector("numeric")
    
    w[1]<-canmu[1]
    x[1]<-caneta[1]
    y[1]<-canxi[1]
    
    loglik<-function(k,dataset,MU,ETA,XI)
    {
        m<-min((1+(XI*(dataset-MU)/exp(ETA))))
        if(m<0.00001)return(as.double(-1000000))
        if(exp(ETA)<0.00001)return(as.double(-1000000))
        loglik<--length(dataset)*ETA-(1/XI+1)*sum(log(1+(XI*(dataset-MU)/exp(ETA))))-sum((1+(XI*(dataset-MU)/exp(ETA)))**(-1/XI))
        loglik
    }
    
    for(i in 2:n){
        print(i)
        canmu[i]<-w[i-1]+rnorm(1,0,errmu)
        likely<-exp((loglik(k,dataset,canmu[i],x[i-1],y[i-1]))-(loglik(k,dataset,w[i-1],x[i-1],y[i-1])))
        aprobmu[i]<-min(1,(likely*((dnorm(canmu[i],0,sdmu))*(dnorm(x[i-1],0,sdlogsigma))*(dnorm(y[i-1],0,sdxi)))/((dnorm(w[i-1],0,sdmu))*(dnorm(x[i-1],0,sdlogsigma))*(dnorm(y[i-1],0,sdxi)))))
        u<-runif(1)
        if(u<aprobmu[i]){w[i]<-canmu[i]}
        if(u>=aprobmu[i]){w[i]<-w[i-1]}
        
        caneta[i]<-x[i-1]+rnorm(1,0,errlogsigma)
        likely2<-exp((loglik(k,dataset,w[i],caneta[i],y[i-1]))-(loglik(k,dataset,w[i],x[i-1],y[i-1])))
        aprobeta[i]<-min(1,(likely2*((dnorm(w[i],0,sdmu))*(dnorm(caneta[i],0,sdlogsigma))*(dnorm(y[i-1],0,sdxi)))/((dnorm(w[i],0,sdmu))*(dnorm(x[i-1],0,sdlogsigma))*(dnorm(y[i-1],0,sdxi)))))
        u<-runif(1)
        if(u<aprobeta[i]){x[i]<-caneta[i]}
        if(u>=aprobeta[i]){x[i]<-x[i-1]}
        
        
        canxi[i]<-y[i-1]+rnorm(1,0,errxi)
        likely3<-exp((loglik(k,dataset,w[i],x[i],canxi[i]))-(loglik(k,dataset,w[i],x[i],y[i-1])))
        aprobxi[i]<-min(1,(likely3*((dnorm(w[i],0,sdmu))*(dnorm(x[i],0,sdlogsigma))*(dnorm(canxi[i],0,sdxi)))/((dnorm(w[i],0,sdmu))*(dnorm(x[i],0,sdlogsigma))*(dnorm(y[i-1],0,sdxi)))))
        if(u<aprobxi[i]){y[i]<-canxi[i]}
        if(u>=aprobxi[i]){y[i]<-y[i-1]}
    }
    
    aprobmu<-aprobmu[!is.na(aprobmu)]
    aprobeta<-aprobeta[!is.na(aprobeta)]
    aprobxi<-aprobxi[!is.na(aprobxi)]
    mu<-w
    logsigma<-x
    xi<-y
    aproblogsigma<-aprobeta
    return(mu,logsigma,xi,aprobmu,aproblogsigma,aprobxi)
}


#GPD - BAYES
bayes5<-function(n,dataset,sigmastart,xistart,erreta,errxi,sdeta,sdxi)
{
    k<-length(dataset)
    sigma<-sigmastart
    xi<-xistart
    eta<-log(sigma)
    caneta<-vector("numeric")
    canxi<-vector("numeric")
    caneta[1]<-eta
    canxi[1]<-xi
    x<-vector("numeric")
    y<-vector("numeric")
    aprobeta<-vector("numeric")
    aprobxi<-vector("numeric")
    x[1]<-caneta[1]
    y[1]<-canxi[1]
    loglik<-function(k,dataset,ETA,XI)
    {
        m<-min(1+(dataset*(XI/exp(ETA))))
        if(m<0.00001)return(as.double(-1000000))
        if(exp(ETA)<0.00001)return(as.double(-1000000))
        loglik<--k*ETA-(1+(1/XI))*sum(log(1+((XI*dataset)/exp(ETA))))
        loglik
    }
    for(i in 2:n){
        caneta[i]<-x[i-1]+rnorm(1,0,erreta)
        likely<-exp((loglik(k,dataset,caneta[i],y[i-1]))-(loglik(k,dataset,x[i-1],y[i-1])))
        aprobeta[i]<-min(1,(likely*((dnorm(caneta[i],0,sdeta))*(dnorm(y[i-1],0,sdxi)))/((dnorm(x[i-1],0,sdeta))*(dnorm(y[i-1],0,sdxi)))))
        u<-runif(1)
        if(u<aprobeta[i]){x[i]<-caneta[i]}
        if(u>=aprobeta[i]){x[i]<-x[i-1]}
        canxi[i]<-y[i-1]+rnorm(1,0,errxi)
        likely2<-exp((loglik(k,dataset,x[i],canxi[i]))-(loglik(k,dataset,x[i],y[i-1])))
        aprobxi[i]<-min(1,(likely2*((dnorm(x[i],0,sdeta))*(dnorm(canxi[i],0,sdxi)))/((dnorm(x[i],0,sdeta))*(dnorm(y[i-1],0,sdxi)))))
        if(u<aprobxi[i]){y[i]<-canxi[i]}
        if(u>=aprobxi[i]){y[i]<-y[i-1]}
    }
    aprobeta<-aprobeta[!is.na(aprobeta)]
    aprobxi<-aprobxi[!is.na(aprobxi)]
    return(x,y,aprobeta,aprobxi)
}
