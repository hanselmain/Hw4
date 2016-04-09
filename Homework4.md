Facebook粉絲團分析（分析專頁名稱朱立倫）
================

分析朱立倫粉絲團資料，資料分析區間為2016/01/01至2016/04/09貼文分析

``` r
if (!require('Rfacebook')){
    install.packages("Rfacebook")
    library(Rfacebook)
}
```

讀取朱立倫粉絲團資料
--------------------

``` r
token<-'CAACEdEose0cBANn5pL5P2QDRqZCwheBhHUjCDmYBSlP5tJFmiOS4sRH9bN3eNnADNlBOfdk43Vt7PCebU7R1Jx8Q6JH8fHx0FtYFyEK91bhx9erxhGsMasyIRZCZAUZB6p28lE3tUIih8NWAMoZC2nYo9Hj6K60y1Vg3mZAUlVLsbm87mZAIZChhIy9zPmWRYYCQjZAe8dJlIjgZDZD'
totalPage<-NULL
lastDate<-Sys.Date()
DateVectorStr<-as.character(seq(as.Date("2016-01-01"),lastDate,by="5 days"))
for(i in 1:(length(DateVectorStr)-1)){
    tempPage<-getPage("llchu", token,
                      since = DateVectorStr[i],until = DateVectorStr[i+1])
    totalPage<-rbind(totalPage,tempPage)
}
```

    ## 14 posts 13 posts 25 posts 4 posts 5 posts 5 posts 7 posts 5 posts 1 posts 6 posts 5 posts 3 posts 4 posts 6 posts 4 posts 8 posts 5 posts 5 posts 4 posts

``` r
nrow(totalPage)
```

    ## [1] 129

2016/01/01至2016/04/09朱立倫粉絲團一共有129篇文章

每日發文數分析
--------------

分析朱立倫粉絲團每天的發文數

``` r
totalPage$datetime <- as.POSIXct(totalPage$created_time, 
                                 format = "%Y-%m-%dT%H:%M:%S+0000", 
                                 tz = "GMT") #2016-01-16T15:05:36+0000
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei") #2016-01-16
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
PostCount<-aggregate(id~dateTPE,totalPage,length)
library(knitr)
kable(head(PostCount[order(PostCount$id,decreasing = T),]))
```

|     | dateTPE    |   id|
|-----|:-----------|----:|
| 12  | 2016-01-12 |    7|
| 13  | 2016-01-13 |    5|
| 14  | 2016-01-14 |    5|
| 15  | 2016-01-15 |    5|
| 65  | 2016-03-20 |    4|
| 1   | 2016-01-01 |    3|

01/16 是總統大選，所以前幾天的發文數較高
03/20 新北市的活動較多"萬金石馬拉松"、"新北市美術家雙年展"、HBL籃球賽

每日likes數分析
---------------

分析朱立倫粉絲團每天的likes數

``` r
totalPage$datetime <- as.POSIXct(totalPage$created_time, 
                                 format = "%Y-%m-%dT%H:%M:%S+0000", 
                                 tz = "GMT") #2016-01-16T15:05:36+0000
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei") #2016-01-16
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
LikesCount<-aggregate(likes_count~dateTPE,totalPage,mean)
library(knitr)
kable(head(LikesCount[order(LikesCount$likes_count,decreasing = T),]))
```

|     | dateTPE    |  likes\_count|
|-----|:-----------|-------------:|
| 16  | 2016-01-16 |       83386.0|
| 34  | 2016-02-06 |       57639.0|
| 9   | 2016-01-09 |       52729.5|
| 15  | 2016-01-15 |       49404.8|
| 17  | 2016-01-18 |       46132.0|
| 36  | 2016-02-08 |       41877.0|

01/16 總統大選，落選的文章讚數有13萬，但同天有另一篇只有3萬讚的貼文，使得讚數被平均了
02/06 台南地震，有許多人為台南加油打氣，但也有不少造口業的

每日comments數分析
------------------

分析朱立倫粉絲團每天的comments數

``` r
totalPage$datetime <- as.POSIXct(totalPage$created_time, 
                                 format = "%Y-%m-%dT%H:%M:%S+0000", 
                                 tz = "GMT") #2016-01-16T15:05:36+0000
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei") #2016-01-16
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
CommentCount<-aggregate(comments_count~dateTPE,totalPage,mean)
library(knitr)
kable(head(CommentCount[order(CommentCount$comments_count,decreasing = T),]))
```

|     | dateTPE    |  comments\_count|
|-----|:-----------|----------------:|
| 16  | 2016-01-16 |          10605.5|
| 15  | 2016-01-15 |           7843.8|
| 17  | 2016-01-18 |           3629.0|
| 9   | 2016-01-09 |           1883.0|
| 18  | 2016-01-19 |           1649.0|
| 34  | 2016-02-06 |           1377.0|

01/16 由於總統大選的落選與對周子瑜事件的不滿，使得留言數很多 
01/15 的拉票貼文較多，使得留言數被平均了

每日shares數分析
----------------

分析朱立倫粉絲團每天的shares數

``` r
totalPage$datetime <- as.POSIXct(totalPage$created_time, 
                                 format = "%Y-%m-%dT%H:%M:%S+0000", 
                                 tz = "GMT") #2016-01-16T15:05:36+0000
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei") #2016-01-16
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
SharesCount<-aggregate(shares_count~dateTPE,totalPage,mean)
library(knitr)
kable(head(SharesCount[order(SharesCount$shares_count,decreasing = T),]))
```

|     | dateTPE    |  shares\_count|
|-----|:-----------|--------------:|
| 15  | 2016-01-15 |       2342.600|
| 1   | 2016-01-01 |       1521.000|
| 16  | 2016-01-16 |       1364.000|
| 34  | 2016-02-06 |       1265.000|
| 12  | 2016-01-12 |       1000.571|
| 9   | 2016-01-09 |        937.000|

01/15 因為周子瑜事件，許多不滿一直執政黨對這件事情處理態度的人留言謾罵，並且分享了這篇文章
比同天的"明天，我們需要你，投下這一票"之類的拉票貼文分享數要高出許多
