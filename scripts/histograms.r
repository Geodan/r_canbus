###
# Several analysis on canbus data. 
# e.g.: id's per day, aggregated measurements, heatmaps
# authors: TomT
###


require("RPostgreSQL")
require("ggplot2")
require("hexbin")
require("svglite")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "research",
                 host = "localhost", port = 5433,
                 user = "postgres")

#How many unique id's are active per day
df_postgres <- dbGetQuery(con, "
  SELECT count(DISTINCT vin) vehicles, date_trunc('day',time) date
  FROM canbus.data_2017 
  GROUP BY date_trunc('day',time)
  ORDER BY date_trunc('day',time)
")
#Conclusion: before 19-march id's are switched more then 1x a day
ggplot(df_postgres,aes(date,vehicles)) + geom_line(color="steelblue") + theme_bw()


#Histogram of number of measurements per signal
df_postgres <- dbGetQuery(con, "
  SELECT b.name, count(a.signalid) n
  FROM canbus.data_2017 a
  INNER JOIN canbus.signals b
  ON a.signalid = b.signalid
  WHERE time > '2017-03-19'
  GROUP BY b.name
  ORDER BY b.name
")
ggplot(df_postgres, aes(x=name, y=n)) +
  geom_bar(stat='identity',fill='steelblue') +
  coord_flip() + theme_minimal()

#histogram of number of speed measurements per geom
df_postgres <- dbGetQuery(con, "
  WITH grouped AS (
    SELECT geom,count(geom) n
    FROM canbus.data_2017 
    WHERE signalid = 191 
    AND value > 10
    AND time > '2017-03-19'
    GROUP BY geom
  )
  SELECT n, count(geom) freq
  FROM grouped
  GROUP BY n
  ORDER BY n
")
ggplot(data=df_postgres, aes(x=n, y=freq)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(title="Number of speedmeasurements per GPS point", 
       x="#-measurements", y = "occurance") +
  theme_minimal()

