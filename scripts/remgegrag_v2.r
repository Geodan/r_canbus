# positie toevoegen
# naar dataframe

require("RPostgreSQL")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "research",
                 host = "metis", port = 5433,
                 user = "postgres")


braking <- dbGetQuery(con, statement = 
                        "WITH a as (
                      SELECT vin, time, value, signalid, geom
                      FROM canbus.data_2017 
                      WHERE time > '2017-09-01' 
                      --WHERE vin = '03662eafcd5b8852e3663484376c027b8b7ea8b30829cae92853ba97d5d63682' 
),
                      
                      -- remsignalen en geometrie naar RD
                      b AS (
                      SELECT vin, 
                      time,
                      value, 
                      ST_Transform(geom, 28992) as geom
                      FROM a
                      WHERE signalid = 25
                      ),
                      
                      -- snelheidsmetingen en geometrie
                      v AS (
                      SELECT vin, 
                      value as speed, 
                      time,
                      ST_Transform(geom, 28992) as geom
                      FROM a
                      WHERE signalid = 191
                      ),
                      
                      bk AS(
                      SELECT 
                      b.*, 
                      -- CASE WHEN b.value - LAG(b.value, -1) OVER(ORDER BY vin, time) = 1 AND b.vin = LAG(b.vin, -1) OVER(ORDER BY vin, time) THEN LAG(b.time, -1) OVER(ORDER BY vin, time)
                      CASE WHEN b.value - LAG(b.value, -1) OVER(PARTITION BY vin ORDER BY vin, time) = 1 THEN LAG(b.time, -1) OVER(PARTITION BY vin ORDER BY time)
                      ELSE NULL
                      END as time2,
                      --CASE WHEN b.value - LAG(b.value, -1) OVER(ORDER BY vin, time) = 1 AND b.vin = LAG(b.vin, -1) OVER(ORDER BY vin, time) THEN 'brakeon'
                      CASE WHEN b.value - LAG(b.value, -1) OVER(PARTITION BY vin ORDER BY time) = 1 THEN 'brakeon'
                      ELSE 'brakeoff'
                      END as aanuit
                      FROM b
                      ),
                      
                      b3 AS (
                      SELECT bk.vin as vin, bk.time as aan, bk.time2 as uit, bk.aanuit as signal, geom, null::integer as value --, 'brake'::text as signal
                      FROM bk
                      UNION 
                      SELECT vin, time as aan, null::timestamp as uit, 'speed'::text as signal, geom, speed as value 
                      FROM v
                      ORDER by aan
                      ),-- SELECT * FROM b3
                      
                      b4 AS (
                      SELECT vin, aan, uit, geom, value, signal, 
                      CASE WHEN signal = 'brakeon' AND uit > aan AND LAG(signal, -1) OVER (PARTITION BY vin ORDER BY aan) = 'speed' THEN LAG(value, -1) OVER(PARTITION BY vin ORDER BY aan)
                      WHEN signal = 'brakeoff' AND uit is NULL AND LAG(signal, 1) OVER (PARTITION BY vin ORDER BY aan) = 'speed' THEN LAG(value, 1) OVER(PARTITION BY vin ORDER BY aan)
                      ELSE NULL
                      END as v0,
                      CASE WHEN signal = 'brakeon' AND uit > aan AND LAG(signal, -1) OVER (PARTITION BY vin ORDER BY aan) = 'speed' THEN LAG(aan, -1) OVER(PARTITION BY vin ORDER BY aan) ELSE NULL END as t_v0,
                      CASE WHEN signal = 'brakeoff' AND uit is NULL AND LAG(signal, 1) OVER (PARTITION BY vin ORDER BY aan) = 'speed' THEN LAG(aan, 1) OVER(PARTITION BY vin ORDER BY aan) ELSE NULL END as t_v1,
                      EXTRACT(EPOCH FROM uit - aan) as dt1
                      FROM b3
                      ),--  SELECT * FROM b4
                      
                      /***brakeoff AS(
                      SELECT vin, time, geom from b where value = 0
                      ),
                      ***/
                      -- selecteer rem-uit momenten
                      c as (SELECT vin, aan, v0 as v1, t_v1 FROM b4 WHERE signal = 'brakeoff')
                      
                      -- voeg snelheid en tijdstip van rem-uit toe aan rem-aan o.b.v. timestamp
                      SELECT b4.vin, b4.aan as brakeon, b4.uit as brakeoff, b4.dt1, b4.v0, c.v1, b4.geom, b4.t_v0, c.t_v1,  EXTRACT(EPOCH FROM c.t_v1 - b4.t_v0) as dt2 FROM b4
                      JOIN c ON b4.uit = c.aan and b4.vin = c.vin;
                      ")
close(zz)


speed <- dbGetQuery(con, "
                    SELECT value as speed, vin, time, location from canbus.data_2017
                    WHERE signalid = 191 AND vin = '5a4894f5dbc7e84c0dda99c8c09c42a35a26a34f1449d93fc3bc54d2e373779a'
                    ")
dbDisconnect(con)


braking$cut = cut(braking$v0, breaks = seq(0,150,length.out = 11))

par(mfrow=c(5,2), mar=c(1,1,1,1), oma = c(4,4,0,0))
for (cut in sort(na.omit(unique(braking$cut)))){
  x = braking[braking$cut == cut, c('v0', 'v1', 'dt2', 'cut') ]
  plot(x$v0-x$v1, x$dt2, ylim=c(0,60), col=rgb(0,0,0,0.1), main=cut, xlim=c(0,150), xaxt='n', yaxt='n')
  axis(1, lab=T)
  axis(2, lab=T, las=1, at=c(0,20,40,60))
}
mtext(text = 'snelheidsafname (km/h)', side = 1, outer = T, line=2)
mtext(text = 'remtijd (s)', side = 2, outer = T, line=2)

