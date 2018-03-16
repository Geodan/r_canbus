# st_distance vergelijken met snelheid berekend uit snelheid en tijd

require("RPostgreSQL")
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, 
                 dbname = "research",
                 #dbname = "localhost"
                 host = "metis", 
                 port = 5433,
                 user = "postgres")

## query 1, unieke autos
autos <- dbGetQuery(con, 
                    "
                    SELECT DISTINCT vin as auto 
                    FROM canbus.data_2017
                    WHERE time >= '2017-04-01' AND signalid = 191
                    "
)

## query 2, afstand tussen punten
a <- dbGetQuery(con, 
                
                
                "
                -- tijdsverschil en gemiddelde snelheid tussen twee opeenvolgende punten
                WITH distance AS (
                SELECT 	vin as auto, 
                value as speed, 
                time, 
                ST_Transform(geom, 28992) as geom, 
                --DATE_PART('second', time - LAG(time, 1) OVER(ORDER BY vin, time)) dt,
                EXTRACT(EPOCH FROM (time - LAG(time, 1) OVER(ORDER BY vin, time))) dt,
                (value + LAG(value, 1) OVER(ORDER BY vin, time)) / 2 avspeed
                FROM canbus.data_2017
                WHERE signalid = 191 AND vin = '71481e11a9d0804b1121dbd7b1d102ad84ff45e39427ada094c31a1adbaa4969' 
                ), 
                
                --SELECT * from distance
                -- som van afgelegde afstanden (v*dt) per gps-punt
                distance2 AS (
                SELECT min(time) as time, geom, (SUM(d.dt * d.avspeed/3.6))*1 as x -- km/u naar m/s
                FROM 
                distance d
                GROUP BY geom
                ORDER by min(time)
                ), 
                --SELECT * from distance2;
                
                distance3 as ( 
                SELECT 	b.time, b.x as calc_dist, 
                ST_DISTANCE(b.geom, (LAG(b.geom, 1) OVER (ORDER BY b.time))) as geom_dist
                FROM distance2 b ORDER BY b.time
                )
                SELECT *, geom_dist - calc_dist as diff from distance3;
                ")

dbDisconnect(con)

a$time = as.POSIXct(a$time)
datum = as.Date(a$time[1])

# plot verloop van afstand tussen gps-meetpunten gedurende rit
#png(filename = './fig/distance_between_gps_points.png', width=15, height=10, units = 'cm', res=300)
par(mar=c(4,4,3,1))
plot(range(a$time), range(a$geom_dist, na.rm=T), type='n', xaxt='n',
     xlab = 'time',
     ylab = 'distance between points (m)',
     xlim=c(as.numeric(as.POSIXct(paste(datum,"12:45:00"))), as.numeric(as.POSIXct(paste(datum,"14:30:00")))),
     #ylim=c(0,4000),
     las=1,
     cex.lab=0.6, cex.axis=0.6)
r <- as.POSIXct(round(range(as.POSIXct(a$time)), "hours"))
axis.POSIXct(1, at = seq(r[1], r[2], by = "15 min"), format = "%H:%M", cex.axis=0.6)
legend(x='topleft', legend = c('geometric distance (m)', 'calculated from speed and time (m)'), col=c(col1,col2), lty=c(1,2), cex=.6, inset=c(0,-.2), xpd=NA)
grid()
lines(a$time, a$geom_dist, col=col1)
lines(a$time, a$calc_dist, col=col2, lty=2)

# voeg op x-ticks toe per kwartier 

#dev.off()


# scatterplot van afstand met ST_DISTANCE en berekende afstand met avgspeed * dt
#png(filename = 'D:/canbus/distance_compared.png', width=15, height=10, units = 'cm', res=180)
plot(a$calc_dist, a$geom_dist, xlim=c(0,1000), ylim=c(0,1000), 
     xlab='distance from speed and time (m)',
     ylab='distance between GPS positions (m)',
     las=1)
abline(coef=c(1,1))
#dev.off()

col1 = 'darkblue'
col2 = 'black'



