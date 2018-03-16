## nog doen: zoek snelheid voor en na remmen bij het rem-event

require("RPostgreSQL")
library("RColorBrewer")
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "research",
                 host = "localhost", port = 5433,
                 user = "postgres")

# snelheid van gegeven auto
speed <- dbGetQuery(con, "
                    SELECT value as speed, vin as auto, time, location from canbus.data_2017
                    WHERE signalid = 191 AND vin = 'fe41054ca9a55158862afd2b06aafe74eb31900f64ba89e070adaa52555b261b'
                    ")

# remgedrag van gegeven auto
brake <- dbGetQuery(con, "
                    SELECT value as brake, vin as auto, time, location from canbus.data_2017
                    WHERE signalid = 25 AND vin = 'fe41054ca9a55158862afd2b06aafe74eb31900f64ba89e070adaa52555b261b'
                    ")
dbDisconnect(con)
s = speed

# rem aan
b = brake[brake$brake==1,]

# rem uit
l = brake[brake$brake==0,]

# zoek eerst waarneming 'brake = 0' volgend op 'brake = 1'
eerstvlgnd = apply(X = b, MARGIN = 1, FUN = function(x) {as.vector(
  c(as.numeric(as.POSIXct(x[3])),
    l$time[min(which(as.POSIXct(l$time) > as.POSIXct(x[3])))]))})

eerstvlgnd = data.frame(t(eerstvlgnd))
colnames(eerstvlgnd) = c('rem_aan', 'rem_uit')

# als een waarneming brake = 0 aan meerdere brake = 1 gekoppeld is, toekennen aan laatste
#abline(v=eerstvlgnd[duplicated(eerstvlgnd$X2),2])
rem = eerstvlgnd[!rev(duplicated(rev(eerstvlgnd$rem_uit))),]

v = apply(X = rem, MARGIN = 1, FUN = function(x) {as.vector(
  c(as.numeric(x[1]),
    as.numeric(x[2]),
    s$time[min(which(as.numeric(s$time) > x[1]))],
    s$speed[min(which(as.numeric(s$time) > x[1]))], 
    s$time[max(which(as.numeric(s$time) < x[2]))],
    s$speed[max(which(as.numeric(s$time) < x[2]))]))})

v = data.frame(t(v))
colnames(v)  = c('rem_aan', 'rem_uit', 't0', 'v0', 't1', 'v1')
v$dt = (v$t1 - v$t0)
v$dv = (v$v1 - v$v0)
v$a = v$dv / v$dt

# als een waarneming brake = 0 aan meerdere brake =1 gekoppeld is, toekennen aan laatste
#x = eerstvlgnd[!rev(duplicated(rev(eerstvlgnd$rem_uit))),]


# test remmomenten voor een auto
plot(x = s$time, 
     y = s$speed, 
     type='o', 
     pch=16, 
     cex=0.5,
     xlim=c(as.numeric(as.POSIXct("2017-06-01 12:00:00")), as.numeric(as.POSIXct("2017-06-01 12:10:00"))),
     xlab='time (minutes)',
     ylab='speed (km/h)', 
     las=1)

r <- as.POSIXct(round(range(s$time), "hours"))
axis.POSIXct(1, at = seq(r[1], r[2], by = "min"), format = "%M")


col = rev(brewer.pal(n = 9, name = 'Reds'))

rect(xleft = v$rem_aan, xright = v$rem_uit, ytop = 0, ybottom = -5, col= 'grey')
vv = subset(x = v, subset = dv < 0 & dt > 0)
rect(xleft = vv$t0, xright = vv$t1, ytop = vv$v0, ybottom = vv$v1, col=rgb(1,0,0,0.5), border = NA)
rug(x=b$time, col=2, lwd=2, ticksize =0.03)
rug(x=l$time, col=3, lwd=2, ticksize =0.03)
legend(x='topleft', 
       legend = c('brake on - brake off', 'speed reduction detected', 'speed'), 
       fill=c('grey', rgb(1,0,0,0.5), NA), 
       border=NA, 
       lty=c(NA,NA,1), bty= 'n', pch=c(NA,NA,16))

