###
# Creates a fundamenteel diagram on NDW data
# author: TomT
###

require("RPostgreSQL")
require("ggplot2")
require("hexbin")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "research",
                 host = "localhost", port = 5433,
                 user = "postgres")
df_postgres <- dbGetQuery(con, "
WITH elements AS (
	SELECT mst_id,json_array_elements(characteristics) elem
	FROM ndw.mst_points
	WHERE mst_id = 'RWS01_MONIBAS_0161hrl0173ra'
)
,indices AS (
	SELECT 
	mst_id,
	(elem->>'index')::int as index,
	elem->'measurementSpecificCharacteristics'->>'specificMeasurementValueType' as type
	FROM elements
	WHERE 
	elem->'measurementSpecificCharacteristics'->'specificVehicleCharacteristics'->>'vehicleType' = 'anyVehicle'
),
data AS (
	SELECT DISTINCT a.date, a.location, 
		b.index as index, 
		b.type as type, 
		CASE WHEN b.type ='trafficSpeed' THEN values[b.index] END as speedvalue,
		CASE WHEN b.type ='trafficFlow' THEN values[b.index] END as flowvalue
	FROM 
	ndw.trafficspeed_2 a
	INNER JOIN indices b ON (a.location = b.mst_id)
	WHERE --a.date = '2017-10-27T12:13:00Z' 
	a.date > now() - '7 days'::interval
	AND a.values[b.index] > 0
)
SELECT 
	date, 
  location,
  CASE 
    WHEN date_part('dow',date) = 5 THEN 'friday'
    ELSE 'not friday'
  END as day,
  CASE 
    WHEN date_part('hour',date) < 6 OR date_part('hour',date) >= 20 THEN 'night'
    WHEN date_part('hour',date) >= 6 AND date_part('hour',date) < 10 THEN 'morning'
    WHEN date_part('hour',date) >= 10 AND date_part('hour',date) < 17 THEN 'day'
    WHEN date_part('hour',date) >= 17 AND date_part('hour',date) < 20 THEN 'evening'
    END as tod,
	avg(speedvalue) as speed_avg, count(speedvalue) as speedvalues,
	sum(flowvalue) as flow_sum, count(flowvalue) as flowvalues
FROM data a
GROUP BY a.location, a.date
")
ggplot(df_postgres,aes(flow_sum,speed_avg, colour = factor(tod))) + geom_point() + theme_bw()

hist(df_postgres$speed_avg, breaks = seq(min(df_postgres$speed_avg, na.rm = T),max(df_postgres$speed_avg, na.rm=T),1))
hist(df_postgres$flow_sum, breaks = seq(min(df_postgres$flow_sum, na.rm = T),max(df_postgres$flow_sum, na.rm=T),1))
