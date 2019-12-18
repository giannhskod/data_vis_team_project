###################################################################
# SQLITE QUERIES                                                ###
###################################################################

query.flights.inbound = 
"SELECT 
	'INBOUND' as type,
	COUNT(*) AS itineraries_count,
	a_origin.iata AS origin_airport_code,
	a_origin.airport AS origin_airport_name,
	a_origin.long AS origin_airport_long,
	a_origin.lat AS origin_airport_lat,
	a_dest.iata AS destination_airport_code,
	a_dest.airport AS destination_airport_name,
	a_dest.long AS destination_airport_long,
	a_dest.lat AS destination_airport_lat
FROM 
	flights AS f,
	airports AS a_origin,
	airports AS a_dest
WHERE 1 = 1
AND f.Origin = a_origin.iata
AND f.Dest = a_dest.iata
AND f.Year IN (2004, 2005, 2006, 2007, 2008)
AND f.Dest IN ('TUL', 'MKE')
GROUP BY 
	a_origin.iata,
	a_dest.iata"

query.flights.outbound = 
"SELECT 
	'OUTBOUND' as type,
	COUNT(*) AS itineraries_count,
	a_origin.iata AS origin_airport_code,
	a_origin.airport AS origin_airport_name,
	a_origin.long AS origin_airport_long,
	a_origin.lat AS origin_airport_lat,
	a_dest.iata AS destination_airport_code,
	a_dest.airport AS destination_airport_name,
	a_dest.long AS destination_airport_long,
	a_dest.lat AS destination_airport_lat
FROM 
	flights AS f,
	airports AS a_origin,
	airports AS a_dest
WHERE 1 = 1
AND f.Origin = a_origin.iata
AND f.Dest = a_dest.iata
AND f.Year IN (2004, 2005, 2006, 2007, 2008)
AND f.Origin IN ('TUL', 'MKE')
GROUP BY 
	a_origin.iata,
	a_dest.iata"

query.flights.inbound.outbound =
"SELECT 
	'INBOUND' as type,
	COUNT(*) AS itineraries_count,
	a_origin.iata AS origin_airport_code,
	a_origin.airport AS origin_airport_name,
	a_origin.long AS origin_airport_long,
	a_origin.lat AS origin_airport_lat,
	a_dest.iata AS destination_airport_code,
	a_dest.airport AS destination_airport_name,
	a_dest.long AS destination_airport_long,
	a_dest.lat AS destination_airport_lat
FROM 
	flights AS f,
	airports AS a_origin,
	airports AS a_dest
WHERE 1 = 1
AND f.Origin = a_origin.iata
AND f.Dest = a_dest.iata
AND f.Year IN (2004, 2005, 2006, 2007, 2008)
AND f.Dest IN ('TUL', 'MKE')
GROUP BY 
	a_origin.iata,
	a_dest.iata
UNION ALL
SELECT 
	'OUTBOUND' as type,
	COUNT(*) AS itineraries_count,
	a_origin.iata AS origin_airport_code,
	a_origin.airport AS origin_airport_name,
	a_origin.long AS origin_airport_long,
	a_origin.lat AS origin_airport_lat,
	a_dest.iata AS destination_airport_code,
	a_dest.airport AS destination_airport_name,
	a_dest.long AS destination_airport_long,
	a_dest.lat AS destination_airport_lat
FROM 
	flights AS f,
	airports AS a_origin,
	airports AS a_dest
WHERE 1 = 1
AND f.Origin = a_origin.iata
AND f.Dest = a_dest.iata
AND f.Year IN (2004, 2005, 2006, 2007, 2008)
AND f.Origin IN ('TUL', 'MKE')
GROUP BY 
	a_origin.iata,
	a_dest.iata"

query.delays.security.by_destination =
"SELECT
	'SECURITY_DELAY' as type,
	a_origin.iata AS origin_airport_code,
	a_origin.airport AS origin_airport_name,
	a_origin.long AS origin_airport_long,
	a_origin.lat AS origin_airport_lat,
	a_dest.iata AS destination_airport_code,
	a_dest.airport AS destination_airport_name,
	a_dest.long AS destination_airport_long,
	a_dest.lat AS destination_airport_lat,
	AVG(f.SecurityDelay) as average_security_delay
FROM 
	flights AS f,
	airports AS a_origin,
	airports AS a_dest
WHERE 1 = 1
AND f.Origin = a_origin.iata
AND f.Dest = a_dest.iata
AND f.Year IN (2004, 2005, 2006, 2007, 2008)
AND f.Origin IN ('TUL', 'MKE')
AND (f.SecurityDelay IS NOT NULL AND f.SecurityDelay <> 0)
GROUP BY
	f.Origin,
	f.Dest"
