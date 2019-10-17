create table events (id INTEGER PRIMARY KEY AUTOINCREMENT, timestamp TEXT, lat FLOAT, lon FLOAT, x FLOAT, y FLOAT, z FLOAT);

UPDATE events SET position = MakePoint(lon, lat, 4326); -- XY


-- INSERT events VALUES (NULL, timestamp, lat, lon, x, y, z, MakePoint(lat, lon, 4326));
