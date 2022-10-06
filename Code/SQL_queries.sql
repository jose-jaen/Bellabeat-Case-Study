-- Exploratory data analysis with somple queries
SELECT COUNT(DISTINCT id) AS total_ids FROM daily_activity; 

SELECT COUNT(DISTINCT id) AS sleep_ids FROM daily_sleep;

SELECT COUNT(DISTINCT id) AS weight_ids FROM weight;

SELECT COUNT(DISTINCT activitydate) AS days  
FROM daily_activity;

SELECT DISTINCT id 
FROM daily_activity
WHERE id NOT IN (SELECT DISTINCT id FROM daily_sleep);

SELECT DISTINCT id 
FROM daily_activity
WHERE id NOT IN (SELECT DISTINCT id FROM weight);

SELECT 
COUNT(DISTINCT activitydate) AS usage_days
FROM daily_activity
GROUP BY id;

-- Average activity days per runner group
WITH temp_table AS (
    SELECT COUNT(DISTINCT activitydate)
    FROM daily_activity
    GROUP BY id)
SELECT ROUND(AVG(usage_days), 2) AS average_days 
FROM temp_table;

-- Categorizing runners according to their activity
WITH temp_table2 AS (
    SELECT 
        ROUND(AVG(TotalDistance), 2) AS mean_distance,
        COUNT(DISTINCT activitydate) AS number_of_days,
        id,
        CASE
            WHEN ROUND(AVG(TotalDistance), 2) < 5
            THEN 'Beginner Level'
            WHEN ROUND(AVG(TotalDistance), 2) >= 5 AND ROUND(AVG(TotalDistance), 2) < 8
            THEN 'Intermediate Level'
        ELSE 'Pro level'
        END AS user_type
    FROM daily_activity
    GROUP BY id)
SELECT 
  MAX(TEMP.mean_distance) AS average_distance_best_runner
  MIN(ACT.totaldistance) AS minimum_distance
  ROUND(AVG(ACT.totaldistance), 2) AS average_distance_all_runners
FROM temp_table2 TEMP
INNER JOIN daily_activity ACT ON TEMP.id = ACT.id;

-- Distribution of running distance for each user type
WITH temp_table3 AS (
SELECT 
    CASE
        WHEN ROUND(AVG(TotalDistance), 2) < 5
        THEN 'Beginner Level'
        WHEN ROUND(AVG(TotalDistance), 2) >= 5 AND 
        ROUND(AVG(TotalDistance), 2) < 8
        THEN 'Intermediate Level'
    ELSE 'Pro level'
    END AS user_type,
    id
FROM daily_activity
GROUP BY id)
SELECT 
    ACT.*, TEMPO.* 
FROM daily_activity ACT
INNER JOIN temp_table3 TEMPO ON TEMPO.id = ACT.id;

-- Relationship between intensity and calories
SELECT 
    sedentaryminutes AS sedentary_minutes,
    lightlyactiveminutes AS lightly_minutes,
    fairlyactiveminutes AS fairly_active_minutes,
    veryactiveminutes AS very_active_minutes,
    calories, 
     CASE
        WHEN ROUND(AVG(TotalDistance), 2) < 5
        THEN 'Beginner Level'
        WHEN ROUND(AVG(TotalDistance), 2) >= 5 AND 
        ROUND(AVG(TotalDistance), 2) < 8
        THEN 'Intermediate Level'
    ELSE 'Pro level'
    END AS user_type
FROM daily_activity
GROUP BY 
  id, sedentaryminutes, lightlyactiveminutes, 
  fairlyactiveminutes, veryactiveminutes, calories;

-- Average intensity minutes per runner type
SELECT  
    INTEN.id, 
    ROUND(AVG(INTEN.sedentaryminutes), 2) AS average_sedentary,
    ROUND(AVG(INTEN.lightlyactiveminutes), 2) AS average_light,
    ROUND(AVG(INTEN.fairlyactiveminutes), 2) AS average_faire,
    ROUND(AVG(INTEN.veryactiveminutes), 2) AS average_active, 
    CASE
        WHEN ROUND(AVG(ACT.TotalDistance), 2) < 5
        THEN 'Beginner Level'
        WHEN ROUND(AVG(ACT.TotalDistance), 2) >= 5 AND 
        ROUND(AVG(ACT.TotalDistance), 2) < 8
        THEN 'Intermediate Level'
    ELSE 'Pro level'
    END AS user_type
FROM daily_activity ACT
JOIN daily_intensities INTEN
ON INTEN.id = ACT.id
GROUP BY INTEN.id
ORDER BY ROUND(AVG(ACT.TotalDistance), 2) DESC;

-- Distribution of daily sleeping hours
SELECT 
    TO_CHAR(TO_DATE(sleepday, 'MM/DD/YYYY'), 'Dy') AS dates,
    ROUND(totaltimeinbed/60, 2) AS bed_hours
FROM daily_sleep
GROUP BY dates, bed_hours
ORDER BY dates ASC;

-- Getting sleeping time and distance for each group
SELECT 
    ROUND(SLEEP.totaltimeinbed/60, 2) AS sleep_hours,
    ROUND(ACT.totaldistance, 2) AS distance,
    ACT.activitydate AS date,
    CASE
        WHEN ROUND(AVG(ACT.TotalDistance), 2) < 5
        THEN 'Beginner Level'
        WHEN ROUND(AVG(ACT.TotalDistance), 2) >= 5 AND 
        ROUND(AVG(ACT.TotalDistance), 2) < 8
        THEN 'Intermediate Level'
    ELSE 'Pro level'
    END AS user_type
FROM daily_activity ACT
INNER JOIN daily_sleep SLEEP 
ON SLEEP.id = ACT.id
GROUP BY ACT.id, sleep_hours, distance, date
ORDER BY date ASC;