
SELECT business_id, 
    name,
    address,
    city,
    state,
    postal_code,
    latitude,
    longitude,
    stars,
    review_count,
    --Select relevant attributes
    (CASE 
            WHEN STRPOS((attributes::json) ->> 'WiFi', 'free') <> 0
                THEN 'true'
            ELSE 'false'--Only go for free WiFi or no Wifi 
    END) AS free_WiFi,  
    (CASE 
            WHEN STRPOS((attributes::json) ->> 'HasTV', 'True') <> 0
                THEN 'true'
            WHEN STRPOS((attributes::json) ->> 'HasTV', 'False') <> 0
                THEN 'false'
            ELSE 'NA'--There might be None values 
     END) AS has_TV,
    (CASE 
            WHEN STRPOS((attributes::json) ->> 'Caters', 'True') <> 0
                THEN 'true'
            WHEN STRPOS((attributes::json) ->> 'Caters', 'False') <> 0
                THEN 'false'
            ELSE 'NA'--There might be None values 
    END) AS caters,
    attributes::json ->> 'Alcohol' AS alcohol,
    attributes::json ->> 'NoiseLevel' AS noise_level,
    (CASE 
            WHEN STRPOS((attributes::json) ->> 'GoodForKids', 'True') <> 0
                THEN 'true'
            WHEN STRPOS((attributes::json) ->> 'GoodForKids', 'False') <> 0
                THEN 'false'
            ELSE 'NA'--There might be None values 
    END) AS for_kids,
    (CASE 
            WHEN STRPOS((attributes::json) ->> 'OutdoorSeating', 'True') <> 0
                THEN 'true'
            WHEN STRPOS((attributes::json) ->> 'OutdoorSeating', 'False') <> 0
                THEN 'false'
            ELSE 'NA'--There might be None values 
    END) AS outdoor_seating,
    (CASE 
            WHEN STRPOS((attributes::json) ->> 'BusinessParking', 'True') <> 0 --If any of the levels are True, then True
                THEN 'true'
            WHEN STRPOS((attributes::json) ->> 'BusinessParking', 'False') <> 0
                THEN 'false'
            ELSE 'NA'--There might be None values 
    END) AS has_parking,
    (CASE 
            WHEN STRPOS((attributes::json) ->> 'RestaurantsTakeOut', 'True') <> 0 
                THEN 'true'
            WHEN STRPOS((attributes::json) ->> 'RestaurantsTakeOut', 'False') <> 0
                THEN 'false'
            ELSE 'NA'--There might be None values 
    END) AS has_takeout,
    CAST((businesstable.attributes::json) ->> 'RestaurantsPriceRange2' AS INTEGER) AS price_range,
    (CASE 
            WHEN STRPOS((attributes::json) ->> 'RestaurantsReservations', 'True') <> 0 
                THEN 'true'
            WHEN STRPOS((attributes::json) ->> 'RestaurantsReservations', 'False') <> 0
                THEN 'false'
            ELSE 'NA'--There might be None values 
    END) AS takes_reservations,
    (CASE 
            WHEN STRPOS((attributes::json) ->> 'RestaurantsGoodForGroups', 'True') <> 0 
                THEN 'true'
            WHEN STRPOS((attributes::json) ->> 'RestaurantsGoodForGroups', 'False') <> 0
                THEN 'false'
            ELSE 'NA'--There might be None values 
    END) AS good_for_groups,
    (CASE 
            WHEN STRPOS((attributes::json) ->> 'BusinessAcceptsCreditCards', 'True') <> 0 
                THEN 'true'
            WHEN STRPOS((attributes::json) ->> 'BusinessAcceptsCreditCards', 'False') <> 0
                THEN 'false'
            ELSE 'NA'--There might be None values 
    END) AS credit_card_acceptance,
    (CASE 
            WHEN categories LIKE '%Italian%' OR categories LIKE '%Pizza%'
                THEN 'Italian'
            WHEN categories LIKE '%Mexican%' OR categories LIKE '%Latin American%' OR categories LIKE '%Caribbean%'
                THEN 'Latin American'
            WHEN categories LIKE '%Asian%' OR categories LIKE '%Chinese%' OR categories LIKE '%Japanese%' OR categories LIKE '%Sushi%' OR categories LIKE '%Thai%' OR categories LIKE '%Taiwanese%' OR categories LIKE '%Ramen%' OR categories LIKE '%Vietnamese%' OR categories LIKE '%Korean%' OR categories LIKE '%Mongolian%' OR categories LIKE '%Indian%'
                THEN 'Asian'
            WHEN categories LIKE '%American%' OR categories LIKE '%Burger%' OR categories LIKE '%Waffles%' OR categories LIKE '%Chicken Wings%' OR categories LIKE '%Hot Dog%' OR categories LIKE '%Barbeque%' OR categories LIKE '%Sandwiches%' OR categories LIKE '%Steak%' OR categories LIKE '%Diner%'
                THEN 'American'
            --WHEN categories LIKE '%African%' OR categories LIKE '%Ethiopian%'--Only a few restaurants, put in Misc.
            --  THEN 'African'
            WHEN categories LIKE '%Mediterranean%' OR categories LIKE '%Seafood%'
                THEN 'Mediterranean'
            ELSE 'Other'--Misc.
    END) AS categories,
    hours::json -> 'Monday' AS mon_open_hours,
    hours::json -> 'Tuesday' AS tue_open_hours,
    hours::json -> 'Wednesday' AS wed_open_hours,
    hours::json -> 'Thursday' AS thr_open_hours,
    hours::json -> 'Friday' AS fri_open_hours,
    hours::json -> 'Saturday' AS sat_open_hours,
    hours::json -> 'Sunday' AS sun_open_hours
FROM public3.businesstable
WHERE businesstable.categories LIKE '%Restaurants' 
    AND city LIKE '%Charlotte' --If we want to limit ourselves to only a subset of city/cities
    AND is_open = TRUE --If we only want to include open restaurants





--Review Table (static):
--csv name: reviewtable_infos.csv


SELECT business_id, avg(stars) AS avg_stars_us, count(*) AS n_reviews_us, SUM(useful) AS usefull, SUM(funny) AS funny, SUM(cool) AS cool
FROM public3.reviewtable
GROUP BY business_id


-- For Sentiment Analysis:
--NOTE: Reviews and Tips for one year only (2017) --> Take the last year with complete dataset (2017)


--Reviews
--csv name: reviews_for_nlp.csv
SELECT business.business_id, review.stars, review.date, review.text
FROM public3.businesstable AS business
LEFT JOIN(  
    SELECT *
    FROM public3.reviewtable
) AS review
ON business.business_id = review.business_id
WHERE city = 'Charlotte' AND review.date >= '2017-01-01' AND review.date <= '2017-12-31'
ORDER BY business.business_id


--Tips
--csv name: tips_for_nlp.csv
SELECT business.business_id, tips.compliment_count, tips.date, tips.text
FROM public3.businesstable AS business
LEFT JOIN(  
    SELECT *
    FROM public3.tipstable
) AS tips
ON business.business_id = tips.business_id
WHERE city = 'Charlotte' AND tips.date >= '2017-01-01' AND tips.date <= '2017-12-31'
ORDER BY business.business_id


--Phototable:
--csv name: phototable_infos.csv
SELECT COUNT(photo_id) as n_photo, business_id
FROM public3.phototable
GROUP BY business_id



--Adapted code from prof with the variables “check-in” per day, cumulated number of tips for specified period, the max number of years a user who left a tip was elite and the max number of tips from all users who left a tip:
-- NOTE: For one year only (2017) --> Take the last year with complete dataset (2017)
--csv name: dcheckins_cum_infos.csv


SELECT t_ru.business_id6 AS business_id
    ,(CASE WHEN t_ch.date3 IS NULL THEN 0 ELSE 1 END) AS ch_in
    ,t_ru.date6 AS date_tip
    ,t_ru.cum_n_tips AS cum_n_tips
    ,t_ru.cum_max_u_elite AS cum_max_u_elite
    ,t_ru.cum_max_us_tip AS cum_max_us_tip
FROM (
    -- table3: for checkins
    SELECT t3_1.business_id AS business_id3
        ,date1::DATE AS date3
    FROM (
        SELECT public3.checkintable.business_id AS business_id
            ,unnest(string_to_array(DATE, ',')) AS date1
        FROM public3.checkintable, public3.businesstable
        WHERE public3.checkintable.business_id = public3.businesstable.business_id AND public3.businesstable.city='Charlotte'
        ) AS t3_1
    GROUP BY business_id3
        ,date3
    ) AS t_ch
RIGHT JOIN (
    -- table6.2: a much more elegant, but more complex query
    SELECT tip_user.business_id51 AS business_id6
        ,tip_user.date5 AS date6
        ,tip_user.n_tips AS n_tips
        ,tip_user.cum_n_tips AS cum_n_tips
        ,(
            SELECT max(max_u_elite) AS cum_max_u_elite
            FROM (
                SELECT business_id
                    ,DATE
                    ,max(users.n_elite) AS max_u_elite
                FROM public3.tipstable
                LEFT JOIN (
                    SELECT user_id AS user_id
                        ,array_length(string_to_array(users.elite, ','), 1) AS n_elite
                    FROM public3.userstable AS users
                    ) AS users ON public3.tipstable.user_id = users.user_id
                GROUP BY business_id
                    ,DATE
                ) AS t53
            WHERE t53.business_id = tip_user.business_id51
                AND t53.DATE::DATE < tip_user.date5
            )
        ,(
                    SELECT max(max_us_tip) AS cum_max_us_tip
                    FROM (
                        SELECT business_id
                            ,DATE
                            ,max(users.us_tip) AS max_us_tip
                        FROM public3.tipstable
                        LEFT JOIN (
                            SELECT user_id AS user_id
                                ,review_count AS us_tip
                            FROM public3.userstable AS users
                            ) AS users ON public3.tipstable.user_id = users.user_id
                        GROUP BY business_id
                            ,DATE
                        ) AS t53
                    WHERE t53.business_id = tip_user.business_id51
                        AND t53.DATE::DATE < tip_user.date5
                    )
    FROM (
        SELECT t52.business_id51 AS business_id51
            ,t52.date5 AS date5
            ,t52.n_tips AS n_tips
            ,(
                SELECT COUNT(t51.TEXT)
                FROM public3.tipstable AS t51
                WHERE t51.business_id = t52.business_id51
                    AND t51.DATE::DATE < t52.date5
                ) AS cum_n_tips
        FROM (
            SELECT business_id53 AS business_id51, date53 AS date5, n_tips
            FROM (SELECT tip.business_id AS business_id53, date_trunc('day', generate_series
                    ( '2017-01-01'::timestamp 
                    , '2017-12-31'::timestamp
                    , '1 day'::interval))::date AS date53
                FROM public3.tipstable AS tip, public3.businesstable AS bus
                WHERE tip.business_id=bus.business_id AND bus.city='Charlotte'
                GROUP BY tip.business_id) AS t53
                LEFT JOIN 
                    (SELECT tip.business_id AS business_id5x
                                            ,DATE::DATE AS date5x
                                            ,COUNT(tip.TEXT) AS n_tips
                                        FROM public3.tipstable AS tip, public3.businesstable AS bus
                                        WHERE tip.business_id=bus.business_id AND bus.city='Charlotte'
                                        GROUP BY tip.business_id
                                            ,date5x) AS t5x
                    ON business_id5x=business_id53 AND date5x=date53
            ) AS t52
        ) AS tip_user
    ) AS t_ru ON t_ch.date3 = t_ru.date6
    AND t_ch.business_id3 = t_ru.business_id6
WHERE cum_n_tips <> 0;


--New variable “Reach”:
-- Example: At Restaurant X, on the 4th of January, 1 tip and 1 review were left by 2 different users. Each user had 50 friends and 10 fans. Hence, reach for Restaurant X on January 4th would be 120 (50+10+50+10)!
--IDEA for later calculations: Have a rolling reach sum (average??) (for example last two weeks) --> Tip from person with high reach will lead to more check-ins


--Number of fans which are potentially reached by a review/tipdate
--csv name: n_reached_fans.csv
SELECT *
FROM(
    SELECT business_id, SUM(fans) AS n_reached_fans, date::DATE AS date
    FROM (SELECT review_tips.user_id,review_tips.business_id, review_tips.date, fans
        FROM( SELECT user_id, business_id, date FROM public3.reviewtable UNION SELECT user_id, business_id, date 
            FROM public3.tipstable) AS review_tips LEFT JOIN public3.userstable AS users ON review_tips.user_id=users.user_id) AS reach_data_fans 
        GROUP BY business_id, date
        ) AS fans_reach
ORDER BY business_id, date



--Number of friends which are potentially reached by a review/tip
--csv name: n_reached_friends.csv           
SELECT *
FROM(
    SELECT business_id, COUNT(user_names) AS n_reached_friends, date::DATE AS date
    FROM (SELECT review_tips.user_id,review_tips.business_id, unnest(string_to_array(users.friends, ',')) AS user_names, review_tips.date, fans
        FROM( SELECT user_id, business_id, date FROM public3.reviewtable UNION SELECT user_id, business_id, date 
            FROM public3.tipstable) AS review_tips LEFT JOIN public3.userstable AS users ON review_tips.user_id=users.user_id) AS reach_data_friends
        GROUP BY business_id, date
        ) AS friends_reach
ORDER BY business_id, date


--Get average number of friends and fans per business_id for all users who have left a tip/review in charlotte as proxy for check-in reach 
--csv name: Average number of friends and fans per business id.csv  
SELECT business.business_id, AVG(users.n_friends) AS avg_n_friends_bus, AVG(users.fans) AS avg_n_fans_bus
FROM public3.businesstable AS business
LEFT JOIN(
    SELECT user_reviews.user_id, user_reviews.business_id, n_friends, fans
    FROM(
        SELECT user_id, business_id FROM public3.reviewtable 
        UNION SELECT user_id, business_id FROM public3.tipstable --Also get tips 
    ) AS user_reviews
    LEFT JOIN (
        SELECT user_id, array_length(string_to_array(friends,',') ,1) AS n_friends, fans
        FROM public3.userstable
        ) AS user_friends
    ON user_friends.user_id=user_reviews.user_id
    ) as users
ON business.business_id = users.business_id
WHERE city = 'Charlotte' AND is_open = True
GROUP BY business.business_id
ORDER BY business.business_id




------------------ END OF CODE FOR ANALYSIS --------------------------


--SOME HELPFUL QUERIES!--


--Example where there was a none in the attributes:


SELECT attributes::json ->> 'BusinessParking'
FROM public3.businesstable
WHERE business_id = 'fgxW2Eww2MzMHd8eWT58Ig';


--Find distinct attribute levels:


SELECT distinct(attributes::json) ->> 'Caters'
FROM public3.businesstable
WHERE businesstable.categories LIKE '%Restaurants' 
    AND city LIKE '%Charlotte' --If we want to limit ourselves to only a subset of city/cities
    AND is_open = TRUE --If we only want to include open restaurants


--Get Category value frequency in descending order:


SELECT count(categories), categories
FROM (SELECT unnest(string_to_array(categories, ',')) AS categories
    FROM public3.businesstable
    WHERE businesstable.categories LIKE '%Restaurants') AS categories


GROUP BY categories
ORDER BY count DESC



--How many friends and fans did the user from a review/tip for a restaurant have?
SELECT user_reviews.user_id, user_reviews.business_id, n_friends, fans
    FROM(
        SELECT user_id, business_id FROM public3.reviewtable 
        UNION SELECT user_id, business_id FROM public3.tipstable --Also get tips 
    ) AS user_reviews
    LEFT JOIN (
        SELECT user_id, array_length(string_to_array(friends,',') ,1) AS n_friends, fans
        FROM public3.userstable
        ) AS user_friends
    ON user_friends.user_id=user_reviews.user_id
WHERE user_reviews.user_id = 'MyFtpeGsV4z3zvFBnfGIvw'


AS
--Sanity check, that "Reach" variable works
SELECT t51.user_id, business_id, date::DATE AS date
FROM(
    SELECT user_id, business_id, date FROM public3.reviewtable UNION SELECT user_id, business_id, date
                        FROM public3.tipstable) AS t51
WHERE business_id = '01fuY2NNscttoTxOYbuZXw' --AND date = '2010-11-19'
ORDER BY date


SELECT user_id, array_length(string_to_array(friends,',') ,1) AS n_friends, fans
        FROM public3.userstable
WHERE user_id = 'MyFtpeGsV4z3zvFBnfGIvw'


--Information  on User Table:


SELECT user_id, name, review_count, 
    array_length(string_to_array(friends,',') ,1) AS n_friends,
    useful,
    funny,
    cool,
    array_length(string_to_array(elite,',') ,1) AS years_elite,--Amount of years, the user was elite
    fans,
    average_stars
FROM public3.userstable


-- What's the latest date where there is data on tips? --> November 2018 --> Take the last year with complete dataset (2017)
SELECT *
FROM public3.tipstable
ORDER BY date DESC


SELECT *
FROM public3.reviewtable
ORDER BY date DESC
