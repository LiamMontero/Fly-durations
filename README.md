# Data Analyst (Academic Project) – Flight Trend Analysis

## Exploratory Data Analysis with R and dplyr

I was tasked with analyzing a flight dataset from the second half of 2022 to extract key trends and answer specific operational questions. My goal was to transform the raw data into actionable insights, using R and the dplyr package for data manipulation and aggregation.

My approach was based on the integration of three distinct data sources: a detailed flight log, an airline catalog, and an airport database. By combining these sources, I was able to enrich the transactional data with descriptive information, such as the full names of airlines and airports, which was crucial for presenting clear and understandable results.

To answer the business questions, I performed a series of focused and precise analyses:

1. Identifying the Busiest Route: I analyzed all flights departing from New York City-area airports (robustly identified by their time zone, "America/New_York"). By grouping and counting the flights, I accurately determined the combination of airline and destination airport that represents the highest-volume air corridor, as well as the average flight duration for this critical route.

2. Determining the Longest-Duration Route: To identify long-haul operations, I calculated the average flight duration in hours for each route departing from New York City. This allowed me to identify the destination that, on average, requires the longest flight time, a key input for crew logistics and route planning.

3. Least-Flying Destination Discovery: With a focus on JFK Airport, I performed a frequency analysis to identify the destination airport that receives the fewest flights. This type of insight is valuable for business strategy, as it can point to unmet market opportunities or routes with low demand that may require review.

My work not only provided direct answers to the questions posed, but also demonstrated a complete data analysis workflow, from loading and cleansing to integrating multiple tables and generating business metrics.
Accomplishments and Demonstrated Skills:
+ Extracted key business insights from complex flight data, answering questions about the most and least frequent routes and those with the longest durations.
+ Demonstrated high proficiency in data manipulation with dplyr, using functions such as filter, group_by, summarise, and which.max to perform complex analyses.
+ Successfully integrated and managed multiple data sources to enrich analysis and present understandable results (e.g., converting FAA airport codes into full names).
+ Performed frequency and aggregation analysis to quantify operational patterns, such as identifying the busiest air corridor from New York.
+ Transformed data into business-useful metrics, such as converting flight time from minutes to hours for better interpretation.
+ Provided actionable business intelligence, identifying both high-performing routes and potential opportunities in less-frequented destinations.
