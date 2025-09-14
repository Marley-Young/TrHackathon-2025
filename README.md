# TrHackathon-2025
Interactive dashboard for visualising and forecasting track and field data,
built in R using Shiny. You can view the app at https://marley-young.shinyapps.io/Trhackathon/

This is an entry into the TrHackathon (AthTech Challenge) 2025: https://athtech.run/2025/hackathon
The data for this project can be found at https://github.com/openath/trhackathon2025

It is parsed in the script collectData.R, and weather data is collected using
the open source API openmeteo.

The precomputed forecasts in MDT_forecast.rds (which are required for dashboard.R)
were too large to upload here, but can be computed using the timeSeries.R script 
(this takes a long time however)
