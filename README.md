# Delivery-Times-Dashboard

Delivery Times Dashboard
 Try the Live App: https://72a74w-nour-mohamed.shinyapps.io/finalprojectGUI/

An interactive dashboard built using R Shiny to explore food delivery data, understand factors affecting delivery times, and discover patterns through data visualization, K-Means clustering, and association rule mining.

Features:
1.Data Preview & Cleaning

- Upload datasets in CSV or Excel (.xlsx) format
- Remove duplicate records.
- Handle missing values.
- Convert categorical columns into factors.
- Detect and remove outliers from delivery time.
- Calculate delivery speed and create a late delivery indicator.
- View cleaned data and summary statistics.

2.Data Visualization
Explore delivery data through different plots, including:

- Scatter plots showing relationships between delivery time, preparation time, distance, and speed.
- Histograms for delivery time, distance, and preparation time.
- Boxplots to explore delivery time across weather conditions, traffic levels, and vehicle types.
- Bar plots showing weather and traffic distributions.
- A line plot showing delivery time throughout different times of the day.

3.K-Means Clustering
- Apply K-Means clustering to group delivery records.
- Choose between 3 and 4 clusters.
- Scale numerical features before clustering.
- Visualize the resulting clusters.
Note: Three clusters are considered the preferred choice in this project

4.Association Rule Mining
- Apply the Apriori algorithm to discover patterns related to late deliveries.
- Explore relationships between vehicle type, traffic level, weather conditions, and late delivery status.
- Adjust support and confidence values.
- Display the top 10 rules ranked by lift.

Technologies & Libraries:
- R
- Shiny
- ShinyWidgets
- ShinyThemes
- bslib
- ggplot2
- DT
- readxl
- arules
- arulesViz
- factoextra

How to Run Locally
1. Copy or download this repository.
2. Download the dataset "Food_Delivery_Times" from the same respository
3. Open the project in RStudio.
4. Install the required packages:
install.packages(c(
  "shiny", "shinyWidgets", "shinythemes",
  "bslib", "fresh", "ggplot2", "DT",
  "readxl", "arules", "arulesViz", "factoextra"
))
5. Run app.R
6. Upload delivery dataset using the file upload option in the dashboard.
7. Explore the data, visualizations, clusters, and association rules.

The dashboard will be empty before uploading the dataset

 Dashboard Design: The dashboard uses a lavender-themed interface with interactive controls and multiple tabs to make exploring delivery data easier

Project Goal: To explore delivery patterns and understand how different factors relate to delivery time and late deliveries using data analysis and unsupervised learning
