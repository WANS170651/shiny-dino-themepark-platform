# global.R

# Load necessary packages
library(shinythemes)
library(shinyWidgets)
library(shinyalert)
library(leaflet)
library(tidyverse)
library(DT)
library(RPostgres)
library(shiny)
library(bslib)     # Add jsonlite package for JSON operations
library(jsonlite)     # Add jsonlite package for JSON operations

# Connect to Postgres database
con <- dbConnect(
  drv = dbDriver('Postgres'),
  dbname = 'themepark_4',
  host = 'db-postgresql-nyc1-44203-do-user-8018943-0.b.db.ondigitalocean.com',
  port = 25061,
  user = 'proj4',
  password = 'AVNS_kKHr9uGrcsj1IrEFtJ_'
)

# Load tables
rides <- dbReadTable(con, "rides")
hotels <- dbReadTable(con, "hotels")
restaurants <- dbReadTable(con, "restaurants")
menu_items <- dbReadTable(con, "menu_items")
tickets <- dbReadTable(con, "tickets")
park_hours <- dbReadTable(con, "park_hours")

# Create users table if it doesn't exist
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS users (
    user_id SERIAL PRIMARY KEY,
    username VARCHAR(50) UNIQUE NOT NULL,
    password VARCHAR(100) NOT NULL,
    email VARCHAR(100) UNIQUE NOT NULL,
    full_name VARCHAR(100) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  )
")

# Create orders table if it doesn't exist
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS orders (
    order_id SERIAL PRIMARY KEY,
    user_id INTEGER REFERENCES users(user_id),
    order_type VARCHAR(20) NOT NULL,
    item_id INTEGER NOT NULL,
    quantity INTEGER NOT NULL,
    total_price DECIMAL(10,2) NOT NULL,
    order_date TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    status VARCHAR(20) DEFAULT 'pending',
    details VARCHAR(1000)
  )
")

# Team members
mems <- c(
  "Chenxiao Lu", 
  "Yuehan Wang", 
  "Jinming Liu", 
  "Yuqi Wang", 
  "Teng Lin", 
  "Zhe Li", 
  "Xiang Liu"
)
