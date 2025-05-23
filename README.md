````markdown
# Dino World — Prehistoric Adventure Theme Park Interactive App

## Project Overview  
Dino World is an R Shiny–based interactive theme-park demonstration application that offers visitors a one-stop online experience: “Map Explorer,” “Attraction Details,” “Food Ordering,” “Hotel Booking,” “Ticket Purchase,” and “My Orders.” It combines a modern web interface with responsive design to simulate real-world theme-park operations, improving both user experience and operational efficiency.

## Key Features  
- **Home Page**: Custom branding with logo and tagline, plus a one-click entry to the ticket-purchase flow.  
- **Map Explorer**: Dynamic Leaflet map displaying attractions, dining venues, and hotels; includes category filtering and zoom controls. :contentReference[oaicite:0]{index=0}  
- **Attraction Details**: Detailed listings of rides and shows—name, category, height requirement, duration, and capacity. :contentReference[oaicite:1]{index=1}  
- **Food Ordering**: Restaurant list with menu pop-ups, order form validation, and persistent order saving. :contentReference[oaicite:2]{index=2}  
- **Hotel Booking**: Distance- and price-based filtering of hotel options, date-picker for check-in/check-out, multi-room support, and payment processing. :contentReference[oaicite:3]{index=3}  
- **Ticket Purchase**: Multiple ticket types, duration and quantity selection, shopping-cart checkout, and secure payment flow. :contentReference[oaicite:4]{index=4}  
- **User & Orders**: Registration/login system, order history retrieval, and detailed order views. :contentReference[oaicite:5]{index=5}  

## Technical Architecture & Implementation  
- **Global Configuration (`global.R`)**  
  - Loads required packages: `shiny`, `leaflet`, `tidyverse`, `DT`, `RPostgres`, `bslib`, `jsonlite`, etc.  
  - Establishes PostgreSQL connection and auto-creates `users` and `orders` tables if missing. :contentReference[oaicite:6]{index=6}  

- **User Interface (`ui.R`)**  
  - Uses `navbarPage` with `uiOutput()` for dynamic tab rendering.  
  - Organizes UI into distinct modules: Map, Attractions, Food, Hotels, Tickets, and Account. :contentReference[oaicite:7]{index=7}  

- **Server Logic (`server.R`)**  
  - Manages reactivity with reactive values and observers (`observeEvent`).  
  - Renders Leaflet map layers, handles form validation, triggers modal dialogs, and performs database reads/writes. :contentReference[oaicite:8]{index=8}  

## Installation & Usage  
1. **Clone the repository**  
   ```bash
   git clone https://github.com/yourusername/dino-world.git
   cd dino-world
````

2. **Install R package dependencies**

   ```r
   install.packages(c(
     "shiny", "leaflet", "tidyverse", "DT",
     "RPostgres", "bslib", "jsonlite"
   ))
   ```
3. **Configure database connection**

   * Edit the `dbConnect()` parameters in `global.R` (`host`, `port`, `user`, `password`, `dbname`) to match your PostgreSQL instance.
4. **Launch the application**

   ```r
   library(shiny)
   runApp()
   ```

## Project Structure

```
├── global.R       # Global dependencies & database initialization  
├── ui.R           # User interface definitions  
├── server.R       # Server-side logic  
├── www/           # Static assets (images, CSS, JS)  
└── README.md      # Project documentation  
```
