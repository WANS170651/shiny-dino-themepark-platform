[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)  

# Dino World 🦖  
**Prehistoric Adventure Theme Park Interactive App**

---

## 📋 Table of Contents  
1. [Overview](#overview)  
2. [Features](#features)  
3. [Architecture & Implementation](#architecture--implementation)  
   - [Global Configuration](#global-configuration)  
   - [User Interface](#user-interface)  
   - [Server Logic](#server-logic)  
4. [Getting Started](#getting-started)  
   - [Prerequisites](#prerequisites)  
   - [Installation](#installation)  
   - [Configuration](#configuration)  
   - [Launch](#launch)  
5. [Project Structure](#project-structure)  
6. [Team](#team)  
7. [Contact & License](#contact--license)  

---

## 🌟 Overview  
**Dino World** is an interactive **R Shiny** application simulating a full-service theme-park experience. Visitors can:  
- 🗺️ Explore an interactive map of rides, restaurants, and hotels  
- 🎢 View attraction details (height, duration, capacity)  
- 🍔 Order food online  
- 🏨 Book hotels with date-picker and multi-room support  
- 🎟️ Purchase tickets via a secure shopping-cart flow  
- 🔐 Register, log in, and track order history  

This demo app showcases modern web UI/UX, responsive design, and real-world operational workflows.

---

## 🚀 Features  
- **Home** 🏠: Branding banner, logo + tagline, “Buy Tickets” quick-start button  
- **Map Explorer** 🗺️: Dynamic **Leaflet** map with category filters and zoom controls  
- **Attraction Details** 🎡: Comprehensive ride/show info (name, type, requirements)  
- **Food Ordering** 🍕: Menu pop-ups, form validation, persisted orders  
- **Hotel Booking** 🏩: Distance/price filters, check-in/out picker, payment processing  
- **Ticket Purchase** 🎫: Select type, duration, quantity; cart and checkout flow  
- **User & Orders** 👤🛒: Secure signup/login, order history table with details  

---

## 🏗️ Architecture & Implementation

### ⚙️ Global Configuration (`global.R`)
- Loads packages: `shiny`, `leaflet`, `tidyverse`, `DT`, `RPostgres`, `bslib`, `jsonlite`  
- Connects to PostgreSQL and auto-creates `users` & `orders` tables if absent  

### 🖥️ User Interface (`ui.R`)
- Uses `navbarPage()` for responsive navigation  
- Renders modules via `uiOutput()` for:  
  - Map Explorer  
  - Attractions  
  - Food  
  - Hotels  
  - Tickets  
  - Account  

### 🔄 Server Logic (`server.R`)
- Manages reactive data with `reactiveValues()` and `observeEvent()`  
- Renders Leaflet layers, validates forms, triggers modals  
- Reads/writes to the database for orders and user actions  

---

## 🏁 Getting Started

### ✅ Prerequisites  
- **R** ≥ 4.0  
- **PostgreSQL** instance (e.g., DigitalOcean)  
- Internet connection for Leaflet tiles  

### 💾 Installation  
```bash
git clone https://github.com/yourusername/dino-world.git
cd dino-world
