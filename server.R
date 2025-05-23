# server.R

server = function(input, output, session) {
  
  # User session management
  user <- reactiveValues(logged_in = FALSE, user_id = NULL, username = NULL)
  
  # Set background based on page
  output$uiBkgrd = renderUI({
    pg = input$tabs
    
    # Don't show background for Account page (login/register)
    if (pg == "7") {
      return(NULL)
    }
    
    # Use bkgrd7 for About Us page
    if (pg == "9") {
      setBackgroundImage(src = 'bkgrd7.jpeg')
    } 
    # Use bkgrd1 for Home page
    else if (pg == "1") {
      setBackgroundImage(src = 'bkgrd1.png')
    }
    else {
      setBackgroundImage(src = paste0('bkgrd', pg, '.jpeg'))
    }
  })
  
  # Home Page
  output$uiHome = renderUI({
    div(
      style = 'padding:8vh 0 0 2%; text-align:left;',
      img(src = "dino_logo.png", height = "60px"),
      h1('Dino World', style = 'font-size:75px; font-weight:bold; color:#4CAF50;'),
      p('The Ultimate Prehistoric Adventure', style = 'font-size:35px; color:  #ffffff; font-weight:bold;'),
      p(HTML('Explore a prehistoric paradise filled with <br> Awe-inspiring dinosaurs | Thrilling rides | Immersive experiences'), 
        style = 'font-size:25px; color:#ffffff;font-weight:bold;'),
      br(),
      actionButton("gotoTickets", "Get Your Tickets Now!", class = "btn btn-success btn-lg")
    )
  })
  
  observeEvent(input$gotoTickets, {
    updateTabsetPanel(session, "tabs", selected = "5")
    updateTabsetPanel(session, "tabs", selected = "6")  
  })
  
  
  # Maps Page
  # Define attractions
  attractions <- data.frame(
    name = c(
      # Rides
      "T-Rex Terror Coaster", "VelociSplash Rapids", "PteroFlyer VR", 
      "Dino Discovery Safari", "Raptor Run Maze", "Jurassic Skyway",
      "Carnotaurus Crash Cars", "Mosasaurus Wave Pool", "Dino Egg Ferris Wheel",
      "Stego Slide Adventure",
      # Food
      "T-Rex Grill", "Triceratops Pizza", "Raptor Cafe", "Stego Snacks", "PteroSmoothies", "Jurassic Java",
      # Hotels
      "Dino Resort Hotel", "Jurassic Lodge", "Prehistoric Inn",
      "Fossil Flats Suites", "Paleo Park Hotel"
    ),
    type = c(
      rep("Ride", 10),
      rep("Food", 6),
      rep("Hotel", 5)
    ),
    lat = c(
      # Rides (39.7385 - 39.7405)
      39.7395, 39.7388, 39.7392, 39.7398, 39.7385, 39.7400,
      39.7390, 39.7386, 39.7393, 39.7397,
      # Food (39.7385 - 39.7405)
      39.7391, 39.7394, 39.7389, 39.7396, 39.7387, 39.7399,
      # Hotels (39.7400 - 39.7420)
      39.7400, 39.7405, 39.7410, 39.7415, 39.7420
    ),
    lng = c(
      # Rides (-104.9920 - -104.9880)
      -104.9902, -104.9910, -104.9905, -104.9895, -104.9915,
      -104.9885, -104.9900, -104.9890, -104.9888, -104.9892,
      # Food (-104.9920 - -104.9880)
      -104.9908, -104.9898, -104.9912, -104.9893, -104.9906, -104.9889,
      # Hotels (-104.9920 - -104.9880)
      -104.9895, -104.9905, -104.9915, -104.9885, -104.9890
    ),
    description = c(
      # Rides
      "The most thrilling dinosaur roller coaster ride!",
      "Get soaked while racing through prehistoric rapids!",
      "Experience flying with pterosaurs in virtual reality!",
      "Embark on a safari to discover living dinosaurs!",
      "Navigate through a maze while escaping from raptors!",
      "Soar above the park on a prehistoric skyway!",
      "Race through the park in dinosaur-themed bumper cars!",
      "Splash and play in our massive wave pool!",
      "Take a ride on our giant dinosaur egg Ferris wheel!",
      "Slide down the back of a giant stegosaurus!",
      # Food
      "Savor delicious grilled specialties in a T-Rex themed setting!",
      "Enjoy authentic Italian pizza with a prehistoric twist!",
      "Relax and refuel at our raptor-themed cafe!",
      "Quick bites and snacks for your dino adventure!",
      "Fresh smoothies and healthy drinks to keep you energized!",
      "Premium coffee and pastries in a Jurassic setting!",
      # Hotels
      "Luxury resort just steps away from the Dino World entrance.",
      "Experience the Jurassic era in our themed lodge.",
      "Comfortable accommodations with prehistoric decor.",
      "Modern suites with fossil-inspired designs.",
      "Family-friendly hotel with dinosaur-themed rooms."
    ),
    stringsAsFactors = FALSE
  )
  
  # Map UI
  output$uiMap = renderUI({
    div(
      style = 'padding:50px 5%; background: rgba(255,255,255,0.95); border-radius:20px;',
      
      h2("Explore Dino World", style = 'text-align:center; margin-bottom:20px; font-weight:bold; color:#4CAF50;'),
      
      checkboxGroupInput(
        "typeFilter",
        "Select categories to show:",
        choices = unique(attractions$type),
        selected = unique(attractions$type),
        inline = TRUE
      ),
      
      leafletOutput("parkMap", height = "600px")
    )
  })
  
  # Render the initial map
  output$parkMap = renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      setView(lng = -104.9903, lat = 39.7392, zoom = 15) %>%
      addAwesomeMarkers(
        lng = attractions$lng,
        lat = attractions$lat,
        label = attractions$name,
        popup = paste0("<b>", attractions$name, "</b><br>", attractions$description),
        icon = awesomeIcons(
          icon = ifelse(attractions$type == "Ride", "bolt", 
                        ifelse(attractions$type == "Food", "cutlery", "hotel")),
          iconColor = 'white',
          markerColor = ifelse(attractions$type == "Ride", "red",
                               ifelse(attractions$type == "Food", "green", "blue")),
          library = 'fa'
        )
      )
  })
  
  # Update markers based on user selection
  observe({
    req(input$typeFilter)
    
    filtered_data <- attractions %>%
      filter(type %in% input$typeFilter)
    
    leafletProxy("parkMap") %>%
      clearMarkers() %>%
      addAwesomeMarkers(
        lng = filtered_data$lng,
        lat = filtered_data$lat,
        label = filtered_data$name,
        popup = paste0("<b>", filtered_data$name, "</b><br>", filtered_data$description),
        icon = awesomeIcons(
          icon = ifelse(filtered_data$type == "Ride", "bolt", 
                        ifelse(filtered_data$type == "Food", "cutlery", "hotel")),
          iconColor = 'white',
          markerColor = ifelse(filtered_data$type == "Ride", "red",
                               ifelse(filtered_data$type == "Food", "green", "blue")),
          library = 'fa'
        )
      )
  })
  
  
  
  # Rides Page
  output$uiRides = renderUI({
    div(
      style = 'padding:50px 5%; background: rgba(255,255,255,0.9); border-radius:20px;',
      
      h2("Our Thrilling Rides", style = 'text-align:center; margin-bottom:10px; font-weight:bold; color:#3F51B5;'),
      
      div(
        style = 'text-align:center; margin-bottom:30px;',
        actionButton("fastPassBtn", "Buy Fast Pass", class = "btn btn-danger btn-lg")
      ),
      
      fluidRow(
        lapply(1:nrow(rides), function(i){
          column(4,
                 div(
                   style = "background:#f9f9f9; border-radius:15px; box-shadow:0px 4px 12px rgba(0,0,0,0.15); margin-bottom:25px; padding:20px; text-align:center; transition: transform 0.3s ease;",
                   
                   img(src = paste0('ride', i, '.png'), 
                       width = "100%", height = "250px",
                       style = "object-fit:cover; border-radius:15px 15px 0 0;"),
                   
                   h3(rides$ride_name[i], style="margin-top:15px; font-weight:bold; font-size:22px; color:#333;"),
                   tags$div(
                     style = "margin-top:10px; text-align:left; padding-left:10px;",
                     tags$p(strong("Type:"), rides$type[i]),
                     tags$p(strong("Min Height:"), paste0(rides$min_height[i], " inches")),
                     tags$p(strong("Duration:"), paste0(rides$duration_mins[i], " mins")),
                     tags$p(strong("Capacity:"), rides$capacity[i])
                   )
                 )
          )
        })
      )
    )
  })
  
  # Fast Pass按钮监听
  observeEvent(input$fastPassBtn, {
    shinyalert(
      title = "Fast Pass Available!",
      text = "Enjoy priority access to all rides. Visit the Tickets page to purchase your Fast Pass!",
      type = "info"
    )
  })
  
  
  
  # Food Page
  output$uiFood = renderUI({
    div(
      style = 'padding:50px 5%; background: rgba(255,255,255,0.95); border-radius:20px;',
      
      h2("Dino-Themed Dining", style = 'text-align:center; margin-bottom:30px; font-weight:bold; color:#FF5722;'),
      
      fluidRow(
        lapply(1:nrow(restaurants), function(i){
          column(4,
                 div(
                   style = "background:white; border-radius:10px; box-shadow:0px 4px 12px rgba(0,0,0,0.2); margin-bottom:25px; padding:15px; text-align:center;",
                   
                   img(src = paste0('food', i, '.jpg'), 
                       width = "100%", height = "250px", 
                       style = "object-fit:cover; border-radius:10px 10px 0 0;"),
                   
                   h4(restaurants$name[i], style="margin-top:10px; font-weight:bold;"),
                   p(paste0("Theme: ", restaurants$theme[i])),
                   
                   br(),
                   
                   actionButton(
                     inputId = paste0("viewMenu", i),
                     label = "View Menu",
                     class = "btn btn-warning btn-sm"
                   ),
                   
                   br(), br(),
                   
                   actionButton(
                     inputId = paste0("reserveTable", i),
                     label = "Reserve Table",
                     class = "btn btn-success btn-sm"
                   )
                 )
          )
        })
      )
    )
  })
  
  
  # 监听所有View Menu按钮
  observe({
    lapply(1:nrow(restaurants), function(i){
      observeEvent(input[[paste0("viewMenu", i)]], {
        
        selected_restaurant_id <- restaurants$restaurant_id[i]
        
        selected_menu <- menu_items %>%
          filter(restaurant_id == selected_restaurant_id) %>%
          select(item_name, price, dietary_flags)
        
        if(nrow(selected_menu) == 0){
          showModal(
            modalDialog(
              title = paste("Menu -", restaurants$name[i]),
              "No menu items available for this restaurant.",
              easyClose = TRUE,
              size = "m"
            )
          )
        } else {
          showModal(
            modalDialog(
              title = paste("Menu -", restaurants$name[i]),
              renderTable(selected_menu),
              easyClose = TRUE,
              size = "m"
            )
          )
        }
      })
    })
  })
  
  # Food Page - Modified reservation modal
  observe({
    lapply(1:nrow(restaurants), function(i){
      observeEvent(input[[paste0("reserveTable", i)]], {
        req(user$logged_in)
        
        showModal(
          modalDialog(
            title = paste("Reserve a Table at", restaurants$name[i]),
            size = "l",
            div(
              style = "padding:20px;",
            
              # Party Information
              h4("Party Information"),
            numericInput(
              inputId = paste0("partySize", i),
              label = "Number of Guests",
              value = 2, min = 1, max = 10
            ),
            
              # Date and Time
              h4("Date and Time"),
            dateInput(
              inputId = paste0("reserveDate", i),
              label = "Select Date",
              value = Sys.Date(),
              min = Sys.Date()
            ),
            
            selectInput(
              inputId = paste0("reserveTime", i),
              label = "Select Time",
              choices = c("11:00 AM", "12:00 PM", "1:00 PM", "5:00 PM", "6:00 PM", "7:00 PM")
            ),
            
              # Menu Selection
              h4("Menu Selection"),
              div(
                style = "max-height: 300px; overflow-y: auto;",
                lapply(1:nrow(menu_items), function(j) {
                  if (menu_items$restaurant_id[j] == restaurants$restaurant_id[i]) {
                    div(
                      style = "margin-bottom: 15px; padding: 10px; border: 1px solid #ddd; border-radius: 5px;",
                      h5(menu_items$item_name[j], style = "margin-top: 0;"),
                      p(paste("Price: $", menu_items$price[j])),
                      numericInput(
                        inputId = paste0("menuItemQty_", i, "_", j),
                        label = "Quantity",
                        value = 0,
                        min = 0,
                        max = 10,
                        width = "100px"
                      )
                    )
                  }
                })
              ),
              
              # Order Remarks
              h4("Order Remarks"),
              textAreaInput(
                inputId = paste0("foodRemarks", i),
                label = "Special Requests or Notes",
                placeholder = "Enter any special requests or notes here...",
                rows = 3
              ),
              
              # Payment Information
              h4("Payment Information"),
              textInput(
                inputId = paste0("cardName", i),
                label = "Name on Card",
                placeholder = "Full Name"
              ),
              textInput(
                inputId = paste0("cardNumber", i),
                label = "Card Number",
                placeholder = "1234 5678 9012 3456"
              ),
              textInput(
                inputId = paste0("cardExpiry", i),
                label = "Expiration Date (MM/YY)",
                placeholder = "08/26"
              ),
              textInput(
                inputId = paste0("cardCVV", i),
                label = "CVV",
                placeholder = "123"
              ),
              
              # Order Summary
              h4("Order Summary"),
              div(
                style = "background: #f8f9fa; padding: 15px; border-radius: 5px;",
                textOutput(paste0("subtotal", i)),
                textOutput(paste0("tax", i)),
                textOutput(paste0("total", i))
              )
            ),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(paste0("confirmFoodOrder", i), "Confirm Order", class = "btn btn-success")
            )
          )
        )
      })
      
      # Calculate food order totals
      output[[paste0("subtotal", i)]] <- renderText({
        subtotal <- 0
        for (j in 1:nrow(menu_items)) {
          if (menu_items$restaurant_id[j] == restaurants$restaurant_id[i]) {
            qty <- input[[paste0("menuItemQty_", i, "_", j)]]
            if (!is.null(qty) && qty > 0) {
              subtotal <- subtotal + (menu_items$price[j] * qty)
            }
          }
        }
        paste("Subtotal: $", sprintf("%.2f", subtotal))
      })
      
      output[[paste0("tax", i)]] <- renderText({
        subtotal <- 0
        for (j in 1:nrow(menu_items)) {
          if (menu_items$restaurant_id[j] == restaurants$restaurant_id[i]) {
            qty <- input[[paste0("menuItemQty_", i, "_", j)]]
            if (!is.null(qty) && qty > 0) {
              subtotal <- subtotal + (menu_items$price[j] * qty)
            }
          }
        }
        tax <- subtotal * 0.08875
        paste("Tax (8.875%): $", sprintf("%.2f", tax))
      })
      
      output[[paste0("total", i)]] <- renderText({
        subtotal <- 0
        for (j in 1:nrow(menu_items)) {
          if (menu_items$restaurant_id[j] == restaurants$restaurant_id[i]) {
            qty <- input[[paste0("menuItemQty_", i, "_", j)]]
            if (!is.null(qty) && qty > 0) {
              subtotal <- subtotal + (menu_items$price[j] * qty)
            }
          }
        }
        total <- subtotal * 1.08875
        paste("Total: $", sprintf("%.2f", total))
      })
      
      # Confirm food order
      observeEvent(input[[paste0("confirmFoodOrder", i)]], {
        req(user$logged_in)
        
        # Validate payment information
        if (nchar(input[[paste0("cardName", i)]]) == 0 ||
            nchar(input[[paste0("cardNumber", i)]]) < 16 ||
            nchar(input[[paste0("cardExpiry", i)]]) < 5 ||
            nchar(input[[paste0("cardCVV", i)]]) < 3) {
          shinyalert("Error", "Please fill in all payment information correctly.", type = "error")
          return()
        }
        
        # Calculate total and collect order details
        subtotal <- 0
        order_items <- list()
        for (j in 1:nrow(menu_items)) {
          if (menu_items$restaurant_id[j] == restaurants$restaurant_id[i]) {
            qty <- input[[paste0("menuItemQty_", i, "_", j)]]
            if (!is.null(qty) && qty > 0) {
              subtotal <- subtotal + (menu_items$price[j] * qty)
              order_items[[length(order_items) + 1]] <- list(
                item_name = menu_items$item_name[j],
                quantity = qty,
                price = menu_items$price[j]
              )
            }
          }
        }
        
        total_price <- subtotal * 1.08875
        
        # Save order details
        details <- list(
          restaurant_name = restaurants$name[i],
          party_size = input[[paste0("partySize", i)]],
          date = input[[paste0("reserveDate", i)]],
          time = input[[paste0("reserveTime", i)]],
          order_items = order_items,
          subtotal = subtotal,
          tax = subtotal * 0.08875,
          total_price = total_price,
          remarks = ifelse(nchar(input[[paste0("foodRemarks", i)]]) > 0, 
                          input[[paste0("foodRemarks", i)]], 
                          "N/A")
        )
        
        # Save to database
        dbExecute(con,
          "INSERT INTO orders (user_id, order_type, item_id, quantity, total_price, details) 
           VALUES ($1, 'food', $2, $3, $4, $5)",
          params = list(
            user$user_id,
            restaurants$restaurant_id[i],
            input[[paste0("partySize", i)]],
            total_price,
            toJSON(details)
          )
        )
        
        shinyalert("Success!", "Your food order has been confirmed and saved!", type = "success")
        removeModal()
        updateTabsetPanel(session, "tabs", selected = "8")  # Changed from 9 to 8
      })
    })
  })
  
  
  
  # Lodging Page
  output$uiLodging = renderUI({
    div(
      style = 'padding:50px 5%; background: rgba(255,255,255,0.6); border-radius:20px;',
      
      h2("Stay Near Dino World", style = 'text-align:center; margin-bottom:30px; font-weight:bold; color:#4CAF50;'),
      
fluidRow(
  column(6,
         wellPanel(
           h4(tagList(icon("map-marker-alt"), "Max Distance (miles)")),
           
           radioButtons(
             inputId = "distancePreset",
             label = "Quick Select:",
             choices = c("≤ 1 mile" = 1,
                         "≤ 2 miles" = 2,
                         "≤ 5 miles" = 5,
                         "No Limit" = Inf),
             selected = Inf,
             inline = TRUE
           ),
           
           numericInput(
             inputId = "distanceFilter",
             label = "Or enter your own maximum distance:",
             value = NA,
             min = 0,
             step = 0.1
           )
         )
  ),
  column(6,
         wellPanel(
           h4(tagList(icon("dollar-sign"), "Max Nightly Rate ($)")),
           
           radioButtons(
             inputId = "pricePreset",
             label = "Quick Select:",
             choices = c("≤ $100" = 100,
                         "≤ $150" = 150,
                         "≤ $200" = 200,
                         "No Limit" = Inf),
             selected = Inf,
             inline = TRUE
           ),
           
           numericInput(
             inputId = "priceFilter",
             label = "Or enter your own maximum price ($):",
             value = NA,
             min = 0,
             step = 1
           )
         )
  )
),
      
      br(),
      
      uiOutput("hotelCards")
    )
  })

## Hotel Cards  
output$hotelCards = renderUI({
  
  # Determine the value of the final screening
  distance_value <- if (!is.na(input$distanceFilter)) input$distanceFilter else input$distancePreset
  price_value <- if (!is.na(input$priceFilter)) input$priceFilter else input$pricePreset

  # Filter hotels by final screening criteria
  filtered_hotels <- hotels %>%
    filter(
      distance_miles <= distance_value,
      nightly_rate <= as.numeric(price_value)  # Ensure price_value is treated as numeric
    )
  
  if (nrow(filtered_hotels) == 0) {
    return(
      h4("No hotels match your filters.", style = "text-align:center; margin-top:30px; color:#888;")
    )
  }
  
  # Sort hotels by distance
  filtered_hotels <- filtered_hotels %>%
    arrange(distance_miles)
  
  fluidRow(
    lapply(1:nrow(filtered_hotels), function(i) {
      hotel <- filtered_hotels[i, ]
      column(4,
             div(
               style = "background: rgba(255, 255, 255, 0.8); border-radius:10px; box-shadow:0px 4px 12px rgba(0,0,0,0.2); margin-bottom:25px; padding:15px; text-align:center;",
               
               img(src = paste0('hotel', hotel$hotel_id, '.jpg'), 
                   width = "100%", height = "250px", 
                   style = "object-fit:cover; border-radius:10px 10px 0 0;"),
               
               h4(hotel$hotel_name, style = "margin-top:10px; font-weight:bold;"),
               p(paste0("Distance: ", hotel$distance_miles, " miles")),
               p(paste0("Nightly Rate: $", hotel$nightly_rate)),
               
               br(),
               
               actionButton(
                 inputId = paste0("bookHotel_", hotel$hotel_id),
                 label = "Book Now",
                 class = "btn btn-success btn-sm"
               )
             )
      )
    })
  )
})
  
  # Initialize a reactiveValues object to store the hotels
  rv <- reactiveValues(filtered_hotels = NULL)
  
  # Lodging Page - Modified booking modal
  observe({
    lapply(1:nrow(hotels), function(i){
      observeEvent(input[[paste0("bookHotel_", i)]], {
        req(user$logged_in)
        
        showModal(
          modalDialog(
            title = paste("Book a Room at", hotels$hotel_name[i]),
            size = "l",
            div(
              style = "padding:20px;",
              
              # Stay Information
              h4("Stay Information"),
              dateInput(
                inputId = paste0("checkInDate", i),
                label = "Check-in Date",
                value = Sys.Date(),
                min = Sys.Date()
              ),
              dateInput(
                inputId = paste0("checkOutDate", i),
                label = "Check-out Date",
                value = Sys.Date() + 1,
                min = Sys.Date() + 1
              ),
              numericInput(
                inputId = paste0("numGuests", i),
                label = "Number of Guests",
                value = 2, min = 1, max = 4
              ),
              
              # Room Selection
              h4("Room Selection"),
              selectInput(
                inputId = paste0("roomType", i),
                label = "Room Type",
                choices = c("Standard", "Deluxe", "Suite")
              ),
              
              # Order Remarks
              h4("Order Remarks"),
              textAreaInput(
                inputId = paste0("hotelRemarks", i),
                label = "Special Requests or Notes",
                placeholder = "Enter any special requests or notes here...",
                rows = 3
              ),
              
              # Payment Information
              h4("Payment Information"),
              textInput(
                inputId = paste0("hotelCardName", i),
                label = "Name on Card",
                placeholder = "Full Name"
              ),
              textInput(
                inputId = paste0("hotelCardNumber", i),
                label = "Card Number",
                placeholder = "1234 5678 9012 3456"
              ),
              textInput(
                inputId = paste0("hotelCardExpiry", i),
                label = "Expiration Date (MM/YY)",
                placeholder = "08/26"
              ),
              textInput(
                inputId = paste0("hotelCardCVV", i),
                label = "CVV",
                placeholder = "123"
              ),
              
              # Order Summary
              h4("Order Summary"),
              textOutput(paste0("hotelTotal", i))
            ),
            footer = tagList(
              modalButton("Cancel"),
              actionButton(paste0("confirmHotelBooking", i), "Confirm Booking", class = "btn btn-success")
            )
          )
        )
      })
      
      # Calculate hotel booking total
      output[[paste0("hotelTotal", i)]] <- renderText({
        nights <- as.numeric(difftime(input[[paste0("checkOutDate", i)]], 
                                    input[[paste0("checkInDate", i)]], 
                                    units = "days"))
        subtotal <- hotels$nightly_rate[i] * nights
        tax <- subtotal * 0.08875  # 8.875% tax
        total <- subtotal + tax
        HTML(paste0(
          "Subtotal: $", sprintf("%.2f", subtotal), "<br>",
          "Tax (8.875%): $", sprintf("%.2f", tax), "<br>",
          "Total: $", sprintf("%.2f", total)
        ))
      })
      
      # Confirm hotel booking
      observeEvent(input[[paste0("confirmHotelBooking", i)]], {
        req(user$logged_in)
        
        tryCatch({
          # Validate payment information
          if (nchar(input[[paste0("hotelCardName", i)]]) == 0 ||
              nchar(input[[paste0("hotelCardNumber", i)]]) < 16 ||
              nchar(input[[paste0("hotelCardExpiry", i)]]) < 5 ||
              nchar(input[[paste0("hotelCardCVV", i)]]) < 3) {
            shinyalert("Error", "Please fill in all payment information correctly.", type = "error")
            return()
          }
          
          # Calculate total with tax
          nights <- as.numeric(difftime(input[[paste0("checkOutDate", i)]], 
                                      input[[paste0("checkInDate", i)]], 
                                      units = "days"))
          subtotal <- hotels$nightly_rate[i] * nights
          tax <- subtotal * 0.08875  # 8.875% tax
          total_price <- subtotal + tax
          
          # Save order details
          details <- list(
            hotel_name = hotels$hotel_name[i],
            room_type = input[[paste0("roomType", i)]],
            check_in = input[[paste0("checkInDate", i)]],
            check_out = input[[paste0("checkOutDate", i)]],
            guests = input[[paste0("numGuests", i)]],
            subtotal = subtotal,
            tax = tax,
            total_price = total_price,
            remarks = ifelse(nchar(input[[paste0("hotelRemarks", i)]]) > 0, 
                           input[[paste0("hotelRemarks", i)]], 
                           "N/A")
          )
          
          # Save to database
          dbExecute(con,
            "INSERT INTO orders (user_id, order_type, item_id, quantity, total_price, details) 
             VALUES ($1, 'hotel', $2, $3, $4, $5)",
            params = list(
              user$user_id,
              hotels$hotel_id[i],
              input[[paste0("numGuests", i)]],
              total_price,
              toJSON(details)
            )
          )
          
          shinyalert("Success!", "Your hotel booking has been confirmed and saved!", type = "success")
          removeModal()
          updateTabsetPanel(session, "tabs", selected = "8")  # Changed from 9 to 8
        }, error = function(e) {
          shinyalert("Error", paste("Booking failed:", e$message), type = "error")
        })
      })
    })
  })
  

  
  # Tickets Page
  # Create reactive value to track page state
  pageState <- reactiveVal("cart") 
  
  # Quantities
  quantities <- reactiveValues()
  
  observe({
    lapply(1:nrow(tickets), function(i){
      quantities[[paste0("q", i)]] <- 0
    })
  })
  
  lapply(1:nrow(tickets), function(i){
    observeEvent(input[[paste0("plus_", i)]], {
      quantities[[paste0("q", i)]] <- quantities[[paste0("q", i)]] + 1
    })
    observeEvent(input[[paste0("minus_", i)]], {
      if (quantities[[paste0("q", i)]] > 0) {
        quantities[[paste0("q", i)]] <- quantities[[paste0("q", i)]] - 1
      }
    })
    output[[paste0("quantity_", i)]] <- renderText({
      quantities[[paste0("q", i)]]
    })
  })
  
  # Subtotal / tax / grand total
  output$subtotalText <- renderText({
    req(input$daysVisit)
    subtotal = 0
    for (i in 1:nrow(tickets)) {
      if (tickets$ticket_type[i] == "Annual Pass") {
        subtotal <- subtotal + quantities[[paste0("q", i)]] * tickets$price[i]
      } else {
        subtotal <- subtotal + quantities[[paste0("q", i)]] * tickets$price[i] * as.numeric(input$daysVisit)
      }
    }
    paste0("Subtotal: $", round(subtotal,2))
  })
  
  output$taxText <- renderText({
    req(input$daysVisit)
    subtotal = 0
    for (i in 1:nrow(tickets)) {
      if (tickets$ticket_type[i] == "Annual Pass") {
        subtotal <- subtotal + quantities[[paste0("q", i)]] * tickets$price[i]
      } else {
        subtotal <- subtotal + quantities[[paste0("q", i)]] * tickets$price[i] * as.numeric(input$daysVisit)
      }
    }
    tax = subtotal * 0.08875
    paste0("Tax (8.875%): $", round(tax,2))
  })
  
  output$grandTotalText <- renderText({
    req(input$daysVisit)
    subtotal = 0
    for (i in 1:nrow(tickets)) {
      if (tickets$ticket_type[i] == "Annual Pass") {
        subtotal <- subtotal + quantities[[paste0("q", i)]] * tickets$price[i]
      } else {
        subtotal <- subtotal + quantities[[paste0("q", i)]] * tickets$price[i] * as.numeric(input$daysVisit)
      }
    }
    grand_total = subtotal * 1.08875
    paste0("Grand Total: $", round(grand_total,2))
  })
  
  # Tickets Page
  output$uiTickets = renderUI({
    div(
      style = 'padding:0; margin:0;',
      
      div(
        style = "background-image: url('banner.jpg'); background-size: cover; background-position: center; height: 220px; display: flex; align-items: center; justify-content: center;",
        h1("Plan Your Dino Adventure", style = "color:white; font-weight:bold; font-size:50px; text-shadow:2px 2px 8px rgba(0,0,0,0.5);")
      ),
      
      br(),
      
      div(
        style = 'padding:0 5%;',
        
        fluidRow(
          column(8,
                 
                 if (pageState() == "cart") {
                   div(
                     style = "background: white; padding:20px; border-radius:15px; box-shadow:0px 6px 12px rgba(0,0,0,0.15); margin-bottom:40px;",
                     h3("Select Your Visit Details", style = "font-weight:bold; text-align:center; margin-bottom:20px;"),
                     fluidRow(
                       column(6,
                              div(style="text-align:center;",
                                  dateInput("visitDate", "Choose Visit Date:", min = Sys.Date(), width="80%")
                              )
                       ),
                       column(6,
                              div(style="text-align:center;",
                                  numericInput("daysVisit", "How Many Days?", value = 1, min = 1, step = 1, width="80%")
                              )
                       )
                     )
                   )
                 },
                 
                 if (pageState() == "cart") {
                   fluidRow(
                     lapply(1:nrow(tickets), function(i) {
                       column(6,
                              div(
                                style = "background:white; padding:20px; border-radius:15px; margin:15px; min-height:300px; text-align:center;
                                     box-shadow:0px 4px 12px rgba(0,0,0,0.1);
                                     transition: transform 0.3s, box-shadow 0.3s;",
                                onmouseover = "this.style.transform='translateY(-5px)'; this.style.boxShadow='0px 12px 20px rgba(0,0,0,0.2)';",
                                onmouseout = "this.style.transform='translateY(0)'; this.style.boxShadow='0px 4px 12px rgba(0,0,0,0.1)';",
                                
                                h4(tickets$ticket_type[i], style = "font-weight:bold; margin-bottom:10px; color:#222;"),
                                p(c(
                                  "Perfect for adults seeking a thrilling day among dinosaurs.",
                                  "Special adventures tailored for young explorers!",
                                  "Relax and enjoy timeless dinosaur experiences.",
                                  "Exclusive VIP access to premium attractions.",
                                  "Fun-filled dinosaur adventures for the family!",
                                  "Discounted rates for student dinosaur lovers.",
                                  "Perfect for groups seeking a roaring good time!",
                                  "Enjoy unlimited dino fun all year long!"
                                )[i], style = "color:#555; font-size:14px; margin-bottom:20px;"),
                                
                                h2(paste0("$", tickets$price[i]), style = "color:#4CAF50; font-weight:bold; margin-bottom:20px; font-size:30px;"),
                                
                                div(style = "display:flex; justify-content:center; align-items:center;",
                                    actionButton(paste0("minus_", i), "-", class = "btn btn-outline-secondary btn-sm", style = "width:40px; margin:5px;"),
                                    div(textOutput(paste0("quantity_", i), inline = TRUE), style = "margin:5px; font-size:18px; width:30px;"),
                                    actionButton(paste0("plus_", i), "+", class = "btn btn-outline-secondary btn-sm", style = "width:40px; margin:5px;")
                                )
                              )
                       )
                     })
                   )
                 },
                 
                 if (pageState() == "checkout") {
                   div(
                     style = "background:white; padding:30px; border-radius:15px; box-shadow:0px 6px 12px rgba(0,0,0,0.2); margin-bottom:20px;",
                     h3("Payment Information", style = 'font-weight:bold; text-align:center; margin-bottom:30px; color:#222;'),
                     
                     div(style="width:80%; margin:0 auto;",
                         
                         fluidRow(
                           column(6, textInput("cardName", "Name on Card:", placeholder = "Full Name")),
                           column(6, textInput("cardNumber", "Card Number:", placeholder = "1234 5678 9012 3456"))
                         ),
                         fluidRow(
                           column(6, textInput("cardExpiry", "Expiration Date (MM/YY):", placeholder = "08/26")),
                           column(6, textInput("cardCVV", "CVV:", placeholder = "123"))
                         ),
                         br(),
                         
                         actionButton("confirmPurchase", "Confirm and Pay", class = "btn btn-success btn-lg", style = "width:100%; font-size:18px; margin-bottom:15px;"),
                         actionButton("backToCart", "Back to Shopping Cart", class = "btn btn-outline-primary btn-lg", style = "width:100%; font-size:18px;")
                     )
                   )
                 }
          ),
          
          column(4,
                 div(
                   style = "background:white; padding:30px; border-radius:15px; box-shadow:0px 6px 12px rgba(0,0,0,0.2); position:sticky; top:120px;",
                   
                   h3("Order Summary", style = "font-weight:bold; text-align:center; margin-bottom:20px;"),
                   div(style = "font-size:18px; text-align:center; color:#555;",
                       textOutput("subtotalText"),
                       textOutput("taxText"),
                       textOutput("grandTotalText")
                   ),
                   br(),
                   div(style = "text-align:center;",
                       actionButton("proceedCheckout", "Proceed to Checkout", class = "btn btn-primary btn-lg", style = "width:80%; font-size:18px;")
                   )
                 )
          )
        )
      )
    )
  })
  
  # Proceed Checkout
  observeEvent(input$proceedCheckout, {
    req(user$logged_in)
    
    showModal(
      modalDialog(
        title = "Complete Your Purchase",
        size = "l",
        div(
          style = "padding:20px;",
          
          # Order Summary
          h4("Order Summary"),
          textOutput("orderSubtotal"),
          textOutput("orderTax"),
          textOutput("orderTotal"),
          
          # Order Remarks
          h4("Order Remarks"),
          textAreaInput(
            inputId = "ticketRemarks",
            label = "Special Requests or Notes",
            placeholder = "Enter any special requests or notes here...",
            rows = 3
          ),
          
          # Payment Information
          h4("Payment Information"),
          textInput("cardName", "Name on Card", placeholder = "Full Name"),
          textInput("cardNumber", "Card Number", placeholder = "1234 5678 9012 3456"),
          textInput("cardExpiry", "Expiration Date (MM/YY)", placeholder = "08/26"),
          textInput("cardCVV", "CVV", placeholder = "123")
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirmPurchase", "Complete Purchase", class = "btn btn-success")
        )
      )
    )
  })
  
  # Calculate ticket order totals
  output$orderSubtotal <- renderText({
    subtotal <- 0
    for (i in 1:nrow(tickets)) {
      if (quantities[[paste0("q", i)]] > 0) {
        price <- if (tickets$ticket_type[i] == "Annual Pass") {
          tickets$price[i] * quantities[[paste0("q", i)]]
        } else {
          tickets$price[i] * quantities[[paste0("q", i)]] * as.numeric(input$daysVisit)
        }
        subtotal <- subtotal + price
      }
    }
    paste("Subtotal: $", sprintf("%.2f", subtotal))
  })

  output$orderTax <- renderText({
    subtotal <- 0
    for (i in 1:nrow(tickets)) {
      if (quantities[[paste0("q", i)]] > 0) {
        price <- if (tickets$ticket_type[i] == "Annual Pass") {
          tickets$price[i] * quantities[[paste0("q", i)]]
        } else {
          tickets$price[i] * quantities[[paste0("q", i)]] * as.numeric(input$daysVisit)
        }
        subtotal <- subtotal + price
      }
    }
    tax <- subtotal * 0.08875
    paste("Tax (8.875%): $", sprintf("%.2f", tax))
  })

  output$orderTotal <- renderText({
    subtotal <- 0
    for (i in 1:nrow(tickets)) {
      if (quantities[[paste0("q", i)]] > 0) {
        price <- if (tickets$ticket_type[i] == "Annual Pass") {
          tickets$price[i] * quantities[[paste0("q", i)]]
        } else {
          tickets$price[i] * quantities[[paste0("q", i)]] * as.numeric(input$daysVisit)
        }
        subtotal <- subtotal + price
      }
    }
    total <- subtotal * 1.08875
    paste("Total: $", sprintf("%.2f", total))
  })

  # Confirm ticket purchase with error handling
  observeEvent(input$confirmPurchase, {
    req(user$logged_in)
    
    tryCatch({
      # Validate payment information
      if (nchar(input$cardName) == 0 ||
          nchar(input$cardNumber) < 16 ||
          nchar(input$cardExpiry) < 5 ||
          nchar(input$cardCVV) < 3) {
        shinyalert("Error", "Please fill in all payment information correctly.", type = "error")
        return()
      }
      
      total_price <- 0
      for (i in 1:nrow(tickets)) {
        if (quantities[[paste0("q", i)]] > 0) {
          price <- if (tickets$ticket_type[i] == "Annual Pass") {
            tickets$price[i] * quantities[[paste0("q", i)]]
          } else {
            tickets$price[i] * quantities[[paste0("q", i)]] * as.numeric(input$daysVisit)
          }
          
          subtotal <- price
          tax <- subtotal * 0.08875  # 8.875% tax
          total <- subtotal + tax
          
          details <- list(
            ticket_type = tickets$ticket_type[i],
            visit_date = input$visitDate,
            quantity = quantities[[paste0("q", i)]],
            price_per_ticket = tickets$price[i],
            subtotal = subtotal,
            tax = tax,
            total_price = total,
            remarks = ifelse(nchar(input$ticketRemarks) > 0, 
                           input$ticketRemarks, 
                           "N/A")
          )
          
          dbExecute(con,
            "INSERT INTO orders (user_id, order_type, item_id, quantity, total_price, details) 
             VALUES ($1, 'ticket', $2, $3, $4, $5)",
            params = list(
              user$user_id,
              tickets$ticket_id[i],
              quantities[[paste0("q", i)]],
              total,
              toJSON(details)
            )
          )
          
          total_price <- total_price + total
        }
      }
      
      shinyalert("Success!", "Your tickets have been purchased and saved!", type = "success")
      removeModal()
      updateTabsetPanel(session, "tabs", selected = "8")  # Changed from 9 to 8
    }, error = function(e) {
      shinyalert("Error", paste("Purchase failed:", e$message), type = "error")
    })
  })
  
  
  # About Us Page
  output$uiAbout = renderUI({
    div(
      style = 'padding:50px 8%; background: rgba(255,255,255,0.6); border-radius:20px;',
      
      h2("About Dino World", style = 'text-align:center; margin-bottom:30px; font-weight:bold; color:#4CAF50;'),
      
      p("Welcome to Dino World! Our team of passionate visionaries, designers, engineers, and dreamers came together with one goal: to create the ultimate prehistoric adventure. 
      We blend cutting-edge technology, immersive storytelling, and love for dinosaurs to deliver unforgettable experiences for visitors of all ages.",
        style = "font-size:18px; text-align:center; margin-bottom:50px; color:#262525;"),
      
      h3("Meet Our Amazing Team", style = 'text-align:center; margin-bottom:30px; font-weight:bold; color:#3F51B5;'),
      
      fluidRow(
        lapply(1:length(mems), function(i) {
          column(
            width = 4,
            div(
              style = "background: rgba(255,255,255,0.8); border-radius:20px; box-shadow:0px 4px 10px rgba(0,0,0,0.1); margin:15px; padding:20px; text-align:center;",
              
              img(src = paste0('member', i, '.jpg'), 
                  width = "150px", height = "150px", 
                  style = "border-radius:50%; object-fit:cover; margin-bottom:15px; box-shadow:0px 2px 6px rgba(0,0,0,0.2);"),
              
              h4(mems[i], style="font-weight:bold; color:#333;"),
              
              p(
                case_when(
                  mems[i] == "Chenxiao Lu" ~ "Creative Director leading immersive dinosaur experiences.",
                  mems[i] == "Yuehan Wang" ~ "Architect specializing in theme park design and layout.",
                  mems[i] == "Jinming Liu" ~ "Chief Engineer mastering animatronics and virtual rides.",
                  mems[i] == "Yuqi Wang" ~ "Project Manager ensuring seamless park operations.",
                  mems[i] == "Teng Lin" ~ "Data Analyst optimizing visitor experiences and business intelligence.",
                  mems[i] == "Zhe Li" ~ "Marketing Strategist spreading Dino World magic worldwide.",
                  mems[i] == "Xiang Liu" ~ "Logistics and Maintenance Manager, ensuring safety and fun.",
                  TRUE ~ ""
                ),
                style="font-size:14px; color:#666; margin-top:10px;"
              )
            )
          )
        })
      )
    )
  })
  
  # Account Management UI
  output$uiAccount = renderUI({
    if (!user$logged_in) {
      div(
        style = 'padding:50px 5%; background: rgba(255,255,255,0.95); border-radius:20px;',
        
        tabsetPanel(
          tabPanel("Login",
            div(
              style = 'max-width:400px; margin:0 auto;',
              textInput("loginUsername", "Username"),
              passwordInput("loginPassword", "Password"),
              actionButton("loginBtn", "Login", class = "btn btn-primary"),
              br(), br(),
              p("Don't have an account? Register below!")
            )
          ),
          tabPanel("Register",
            div(
              style = 'max-width:400px; margin:0 auto;',
              textInput("regUsername", "Username"),
              passwordInput("regPassword", "Password"),
              textInput("regEmail", "Email"),
              textInput("regFullName", "Full Name"),
              actionButton("registerBtn", "Register", class = "btn btn-success")
            )
          )
        )
      )
    } else {
      div(
        style = 'padding:50px 5%; background: rgba(255,255,255,0.95); border-radius:20px;',
        h3(paste("Welcome,", user$username)),
        p("You are logged in!"),
        actionButton("logoutBtn", "Logout", class = "btn btn-danger")
      )
    }
  })

  # Registration functionality
  observeEvent(input$registerBtn, {
    req(input$regUsername, input$regPassword, input$regEmail, input$regFullName)
    
    # Validate input fields
    if (nchar(input$regUsername) < 3) {
      shinyalert("Error", "Username must be at least 3 characters long.", type = "error")
      return()
    }
    if (nchar(input$regPassword) < 6) {
      shinyalert("Error", "Password must be at least 6 characters long.", type = "error")
      return()
    }
    if (!grepl("@", input$regEmail)) {
      shinyalert("Error", "Please enter a valid email address.", type = "error")
      return()
    }
    
    tryCatch({
      # Check if username already exists
      existing_user <- dbGetQuery(con,
        "SELECT username FROM users WHERE username = $1",
        params = list(input$regUsername)
      )
      
      if (nrow(existing_user) > 0) {
        shinyalert("Error", "Username already exists. Please choose another one.", type = "error")
        return()
      }
      
      # Check if email already exists
      existing_email <- dbGetQuery(con,
        "SELECT email FROM users WHERE email = $1",
        params = list(input$regEmail)
      )
      
      if (nrow(existing_email) > 0) {
        shinyalert("Error", "Email already registered. Please use another email.", type = "error")
        return()
      }
      
      # Insert new user
      dbExecute(con,
        "INSERT INTO users (username, password, email, full_name) VALUES ($1, $2, $3, $4)",
        params = list(input$regUsername, input$regPassword, input$regEmail, input$regFullName)
      )
      
      # Verify the user was created
      new_user <- dbGetQuery(con,
        "SELECT user_id, username FROM users WHERE username = $1",
        params = list(input$regUsername)
      )
      
      if (nrow(new_user) > 0) {
        shinyalert("Success!", "Registration successful! Please login.", type = "success")
        # Clear registration form
        updateTextInput(session, "regUsername", value = "")
        updateTextInput(session, "regPassword", value = "")
        updateTextInput(session, "regEmail", value = "")
        updateTextInput(session, "regFullName", value = "")
      } else {
        shinyalert("Error", "Registration failed. Please try again.", type = "error")
      }
    }, error = function(e) {
      shinyalert("Error", paste("Registration failed:", e$message), type = "error")
    })
  })

  # Login functionality
  observeEvent(input$loginBtn, {
    req(input$loginUsername, input$loginPassword)
    
    tryCatch({
      # Get user data with error handling
      user_data <- dbGetQuery(con, 
        "SELECT user_id, username FROM users WHERE username = $1 AND password = $2",
        params = list(input$loginUsername, input$loginPassword)
      )
      
      if (nrow(user_data) > 0) {
        user$logged_in <- TRUE
        user$user_id <- user_data$user_id[1]
        user$username <- user_data$username[1]
        
        # Clear login form
        updateTextInput(session, "loginUsername", value = "")
        updateTextInput(session, "loginPassword", value = "")
        
        shinyalert("Success!", "You have been logged in.", type = "success")
      } else {
        shinyalert("Error", "Invalid username or password.", type = "error")
      }
    }, error = function(e) {
      shinyalert("Error", paste("Login failed:", e$message), type = "error")
    })
  })

  # Logout functionality
  observeEvent(input$logoutBtn, {
    user$logged_in <- FALSE
    user$user_id <- NULL
    user$username <- NULL
    shinyalert("Logged Out", "You have been logged out.", type = "info")
  })

  # Add a reactive value to track order updates
  orderUpdateTrigger <- reactiveVal(0)
  
  # Orders UI
  output$uiOrders = renderUI({
    if (!user$logged_in) {
      return(div(
        style = 'padding:50px 5%; text-align:center;',
        h3("Please login to view your orders")
      ))
    }
    
    # Trigger refresh when orders are updated
    orderUpdateTrigger()
    
    orders <- dbGetQuery(con,
      "SELECT * FROM orders WHERE user_id = $1 ORDER BY order_date DESC",
      params = list(user$user_id)
    )
    
    if (nrow(orders) == 0) {
      return(div(
        style = 'padding:50px 5%; text-align:center;',
        h3("You have no orders yet")
      ))
    }
    
    div(
      style = 'padding:50px 5%; background: rgba(255,255,255,0.95); border-radius:20px;',
      h3("Your Orders"),
      DTOutput("ordersTable")
    )
  })

  # Orders table
  output$ordersTable = renderDT({
    req(user$logged_in)
    
    # Trigger refresh when orders are updated
    orderUpdateTrigger()
    
    orders <- dbGetQuery(con,
      "SELECT * FROM orders WHERE user_id = $1 ORDER BY order_date DESC",
      params = list(user$user_id)
    )
    
    # Format the orders for display
    formatted_orders <- orders %>%
      mutate(
        order_type = case_when(
          order_type == "food" ~ "Restaurant Reservation",
          order_type == "hotel" ~ "Hotel Booking",
          order_type == "ticket" ~ "Park Ticket"
        ),
        order_date = format(as.POSIXct(order_date), "%Y-%m-%d %H:%M:%S"),
        # Extract subtotal and tax from details if available
        subtotal = sapply(details, function(x) {
          det <- fromJSON(x)
          if ("subtotal" %in% names(det)) det$subtotal else NA
        }),
        tax = sapply(details, function(x) {
          det <- fromJSON(x)
          if ("tax" %in% names(det)) det$tax else NA
        }),
        # If subtotal and tax are available, use them, otherwise use total_price
        total_price = ifelse(!is.na(subtotal) & !is.na(tax), 
                           subtotal + tax,
                           total_price)
      ) %>%
      select(order_type, quantity, total_price, order_date) %>%
      mutate(total_price = sprintf("$%.2f", total_price))
    
    datatable(
      formatted_orders,
      options = list(
        pageLength = 10
      )
    )
  })

  # Update the orderUpdateTrigger when a new order is placed
  observeEvent(input$confirmPurchase, {
    orderUpdateTrigger(orderUpdateTrigger() + 1)
  })

  # Update the orderUpdateTrigger when a food order is confirmed
  observe({
    lapply(1:nrow(restaurants), function(i){
      observeEvent(input[[paste0("confirmFoodOrder", i)]], {
        orderUpdateTrigger(orderUpdateTrigger() + 1)
      })
    })
  })

  # Update the orderUpdateTrigger when a hotel booking is confirmed
  observe({
    lapply(1:nrow(hotels), function(i){
      observeEvent(input[[paste0("confirmHotelBooking", i)]], {
        orderUpdateTrigger(orderUpdateTrigger() + 1)
      })
    })
  })
}
