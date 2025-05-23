# ui.R

ui <- navbarPage(
  
  id = 'tabs',
  windowTitle = 'Dino World',
  position = 'fixed-top',
  collapsible = TRUE,
  inverse = FALSE,
  theme = shinytheme('paper'), 
  
  uiOutput('uiBkgrd'),
  
  tabPanel(
    value = 1,
    title = 'Home',
    uiOutput('uiHome')
  ),
  
  tabPanel(
    value = 2,
    title = 'Map',
    uiOutput('uiMap')
  ),
  
  tabPanel(
    value = 3,
    title = 'Rides',
    uiOutput('uiRides')
  ),
  
  tabPanel(
    value = 4,
    title = 'Food',
    uiOutput('uiFood')
  ),
  
  tabPanel(
    value = 5,
    title = 'Lodging',
    uiOutput('uiLodging')
  ),
  
  tabPanel(
    value = 6,
    title = 'Tickets',
    uiOutput('uiTickets')
  ),

  tabPanel(
    value = 7,
    title = 'Account',
    uiOutput('uiAccount')
  ),

  tabPanel(
    value = 8,
    title = 'My Orders',
    uiOutput('uiOrders')
  ),

  tabPanel(
    value = 9,
    title = 'About Us',
    uiOutput('uiAbout')
  )
)
