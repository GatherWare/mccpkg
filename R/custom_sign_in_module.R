
#' custom_sign_in_page
#' 
#' @export
#'
#' @importFrom htmltools tags tagList
#' 
custom_sign_in_page <- function() {
  polished::sign_in_ui_default(
    sign_in_module = polished::sign_in_module_ui(
      "sign_in", 
      register_link= NULL,
      password_reset_link = NULL
    ), 
    color = "#1d5899",
    company_name = "GatherWare, Inc.",
    logo_top = htmltools::tagList(
      tags$img(
        src = "images/moffitt-logo.svg",
        alt = "Moffitt Cancer Center Logo",
        style = "margin-top: 50px; margin-bottom: 25px; width: 400px;"
      )
      , tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/three.js/r121/three.min.js")
      , tags$script(src="https://cdn.jsdelivr.net/npm/vanta@latest/dist/vanta.net.min.js")
      , tags$script('
          VANTA.NET({
            el: "body",
            mouseControls: true,
            touchControls: true,
            gyroControls: false,
            minHeight: 200.00,
            minWidth: 200.00,
            scale: 1.00,
            scaleMobile: 1.00,
            color: "#58afe2",
            backgroundColor: "#FFF"
          })
        ')
    )
    , logo_bottom = tags$img(
      src = "mcc/images/orion_logo_small.png",
      alt = "Orion Logo",
      style = "width: 150px; margin-top: 30px; margin-bottom: 30px;"
    )
    , icon_href = "mcc/images/orion_logo_small.png"
  )
}

# custom_sign_in_module_ui <- function(id) {
#   ns <- NS(id)
# 
#   icon_href <- "img/logos/orion_logo_small.png"
#   company_name <- "Gatherware, Inc."
#   color <- "#006CB5"
#   button_color <- color
#   logo_top <- tags$img(
#     src = "img/logos/moffitt-logo.svg",
#     alt = "Moffitt Cancer Center Logo",
#     style = "margin-top: 50px; margin-bottom: 25px; width: 400px;"
#   )
#   logo_bottom <- tags$img(
#     src = "img/logos/orion_logo_small.png",
#     alt = "Orion Logo",
#     style = "width: 150px; margin-top: 30px; margin-bottom: 30px;"
#   )
# 
#   fluidPage(
#     style = "height: 100vh;",
#     shinyjs::useShinyjs(),
#     tags$head(
#       tags$link(rel = "shortcut icon", href = icon_href),
#       tags$title(company_name),
#       tags$meta(
#         name = "viewport",
#         content = "
#         width=device-width,
#         initial-scale=1,
#         maximum-scale=1,
#         minimum-scale=1,
#         user-scalable=no,
#         viewport-fit=cover"
#       ),
#       tags$style(
#         stringr::str_interp("
#         .auth_panel {
#           width: 100%;
#           max-width: 300px;
#           padding: 10px 25px;
#           background-color: #fff;
#           color: #080021;
#           margin: 0 auto;
#           box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
#         }
# 
#         .btn-primary {
#           background-color: ${button_color} !important;
#           border: none;
#           width: 100%;
#           color: #FFF;
#         }
# 
#         .footer {
#           color: #FFF;
#           text-align: center;
#           z-index: 1;
#           margin-top: -40px;
#         }
# 
#         body {
#           background-color: ${color} !important;
#         }
# 
#       ")
#       )
#     ),
# 
#     # dynamic JS background
#     tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/three.js/r121/three.min.js"),
#     tags$script(src="https://cdn.jsdelivr.net/npm/vanta@latest/dist/vanta.net.min.js"),
#     tags$script('
#       VANTA.NET({
#         el: "body",
#         mouseControls: true,
#         touchControls: true,
#         gyroControls: false,
#         minHeight: 200.00,
#         minWidth: 200.00,
#         scale: 1.00,
#         scaleMobile: 1.00,
#         color: "#58afe2"
#       })
#     '),
# 
# 
#     shiny::fluidRow(
#       style = "padding-bottom: 50px; min-height: 100%;",
#       shiny::column(
#         width = 12,
#         align = "center",
#         logo_top,
#         tags$div(
# 
#           div(
#             id = ns("sign_in_page"),
#             email_input(
#               ns("sign_in_email")
#             ),
#             password_input(
#               ns("sign_in_password")
#             ),
#             actionButton(
#               ns("sign_in_submit"),
#               "Sign In"
#             ),
#             actionLink(
#               ns("go_to_register"),
#               "Not a member? Register!"
#             )
#           ),
# 
#           # your custom registration inputs.  Your inputs
#           shinyjs::hidden(
#             div(
#               id = ns("register_page"),
#               password_input(
#                 ns("register_password")
#               ),
#               password_input(
#                 ns("register_password_verify")
#               ),
#               actionButton(
#                 ns("register_submit"),
#                 "Register"
#               ),
#               actionLink(
#                 ns("go_to_sign_in"),
#                 "Already a member? Sign in!"
#               )
#             )
#           ),
#           logo_bottom
#         )
#       )
#     ),
# 
# 
# 
#     shiny::fluidRow(
#       shiny::column(
#         12,
#         class = "footer",
#         tags$p(
#           htmltools::HTML("&copy;"),
#           paste0(
#             substr(Sys.Date(), 1, 4),
#             " - ",
#             company_name
#           )
#         )
#       )
#     ),
#     # make sure to call this function somewhere in your sign in page UI.  It loads
#     # the JavaScript used in the sign in and registration process.
#     sign_in_js(ns)
#   )
# 
# }

#' #' Server logic for the Sign In & Register pages
#'
#' This server logic accompanies the \code{\link{sign_in_module_ui}}.
#' 
#' @param input the Shiny \code{input}
#' @param output the Shiny \code{output}
#' @param session the Shiny \code{session}
#' 
#' @export
custom_sign_in_server <- function(input, output, session) {
  callModule(
    custom_sign_in_module,
    "sign_in"
  )
}

#' Server module for the Sign In & Register pages
#'
#' This server logic accompanies the \code{\link{sign_in_module_ui}}.
#'
#' @param input the Shiny \code{input}
#' @param output the Shiny \code{output}
#' @param session the Shiny \code{session}
#'
#' @importFrom shiny observeEvent observe getQueryString updateTextInput isolate
#' @importFrom shinyjs show hide enable disable
#' @importFrom shinyWidgets sendSweetAlert
#' @importFrom shinyFeedback showToast hideFeedback showFeedbackDanger resetLoadingButton
#' @importFrom digest digest
#'
#' @export
#'
custom_sign_in_module <- function(input, output, session) {
  ns <- session$ns

  # Email Sign-In validation
  observeEvent(input$sign_in_email, {
    shinyFeedback::hideFeedback("sign_in_email")
  })

  # Email Registration validation
  observeEvent(input$register_email, {
    shinyFeedback::hideFeedback("register_email")
  })

  observeEvent(input$sign_in_with_email, {
    shinyjs::show("email_ui")
    shinyjs::hide("providers_ui")
  })


  go_to_registration_page <- function() {
    # go to the user registration page
    shinyjs::hide("sign_in_panel_top")
    shinyjs::hide("sign_in_panel_bottom")
    shinyjs::show("register_panel_top")
    shinyjs::show("register_panel_bottom")
  }

  shiny::observeEvent(input$go_to_register, {
    go_to_registration_page()
  })

  shiny::callModule(
    send_password_reset_email_module,
    "reset_password",
    email = reactive({input$sign_in_email})
  )


  # if query parameter "register" == TRUE, then go directly to registration page
  observe({
    query_string <- shiny::getQueryString()

    if (identical(query_string$register, "TRUE")) {
      go_to_registration_page()
    }
  })


  shiny::observeEvent(input$submit_continue_sign_in, {

    email <- tolower(input$sign_in_email)

    is_email_sign_in <- is.null(input$check_jwt$jwt)
    if (isTRUE(is_email_sign_in) && !polished:::is_valid_email(email)) {
      shinyFeedback::showFeedbackDanger(
        "sign_in_email",
        text = "Invalid email"
      )
      return()
    }

    # check user invite
    invite <- NULL
    tryCatch({

      invite_res <- get_app_users(
        app_uid = .polished$app_uid,
        email = email
      )

      invite <- invite_res$content


      if (!identical(nrow(invite), 1L)) {

        shinyWidgets::sendSweetAlert(
          session,
          title = "Not Authorized",
          text = "You must have an invite to access this app",
          type = "error"
        )

      } else {


        # check if user is not registered.  If user is not registered, send them to
        # the registration page and auto populate the registration email input
        if (polished:::is_email_registered(email)) {
          # user is invited, so continue the sign in process
          shinyjs::hide("submit_continue_sign_in")

          shinyjs::show(
            "sign_in_password_ui",
            anim = TRUE
          )

          # NEED to sleep this exact amount to allow animation (above) to show w/o bug
          Sys.sleep(.25)

          shinyjs::runjs(paste0("$('#", ns('sign_in_password'), "').focus()"))
        } else {


          # user is not registered (they are accidentally attempting to sign in before
          # they have registed), so send them to the registration page and auto populate
          # the registration email input
          go_to_registration_page()

          shiny::updateTextInput(
            session,
            "register_email",
            value = email
          )

          shinyjs::hide("continue_registration")

          shinyjs::show(
            "register_passwords",
            anim = TRUE
          )

          # NEED to sleep this exact amount to allow animation (above) to show w/o bug
          Sys.sleep(0.3)

          shinyjs::runjs(paste0("$('#", ns('register_password'), "').focus()"))
        }



      }

    }, error = function(err) {
      # user is not invited
      print(err)
      shinyWidgets::sendSweetAlert(
        session,
        title = "Error",
        text = err$message,
        type = "error"
      )

    })

  })
  
  
  
  
  
  shiny::observeEvent(input$go_to_sign_in, {
    shinyjs::hide("register_panel_top")
    shinyjs::hide("register_panel_bottom")
    shinyjs::show("sign_in_panel_top")
    shinyjs::show("sign_in_panel_bottom")
  })
  
  
  
  
  
  shiny::observeEvent(input$submit_continue_register, {
    
    email <- tolower(input$register_email)
    
    if (!polished:::is_valid_email(email)) {
      shinyFeedback::showFeedbackDanger(
        "register_email",
        text = "Invalid email"
      )
      return(NULL)
    }
    
    invite <- NULL
    tryCatch({
      invite_res <- get_app_users(
        app_uid = .polished$app_uid,
        email = email
      )
      
      invite <- invite_res$content
      
      if (is.null(invite)) {
        
        shinyWidgets::sendSweetAlert(
          session,
          title = "Not Authorized",
          text = "You must have an invite to access this app",
          type = "error"
        )
        return()
      }
      
      # user is invited
      shinyjs::hide("continue_registration")
      
      shinyjs::show(
        "register_passwords",
        anim = TRUE
      )
      
      # NEED to sleep this exact amount to allow animation (above) to show w/o bug
      Sys.sleep(.25)
      
      shinyjs::runjs(paste0("$('#", ns('register_password'), "').focus()"))
      
    }, error = function(e) {
      # user is not invited
      print(e)
      shinyWidgets::sendSweetAlert(
        session,
        title = "Error",
        text = "Error checking invite",
        type = "error"
      )
    })
    
  }, ignoreInit = TRUE)
  
  
  observeEvent(input$register_js, {
    hold_email <- input$register_js$email
    hold_password <- input$register_js$password
    cookie <- input$register_js$cookie
    
    
    if (!polished:::is_valid_email(hold_email)) {
      
      shinyFeedback::showFeedbackDanger(
        "register_email",
        text = "Invalid email"
      )
      shinyFeedback::resetLoadingButton("register_submit")
      return(NULL)
      
    }
    
    hashed_cookie <- digest::digest(cookie)
   
    # apply email checks here
    if (nchar(hold_password) < 10) {
      shinyFeedback::showFeedbackDanger(
        "register_password",
        text = "password must be at least 10 characters"
      )
      shinyFeedback::resetLoadingButton("register_submit")
      return(NULL)
    }
    
    # "complex passwords" check. password must contain a number, an upper case letter, a lower case letter
    # and a special character.
    has_letter_upper <- grepl("[A-Z]", hold_password)
    has_letter_lower <- grepl("[a-z]", hold_password)
    has_number <- grepl("[0-9]", hold_password)
    has_special_character <- FALSE
    special_chars <- c("!", "#", "$", "%", "&", "=", "?", "@")
    for (special_char in special_chars) {
      check_special <- grepl(special_char, hold_password, fixed = TRUE)
      if (isTRUE(check_special)) {
        has_special_character <- TRUE
      }
    }
    if (!all(has_letter_upper, has_letter_lower, has_number, has_special_character)) {
      shinyFeedback::showToast(
        "error", 
        "password must contain a number, an upper and lower case letter, and a special character"
      )
      shinyFeedback::resetLoadingButton("register_submit")
      return(NULL)
    }
    
    
    out <- NULL
    old_pws <- NULL
    had_err <- FALSE
    hashed_pw <- digest::digest(hold_password)
    tryCatch({
      
      user_res <- get_users(
        email = hold_email
      )
      user <- user_res$content
      
      if (identical(nrow(user), 1L)) {
        # there should always be a user because invite is required, but if they switch to 
        # invite is required there might not yet be a user and we wont have to do these checks
        # on previous passwords
        
        old_pws <- dbGetQuery(
          con,
          "SELECT * FROM all_passwords WHERE user_uid=? ORDER BY created_at DESC LIMIT 8",
          params = list(
            user$uid
          )
        )
        
      }
      
    }, error = function(err) {
      
      msg <- "unable to check password"
      print(msg)
      print(err)
      shinyFeedback::showToast("error", msg)
      shinyFeedback::resetLoadingButton("register_submit")
      had_err <<- TRUE
      invisible(NULL)
    })
    
    if (isTRUE(had_err)) {
      return(NULL)
    }
    
    
    if (is.null(old_pws) || nrow(old_pws) > 0L) {
      
      # check that this password has not been used in one of the last 8 passwords
      if (hashed_pw %in% old_pws$hashed_pw) {
        shinyFeedback::showToast("error", "you've used this same password too recently")
        shinyFeedback::resetLoadingButton("register_submit")
        return(NULL)
      }
    }
    
    
    tryCatch({
      
      polished:::register_email(
        hold_email,
        hold_password,
        hashed_cookie
      )
      
      rows_affected <- dbExecute(
        con,
        "INSERT INTO all_passwords (user_uid, hashed_pw) VALUES (?, ?)",
        params = list(
          user$uid,
          hashed_pw
        )
      )
      
      remove_query_string()
      session$reload()
    }, error = function(err) {
      
      shinyFeedback::resetLoadingButton('register_submit')
      
      print(err)
      shinyFeedback::showToast(
        "error",
        err$message,
        .options = polished:::polished_toast_options
      )
    })
    
  })
  
  
  
  valid_email_pw <- reactive({
    req(input$check_jwt)
    hold_email <- input$sign_in_email
    
    if (!polished:::is_valid_email(isolate({hold_email}))) {
      
      shinyFeedback::showFeedbackDanger(
        "sign_in_email",
        text = "Invalid email"
      )
      shinyFeedback::resetLoadingButton("sign_in_submit")
      return(NULL)
    }
    
    
    out <- input$check_jwt
    tryCatch({
      
      user_res <- get_users(
        email = hold_email
      )
      user <- user_res$content
      
      current_pw <- dbGetQuery(
        con,
        "SELECT * FROM all_passwords WHERE user_uid=? ORDER BY created_at DESC LIMIT 1",
        params = list(
          user$uid
        )
      )
      
      
      # check that password is less than 360 days old
      if (identical(nrow(current_pw), 1L) && (Sys.Date() - as.Date(current_pw$created_at[1]) > 360)) {
        
        # update_user to delete user's password and send them to registration page
        update_user(
          user_uid = user$user_uid,
          user_data = list(
            password = NA
          )
        )
        
        shinyFeedback::showToast(
          "info", 
          "Your password is expired, please set a new password"
        )
        
        go_to_registration_page()
      }
      
    }, error = function(err) {
      msg <- "unable to check password for expiration"
      print(msg)
      print(err)
      shinyFeedback::showToast("error", msg)
      
      invisible(NULL)
    })
    # check if password has expired and force password reset if expired
      
    out
  })
  
  
  
  observeEvent(valid_email_pw(), {
    hold_jwt <- valid_email_pw()
    
    tryCatch({
        
      # attempt sign in with email
      new_user <- polished:::sign_in_email(
        email = hold_jwt$email,
        password = hold_jwt$password,
        hashed_cookie = digest::digest(hold_jwt$cookie)
      )
      
      
        
      if (!is.null(new_user$message) && identical(new_user$message, "Password reset email sent")) {
        shinyFeedback::resetLoadingButton('sign_in_submit')
        shinyFeedback::showToast(
          "info",
          "Password reset required.  Check your email to reset your password.",
          .options = polished:::polished_toast_options
        )
        return()
      }
      
      if (is.null(new_user)) {
        shinyFeedback::resetLoadingButton('sign_in_submit')
        # show unable to sign in message
        shinyFeedback::showToast(
          'error',
          'sign in error',
          .options = polished:::polished_toast_options
        )
        stop('sign_in_module: sign in error', call. = FALSE)
        
      } else {
        # sign in success
        
        # hash password and save it to the "all_passwords" table
        
        remove_query_string()
        session$reload()
      }
      
    }, error = function(err) {
      shinyFeedback::resetLoadingButton('sign_in_submit')
      print(err)
      
      shinyFeedback::showToast(
        "error",
        err$message,
        .options = polished:::polished_toast_options
      )
      
    })
    
  })
  
  invisible()
}
