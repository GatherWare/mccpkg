#' The UI for the "Admin Panel" dashboard
#'
#' The `shiny` module UI for the `polished` Admin Panel, accessible to Admin users.
#'
#' @param id the Shiny module id.
#'
#'
#' @importFrom shiny actionButton NS icon
#' @importFrom shinydashboard dashboardHeader dashboardPage dashboardSidebar dashboardBody sidebarMenu menuItem tabItems
#' @importFrom htmltools HTML tags
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyFeedback useShinyFeedback
#'
#' @return the UI for the "Admin Panel"
#'
#' @export
#'
custom_admin_module_ui <- function() {
  
  if (identical(.polished$app_name, "MCC-Dynamic-Master-Plan")) {
    dm_li <- tags$li( 
      tags$a(
        href = "https://gatherware.shinyapps.io/MCC-Data-Management"
        , target = "blank_"
        , icon("database")
        , "Data Management"
      ),
    )
    name_out <- tagList(
      icon("rocket"),  
      "Master Plan"
    )
    
  } else {
    dm_li <-dm_li <- tags$li( 
      tags$a(
        href = "https://gatherware.shinyapps.io/MCC-Dynamic-Master-Plan"
        , target = "blank_"
        , icon("rocket")
        , "Master Plan"
      ),
    )
    name_out <- name_out <- tagList(
      icon("database"),
      "Data Management"
    )
  }
  
  
  
  
  head <- shinydashboard::dashboardHeader(
    title = tagList(
      tags$img(
        src = 'mccpkg/images/orion_logo_small.png', 
        width = "45px"
      ), 
      htmltools::tags$head(htmltools::tags$title("Admin Panel"))
    ),
    polished::profile_module_ui(
      "profile", 
      other_lis = tagList(
        tags$li(
          tags$a(
            id = "shiny_app_link", 
            href = app_config$base_url, 
            name_out
          )
        ),
        dm_li, 
        tags$li(
          actionLink("user_feedback", label = "Leave Feedback", icon = icon("comments"))
        )
      )
    )
  )
  
  
  
  
  sidebar <- shinydashboard::dashboardSidebar(
    collapsed = TRUE,
    tags$script(htmlwidgets::JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")), # Removes hamburger menu
    shinydashboard::sidebarMenu(
      id = "sidebar_menu",
      shinydashboard::menuItem(
        text = "User Access",
        tabName = "user_access",
        icon = shiny::icon("users")
      )
    )
  )
  
  
  
  tab_items <- shinydashboard::tabItems(
    user_access_module_ui("user_access")
  )
  
  
  
  
  
  body <- shinydashboard::dashboardBody(
    htmltools::tags$head(
      tags$link(
        rel = "shortcut icon", 
        href = "mccpkg/images/orion_logo_small.png"
      ),
      htmltools::tags$link(rel = "stylesheet", href = "mccpkg/css/admin_styles.css?version=1")
    ),
    shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback(),
    
    tab_items
  )
  
  
  
  
  shinydashboardPlus::dashboardPagePlus(
    header = head,
    sidebar = sidebar,
    body = body,
    title = "Admin Panel",
    skin = "black",
    collapse_sidebar = TRUE
  )
}


#' The server logic for the defaul "Admin Panel" dashboard
#'
#' The `shiny` module server logic for the `polished` Admin Panel, accessible to Admin users.
#'
#' @param input the Shiny server input
#' @param output the Shiny server output
#' @param session the Shiny server session
#'
#' @importFrom shiny callModule observeEvent
#'
#' @export
#'
custom_admin_module <- function(input, output, session) {
  ns <- session$ns
  
  shiny::callModule(
    profile_module,
    "profile"
  )
  
  shiny::observeEvent(input$go_to_shiny_app, {
    
    # to to the Shiny app
    remove_query_string(mode = "push")
    
    session$reload()
    
  }, ignoreInit = TRUE)
  
  shiny::callModule(user_access_module, "user_access")
}


#' admin_user_access_ui
#'
#' @param id the module id
#'
#' @importFrom shiny fluidRow column actionButton
#' @importFrom shinydashboard tabItem box
#' @importFrom shinycssloaders withSpinner
#' @importFrom htmltools br tags
#' @importFrom DT DTOutput
#'
#' @noRd
#'
user_access_module_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shinydashboard::tabItem(
    tabName = "user_access",
    shiny::fluidRow(
      tags$style(
        paste0(
          "#", ns('users_table'), " .dataTables_length {
            padding-top: 10px;
          }"
        )
      ),
      shinydashboard::box(
        width = 12,
        title = "Users",
        #style = "min-height: 500px;",
        # collapsible = TRUE,
        shiny::fluidRow(
          shiny::column(
            12,
            shiny::actionButton(
              ns("add_user"),
              "Add User",
              class = "btn-success",
              #style = "color: #fff; position: absolute: top: 20, left: 15; margin-bottom: 0;",
              style = "color: #fff;",
              icon = icon("user-plus")
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            12,
            style = "z-index: 10",
            DT::DTOutput(ns("users_table")) %>%
              shinycssloaders::withSpinner(
                type = 8,
                proxy.height = "300px"
              )
          )
        )
      ),
      shiny::column(
        12,
        br(),
        br()
      )
    ),
    # users table
    tags$script(src = "polish/js/user_access_module.js?version=2"),
    tags$script(paste0("user_access_module('", ns(''), "')")),
    tags$script(src = "mccpkg/js/custom_admin_module.js?version=2"),
    tags$script(paste0("custom_admin_module('", ns(''), "')"))
  )
}

#' admin_user_access
#'
#' @param input the Shiny server input
#' @param output the Shiny server output
#' @param session the Shiny server session
#'
#' @importFrom shiny showModal modalDialog removeModal reactiveVal reactive observeEvent callModule req eventReactive
#' @importFrom htmltools tags
#' @importFrom DT renderDT datatable dataTableProxy formatDate replaceData JS
#' @importFrom dplyr filter select %>% left_join mutate
#' @importFrom tibble tibble
#' @importFrom shinyFeedback showToast
#' @importFrom purrr map_chr
#' @importFrom lubridate force_tz as_datetime
#' @importFrom rlang .data
#'
#' @noRd
#'
user_access_module <- function(input, output, session) {
  ns <- session$ns
  
  # trigger to reload the `users` reactive from the database
  users_trigger <- reactiveVal(0)
  users <- reactive({
    users_trigger()
    
    
    out <- NULL
    tryCatch({
      
      app_users_res <- get_app_users(
        app_uid = .polished$app_uid
      )
      
      app_users <- app_users_res$content
      
      app_users <- app_users %>%
        mutate(created_at = as.POSIXct(.data$created_at))
      
      res <- httr::GET(
        url = paste0(.polished$api_url, "/last-active-session-time"),
        query = list(
          app_uid = .polished$app_uid
        ),
        httr::authenticate(
          user = get_api_key(),
          password = ""
        )
      )
      
      httr::stop_for_status(res)
      
      last_active_times <- jsonlite::fromJSON(
        httr::content(res, "text", encoding = "UTF-8")
      )
      
      last_active_times <- tibble::as_tibble(last_active_times)
      
      last_active_times <- last_active_times %>%
        mutate(last_sign_in_at = lubridate::force_tz(lubridate::as_datetime((.data$last_sign_in_at)), tzone = "UTC"))
      
      out <- app_users %>%
        left_join(last_active_times, by = 'user_uid')
      
    }, error = function(err) {
      
      msg <- "unable to get users from API"
      print(paste0("[polished] error: ", msg))
      print(err)
      
      showToast(
        "error",
        msg,
        .options = polished_toast_options
      )
    })
    
    out
  })
  
  users_table_prep <- reactiveVal(NULL)
  observeEvent(users(), {
    
    out <- users()
    n_rows <- nrow(out)
    
    if (n_rows == 0) {
      actions <- character(0)
    } else {
      
      actions <- purrr::map_chr(seq_len(n_rows), function(row_num) {
        
        the_row <- out[row_num, ]
        
        if (isTRUE(the_row$is_admin)) {
          buttons_out <- paste0('<div class="btn-group" style="width: 135px" role="group" aria-label="User Action Buttons">
            <button class="btn btn-default btn-sm sign_in_as_btn" data-toggle="tooltip" data-placement="top" title="Sign In As" id = ', the_row$user_uid, ' style="margin: 0"><i class="fas fa-user-astronaut"></i></button>
            <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Edit User" id = ', the_row$user_uid, ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
            <button class="btn btn-danger btn-sm delete_btn" id = ', the_row$user_uid, ' style="margin: 0" disabled><i class="fa fa-trash-o"></i></button>
            <button class="btn btn-warning btn-sm reset_pw_btn" data-toggle="tooltip" data-placement="top" title="Reset Password" id = ', the_row$user_uid, ' style="margin: 0"><i class="fas fa-lock"></i></button>
          </div>')
        } else {
          buttons_out <- paste0('<div class="btn-group" style="width: 135px" role="group" aria-label="User Action Buttons">
            <button class="btn btn-default btn-sm sign_in_as_btn" data-toggle="tooltip" data-placement="top" title="Sign In As" id = ', the_row$user_uid, ' style="margin: 0"><i class="fas fa-user-astronaut"></i></button>
            <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Edit User" id = ', the_row$user_uid, ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
            <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete User" id = ', the_row$user_uid, ' style="margin: 0"><i class="fa fa-trash-o"></i></button>
            <button class="btn btn-warning btn-sm reset_pw_btn" data-toggle="tooltip" data-placement="top" title="Reset Password" id = ', the_row$user_uid, ' style="margin: 0"><i class="fas fa-lock"></i></button>
          </div>')
        }
        
        buttons_out
      })
      
      
      out <- cbind(
        tibble::tibble(actions = actions),
        out
      ) %>%
        dplyr::mutate(
          invite_status = ifelse(is.na(.data$last_sign_in_at), "Pending", "Accepted")
        ) %>%
        dplyr::select(.data$actions, .data$email, .data$invite_status, .data$is_admin, .data$last_sign_in_at)
    }
    
    if (is.null(users_table_prep())) {
      users_table_prep(out)
    } else {
      shinyjs::runjs("$('.btn-sm').tooltip('hide')")
      DT::replaceData(users_proxy, out, resetPaging = FALSE, rownames = FALSE)
    }
    
  })
  
  output$users_table <- DT::renderDT({
    shiny::req(users_table_prep())
    out <- users_table_prep()
    
    DT::datatable(
      out,
      rownames = FALSE,
      colnames = c(
        "",
        "Email",
        "Invite Status",
        "Is Admin?",
        "Last Sign In"
      ),
      escape = -1,
      selection = "none",
      callback = DT::JS("$( table.table().container() ).addClass( 'table-responsive' ); return table;"),
      options = list(
        dom = 'ftlp',
        columnDefs = list(
          list(targets = 0, orderable = FALSE),
          list(targets = 0, class = "dt-center"),
          list(targets = 0, width = "105px")
        ),
        order = list(
          list(4, 'desc')
        ),
        # removes any lingering tooltips
        drawCallback = JS("function(settings) {
          $('.tooltip').remove();
        }")
      )
    ) %>%
      DT::formatDate(5, method = "toLocaleString")
  })
  
  users_proxy <- DT::dataTableProxy("users_table")
  
  add_user_return <- shiny::callModule(
    polished:::user_edit_module,
    "add_user",
    modal_title = "Add User",
    user_to_edit = function() NULL,
    open_modal_trigger = reactive({input$add_user}),
    existing_users = users
  )
  
  observeEvent(add_user_return$users_trigger(), {
    users_trigger(users_trigger() + 1)
  }, ignoreInit = TRUE)
  
  
  
  user_to_edit <- reactiveVal(NULL)
  observeEvent(input$user_uid_to_edit, {
    
    out <- users() %>%
      dplyr::filter(.data$user_uid == input$user_uid_to_edit)
    
    user_to_edit(out)
  }, priority = 1)
  
  edit_user_return <- shiny::callModule(
    polished:::user_edit_module,
    "edit_user",
    modal_title = "Edit User",
    user_to_edit = user_to_edit,
    open_modal_trigger = reactive({input$user_uid_to_edit}),
    existing_users = users
  )
  
  observeEvent(edit_user_return$users_trigger(), {
    users_trigger(users_trigger() + 1)
  }, ignoreInit = TRUE)
  
  
  
  
  
  
  user_to_delete <- reactiveVal(NULL)
  observeEvent(input$user_uid_to_delete, {
    
    out <- users() %>%
      dplyr::filter(.data$user_uid == input$user_uid_to_delete)
    
    user_to_delete(out)
  }, priority = 1)
  
  
  
  observeEvent(input$user_uid_to_delete, {
    hold_user <- user_to_delete()
    shiny::req(nrow(hold_user) == 1)
    
    shiny::showModal(
      shiny::modalDialog(
        title = "Delete User",
        footer = list(
          modalButton("Cancel"),
          actionButton(
            ns("submit_user_delete"),
            "Delete User",
            class = "btn-danger",
            style = "color: white",
            icon = icon("times")
          )
        ),
        size = "m",
        
        # modal content
        tags$div(
          class = "text-center",
          style = "padding: 30px;",
          tags$h3(
            style = "line-height: 1.5;",
            HTML(paste0(
              "Are you sure you want to delete ", tags$b(hold_user$email), "?"
            ))
          ),
          tags$br()
        )
      )
    )
    
  })
  
  
  shiny::observeEvent(input$submit_user_delete, {
    shiny::removeModal()
    
    user_uid <- user_to_delete()$user_uid
    app_uid <- .polished$app_uid
    
    tryCatch({
      
      res <- httr::DELETE(
        url = paste0(.polished$api_url, "/app-users"),
        body = list(
          user_uid = user_uid,
          app_uid = app_uid,
          req_user_uid = session$userData$user()$user_uid
        ),
        httr::authenticate(
          user = get_api_key(),
          password = ""
        ),
        encode = "json"
      )
      
      httr::stop_for_status(res)
      
      shinyFeedback::showToast(
        "success",
        "User successfully deleted",
        .options = polished_toast_options
      )
      users_trigger(users_trigger() + 1)
    }, error = function(e) {
      shinyFeedback::showToast(
        "error",
        "Error deleting user",
        .options = polished_toast_options
      )
      print(e)
    })
    
  })
  
  
  shiny::observeEvent(input$sign_in_as_btn_user_uid, {
    
    hold_user <- session$userData$user()
    
    user_to_sign_in_as <- users() %>%
      filter(.data$user_uid == input$sign_in_as_btn_user_uid) %>%
      dplyr::pull("user_uid")
    
    # sign in as another user
    polished:::update_session(
      session_uid = hold_user$session_uid,
      session_data = list(
        signed_in_as = user_to_sign_in_as
      )
    )
    
    # to to the Shiny app
    remove_query_string(mode = "push")
    
    session$reload()
  }, ignoreInit = TRUE)
  
  
  observeEvent(input$reset_pw_btn_user_uid, {
    input$reset_pw_btn_user_uid
    
    tryCatch({
      
      update_user(
        user_uid = input$reset_pw_btn_user_uid,
        user_data = list(
          password = NA
        )
      )
      
      shinyFeedback::showToast("success", "password reset.  User will be asked to make a new password on next sign in.")
      
    }, error = function(err) {
      msg <- "unable to reset password"
      print(msg)
      print(err)
      shinyFeedback::showToast("error", msg)
      
      invisible(NULL)
    })
  })
  
  
  return(list(
    
  ))
}
