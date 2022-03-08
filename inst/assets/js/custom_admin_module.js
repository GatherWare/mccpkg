function custom_admin_module(ns_prefix) {
  
  $(`#${ns_prefix}users_table`).on("click", ".reset_pw_btn", e => {
    $(e.currentTarget).tooltip("hide");
    Shiny.setInputValue(`${ns_prefix}reset_pw_btn_user_uid`, e.currentTarget.id, { priority: "event"});
  });
  
}
