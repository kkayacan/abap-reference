FORM exclude_toolbar_functions CHANGING ct_excl TYPE ui_functions.
  APPEND '&DETAIL' TO t_excl.
  APPEND '&SORT_ASC' TO t_excl.
  APPEND '&SORT_DSC' TO t_excl.
  APPEND '&FIND' TO t_excl.
  APPEND '&PRINT_BACK' TO t_excl.
  APPEND '&PRINT_BACK_PREVIEW' TO t_excl.
  APPEND '&VEXCEL' TO t_excl.
  APPEND '&XXL' TO t_excl.
  APPEND '&AQW' TO t_excl.
  APPEND '&PC' TO t_excl.
  APPEND '&SEND' TO t_excl.
  APPEND '&ML' TO t_excl.
  APPEND '&HTML' TO t_excl.
  APPEND '&COL0' TO t_excl.
  APPEND '&INFO' TO t_excl.
  APPEND cl_gui_alv_grid=>mc_mb_filter TO t_excl.
  APPEND cl_gui_alv_grid=>mc_mb_sum TO t_excl.
  APPEND cl_gui_alv_grid=>mc_mb_subtot TO t_excl.
  APPEND cl_gui_alv_grid=>mc_mb_view TO t_excl.
  APPEND '&CHECK' TO t_excl.
  APPEND '&REFRESH' TO t_excl.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut TO t_excl.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy TO t_excl.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste TO t_excl.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO t_excl.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo TO t_excl.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO t_excl.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO t_excl.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO t_excl.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row TO t_excl.
  APPEND cl_gui_alv_grid=>mc_fc_call_abc TO t_excl.
ENDFORM.