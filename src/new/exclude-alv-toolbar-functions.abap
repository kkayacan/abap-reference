FORM exclude_toolbar_functions CHANGING ct_excl TYPE ui_functions.
    APPEND '&DETAIL' TO ct_excl.
    APPEND '&SORT_ASC' TO ct_excl.
    APPEND '&SORT_DSC' TO ct_excl.
    APPEND '&FIND' TO ct_excl.
    APPEND '&PRINT_BACK' TO ct_excl.
    APPEND '&PRINT_BACK_PREVIEW' TO ct_excl.
    APPEND '&VEXCEL' TO ct_excl.
    APPEND '&XXL' TO ct_excl.
    APPEND '&AQW' TO ct_excl.
    APPEND '&PC' TO ct_excl.
    APPEND '&SEND' TO ct_excl.
    APPEND '&ML' TO ct_excl.
    APPEND '&HTML' TO ct_excl.
    APPEND '&COL0' TO ct_excl.
    APPEND '&INFO' TO ct_excl.
    APPEND cl_gui_alv_grid=>mc_mb_filter TO ct_excl.
    APPEND cl_gui_alv_grid=>mc_mb_sum TO ct_excl.
    APPEND cl_gui_alv_grid=>mc_mb_subtot TO ct_excl.
    APPEND cl_gui_alv_grid=>mc_mb_view TO ct_excl.
  ENDFORM.