FORM display_alv.

    DATA: lt_fcat TYPE lvc_t_fcat,
          lt_excl TYPE ui_functions.
  
    IF go_grid IS NOT BOUND.
  
      IF gt_invo IS INITIAL.
        APPEND INITIAL LINE TO gt_invo.
      ENDIF.
  
      CREATE OBJECT go_cont
        EXPORTING
          container_name              = 'ALVGRID'
          lifetime                    = cl_gui_control=>lifetime_dynpro
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5
          OTHERS                      = 6.
  
      CREATE OBJECT go_grid
        EXPORTING
          i_lifetime        = cl_gui_control=>lifetime_dynpro
          i_parent          = go_cont
          i_appl_events     = abap_true "Triggers PAI/PBO on alv events
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.
  
      PERFORM build_fcat USING gt_invo CHANGING lt_fcat.
  *    PERFORM change_fcat CHANGING lt_fcat.
  
      LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
        <ls_fcat>-ref_table  = 'VBRP'.
        <ls_fcat>-edit       = abap_true.
        <ls_fcat>-f4availabl = abap_true.
      ENDLOOP.
  
      go_grid->register_f4_for_fields( VALUE #( ( fieldname = 'VBELN' register = abap_true )
                                                ( fieldname = 'POSNR' register = abap_true ) ) ).
  
      SET HANDLER lcl_events=>on_toolbar      FOR go_grid.
      SET HANDLER lcl_events=>on_user_command FOR go_grid.
      SET HANDLER lcl_events=>on_after_user_command FOR go_grid.
  *    SET HANDLER lcl_events=>on_double_click       FOR go_grid.
      SET HANDLER lcl_events=>on_f4       FOR go_grid.
  
      PERFORM exclude_toolbar_functions CHANGING lt_excl.
  
      go_grid->set_table_for_first_display(
        EXPORTING
          is_layout                     = VALUE #( cwidth_opt = abap_true
                                                   sel_mode   = ''
                                                   info_fname = ''  )
          it_toolbar_excluding          = lt_excl
        CHANGING
          it_outtab                     = gt_invo
          it_fieldcatalog               = lt_fcat
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4 ).
  
    ELSE.
  
      go_grid->set_frontend_layout( VALUE #( cwidth_opt = abap_true ) ).
  
      go_grid->refresh_table_display(
        EXPORTING
          is_stable      = VALUE #( row = abap_true col = abap_true )
        EXCEPTIONS
          finished       = 1
          OTHERS         = 2 ).
  
    ENDIF.
  
  ENDFORM.
  
  FORM build_fcat USING it_list TYPE ANY TABLE
               CHANGING ct_fcat TYPE lvc_t_fcat.
    cl_salv_table=>factory(
      IMPORTING
        r_salv_table = DATA(lo_salv)
      CHANGING
        t_table      = it_list ).
  
    ct_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
          r_columns      = lo_salv->get_columns( )
          r_aggregations = lo_salv->get_aggregations( ) ).
  
    LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
      <ls_fcat>-no_sign = abap_false.
    ENDLOOP.
  ENDFORM.
  
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
    APPEND '&CHECK' TO ct_excl.
    APPEND '&REFRESH' TO ct_excl.
    APPEND cl_gui_alv_grid=>mc_fc_loc_cut TO ct_excl.
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy TO ct_excl.
    APPEND cl_gui_alv_grid=>mc_fc_loc_paste TO ct_excl.
    APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO ct_excl.
    APPEND cl_gui_alv_grid=>mc_fc_loc_undo TO ct_excl.
    APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO ct_excl.
    APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row TO ct_excl.
    APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row TO ct_excl.
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row TO ct_excl.
  ENDFORM.