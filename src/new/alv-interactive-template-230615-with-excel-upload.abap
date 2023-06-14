REPORT zmm_p_post_inv_count.

INCLUDE zabap2xlsx_simple.

CLASS lcl_controller DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      initialization,
      at_selection_screen,
      start_of_selection,
      end_of_selection,
      pbo,
      pai,
      file_open_dialog RETURNING VALUE(rv_file) TYPE string.

  PRIVATE SECTION.

    TYPES: BEGIN OF st_list,
             statu   TYPE vvis_lights,
             iblnr   TYPE ikpf-iblnr,
             gjahr   TYPE ikpf-gjahr,
             gidat   TYPE ikpf-gidat,
             zeili   TYPE iseg-zeili,
             matnr   TYPE iseg-matnr,
             maktx   TYPE makt-maktx,
             charg   TYPE iseg-charg,
             menge   TYPE iseg-menge,
             meins   TYPE iseg-meins,
             message TYPE bapiret2-message,
           END OF st_list.

    CLASS-DATA:
      t_list TYPE TABLE OF st_list,
      o_grid TYPE REF TO cl_gui_alv_grid,
      t_excl TYPE ui_functions,
      s_layo TYPE lvc_s_layo.

    CLASS-METHODS:
      set_gui,
      save
        IMPORTING
          ip_all TYPE abap_bool DEFAULT abap_false.

    "ALV grid
    CLASS-METHODS:
      display_grid,
      build_fcat
        IMPORTING
          it_list        TYPE ANY TABLE
        RETURNING
          VALUE(rt_fcat) TYPE lvc_t_fcat,
      change_fcat
        IMPORTING
          it_fcat        TYPE lvc_t_fcat
        RETURNING
          VALUE(rt_fcat) TYPE lvc_t_fcat,
      exclude_toolbar_functions,
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,
      on_after_user_command FOR EVENT after_user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no.

*    CLASS-METHODS:
*      reject_order
*        IMPORTING
*          iv_vbeln      TYPE vbap-vbeln
*          iv_posnr      TYPE vbap-posnr
*        RETURNING
*          VALUE(rs_msg) TYPE bapiret2.

ENDCLASS.

TABLES: sscrfields, vbak.

SELECTION-SCREEN BEGIN OF BLOCK b01.
  PARAMETERS: p_file TYPE rlgrap-filename.
SELECTION-SCREEN END OF BLOCK b01.

INITIALIZATION.
  lcl_controller=>initialization( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  p_file = lcl_controller=>file_open_dialog( ).

AT SELECTION-SCREEN.
  lcl_controller=>at_selection_screen( ).

START-OF-SELECTION.
  lcl_controller=>start_of_selection( ).

END-OF-SELECTION.
  lcl_controller=>end_of_selection( ).

CLASS lcl_controller IMPLEMENTATION.

  METHOD initialization.
*    DATA ls_functxt TYPE smp_dyntxt.
*    ls_functxt-icon_id   = icon_change_number.
*    ls_functxt-quickinfo = TEXT-s02.
*    ls_functxt-icon_text = TEXT-s02.
*    sscrfields-functxt_01   = ls_functxt.
  ENDMETHOD.

  METHOD at_selection_screen.
*    CASE sscrfields-ucomm.
*      WHEN'FC01'.
*
*    ENDCASE.
  ENDMETHOD.

  METHOD start_of_selection.

    DATA: excel  TYPE REF TO zcl_excel,
          reader TYPE REF TO zif_excel_reader.

    DATA: worksheet      TYPE REF TO zcl_excel_worksheet,
          highest_column TYPE zexcel_cell_column,
          highest_row    TYPE int4,
          column         TYPE zexcel_cell_column VALUE 1,
          col_str        TYPE zexcel_cell_column_alpha,
          row            TYPE int4               VALUE 2,
          value          TYPE zexcel_cell_value.

    DATA ls_list LIKE LINE OF t_list.

    CREATE OBJECT reader TYPE zcl_excel_reader_2007.
    excel = reader->load_file( p_file ).

    worksheet = excel->get_active_worksheet( ).
    highest_column = worksheet->get_highest_column( ).
    highest_row    = worksheet->get_highest_row( ).

    WHILE row <= highest_row.
      WHILE column <= highest_column.
        col_str = zcl_excel_common=>convert_column2alpha( column ).
        worksheet->get_cell(
          EXPORTING
            ip_column = col_str
            ip_row    = row
          IMPORTING
            ep_value = value ).
        CASE column.
          WHEN 1. ls_list-iblnr = value.
          WHEN 2. ls_list-gjahr = value.
          WHEN 3. ls_list-gidat = zcl_excel_common=>excel_string_to_date( value ).
          WHEN 4. ls_list-zeili = value.
          WHEN 5.
            ls_list-matnr = value.
            CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
              EXPORTING
                input        = value
              IMPORTING
                output       = ls_list-matnr
              EXCEPTIONS
                length_error = 1
                OTHERS       = 2.
            SELECT SINGLE maktx FROM makt
              WHERE matnr = @ls_list-matnr
                AND spras = @sy-langu
              INTO @ls_list-maktx.
          WHEN 6. ls_list-charg = value.
          WHEN 7. ls_list-meins = value.
        ENDCASE.
        column = column + 1.
      ENDWHILE.
      APPEND ls_list TO t_list.
      column = 1.
      row = row + 1.
    ENDWHILE.

*    IF t_list IS INITIAL.
*      MESSAGE s004(sv) DISPLAY LIKE 'E'.
*      RETURN.
*    ENDIF.

  ENDMETHOD.

  METHOD end_of_selection.
    CHECK t_list IS NOT INITIAL.
    CASE sy-batch.
      WHEN abap_false. CALL SCREEN 0001.
      WHEN abap_true.  save( abap_true ).
    ENDCASE.
  ENDMETHOD.

  METHOD pbo.
    set_gui( ).
    display_grid( ).
  ENDMETHOD.

  METHOD pai.
    o_grid->check_changed_data( ).
    CASE sy-dynnr.
      WHEN '0001'.
        CASE sy-ucomm.
          WHEN 'BACK'. SET SCREEN 0.
          WHEN 'EXIT'. LEAVE PROGRAM.
          WHEN 'CANC'. LEAVE PROGRAM.
          WHEN 'SAVE'. save( ).
        ENDCASE.
    ENDCASE.
  ENDMETHOD.

  METHOD file_open_dialog.
    DATA: lt_file_tab TYPE filetable,
          lv_rc       TYPE i.

    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        window_title         = ''
        file_filter          = 'Excel Files (*.xlsx)|*.XLSX'
      CHANGING
        file_table           = lt_file_tab
        rc                   = lv_rc
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4.

    IF sy-subrc = 0 AND lt_file_tab IS NOT INITIAL.
      rv_file = lt_file_tab[ 1 ].
    ENDIF.
  ENDMETHOD.

  METHOD set_gui.
    DATA title TYPE sy-title.
    title = sy-title.
    CASE sy-dynnr.
      WHEN '0001'.
        SET TITLEBAR 'TITLE' WITH title.
        SET PF-STATUS 'STATUS'.
    ENDCASE.
  ENDMETHOD.

  METHOD save.

    DATA lt_items   TYPE STANDARD TABLE OF bapi_physinv_count_items.
    DATA lt_return  TYPE STANDARD TABLE OF bapiret2.

    DATA(lt_doc) = t_list.
    DELETE lt_doc WHERE menge IS INITIAL.
    SORT lt_doc BY iblnr.
    DELETE ADJACENT DUPLICATES FROM lt_doc COMPARING iblnr.

    LOOP AT lt_doc ASSIGNING FIELD-SYMBOL(<ls_doc>).

      CLEAR: lt_items, lt_return.

      LOOP AT t_list ASSIGNING FIELD-SYMBOL(<ls_list>) WHERE iblnr  = <ls_doc>-iblnr
                                                       AND   menge <> 0.
        APPEND INITIAL LINE TO lt_items ASSIGNING FIELD-SYMBOL(<ls_item>).
        <ls_item>-item      = <ls_list>-zeili.
        <ls_item>-material  = <ls_list>-matnr.
        <ls_item>-batch     = <ls_list>-charg.
        <ls_item>-entry_qnt = <ls_list>-menge.
        <ls_item>-entry_uom = <ls_list>-meins.
      ENDLOOP.

      CALL FUNCTION 'BAPI_MATPHYSINV_COUNT'
        EXPORTING
          physinventory = <ls_doc>-iblnr
          fiscalyear    = <ls_doc>-gjahr
          count_date    = <ls_doc>-gidat
        TABLES
          items         = lt_items
          return        = lt_return.

      READ TABLE lt_return INTO DATA(ls_return) WITH KEY type = 'E'.
      LOOP AT t_list ASSIGNING <ls_list> WHERE iblnr  = <ls_doc>-iblnr
                                         AND   menge <> 0.
        <ls_list>-message = ls_return-message.
        IF ls_return-message IS NOT INITIAL.
          <ls_list>-statu = '1'.
        ELSE.
          <ls_list>-statu = '3'.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

*    DATA: lt_roid TYPE lvc_t_roid.
*    FIELD-SYMBOLS: <ls_roid> LIKE LINE OF lt_roid,
*                   <ls_list> LIKE LINE OF t_list.
*    CASE ip_all.
*      WHEN abap_false.
*        o_grid->get_selected_rows(
*          IMPORTING
*            et_row_no     = lt_roid ).
*      WHEN abap_true.
*        DATA(lv_lines) = lines( t_list ).
*        DO lv_lines TIMES.
*          APPEND VALUE #( row_id = sy-index ) TO lt_roid.
*        ENDDO.
*    ENDCASE.
*
*    LOOP AT lt_roid ASSIGNING <ls_roid>.
*      READ TABLE t_list ASSIGNING <ls_list> INDEX <ls_roid>-row_id.
*      IF sy-subrc = 0.
*        DATA(ls_msg) = reject_order( iv_vbeln = <ls_list>-vbeln iv_posnr = <ls_list>-posnr ).
*        IF ls_msg IS NOT INITIAL.
*          <ls_list>-statu = '1'.
*          <ls_list>-message = ls_msg-message.
*          IF sy-batch = abap_true.
*            WRITE:/ icon_led_red AS ICON, <ls_list>-vbeln, <ls_list>-posnr, ls_msg-message.
*          ENDIF.
*        ELSE.
*          <ls_list>-statu = '3'.
*          IF sy-batch = abap_true.
*            WRITE:/ icon_led_green AS ICON, <ls_list>-vbeln, <ls_list>-posnr, ls_msg-message.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.

  ENDMETHOD.

  METHOD display_grid.

    DATA lt_fcat TYPE lvc_t_fcat.

    IF o_grid IS NOT BOUND.

      CREATE OBJECT o_grid
        EXPORTING
          i_lifetime        = cl_gui_control=>lifetime_dynpro
          i_parent          = cl_gui_custom_container=>screen0
          i_appl_events     = abap_true "Triggers PAI/PBO on alv events
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.

      lt_fcat = build_fcat( t_list ).
      lt_fcat = change_fcat( lt_fcat ).
      exclude_toolbar_functions( ).

*    SET HANDLER lcl_controller=>on_toolbar            FOR o_grid.
*    SET HANDLER lcl_controller=>on_after_user_command FOR o_grid.
*    SET HANDLER lcl_controller=>on_double_click       FOR o_grid.

      s_layo-cwidth_opt = abap_true.
      s_layo-sel_mode   = 'B'.
      s_layo-no_rowmark = abap_true.
*    s_layo-info_fname = 'ROWCOLOR'.
      s_layo-excp_fname = 'STATU'.
      s_layo-excp_rolln = 'VVIS_LIGHTS'.
      s_layo-excp_led   = abap_true.

      o_grid->set_table_for_first_display(
        EXPORTING
          is_layout                     = s_layo
          it_toolbar_excluding          = t_excl
        CHANGING
          it_outtab                     = t_list
          it_fieldcatalog               = lt_fcat
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4 ).

    ELSE.

      o_grid->set_frontend_layout( s_layo ).

      DATA ls_stbl TYPE lvc_s_stbl.
      ls_stbl-row = abap_true.
      ls_stbl-col = abap_true.

      o_grid->refresh_table_display(
        EXPORTING
          is_stable      = ls_stbl
        EXCEPTIONS
          finished       = 1
          OTHERS         = 2 ).

    ENDIF.

  ENDMETHOD.

  METHOD build_fcat.
    DATA: lo_salv TYPE REF TO cl_salv_table,
          lr_list TYPE REF TO data,
          lo_cols TYPE REF TO cl_salv_columns_table,
          lo_aggs TYPE REF TO cl_salv_aggregations.
    FIELD-SYMBOLS: <lt_list> TYPE ANY TABLE,
                   <ls_fcat> LIKE LINE OF rt_fcat.
    CREATE DATA lr_list LIKE it_list.
    ASSIGN lr_list->* TO <lt_list>.
    cl_salv_table=>factory(
      IMPORTING
        r_salv_table = lo_salv
      CHANGING
        t_table      = <lt_list> ).
    lo_cols = lo_salv->get_columns( ).
    lo_aggs = lo_salv->get_aggregations( ).
    rt_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
          r_columns      = lo_cols
          r_aggregations = lo_aggs ).
    LOOP AT rt_fcat ASSIGNING <ls_fcat>.
      <ls_fcat>-no_sign = abap_false.
    ENDLOOP.
  ENDMETHOD.

  METHOD change_fcat.
    FIELD-SYMBOLS <ls_fcat> LIKE LINE OF rt_fcat.
    rt_fcat = it_fcat.
    LOOP AT rt_fcat ASSIGNING <ls_fcat>.
      CASE <ls_fcat>-fieldname.
        WHEN 'MENGE'.
          <ls_fcat>-edit = abap_true.
*        WHEN 'KWMENG_TO'.
*          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c01.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD exclude_toolbar_functions.
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
  ENDMETHOD.

  METHOD on_toolbar.

  ENDMETHOD.

  METHOD on_after_user_command.

  ENDMETHOD.

  METHOD on_double_click.

  ENDMETHOD.

ENDCLASS.
MODULE pbo OUTPUT.
  lcl_controller=>pbo( ).
ENDMODULE.
MODULE pai INPUT.
  lcl_controller=>pai( ).
ENDMODULE.