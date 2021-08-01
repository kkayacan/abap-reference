REPORT  zhr_sf_send_quota.

*----------------------------------------------------------------------*
*       CLASS lcl_controller DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_controller DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
    initialization,
    start_of_selection,
    end_of_selection,
    pbo,
    pai.

  PRIVATE SECTION.

    CLASS-DATA:
    t_list TYPE zcl_hr_sf_quota_integration=>tt_list,
    o_grid TYPE REF TO cl_gui_alv_grid.

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
        it_list TYPE ANY TABLE
      RETURNING
        value(rt_fcat) TYPE lvc_t_fcat,
    change_fcat
      IMPORTING
        it_fcat TYPE lvc_t_fcat
      RETURNING
        value(rt_fcat) TYPE lvc_t_fcat,
    move_col
      IMPORTING
        it_fcat  TYPE lvc_t_fcat
        ip_col   TYPE simple
        ip_after TYPE simple
      RETURNING
        value(rt_fcat) TYPE lvc_t_fcat,
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,
      on_after_user_command FOR EVENT after_user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no.

ENDCLASS.                    "lcl_controller DEFINITION

TABLES zhr_sf_2006bilgi.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-s01.
SELECT-OPTIONS: s_pernr FOR zhr_sf_2006bilgi-pernr MATCHCODE OBJECT prem,
                s_erdat FOR zhr_sf_2006bilgi-erdat,
                s_statu FOR zhr_sf_2006bilgi-sf_status.
SELECTION-SCREEN END OF BLOCK b01.

INITIALIZATION.
  lcl_controller=>initialization( ).

START-OF-SELECTION.
  lcl_controller=>start_of_selection( ).

END-OF-SELECTION.
  lcl_controller=>end_of_selection( ).

*----------------------------------------------------------------------*
*       CLASS lcl_controller IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_controller IMPLEMENTATION.

  METHOD initialization.

  ENDMETHOD.                    "initialization

  METHOD start_of_selection.
    DATA: ls_filters TYPE zcl_hr_sf_quota_integration=>st_filters.
    ls_filters-pernr     = s_pernr[].
    ls_filters-erdat     = s_erdat[].
    ls_filters-sf_status = s_statu[].
    t_list = zcl_hr_sf_quota_integration=>get_list( ls_filters ).
    IF t_list IS INITIAL.
      MESSAGE s004(sv) DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.                    "start_of_selection

  METHOD end_of_selection.
    CHECK t_list IS NOT INITIAL.
    CASE sy-batch.
      WHEN abap_false. CALL SCREEN 0001.
      WHEN abap_true.  save( abap_true ).
    ENDCASE.
  ENDMETHOD.                    "end_of_selection

  METHOD pbo.
    set_gui( ).
    display_grid( ).
  ENDMETHOD.                    "pbo

  METHOD pai.
    CASE sy-dynnr.
      WHEN '0001'.
        CASE sy-ucomm.
          WHEN 'BACK'. SET SCREEN 0.
          WHEN 'EXIT'. LEAVE PROGRAM.
          WHEN 'CANC'. LEAVE PROGRAM.
          WHEN 'SAVE'. save( ).
        ENDCASE.
    ENDCASE.
  ENDMETHOD.                    "pai

  METHOD set_gui.
    DATA title TYPE sy-title.
    title = sy-title.
    CASE sy-dynnr.
      WHEN '0001'.
        SET TITLEBAR 'TITLE' WITH title.
        SET PF-STATUS 'STATUS'.
    ENDCASE.
  ENDMETHOD.                    "set_gui

  METHOD save.
    DATA: lt_roid TYPE lvc_t_roid,
          ls_data   TYPE zhr_sf_2006bilgi_d,
          ls_result TYPE zhr_sf_2006bilgi_r.
    FIELD-SYMBOLS: <ls_roid> LIKE LINE OF lt_roid,
                   <ls_list> LIKE LINE OF t_list.
    CASE ip_all.
      WHEN abap_false.
        o_grid->get_selected_rows(
          IMPORTING
            et_row_no     = lt_roid ).
        LOOP AT lt_roid ASSIGNING <ls_roid>.
          READ TABLE t_list ASSIGNING <ls_list> INDEX <ls_roid>-row_id.
          IF sy-subrc = 0.
            IF <ls_list>-sf_status = '0'.
              MOVE-CORRESPONDING <ls_list> TO ls_data.
              ls_result = zcl_hr_sf_quota_integration=>send( ip_pernr  = <ls_list>-pernr
                                                             ip_subty  = <ls_list>-subty
                                                             ip_begda  = <ls_list>-begda
                                                             ip_endda  = <ls_list>-endda
                                                             is_data   = ls_data ).
              MOVE-CORRESPONDING ls_result TO <ls_list>.
              <ls_list>-sf_statusx = zcl_hr_sf_quota_integration=>get_val_text( ip_domain = 'ZHR_SF_STATUS' ip_value = <ls_list>-sf_status ).
            ENDIF.
          ENDIF.
        ENDLOOP.
      WHEN abap_true.
        LOOP AT t_list ASSIGNING <ls_list>.
          IF <ls_list>-sf_status = '0'.
            MOVE-CORRESPONDING <ls_list> TO ls_data.
            ls_result = zcl_hr_sf_quota_integration=>send( ip_pernr  = <ls_list>-pernr
                                                           ip_subty  = <ls_list>-subty
                                                           ip_begda  = <ls_list>-begda
                                                           ip_endda  = <ls_list>-endda
                                                           is_data   = ls_data ).
            MOVE-CORRESPONDING ls_result TO <ls_list>.
            <ls_list>-sf_statusx = zcl_hr_sf_quota_integration=>get_val_text( ip_domain = 'ZHR_SF_STATUS' ip_value = <ls_list>-sf_status ).
          ENDIF.
        ENDLOOP.
    ENDCASE.
  ENDMETHOD.                    "save

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

*    SET HANDLER lcl_controller=>on_toolbar            FOR o_grid.
*    SET HANDLER lcl_controller=>on_after_user_command FOR o_grid.
*    SET HANDLER lcl_controller=>on_double_click       FOR o_grid.

      DATA ls_layo TYPE lvc_s_layo.
      ls_layo-cwidth_opt = abap_true.
      ls_layo-sel_mode   = 'A'.
*    ls_layo-info_fname = 'ROWCOLOR'.

      o_grid->set_table_for_first_display(
        EXPORTING
          is_layout                     = ls_layo
        CHANGING
          it_outtab                     = t_list
          it_fieldcatalog               = lt_fcat
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4 ).

    ELSE.

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

  ENDMETHOD.                    "display_grid

  METHOD build_fcat.
    DATA: lo_salv    TYPE REF TO cl_salv_table,
          lr_list    TYPE REF TO data,
          lo_cols    TYPE REF TO cl_salv_columns_table,
          lo_aggs    TYPE REF TO cl_salv_aggregations,
          lv_col_pos TYPE lvc_s_fcat-col_pos.
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
      ADD 1 TO lv_col_pos.
      <ls_fcat>-col_pos = lv_col_pos.
      <ls_fcat>-no_sign = abap_false.
    ENDLOOP.
  ENDMETHOD.                    "build_fcat

  METHOD change_fcat.
    FIELD-SYMBOLS <ls_fcat> LIKE LINE OF rt_fcat.
    rt_fcat = it_fcat.
    LOOP AT rt_fcat ASSIGNING <ls_fcat>.
      CASE <ls_fcat>-fieldname.
        WHEN 'MANDT'. <ls_fcat>-tech = abap_true.
      ENDCASE.
    ENDLOOP.
    rt_fcat = move_col( it_fcat = rt_fcat ip_col = 'RECORD_STATUSX' ip_after = 'RECORD_STATUS' ).
    rt_fcat = move_col( it_fcat = rt_fcat ip_col = 'SF_STATUSX'     ip_after = 'SF_STATUS' ).
  ENDMETHOD.                    "change_fcat

  METHOD move_col.
    DATA: lv_col_pos TYPE lvc_s_fcat-col_pos.
    FIELD-SYMBOLS <ls_fcat> LIKE LINE OF rt_fcat.
    rt_fcat = it_fcat.

    READ TABLE rt_fcat ASSIGNING <ls_fcat> WITH KEY fieldname = ip_after.
    IF sy-subrc = 0.
      lv_col_pos = <ls_fcat>-col_pos.
      LOOP AT rt_fcat ASSIGNING <ls_fcat> WHERE col_pos > lv_col_pos.
        ADD 1 TO <ls_fcat>-col_pos.
      ENDLOOP.
      READ TABLE rt_fcat ASSIGNING <ls_fcat> WITH KEY fieldname = ip_col.
      IF sy-subrc = 0.
        <ls_fcat>-col_pos = lv_col_pos + 1.
      ENDIF.
    ENDIF.

    SORT rt_fcat BY col_pos.

  ENDMETHOD.                    "move_col

  METHOD on_toolbar.

  ENDMETHOD.                    "on_toolbar

  METHOD on_after_user_command.

  ENDMETHOD.                    "on_after_user_command

  METHOD on_double_click.

  ENDMETHOD.                    "on_double_click

ENDCLASS.                    "lcl_controller IMPLEMENTATION
*----------------------------------------------------------------------*
*  MODULE pbo OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE pbo OUTPUT.
  lcl_controller=>pbo( ).
ENDMODULE.                    "pbo OUTPUT
*----------------------------------------------------------------------*
*  MODULE pai INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE pai INPUT.
  lcl_controller=>pai( ).
ENDMODULE.                    "pai INPUT