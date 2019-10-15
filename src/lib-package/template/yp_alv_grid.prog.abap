REPORT  yp_alv_grid.

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
    t_list TYPE TABLE OF mara,
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
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,
      on_after_user_command FOR EVENT after_user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no.

ENDCLASS.

TABLES mara.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-s01.
SELECT-OPTIONS: s_mtart FOR mara-mtart.
SELECTION-SCREEN END OF BLOCK b01.

INITIALIZATION.
  lcl_controller=>initialization( ).

START-OF-SELECTION.
  lcl_controller=>start_of_selection( ).

END-OF-SELECTION.
  lcl_controller=>end_of_selection( ).

CLASS lcl_controller IMPLEMENTATION.

  METHOD initialization.

  ENDMETHOD.

  METHOD start_of_selection.
    SELECT * FROM  mara INTO TABLE t_list UP TO 25 ROWS
           WHERE  mtart IN s_mtart.
    IF t_list IS INITIAL.
      MESSAGE s004(sv) DISPLAY LIKE 'E'.
    ENDIF.
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
    DATA: lt_roid TYPE lvc_t_roid.
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
            "save data
          ENDIF.
        ENDLOOP.
      WHEN abap_true.
        LOOP AT t_list ASSIGNING <ls_list>.
          "save data
        ENDLOOP.
    ENDCASE.
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
        WHEN 'MANDT'. <ls_fcat>-tech = abap_true.
      ENDCASE.
    ENDLOOP.
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
