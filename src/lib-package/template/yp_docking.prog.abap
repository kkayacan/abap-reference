REPORT yp_docking.

CLASS lcl_events DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,
      on_after_user_command FOR EVENT after_user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no.
ENDCLASS.

TYPES: gvt_rowcolor TYPE c LENGTH 4,
       BEGIN OF gst_list.
    INCLUDE TYPE bkpf.
TYPES: rowcolor TYPE gvt_rowcolor,
       END OF gst_list.

CONSTANTS c_selected_row TYPE gvt_rowcolor VALUE 'C600'.

TABLES: bkpf, bseg, t001.

DATA: gt_list TYPE TABLE OF gst_list,
      go_dock TYPE REF TO cl_gui_docking_container,
      go_grid TYPE REF TO cl_gui_alv_grid.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-s01.
SELECT-OPTIONS s_budat FOR bkpf-budat.
SELECTION-SCREEN END OF BLOCK b01.

INITIALIZATION.
  PERFORM initialize.

START-OF-SELECTION.
  PERFORM retrieve_data.

END-OF-SELECTION.
  CALL SCREEN 0001.

FORM initialize.
*  SELECT 'I' AS sign, 'EQ' AS option, MAX( budat ) AS low "7.40
*    FROM bkpf INTO TABLE @s_budat.                        "7.40
  SELECT MAX( budat ) FROM  bkpf INTO s_budat-low.         "7.02
  s_budat-sign   = 'I'.                                    "7.02
  s_budat-option = 'EQ'.                                   "7.02
  APPEND s_budat.                                          "7.02
ENDFORM.

FORM retrieve_data.

  SELECT * FROM  bkpf INTO TABLE gt_list
         WHERE  budat IN s_budat.

ENDFORM.

MODULE pbo OUTPUT.
*  DATA(title) = sy-title.  "7.40
  DATA title TYPE sy-title. "7.02
  title = sy-title.         "7.02
  SET TITLEBAR 'TITLE' WITH title.
  SET PF-STATUS 'STATUS'.
  PERFORM display_grid.
ENDMODULE.

MODULE pai INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'. SET SCREEN 0.
    WHEN 'EXIT'. LEAVE PROGRAM.
    WHEN 'CANC'. LEAVE PROGRAM.
    WHEN 'PREV'. PERFORM change_selection USING '-'.
    WHEN 'NEXT'. PERFORM change_selection USING '+'.
  ENDCASE.
ENDMODULE.

FORM display_grid.

  DATA lt_fcat TYPE lvc_t_fcat.

  IF go_grid IS NOT BOUND.

    CREATE OBJECT go_dock
      EXPORTING
        side                        = cl_gui_docking_container=>dock_at_bottom
        extension                   = 150
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
        i_parent          = go_dock
        i_appl_events     = abap_true "Triggers PAI/PBO on alv events
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    PERFORM build_fcat USING gt_list CHANGING lt_fcat.
    PERFORM change_fcat CHANGING lt_fcat.

    SET HANDLER lcl_events=>on_toolbar            FOR go_grid.
    SET HANDLER lcl_events=>on_after_user_command FOR go_grid.
    SET HANDLER lcl_events=>on_double_click       FOR go_grid.

    DATA ls_layo TYPE lvc_s_layo.    "7.02
    ls_layo-cwidth_opt = abap_true.  "7.02
    ls_layo-sel_mode   = ''.         "7.02
    ls_layo-info_fname = 'ROWCOLOR'. "7.02

    go_grid->set_table_for_first_display(
      EXPORTING
*        is_layout                     = VALUE #( cwidth_opt = abap_true     "7.40
*                                                 sel_mode   = ''            "7.40
*                                                 info_fname = 'ROWCOLOR'  ) "7.40
        is_layout                     = ls_layo                              "7.02
      CHANGING
        it_outtab                     = gt_list
        it_fieldcatalog               = lt_fcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4 ).

  ELSE.

    DATA ls_stbl TYPE lvc_s_stbl. "7.02
    ls_stbl-row = abap_true.      "7.02
    ls_stbl-col = abap_true.      "7.02

    go_grid->refresh_table_display(
      EXPORTING
*        is_stable      = VALUE #( row = abap_true col = abap_true ) "7.40
        is_stable      = ls_stbl                                     "7.02
      EXCEPTIONS
        finished       = 1
        OTHERS         = 2 ).

  ENDIF.

ENDFORM.

FORM build_fcat USING it_list TYPE ANY TABLE
             CHANGING ct_fcat TYPE lvc_t_fcat.
  DATA lo_salv TYPE REF TO cl_salv_table. "7.02
  cl_salv_table=>factory(
    IMPORTING
*      r_salv_table = DATA(lo_salv)       "7.40
      r_salv_table = lo_salv              "7.02
    CHANGING
      t_table      = it_list ).

  ct_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
        r_columns      = lo_salv->get_columns( )
        r_aggregations = lo_salv->get_aggregations( ) ).

  FIELD-SYMBOLS <ls_fcat> LIKE LINE OF ct_fcat.       "7.02
*  LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>). "7.40
  LOOP AT ct_fcat ASSIGNING <ls_fcat>.                "7.02
    <ls_fcat>-no_sign = abap_false.
  ENDLOOP.
ENDFORM.

FORM change_fcat CHANGING ct_fcat TYPE lvc_t_fcat.
  DEFINE change_text.
    <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m  = <ls_fcat>-scrtext_l = <ls_fcat>-reptext = &1.
  END-OF-DEFINITION.
  FIELD-SYMBOLS <ls_fcat> LIKE LINE OF ct_fcat.       "7.02
*  LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>). "7.40
  LOOP AT ct_fcat ASSIGNING <ls_fcat>.                "7.02
    CASE <ls_fcat>-fieldname.
      WHEN 'MANDT'. <ls_fcat>-tech = abap_true.
      WHEN 'GJAHR'. change_text 'Year'(c01).
    ENDCASE.
  ENDLOOP.
ENDFORM.

CLASS lcl_events IMPLEMENTATION.

  METHOD on_toolbar.
    PERFORM add_buttons USING e_object e_interactive.
  ENDMETHOD.

  METHOD on_after_user_command.
    CASE e_ucomm.
      WHEN 'FILT'. PERFORM set_filter.
      WHEN 'CLFL'. PERFORM clear_filter.
    ENDCASE.
  ENDMETHOD.

  METHOD on_double_click.
    PERFORM color_line USING es_row_no-row_id.
    PERFORM retrieve_detail USING es_row_no-row_id.
  ENDMETHOD.

ENDCLASS.

FORM add_buttons USING io_object TYPE REF TO cl_alv_event_toolbar_set
                          ip_interactive TYPE char01.
*  APPEND VALUE #( function = 'FILT'                                           "7.40
*                  butn_type = 0                                               "7.40
*                  icon      = icon_filter                                     "7.40
*                  quickinfo = 'Quick filter'(t01)                             "7.40
*                  text      = 'Quick filter'(t01) ) TO io_object->mt_toolbar. "7.40
*  APPEND VALUE #( function = 'CLFL'                                           "7.40
*                  butn_type = 0                                               "7.40
*                  icon      = icon_filter_undo                                "7.40
*                  quickinfo = 'Clear filter'(t02)                             "7.40
*                  text      = 'Clear filter'(t02) ) TO io_object->mt_toolbar. "7.40
  DATA ls_toolbar LIKE LINE OF io_object->mt_toolbar.                          "7.02
  ls_toolbar-function  = 'FILT'.                                               "7.02
  ls_toolbar-butn_type = 0.                                                    "7.02
  ls_toolbar-icon      = icon_filter.                                          "7.02
  ls_toolbar-quickinfo = 'Quick filter'(t01).                                  "7.02
  ls_toolbar-function  = 'Quick filter'(t01).                                  "7.02
  APPEND ls_toolbar TO io_object->mt_toolbar.                                  "7.02
  ls_toolbar-function  = 'CLFL'.                                               "7.02
  ls_toolbar-butn_type = 0.                                                    "7.02
  ls_toolbar-icon      = icon_filter_undo.                                     "7.02
  ls_toolbar-quickinfo = 'Clear filter'(t02).                                  "7.02
  ls_toolbar-function  = 'Clear filter'(t02).                                  "7.02
  APPEND ls_toolbar TO io_object->mt_toolbar.                                  "7.02
ENDFORM.

FORM set_filter.

  DATA: lt_fields TYPE TABLE OF sval,
        lv_rc     TYPE c LENGTH 1,
        lt_filt   TYPE lvc_t_filt.

*  lt_fields = VALUE #( ( tabname = 'BKPF' fieldname = 'BKTXT' ) ). "7.40
  DATA ls_field LIKE LINE OF lt_fields.                             "7.02
  ls_field-tabname   = 'BKPF'.                                      "7.02
  ls_field-fieldname = 'BKTXT'.                                     "7.02
  APPEND ls_field TO lt_fields.                                     "7.02

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = TEXT-t01
    IMPORTING
      returncode      = lv_rc
    TABLES
      fields          = lt_fields
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  IF lv_rc = 'A'.
    RETURN.
  ENDIF.

  FIELD-SYMBOLS <ls_field> LIKE LINE OF lt_fields.       "7.02
  DATA ls_filt LIKE LINE OF lt_filt.                     "7.02
*  LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_field>). "7.40
  LOOP AT lt_fields ASSIGNING <ls_field>.                "7.02
    CHECK <ls_field>-value IS NOT INITIAL.
*    APPEND VALUE #( fieldname = <ls_field>-fieldname                   "7.40
*                    sign      = 'I'                                    "7.40
*                    option    = 'CP'                                   "7.40
*                    low       = |*{ <ls_field>-value }*| ) TO lt_filt. "7.40
    ls_filt-fieldname = <ls_field>-fieldname.     "7.02
    ls_filt-sign      = 'I'.                      "7.02
    ls_filt-option    = 'CP'.                     "7.02
    ls_filt-low       = |*{ <ls_field>-value }*|. "7.02
    APPEND ls_filt TO lt_filt.                    "7.02
  ENDLOOP.

  go_grid->set_filter_criteria(
    EXPORTING
      it_filter = lt_filt
    EXCEPTIONS
      no_fieldcatalog_available = 1
      OTHERS                    = 2 ).

  go_grid->refresh_table_display(
    EXCEPTIONS
      finished       = 1
      OTHERS         = 2 ).

ENDFORM.

FORM clear_filter.

  DATA lt_filt   TYPE lvc_t_filt.

  go_grid->set_filter_criteria(
    EXPORTING
      it_filter = lt_filt
    EXCEPTIONS
      no_fieldcatalog_available = 1
      OTHERS                    = 2 ).

  go_grid->refresh_table_display(
    EXCEPTIONS
      finished       = 1
      OTHERS         = 2 ).

ENDFORM.

FORM change_selection USING ip_direction.
*  DATA(lv_tabix) = line_index( gt_list[ rowcolor = c_selected_row ] ).         "7.40
  DATA lv_tabix TYPE sy-tabix.                                                  "7.02
  READ TABLE gt_list TRANSPORTING NO FIELDS WITH KEY rowcolor = c_selected_row. "7.02
  lv_tabix = sy-tabix.                                                          "7.02
  CASE ip_direction.
    WHEN '-'.
      IF lv_tabix = 1.
        RETURN.
      ENDIF.
      SUBTRACT 1 FROM lv_tabix.
    WHEN '+'.
      IF lv_tabix = lines( gt_list ).
        RETURN.
      ENDIF.
      ADD 1 TO lv_tabix.
  ENDCASE.
  PERFORM color_line USING lv_tabix.
  PERFORM retrieve_detail USING lv_tabix.
ENDFORM.

FORM color_line USING ip_tabix.
  DATA ls_list LIKE LINE OF gt_list.
  MODIFY gt_list FROM ls_list TRANSPORTING rowcolor WHERE rowcolor IS NOT INITIAL.
  ls_list-rowcolor = c_selected_row.
  MODIFY gt_list FROM ls_list INDEX ip_tabix TRANSPORTING rowcolor.
  DATA ls_stbl TYPE lvc_s_stbl. "7.02
  ls_stbl-row = abap_true.      "7.02
  ls_stbl-col = abap_true.      "7.02
  go_grid->refresh_table_display(
    EXPORTING
*      is_stable      = VALUE #( row = abap_true col = abap_true ) "7.40
      is_stable      = ls_stbl                                     "7.02
    EXCEPTIONS
      finished       = 1
      OTHERS         = 2 ).
ENDFORM.

FORM retrieve_detail USING ip_tabix.
  DATA ls_bseg TYPE bseg.
  CLEAR: bkpf, bseg, t001.
*  ASSIGN gt_list[ ip_tabix ] TO FIELD-SYMBOL(<ls_list>). "7.40
  FIELD-SYMBOLS <ls_list> LIKE LINE OF gt_list.           "7.02
  READ TABLE gt_list ASSIGNING <ls_list> INDEX ip_tabix.  "7.02
  SELECT dmbtr wrbtr
    FROM bseg INTO CORRESPONDING FIELDS OF ls_bseg
    WHERE bukrs = <ls_list>-bukrs
    AND   belnr = <ls_list>-belnr
    AND   gjahr = <ls_list>-gjahr
    AND   shkzg = 'S'.
    ADD ls_bseg-wrbtr TO bseg-wrbtr.
    ADD ls_bseg-dmbtr TO bseg-dmbtr.
  ENDSELECT.
  SELECT SINGLE bkpf~waers t001~waers
    FROM bkpf
    JOIN t001 ON t001~bukrs = bkpf~bukrs
    INTO (bkpf-waers,t001-waers)
    WHERE bkpf~bukrs = <ls_list>-bukrs
    AND   bkpf~belnr = <ls_list>-belnr
    AND   bkpf~gjahr = <ls_list>-gjahr.
ENDFORM.
