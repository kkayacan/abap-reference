REPORT zsd_siparis_kapatma.

CLASS lcl_controller DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      initialization,
      at_selection_screen,
      start_of_selection,
      end_of_selection,
      pbo,
      pai.

  PRIVATE SECTION.

    TYPES: BEGIN OF st_list,
             statu      TYPE vvis_lights,
             vbeln      TYPE vbap-vbeln,
             posnr      TYPE vbap-posnr,
             matnr      TYPE vbap-matnr,
             maktx      TYPE makt-maktx,
             kwmeng     TYPE vbap-kwmeng,
             vrkme      TYPE vbap-vrkme,
             lfimg      TYPE lips-lfimg,
             lfime      TYPE lips-vrkme,
             zztolerans TYPE zsd_118_t001-zztolerans,
             kwmeng_t   TYPE vbap-kwmeng,
             kwmeng_to  TYPE vbap-kwmeng,
             message    TYPE bapiret2-message,
           END OF st_list.

    CLASS-DATA:
      t_list TYPE TABLE OF st_list,
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
          it_list        TYPE ANY TABLE
        RETURNING
          VALUE(rt_fcat) TYPE lvc_t_fcat,
      change_fcat
        IMPORTING
          it_fcat        TYPE lvc_t_fcat
        RETURNING
          VALUE(rt_fcat) TYPE lvc_t_fcat,
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,
      on_after_user_command FOR EVENT after_user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no.

    CLASS-METHODS:
      reject_order
        IMPORTING
          iv_vbeln      TYPE vbap-vbeln
          iv_posnr      TYPE vbap-posnr
        RETURNING
          VALUE(rs_msg) TYPE bapiret2.

ENDCLASS.

TABLES: sscrfields, vbak.

SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-s01.
SELECT-OPTIONS: s_auart FOR vbak-auart,
                s_audat FOR vbak-audat,
                s_vbeln FOR vbak-vbeln.
SELECTION-SCREEN END OF BLOCK b01.

INITIALIZATION.
  lcl_controller=>initialization( ).

AT SELECTION-SCREEN.
  lcl_controller=>at_selection_screen( ).

START-OF-SELECTION.
  lcl_controller=>start_of_selection( ).

END-OF-SELECTION.
  lcl_controller=>end_of_selection( ).

CLASS lcl_controller IMPLEMENTATION.

  METHOD initialization.
    DATA ls_functxt TYPE smp_dyntxt.
    ls_functxt-icon_id   = icon_change_number.
    ls_functxt-quickinfo = TEXT-s02.
    ls_functxt-icon_text = TEXT-s02.
    sscrfields-functxt_01   = ls_functxt.
  ENDMETHOD.

  METHOD at_selection_screen.
    CASE sscrfields-ucomm.
      WHEN'FC01'.
        CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
          EXPORTING
            action    = 'U'
            view_name = 'ZSD_118_T001'
          EXCEPTIONS
            OTHERS    = 15.
    ENDCASE.
  ENDMETHOD.

  METHOD start_of_selection.

    SELECT vbap~vbeln, vbap~posnr, vbap~matnr, makt~maktx, vbap~kwmeng, vbap~vrkme,
           SUM( lips~lfimg ) AS lfimg, lips~vrkme AS lfime
      FROM vbap
      JOIN vbak ON vbak~vbeln = vbap~vbeln
      JOIN lips ON lips~vgbel = vbap~vbeln
               AND lips~vgpos = vbap~posnr
      JOIN likp ON likp~vbeln = lips~vbeln
               AND likp~wbstk = 'C'
      LEFT OUTER JOIN makt ON makt~matnr = vbap~matnr
                          AND makt~spras = @sy-langu
      WHERE vbak~audat IN @s_audat
      AND   vbak~auart IN @s_auart
      AND   vbak~vbeln IN @s_vbeln
      AND   vbak~vbtyp  = 'C'
      AND   vbap~gbsta <> 'C'
      AND   vbap~sobkz  = @space
      GROUP BY vbap~vbeln, vbap~posnr, vbap~matnr, makt~maktx, vbap~kwmeng, vbap~vrkme, lips~vrkme
      INTO CORRESPONDING FIELDS OF TABLE @t_list.
    IF t_list IS INITIAL.
      MESSAGE s004(sv) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    SELECT * FROM zsd_118_t001 INTO TABLE @DATA(lt_tlrn).
    SORT lt_tlrn BY zzkgmik_bas.

    LOOP AT t_list ASSIGNING FIELD-SYMBOL(<ls_list>).
      LOOP AT lt_tlrn ASSIGNING FIELD-SYMBOL(<ls_tlrn>) WHERE zzkgmik_bas   <= <ls_list>-kwmeng
                                                        AND   zzkgmik_bitis >= <ls_list>-kwmeng.
        <ls_list>-zztolerans = <ls_tlrn>-zztolerans.
        <ls_list>-kwmeng_t   = <ls_list>-kwmeng * <ls_list>-zztolerans / 100.
        <ls_list>-kwmeng_to  = <ls_list>-kwmeng - <ls_list>-kwmeng_t.
        EXIT.
      ENDLOOP.
      IF <ls_list>-lfimg < <ls_list>-kwmeng_to.
        DELETE t_list.
        CONTINUE.
      ENDIF.
    ENDLOOP.
    IF t_list IS INITIAL.
      MESSAGE s004(sv) DISPLAY LIKE 'E'.
      RETURN.
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
      WHEN abap_true.
        DATA(lv_lines) = lines( t_list ).
        DO lv_lines TIMES.
          APPEND VALUE #( row_id = sy-index ) TO lt_roid.
        ENDDO.
    ENDCASE.

    LOOP AT lt_roid ASSIGNING <ls_roid>.
      READ TABLE t_list ASSIGNING <ls_list> INDEX <ls_roid>-row_id.
      IF sy-subrc = 0.
        DATA(ls_msg) = reject_order( iv_vbeln = <ls_list>-vbeln iv_posnr = <ls_list>-posnr ).
        IF ls_msg IS NOT INITIAL.
          <ls_list>-statu = '1'.
          <ls_list>-message = ls_msg-message.
          IF sy-batch = abap_true.
            WRITE:/ icon_led_red AS ICON, <ls_list>-vbeln, <ls_list>-posnr, ls_msg-message.
          ENDIF.
        ELSE.
          <ls_list>-statu = '3'.
          IF sy-batch = abap_true.
            WRITE:/ icon_led_green AS ICON, <ls_list>-vbeln, <ls_list>-posnr, ls_msg-message.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

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
      ls_layo-excp_fname = 'STATU'.
      ls_layo-excp_rolln = 'VVIS_LIGHTS'.
      ls_layo-excp_led   = abap_true.

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
        WHEN 'KWMENG_T'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c02.
        WHEN 'KWMENG_TO'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c01.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD on_toolbar.

  ENDMETHOD.

  METHOD on_after_user_command.

  ENDMETHOD.

  METHOD on_double_click.

  ENDMETHOD.

  METHOD reject_order.

    DATA: lt_return TYPE TABLE OF bapiret2,
          lt_item   TYPE TABLE OF bapisditm,
          lt_itemx  TYPE TABLE OF bapisditmx.

    lt_item  = VALUE #( ( itm_number = iv_posnr reason_rej = 'ZY' ) ).
    lt_itemx = VALUE #( ( itm_number = iv_posnr updateflag = 'U' reason_rej = abap_true ) ).

    CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
      EXPORTING
        salesdocument    = iv_vbeln
        order_header_inx = VALUE bapisdh1x( updateflag = 'U' )
      TABLES
        return           = lt_return
        order_item_in    = lt_item
        order_item_inx   = lt_itemx.

    LOOP AT lt_return INTO rs_msg WHERE type = 'E'.
    ENDLOOP.
    IF rs_msg IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
MODULE pbo OUTPUT.
  lcl_controller=>pbo( ).
ENDMODULE.
MODULE pai INPUT.
  lcl_controller=>pai( ).
ENDMODULE.