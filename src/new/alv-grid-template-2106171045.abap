*&---------------------------------------------------------------------*
*& Report ZGI_FI_NAKIT_AKIS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgi_fi_nakit_akis.

CLASS lcl_controller DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      initialization,
      at_selection_screen_output,
      at_selection_screen
        RETURNING
          VALUE(error) TYPE xfeld,
      start_of_selection,
      end_of_selection,
      pbo,
      pai.

  PRIVATE SECTION.

    TYPES: BEGIN OF st_list,
             lifnr       TYPE lfa1-lifnr,
             name1       TYPE lfa1-name1,
             belnr       TYPE bkpf-belnr,
             bldat       TYPE bkpf-bldat,
             budat       TYPE bkpf-budat,
             vade_tarihi TYPE bseg-zfbdt,
             vade_gun    TYPE i,
             gun         TYPE i,
             toplam_gun  TYPE i,
             xref3       TYPE bseg-xref3,
             wrbtr       TYPE bseg-wrbtr,
             waers       TYPE bkpf-waers,
             dmbtr       TYPE bseg-dmbtr,
             sgtxt       TYPE bseg-sgtxt,
             umskz       TYPE bseg-umskz,
             shkzg       TYPE bseg-shkzg,
             zfbdt       TYPE bseg-zfbdt,
             zbd1t       TYPE bseg-zbd1t,
             zbd2t       TYPE bseg-zbd2t,
             zbd3t       TYPE bseg-zbd3t,
             rebzg       TYPE bseg-rebzg,
           END OF st_list.

    CLASS-DATA:
      t_list TYPE TABLE OF st_list,
      o_grid TYPE REF TO cl_gui_alv_grid,
      r_list TYPE REF TO data.

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
      get_stcdt,
      get_stcoz,
      get_mstoz.

ENDCLASS.

TABLES: sscrfields, bseg.

*SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-s01.
  PARAMETERS: p_stcdt RADIOBUTTON GROUP r01 DEFAULT 'X' USER-COMMAND rb,
              p_stcoz RADIOBUTTON GROUP r01,
              p_mstoz RADIOBUTTON GROUP r01.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-s02.
  PARAMETERS: p_bukrs TYPE t001-bukrs MEMORY ID buk,
              p_gjahr TYPE bkpf-gjahr MEMORY ID gjr,
              p_keydt TYPE bkpf-budat MODIF ID det.
  SELECT-OPTIONS: s_belnr FOR bseg-belnr MODIF ID std,
                  s_umskz FOR bseg-umskz MODIF ID std,
                  s_lifnr FOR bseg-lifnr MODIF ID stc,
                  s_kunnr FOR bseg-kunnr MODIF ID mst.
*  SELECTION-SCREEN BEGIN OF LINE.
*    SELECTION-SCREEN COMMENT 1(31) TEXT-s03 FOR FIELD p_gjahl MODIF ID sto.
*    PARAMETERS: p_gjahl TYPE bkpf-gjahr MODIF ID sto,
*                p_monal TYPE bkpf-monat MODIF ID sto.
*    SELECTION-SCREEN COMMENT 52(5) TEXT-s04 FOR FIELD p_gjahh MODIF ID sto.
*    PARAMETERS: p_gjahh TYPE bkpf-gjahr MODIF ID sto,
*                p_monah TYPE bkpf-monat MODIF ID sto.
*  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b02.

INITIALIZATION.
  lcl_controller=>initialization( ).

AT SELECTION-SCREEN OUTPUT.
  lcl_controller=>at_selection_screen_output( ).

AT SELECTION-SCREEN.
  DATA(error) = lcl_controller=>at_selection_screen( ).
  IF error = abap_true.
    STOP.
  ENDIF.

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

  METHOD at_selection_screen_output.
    LOOP AT SCREEN.
      CASE screen-name.
        WHEN 'P_BUKRS' OR 'P_GJAHR' OR 'P_KEYDT' OR 'P_GJAHL' OR 'P_MONAL' OR 'P_GJAHH' OR 'P_MONAH'.
          screen-required = '2'.
      ENDCASE.
      CASE screen-group1.
        WHEN 'STD'.
          IF p_stcdt = abap_false.
            screen-required = '0'.
            screen-active = 0.
          ENDIF.
        WHEN 'STO'.
          IF p_stcoz = abap_false.
            screen-required = '0'.
            screen-active = 0.
          ENDIF.
        WHEN 'STC'.
          IF p_stcoz = abap_false AND
             p_stcdt = abap_false.
            screen-required = '0'.
            screen-active = 0.
          ENDIF.
        WHEN 'MST'.
          IF p_mstoz = abap_false.
            screen-required = '0'.
            screen-active = 0.
          ENDIF.
        WHEN 'DET'.
          IF p_stcdt = abap_false.
            screen-required = '0'.
            screen-active = 0.
          ENDIF.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.
    LOOP AT SCREEN.
      IF screen-required = '2'.
        ASSIGN (screen-name) TO FIELD-SYMBOL(<field>).
        IF <field> IS INITIAL.
          SET CURSOR FIELD screen-name.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD at_selection_screen.
*    CASE sscrfields-ucomm.
*      WHEN'FC01'.
*        CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
*          EXPORTING
*            action    = 'U'
*            view_name = 'ZSD_118_T001'
*          EXCEPTIONS
*            OTHERS    = 15.
*    ENDCASE.
    IF sy-ucomm = 'ONLI'.
      LOOP AT SCREEN.
        IF screen-required = '2'.
          ASSIGN (screen-name) TO FIELD-SYMBOL(<field>).
          IF <field> IS INITIAL.
            error = abap_true.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
    IF error = abap_true.
      MESSAGE s055(00) DISPLAY LIKE 'E'.
* Fill out all required entry fields
    ENDIF.
  ENDMETHOD.

  METHOD start_of_selection.

    CASE abap_true.
      WHEN p_stcdt. get_stcdt( ).
      WHEN p_stcoz. get_stcoz( ).
      WHEN p_mstoz. get_mstoz( ).
    ENDCASE.

  ENDMETHOD.

  METHOD end_of_selection.
*    CHECK t_list IS NOT INITIAL.
    ASSIGN r_list->* TO FIELD-SYMBOL(<t_list>).
    CHECK <t_list> IS ASSIGNED AND <t_list> IS NOT INITIAL.
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
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD display_grid.

    ASSIGN r_list->* TO FIELD-SYMBOL(<t_list>).

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

      lt_fcat = build_fcat( <t_list> ).
      lt_fcat = change_fcat( lt_fcat ).

*    SET HANDLER lcl_controller=>on_toolbar            FOR o_grid.
*    SET HANDLER lcl_controller=>on_after_user_command FOR o_grid.
*    SET HANDLER lcl_controller=>on_double_click       FOR o_grid.

      DATA ls_layo TYPE lvc_s_layo.
      ls_layo-cwidth_opt = abap_true.
*      ls_layo-sel_mode   = 'A'.
*    ls_layo-info_fname = 'ROWCOLOR'.
*      ls_layo-excp_fname = 'STATU'.
*      ls_layo-excp_rolln = 'VVIS_LIGHTS'.
*      ls_layo-excp_led   = abap_true.

      o_grid->set_table_for_first_display(
        EXPORTING
          is_layout                     = ls_layo
        CHANGING
*          it_outtab                     = t_list
          it_outtab                     = <t_list>
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
        WHEN 'SHKZG' OR 'ZFBDT' OR 'ZBD1T' OR 'ZBD2T' OR 'ZBD3T' OR 'REBZG'.
          <ls_fcat>-tech = abap_true.
        WHEN 'VADE_TARIHI'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c01.
        WHEN 'VADE_GUN'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c02.
        WHEN 'GUN'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c03.
        WHEN 'TOPLAM_GUN'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c04.
        WHEN 'WRBTR_M'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c05.
        WHEN 'DMBTR_M'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c06.
        WHEN 'WRBTR_G'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c07.
        WHEN 'DMBTR_G'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c08.
        WHEN 'WRBTR_A'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c09.
        WHEN 'DMBTR_A'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c10.
        WHEN 'WRBTR_BF'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c11.
        WHEN 'DMBTR_BF'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c12.
        WHEN 'WRBTR_AF'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c13.
        WHEN 'DMBTR_AF'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c14.
        WHEN OTHERS.
          IF strlen( <ls_fcat>-fieldname ) = 7 AND <ls_fcat>-fieldname+1(4) = p_gjahr.
            CASE <ls_fcat>-fieldname(1).
              WHEN 'W'.
                <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext
                                    = <ls_fcat>-fieldname+1(4) && '/' && <ls_fcat>-fieldname+5(2).
              WHEN 'D'.
                <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext
                                    = <ls_fcat>-fieldname+1(4) && '/' && <ls_fcat>-fieldname+5(2) && '-' && TEXT-t01.
            ENDCASE.
          ENDIF.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD on_toolbar.

  ENDMETHOD.

  METHOD on_after_user_command.

  ENDMETHOD.

  METHOD on_double_click.

  ENDMETHOD.

  METHOD get_stcdt.

    DATA lv_faedt TYPE rfpos-faedt.

    SELECT lfa1~lifnr, lfa1~name1, bsik~belnr, bsik~bldat, bsik~budat, bsik~waers, bsik~wrbtr, bsik~dmbtr, bsik~umskz, bsik~sgtxt, bsik~xref3,
      bsik~shkzg, bsik~zfbdt, bsik~zbd1t, bsik~zbd2t, bsik~zbd3t, bsik~rebzg
      FROM bsik
      JOIN lfb1 ON lfb1~lifnr = bsik~lifnr
               AND lfb1~bukrs = bsik~bukrs
      JOIN lfa1 ON lfa1~lifnr = lfb1~lifnr
      WHERE lfb1~bukrs  = @p_bukrs
      AND   bsik~gjahr  = @p_gjahr
      AND   bsik~belnr IN @s_belnr
      AND   bsik~umskz IN @s_umskz
      AND   bsik~shkzg  = 'H'
      AND   lfa1~lifnr IN @s_lifnr
      INTO CORRESPONDING FIELDS OF TABLE @t_list.
    IF t_list IS INITIAL.
      MESSAGE s004(sv) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    LOOP AT t_list ASSIGNING FIELD-SYMBOL(<ls_list>).
      CALL FUNCTION 'NET_DUE_DATE_GET'
        EXPORTING
          i_zfbdt = <ls_list>-zfbdt
          i_zbd1t = <ls_list>-zbd1t
          i_zbd2t = <ls_list>-zbd2t
          i_zbd3t = <ls_list>-zbd3t
          i_shkzg = <ls_list>-shkzg
          i_rebzg = <ls_list>-rebzg
          i_koart = 'K'
        IMPORTING
          e_faedt = <ls_list>-vade_tarihi.
      <ls_list>-vade_gun = <ls_list>-vade_tarihi - <ls_list>-budat.
      <ls_list>-gun = p_keydt - <ls_list>-vade_tarihi.
    ENDLOOP.

    GET REFERENCE OF t_list INTO r_list.

  ENDMETHOD.

  METHOD get_stcoz.

    DATA: lt_comp TYPE cl_abap_structdescr=>component_table,
          lr_line TYPE REF TO data.
    FIELD-SYMBOLS: <lt_list> TYPE STANDARD TABLE,
                   <ls_list> TYPE any.

    APPEND INITIAL LINE TO lt_comp ASSIGNING FIELD-SYMBOL(<ls_comp>).
    <ls_comp>-name = 'LIFNR'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'LIFNR' ).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'NAME1'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'NAME1_GP' ).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'BPKIND'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'BU_BPKIND' ).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'TEXT40'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'BU_TEXT40' ).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'WAERS'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'WAERS' ).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'WRBTR'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'WRBTR' ).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'DMBTR'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'DMBTR' ).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'WRBTR_M'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'WRBTR' ).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'DMBTR_M'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'DMBTR' ).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'WRBTR_G'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'WRBTR' ).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'DMBTR_G'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'DMBTR' ).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'WRBTR_A'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'WRBTR' ).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'DMBTR_A'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'DMBTR' ).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'WRBTR_BF'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'WRBTR' ).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'DMBTR_BF'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'DMBTR' ).

    DATA(lv_date_h) = CONV d( |{ p_gjahr }1201| ).
    DATA(lv_date_l) = CONV d( |{ p_gjahr }0101| ).
    DO.
      APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
      <ls_comp>-name = 'W' && lv_date_l(6).
      <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'WRBTR' ).
      APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
      <ls_comp>-name = 'D' && lv_date_l(6).
      <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'DMBTR' ).
      CALL FUNCTION 'OIL_GET_NEXT_MONTH'
        EXPORTING
          i_date = lv_date_l
        IMPORTING
          e_date = lv_date_l.
      IF lv_date_l > lv_date_h.
        EXIT.
      ENDIF.
    ENDDO.
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'WRBTR_AF'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'WRBTR' ).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'DMBTR_AF'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'DMBTR' ).

    DATA(lr_strd) = cl_abap_structdescr=>create( lt_comp ).
    CREATE DATA lr_line TYPE HANDLE lr_strd.
    ASSIGN lr_line->* TO <ls_list>.
    DATA(lr_tabd) = cl_abap_tabledescr=>create( lr_strd ).
    CREATE DATA r_list TYPE HANDLE lr_tabd.
    ASSIGN r_list->* TO <lt_list>.

    SELECT lfa1~lifnr, lfa1~name1, but000~bpkind, tb004t~text40, bsik~belnr, bsik~bldat,
      bsik~budat, bsik~waers, bsik~wrbtr, bsik~dmbtr, bsik~umskz, bsik~sgtxt, bsik~xref3,
      bsik~shkzg, bsik~zfbdt, bsik~zbd1t, bsik~zbd2t, bsik~zbd3t, bsik~rebzg
      FROM bsik
      JOIN lfb1 ON lfb1~lifnr = bsik~lifnr
               AND lfb1~bukrs = bsik~bukrs
      JOIN lfa1 ON lfa1~lifnr = lfb1~lifnr
      LEFT OUTER JOIN but000 ON but000~partner = lfa1~lifnr
      LEFT OUTER JOIN tb004t ON tb004t~spras   = @sy-langu
                            AND tb004t~bpkind  = but000~bpkind
      WHERE lfb1~bukrs  = @p_bukrs
      AND   bsik~gjahr  = @p_gjahr
      AND   bsik~shkzg  = 'H'
      AND   lfa1~lifnr IN @s_lifnr
      INTO TABLE @DATA(lt_data).
    IF lt_data IS INITIAL.
      MESSAGE s004(sv) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    FIELD-SYMBOLS <lv_field> TYPE simple.
    DEFINE setval.
      ASSIGN COMPONENT &1 OF STRUCTURE <ls_list> TO <lv_field>.
      IF sy-subrc = 0.
        <lv_field> = &2.
      ENDIF.
    end-of-DEFINITION.

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<ls_data>).
      CLEAR <ls_list>.
      setval 'LIFNR'  <ls_data>-lifnr.
      setval 'NAME1'  <ls_data>-name1.
      setval 'BPKIND' <ls_data>-bpkind.
      setval 'TEXT40' <ls_data>-text40.
      setval 'WAERS'  <ls_data>-waers.
      DATA(lv_faedt) = CONV d( space ).
      CALL FUNCTION 'NET_DUE_DATE_GET'
        EXPORTING
          i_zfbdt = <ls_data>-zfbdt
          i_zbd1t = <ls_data>-zbd1t
          i_zbd2t = <ls_data>-zbd2t
          i_zbd3t = <ls_data>-zbd3t
          i_shkzg = <ls_data>-shkzg
          i_rebzg = <ls_data>-rebzg
          i_koart = 'K'
        IMPORTING
          e_faedt = lv_faedt.
      DATA(lv_field) = |W{ lv_faedt(6) }|.
      ASSIGN COMPONENT lv_field OF STRUCTURE <ls_list> TO <lv_field>.
      IF sy-subrc = 0.
        <lv_field> = <ls_data>-wrbtr.
        lv_field = |D{ lv_faedt(6) }|.
        setval lv_field  <ls_data>-dmbtr.
      ELSEIF lv_faedt > lv_date_h.
        setval 'WRBTR_AF' <ls_data>-wrbtr.
        setval 'DMBTR_AF' <ls_data>-dmbtr.
      ELSE.
        setval 'WRBTR_BF' <ls_data>-wrbtr.
        setval 'DMBTR_BF' <ls_data>-dmbtr.
      ENDIF.
      IF <ls_data>-xref3(2) = '73'.
        setval 'WRBTR_G' <ls_data>-wrbtr.
        setval 'DMBTR_G' <ls_data>-dmbtr.
      ELSEIF <ls_data>-xref3(2) = '75'.
        setval 'WRBTR_A' <ls_data>-wrbtr.
        setval 'DMBTR_A' <ls_data>-dmbtr.
      ELSE.
        setval 'WRBTR_M' <ls_data>-wrbtr.
        setval 'DMBTR_M' <ls_data>-dmbtr.
      ENDIF.
      setval 'WRBTR' <ls_data>-wrbtr.
      setval 'DMBTR' <ls_data>-dmbtr.
      COLLECT <ls_list> INTO <lt_list>.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_mstoz.

    DATA: lt_comp TYPE cl_abap_structdescr=>component_table,
          lr_line TYPE REF TO data.
    FIELD-SYMBOLS: <lt_list> TYPE STANDARD TABLE,
                   <ls_list> TYPE any.

    APPEND INITIAL LINE TO lt_comp ASSIGNING FIELD-SYMBOL(<ls_comp>).
    <ls_comp>-name = 'KUNNR'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'KUNNR' ).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'NAME1'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'NAME1_GP' ).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'BPKIND'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'BU_BPKIND' ).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'TEXT40'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'BU_TEXT40' ).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'WAERS'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'WAERS' ).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'WRBTR'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'WRBTR' ).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'DMBTR'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'DMBTR' ).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'WRBTR_M'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'WRBTR' ).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'DMBTR_M'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'DMBTR' ).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'WRBTR_G'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'WRBTR' ).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'DMBTR_G'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'DMBTR' ).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'WRBTR_A'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'WRBTR' ).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'DMBTR_A'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'DMBTR' ).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'WRBTR_BF'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'WRBTR' ).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'DMBTR_BF'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'DMBTR' ).

    DATA(lv_date_h) = CONV d( |{ p_gjahr }1201| ).
    DATA(lv_date_l) = CONV d( |{ p_gjahr }0101| ).
    DO.
      APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
      <ls_comp>-name = 'W' && lv_date_l(6).
      <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'WRBTR' ).
      APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
      <ls_comp>-name = 'D' && lv_date_l(6).
      <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'DMBTR' ).
      CALL FUNCTION 'OIL_GET_NEXT_MONTH'
        EXPORTING
          i_date = lv_date_l
        IMPORTING
          e_date = lv_date_l.
      IF lv_date_l > lv_date_h.
        EXIT.
      ENDIF.
    ENDDO.
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'WRBTR_AF'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'WRBTR' ).
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'DMBTR_AF'.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'DMBTR' ).

    DATA(lr_strd) = cl_abap_structdescr=>create( lt_comp ).
    CREATE DATA lr_line TYPE HANDLE lr_strd.
    ASSIGN lr_line->* TO <ls_list>.
    DATA(lr_tabd) = cl_abap_tabledescr=>create( lr_strd ).
    CREATE DATA r_list TYPE HANDLE lr_tabd.
    ASSIGN r_list->* TO <lt_list>.

    SELECT kna1~kunnr, kna1~name1, but000~bpkind, tb004t~text40, bsid~belnr, bsid~bldat,
      bsid~budat, bsid~waers, bsid~wrbtr, bsid~dmbtr, bsid~umskz, bsid~sgtxt, bsid~xref3,
      bsid~shkzg, bsid~zfbdt, bsid~zbd1t, bsid~zbd2t, bsid~zbd3t, bsid~rebzg
      FROM bsid
      JOIN knb1 ON knb1~kunnr = bsid~kunnr
               AND knb1~bukrs = bsid~bukrs
      JOIN kna1 ON kna1~kunnr = knb1~kunnr
      LEFT OUTER JOIN but000 ON but000~partner = kna1~kunnr
      LEFT OUTER JOIN tb004t ON tb004t~spras   = @sy-langu
                            AND tb004t~bpkind  = but000~bpkind
      WHERE knb1~bukrs  = @p_bukrs
      AND   bsid~gjahr  = @p_gjahr
      AND   bsid~shkzg  = 'S'
      AND   kna1~kunnr IN @s_kunnr
      INTO TABLE @DATA(lt_data).
    IF lt_data IS INITIAL.
      MESSAGE s004(sv) DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    FIELD-SYMBOLS <lv_field> TYPE simple.
    DEFINE setval.
      ASSIGN COMPONENT &1 OF STRUCTURE <ls_list> TO <lv_field>.
      IF sy-subrc = 0.
        <lv_field> = &2.
      ENDIF.
    end-of-DEFINITION.

    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<ls_data>).
      CLEAR <ls_list>.
      setval 'KUNNR'  <ls_data>-kunnr.
      setval 'NAME1'  <ls_data>-name1.
      setval 'BPKIND' <ls_data>-bpkind.
      setval 'TEXT40' <ls_data>-text40.
      setval 'WAERS'  <ls_data>-waers.
      DATA(lv_faedt) = CONV d( space ).
      CALL FUNCTION 'NET_DUE_DATE_GET'
        EXPORTING
          i_zfbdt = <ls_data>-zfbdt
          i_zbd1t = <ls_data>-zbd1t
          i_zbd2t = <ls_data>-zbd2t
          i_zbd3t = <ls_data>-zbd3t
          i_shkzg = <ls_data>-shkzg
          i_rebzg = <ls_data>-rebzg
          i_koart = 'D'
        IMPORTING
          e_faedt = lv_faedt.
      DATA(lv_field) = |W{ lv_faedt(6) }|.
      ASSIGN COMPONENT lv_field OF STRUCTURE <ls_list> TO <lv_field>.
      IF sy-subrc = 0.
        <lv_field> = <ls_data>-wrbtr.
        lv_field = |D{ lv_faedt(6) }|.
        setval lv_field  <ls_data>-dmbtr.
      ELSEIF lv_faedt > lv_date_h.
        setval 'WRBTR_AF' <ls_data>-wrbtr.
        setval 'DMBTR_AF' <ls_data>-dmbtr.
      ELSE.
        setval 'WRBTR_BF' <ls_data>-wrbtr.
        setval 'DMBTR_BF' <ls_data>-dmbtr.
      ENDIF.
      IF <ls_data>-xref3(2) = '73'.
        setval 'WRBTR_G' <ls_data>-wrbtr.
        setval 'DMBTR_G' <ls_data>-dmbtr.
      ELSEIF <ls_data>-xref3(2) = '75'.
        setval 'WRBTR_A' <ls_data>-wrbtr.
        setval 'DMBTR_A' <ls_data>-dmbtr.
      ELSE.
        setval 'WRBTR_M' <ls_data>-wrbtr.
        setval 'DMBTR_M' <ls_data>-dmbtr.
      ENDIF.
      setval 'WRBTR' <ls_data>-wrbtr.
      setval 'DMBTR' <ls_data>-dmbtr.
      COLLECT <ls_list> INTO <lt_list>.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
MODULE pbo OUTPUT.
  lcl_controller=>pbo( ).
ENDMODULE.
MODULE pai INPUT.
  lcl_controller=>pai( ).
ENDMODULE.