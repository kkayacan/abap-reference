*&---------------------------------------------------------------------*
*& Report ZGI_FI_ODEME_ONERI
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgi_fi_odeme_oneri.

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

    TYPES: BEGIN OF st_mast,
             statu           TYPE vvis_lights,
             zbukr           TYPE reguh-zbukr,
             lifnr           TYPE reguh-lifnr,
             name            TYPE c LENGTH 107,
             rzawe           TYPE reguh-rzawe,
             text2           TYPE t042zt-text2,
             ort_gec_gun     TYPE p LENGTH 8 DECIMALS 8,
             count           TYPE i,
             plan_tutar      TYPE regup-dmbtr,
             vade_gecmis     TYPE regup-dmbtr,
             vade_gelmemis   TYPE regup-dmbtr,
             toplam_odenecek TYPE regup-dmbtr,
             rowcolor        TYPE c LENGTH 4,
           END OF st_mast,

           BEGIN OF st_detl,
             statu           TYPE vvis_lights,
             zbukr           TYPE reguh-zbukr,
             lifnr           TYPE regup-lifnr,
             name            TYPE c LENGTH 107,
             bpkind          TYPE but000-bpkind,
             text40          TYPE tb004t-text40,
             vblnr           TYPE regup-vblnr,
             rzawe           TYPE reguh-rzawe,
             text2           TYPE t042zt-text2,
             zlspr           TYPE regup-zlspr,
             textl           TYPE t008t-textl,
             umskz           TYPE regup-umskz,
             ltext           TYPE t074t-ltext,
             bukrs           TYPE regup-bukrs,
             belnr           TYPE regup-belnr,
             gjahr           TYPE regup-gjahr,
             bldat           TYPE regup-bldat,
             zfbdt           TYPE regup-zfbdt,
             zterm           TYPE regup-zterm,
             vade_gun        TYPE i,
             laufd           TYPE regup-laufd,
             gerceklesen_gun TYPE i,
             toplam_gun      TYPE i,
             dmbtr           TYPE regup-dmbtr,
             adat            TYPE regup-dmbtr,
             waers           TYPE reguh-waers,
             wrbtr           TYPE regup-wrbtr,
             dmbe2           TYPE regup-dmbe2,
             dmbe3           TYPE regup-dmbe3,
             aufnr           TYPE regup-aufnr,
             matnr           TYPE string,
             maktx           TYPE string,
             gkont           TYPE string,
           END OF st_detl.

    CLASS-DATA:
      o_splt TYPE REF TO cl_gui_splitter_container,
      o_mstc TYPE REF TO cl_gui_container,
      o_dtlc TYPE REF TO cl_gui_container,
      o_mstg TYPE REF TO cl_gui_alv_grid,
      o_dtlg TYPE REF TO cl_gui_alv_grid,
      t_mast TYPE TABLE OF st_mast,
      t_detl TYPE TABLE OF st_detl.

    CONSTANTS c_selected_row TYPE c LENGTH 4 VALUE 'C600'.

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
          ip_grid        TYPE csequence
        RETURNING
          VALUE(rt_fcat) TYPE lvc_t_fcat,
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,
      on_after_user_command FOR EVENT after_user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no.

ENDCLASS.

TABLES: sscrfields.

*SELECTION-SCREEN FUNCTION KEY 1.
*SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-s01.
*  PARAMETERS: p_smmry RADIOBUTTON GROUP r01 DEFAULT 'X',
*              p_dtail RADIOBUTTON GROUP r01.
*SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-s02.
  PARAMETERS: p_laufd TYPE reguh-laufd OBLIGATORY,
              p_laufi TYPE reguh-laufi OBLIGATORY.
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
*    LOOP AT SCREEN.
*      IF screen-required = '2'.
*        ASSIGN (screen-name) TO FIELD-SYMBOL(<field>).
*        IF <field> IS INITIAL.
*          SET CURSOR FIELD screen-name.
*          EXIT.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
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

    TYPES: BEGIN OF lst_rseg,
             bukrs TYPE bseg-bukrs,
             belnr TYPE bseg-belnr,
             gjahr TYPE bseg-gjahr,
             matnr TYPE rseg-matnr,
             maktx TYPE makt-maktx,
           END OF lst_rseg,

           BEGIN OF lst_makt,
             matnr TYPE makt-matnr,
             maktx TYPE makt-maktx,
           END OF lst_makt,

           BEGIN OF lst_acdoca,
             rbukrs TYPE acdoca-rbukrs,
             gjahr  TYPE acdoca-gjahr,
             belnr  TYPE acdoca-belnr,
             gkont  TYPE acdoca-gkont,
           END OF lst_acdoca.

    DATA: lt_rseg       TYPE SORTED TABLE OF lst_rseg WITH NON-UNIQUE KEY bukrs belnr gjahr,
          lt_rseg_stnd  TYPE TABLE OF lst_rseg,
          lt_makt       TYPE SORTED TABLE OF lst_makt WITH UNIQUE KEY matnr maktx,
          ls_makt       TYPE lst_makt,
          lv_matnr      TYPE mara-matnr,
          ls_mast       TYPE st_mast,
          lt_acdoca     TYPE SORTED TABLE OF lst_acdoca WITH NON-UNIQUE KEY rbukrs gjahr belnr,
          lt_acdoca_tmp TYPE TABLE OF lst_acdoca.

    SELECT CASE regup~vblnr WHEN @space THEN '1' ELSE @space END AS statu,
           reguh~zbukr, coalesce( reguh~lifnr, reguh~kunnr ) AS lifnr,
           concat_with_space( concat_with_space( reguh~name1, reguh~name2, 1 ), reguh~name3, 1 ) AS name,
           coalesce( s~bpkind, c~bpkind ) AS bpkind, coalesce( st~text40, ct~text40 ) AS text40, regup~vblnr,
           reguh~rzawe, t042zt~text2, reguh~zlspr, regup~umskz, t074t~ltext, regup~bukrs, regup~belnr, regup~gjahr,
           regup~bldat, regup~zfbdt, regup~zterm, regup~laufd, regup~dmbtr, reguh~waers, regup~wrbtr, regup~dmbe2,
           regup~dmbe3, regup~aufnr ", regup~hkont
      FROM regup
      JOIN reguh ON reguh~laufd = regup~laufd
                AND reguh~laufi = regup~laufi
                AND reguh~xvorl = regup~xvorl
                AND reguh~zbukr = regup~zbukr
                AND reguh~lifnr = regup~lifnr
                AND reguh~kunnr = regup~kunnr
                AND reguh~empfg = regup~empfg
                AND reguh~vblnr = regup~vblnr
      LEFT OUTER JOIN but000 AS s ON s~partner = reguh~lifnr
      LEFT OUTER JOIN tb004t AS st ON st~spras  = @sy-langu
                                  AND st~bpkind = s~bpkind
      LEFT OUTER JOIN but000 AS c ON c~partner = reguh~kunnr
      LEFT OUTER JOIN tb004t AS ct ON ct~spras  = @sy-langu
                                  AND ct~bpkind = c~bpkind
      LEFT OUTER JOIN t008t ON t008t~spras = @sy-langu
                           AND t008t~zahls = reguh~zlspr
      LEFT OUTER JOIN t074t ON t074t~spras = @sy-langu
                           AND t074t~koart = regup~koart
                           AND t074t~shbkz = regup~umskz
      LEFT OUTER JOIN t042zt ON t042zt~spras = @sy-langu
                            AND t042zt~land1 = reguh~land1
                            AND t042zt~zlsch = reguh~rzawe
      WHERE reguh~laufd = @p_laufd
      AND   reguh~laufi = @p_laufi
      INTO CORRESPONDING FIELDS OF TABLE @t_detl.

    IF t_detl IS NOT INITIAL.
      SELECT bseg~bukrs, bseg~belnr, bseg~gjahr, rseg~matnr, makt~maktx
        FROM bseg
        JOIN rseg ON rseg~ebeln = bseg~ebeln
*                   AND rseg~ebelp = bseg~ebelp
*                   AND rseg~zekkn = bseg~zekkn
                 AND rseg~gjahr = bseg~gjahr
        LEFT OUTER JOIN makt ON makt~spras = @sy-langu
                            AND makt~matnr = rseg~matnr
        FOR ALL ENTRIES IN @t_detl
        WHERE bseg~bukrs = @t_detl-bukrs
          AND bseg~belnr = @t_detl-belnr
          AND bseg~gjahr = @t_detl-gjahr
        INTO TABLE @lt_rseg_stnd.
      SORT lt_rseg_stnd BY bukrs belnr gjahr.
      lt_rseg = lt_rseg_stnd.
      SELECT rbukrs, gjahr, belnr, gkont FROM acdoca
        FOR ALL ENTRIES IN @t_detl
        WHERE rldnr  = '0L'
          AND rbukrs = @t_detl-bukrs
          AND gjahr  = @t_detl-gjahr
          AND belnr  = @t_detl-belnr
          AND gkoar  = 'S'
         INTO TABLE @lt_acdoca_tmp.
      SORT lt_acdoca_tmp BY rbukrs gjahr belnr gkont.
      DELETE ADJACENT DUPLICATES FROM lt_acdoca_tmp COMPARING ALL FIELDS.
      lt_acdoca = lt_acdoca_tmp.
      FREE lt_acdoca_tmp.
    ENDIF.

    LOOP AT t_detl ASSIGNING FIELD-SYMBOL(<ls_detl>).
      CLEAR lt_makt.
      LOOP AT lt_rseg ASSIGNING FIELD-SYMBOL(<ls_rseg>) WHERE bukrs = <ls_detl>-bukrs
                                                        AND   belnr = <ls_detl>-belnr
                                                        AND   gjahr = <ls_detl>-gjahr.
        MOVE-CORRESPONDING <ls_rseg> TO ls_makt.
        COLLECT ls_makt INTO lt_makt.
      ENDLOOP.
      LOOP AT lt_makt INTO ls_makt.
        CLEAR lv_matnr.
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
          EXPORTING
            input  = ls_makt-matnr
          IMPORTING
            output = lv_matnr.
        IF <ls_detl>-matnr IS INITIAL.
          <ls_detl>-matnr = lv_matnr.
          <ls_detl>-maktx = ls_makt-maktx.
        ELSE.
          <ls_detl>-matnr = <ls_detl>-matnr && `, ` && lv_matnr.
          <ls_detl>-maktx = <ls_detl>-maktx && `, ` && ls_makt-maktx.
        ENDIF.
      ENDLOOP.
      DATA(lv_zterm) = <ls_detl>-zterm.
      REPLACE ALL OCCURRENCES OF REGEX '[^[:digit:] ]' IN lv_zterm WITH space.
      CONDENSE lv_zterm NO-GAPS.
      <ls_detl>-vade_gun        = lv_zterm.
      <ls_detl>-zfbdt           = <ls_detl>-zfbdt + <ls_detl>-vade_gun.
      <ls_detl>-gerceklesen_gun = <ls_detl>-zfbdt - <ls_detl>-laufd.
      <ls_detl>-toplam_gun      = <ls_detl>-vade_gun + <ls_detl>-gerceklesen_gun.
      <ls_detl>-adat            = <ls_detl>-dmbtr * <ls_detl>-toplam_gun.
      LOOP AT lt_acdoca ASSIGNING FIELD-SYMBOL(<ls_acdoca>) WHERE rbukrs = <ls_detl>-bukrs
                                                            AND   belnr  = <ls_detl>-belnr
                                                            AND   gjahr  = <ls_detl>-gjahr.
        IF <ls_detl>-gkont IS INITIAL.
          <ls_detl>-gkont = |{ <ls_acdoca>-gkont ALPHA = OUT }|.
        ELSE.
          <ls_detl>-gkont = <ls_detl>-gkont && `, ` && |{ <ls_acdoca>-gkont ALPHA = OUT }|.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    LOOP AT t_detl ASSIGNING <ls_detl>.
      ls_mast-zbukr = <ls_detl>-zbukr.
      ls_mast-lifnr = <ls_detl>-lifnr.
      ls_mast-name  = <ls_detl>-name.
      ls_mast-rzawe = <ls_detl>-rzawe.
      ls_mast-text2 = <ls_detl>-text2.
      ls_mast-ort_gec_gun = <ls_detl>-toplam_gun.
      ls_mast-count = 1.
      IF <ls_detl>-zlspr IS INITIAL.
        ls_mast-plan_tutar = <ls_detl>-dmbtr.
      ENDIF.
      IF <ls_detl>-gerceklesen_gun > 0.
        ls_mast-vade_gecmis = <ls_detl>-dmbtr.
      ELSE.
        ls_mast-vade_gelmemis = <ls_detl>-dmbtr.
      ENDIF.
      ls_mast-toplam_odenecek = <ls_detl>-dmbtr.
      COLLECT ls_mast INTO t_mast.
    ENDLOOP.

    LOOP AT t_mast ASSIGNING FIELD-SYMBOL(<ls_mast>).
      <ls_mast>-ort_gec_gun = <ls_mast>-ort_gec_gun / <ls_mast>-count.
    ENDLOOP.

  ENDMETHOD.

  METHOD end_of_selection.
    CHECK t_mast IS NOT INITIAL.
    CASE sy-batch.
      WHEN abap_false.
        CALL SCREEN 0001.
      WHEN abap_true.
        save( abap_true ).
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
          WHEN 'BACK'.
            SET SCREEN 0.
          WHEN 'EXIT'.
            LEAVE PROGRAM.
          WHEN 'CANC'.
            LEAVE PROGRAM.
          WHEN 'SAVE'.
            save( ).
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
**        DATA(ls_msg) = reject_order( iv_vbeln = <ls_list>-vbeln iv_posnr = <ls_list>-posnr ).
**        IF ls_msg IS NOT INITIAL.
**          <ls_list>-statu = '1'.
**          <ls_list>-message = ls_msg-message.
**          IF sy-batch = abap_true.
**            WRITE:/ icon_led_red AS ICON, <ls_list>-vbeln, <ls_list>-posnr, ls_msg-message.
**          ENDIF.
**        ELSE.
**          <ls_list>-statu = '3'.
**          IF sy-batch = abap_true.
**            WRITE:/ icon_led_green AS ICON, <ls_list>-vbeln, <ls_list>-posnr, ls_msg-message.
**          ENDIF.
**        ENDIF.
*      ENDIF.
*    ENDLOOP.

  ENDMETHOD.

  METHOD display_grid.

    DATA lt_fcat TYPE lvc_t_fcat.

    IF o_mstg IS NOT BOUND.

      o_splt = NEW cl_gui_splitter_container( parent  = NEW cl_gui_custom_container( 'CONT' )
                                              rows    = 2
                                              columns = 1 ).

      o_mstc = o_splt->get_container( row = 1 column = 1 ).
      o_dtlc = o_splt->get_container( row = 2 column = 1 ).

      CREATE OBJECT o_mstg
        EXPORTING
          i_lifetime        = cl_gui_control=>lifetime_dynpro
          i_parent          = o_mstc
          i_appl_events     = abap_true "Triggers PAI/PBO on alv events
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.

      lt_fcat = build_fcat( t_mast ).
      lt_fcat = change_fcat( it_fcat = lt_fcat ip_grid = 'MASTER' ).

*      SET HANDLER on_toolbar            FOR o_mstg.
*      SET HANDLER on_after_user_command FOR o_mstg.
      SET HANDLER on_double_click       FOR o_mstg.

      DATA ls_layo TYPE lvc_s_layo.
      ls_layo-cwidth_opt = abap_true.
*      ls_layo-sel_mode   = 'A'.
      ls_layo-info_fname = 'ROWCOLOR'.
      ls_layo-excp_fname = 'STATU'.
      ls_layo-excp_rolln = 'VVIS_LIGHTS'.
      ls_layo-excp_led   = abap_true.
      ls_layo-zebra      = abap_true.

      o_mstg->set_table_for_first_display(
        EXPORTING
          is_layout                     = ls_layo
        CHANGING
          it_outtab                     = t_mast
          it_fieldcatalog               = lt_fcat
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4 ).

      CREATE OBJECT o_dtlg
        EXPORTING
          i_lifetime        = cl_gui_control=>lifetime_dynpro
          i_parent          = o_dtlc
          i_appl_events     = abap_true "Triggers PAI/PBO on alv events
        EXCEPTIONS
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          OTHERS            = 5.

      lt_fcat = build_fcat( t_detl ).
      lt_fcat = change_fcat( it_fcat = lt_fcat ip_grid = 'DETAIL' ).

      CLEAR ls_layo.
      ls_layo-cwidth_opt = abap_true.
      ls_layo-zebra      = abap_true.
      ls_layo-no_toolbar = abap_true.

      o_dtlg->set_table_for_first_display(
        EXPORTING
          is_layout                     = ls_layo
        CHANGING
          it_outtab                     = t_detl
          it_fieldcatalog               = lt_fcat
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4 ).

      o_dtlg->set_filter_criteria(
        EXPORTING
          it_filter = VALUE #( ( fieldname = 'LIFNR' sign = 'I' option = 'EQ' low = ''  ) )
        EXCEPTIONS
          no_fieldcatalog_available = 1
          OTHERS                    = 2 ).

      o_dtlg->refresh_table_display( ).

    ELSE.

      DATA ls_stbl TYPE lvc_s_stbl.
      ls_stbl-row = abap_true.
      ls_stbl-col = abap_true.

      o_mstg->refresh_table_display(
        EXPORTING
          is_stable      = ls_stbl
        EXCEPTIONS
          finished       = 1
          OTHERS         = 2 ).

      o_dtlg->refresh_table_display(
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
        WHEN 'COUNT'.
          <ls_fcat>-tech = abap_true.
        WHEN 'NAME'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c01.
        WHEN 'ORT_GEC_GUN'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c02.
        WHEN 'PLAN_TUTAR'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c03.
        WHEN 'VADE_GECMIS'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c04.
        WHEN 'VADE_GELMEMIS'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c05.
        WHEN 'TOPLAM_ODENECEK'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c06.
        WHEN 'ZFBDT'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c07.
        WHEN 'VADE_GUN'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c08.
        WHEN 'GERCEKLESEN_GUN'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c09.
        WHEN 'TOPLAM_GUN'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c10.
        WHEN 'DMBTR'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c11.
        WHEN 'ADAT'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c12.
        WHEN 'DMBE2'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c13.
        WHEN 'DMBE3'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c14.
        WHEN 'MATNR'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c15.
        WHEN 'MAKTX'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c16.
        WHEN 'GKONT'.
          <ls_fcat>-scrtext_s = <ls_fcat>-scrtext_m = <ls_fcat>-scrtext_l = <ls_fcat>-coltext = <ls_fcat>-seltext = TEXT-c17.
        WHEN 'RZAWE' OR 'TEXT2'.
          IF ip_grid = 'DETAIL'.
            <ls_fcat>-no_out = abap_true.
          ENDIF.
        WHEN 'VBLNR' OR 'BUKRS' OR 'GJAHR' OR 'ZTERM' OR 'LAUFD'.
          <ls_fcat>-no_out = abap_true.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD on_toolbar.

  ENDMETHOD.

  METHOD on_after_user_command.

  ENDMETHOD.

  METHOD on_double_click.

    DATA ls_mast LIKE LINE OF t_mast.
    MODIFY t_mast FROM ls_mast TRANSPORTING rowcolor WHERE rowcolor IS NOT INITIAL.
    ls_mast-rowcolor = c_selected_row.
    MODIFY t_mast FROM ls_mast INDEX es_row_no-row_id TRANSPORTING rowcolor.

    CLEAR ls_mast.
    READ TABLE t_mast INTO ls_mast INDEX es_row_no-row_id.

    o_dtlg->set_filter_criteria(
      EXPORTING
        it_filter = VALUE #( ( fieldname = 'LIFNR' sign = 'I' option = 'EQ' low = ls_mast-lifnr )
                             ( fieldname = 'RZAWE' sign = 'I' option = 'EQ' low = ls_mast-rzawe ) )
      EXCEPTIONS
        no_fieldcatalog_available = 1
        OTHERS                    = 2 ).

  ENDMETHOD.

ENDCLASS.
MODULE pbo OUTPUT.
  lcl_controller=>pbo( ).
ENDMODULE.
MODULE pai INPUT.
  lcl_controller=>pai( ).
ENDMODULE.