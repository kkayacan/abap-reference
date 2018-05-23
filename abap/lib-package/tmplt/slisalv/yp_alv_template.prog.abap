REPORT YP_ALV_TEMPLATE.

INCLUDE yi_globalalv.

INCLUDE <icon>.

TABLES :  mara.

TYPE-POOLS: sscr.

DATA : BEGIN OF it_report OCCURS 10,

         selkz(1),
         stats_h LIKE icon-id,
         matnr   LIKE mara-matnr.
DATA : END OF it_report.

DATA : p_error TYPE sy-subrc .

""""sabit
DATA : ln_t001 LIKE t001.
DATA : v_datum LIKE sy-datum,
       v_uzeit LIKE sy-uzeit.
DATA : BEGIN OF it_detail OCCURS 10,
         selkz(1).

DATA : END OF it_detail.


DATA: v_recname TYPE slis_tabname VALUE 'IT_DETAIL'.
DATA: v_tabname TYPE slis_tabname VALUE 'IT_DETAIL[]'.
DATA : c_red    LIKE icon-id.
DATA : c_green  LIKE icon-id.
DATA : c_yellow LIKE icon-id.
DATA: gs_variant_d        LIKE disvariant,
      g_save_d(1)         TYPE c  VALUE 'A',
      lt_t_fieldcatalog_d TYPE slis_t_fieldcat_alv WITH HEADER LINE.
DATA: gs_layout_d TYPE slis_layout_alv.

FIELD-SYMBOLS: <lfs_excel>  TYPE          alsmex_tabline.
* Temporary excel file table
DATA: gt_excel_file       TYPE TABLE OF alsmex_tabline .

DATA: gv_max_rows            TYPE          i VALUE 5000.
DATA: gv_begin_row           TYPE          i VALUE 2.
DATA: gv_end_col             TYPE          i VALUE 7.

DATA: col      TYPE i.
FIELD-SYMBOLS <fs> TYPE any.
"""""""""""""""""""""

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-a00.
SELECT-OPTIONS matnr   FOR mara-matnr.
PARAMETERS : p_detay AS CHECKBOX .

SELECTION-SCREEN END OF BLOCK b2.

PARAMETERS     : p_vari LIKE disvariant-variant MODIF ID blo.


PARAMETERS show_po NO-DISPLAY.

INITIALIZATION.
  v_default_report_name = sy-repid.
  v_default_recname     = 'IT_REPORT'.
  gs_variant-report = v_default_report_name.
  show_po = 'X'.


AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF  screen-group1 EQ 'BPO' AND show_po IS INITIAL.
      screen-input     = 0.
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  IF show_po IS INITIAL.
    sscrfields-functxt_01 = text-001.
  ELSE.
    sscrfields-functxt_01 = text-002.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM lt_f4_for_variants USING p_vari.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_file.
*  PERFORM f4_for_files CHANGING pa_file.



AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
      IF show_po IS INITIAL.
        show_po = 'X'.
      ELSE.
        CLEAR show_po.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

START-OF-SELECTION.

  PERFORM get_report_data.
  DESCRIBE TABLE it_report LINES v_line.
  CHECK v_line = 0.
  MESSAGE i899(s1) WITH 'Veri bulunamadı'.


END-OF-SELECTION.

*  CHECK V_LINE > 0.
  IF sy-batch EQ abap_false .
    PERFORM set_report_fcat.
    PERFORM show_report_fcat TABLES it_report
                        USING  p_vari
                               gs_variant
                               v_default_report_name
                               v_default_recname.
  ELSE.
    PERFORM send_object USING abap_false.
  ENDIF.



*&---------------------------------------------------------------------*
*&      Form  get_report_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_report_data.

  REFRESH it_report.

  PERFORM set_led_color.
***  PERFORM read_excel_file.


  SELECT matnr FROM mara INTO CORRESPONDING FIELDS OF TABLE @it_report.



ENDFORM.                    " get_report_data
*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM set_top_of_page.
  PERFORM comment_build USING gt_list_top_of_page[].
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      i_logo             = 'BEYMEN_LOGO'
*     I_LOGO             = 'TRVPICTURE_REC_WIZ03'
      it_list_commentary = gt_list_top_of_page.
ENDFORM.                    "set_top_of_page

*&--------------------------------------------------------------------*
*&      Form  set_end_of_page
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM set_end_of_page.


ENDFORM.                    "set_end_of_page

*&--------------------------------------------------------------------*
*&      Form  set_end_of_list
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM set_end_of_list.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
*     I_LOGO             = 'TRVPICTURE_REC_WIZ03'
      it_list_commentary = gt_list_top_of_page
      i_end_of_list_grid = 'X'.


ENDFORM.                    "set_end_of_list
*---------------------------------------------------------------------*
*       FORM COMMENT_BUILD                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  LT_TOP_OF_PAGE                                                *
*---------------------------------------------------------------------*
FORM comment_build USING lt_top_of_page TYPE
                                        slis_t_listheader.
  DATA: ls_line TYPE slis_listheader.
  DATA: tarh(10) TYPE c,
        saat(8)  TYPE c,
        trh1(10),
        trh2(10).
  v_datum = sy-datum.
  v_uzeit = sy-uzeit.
  REFRESH : lt_top_of_page.
  CLEAR   : lt_top_of_page.
  WRITE :  v_datum TO tarh DD/MM/YYYY.
  WRITE :  v_uzeit TO saat USING EDIT MASK '__:__:__'.
* LIST HEADING LINE: TYPE H
  CLEAR ls_line.
  ls_line-typ  = 'H'.
* LS_LINE-KEY:  NOT USED FOR THIS TYPE
  ls_line-info =
       sy-title.
  APPEND ls_line TO lt_top_of_page.
* STATUS LINE: TYPE S
  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key = 'Tarih - Saat :'.
  CONCATENATE tarh '-' saat INTO ls_line-info.
  APPEND ls_line TO lt_top_of_page.

  CLEAR ls_line.
  ls_line-typ  = 'S'.
  ls_line-key = 'Satýr Sayýsý'.
  ls_line-info = v_line.
  APPEND ls_line TO lt_top_of_page.




  CLEAR ls_line.
  APPEND ls_line TO lt_top_of_page.
* ACTION LINE: TYPE A
  CLEAR ls_line.
  ls_line-typ  = 'A'.
* LS_LINE-KEY:  NOT USED FOR THIS TYPE
*  ls_line-info = text-105.
  APPEND ls_line TO lt_top_of_page.
ENDFORM.                    "comment_build
*---------------------------------------------------------------------*
*  FORM f01_user_command
*---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_user_command USING r_ucomm     LIKE sy-ucomm
                            rs_selfield TYPE slis_selfield. "#EC CALLED

  DATA : lv_mblnr TYPE mkpf-mblnr.
  DATA : lv_mjahr TYPE mkpf-mjahr.

*  field-symbols: <ls_outtab> type g_ty_s_outtab.
  gs_selfield = rs_selfield.
  CLEAR it_report.
  READ TABLE it_report INDEX gs_selfield-tabindex.
  CASE r_ucomm.

    WHEN '&SAVE' .

      PERFORM send_object USING abap_true .
      PERFORM get_report_data.

    WHEN '&REFR' .

      PERFORM get_report_data.

    WHEN '&KALEM' .

*      PERFORM display_item_view .

    WHEN '&DEL' .

*          MESSAGE I000(ZSD) WITH 'Önce Teslimat tayinini kaldýrýnýz! '.


    WHEN '&IC1' OR '&ETA'. "Double Click


***      CHECK IT_REPORT-REF_DOCUMENT IS NOT INITIAL.
***
******      CHECK NOT GS_SELFIELD-FIELDNAME IS INITIAL.
******      CASE GS_SELFIELD-FIELDNAME.
******        WHEN 'REF_DOCUMENT'.
***      IF IT_REPORT-HAREKETKODU = '008' OR
***         IT_REPORT-HAREKETKODU = '152' OR
***         IT_REPORT-HAREKETKODU = '651' .
***        SET PARAMETER ID 'VL' FIELD IT_REPORT-REF_DOCUMENT.
***        CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN .
***
***
***      ELSE.
***
***        LV_MBLNR = IT_REPORT-REF_DOCUMENT.
***        LV_MJAHR = IT_REPORT-ISLEMETARIH(4).
***
***        CALL FUNCTION 'MIGO_DIALOG'
***         EXPORTING
***           I_MBLNR                   = LV_MBLNR
***           I_MJAHR                   = LV_MJAHR
***         EXCEPTIONS
***           ILLEGAL_COMBINATION       = 1
***           OTHERS                    = 2
***                  .
***
***
***      ENDIF.
***        WHEN OTHERS.
***      ENDCASE.
*    WHEN '&CREATEOUT'.
*    WHEN '&CREATE_FI'.
*    WHEN '&SHOW_LOG'.
  ENDCASE.
  rs_selfield = gs_selfield.
  rs_selfield-refresh = 'X'.
ENDFORM.                    "f01_user_command
*---------------------------------------------------------------------*
*  FORM f01_set_status
*---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_pf_status_set USING rt_extab TYPE slis_t_extab .   "#EC CALLED
  PERFORM set_excluding_tab TABLES rt_extab.
  SET PF-STATUS 'STANDART' EXCLUDING rt_extab[].
ENDFORM.                    "f01_set_status
*&---------------------------------------------------------------------*
*&      Form  excluding_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excluding_events.
  PERFORM exclude_events TABLES ex_events USING 'CALLER_EXIT'.
* perform exclude_events tables ex_events using 'USER_COMMAND'.
  PERFORM exclude_events TABLES ex_events USING 'TOP_OF_PAGE'.
  PERFORM exclude_events TABLES ex_events USING 'TOP_OF_COVERPAGE'.
  PERFORM exclude_events TABLES ex_events USING 'END_OF_COVERPAGE'.
  PERFORM exclude_events TABLES ex_events USING 'FOREIGN_TOP_OF_PAGE'.
  PERFORM exclude_events TABLES ex_events USING 'FOREIGN_END_OF_PAGE'.
*  PERFORM exclude_events TABLES ex_events USING 'PF_STATUS_SET'.
  PERFORM exclude_events TABLES ex_events USING 'LIST_MODIFY'.
  PERFORM exclude_events TABLES ex_events USING 'TOP_OF_LIST'.
  PERFORM exclude_events TABLES ex_events USING 'END_OF_PAGE'.
  PERFORM exclude_events TABLES ex_events USING 'END_OF_LIST'.
  PERFORM exclude_events TABLES ex_events USING 'AFTER_LINE_OUTPUT'.
  PERFORM exclude_events TABLES ex_events USING 'BEFORE_LINE_OUTPUT'.
  PERFORM exclude_events TABLES ex_events USING 'REPREP_SEL_MODIFY'.
  PERFORM exclude_events TABLES ex_events USING 'SUBTOTAL_TEXT'.
  PERFORM exclude_events TABLES ex_events USING 'GROUPLEVEL_CHANGE'.
*  PERFORM APPEND_EVENTS  TABLES AP_EVENTS USING 'DATA_CHANGED'.
  PERFORM append_events  TABLES ap_events USING 'ITEM_DATA_EXPAND'.
  PERFORM append_events  TABLES ap_events USING 'GROUPLEVEL_CHANGE'.
ENDFORM.                    " excluding_events
*&---------------------------------------------------------------------*
*&      Form  set_excluding_tab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RT_EXTAB  text
*----------------------------------------------------------------------*
FORM set_excluding_tab TABLES extab.
  REFRESH extab.
*  extab = '&UMC'.      append extab.
*  extab = '&SUM'.      append extab.
*  extab = '&OL0'.      append extab.
*  extab = '&OAD'.      append extab.
*  extab = '&AVE'.      append extab.
*  extab = '&ILT'.      append extab.
*  extab = '&ETA'.      append extab.
*  extab = '%PC' .      append extab.
*  extab = '&ALL'.      append extab.
*  extab = '&SAL'.      append extab.
*  extab = '&EB9'.      APPEND extab.
*  extab = '&REFRESH'.  APPEND extab.
*  extab = '&OUP'.      append extab.
*  extab = '&ODN'.      append extab.
*  extab = '&RNT_PREV'. append extab.
*  extab = '&VEXCEL'.   append extab.
  extab = '&AQW'.      APPEND extab.
*   extab = '&XXL'.      APPEND extab.
*   extab = '%PC'.      APPEND extab.
  extab = '&ABC'.      APPEND extab.
*  extab = '%SL' .      append extab.
*  extab = '&AOW'.      append extab.
*  extab = '&GRAPH'.    APPEND extab.
*  extab = '&INFO'.     APPEND extab.
  extab = '&LFO'.     APPEND extab.
  extab = '&NFO'.     APPEND extab.
  extab = '&CRB'.     APPEND extab.
  extab = '&CRL'.     APPEND extab.
  extab = '&CRR'.     APPEND extab.
  extab = '&CRE'.     APPEND extab.
  extab = '&XPA'.     APPEND extab.
  extab = '&OMP'.     APPEND extab.

  extab = '&SAVE_TAK'.     APPEND extab.
ENDFORM.                    " set_excluding_tab
*&---------------------------------------------------------------------*
*&      Form  set_report_fcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_report_fcat.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_internal_tabname     = v_default_recname
      i_inclname             = v_default_report_name
      i_client_never_display = 'X'
      i_bypassing_buffer     = 'X'
    CHANGING
      ct_fieldcat            = lt_t_fieldcatalog[]
    EXCEPTIONS
      OTHERS                 = 3.

*  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
*    EXPORTING
*      i_program_name         = sy-repid
*      i_internal_tabname     = v_recname
*      i_inclname             = v_default_report_name
*      i_client_never_display = 'X'
*      i_bypassing_buffer     = 'X'
*    CHANGING
*      ct_fieldcat            = lt_t_fieldcatalog_d[]
*    EXCEPTIONS
*      OTHERS                 = 3.


**-------------------------------------------------------*
  PERFORM set_field_cat_user_exit.



ENDFORM.                    " set_report_fcat
*&---------------------------------------------------------------------*
*&      Form  set_fcat_user_exit
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ITNAME  text
*      -->P_SGRP  text
*----------------------------------------------------------------------*
FORM set_fcat_user_exit USING    p_itname
                                 p_sgrp.
*  DATA : it_fcat_wa TYPE slis_fieldcat_alv.
*  LOOP AT lt_t_fieldcatalog INTO it_fcat_wa.
*    IF   it_fcat_wa-fieldname EQ 'SELKZ'.
*      it_fcat_wa-checkbox = 'X'.
*      MODIFY lt_t_fieldcatalog FROM it_fcat_wa.
*    ENDIF.
*
*  ENDLOOP.
ENDFORM.                    " set_fcat_user_exit

*&---------------------------------------------------------------------*
*&      Form  set_field_cat_user_exit
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_field_cat_user_exit .
  DATA: recname TYPE slis_tabname.
  DATA : v_title(42) TYPE c.
  MOVE: 'SELTEXT_L/SELTEXT_M/SELTEXT_S/REPTEXT_DDIC' TO v_title.
  recname = v_default_recname .

  PERFORM
      set_line_field_cat TABLES lt_t_fieldcatalog
        USING :
* recname 'STATS'        'ICON' 'X'.
 recname 'ZZMESSAGE_I'          v_title 'Mesaj Kalem',
 recname 'STATS_H'              v_title 'Başlık',
 recname 'STATS_D'              v_title 'Kalem',
 recname 'ZZMESSAGE'            v_title 'Mesaj Başlık',
 recname 'ZZMESSAGE'            'OUTPUTLEN' '20',
 recname 'ZZMESSAGE_I'          'OUTPUTLEN' '20'.
* RECNAME 'BATCHNO'   'EDIT' 'X'.








*  recname = v_recname .
*  PERFORM
*      set_line_field_cat TABLES lt_t_fieldcatalog_d
*        USING :
*                recname 'TEKLIFNO'  v_title      'Eski Teklif No',



  DELETE lt_t_fieldcatalog WHERE fieldname = 'SELKZ'.





*  PERFORM
*      SET_LINE_FIELD_CAT TABLES LT_T_FIELDCATALOG
*        USING : RECNAME 'DMBTR' 'CFIELDNAME' 'BWAERS',
*                RECNAME 'DMBTR' 'CTABNAME'   RECNAME,
*                RECNAME 'BELNR' 'HOTSPOT'    'X',
*                RECNAME 'MONAT' 'KEY'        'X',
*                RECNAME 'ZBASE' V_TITLE      'Base Tarih'.
*                recname 'KWMENG' 'QFIELDNAME' 'VRKME',
*                recname 'KWMENG' 'QTABNAME'    recname.
*               recname 'BUKRS' 'NO_OUT'     'X'           ,
ENDFORM.                    " set_field_cat_user_exit
*&---------------------------------------------------------------------*
*&      Form  set_layout_user_exit
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PVARI  text
*      -->P_DRNAME  text
*----------------------------------------------------------------------*
FORM set_layout_user_exit USING    p_pvari
                                   p_drname.

  gs_grid_set-edt_cll_cb = 'X'.

*  GS_LAYOUT-GET_SELINFOS       = 'X'.
*  GS_LAYOUT-COLWIDTH_OPTIMIZE  = 'X'.

*  gs_layout-coltab_fieldname   = 'COLOR'.
*  GS_LAYOUT-INFO_FIELDNAME     = 'COLOR'.
*  gs_layout-coltab_fieldname   = 'COLOR'.
*  gs_layout-expand_fieldname  = 'BUKRS'.
  gs_layout-box_fieldname = 'SELKZ'.
*  GS_LAYOUT-BOX_TABNAME   = V_DEFAULT_RECNAME.

*  alv_layout-colwidth_optimize  = 'X'    .
*  alv_layout-numc_sum           = 'X'    .
*  alv_layout-no_subchoice       = 'X'    .
*  alv_layout-zebra               = 'X'    .
*
*
*  alv_layout-info_fieldname = 'COLOR'.
*
ENDFORM.                    " set_layout_user_exit


*&--------------------------------------------------------------------*
*&      Form  F4_FOR_FILES
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM f4_for_files CHANGING p_file.
  DATA: ld_default_extension TYPE string VALUE 'CSV'.
  DATA: lt_file_tab          TYPE filetable.
  DATA: ls_file_line         TYPE file_table.
  DATA: ld_rc                TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      default_extension       = ld_default_extension
      multiselection          = abap_false
    CHANGING
      file_table              = lt_file_tab
      rc                      = ld_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    CHECK ld_rc = 1.
    READ TABLE lt_file_tab INTO ls_file_line INDEX 1.
    CHECK sy-subrc = 0.
    MOVE ls_file_line-filename TO p_file.
  ENDIF.

ENDFORM.                    " F4_FOR_FILES




*&---------------------------------------------------------------------*
*&      Form  DISPLAY_XLS_VIEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_item_view .

  gs_variant_d-report    = v_default_report_name.
  gs_variant_d-username  = sy-uname.
  gs_variant_d-variant   = 'x'.
  gs_variant_d-text      = ''.

  g_save_d = 'A'.

  FIELD-SYMBOLS : <gt_alvtab> TYPE STANDARD TABLE.

  ASSIGN (v_tabname) TO <gt_alvtab>.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_bypassing_buffer      = 'X'
      i_callback_program      = v_default_report_name
*     i_callback_pf_status_set = 'SET_PF_STATUS_SET'
*     I_CALLBACK_USER_COMMAND = 'SET_USER_COMMAND_DETAIL'
      is_layout               = gs_layout_d
*     i_save                  = g_save_d
*     is_variant              = gs_variant_D
      it_fieldcat             = lt_t_fieldcatalog_d[]
    IMPORTING
      e_exit_caused_by_caller = g_exit_caused_by_caller
      es_exit_caused_by_user  = gs_exit_caused_by_user
    TABLES
      t_outtab                = <gt_alvtab>
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.

ENDFORM.                    " DISPLAY_XLS_VIEW



*&---------------------------------------------------------------------*
*&      Form  SET_INIT_PARAMETERS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_led_color.


  SELECT SINGLE id FROM icon
    INTO c_red
    WHERE name = 'ICON_LED_RED'.


  SELECT SINGLE id FROM icon
    INTO c_yellow
    WHERE name = 'ICON_LED_YELLOW'.


  SELECT SINGLE id FROM icon
    INTO c_green
    WHERE name = 'ICON_LED_GREEN'.

ENDFORM.                    "

*&---------------------------------------------------------------------*
*&      Form  GET_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_MTEXT  text
*----------------------------------------------------------------------*
FORM get_message  CHANGING p_mtext p_result.


  DELETE messtab WHERE msgtyp = 'W'.

  CLEAR p_mtext.
  CLEAR p_result.

*BELGE KAYDEDILDI MESAJI
  LOOP AT messtab WHERE msgid = 'F5' AND
                        msgnr = '312'.
    p_result = 'OK'.
    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = messtab-msgid
        msgnr               = messtab-msgnr
        msgv1               = messtab-msgv1
        msgv2               = messtab-msgv2
        msgv3               = messtab-msgv3
        msgv4               = messtab-msgv4
      IMPORTING
        message_text_output = p_mtext.
    EXIT.
  ENDLOOP.

*HATA MESAJI
  IF sy-subrc NE 0 .

    LOOP AT messtab WHERE msgtyp = 'E' .
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = messtab-msgid
          msgnr               = messtab-msgnr
          msgv1               = messtab-msgv1
          msgv2               = messtab-msgv2
          msgv3               = messtab-msgv3
          msgv4               = messtab-msgv4
        IMPORTING
          message_text_output = p_mtext.

      EXIT.
    ENDLOOP.

    IF sy-subrc NE 0 .

      LOOP AT messtab .
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            msgid               = messtab-msgid
            msgnr               = messtab-msgnr
            msgv1               = messtab-msgv1
            msgv2               = messtab-msgv2
            msgv3               = messtab-msgv3
            msgv4               = messtab-msgv4
          IMPORTING
            message_text_output = p_mtext.

      ENDLOOP.

    ENDIF.

  ENDIF.
ENDFORM.                    " GET_MESSAGE
*&---------------------------------------------------------------------*
*&      Form  CHANGE_SALES_ORDER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_object USING p_selkz.



  DATA : mtext       LIKE t100-text,
         result      TYPE char2,
         c_datab(10),
         c_datbi(10).



  LOOP AT it_report WHERE selkz = p_selkz .
    EXIT .
  ENDLOOP.

  IF sy-subrc NE 0 .
    MESSAGE i899(s1) WITH 'Satır seçiniz'.
    EXIT.
  ENDIF.


***      WHEN 'ZALP_DELIVERY_CLOSE'.
****        CALL FUNCTION 'ZALP_DELIVERY_CLOSE'
****          EXPORTING
****            i_vbeln             =
****            i_delete            =
****            i_xblnr             =
*****           I_WADAT_IST         = SY-DATUM
*****           I_GOODS_ISSUE       = 'X'
****          TABLES
****            return              =
****            it_items            =

  IF sy-subrc EQ 0 .
    MESSAGE s899(s1) WITH 'Sonucları Kontrol ediniz.'.
  ENDIF.


ENDFORM.                    " CHANGE_SALES_ORDER
*&---------------------------------------------------------------------*
*&      Form  SET_LIGHT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_light .
*  CASE it_report-type.
*    WHEN 'C'."iptal
      it_report-stats_h = c_red.
*    WHEN 'E' OR 'A' ."hata
*      it_report-stats_h = c_red.
*    WHEN 'S'.
*      it_report-stats_h = c_green.
*    WHEN OTHERS.
*      it_report-stats_h = c_yellow.
*  ENDCASE.

*
*  CASE it_report-i_type.
*    WHEN 'C'."iptal
*      it_report-stats_d = c_red.
*    WHEN 'E' OR 'A' ."hata
*      it_report-stats_d = c_red.
*    WHEN 'S'.
*      it_report-stats_d = c_green.
*    WHEN OTHERS.
*      it_report-stats_d = c_yellow.
*  ENDCASE.

ENDFORM.



*&---------------------------------------------------------------------*
*&      Form  LOCK_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM lock_report CHANGING p_error.

  DATA: lv_lockuser TYPE sy-uname.
  CALL FUNCTION 'ENQUEUE_ESWREP'
    EXPORTING
      mode_swclirep  = 'X'
*     CLIENT         = im_client  "note 1391948
      name           = sy-repid
*     X_NAME         = ' '
      _scope         = '1'
*     _WAIT          = ' '
*     _COLLECT       = ' '
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc = 1.
    lv_lockuser = sy-msgv1.
    sy-msgv1 = sy-repid.
    sy-msgv2 = lv_lockuser.
    sy-msgid = 'SWF_UTL_001'.
    sy-msgty = 'S'.
    sy-msgno = 200.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
* --- note 1889642 ---
    p_error = 4.
  ELSEIF sy-subrc <> 0.
    sy-msgid = 'WL'.
    sy-msgty = 'S'.
    sy-msgno = 583.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    p_error = 4.
  ENDIF.

ENDFORM.
