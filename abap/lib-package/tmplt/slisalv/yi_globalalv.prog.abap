*       FORM BDC_DYNPRO                                               *
*       FORM BDC_FIELD                                                *
*       FORM CALL_TRANSACTION                                         *
*       FORM CALL_TRANSACTION_OPT                                     *
*       FORM OPEN_BDC_GROUP                                           *
*       FORM BDC_INSERT                                               *
*       FORM BDC_INSERT_OPT                                           *
*       FORM CLOSE_BDC_GROUP                                          *
*       FORM WRT_BATCH                                                *
*       FORM WRT_LOGS                                                 *
*       FORM SET_LAYOUT                                               *
*       FORM SET_FCAT                                                 *
*       FORM GET_FCAT                                                 *
*       FORM VARIANT_LOAD                                             *
*       FORM SHOW_REPORT                                              *
*       FORM SHOW_HIER_REPORT                                         *
*       FORM BEGIN_FB01                                               *
*       FORM SETBCD                                                   *
*       FORM FTPOST_FIELD                                             *
*       FORM POP_MESS                                                 *

TABLES : t100,
         t001,
         tbsl,
         t019,
         sscrfields,
         ltdx,
         ltdxt.

CONSTANTS : color_light_blue  LIKE lipov-color VALUE 'C100',
            color_grey        LIKE lipov-color VALUE 'C210',
            color_white       LIKE lipov-color VALUE 'C200',
            color_light_green LIKE lipov-color VALUE 'C500',
            color_light_red   LIKE lipov-color VALUE 'C600'.

TYPE-POOLS : icon.
TYPE-POOLS: slis.
TYPE-POOLS : kkblo.


DATA : BEGIN OF ex_events OCCURS 17,
         event(20),
       END OF ex_events.

DATA : BEGIN OF ap_events OCCURS 17,
         event(20),
       END OF ap_events.

DATA : BEGIN OF bdctab OCCURS 100.
        INCLUDE STRUCTURE bdcdata.
DATA : END OF bdctab.

DATA BEGIN OF messtab OCCURS 10.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA END OF messtab.

DATA BEGIN OF bdcopt OCCURS 1.
        INCLUDE STRUCTURE ctu_params.
DATA END OF bdcopt.

DATA: v_default_recname     TYPE slis_tabname  VALUE 'GT_OUT',
      v_default_report_name LIKE disvariant-report.

DATA : BEGIN OF it_fcatfies OCCURS 0.
        INCLUDE STRUCTURE dfies.
DATA : tb LIKE dfies-tabname,
       fn LIKE dfies-fieldname,
       END OF it_fcatfies.

DATA : BEGIN OF bibkpf OCCURS 10,
         bldat LIKE bkpf-bldat,
         budat LIKE bkpf-budat,
         xblnr LIKE bkpf-xblnr,
         bktxt LIKE bkpf-bktxt,
         blart LIKE bkpf-blart,
         bukrs LIKE bkpf-bukrs,
         waers LIKE bkpf-waers,
         kursf LIKE bkpf-kursf,
       END OF bibkpf.

DATA : BEGIN OF bibseg OCCURS 100,
         newbs    LIKE rf05a-newbs,
         newko    LIKE rf05a-newko,
         newum    LIKE rf05a-newum,
         wrbtr    LIKE bseg-wrbtr,
         mwskz    LIKE bseg-mwskz,
         kostl    LIKE bseg-kostl,
         aufnr    LIKE bseg-aufnr,
         xmwst(1),
         valut    LIKE bseg-valut,
         zfbdt    LIKE bseg-zfbdt,
         zuonr    LIKE bseg-zuonr,
         sgtxt    LIKE bseg-sgtxt,
         zterm    LIKE bseg-zterm,
         zlspr    LIKE bseg-zlspr,
         zlsch    LIKE bseg-zlsch,
         fwbas    LIKE bseg-fwbas,
         matnr    LIKE bseg-matnr,
         werks    LIKE bseg-werks,
         menge    LIKE bseg-menge,
         meins    LIKE bseg-meins,
       END OF bibseg.

DATA : BEGIN OF ftpost OCCURS 20.
        INCLUDE STRUCTURE ftpost.
DATA : END OF ftpost.

DATA : BEGIN OF xblntab OCCURS 2.
        INCLUDE STRUCTURE blntab.
DATA : END OF xblntab.

DATA : BEGIN OF xfttax OCCURS 2.
        INCLUDE STRUCTURE fttax.
DATA : END OF xfttax.

DATA : fvalue                 LIKE ftpost-fval,
       default_tabname_master TYPE kkblo_tabname,
       default_tabname_slave  TYPE kkblo_tabname,
       it_keyinfo             TYPE kkblo_keyinfo,
       it_sp_groups           TYPE kkblo_t_sp_group     WITH HEADER LINE,
       it_sort                TYPE kkblo_t_sortinfo     WITH HEADER LINE,
       it_extab               TYPE kkblo_t_extab        WITH HEADER LINE,
*       it_fcat_init    type kkblo_fieldcat,
       it_filter              TYPE kkblo_t_filter,
*       it_fcat         type kkblo_t_fieldcat,
*       it_fieldcat_def type kkblo_t_fieldcat,
       v_line                 LIKE sy-tabix.
DATA: gs_print TYPE  slis_print_alv.
DATA: gs_layout               TYPE slis_layout_alv,
      g_exit_caused_by_caller,
      gs_exit_caused_by_user  TYPE slis_exit_by_user,
      g_repid                 LIKE sy-repid,
      gs_grid_set             TYPE  lvc_s_glay.


DATA: gt_events           TYPE slis_t_event,
      gt_list_top_of_page TYPE slis_t_listheader.

DATA: lt_fieldcatalog   TYPE slis_fieldcat_alv,
      lt_t_fieldcatalog TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      lt_excluding      TYPE slis_t_extab.
DATA: gs_selfield TYPE slis_selfield.

DATA : color     TYPE kkblo_t_specialcol WITH HEADER LINE.

DATA: gs_variant LIKE disvariant,
      g_save(1)  TYPE c  VALUE 'A',
      g_exit(1)  TYPE c.
DATA : r_tool(2)   VALUE 'LT'.

DEFINE set_alvcol.
  &1-FIELDNAME = &2.
  &1-COLOR-COL = &3.
  &1-COLOR-INT = &4.
  &1-COLOR-INV = &5.
  &1-NOKEYCOL = &6.
  APPEND &1.
END-OF-DEFINITION.



*---------------------------------------------------------------------*
*       FORM WRT_BATCH                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM wrt_batch.
  WRITE : / sy-uline(110).
  WRITE : / sy-vline, 'PROGRAM ',
            sy-vline, 'EKR.',
            sy-vline, 'X',
            sy-vline, (30) 'ALAN ADI',
            sy-vline, (51) 'ALANA YAZILAN DE#ER', sy-vline.
  WRITE : / sy-uline(110).
  LOOP AT bdctab.
    WRITE : / sy-vline, bdctab-program(8),
              sy-vline, bdctab-dynpro,
              sy-vline, bdctab-dynbegin,
              sy-vline, bdctab-fnam(30),
              sy-vline, bdctab-fval(51), sy-vline.
  ENDLOOP.
  WRITE : / sy-uline(110).
ENDFORM.

*---------------------------------------------------------------------*
*       FORM WRT_LOGS                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM wrt_logs.
  DATA : xmsgv1(255),
         cnt(6).
  FORMAT RESET.
  cnt = 0.
  WRITE : / sy-uline(130).
  WRITE : / sy-vline, 'T',
            sy-vline, 'ID   ',
            sy-vline, 'NO ',
            sy-vline, (90) 'A#IKLAMA',
            sy-vline, 'PROGRAM ',
            sy-vline, 'EKR.', sy-vline.
  WRITE : / sy-uline(130).
  LOOP AT messtab.
    IF messtab-msgtyp = '#'.
      WRITE : / sy-uline(130).
      cnt = cnt + 1.
    ELSE.
      CLEAR t100 .
      SELECT SINGLE * FROM t100
            WHERE sprsl = sy-langu AND
                  arbgb = messtab-msgid AND
                  msgnr = messtab-msgnr.
      MOVE t100-text TO xmsgv1.
      REPLACE '&' WITH messtab-msgv1 INTO xmsgv1. CONDENSE xmsgv1.
      REPLACE '&' WITH messtab-msgv2 INTO xmsgv1. CONDENSE xmsgv1.
      REPLACE '&' WITH messtab-msgv3 INTO xmsgv1. CONDENSE xmsgv1.
      REPLACE '&' WITH messtab-msgv4 INTO xmsgv1. CONDENSE xmsgv1.
      REPLACE '$' WITH messtab-msgv1 INTO xmsgv1. CONDENSE xmsgv1.
      REPLACE '$' WITH messtab-msgv2 INTO xmsgv1. CONDENSE xmsgv1.
      REPLACE '$' WITH messtab-msgv3 INTO xmsgv1. CONDENSE xmsgv1.
      REPLACE '$' WITH messtab-msgv4 INTO xmsgv1. CONDENSE xmsgv1.
      CASE messtab-msgtyp.
        WHEN 'E'. FORMAT COLOR COL_NEGATIVE.
        WHEN 'W'. FORMAT COLOR COL_TOTAL.
      ENDCASE.
      WRITE : / sy-vline, messtab-msgtyp,
                sy-vline, messtab-msgid(5),
                sy-vline, messtab-msgnr,
                sy-vline, xmsgv1(90),
                sy-vline, messtab-dyname(8),
                sy-vline, messtab-dynumb, sy-vline.
      FORMAT RESET.
    ENDIF.
  ENDLOOP.
  WRITE : / sy-uline(130).
  FORMAT COLOR COL_POSITIVE.
  WRITE : / sy-vline, ' ',
            sy-vline, (5) ' ',
            sy-vline, '   ',
            sy-vline, 'TOPLAM ##LENEN KAYIT : ', cnt, (59) ' ',
            sy-vline, (8) ' ',
            sy-vline, '    ', sy-vline.
  FORMAT RESET.
  WRITE : / sy-uline(130).
ENDFORM.

*---------------------------------------------------------------------*
*       FORM BDC_DYNPRO                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PROGRAM                                                       *
*  -->  DYNPRO                                                        *
*---------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdctab.
  bdctab-program  = program.
  bdctab-dynpro   = dynpro.
  bdctab-dynbegin = 'X'.
  APPEND bdctab.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM BDC_FIELD                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FNAM                                                          *
*  -->  FVAL                                                          *
*---------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdctab.
  bdctab-fnam = fnam.
  bdctab-fval = fval.
  APPEND bdctab.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM CLOSE_BDC_GROUP                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM close_bdc_group.
  CLEAR sy-subrc.
  CALL FUNCTION 'BDC_CLOSE_GROUP'.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM BDC_INSERT                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  TRAN                                                          *
*---------------------------------------------------------------------*
FORM bdc_insert USING tran.
  CLEAR sy-subrc.
  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      tcode     = tran
    TABLES
      dynprotab = bdctab.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM BDC_INSERT_OPT                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  TRAN                                                          *
*---------------------------------------------------------------------*
FORM bdc_insert_opt USING tran.
  bdcopt-updmode = 'S'.
  bdcopt-cattmode = ' '.
  bdcopt-defsize = 'X'.
  bdcopt-racommit = ' '.
  bdcopt-nobinpt  = ' '.
  bdcopt-nobiend  = ' '.

  CLEAR sy-subrc.
  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      tcode     = tran
      ctuparams = bdcopt
    TABLES
      dynprotab = bdctab.
ENDFORM.                    "bdc_insert_opt

*---------------------------------------------------------------------*
*       FORM CALL_TRANSACTION                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  TRAN                                                          *
*  -->  MMODE                                                         *
*---------------------------------------------------------------------*
FORM call_transaction USING tran mmode.
  CALL TRANSACTION  tran
        USING       bdctab
        MODE        mmode
        UPDATE      'S'
        MESSAGES INTO messtab.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM CALL_TRANSACTION                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  TRAN                                                          *
*  -->  MMODE                                                         *
*---------------------------------------------------------------------*
FORM call_transaction_opt USING tran mmode.
  bdcopt-dismode = mmode.
  bdcopt-updmode = 'S'.
  bdcopt-cattmode = ' '.
  bdcopt-defsize = 'X'.
  bdcopt-racommit = ' '.
  bdcopt-nobinpt  = ' '.
  bdcopt-nobiend  = ' '.
  CALL TRANSACTION  tran
        USING       bdctab
        MESSAGES INTO messtab
        OPTIONS  FROM bdcopt.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM OPEN_BDC_GROUP                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  MSESS                                                         *
*---------------------------------------------------------------------*
FORM open_bdc_group USING msess.
  CLEAR sy-subrc.
  CALL FUNCTION 'BDC_OPEN_GROUP'
    EXPORTING
      client   = sy-mandt
      group    = msess
      user     = sy-uname
      keep     = ' '
      holddate = sy-datum.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM SETBCD                                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FNAME                                                         *
*  -->  FVALUE                                                        *
*---------------------------------------------------------------------*
FORM setbcd USING fname fvalue.
  CHECK fvalue NE space.
  ftpost-fnam = fname.
  ftpost-fval = fvalue.
  APPEND ftpost.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM BEGIN_FB01                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  MMODE                                                         *
*---------------------------------------------------------------------*
FORM begin_fb01 USING mmode CHANGING err.
  DATA xsubrc LIKE sy-subrc.
  CALL FUNCTION 'POSTING_INTERFACE_START'
    EXPORTING
      i_function = 'C'
      i_group    = sy-uname
      i_keep     = ' '
      i_mode     = mmode
      i_user     = sy-uname
    EXCEPTIONS
      OTHERS     = 4.
  IF sy-subrc NE 0.
    err = sy-subrc.
    EXIT.
  ENDIF.
  CALL FUNCTION 'POSTING_INTERFACE_DOCUMENT'
    EXPORTING
      i_tcode                  = 'FB01'
    IMPORTING
      e_subrc                  = xsubrc
      e_msgid                  = sy-msgid
      e_msgty                  = sy-msgty
      e_msgno                  = sy-msgno
      e_msgv1                  = sy-msgv1
      e_msgv2                  = sy-msgv2
      e_msgv3                  = sy-msgv3
      e_msgv4                  = sy-msgv4
    TABLES
      t_ftpost                 = ftpost
      t_blntab                 = xblntab
      t_fttax                  = xfttax
    EXCEPTIONS
      account_missing          = 1
      company_code_missing     = 2
      posting_key_invalid      = 3
      posting_key_missing      = 4
      record_type_invalid      = 5
      transaction_code_invalid = 6
      amount_format_error      = 7
      OTHERS                   = 9.
  err = sy-subrc.
  CALL FUNCTION 'POSTING_INTERFACE_END'.
  messtab-tcode   = 'FB01'.
  messtab-msgtyp  = sy-msgty.
  messtab-msgid   = sy-msgid.
  messtab-msgnr   = sy-msgno.
  messtab-msgv1   = sy-msgv1.
  messtab-msgv2   = sy-msgv2.
  messtab-msgv3   = sy-msgv3.
  messtab-msgv4   = sy-msgv4.
  APPEND messtab.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM FTPOST_FIELD                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  FNAM                                                          *
*  -->  FVAL                                                          *
*---------------------------------------------------------------------*
FORM ftpost_field  USING fnam LIKE ftpost-fnam
                         fval LIKE ftpost-fval.
  CLEAR: ftpost-fnam, ftpost-fval.
  ftpost-fnam     = fnam.
  ftpost-fval     = fval.
  APPEND ftpost.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM pop_mess                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  T                                                             *
*  -->  T1                                                            *
*  -->  T2                                                            *
*  -->  T3                                                            *
*  -->  T4                                                            *
*---------------------------------------------------------------------*
FORM pop_mess USING t t1 t2 t3 t4.
  DATA : xtitle(80), tx1(80), tx2(80).
  IF t IS INITIAL.
    xtitle = 'UYARI:'.
  ELSE.
    xtitle = t.
  ENDIF.
  IF t1 IS INITIAL.
    tx1 = '...'.
  ELSE.
    tx1 = t1.
  ENDIF.
  IF t2 IS INITIAL.
    tx2 = '...'.
  ELSE.
    tx2 = t2.
  ENDIF.
  CALL FUNCTION 'POPUP_TO_INFORM'
    EXPORTING
      titel = xtitle
      txt1  = tx1
      txt2  = tx2
      txt3  = t3
      txt4  = t4.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  show_report
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_REPORT  text
*      -->P_PVARI  text
*      -->P_DEFAULT_REPORT_NAME  text
*      -->P_DEFAULT_TAB_NAME  text
*      -->P_DEFAULT_RECNAME  text
*----------------------------------------------------------------------*
FORM show_report TABLES  it_report
                 USING  pvari
                        gs_variant
                        default_report_name
                        default_tab_name
                        default_recname.
  PERFORM layout_init USING gs_layout.
  PERFORM excluding_events.
  PERFORM eventtab_build USING gt_events[].
  PERFORM set_layout USING pvari default_report_name.
  PERFORM get_fcat USING default_tab_name.
  PERFORM set_fcat USING default_recname g_save.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_BACKGROUND_ID         = 'ALV_BACKGROUND'
*     i_buffer_active         = 'X'
      i_bypassing_buffer      = 'X'
      i_callback_program      = default_report_name
      i_structure_name        = default_tab_name
      is_layout               = gs_layout
      i_save                  = g_save
      is_variant              = gs_variant
      it_events               = gt_events[]
      it_excluding            = lt_excluding
      it_fieldcat             = lt_t_fieldcatalog[]
    IMPORTING
      e_exit_caused_by_caller = g_exit_caused_by_caller
      es_exit_caused_by_user  = gs_exit_caused_by_user
    TABLES
      t_outtab                = it_report[]
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
  IF sy-subrc = 0.
    IF g_exit_caused_by_caller = 'X'.
*"  Forced Exit by calling program
*"  <do_something>.
    ELSE.
*"  User left list via F3, F12 or F15
      IF gs_exit_caused_by_user-back = 'X'.       "F3
*"    <do_something>.
      ELSE.
        IF gs_exit_caused_by_user-exit = 'X'.     "F15
*"      <do_something>.
        ELSE.
          IF gs_exit_caused_by_user-cancel = 'X'. "F12
*"        <do_something>.
          ELSE.
*"        should not occur!
*"        <do_Abnormal_End>.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
  ENDIF.

ENDFORM.                    " show_report
*&---------------------------------------------------------------------*
*&      Form  lt_f4_for_variants
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PVARI  text
*----------------------------------------------------------------------*
FORM lt_f4_for_variants USING   pvari.
*
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = gs_variant
      i_save     = g_save
*     it_default_fieldcat =
    IMPORTING
      e_exit     = g_exit
      es_variant = gs_variant
    EXCEPTIONS
      not_found  = 2.
  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S'      NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF g_exit = space.
      pvari = gs_variant-variant.
    ENDIF.
  ENDIF.

ENDFORM.                    " lt_f4_for_variants
*---------------------------------------------------------------------*
*       FORM SET_LAYOUT                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  PVARI                                                         *
*  -->  DRNAME                                                        *
*---------------------------------------------------------------------*
FORM set_layout  USING pvari drname.
  gs_variant-report    = drname.
  gs_variant-username  = '/GENEL'.
  gs_variant-variant   = pvari.
  gs_variant-text      = pvari.
  PERFORM set_layout_user_exit USING pvari drname.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  get_fcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0658   text
*----------------------------------------------------------------------*
FORM get_fcat USING    dtname.
  DATA xrc LIKE sy-subrc.
  REFRESH it_fcatfies.
  CALL FUNCTION 'GET_FIELDTAB'
    EXPORTING
      langu               = sy-langu
      only                = ' '
      tabname             = dtname
      withtext            = 'X'
    IMPORTING
      rc                  = xrc
    TABLES
      fieldtab            = it_fcatfies
    EXCEPTIONS
      internal_error      = 1
      no_texts_found      = 2
      table_has_no_fields = 3
      table_not_activ     = 4.

ENDFORM.                    " get_fcat
*&---------------------------------------------------------------------*
*&      Form  set_fcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0662   text
*      -->P_0663   text
*----------------------------------------------------------------------*
FORM set_fcat USING itname sgrp.
  REFRESH it_sp_groups.
  LOOP AT it_fcatfies.
    CHECK it_fcatfies-fieldname NE 'MANDT'.
    CLEAR lt_fieldcatalog.
    lt_fieldcatalog-fieldname  = it_fcatfies-fieldname.
    lt_fieldcatalog-ref_fieldname = it_fcatfies-fieldname.
    lt_fieldcatalog-ref_tabname   = it_fcatfies-tabname.
    lt_fieldcatalog-tabname       = itname.
    lt_fieldcatalog-key           = it_fcatfies-keyflag.
    lt_fieldcatalog-col_pos       = it_fcatfies-position.
    lt_fieldcatalog-row_pos       = 1.
    lt_fieldcatalog-sp_group      = sgrp.
    IF it_fcatfies-inttype = 'P'.
      lt_fieldcatalog-do_sum = 'X'.
      IF it_fcatfies-datatype(3) = 'CUR'.
        lt_fieldcatalog-cfieldname = it_fcatfies-reffield.
        lt_fieldcatalog-ctabname = itname.
      ELSE.
        lt_fieldcatalog-qfieldname = it_fcatfies-reffield.
        lt_fieldcatalog-qtabname = itname.
      ENDIF.
    ELSE.
*      it_fcat_init-no_sum = 'X'.
    ENDIF.

    APPEND lt_fieldcatalog TO lt_t_fieldcatalog.
  ENDLOOP.
  CLEAR it_sp_groups.
  it_sp_groups-sp_group = sgrp.
  it_sp_groups-text     = sgrp.
  APPEND it_sp_groups.
  PERFORM set_fcat_user_exit USING itname sgrp.

ENDFORM.                    " set_fcat
*---------------------------------------------------------------------*
*       FORM LAYOUT_INIT                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  RS_LAYOUT                                                     *
*---------------------------------------------------------------------*
FORM layout_init USING rs_layout TYPE slis_layout_alv.
*"Build layout for list display
  rs_layout-detail_popup      = 'X'.
  rs_layout-zebra = 'X'.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM EVENTTAB_BUILD                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  RT_EVENTS                                                     *
*---------------------------------------------------------------------*
FORM eventtab_build USING rt_events TYPE slis_t_event.
*"Registration of events to happen during list display
  DATA: ls_event TYPE slis_alv_event.
*
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = rt_events.
  READ TABLE rt_events WITH KEY name = slis_ev_top_of_page
                           INTO ls_event.
  IF sy-subrc = 0.
    LOOP AT ex_events.
      DELETE rt_events WHERE name EQ ex_events-event.
    ENDLOOP.

    LOOP AT ap_events.
      CLEAR ls_event.
      READ TABLE rt_events INTO ls_event
                    WITH KEY name = ap_events-event.
      IF sy-subrc NE 0.
        ls_event-name = ap_events-event.
        APPEND ls_event TO rt_events.
      ENDIF.
    ENDLOOP.

    LOOP AT rt_events INTO ls_event.
      CONCATENATE 'SET_' ls_event-name INTO ls_event-form.
      MODIFY rt_events FROM ls_event INDEX sy-tabix.
    ENDLOOP.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  exclude_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EX_EVENTS  text
*      -->P_2983   text
*----------------------------------------------------------------------*
FORM exclude_events TABLES   ex_events STRUCTURE ex_events
                    USING    p_event.
  ex_events-event = p_event.
  APPEND ex_events.
ENDFORM.                    " exclude_events
*---------------------------------------------------------------------*
*       FORM APPEND_EVENTS                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  AP_EVENTS                                                     *
*  -->  P_EVENT                                                       *
*---------------------------------------------------------------------*
FORM append_events TABLES   ap_events STRUCTURE ap_events
                    USING    p_event.
  ap_events-event = p_event.
  APPEND ap_events.
ENDFORM.                    " exclude_events
*&---------------------------------------------------------------------*
*&      Form  ato_fcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1402   text
*      -->P_1403   text
*      -->P_1404   text
*      -->P_1405   text
*      -->P_1406   text
*      -->P_1407   text
*      -->P_1408   text
*      -->P_1      text
*----------------------------------------------------------------------*
FORM ato_fcat USING tb fn rt rf in ik ts tm tl ps.
  "tb : table ad#
  "fn : field ad#
  "rt : referans table ad#
  "rf : referans field ad#
  "rt : IT_REPORT rf : 'WAERK' gibi
  "in : Alan#n IT_REPORT taki ad#
  "ik : Key alan g#stergesi X olabilir
  "ts : Short text
  "tm : Medium text
  "tl : Long Text
  "ps : Position. Fcat te ki s#ras#
  DATA xdfies LIKE dfies OCCURS 100.
  CLEAR it_fcatfies.
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname        = tb
      fieldname      = fn
      langu          = sy-langu
    TABLES
      dfies_tab      = xdfies
    EXCEPTIONS
      not_found      = 1
      internal_error = 2
      OTHERS         = 3.
  IF sy-subrc <> 0. ENDIF.
  READ TABLE xdfies INTO it_fcatfies INDEX 1.
  it_fcatfies-reftable = rt.
  it_fcatfies-reffield = rf.
  it_fcatfies-fieldname = in.
  it_fcatfies-tb = tb.
  it_fcatfies-fn = fn.
*  move-corresponding it_fcatfies to lt_fieldcatalog.
*  describe table it_fcatfies lines sy-tfill.
*  it_fcatfies-col_pos = sy-tfill + 1.
*  it_fcat_init-datatype = dt.
*  it_fcat_init-outputlen = ol.
*  it_fcat_init-col_pos = cp.
*  it_fcat_init-no_zero = nz.
  it_fcatfies-position = ps.
  IF ts NE ''.
    it_fcatfies-scrtext_s = ts.
    it_fcatfies-reptext = ts.
  ENDIF.
  IF tm NE ''.
    it_fcatfies-scrtext_m = tm.
  ENDIF.
  IF tl NE ''.
    it_fcatfies-scrtext_l = tl.
  ENDIF.
  it_fcatfies-keyflag = ik.
  APPEND it_fcatfies.
ENDFORM.                    " ato_fcat
*&---------------------------------------------------------------------*
*&      Form  show_report_fcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_REPORT  text
*      -->P_PVARI  text
*      -->P_GS_VARIANT  text
*      -->P_DEFAULT_REPORT_NAME  text
*      -->P_DEFAULT_RECNAME  text
*----------------------------------------------------------------------*
FORM show_report_fcat TABLES it_report
               USING    p_vari
                        gs_variant
                        default_report_name
                        default_recname.
  PERFORM layout_init USING gs_layout.
  PERFORM excluding_events.
  PERFORM eventtab_build USING gt_events[].
  PERFORM set_layout USING p_vari default_report_name.
  PERFORM s_fcat USING default_recname g_save.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_BACKGROUND_ID         = 'ALV_BACKGROUND'
*     i_buffer_active         = 'X'
      i_bypassing_buffer      = 'X'
      i_callback_program      = default_report_name
*     i_structure_name        = default_tab_name
      i_grid_settings         = gs_grid_set
      is_layout               = gs_layout
      i_save                  = g_save
      is_variant              = gs_variant
      it_events               = gt_events[]
      it_excluding            = lt_excluding
      it_fieldcat             = lt_t_fieldcatalog[]
      is_print                = gs_print
    IMPORTING
      e_exit_caused_by_caller = g_exit_caused_by_caller
      es_exit_caused_by_user  = gs_exit_caused_by_user
    TABLES
      t_outtab                = it_report[]
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
  IF sy-subrc = 0.
    IF g_exit_caused_by_caller = 'X'.
*"  Forced Exit by calling program
*"  <do_something>.
    ELSE.
*"  User left list via F3, F12 or F15
      IF gs_exit_caused_by_user-back = 'X'.       "F3
*"    <do_something>.
      ELSE.
        IF gs_exit_caused_by_user-exit = 'X'.     "F15
*"      <do_something>.
        ELSE.
          IF gs_exit_caused_by_user-cancel = 'X'. "F12
*"        <do_something>.
          ELSE.
*"        should not occur!
*"        <do_Abnormal_End>.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
  ENDIF.
ENDFORM.                    " show_report_fcat
*&---------------------------------------------------------------------*
*&      Form  s_fcat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DEFAULT_RECNAME  text
*      -->P_G_SAVE  text
*----------------------------------------------------------------------*
FORM s_fcat USING itname sgrp.
  REFRESH it_sp_groups.
  LOOP AT it_fcatfies.
    CHECK it_fcatfies-fieldname NE 'MANDT'.
    CLEAR lt_fieldcatalog.
    lt_fieldcatalog-fieldname  = it_fcatfies-fieldname.
*    lt_fieldcatalog-ref_fieldname = it_fcatfies-fieldname.
*    lt_fieldcatalog-ref_tabname   = it_fcatfies-tabname.
    lt_fieldcatalog-ref_fieldname = it_fcatfies-fn.
    lt_fieldcatalog-ref_tabname   = it_fcatfies-tb.
    lt_fieldcatalog-tabname       = itname.
    lt_fieldcatalog-key           = it_fcatfies-keyflag.
    lt_fieldcatalog-col_pos       = it_fcatfies-position.
    lt_fieldcatalog-row_pos       = 1.
    lt_fieldcatalog-sp_group      = sgrp.
*    lt_fieldcatalog-reptext       = it_fcatfies-reptext.
*    lt_fieldcatalog-ddictxt(1)     type c,    " (S)hort (M)iddle (L)ong
    lt_fieldcatalog-seltext_s     = it_fcatfies-scrtext_s.
    lt_fieldcatalog-seltext_m     = it_fcatfies-scrtext_m.
    lt_fieldcatalog-seltext_l     = it_fcatfies-scrtext_l.
    lt_fieldcatalog-reptext_ddic = it_fcatfies-scrtext_s.

    IF it_fcatfies-inttype = 'P'.
      lt_fieldcatalog-do_sum = 'X'.
      IF it_fcatfies-datatype(3) = 'CUR'.
        lt_fieldcatalog-cfieldname = it_fcatfies-reffield.
        lt_fieldcatalog-ctabname = itname.
      ELSE.
        lt_fieldcatalog-qfieldname = it_fcatfies-reffield.
        lt_fieldcatalog-qtabname = itname.
      ENDIF.
    ELSE.
*      it_fcat_init-no_sum = 'X'.
    ENDIF.

    APPEND lt_fieldcatalog TO lt_t_fieldcatalog.
  ENDLOOP.
  CLEAR it_sp_groups.
  it_sp_groups-sp_group = sgrp.
  it_sp_groups-text     = sgrp.
  APPEND it_sp_groups.
  PERFORM set_fcat_user_exit USING itname sgrp.
ENDFORM.                    " s_fcat
*&---------------------------------------------------------------------*
*&      Form  set_color
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1116   text
*      -->P_1117   text
*      -->P_1118   text
*----------------------------------------------------------------------*
FORM set_color USING fieldname col int inv nokeycol.
  CLEAR color.
  REFRESH color.
  set_alvcol : color fieldname col int inv nokeycol.
  "Col de#erleri
  "1 = Mavi
  "2 = A##k mavi
  "3 = Sar#
  "5 = Ye#il
  "6 = K#rm#z#
  "7 = A##k kahverengi
ENDFORM.                " set_color
*&---------------------------------------------------------------------*
*&      Form  set_color_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_REPORT  text
*----------------------------------------------------------------------*
FORM set_color_table TABLES it_color TYPE kkblo_t_specialcol USING idx.

  DATA : idm TYPE i.

  CHECK it_color IS INITIAL.

  idm = idx MOD 2.

  IF idm EQ 0.
    PERFORM set_color USING '' '1' '0' '0' ' '.
  ELSE.
    PERFORM set_color USING '' '0' '1' '0' ' '.
  ENDIF.
  APPEND LINES OF color TO it_color.

ENDFORM.                    " set_color_table
*&---------------------------------------------------------------------*
*&      Form  set_line_field_cat
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LT_T_FIELDCATALOG  text
*      -->P_TABNAME  text
*      -->P_1551   text
*      -->P_1552   text
*      -->P_1553   text
*----------------------------------------------------------------------*
FORM set_line_field_cat TABLES
                     t_fieldcatalog STRUCTURE  lt_t_fieldcatalog
                        USING  p_tabname TYPE  slis_tabname
                               p_fieldname
                               p_property
                               p_value.

  DATA: v_field    TYPE STANDARD TABLE OF slis_fieldname WITH HEADER LINE,
        v_property TYPE STANDARD TABLE OF slis_fieldname WITH HEADER LINE.


  DATA tabix LIKE sy-tabix.
  FIELD-SYMBOLS: <f1d>.
  SPLIT p_fieldname AT '/' INTO TABLE v_field.
  SPLIT p_property  AT '/' INTO TABLE v_property.
  LOOP AT v_field.
    READ TABLE t_fieldcatalog WITH KEY tabname   = p_tabname
                                       fieldname = v_field.
    tabix = sy-tabix.
    IF sy-subrc = 0.
      LOOP AT v_property.
        ASSIGN COMPONENT v_property OF STRUCTURE t_fieldcatalog TO <f1d>.
        <f1d> = p_value.
        MODIFY t_fieldcatalog INDEX tabix.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " set_line_field_cat
