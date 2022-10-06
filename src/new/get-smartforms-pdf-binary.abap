FUNCTION z_get_return_dlv_print.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_MBLNR) TYPE  MKPF-MBLNR
*"     REFERENCE(IV_MJAHR) TYPE  MKPF-MJAHR
*"  EXPORTING
*"     REFERENCE(EV_PDF) TYPE  XSTRING
*"----------------------------------------------------------------------

  DATA: lv_fm_name            TYPE rs38l_fnam.
  DATA: lv_formname           TYPE tdsfname.
  DATA: lv_objky              TYPE nast-objky.
  DATA: cf_retcode TYPE sy-subrc.

* SmartForm from customizing table TNAPR
  lv_formname = 'ZMM_SF_RETURNDELIVERY'.
  lv_objky    = iv_mblnr && iv_mjahr && '%'.

  SELECT * FROM nast
    WHERE objky LIKE lv_objky
    AND   kschl = 'ZRTR'
    ORDER BY erdat DESCENDING eruhr DESCENDING.
    EXIT.
  ENDSELECT.

  IF cf_retcode = 0.
    PERFORM get_data CHANGING cf_retcode.
  ENDIF.

  IF cf_retcode = 0.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = lv_formname
      IMPORTING
        fm_name            = lv_fm_name
      EXCEPTIONS
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    gwa_control-langu      = nast-spras.
    gwa_control-no_dialog  = abap_true.
    gwa_control-getotf     = abap_true.
    gwa_ssfcompop-tddest   = 'LPCS'.
*   gwa_ssfcompop-tdnoprev = 'X' .

    CALL FUNCTION lv_fm_name
      EXPORTING
        control_parameters = gwa_control
        output_options     = gwa_ssfcompop
        is_header          = gs_header
        it_items           = gt_items[]
        is_nast            = nast
      IMPORTING
        job_output_info    = gs_job_output
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
      cf_retcode = sy-subrc.
      PERFORM protocol_update.
      PERFORM add_smfrm_prot.
    ENDIF.

    DATA lt_lines TYPE STANDARD TABLE OF tline.
    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        format                = 'PDF'
      IMPORTING
        bin_file              = ev_pdf
      TABLES
        otf                   = gs_job_output-otfdata
        lines                 = lt_lines
      EXCEPTIONS
        err_max_linewidth     = 1
        err_format            = 2
        err_conv_not_possible = 3
        err_bad_otf           = 4
        OTHERS                = 5.

  ENDIF.

ENDFUNCTION.

FORM get_data  CHANGING cf_retcode.

  DATA: lv_mblnr TYPE mseg-mblnr,
        lv_total TYPE p DECIMALS 2 VALUE 0.

  DATA: lt_hdlines TYPE TABLE OF tline WITH HEADER LINE .

  lv_mblnr = nast-objky.


  CLEAR : gs_header,
          gt_items.



  SELECT SINGLE mseg~mblnr,
                mseg~ebeln,
                mseg~bukrs,
                mseg~lifnr,
                mseg~usnam_mkpf,
                adrc~name1,
                adrc~city1,
                adrc~street,
                adrc~house_num1,
                adrc~post_code1
 FROM mseg
 INNER JOIN tvko ON tvko~bukrs EQ mseg~bukrs
 INNER JOIN adrc ON adrc~addrnumber EQ tvko~adrnr
 INTO @DATA(ls_datab)
 WHERE mseg~mblnr EQ @lv_mblnr
   AND mseg~bukrs EQ 'C003'.


  SELECT SINGLE bedat
    FROM ekko
   INTO @DATA(lv_bedat)
   WHERE ebeln EQ @ls_datab-ebeln.



  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = ls_datab-ebeln
    IMPORTING
      output = gs_header-ebeln.

  gs_header-mblnr        = ls_datab-mblnr.
  gs_header-bukrs        = ls_datab-bukrs.
  gs_header-bedat        = lv_bedat.
  gs_header-lifnr        = ls_datab-lifnr.
  gs_header-b_nam1       = ls_datab-name1.
  gs_header-b_city1      = ls_datab-city1.
  gs_header-b_street     = ls_datab-street.
  gs_header-b_house_num1 = ls_datab-house_num1.

  CONCATENATE ls_datab-mblnr
              '-'
              ls_datab-bukrs
              '-'
              syst-sysid
              INTO
              gs_header-po_num.

  CONCATENATE ls_datab-ebeln
            '-'
            ls_datab-bukrs
            '-'
            syst-sysid
            INTO
            gs_header-order_num.


  CONCATENATE 'PSČ'
              ls_datab-post_code1
              INTO
              gs_header-b_post_code1 SEPARATED BY space.



  SELECT SINGLE adrc~name1,
                adrc~name2,
                adrc~city1,
                adrc~street,
                adrc~house_num1,
                adrc~post_code1
   FROM mseg
   INNER JOIN t001 ON t001~bukrs EQ mseg~bukrs
   INNER JOIN adrc ON adrc~addrnumber EQ t001~adrnr
   INTO @DATA(ls_datac)
   WHERE mseg~mblnr EQ @lv_mblnr
     AND mseg~bukrs EQ 'C003'.

  gs_header-ad_nam1       = ls_datac-name1.
  gs_header-ad_nam2       = ls_datac-name2.
  gs_header-ad_city1      = ls_datac-city1.
  gs_header-ad_street     = ls_datac-street.
  gs_header-ad_house_num1 = ls_datac-house_num1.
  gs_header-ad_post_code1 = ls_datac-post_code1.


  SELECT SINGLE lfa1~name1,
                lfa1~stras,
                lfa1~pstlz,
                lfa1~ort01,
                lfa1~land1,
                lfa1~stcd1,
                lfa1~stcd2
 FROM mseg
 INNER JOIN lfa1 ON lfa1~lifnr EQ mseg~lifnr
 INTO @DATA(ls_datad)
 WHERE mseg~mblnr EQ @lv_mblnr
   AND mseg~bukrs EQ 'C003'.

  gs_header-lfa_name  = ls_datad-name1.
  gs_header-lfa_stras = ls_datad-stras.
  gs_header-lfa_pstlz = ls_datad-pstlz.
  gs_header-lfa_ort1  = ls_datad-ort01.
  IF ls_datad-land1 EQ 'CZ'.
    nast-spras = 'C'.
  ELSE.
    gs_header-lfa_land1 = ls_datad-land1.
  ENDIF.

  gs_header-lfa_stcd1 = ls_datad-stcd1.
  gs_header-lfa_stcd2 = ls_datad-stcd2.


  SELECT SINGLE t001w~werks,
                t001w~name1,
                t001w~stras,
                t001w~pstlz,
                t001w~ort01
  FROM mseg
  INNER JOIN t001w ON t001w~werks EQ mseg~werks
  INTO @DATA(ls_dataf)
  WHERE mseg~mblnr EQ @lv_mblnr
    AND mseg~bukrs EQ 'C003'.

  gs_header-t_werks = ls_dataf-werks.
  gs_header-t_name1 = ls_dataf-name1.
  gs_header-t_stras = ls_dataf-stras.
  gs_header-t_pstlz = ls_dataf-pstlz.
  gs_header-t_ort1  = ls_dataf-ort01.




  SELECT SINGLE persnumber
     FROM usr21
    INTO @DATA(lv_persnumber)
   WHERE bname EQ @ls_datab-usnam_mkpf.

  SELECT SINGLE name_first,
                name_last
      FROM adrp
    INTO @DATA(ls_name)
   WHERE persnumber EQ @lv_persnumber.

  gs_header-name_first = ls_name-name_first.
  gs_header-name_last  = ls_name-name_last.


  SELECT SINGLE telnr_long
    FROM adr2
    INTO @DATA(lv_telnr_long)
  WHERE persnumber EQ @lv_persnumber.


  SELECT SINGLE smtp_addr
    FROM adr6
   INTO @DATA(lv_smtp_addr)
  WHERE persnumber EQ @lv_persnumber.


  IF lv_telnr_long IS NOT INITIAL.
    CONCATENATE  ls_name-name_first
                 ls_name-name_last
                 ','
                 'Tel:'
                 lv_telnr_long
                 ','
                 'E-mail:'
                 lv_smtp_addr
                 INTO gs_header-contact_person SEPARATED BY space.
  ELSE.
    CONCATENATE  ls_name-name_first
                 ls_name-name_last
                  ','
                 'E-mail:'
                 lv_smtp_addr
                 INTO gs_header-contact_person SEPARATED BY space.

  ENDIF.



  SELECT SINGLE cpudt,
                cputm,
                budat
  FROM mkpf
  INTO @DATA(ls_datah)
  WHERE mblnr EQ @lv_mblnr.

  gs_header-cpudt = ls_datah-cpudt.
  gs_header-cputm = ls_datah-cputm.
  gs_header-budat = ls_datah-budat.

  SELECT mseg~zeile,
         mseg~matnr,
         ekpo~txz01,
         ekpo~idnlf,
         mseg~erfmg,
         mseg~erfme
   FROM mseg
   INNER JOIN ekpo ON  ekpo~ebeln EQ mseg~ebeln
                   AND ekpo~ebelp EQ mseg~ebelp
   INTO TABLE @DATA(lt_items)
   WHERE mseg~mblnr EQ @lv_mblnr
     AND mseg~bukrs EQ 'C003'.



  LOOP AT lt_items INTO DATA(ls_items).

    gs_items-zeile = ls_items-zeile.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
      EXPORTING
        input  = ls_items-matnr
      IMPORTING
        output = gs_items-matnr.

    gs_items-idnlf = ls_items-idnlf.
    gs_items-txz01 = ls_items-txz01.
    gs_items-erfmg = ls_items-erfmg.

    gs_items-erfme = ls_items-erfme.

*    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
*      EXPORTING
*        input          = ls_items-meins
*        language       = sy-langu
*      IMPORTING
**       LONG_TEXT      =
*        output         = gs_items-meins
**       SHORT_TEXT     =
*      EXCEPTIONS
*        unit_not_found = 1
*        OTHERS         = 2.
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.


    APPEND gs_items TO gt_items.

  ENDLOOP.



ENDFORM.

FORM protocol_update .

  CHECK xscreen = space.
  CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
    EXPORTING
      msg_arbgb = syst-msgid
      msg_nr    = syst-msgno
      msg_ty    = syst-msgty
      msg_v1    = syst-msgv1
      msg_v2    = syst-msgv2
      msg_v3    = syst-msgv3
      msg_v4    = syst-msgv4
    EXCEPTIONS
      OTHERS    = 1.

ENDFORM.

FORM add_smfrm_prot .

  DATA: lt_errortab             TYPE tsferror.
* DATA: LF_MSGNR                TYPE SY-MSGNO.   "DEL_HP_335958
  FIELD-SYMBOLS: <fs_errortab>  TYPE LINE OF tsferror.

* get smart form protocoll
  CALL FUNCTION 'SSF_READ_ERRORS'
    IMPORTING
      errortab = lt_errortab.

* add smartform protocoll to nast protocoll
  LOOP AT lt_errortab ASSIGNING <fs_errortab>.
*    CLEAR LF_MSGNR.                             "DEL_HP_335958
*    LF_MSGNR = <FS_ERRORTAB>-ERRNUMBER.         "DEL_HP_335958
    CALL FUNCTION 'NAST_PROTOCOL_UPDATE'
      EXPORTING
        msg_arbgb = <fs_errortab>-msgid
*       MSG_NR    = LF_MSGNR               "DEL_HP_335958
        msg_nr    = <fs_errortab>-msgno    "INS_HP_335958
        msg_ty    = <fs_errortab>-msgty
        msg_v1    = <fs_errortab>-msgv1
        msg_v2    = <fs_errortab>-msgv2
        msg_v3    = <fs_errortab>-msgv3
        msg_v4    = <fs_errortab>-msgv4
      EXCEPTIONS
        OTHERS    = 1.
  ENDLOOP.

ENDFORM.