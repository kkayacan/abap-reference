REPORT  zhr_yillik_izin_kaydi_formu.

INFOTYPES: 0001, 0002, 0041, 2001.

INCLUDE ole2incl.
INCLUDE zabap2xlsx_simple.

TABLES pernr.

DATA gt_list TYPE zhr_t_yillik_izin.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-s01.
PARAMETERS: p_pdf RADIOBUTTON GROUP r01,
            p_xls RADIOBUTTON GROUP r01,
            p_xln RADIOBUTTON GROUP r01,
            p_path TYPE string LOWER CASE OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b01.

INITIALIZATION.
  PERFORM initialization.

AT SELECTION-SCREEN OUTPUT.
  PERFORM at_selection_screen_output.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  PERFORM value_request_path CHANGING p_path.

AT SELECTION-SCREEN.
  PERFORM at_selection_screen.

START-OF-SELECTION.

GET pernr.
  PERFORM get_pernr.

END-OF-SELECTION.
  PERFORM end_of_selection.

*&---------------------------------------------------------------------*
*&      Form  initialization
*&---------------------------------------------------------------------*
FORM initialization.
  cl_gui_frontend_services=>get_desktop_directory( CHANGING desktop_directory = p_path ).
  cl_gui_cfw=>flush( ).
ENDFORM.                    "initialization

*&---------------------------------------------------------------------*
*&      Form  at_selection_screen_output
*&---------------------------------------------------------------------*
FORM at_selection_screen_output.
  LOOP AT SCREEN.
    IF screen-name CS 'PNPSTAT2' OR
       screen-name CS 'PNPBUKRS' OR
       screen-name CS 'PNPABKRS' OR
       screen-name CS 'PNPXBWBK' OR
       screen-name CS 'PNPXPGPK'.
      screen-active = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.                    "at_selection_screen_output

*&---------------------------------------------------------------------*
*&      Form  value_request_path
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->SELECTED_FOLDER  text
*----------------------------------------------------------------------*
FORM value_request_path CHANGING selected_folder TYPE string.
  DATA: new_path      TYPE string,
      repid         TYPE syrepid,
      dynnr         TYPE sydynnr,
      lt_dynpfields TYPE TABLE OF dynpread,
      ls_dynpfields LIKE LINE OF lt_dynpfields,
      title         TYPE string.

* Get current value
  dynnr = sy-dynnr.
  repid = sy-repid.
  ls_dynpfields-fieldname = 'P_PATH'.
  APPEND ls_dynpfields TO lt_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = repid
      dynumb               = dynnr
    TABLES
      dynpfields           = lt_dynpfields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  READ TABLE lt_dynpfields INTO ls_dynpfields INDEX 1.

  new_path = ls_dynpfields-fieldvalue.
  selected_folder = new_path.
  title = text-s02.

  cl_gui_frontend_services=>directory_browse(
    EXPORTING
      window_title         = title
      initial_folder       = new_path
    CHANGING
      selected_folder      = new_path
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4
         ).
  cl_gui_cfw=>flush( ).
  CHECK new_path IS NOT INITIAL.
  selected_folder = new_path.
ENDFORM.                    "value_request_path

*&---------------------------------------------------------------------*
*&      Form  at_selection_screen
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM at_selection_screen.
  DATA rc TYPE abap_bool.
  IF sy-ucomm = 'ONLI' AND p_xls = abap_true.
    CALL METHOD cl_gui_frontend_services=>directory_exist
      EXPORTING
        directory            = p_path
      RECEIVING
        result               = rc
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5.
    IF rc <> 'X'.
      MESSAGE text-m01 TYPE 'E'.
    ENDIF.
  ENDIF.
ENDFORM.                    "at_selection_screen
*&---------------------------------------------------------------------*
*&      Form  get_pernr
*&---------------------------------------------------------------------*
FORM get_pernr.

  TYPES: BEGIN OF lst_2006,
          docnr TYPE ptquoded-docnr,
          begda TYPE pa2006-begda,
          anzhl TYPE pa2006-anzhl,
         END OF lst_2006.

  DATA: lt_2006 TYPE TABLE OF lst_2006,
        lv_anzhl_c TYPE c LENGTH 30,
        lv_year  TYPE c LENGTH 4,
        lv_darxx TYPE p0041-dar01,
        lv_datxx TYPE p0041-dat01,
        lv_anzhl TYPE pa2006-anzhl,
        lv_kverb TYPE pa2006-kverb.

  FIELD-SYMBOLS: <ls_list> LIKE LINE OF gt_list,
                 <ls_2006> TYPE lst_2006.

  rp_provide_from_last p0001 '' pn-begda pn-endda.
  rp_provide_from_last p0041 '' pn-begda pn-endda.
  DO 12 TIMES
  VARYING lv_darxx FROM p0041-dar01 NEXT p0041-dar02
  VARYING lv_datxx FROM p0041-dat01 NEXT p0041-dat02.
    IF p0001-persg = '2'.
      IF lv_darxx = '01'.
        EXIT .
      ENDIF.
    ELSEIF p0001-persg = '1'.
      IF lv_darxx = '03'.
        EXIT .
      ENDIF.
    ENDIF.
  ENDDO.

  rp_provide_from_last p0002 '' pn-begda pn-endda.

  SELECT ptquoded~docnr pa2006~begda pa2006~anzhl
    FROM pa2006
    JOIN ptquoded ON ptquoded~quonr = pa2006~quonr
    INTO TABLE lt_2006
    WHERE pa2006~pernr = pernr-pernr.

  SELECT SUM( anzhl ) SUM( kverb )
    FROM pa2006
    INTO (lv_anzhl, lv_kverb)
    WHERE pernr = pernr-pernr
    AND   subty = '01'.

  SORT p2001 BY begda.

  LOOP AT p2001 WHERE ( subty = '0300' OR subty = '0305' )
                AND   begda <= pn-endda
                AND   endda >= pn-begda.
    APPEND INITIAL LINE TO gt_list ASSIGNING <ls_list>.
    <ls_list>-pernr = p2001-pernr.
    <ls_list>-vorna = p0002-vorna.
    <ls_list>-nachn = p0002-nachn.
    CLEAR lv_year.
    LOOP AT lt_2006 ASSIGNING <ls_2006> WHERE docnr = p2001-docnr.
      WRITE <ls_2006>-anzhl TO lv_anzhl_c DECIMALS 0.
      CONDENSE lv_anzhl_c.
      IF <ls_list>-year IS INITIAL OR <ls_2006>-begda(4) = lv_year.
        IF <ls_list>-year IS INITIAL.
          <ls_list>-year = <ls_2006>-begda(4).
        ENDIF.
        IF <ls_list>-anzh6 IS INITIAL.
          <ls_list>-anzh6 = lv_anzhl_c.
        ENDIF.
      ELSE.
        CONCATENATE <ls_list>-year '-' <ls_2006>-begda(4) INTO <ls_list>-year.
        CONCATENATE <ls_list>-anzh6 '-' lv_anzhl_c INTO <ls_list>-anzh6.
      ENDIF.
      lv_year = <ls_2006>-begda(4).
      <ls_list>-begd6 = <ls_2006>-begda.
      <ls_list>-begd1 = p2001-begda.
      <ls_list>-endd1 = p2001-endda.
      <ls_list>-abrtg = p2001-abrtg.
      <ls_list>-dardt = lv_datxx.
    ENDLOOP.
  ENDLOOP.
  IF <ls_list> IS ASSIGNED.
    <ls_list>-quonm = lv_anzhl - lv_kverb.
  ENDIF.

ENDFORM.                    "get_pernr

*&---------------------------------------------------------------------*
*&      Form  end_of_selection
*&---------------------------------------------------------------------*
FORM end_of_selection.
  DATA lt_emp LIKE gt_list.
  DATA lv_count TYPE i.
  DATA lv_total TYPE i.
  FIELD-SYMBOLS <ls_emp> LIKE LINE OF lt_emp.

  IF gt_list IS INITIAL.
    MESSAGE s004(sv) DISPLAY LIKE 'E'.
*   Seçime uygun bir giriş bulunamadı
    RETURN.
  ENDIF.

  lt_emp = gt_list.
  SORT lt_emp BY pernr.
  DELETE ADJACENT DUPLICATES FROM lt_emp COMPARING pernr.
  lv_total = LINES( lt_emp ).
  LOOP AT lt_emp ASSIGNING <ls_emp>.
    ADD 1 TO lv_count.
    CASE 'X'.
      WHEN p_pdf. PERFORM save_pdf USING <ls_emp>-pernr.
      WHEN p_xls. PERFORM save_xls USING <ls_emp>-pernr lv_count lv_total.
      WHEN p_xln. PERFORM save_xln USING <ls_emp>-pernr.
    ENDCASE.
  ENDLOOP.
ENDFORM.                    "end_of_selection
*&---------------------------------------------------------------------*
*&      Form  save_pdf
*&---------------------------------------------------------------------*
FORM save_pdf USING ip_pernr.

  DATA: fmname         TYPE rs38l_fnam,
        ls_output      TYPE ssfcrescl,
        lt_otf         TYPE TABLE OF itcoo,
        lv_size        TYPE i,
        lt_lines       TYPE TABLE OF tline,
        lv_filename    TYPE string,
        lv_path        TYPE string,
        lv_fullpath    TYPE string,
        lv_user_action TYPE i,
        ls_control_param TYPE ssfctrlop,
        ls_output_opt TYPE ssfcompop,
        lt_list TYPE zhr_t_yillik_izin.

  lt_list = gt_list.
  DELETE lt_list WHERE pernr <> ip_pernr.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZHR_YILLIK_IZIN'
    IMPORTING
      fm_name            = fmname
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  ls_control_param-no_dialog = abap_true.
  ls_control_param-getotf    = abap_true.
  ls_output_opt-tddest      = 'LP01'.

  CALL FUNCTION fmname
    EXPORTING
      control_parameters = ls_control_param
      output_options     = ls_output_opt
      user_settings      = abap_false
      it_list            = lt_list
    IMPORTING
      job_output_info    = ls_output
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  APPEND LINES OF ls_output-otfdata TO lt_otf.

  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      format                = 'PDF'
    IMPORTING
      bin_filesize          = lv_size
    TABLES
      otf                   = lt_otf
      lines                 = lt_lines
    EXCEPTIONS
      err_max_linewidth     = 1
      err_format            = 2
      err_conv_not_possible = 3
      err_bad_otf           = 4
      OTHERS                = 5.
  IF sy-subrc <> 0.

  ENDIF.

  PERFORM get_fullpath USING p_path ip_pernr '.PDF' CHANGING lv_fullpath.

  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      bin_filesize            = lv_size
      filename                = lv_fullpath
      filetype                = 'BIN'
    CHANGING
      data_tab                = lt_lines
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      not_supported_by_gui    = 22
      error_no_gui            = 23
      OTHERS                  = 24.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    "save_pdf
*&---------------------------------------------------------------------*
*&      Form  save_xls
*&---------------------------------------------------------------------*
FORM save_xls USING ip_pernr ip_count ip_total.

  CONSTANTS: c_center_align TYPE i VALUE -4108,
             c_top_align    TYPE i VALUE -4160,
             c_thin_border  TYPE i VALUE 2,
             c_wrap_text    TYPE i VALUE 1.

  DATA: ls_excel TYPE ole2_object,
        ls_wbooklist TYPE ole2_object,
        ls_application TYPE ole2_object,
        ls_wbook TYPE ole2_object,
        ls_activesheet TYPE ole2_object,
        lv_fullpath TYPE string,
        lv_line TYPE i,
        lv_val TYPE c LENGTH 100,
        lt_list LIKE gt_list,
        lv_lines TYPE i,
        lv_count TYPE i.

  DATA: lv_perc TYPE i.

  FIELD-SYMBOLS <ls_list> LIKE LINE OF gt_list.

  lv_perc = 100 * ip_count / ip_total.
  SUBTRACT 1 FROM lv_perc.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = lv_perc
      text       = text-s03.

  lt_list = gt_list.
  DELETE lt_list WHERE pernr <> ip_pernr.
  lv_lines = LINES( lt_list ).

  CREATE OBJECT ls_excel 'EXCEL.APPLICATION'.
  SET PROPERTY OF ls_excel 'VISIBLE' = 0.
  CALL METHOD OF ls_excel 'WORKBOOKS' = ls_wbook.
  CALL METHOD OF ls_wbook 'Add' = ls_wbook.
  GET PROPERTY OF ls_excel 'ActiveSheet' = ls_activesheet.

  PERFORM set_col_width USING ls_activesheet 1 '11.43'.
  PERFORM set_col_width USING ls_activesheet 9 '11.43'.
  PERFORM set_col_width USING ls_activesheet 13 '11.43'.
  PERFORM set_col_width USING ls_activesheet 14 '11.43'.
  PERFORM set_row_height USING ls_activesheet 1 '20.25'.
  CONCATENATE 'SİCİL NO :' ip_pernr INTO lv_val SEPARATED BY space.
  PERFORM set_area USING ls_excel 3 6 1 3 lv_val 0 8 1 0 0 c_top_align 1 c_thin_border 0.
  READ TABLE gt_list ASSIGNING <ls_list> WITH KEY pernr = ip_pernr.
  CONCATENATE 'ADI :' <ls_list>-vorna INTO lv_val SEPARATED BY space.
  PERFORM set_area USING ls_excel 3 6 4 8 lv_val 0 8 1 0 0 c_top_align 1 c_thin_border 0.
  CONCATENATE 'SOYADI :' <ls_list>-nachn INTO lv_val SEPARATED BY space.
  PERFORM set_area USING ls_excel 3 6 9 13 lv_val 0 8 1 0 0 c_top_align 1 c_thin_border 0.
  WRITE <ls_list>-dardt TO lv_val DD/MM/YYYY.
  CONCATENATE 'İŞE GİRİŞ TARİHİ :' lv_val INTO lv_val SEPARATED BY space.
  PERFORM set_area USING ls_excel 3 6 14 17 lv_val 0 8 1 0 0 c_top_align 1 c_thin_border 0.

  PERFORM set_area USING ls_excel 7 8 1 1 'Yılı' 0 7 1 0 c_center_align c_center_align 1 c_thin_border 0.
  PERFORM set_area USING ls_excel 7 8 2 2 'Bir Yıl Önceki İzin Hakkını Kazandığı Tarih' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  PERFORM set_area USING ls_excel 7 7 3 8 'Bir Yıllık Çalışma Süresi İçinde Çalışılmayan Gün Sayısı ve Nedenleri' 0 7 1 0 c_center_align c_center_align 1 c_thin_border 0.
  PERFORM set_area USING ls_excel 8 8 3 3 'Hastalık' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  PERFORM set_area USING ls_excel 8 8 4 4 'Askerlik' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  PERFORM set_area USING ls_excel 8 8 5 5 'Zorunluluk Hali' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  PERFORM set_area USING ls_excel 8 8 6 6 'Devamsızlık' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  PERFORM set_area USING ls_excel 8 8 7 7 'Hizmete Ara Verme' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  PERFORM set_area USING ls_excel 8 8 8 8 'Diğer Nedenler' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  PERFORM set_area USING ls_excel 7 8 9 9 'İzne Hak Kazandığı Tarih' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  PERFORM set_area USING ls_excel 7 8 10 10 'İşyerindeki Kıdemi' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  PERFORM set_area USING ls_excel 7 8 11 11 'İzin Süresi' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  PERFORM set_area USING ls_excel 7 8 12 12 'Yol İzni' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  PERFORM set_area USING ls_excel 7 8 13 13 'İzne Başlangıç Tarihi' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  PERFORM set_area USING ls_excel 7 8 14 14 'İzin Bitiş Tarihi' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  PERFORM set_area USING ls_excel 7 8 15 15 'Kullanılan İzin Günü' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  PERFORM set_area USING ls_excel 7 8 16 16 'Kalan İzin Günü' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  PERFORM set_area USING ls_excel 7 8 17 17 'İşçinin İmzası' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.

  lv_line = 8.
  LOOP AT lt_list ASSIGNING <ls_list>.
    ADD 1 TO: lv_line, lv_count.
*    PERFORM set_area USING ls_excel lv_line lv_line 1 1 <ls_list>-year 0 12 0 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
    PERFORM set_area USING ls_excel lv_line lv_line 1 1 '' 0 12 0 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
    PERFORM set_area USING ls_excel lv_line lv_line 2 2 '' 0 12 0 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
    PERFORM set_area USING ls_excel lv_line lv_line 3 3 '' 0 12 0 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
    PERFORM set_area USING ls_excel lv_line lv_line 4 4 '' 0 12 0 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
    PERFORM set_area USING ls_excel lv_line lv_line 5 5 '' 0 12 0 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
    PERFORM set_area USING ls_excel lv_line lv_line 6 6 '' 0 12 0 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
    PERFORM set_area USING ls_excel lv_line lv_line 7 7 '' 0 12 0 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
    PERFORM set_area USING ls_excel lv_line lv_line 8 8 '' 0 12 0 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
    WRITE <ls_list>-begd6 TO lv_val DD/MM/YYYY.
*    PERFORM set_area USING ls_excel lv_line lv_line 9 9 lv_val 0 12 0 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
    PERFORM set_area USING ls_excel lv_line lv_line 9 9 '' 0 12 0 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
    PERFORM set_area USING ls_excel lv_line lv_line 10 10 '' 0 12 0 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
    PERFORM set_area USING ls_excel lv_line lv_line 11 11 <ls_list>-anzh6 0 12 0 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
    PERFORM set_area USING ls_excel lv_line lv_line 12 12 '' 0 12 0 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
    WRITE <ls_list>-begd1 TO lv_val DD/MM/YYYY.
    PERFORM set_area USING ls_excel lv_line lv_line 13 13 lv_val 0 12 0 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
    WRITE <ls_list>-endd1 TO lv_val DD/MM/YYYY.
    PERFORM set_area USING ls_excel lv_line lv_line 14 14 lv_val 0 12 0 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
    WRITE <ls_list>-abrtg TO lv_val DECIMALS 0.
    IF lv_val <> <ls_list>-abrtg.
      WRITE <ls_list>-abrtg TO lv_val DECIMALS 1.
    ENDIF.
    PERFORM set_area USING ls_excel lv_line lv_line 15 15 lv_val 0 12 0 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
    IF lv_count = lv_lines.
      WRITE <ls_list>-quonm TO lv_val DECIMALS 0.
      IF lv_val <> <ls_list>-quonm.
        WRITE <ls_list>-abrtg TO lv_val DECIMALS 1.
      ENDIF.
      PERFORM set_area USING ls_excel lv_line lv_line 16 16 lv_val 0 12 0 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
    ELSE.
      PERFORM set_area USING ls_excel lv_line lv_line 16 16 '' 0 12 0 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
    ENDIF.
    PERFORM set_area USING ls_excel lv_line lv_line 17 17 '' 0 12 0 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  ENDLOOP.

  PERFORM set_area USING ls_excel 1 1 1 17 'YILLIK ÜCRETLİ İZİN KAYDI' 0 16 1 0 c_center_align 0 0 0 0.

  PERFORM get_fullpath USING p_path ip_pernr '.XLSX' CHANGING lv_fullpath.

  CALL METHOD OF ls_wbook 'SAVEAS'
    EXPORTING
    #1 = lv_fullpath.

  CALL METHOD OF ls_wbook 'close'.
  CALL METHOD OF ls_excel 'QUIT'.
  FREE OBJECT: ls_excel, ls_wbook, ls_activesheet.

ENDFORM.                    "save_xls

*&---------------------------------------------------------------------*
*&      Form  save_xln
*&---------------------------------------------------------------------*
FORM save_xln USING ip_pernr.

  DATA: lo_excel              TYPE REF TO zcl_excel,
        lo_worksheet          TYPE REF TO zcl_excel_worksheet,
        lo_column             TYPE REF TO zcl_excel_column,
        lo_row                TYPE REF TO zcl_excel_row,
        lo_border             TYPE REF TO zcl_excel_style_border,
        lo_style_title        TYPE REF TO zcl_excel_style,
        lo_style_info         TYPE REF TO zcl_excel_style,
        lo_style_caption      TYPE REF TO zcl_excel_style,
        lo_style_row          TYPE REF TO zcl_excel_style,
        lv_style_title_guid   TYPE zexcel_cell_style,
        lv_style_info_guid    TYPE zexcel_cell_style,
        lv_style_caption_guid TYPE zexcel_cell_style,
        lv_style_row_guid     TYPE zexcel_cell_style,
        lv_val                TYPE string,
        lv_valc               TYPE c LENGTH 30,
        lv_line               TYPE i,
        lv_count              TYPE i,
        lv_lines              TYPE i,
        lt_list               LIKE gt_list.

  FIELD-SYMBOLS <ls_list> LIKE LINE OF gt_list.

  CREATE OBJECT lo_excel.
  lo_worksheet = lo_excel->get_active_worksheet( ).

  CREATE OBJECT lo_border.
  lo_border->border_color-rgb = zcl_excel_style_color=>c_black.
  lo_border->border_style = zcl_excel_style_border=>c_border_thin.

  lo_column = lo_worksheet->get_column( ip_column = 'A' ).
  lo_column->set_width( ip_width = '11.43' ).
  lo_column = lo_worksheet->get_column( ip_column = 'I' ).
  lo_column->set_width( ip_width = '11.43' ).
  lo_column = lo_worksheet->get_column( ip_column = 'M' ).
  lo_column->set_width( ip_width = '11.43' ).
  lo_column = lo_worksheet->get_column( ip_column = 'N' ).
  lo_column->set_width( ip_width = '11.43' ).
  lo_row = lo_worksheet->get_row( ip_row = 1 ).
  lo_row->set_row_height( ip_row_height = '20.25' ).

  lo_style_title               = lo_excel->add_new_style( ).
  lo_style_title->font->bold   = abap_true.
  lo_style_title->font->name   = zcl_excel_style_font=>c_name_calibri.
  lo_style_title->font->size   = 16.
  lo_style_title->alignment->horizontal = zcl_excel_style_alignment=>c_horizontal_center.
  lv_style_title_guid          = lo_style_title->get_guid( ).
  lo_worksheet->set_cell(  ip_row = 1 ip_column       = 'A' ip_value = 'YILLIK ÜCRETLİ İZİN KAYDI' ).
  lo_worksheet->set_merge( ip_row = 1 ip_column_start = 'A' ip_column_end = 'Q' ip_row_to = 1 ip_style = lv_style_title_guid ).

  lo_style_info               = lo_excel->add_new_style( ).
  lo_style_info->font->bold   = abap_true.
  lo_style_info->font->name   = zcl_excel_style_font=>c_name_calibri.
  lo_style_info->font->size   = 8.
  lo_style_info->alignment->horizontal = zcl_excel_style_alignment=>c_horizontal_left.
  lo_style_info->alignment->vertical   = zcl_excel_style_alignment=>c_vertical_top.
  lo_style_info->borders->allborders = lo_border.
  lv_style_info_guid          = lo_style_info->get_guid( ).
  CONCATENATE 'SİCİL NO :' ip_pernr INTO lv_val SEPARATED BY space.
  lo_worksheet->set_cell(  ip_row = 3 ip_column       = 'A' ip_value = lv_val ).
  lo_worksheet->set_merge( ip_row = 3 ip_column_start = 'A' ip_column_end = 'C' ip_row_to = 6 ip_style = lv_style_info_guid ).

  READ TABLE gt_list ASSIGNING <ls_list> WITH KEY pernr = ip_pernr.
  CONCATENATE 'ADI :' <ls_list>-vorna INTO lv_val SEPARATED BY space.
  lo_worksheet->set_cell(  ip_row = 3 ip_column       = 'D' ip_value = lv_val ).
  lo_worksheet->set_merge( ip_row = 3 ip_column_start = 'D' ip_column_end = 'H' ip_row_to = 6 ip_style = lv_style_info_guid ).

  CONCATENATE 'SOYADI :' <ls_list>-nachn INTO lv_val SEPARATED BY space.
  lo_worksheet->set_cell(  ip_row = 3 ip_column       = 'I' ip_value = lv_val ).
  lo_worksheet->set_merge( ip_row = 3 ip_column_start = 'I' ip_column_end = 'M' ip_row_to = 6 ip_style = lv_style_info_guid ).

  CONCATENATE <ls_list>-dardt+6(2) '.' <ls_list>-dardt+4(2) '.' <ls_list>-dardt(4) INTO lv_val.
  CONCATENATE 'İŞE GİRİŞ TARİHİ :' lv_val INTO lv_val SEPARATED BY space.
  lo_worksheet->set_cell(  ip_row = 3 ip_column       = 'N' ip_value = lv_val ).
  lo_worksheet->set_merge( ip_row = 3 ip_column_start = 'N' ip_column_end = 'Q' ip_row_to = 6 ip_style = lv_style_info_guid ).

  lo_style_caption               = lo_excel->add_new_style( ).
  lo_style_caption->font->bold   = abap_true.
  lo_style_caption->font->name   = zcl_excel_style_font=>c_name_calibri.
  lo_style_caption->font->size   = 7.
  lo_style_caption->alignment->horizontal = zcl_excel_style_alignment=>c_horizontal_center.
  lo_style_caption->alignment->vertical   = zcl_excel_style_alignment=>c_vertical_center.
  lo_style_caption->alignment->wraptext   = abap_true.
  lo_style_caption->borders->allborders = lo_border.
  lv_style_caption_guid = lo_style_caption->get_guid( ).
  lo_worksheet->set_cell(  ip_row = 7 ip_column       = 'A' ip_value = 'Yılı' ).
  lo_worksheet->set_merge( ip_row = 7 ip_column_start = 'A' ip_column_end = 'A' ip_row_to = 8 ip_style = lv_style_caption_guid ).
  lo_worksheet->set_cell(  ip_row = 7 ip_column       = 'B' ip_value = 'Bir Yıl Önceki İzin Hakkını Kazandığı Tarih' ).
  lo_worksheet->set_merge( ip_row = 7 ip_column_start = 'B' ip_column_end = 'B' ip_row_to = 8 ip_style = lv_style_caption_guid ).
  lo_worksheet->set_cell(  ip_row = 7 ip_column       = 'C' ip_value = 'Bir Yıllık Çalışma Süresi İçinde Çalışılmayan Gün Sayısı ve Nedenleri' ).
  lo_worksheet->set_merge( ip_row = 7 ip_column_start = 'C' ip_column_end = 'H' ip_row_to = 7 ip_style = lv_style_caption_guid ).
  lo_worksheet->set_cell(  ip_row = 8 ip_column = 'C' ip_value = 'Hastalık' ip_style = lv_style_caption_guid ).
  lo_worksheet->set_cell(  ip_row = 8 ip_column = 'D' ip_value = 'Askerlik' ip_style = lv_style_caption_guid ).
  lo_worksheet->set_cell(  ip_row = 8 ip_column = 'E' ip_value = 'Zorunluluk Hali' ip_style = lv_style_caption_guid ).
  lo_worksheet->set_cell(  ip_row = 8 ip_column = 'F' ip_value = 'Devamsızlık' ip_style = lv_style_caption_guid ).
  lo_worksheet->set_cell(  ip_row = 8 ip_column = 'G' ip_value = 'Hizmete Ara Verme' ip_style = lv_style_caption_guid ).
  lo_worksheet->set_cell(  ip_row = 8 ip_column = 'H' ip_value = 'Diğer Nedenler' ip_style = lv_style_caption_guid ).
  lo_worksheet->set_cell(  ip_row = 7 ip_column       = 'I' ip_value = 'İzne Hak Kazandığı Tarih' ).
  lo_worksheet->set_merge( ip_row = 7 ip_column_start = 'I' ip_column_end = 'I' ip_row_to = 8 ip_style = lv_style_caption_guid ).
  lo_worksheet->set_cell(  ip_row = 7 ip_column       = 'J' ip_value = 'İşyerindeki Kıdemi' ).
  lo_worksheet->set_merge( ip_row = 7 ip_column_start = 'J' ip_column_end = 'J' ip_row_to = 8 ip_style = lv_style_caption_guid ).
  lo_worksheet->set_cell(  ip_row = 7 ip_column       = 'K' ip_value = 'İzin Süresi' ).
  lo_worksheet->set_merge( ip_row = 7 ip_column_start = 'K' ip_column_end = 'K' ip_row_to = 8 ip_style = lv_style_caption_guid ).
  lo_worksheet->set_cell(  ip_row = 7 ip_column       = 'L' ip_value = 'Yol İzni' ).
  lo_worksheet->set_merge( ip_row = 7 ip_column_start = 'L' ip_column_end = 'L' ip_row_to = 8 ip_style = lv_style_caption_guid ).
  lo_worksheet->set_cell(  ip_row = 7 ip_column       = 'M' ip_value = 'İzne Başlangıç Tarihi' ).
  lo_worksheet->set_merge( ip_row = 7 ip_column_start = 'M' ip_column_end = 'M' ip_row_to = 8 ip_style = lv_style_caption_guid ).
  lo_worksheet->set_cell(  ip_row = 7 ip_column       = 'N' ip_value = 'İzin Bitiş Tarihi' ).
  lo_worksheet->set_merge( ip_row = 7 ip_column_start = 'N' ip_column_end = 'N' ip_row_to = 8 ip_style = lv_style_caption_guid ).
  lo_worksheet->set_cell(  ip_row = 7 ip_column       = 'O' ip_value = 'Kullanılan İzin Günü' ).
  lo_worksheet->set_merge( ip_row = 7 ip_column_start = 'O' ip_column_end = 'O' ip_row_to = 8 ip_style = lv_style_caption_guid ).
  lo_worksheet->set_cell(  ip_row = 7 ip_column       = 'P' ip_value = 'Kalan İzin Günü' ).
  lo_worksheet->set_merge( ip_row = 7 ip_column_start = 'P' ip_column_end = 'P' ip_row_to = 8 ip_style = lv_style_caption_guid ).
  lo_worksheet->set_cell(  ip_row = 7 ip_column       = 'Q' ip_value = 'İşçinin İmzası' ).
  lo_worksheet->set_merge( ip_row = 7 ip_column_start = 'Q' ip_column_end = 'Q' ip_row_to = 8 ip_style = lv_style_caption_guid ).

  lo_style_row               = lo_excel->add_new_style( ).
  lo_style_row->font->name   = zcl_excel_style_font=>c_name_calibri.
  lo_style_row->font->size   = 12.
  lo_style_row->alignment->horizontal = zcl_excel_style_alignment=>c_horizontal_center.
  lo_style_row->alignment->vertical   = zcl_excel_style_alignment=>c_vertical_center.
  lo_style_row->alignment->wraptext   = abap_true.
  lo_style_row->borders->allborders = lo_border.
  lv_style_row_guid = lo_style_row->get_guid( ).

  lt_list = gt_list.
  DELETE lt_list WHERE pernr <> ip_pernr.
  lv_lines = LINES( lt_list ).

  lv_line = 8.
  LOOP AT lt_list ASSIGNING <ls_list>.
    ADD 1 TO: lv_line, lv_count.
    lo_worksheet->set_cell( ip_row = lv_line ip_column = 'A' ip_value = '' ip_style = lv_style_row_guid ).
    lo_worksheet->set_cell( ip_row = lv_line ip_column = 'B' ip_value = '' ip_style = lv_style_row_guid ).
    lo_worksheet->set_cell( ip_row = lv_line ip_column = 'C' ip_value = '' ip_style = lv_style_row_guid ).
    lo_worksheet->set_cell( ip_row = lv_line ip_column = 'D' ip_value = '' ip_style = lv_style_row_guid ).
    lo_worksheet->set_cell( ip_row = lv_line ip_column = 'E' ip_value = '' ip_style = lv_style_row_guid ).
    lo_worksheet->set_cell( ip_row = lv_line ip_column = 'F' ip_value = '' ip_style = lv_style_row_guid ).
    lo_worksheet->set_cell( ip_row = lv_line ip_column = 'G' ip_value = '' ip_style = lv_style_row_guid ).
    lo_worksheet->set_cell( ip_row = lv_line ip_column = 'H' ip_value = '' ip_style = lv_style_row_guid ).
*    CONCATENATE <ls_list>-begd6+6(2) '.' <ls_list>-begd6+4(2) '.' <ls_list>-begd6(4) INTO lv_val.
    lo_worksheet->set_cell( ip_row = lv_line ip_column = 'I' ip_value = '' ip_style = lv_style_row_guid ).
    lo_worksheet->set_cell( ip_row = lv_line ip_column = 'J' ip_value = '' ip_style = lv_style_row_guid ).
    lo_worksheet->set_cell( ip_row = lv_line ip_column = 'K' ip_value = <ls_list>-anzh6 ip_style = lv_style_row_guid ).
    lo_worksheet->set_cell( ip_row = lv_line ip_column = 'L' ip_value = '' ip_style = lv_style_row_guid ).
    CONCATENATE <ls_list>-begd1+6(2) '.' <ls_list>-begd1+4(2) '.' <ls_list>-begd1(4) INTO lv_val.
    lo_worksheet->set_cell( ip_row = lv_line ip_column = 'M' ip_value = lv_val ip_style = lv_style_row_guid ).
    CONCATENATE <ls_list>-endd1+6(2) '.' <ls_list>-endd1+4(2) '.' <ls_list>-endd1(4) INTO lv_val.
    lo_worksheet->set_cell( ip_row = lv_line ip_column = 'N' ip_value = lv_val ip_style = lv_style_row_guid ).
    WRITE <ls_list>-abrtg TO lv_valc DECIMALS 0.
    IF lv_valc <> <ls_list>-abrtg.
      WRITE <ls_list>-abrtg TO lv_valc DECIMALS 1.
    ENDIF.
    lv_val = lv_valc. CONDENSE lv_val.
    lo_worksheet->set_cell( ip_row = lv_line ip_column = 'O' ip_value = lv_val ip_style = lv_style_row_guid ).
    IF lv_count = lv_lines.
      WRITE <ls_list>-quonm TO lv_valc DECIMALS 0.
      IF lv_valc <> <ls_list>-quonm.
        WRITE <ls_list>-abrtg TO lv_valc DECIMALS 1.
      ENDIF.
      lv_val = lv_valc. CONDENSE lv_val.
      lo_worksheet->set_cell( ip_row = lv_line ip_column = 'P' ip_value = lv_val ip_style = lv_style_row_guid ).
    ELSE.
      lo_worksheet->set_cell( ip_row = lv_line ip_column = 'P' ip_value = '' ip_style = lv_style_row_guid ).
    ENDIF.
    lo_worksheet->set_cell( ip_row = lv_line ip_column = 'Q' ip_value = '' ip_style = lv_style_row_guid ).
  ENDLOOP.

  PERFORM download_xln USING ip_pernr lo_excel.

ENDFORM.                    "save_xln

*&---------------------------------------------------------------------*
*&      Form  download_xln
*&---------------------------------------------------------------------*
FORM download_xln USING ip_pernr io_excel TYPE REF TO zcl_excel.
  DATA: cl_writer   TYPE REF TO zif_excel_writer,
        xdata       TYPE xstring,
        t_rawdata   TYPE solix_tab,
        bytecount   TYPE i,
        lv_fullpath TYPE string.
  CREATE OBJECT cl_writer TYPE zcl_excel_writer_2007.
  xdata = cl_writer->write_file( io_excel ).
  t_rawdata = cl_bcs_convert=>xstring_to_solix( iv_xstring = xdata ).
  bytecount = XSTRLEN( xdata ).
  PERFORM get_fullpath USING p_path ip_pernr '.XLSX' CHANGING lv_fullpath.
  cl_gui_frontend_services=>gui_download( EXPORTING bin_filesize = bytecount
                                                    filename     = lv_fullpath
                                                    filetype     = 'BIN'
                                           CHANGING data_tab     = t_rawdata ).
ENDFORM.                    "download_xln

*&---------------------------------------------------------------------*
*&      Form  get_fullpath
*&---------------------------------------------------------------------*
FORM get_fullpath USING ip_path ip_pernr ip_ext CHANGING cp_fullpath.
  cp_fullpath = ip_path.
  IF cp_fullpath CA '/'.
    REPLACE REGEX '([^/])\s*$' IN cp_fullpath WITH '$1/' .
  ELSE.
    REPLACE REGEX '([^\\])\s*$' IN cp_fullpath WITH '$1\\'.
  ENDIF.

  CONCATENATE cp_fullpath ip_pernr ip_ext INTO cp_fullpath.
ENDFORM.                    "get_fullpath

*&---------------------------------------------------------------------*
*&      Form  set_col_width
*&---------------------------------------------------------------------*
FORM set_col_width USING p_sheet p_col p_width.
  DATA ls_cols TYPE ole2_object.
  CALL METHOD OF p_sheet 'Columns' = ls_cols
    EXPORTING
    #1 = p_col.
  SET PROPERTY OF ls_cols 'ColumnWidth' = p_width.
ENDFORM.                    "set_col_width
*&---------------------------------------------------------------------*
*&      Form  set_row_height
*&---------------------------------------------------------------------*
FORM set_row_height USING p_sheet p_row p_height.
  DATA ls_rows TYPE ole2_object.
  CALL METHOD OF p_sheet 'Rows' = ls_rows
    EXPORTING
    #1 = p_row.
  SET PROPERTY OF ls_rows 'RowHeight' = p_height.
ENDFORM.                    "set_row_height
*&---------------------------------------------------------------------*
*&      Form  set_cell
*&---------------------------------------------------------------------*
FORM set_cell  USING p_appl TYPE ole2_object
                     p_row TYPE sy-tabix
                     p_column TYPE sy-tabix
                     p_val TYPE csequence
                     p_int TYPE i
                     p_bold TYPE i
                     p_width TYPE i.

  DATA: ls_cell TYPE ole2_object,
        ls_color TYPE ole2_object,
        ls_bold TYPE ole2_object,
        lv_val  TYPE string.

  lv_val = p_val.

  CALL METHOD OF p_appl 'Cells' = ls_cell
    EXPORTING
    #1 = p_row
    #2 = p_column.
  SET PROPERTY OF ls_cell 'Value' =  lv_val.
  GET PROPERTY OF ls_cell 'Interior' = ls_color.
  SET PROPERTY OF ls_color 'ColorIndex' = p_int.

  GET PROPERTY OF ls_cell 'Font' = ls_bold.
  SET PROPERTY OF ls_bold 'Bold' = p_bold.
  IF p_width > 0.
    SET PROPERTY OF ls_cell 'ColumnWidth' = p_width.
  ENDIF.

ENDFORM.                    "set_cell

*&---------------------------------------------------------------------*
*&      Form  set_area
*&---------------------------------------------------------------------*
FORM set_area  USING p_appl TYPE ole2_object
                     p_row1 TYPE sy-tabix
                     p_row2 TYPE sy-tabix
                     p_col1 TYPE sy-tabix
                     p_col2 TYPE sy-tabix
                     p_val TYPE csequence
                     p_color TYPE i
                     p_fontsize TYPE i
                     p_bold TYPE i
                     p_width TYPE i
                     p_halign TYPE i
                     p_valign TYPE i
                     p_border_type TYPE i
                     p_border_size TYPE i
                     p_wrap TYPE i.

  DATA: ls_cell1 TYPE ole2_object,
        ls_cell2 TYPE ole2_object,
        ls_cells TYPE ole2_object,
        ls_color TYPE ole2_object,
        ls_bold TYPE ole2_object,
        ls_font TYPE ole2_object,
        lv_val  TYPE string.

  lv_val = p_val.
  CONDENSE lv_val.

  CALL METHOD OF p_appl 'Cells' = ls_cell1
    EXPORTING
    #1 = p_row1
    #2 = p_col1.
  CALL METHOD OF p_appl 'Cells' = ls_cell2
    EXPORTING
    #1 = p_row2
    #2 = p_col2.
  CALL METHOD OF p_appl 'Range' = ls_cells
    EXPORTING
    #1 = ls_cell1
    #2 = ls_cell2.
  CALL METHOD OF ls_cells 'Select' .

  CALL METHOD OF ls_cells 'Merge' .
  SET PROPERTY OF ls_cells 'Value' =  lv_val.
  GET PROPERTY OF ls_cells 'Interior' = ls_color.
  SET PROPERTY OF ls_color 'ColorIndex' = p_color.
  GET PROPERTY OF ls_cells 'Font' = ls_font.
  IF p_fontsize <> 0.
    SET PROPERTY OF ls_font 'Size' = p_fontsize.
  ENDIF.
  GET PROPERTY OF ls_cells 'Font' = ls_bold.
  SET PROPERTY OF ls_bold 'Bold' = p_bold.
  IF p_width > 0.
    SET PROPERTY OF ls_cells 'ColumnWidth' = p_width.
  ENDIF.
  IF p_halign <> 0.
    SET PROPERTY OF ls_cells 'HorizontalAlignment' = p_halign.
  ENDIF.
  IF p_valign <> 0.
    SET PROPERTY OF ls_cells 'VerticalAlignment'   = p_valign.
  ENDIF.

  IF p_border_type <> 0 AND p_border_size <> 0.
    CALL METHOD OF ls_cells 'BorderAround'
      EXPORTING
      #1 = p_border_type
      #2 = p_border_size.
  ENDIF.

  SET PROPERTY OF ls_cells 'WrapText' = p_wrap.

ENDFORM.                    "set_area