REPORT  zole2_excel_sample.

INFOTYPES: 0001, 0002, 0041, 2001.

INCLUDE ole2incl.

TYPES: BEGIN OF gst_list,
        pernr TYPE p0001-pernr,
        vorna TYPE p0002-vorna,
        nachn TYPE p0002-nachn,
        year  TYPE string,
        anzh6 TYPE string,
        begd6 TYPE p2006-begda,
        begd1 TYPE p2001-begda,
        endd1 TYPE p2001-endda,
        abrtg TYPE p2001-abrtg,
        dardt TYPE p0041-dat01,
        quonm TYPE p2006-anzhl,
       END OF gst_list.

TABLES pernr.

DATA gt_list TYPE TABLE OF gst_list.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-001.
PARAMETERS: p_path TYPE string LOWER CASE OBLIGATORY.
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
FORM value_request_path CHANGING selected_folder TYPE string.
  DATA: new_path      TYPE string,
        repid         TYPE syrepid,
        dynnr         TYPE sydynnr,
        lt_dynpfields TYPE TABLE OF dynpread,
        ls_dynpfields LIKE LINE OF lt_dynpfields,
        title         TYPE string.

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
FORM at_selection_screen.
  DATA rc TYPE abap_bool.
  IF sy-ucomm = 'ONLI'.
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
      MESSAGE 'Invalid path' TYPE 'E'.
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

  LOOP AT p2001 WHERE subty = '0300'
                OR    subty = '0305'.
    APPEND INITIAL LINE TO gt_list ASSIGNING <ls_list>.
    <ls_list>-pernr = p2001-pernr.
    <ls_list>-vorna = p0002-vorna.
    <ls_list>-nachn = p0002-nachn.
    CLEAR lv_year.
    LOOP AT lt_2006 ASSIGNING <ls_2006> WHERE docnr = p2001-docnr.
      WRITE <ls_2006>-anzhl TO lv_anzhl_c DECIMALS 0.
      CONDENSE lv_anzhl_c.
      IF <ls_list>-year IS INITIAL OR <ls_2006>-begda(4) = lv_year.
        <ls_list>-year = <ls_2006>-begda(4).
        <ls_list>-anzh6 = lv_anzhl_c.
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
    RETURN.
  ENDIF.

  lt_emp = gt_list.
  SORT lt_emp BY pernr.
  DELETE ADJACENT DUPLICATES FROM lt_emp COMPARING pernr.
  lv_total = lines( lt_emp ).
  LOOP AT lt_emp ASSIGNING <ls_emp>.
    ADD 1 TO lv_count.
    PERFORM save_xls USING <ls_emp>-pernr lv_count lv_total.
  ENDLOOP.
ENDFORM.                    "end_of_selection
*&---------------------------------------------------------------------*
*&      Form  save_xls
*&---------------------------------------------------------------------*
FORM save_xls USING ip_pernr ip_count ip_total.

  CONSTANTS: c_center_align TYPE i VALUE -4108,
             c_top_align    TYPE i VALUE -4160,
             c_thin_border  TYPE i VALUE 2,
             c_wrap_text    TYPE i VALUE 1.

  DATA: ls_excel TYPE ole2_object,
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
  lv_lines = lines( lt_list ).

  PERFORM open_file CHANGING ls_excel ls_wbook ls_activesheet.

  PERFORM set_col_width USING ls_activesheet 1 '11.43'.
  PERFORM set_col_width USING ls_activesheet 9 '11.43'.
  PERFORM set_col_width USING ls_activesheet 13 '11.43'.
  PERFORM set_col_width USING ls_activesheet 14 '11.43'.
  PERFORM set_row_height USING ls_activesheet 1 '20.25'.
  CONCATENATE 'NUMBER :' ip_pernr INTO lv_val SEPARATED BY space.
  PERFORM set_area USING ls_excel 3 6 1 3 lv_val 0 8 1 0 0 c_top_align 1 c_thin_border 0.
  READ TABLE gt_list ASSIGNING <ls_list> WITH KEY pernr = ip_pernr.
  CONCATENATE 'NAME :' <ls_list>-vorna INTO lv_val SEPARATED BY space.
  PERFORM set_area USING ls_excel 3 6 4 8 lv_val 0 8 1 0 0 c_top_align 1 c_thin_border 0.
  CONCATENATE 'SURNAME :' <ls_list>-nachn INTO lv_val SEPARATED BY space.
  PERFORM set_area USING ls_excel 3 6 9 13 lv_val 0 8 1 0 0 c_top_align 1 c_thin_border 0.
  WRITE <ls_list>-dardt TO lv_val DD/MM/YYYY.
  CONCATENATE 'START DATE :' lv_val INTO lv_val SEPARATED BY space.
  PERFORM set_area USING ls_excel 3 6 14 17 lv_val 0 8 1 0 0 c_top_align 1 c_thin_border 0.

  PERFORM set_area USING ls_excel 7 8 1 1 'Year' 0 7 1 0 c_center_align c_center_align 1 c_thin_border 0.
  PERFORM set_area USING ls_excel 7 8 2 2 'Previous earned date' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  PERFORM set_area USING ls_excel 7 7 3 8 'Absence days' 0 7 1 0 c_center_align c_center_align 1 c_thin_border 0.
  PERFORM set_area USING ls_excel 8 8 3 3 'Sickness' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  PERFORM set_area USING ls_excel 8 8 4 4 'Military' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  PERFORM set_area USING ls_excel 8 8 5 5 'Obligations' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  PERFORM set_area USING ls_excel 8 8 6 6 'Absence' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  PERFORM set_area USING ls_excel 8 8 7 7 'Suspension' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  PERFORM set_area USING ls_excel 8 8 8 8 'Other' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  PERFORM set_area USING ls_excel 7 8 9 9 'Earned date' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  PERFORM set_area USING ls_excel 7 8 10 10 'Seniority' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  PERFORM set_area USING ls_excel 7 8 11 11 'Duration' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  PERFORM set_area USING ls_excel 7 8 12 12 'Transport' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  PERFORM set_area USING ls_excel 7 8 13 13 'Start date' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  PERFORM set_area USING ls_excel 7 8 14 14 'End date' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  PERFORM set_area USING ls_excel 7 8 15 15 'Used' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  PERFORM set_area USING ls_excel 7 8 16 16 'Remain' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
  PERFORM set_area USING ls_excel 7 8 17 17 'Signature' 0 7 1 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.

  lv_line = 8.
  LOOP AT lt_list ASSIGNING <ls_list>.
    ADD 1 TO: lv_line, lv_count.
    PERFORM set_area USING ls_excel lv_line lv_line 1 1 <ls_list>-year 0 12 0 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
    PERFORM set_area USING ls_excel lv_line lv_line 2 2 '' 0 12 0 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
    PERFORM set_area USING ls_excel lv_line lv_line 3 3 '' 0 12 0 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
    PERFORM set_area USING ls_excel lv_line lv_line 4 4 '' 0 12 0 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
    PERFORM set_area USING ls_excel lv_line lv_line 5 5 '' 0 12 0 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
    PERFORM set_area USING ls_excel lv_line lv_line 6 6 '' 0 12 0 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
    PERFORM set_area USING ls_excel lv_line lv_line 7 7 '' 0 12 0 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
    PERFORM set_area USING ls_excel lv_line lv_line 8 8 '' 0 12 0 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
    WRITE <ls_list>-begd6 TO lv_val DD/MM/YYYY.
    PERFORM set_area USING ls_excel lv_line lv_line 9 9 lv_val 0 12 0 0 c_center_align c_center_align 1 c_thin_border c_wrap_text.
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

  PERFORM set_area USING ls_excel 1 1 1 17 'YEARLY ALLOWANCE RECORD' 0 16 1 0 c_center_align 0 0 0 0.

  PERFORM get_fullpath USING p_path ip_pernr '.XLSX' CHANGING lv_fullpath.

  PERFORM close_file USING lv_fullpath CHANGING ls_excel ls_wbook ls_activesheet.

ENDFORM.                    "save_xls

FORM open_file CHANGING cs_excel cs_wbook cs_sheet.
  CREATE OBJECT cs_excel 'EXCEL.APPLICATION'.
  SET PROPERTY OF cs_excel 'VISIBLE' = 0.
  CALL METHOD OF cs_excel 'WORKBOOKS' = cs_wbook.
  CALL METHOD OF cs_wbook 'Add' = cs_wbook.
  GET PROPERTY OF cs_excel 'ActiveSheet' = cs_sheet.
ENDFORM.

FORM close_file USING ip_path CHANGING cs_excel cs_wbook cs_sheet.
  CALL METHOD OF cs_wbook 'SAVEAS'
    EXPORTING
      #1 = ip_path.
  CALL METHOD OF cs_wbook 'close'.
  CALL METHOD OF cs_excel 'QUIT'.
  FREE OBJECT: cs_excel, cs_wbook, cs_sheet.
ENDFORM.
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
  CALL METHOD OF
      p_sheet
      'Columns' = ls_cols
    EXPORTING
      #1        = p_col.
  SET PROPERTY OF ls_cols 'ColumnWidth' = p_width.
ENDFORM.                    "set_col_width
*&---------------------------------------------------------------------*
*&      Form  set_row_height
*&---------------------------------------------------------------------*
FORM set_row_height USING p_sheet p_row p_height.
  DATA ls_rows TYPE ole2_object.
  CALL METHOD OF
      p_sheet
      'Rows'  = ls_rows
    EXPORTING
      #1      = p_row.
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

  CALL METHOD OF
      p_appl
      'Cells' = ls_cell
    EXPORTING
      #1      = p_row
      #2      = p_column.
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

  CALL METHOD OF
      p_appl
      'Cells' = ls_cell1
    EXPORTING
      #1      = p_row1
      #2      = p_col1.
  CALL METHOD OF
      p_appl
      'Cells' = ls_cell2
    EXPORTING
      #1      = p_row2
      #2      = p_col2.
  CALL METHOD OF
      p_appl
      'Range' = ls_cells
    EXPORTING
      #1      = ls_cell1
      #2      = ls_cell2.
  CALL METHOD OF
      ls_cells
      'Select'.

  CALL METHOD OF ls_cells 'Merge'.
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
        #1             = p_border_type
        #2             = p_border_size.
  ENDIF.

  SET PROPERTY OF ls_cells 'WrapText' = p_wrap.

ENDFORM.                    "set_area
