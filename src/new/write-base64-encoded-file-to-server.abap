TYPES lvt_content(1022) TYPE x.
DATA: lt_content     TYPE TABLE OF lvt_content,
      lv_hex_content TYPE xstring,
      lv_path        TYPE rlgrap-filename,
      lt_parts       TYPE TABLE OF string.

  CLEAR: lv_hex_content, lt_content.

  CALL FUNCTION 'SCMS_BASE64_DECODE_STR'
    EXPORTING
      input  = ls_file-sdata
    IMPORTING
      output = lv_hex_content
    EXCEPTIONS
      failed = 1
      OTHERS = 2.

  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer     = lv_hex_content
    TABLES
      binary_tab = lt_content.

  SPLIT ls_file-sfilename AT '/' INTO TABLE lt_parts.
  LOOP AT lt_parts INTO DATA(lv_part).
  ENDLOOP.

  lv_path = '/usr/sap/trans/BW_AL11/' && lv_part.

  OPEN DATASET lv_path FOR OUTPUT IN BINARY MODE.
  IF sy-subrc = 0.
    LOOP AT lt_content ASSIGNING FIELD-SYMBOL(<lv_cont>).
      TRANSFER <lv_cont> TO lv_path.
    ENDLOOP.
    CLOSE DATASET lv_path.
    WRITE:/ icon_led_green AS ICON, lv_path.
  ELSE.
    WRITE:/ icon_led_red AS ICON, lv_path.
  ENDIF.