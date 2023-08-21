REPORT z_find_struct.

DATA: gv_data_element        TYPE data_element,         " Data Element Name
      gv_exclude_slash       TYPE abap_bool,           " Exclude structures starting with '/'
      gt_matching_structures TYPE TABLE OF dd02l, " Matching Structures
      gs_table               TYPE dd02l.

PARAMETERS: datael   TYPE data_element OBLIGATORY,
            excslash AS CHECKBOX DEFAULT abap_true.

START-OF-SELECTION.

  gv_data_element = datael.
  gv_exclude_slash = excslash.

  SELECT * FROM dd02l
    WHERE tabclass = 'INTTAB'
      AND tabname IN ( SELECT tabname FROM dd03l WHERE rollname = @gv_data_element )
    INTO TABLE @gt_matching_structures.

  FORMAT COLOR COL_HEADING.
  WRITE: 1(30) 'STRUCTURE', 35 'TABLE TYPE'.
  FORMAT COLOR COL_NORMAL.

  LOOP AT gt_matching_structures INTO gs_table.

    IF excslash = abap_true AND gs_table-tabname(1) = '/'.
      CONTINUE.
    ENDIF.

    DATA: lt_fields             TYPE TABLE OF dd03l,
          lv_data_element_count TYPE i,
          lv_tabtype            TYPE c LENGTH 30.

    CLEAR lt_fields.
    SELECT * FROM dd03l
      WHERE tabname = @gs_table-tabname
      INTO TABLE @lt_fields.
    CHECK lines( lt_fields ) = 4.

    "lv_data_element_count = lines( lt_fields WHERE rollname = gv_data_element ).
    lv_data_element_count = 0.
    LOOP AT lt_fields INTO DATA(ls_field) WHERE rollname = gv_data_element.
      ADD 1 TO lv_data_element_count.
    ENDLOOP.

    IF lv_data_element_count = 2
       AND lt_fields[ 1 ]-inttype = 'C'
       AND lt_fields[ 1 ]-intlen = 1
       AND lt_fields[ 2 ]-inttype = 'C'
       AND lt_fields[ 2 ]-intlen = 2.

      SELECT SINGLE typename
        FROM dd40l
        WHERE rowtype = @gs_table-tabname
        INTO @lv_tabtype.

      WRITE: / gs_table-tabname UNDER 'STRUCTURE',
             lv_tabtype UNDER 'TABLE TYPE'.

      CLEAR lv_tabtype.

    ENDIF.
  ENDLOOP.

  IF lines( gt_matching_structures ) = 0.
    WRITE: 'No matching structures found.'.
  ENDIF.