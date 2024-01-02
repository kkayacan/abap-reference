REPORT y_find_range_struc.

DATA: gv_data_element        TYPE rollname,         " Data Element Name
      gv_exclude_slash       TYPE abap_bool,           " Exclude structures starting with '/'
      gt_matching_structures TYPE TABLE OF dd02l, " Matching Structures
      gs_table               TYPE dd02l,
      gtr_tabname            TYPE RANGE OF dd03l-tabname,
      gtr_leng               TYPE RANGE OF dd03l-leng.

PARAMETERS: rdatael  RADIOBUTTON GROUP r01 USER-COMMAND r01 DEFAULT 'X',
            datael   TYPE rollname MODIF ID dat,
            rtype    RADIOBUTTON GROUP r01,
            datatype TYPE dd03l-datatype MODIF ID typ,
            leng     TYPE dd03l-leng MODIF ID typ,
            excslash AS CHECKBOX DEFAULT abap_true.

INITIALIZATION.
  %_rdatael_%_app_%-text = 'Data Element'.
  %_datael_%_app_%-text = 'Data Element'.
  %_rtype_%_app_%-text = 'Type'.
  %_datatype_%_app_%-text = 'Type'.
  %_leng_%_app_%-text = 'Length'.
  %_excslash_%_app_%-text = 'Exclude struc. starting with /'.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CASE abap_false.
      WHEN rdatael.
        IF screen-group1 = 'DAT'.
          screen-input = 0.
        ENDIF.
      WHEN rtype.
        IF screen-group1 = 'TYP'.
          screen-input = 0.
        ENDIF.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.

START-OF-SELECTION.

  gv_data_element = datael.
  gv_exclude_slash = excslash.

  IF excslash = abap_true.
    gtr_tabname = VALUE #( ( sign = 'I' option = 'NP' low = '/*' ) ).
  ENDIF.

  CASE abap_true.
    WHEN rdatael.
      SELECT * FROM dd02l
        WHERE tabclass = 'INTTAB'
          AND tabname IN ( SELECT tabname FROM dd03l WHERE rollname = @gv_data_element
                                                     AND   tabname IN @gtr_tabname )
        INTO TABLE @gt_matching_structures.
    WHEN rtype.
      IF leng IS NOT INITIAL.
        gtr_leng = VALUE #( ( sign = 'I' option = 'EQ' low = leng ) ).
      ENDIF.
      SELECT * FROM dd02l
        WHERE tabclass = 'INTTAB'
          AND tabname IN ( SELECT tabname FROM dd03l WHERE datatype = @datatype
                                                     AND   leng    IN @gtr_leng
                                                     AND   tabname IN @gtr_tabname )
        INTO TABLE @gt_matching_structures.
  ENDCASE.

  FORMAT COLOR COL_HEADING.
  WRITE: 1(30) 'STRUCTURE', 35 'TABLE TYPE'.
  FORMAT COLOR COL_NORMAL.

  DATA: lt_fields  TYPE TABLE OF dd03l,
        lv_tabtype TYPE c LENGTH 30.

  LOOP AT gt_matching_structures INTO gs_table.

    CLEAR lt_fields.
    SELECT * FROM dd03l
      WHERE tabname = @gs_table-tabname
      INTO TABLE @lt_fields.
    CHECK lines( lt_fields ) = 4.

    IF     lt_fields[ 1 ]-fieldname CS 'SIGN'
       AND lt_fields[ 1 ]-leng = 1
       AND lt_fields[ 2 ]-fieldname CS 'OPTION'
       AND lt_fields[ 2 ]-leng = 2
       AND lt_fields[ 3 ]-fieldname = 'LOW'
       AND lt_fields[ 4 ]-fieldname = 'HIGH'.

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
    WRITE:/ 'No matching structures found.'.
  ENDIF.