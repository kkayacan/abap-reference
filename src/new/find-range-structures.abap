REPORT z_find_struct.

DATA: gv_data_element        TYPE data_element,         " Data Element Name
      gv_exclude_slash       TYPE abap_bool,           " Exclude structures starting with '/'
      gt_matching_structures TYPE TABLE OF dd02l, " Matching Structures
      gs_table               TYPE dd02l,
      gtr_tabname            TYPE RANGE OF dd03l-tabname.

PARAMETERS: datael   TYPE data_element OBLIGATORY,
            excslash AS CHECKBOX DEFAULT abap_true.

INITIALIZATION.
  %_datael_%_app_%-text = 'Data Element'.
  %_excslash_%_app_%-text = 'Exclude struc. starting with /'.

START-OF-SELECTION.

  gv_data_element = datael.
  gv_exclude_slash = excslash.

  IF excslash = abap_true.
    gtr_tabname = VALUE #( ( sign = 'I' option = 'NP' low = '/*' ) ).
  ENDIF.

  SELECT * FROM dd02l
    WHERE tabclass = 'INTTAB'
      AND tabname IN ( SELECT tabname FROM dd03l WHERE rollname = @gv_data_element
                                                 AND   tabname IN @gtr_tabname )
    INTO TABLE @gt_matching_structures.

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

    IF     lt_fields[ 1 ]-datatype = 'CHAR'
       AND lt_fields[ 1 ]-leng = 1
       AND lt_fields[ 2 ]-datatype = 'CHAR'
       AND lt_fields[ 2 ]-leng = 2
       AND lt_fields[ 3 ]-rollname = gv_data_element
       AND lt_fields[ 4 ]-rollname = gv_data_element.

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