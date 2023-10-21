DATA: gv_exclude_slash       TYPE abap_bool,           " Exclude structures starting with '/'
      gt_matching_structures TYPE TABLE OF dd02l, " Matching Structures
      gs_table               TYPE dd02l,
      gtr_tabname            TYPE RANGE OF dd03l-tabname.

PARAMETERS: excslash AS CHECKBOX DEFAULT abap_true.

INITIALIZATION.
  %_excslash_%_app_%-text = 'Exclude struc. starting with /'.

START-OF-SELECTION.

  gv_exclude_slash = excslash.

  IF excslash = abap_true.
    DATA ls_tabname LIKE LINE OF gtr_tabname.
    ls_tabname-sign = 'I'.
    ls_tabname-option = 'NP'.
    ls_tabname-low = '/*'.
    APPEND ls_tabname TO gtr_tabname.
  ENDIF.

  SELECT * FROM dd02l
    INTO TABLE gt_matching_structures
    WHERE tabclass = 'INTTAB'
      AND tabname IN ( SELECT tabname FROM dd03l WHERE datatype = 'DATS' AND tabname IN gtr_tabname )
      AND tabname IN ( SELECT tabname FROM dd03l WHERE datatype = 'TIMS' AND tabname IN gtr_tabname ).

  FORMAT COLOR COL_HEADING.
  WRITE: 1(30) 'STRUCTURE', 35 'TABLE TYPE'.
  FORMAT COLOR COL_NORMAL.

  DATA: lt_fields  TYPE TABLE OF dd03l,
        lv_tabtype TYPE c LENGTH 30.

  LOOP AT gt_matching_structures INTO gs_table.

    CLEAR lt_fields.
    SELECT * FROM dd03l
      INTO TABLE lt_fields
      WHERE tabname = gs_table-tabname.
    CHECK lines( lt_fields ) = 2.

    SELECT SINGLE typename
      FROM dd40l
      INTO lv_tabtype
      WHERE rowtype = gs_table-tabname.

    WRITE: / gs_table-tabname UNDER 'STRUCTURE',
           lv_tabtype UNDER 'TABLE TYPE'.

    CLEAR lv_tabtype.

  ENDLOOP.

  IF lines( gt_matching_structures ) = 0.
    WRITE: 'No matching structures found.'.
  ENDIF.