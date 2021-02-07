FORM add_dynamic_data.

  TYPES: BEGIN OF lst_ausp,
           matnr TYPE mara-matnr,
           atinn TYPE ausp-atinn,
           atwrt TYPE ausp-atwrt,
         END OF lst_ausp.

  DATA: lv_objek       TYPE c LENGTH 18,
        lv_max         TYPE i,
        lt_ausp        TYPE TABLE OF lst_ausp,
        lr_itab        TYPE REF TO data,
        lv_count       TYPE n LENGTH 2,
        lt_ausp_sorted TYPE SORTED TABLE OF lst_ausp WITH NON-UNIQUE KEY matnr.

  LOOP AT gt_itab.
    SELECT SINGLE cuobj FROM  inob
           WHERE  klart  = '026'
           AND    objek  = @gt_itab-matnr
       INTO @DATA(lv_cuobj).
    IF sy-subrc = 0.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_cuobj
        IMPORTING
          output = lv_objek.
      SELECT @gt_itab-matnr, atinn, atwrt FROM  ausp
             WHERE  objek  = @lv_objek
             AND    klart  = '026'
        APPENDING TABLE @lt_ausp.
      IF sy-subrc = 0.
        IF sy-dbcnt > lv_max.
          lv_max = sy-dbcnt.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  DATA(lt_comp) = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( gt_itab ) )->get_components( ).

  DATA(lt_fcat) = gt_fieldcat[].
  SORT lt_fcat BY col_pos DESCENDING.
  READ TABLE lt_fcat INTO DATA(ls_fcat) INDEX 1.
  FREE lt_fcat.

  DO lv_max TIMES.
    lv_count = sy-index.
    APPEND INITIAL LINE TO lt_comp ASSIGNING FIELD-SYMBOL(<ls_comp>).
    <ls_comp>-name = 'ATINN' && lv_count.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'ATINN' ).
    APPEND INITIAL LINE TO gt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
    <ls_fcat>-fieldname = <ls_comp>-name.
    <ls_fcat>-tabname   = ls_fcat-tabname.
    ADD 1 TO ls_fcat-col_pos.
    <ls_fcat>-col_pos = ls_fcat-col_pos.
    <ls_fcat>-no_out = abap_true.
    <ls_fcat>-seltext_m = <ls_fcat>-seltext_l = <ls_fcat>-reptext_ddic = |{ TEXT-c01 } { lv_count }|.
    <ls_fcat>-seltext_s = |{ TEXT-c02 } { lv_count }|.
    <ls_fcat>-rollname = 'ATINN'.
    <ls_fcat>-ref_tabname = 'AUSP'.
    <ls_fcat>-ref_fieldname = 'ATINN'.
    <ls_fcat>-no_out = abap_true.
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'ATBEZ' && lv_count.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'ATBEZ' ).
    APPEND INITIAL LINE TO gt_fieldcat ASSIGNING <ls_fcat>.
    <ls_fcat>-fieldname = <ls_comp>-name.
    <ls_fcat>-tabname   = ls_fcat-tabname.
    ADD 1 TO ls_fcat-col_pos.
    <ls_fcat>-col_pos = ls_fcat-col_pos.
    <ls_fcat>-seltext_m = <ls_fcat>-seltext_l = <ls_fcat>-reptext_ddic = |{ TEXT-c03 } { lv_count }|.
    <ls_fcat>-seltext_s = |{ TEXT-c04 } { lv_count }|.
    <ls_fcat>-rollname = 'ATBEZ'.
    <ls_fcat>-ref_tabname = 'CABNT'.
    <ls_fcat>-ref_fieldname = 'ATBEZ'.
    APPEND INITIAL LINE TO lt_comp ASSIGNING <ls_comp>.
    <ls_comp>-name = 'ATWRT' && lv_count.
    <ls_comp>-type ?= cl_abap_elemdescr=>describe_by_name( 'ATWRT' ).
    APPEND INITIAL LINE TO gt_fieldcat ASSIGNING <ls_fcat>.
    <ls_fcat>-tabname   = ls_fcat-tabname.
    <ls_fcat>-fieldname = <ls_comp>-name.
    ADD 1 TO ls_fcat-col_pos.
    <ls_fcat>-col_pos = ls_fcat-col_pos.
    <ls_fcat>-seltext_m = <ls_fcat>-seltext_l = <ls_fcat>-reptext_ddic = |{ TEXT-c05 } { lv_count }|.
    <ls_fcat>-seltext_s = |{ TEXT-c06 } { lv_count }|.
    <ls_fcat>-rollname = 'ATWRT'.
    <ls_fcat>-ref_tabname = 'AUSP'.
    <ls_fcat>-ref_fieldname = 'ATWRT'.
  ENDDO.

  DATA(lo_tabledescr) = cl_abap_tabledescr=>create( cl_abap_structdescr=>create( lt_comp ) ).
  CREATE DATA lr_itab TYPE HANDLE lo_tabledescr.
  ASSIGN lr_itab->* TO <gt_itab>.

  SORT lt_ausp BY matnr atinn.
  lt_ausp_sorted = lt_ausp.

  SORT lt_ausp BY atinn.
  DELETE ADJACENT DUPLICATES FROM lt_ausp COMPARING atinn.

  IF lt_ausp IS NOT INITIAL.
    SELECT atinn, atbez FROM  cabnt
      FOR ALL ENTRIES IN @lt_ausp
           WHERE  atinn  = @lt_ausp-atinn
           AND    spras  = @p_spras
           AND    adzhl  = @space
      INTO TABLE @DATA(lt_cabnt).
    SORT lt_cabnt BY atinn.
  ENDIF.
  FREE lt_ausp.

  LOOP AT gt_itab.
    APPEND INITIAL LINE TO <gt_itab> ASSIGNING FIELD-SYMBOL(<ls_itab>).
    MOVE-CORRESPONDING gt_itab TO <ls_itab>.
    lv_count = '00'.
    LOOP AT lt_ausp_sorted ASSIGNING FIELD-SYMBOL(<ls_ausp>) WHERE matnr = gt_itab-matnr.
      ADD 1 TO lv_count.
      ASSIGN COMPONENT |ATINN{ lv_count }| OF STRUCTURE <ls_itab> TO FIELD-SYMBOL(<lv_field>).
      <lv_field> = <ls_ausp>-atinn.
      ASSIGN COMPONENT |ATWRT{ lv_count }| OF STRUCTURE <ls_itab> TO <lv_field>.
      <lv_field> = <ls_ausp>-atwrt.
      READ TABLE lt_cabnt ASSIGNING FIELD-SYMBOL(<ls_cabnt>) WITH KEY atinn = <ls_ausp>-atinn BINARY SEARCH.
      IF sy-subrc = 0.
        ASSIGN COMPONENT |ATBEZ{ lv_count }| OF STRUCTURE <ls_itab> TO <lv_field>.
        <lv_field> = <ls_cabnt>-atbez.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.