REPORT yread_textpool.

TYPES: BEGIN OF gst_pool,
         object   TYPE tadir-object,
         obj_name TYPE tadir-obj_name.
        INCLUDE TYPE textpool.
TYPES: END OF gst_pool.

DATA: gt_pool TYPE TABLE OF gst_pool,
      lt_pool TYPE TABLE OF textpool,
      gv_prog TYPE program.

FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.

PARAMETERS: p_devc TYPE tadir-devclass OBLIGATORY,
            p_lang TYPE sy-langu OBLIGATORY DEFAULT sy-langu,
            p_prog RADIOBUTTON GROUP r01 DEFAULT 'X',
            p_dtel RADIOBUTTON GROUP r01.

START-OF-SELECTION.

  CASE abap_true.
    WHEN p_prog.
      SELECT object, obj_name, CAST( obj_name AS CHAR( 30 ) ) AS rollname
        FROM tadir
        WHERE pgmid = 'R3TR'
        AND   object IN ('CLAS','FUGR','PROG')
        AND   devclass = @p_devc
        INTO TABLE @DATA(gt_tadir).

      LOOP AT gt_tadir ASSIGNING FIELD-SYMBOL(<ls_tadir>).
        CASE <ls_tadir>-object.
          WHEN 'CLAS'.
            gv_prog = cl_oo_classname_service=>get_classpool_name( CONV seoclsname( <ls_tadir>-obj_name ) ).
          WHEN 'FUGR'.
            gv_prog = 'SAPL' && <ls_tadir>-obj_name.
          WHEN 'PROG'.
            gv_prog = <ls_tadir>-obj_name.
        ENDCASE.
        CLEAR lt_pool.
        READ TEXTPOOL gv_prog INTO lt_pool LANGUAGE p_lang.
        LOOP AT lt_pool ASSIGNING FIELD-SYMBOL(<ls_pool>).
          CHECK NOT <ls_pool>-entry = 'D       .'.
          APPEND INITIAL LINE TO gt_pool ASSIGNING FIELD-SYMBOL(<gs_pool>).
          MOVE-CORRESPONDING <ls_tadir> TO <gs_pool>.
          MOVE-CORRESPONDING <ls_pool> TO <gs_pool>.
        ENDLOOP.
      ENDLOOP.
      ASSIGN gt_pool TO <tab>.
    WHEN p_dtel.
      SELECT object, obj_name, CAST( obj_name AS CHAR( 30 ) ) AS rollname
        FROM tadir
        WHERE pgmid = 'R3TR'
        AND   object = 'DTEL'
        AND   devclass = @p_devc
        INTO TABLE @gt_tadir.
      IF gt_tadir IS NOT INITIAL.
        SELECT * FROM dd04t
          FOR ALL ENTRIES IN @gt_tadir
          WHERE rollname = @gt_tadir-rollname
          AND   ddlanguage = @p_lang
          INTO TABLE @DATA(gt_dd04t).
      ENDIF.
      ASSIGN gt_dd04t TO <tab>.
  ENDCASE.

END-OF-SELECTION.

  CALL METHOD cl_salv_table=>factory
    IMPORTING
      r_salv_table = DATA(go_salv)
    CHANGING
      t_table      = <tab>.
  go_salv->get_functions( )->set_all( abap_true ).
  go_salv->get_columns( )->set_optimize( abap_true ).
  go_salv->display( ).