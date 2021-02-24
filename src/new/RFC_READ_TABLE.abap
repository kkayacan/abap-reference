FUNCTION z_mm_read_be_pr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_BE_CO_CODE) TYPE  MMPUR_D_BE_COCODE
*"     VALUE(IV_BANFN) TYPE  BANFN
*"  EXPORTING
*"     VALUE(ET_BE_PR_ITEM) TYPE  ZTMM_CPROC_EBAN
*"----------------------------------------------------------------------

  DATA: lt_dfies  TYPE TABLE OF dfies,
        lv_length TYPE i,
        lt_values TYPE TABLE OF string,
        lv_banfn  TYPE eban-banfn,
        lv_bnfpo  TYPE eban-bnfpo.

  DATA lt_options     TYPE STANDARD TABLE OF rfc_db_opt.
  DATA lt_fields      TYPE STANDARD TABLE OF rfc_db_fld.
  DATA lt_data        TYPE STANDARD TABLE OF tab512.

  DATA(lv_destination) = zcl_mm_cproc_pr=>retrieve_destination( iv_be_co_code ).

  lt_options = VALUE #( ( text = |BANFN EQ '{ iv_banfn }'| ) ).

  CALL FUNCTION 'DDIF_FIELDINFO_GET' DESTINATION lv_destination
    EXPORTING
      tabname        = 'EBAN'
    TABLES
      dfies_tab      = lt_dfies
    EXCEPTIONS
      not_found      = 1
      internal_error = 2
      OTHERS         = 3.

  DELETE lt_dfies WHERE fieldname = 'MANDT'.

  DO.

    lv_length = 0.
    CLEAR lt_fields.

    LOOP AT lt_dfies ASSIGNING FIELD-SYMBOL(<ls_dfies>).
      lv_length = lv_length + <ls_dfies>-leng.
      IF lv_length > 512.
        EXIT.
      ENDIF.
      ADD 1 TO lv_length.
      APPEND VALUE #( fieldname = <ls_dfies>-fieldname ) TO lt_fields.
      IF <ls_dfies>-keyflag = abap_false.
        CLEAR <ls_dfies>-fieldname.
      ENDIF.
    ENDLOOP.
    DELETE lt_dfies WHERE fieldname IS INITIAL.

    CLEAR lt_data.
    CALL FUNCTION 'RFC_READ_TABLE' DESTINATION lv_destination
      EXPORTING
        query_table          = 'EBAN'
        delimiter            = '|'
      TABLES
        options              = lt_options
        fields               = lt_fields
        data                 = lt_data
      EXCEPTIONS
        table_not_available  = 1
        table_without_data   = 2
        option_not_valid     = 3
        field_not_valid      = 4
        not_authorized       = 5
        data_buffer_exceeded = 6
        OTHERS               = 7.

    CLEAR lt_values.
    LOOP AT lt_data ASSIGNING FIELD-SYMBOL(<ls_data>).
      CLEAR: lv_banfn, lv_bnfpo.
      SPLIT <ls_data>-wa AT '|' INTO TABLE lt_values.
      LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_field>).
        READ TABLE lt_values ASSIGNING FIELD-SYMBOL(<lv_value>) INDEX sy-tabix.
        IF sy-subrc = 0.
          CASE <ls_field>-fieldname.
            WHEN 'BANFN'.
              lv_banfn = <lv_value>.
            WHEN 'BNFPO'.
              lv_bnfpo = <lv_value>.
              READ TABLE et_be_pr_item ASSIGNING FIELD-SYMBOL(<ls_item>) WITH KEY banfn = lv_banfn
                                                                                  bnfpo = lv_bnfpo.
              IF sy-subrc <> 0.
                APPEND INITIAL LINE TO et_be_pr_item ASSIGNING <ls_item>.
                <ls_item>-banfn = lv_banfn.
                <ls_item>-bnfpo = lv_bnfpo.
                <ls_item>-be_co_code = iv_be_co_code.
              ENDIF.
            WHEN OTHERS.
              IF <ls_item> IS ASSIGNED.
                ASSIGN COMPONENT <ls_field>-fieldname OF STRUCTURE <ls_item> TO FIELD-SYMBOL(<lv_target>).
                IF sy-subrc = 0.
                  <lv_target> = <lv_value>.
                ENDIF.
              ENDIF.
          ENDCASE.
        ENDIF.
      ENDLOOP.
      UNASSIGN <ls_item>.
    ENDLOOP.

    IF NOT line_exists( lt_dfies[ keyflag = abap_false ] ).
      EXIT.
    ENDIF.

  ENDDO.

ENDFUNCTION.