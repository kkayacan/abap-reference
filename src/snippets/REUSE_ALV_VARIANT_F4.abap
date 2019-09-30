PARAMETERS: p_vari LIKE disvariant-variant.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM get_vari CHANGING p_vari.

FORM get_vari CHANGING p_vari.

  DATA:

  ls_vari TYPE disvariant.

  ls_vari-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = ls_vari
    IMPORTING
      es_variant    = ls_vari
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  p_vari = ls_vari-variant.

ENDFORM.