TABLES sscrfields.

PARAMETERS: p_fname TYPE ibipparms-path.

SELECTION-SCREEN FUNCTION KEY 1.

INITIALIZATION.
  sscrfields-functxt_01 = text-s01.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'P_FNAME'.
      screen-required = '2'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  PERFORM file_open_dialog CHANGING p_fname.

AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'FC01'.
    WHEN 'ONLI'.
      IF p_fname IS INITIAL.
        MESSAGE e055(00).
*   Tüm zorunlu alanları doldurun
      ENDIF.
  ENDCASE.