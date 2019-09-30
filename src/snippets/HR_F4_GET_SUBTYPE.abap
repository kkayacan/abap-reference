PROCESS ON VALUE-REQUEST.
FIELD zdfpa_t092-bnksa MODULE bnksa_values.

MODULE bnksa_values INPUT.
  CALL FUNCTION 'HR_F4_GET_SUBTYPE'
    EXPORTING
      infty                = '0009'
    IMPORTING
      subty                = zdfpa_t092-bnksa
    EXCEPTIONS
      infty_not_found      = 1
      no_entries_found     = 2
      cancelled            = 3
      infty_not_supported  = 4
      infty_has_no_subties = 5
      OTHERS               = 6.
ENDMODULE.