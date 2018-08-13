DATA t_return TYPE TABLE OF bapiret2.

CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
  EXPORTING
    it_message = t_return.