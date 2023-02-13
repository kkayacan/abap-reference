CALL FUNCTION 'OXT_MESSAGE_TO_POPUP'
EXPORTING
  it_message = gt_return_all
EXCEPTIONS
  bal_error  = 1
  OTHERS     = 2.