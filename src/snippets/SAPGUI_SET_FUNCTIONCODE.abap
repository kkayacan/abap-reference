  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      functioncode           = '/00' "Enter
    EXCEPTIONS
      function_not_supported = 1
      OTHERS                 = 2.