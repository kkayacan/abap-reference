SELECT ddtext FROM dd07t INTO @DATA(lv_ddtext) UP TO 1 ROWS
  WHERE domname    = 'ZRT_D_PERIOD'
    AND ddlanguage = @sy-langu
    AND as4local   = 'A'
    AND domvalue_l = @ip_period.
ENDSELECT.