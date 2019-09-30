PROCESS ON VALUE-REQUEST.
  FIELD p0007-schkz MODULE schkz_f4_request.

MODULE schkz_f4_request INPUT.
  PERFORM schkz_f4_request.
ENDMODULE.

FORM schkz_f4_request.
  DATA: intf   TYPE STANDARD TABLE OF ddshiface,
        intfwa LIKE LINE OF intf.

  intfwa-dispfield = 'X'.

  intfwa-shlpfield = 'ZEITY'.
  intfwa-valfield  = 'T503-ZEITY'.
  intfwa-value     = t503-zeity.
  INSERT intfwa INTO TABLE intf.

  intfwa-shlpfield = 'MOSID'.
  intfwa-valfield  = 'T001P-MOSID'.
  intfwa-value     = t001p-mosid.
  INSERT intfwa INTO TABLE intf.

  intfwa-shlpfield = 'MOFID'.
  intfwa-valfield  = 'T001P-MOFID'.
  intfwa-value     = t001p-mofid.
  INSERT intfwa INTO TABLE intf.

  CLEAR intfwa-dispfield.

  intfwa-shlpfield = 'ENDDA'.
  intfwa-valfield  = 'P0007-BEGDA'.
  intfwa-internal  = 'D'.
  INSERT intfwa INTO TABLE intf.

  intfwa-shlpfield = 'BEGDA'.
  intfwa-valfield  = 'P0007-BEGDA'.
  intfwa-internal  = 'D'.
  INSERT intfwa INTO TABLE intf.

  intfwa-shlpfield = 'SCHKZ'.
  intfwa-valfield  = 'P0007-SCHKZ'.
  intfwa-internal  = 'D'.
  INSERT intfwa INTO TABLE intf.

  CALL FUNCTION 'HR_F4HELP_IN_INTERVAL'
    EXPORTING
      im_shlpname  = 'H_T508A'
      im_shlpfield = 'SCHKZ'
    TABLES
      tab_intf     = intf.

ENDFORM.