DATA garg     TYPE seqg3-garg.
DATA enq      TYPE STANDARD TABLE OF seqg3.
DATA(lv_unlocked) = abap_false.
garg = sy-mandt && iv_vgbel.
DO 60 TIMES.
  WAIT UP TO 1 SECONDS.
  CALL FUNCTION 'ENQUEUE_READ'
    EXPORTING
      gname                 = 'EKKO'
      garg                  = garg
    TABLES
      enq                   = enq
    EXCEPTIONS
      communication_failure = 1
      system_failure        = 2
      OTHERS                = 3.
  IF enq IS INITIAL.
    lv_unlocked = abap_true.
    EXIT.
  ENDIF.
ENDDO.

IF lv_unlocked = abap_false.
  RETURN.
ENDIF.