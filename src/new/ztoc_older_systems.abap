*&---------------------------------------------------------------------*
*& Report  Z_REQUEST_TOC
*&
*&---------------------------------------------------------------------*
*&  created by Firatc 20251010
*&  toc Request yaratma programı
*&---------------------------------------------------------------------*
REPORT z_request_toc.

DATA: ls_toc         TYPE tr001,
      ls_order       TYPE tr001,
      lv_trstatus    TYPE e070-trstatus,
      lv_retcode     TYPE char3,
      lv_message     TYPE text80,
      lv_target      TYPE tr_target,
      lv_target_fm   TYPE tr_target,
      lt_requests    TYPE TABLE OF cts_req WITH HEADER LINE,
      lt_targeth     TYPE trsysclis WITH HEADER LINE,
      lt_target      TYPE trsysclis,
      ls_target      LIKE LINE OF lt_target,
      lt_test_tr_log TYPE TABLE OF zrt_toc WITH HEADER LINE.

SELECTION-SCREEN BEGIN OF BLOCK b_tr_test WITH FRAME TITLE text-001.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 3(20) text-002 FOR FIELD p_ori_tr.
PARAMETERS p_ori_tr TYPE e070-trkorr OBLIGATORY.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 3(20) text-003 FOR FIELD p_target.
PARAMETERS p_target TYPE zrt_de_target OBLIGATORY DEFAULT text-005 AS
LISTBOX VISIBLE LENGTH 12.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN PUSHBUTTON  3(21) text-001 USER-COMMAND pb_create.
SELECTION-SCREEN PUSHBUTTON 26(21) text-004 USER-COMMAND pb_show.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b_tr_test.


AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'PB_SHOW'.
      PERFORM display_test_transport.
    WHEN 'PB_CREATE'.
      PERFORM testtransport.
      EXIT.
  ENDCASE.

START-OF-SELECTION.
  PERFORM testtransport.

*&---------------------------------------------------------------------*
*&      Form  TESTTRANSPORT
*&---------------------------------------------------------------------*
FORM testtransport.

*----------------------------------------------------------------------*

  AUTHORITY-CHECK OBJECT 'S_TRANSPRT'
  ID 'TTYPE' FIELD 'TRAN'
  ID 'ACTVT' FIELD '60' .

  IF sy-subrc NE 0 .
    MESSAGE 'Keine Berechtigung für den Transport von Kopien!' TYPE
    'E' .
  ENDIF .


  CALL FUNCTION 'TR_READ_COMM'
    EXPORTING
      wi_dialog        = ' '
      wi_sel_e070      = 'X'
      wi_sel_e071      = ' '
      wi_trkorr        = p_ori_tr
    IMPORTING
      we_e070          = ls_order
    EXCEPTIONS
      not_exist_e070   = 1
      no_authorization = 2
      OTHERS           = 99.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
    RAISING general_failure.
  ENDIF.

  IF ls_order-trstatus = 'R' .
    MESSAGE e126(tk) WITH p_ori_tr.
    EXIT.
  ENDIF.

* Check transport target
  IF p_target(1) <> '/' AND p_target+3(1) <> '.'.
    CONCATENATE p_target(3) '.' '300'
    INTO lv_target.
  ELSE.
    lv_target = p_target.
  ENDIF.

  REFRESH lt_targeth.
  REFRESH lt_target.
  CLEAR ls_target.

  IF lv_target(1) = '/'.

    CALL FUNCTION 'TR_READ_TARGET_GROUP'
      EXPORTING
        iv_targ_group           = lv_target
      IMPORTING
        et_targets              = lt_target
      EXCEPTIONS
        targ_group_doesnt_exist = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    LOOP AT lt_target INTO ls_target.
      MOVE-CORRESPONDING ls_target TO lt_targeth.
      APPEND lt_targeth.
    ENDLOOP.

  ELSE.
    ls_target-sysname = lv_target(3).
    ls_target-client = lv_target+4(3).
    MOVE-CORRESPONDING ls_target TO lt_targeth.
    APPEND lt_targeth.
  ENDIF.

  lv_target_fm = lv_target(3).

* Create Transport of Copies

  CALL FUNCTION 'TMW_CREATE_TRANSPORT_OF_COPIES'
    EXPORTING
      iv_transport_target    = lv_target_fm
      iv_trkorr              = p_ori_tr
      iv_ignore_request_type = 'X'
    IMPORTING
      es_pre_transport       = ls_toc
    EXCEPTIONS
      order_check_error      = 1
      enqueue_failed         = 2
      error_parameters       = 3
      reuse_request_failure  = 4
      create_order_failure   = 5
      export_request_failed  = 6
      configuration_error    = 7
      general_failure        = 8
      OTHERS                 = 9.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
    RAISING configuration_error.
  ENDIF.

* Write the orginal TR and the test TR into log table
  SELECT *
  FROM zrt_toc
  INTO TABLE lt_test_tr_log
  WHERE orig_transport = p_ori_tr
  AND active = 'X'.

  IF sy-subrc = 0.
    READ TABLE lt_test_tr_log INDEX 1.
    lt_test_tr_log-active = ''.
    MODIFY lt_test_tr_log TRANSPORTING active
    WHERE orig_transport = p_ori_tr.
  ENDIF.

  lt_test_tr_log-orig_transport = p_ori_tr.
  lt_test_tr_log-test_transport = ls_toc-trkorr.
  lt_test_tr_log-create_date = sy-datum.
  lt_test_tr_log-create_time = sy-uzeit.
  lt_test_tr_log-active = 'X'.
  APPEND lt_test_tr_log.
  MODIFY zrt_toc FROM TABLE lt_test_tr_log.


* Check if this ToC is released for 15 times, wait 10 seconds between
* each time
* Import this ToC in the target system(s) when it is released

  DO 15 TIMES.
    CLEAR: lv_trstatus.

    SELECT SINGLE trstatus
    FROM e070
    INTO lv_trstatus
    WHERE trkorr = ls_toc-trkorr.

    IF sy-subrc = 0 AND lv_trstatus = 'R'.
      REFRESH lt_requests.
      lt_requests-request = ls_toc-trkorr.
      APPEND lt_requests.

      LOOP AT lt_targeth INTO ls_target.

        CALL FUNCTION 'CTS_API_IMPORT_CHANGE_REQUEST'
          EXPORTING
            system   = ls_target-sysname(3)
            client   = ls_target-client
          IMPORTING
            retcode  = lv_retcode
            message  = lv_message
          TABLES
            requests = lt_requests
          EXCEPTIONS
            OTHERS   = 99.

        IF sy-subrc <> 0.
          MESSAGE i333(s1) WITH 'import test transport failed.'
          'sy-subrc =' sy-subrc.
          EXIT.
        ENDIF.
        MESSAGE i333(s1) WITH lv_message(38) lv_message+39(41).
      ENDLOOP.

      EXIT.

    ELSE.

      WAIT UP TO 10 SECONDS.

    ENDIF.

  ENDDO.

* Go to the toc display screen
  CALL FUNCTION 'TR_PRESENT_REQUEST'
    EXPORTING
      iv_trkorr = ls_toc-trkorr
    .
ENDFORM.                    "TESTTRANSPORT


*&---------------------------------------------------------------------*
*&      Form  display_test_transport
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_test_transport.

  SELECT *
  FROM zrt_toc
  INTO TABLE lt_test_tr_log
  WHERE orig_transport = p_ori_tr.

  IF sy-subrc <> 0.
    MESSAGE i333(s1) WITH 'No test transport found!'.
  ELSE.

    CALL FUNCTION '/SDF/DISPLAY_TABLE'
      TABLES
        table = lt_test_tr_log.

  ENDIF.

  CLEAR lt_test_tr_log.
  REFRESH lt_test_tr_log[].

ENDFORM.