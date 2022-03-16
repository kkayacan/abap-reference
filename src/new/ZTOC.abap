*&---------------------------------------------------------------------*
*& Report ZREQUEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZREQUEST.

TYPE-POOLS: slis, trwbo.

TABLES: e070.

PARAMETERS: p_new RADIOBUTTON GROUP gr1 USER-COMMAND rb DEFAULT 'X',
            p_old RADIOBUTTON GROUP gr1.

SELECTION-SCREEN SKIP 1.

PARAMETERS: p_oldrq TYPE e070-trkorr.
SELECT-OPTIONS: s_reque FOR e070-trkorr NO INTERVALS.
PARAMETERS: p_uname TYPE xubname.

SELECTION-SCREEN SKIP 1.

PARAMETERS: p_relnw AS CHECKBOX DEFAULT 'X' USER-COMMAND cb,
            p_trnnw AS CHECKBOX DEFAULT 'X',
            p_relsb AS CHECKBOX DEFAULT 'X'.

DATA: BEGIN OF gt_sel OCCURS 0,
        selkz   TYPE selkz,
        trkorr  LIKE v_e071eu-trkorr,
        as4user LIKE v_e071eu-as4user,
        as4text LIKE e07t-as4text,
      END OF gt_sel.
DATA: gt_e071eu TYPE TABLE OF v_e071eu,
      gt_fldct  TYPE slis_t_fieldcat_alv.
DATA: gs_e071eu TYPE v_e071eu,
      gs_e071   TYPE e071,
      gs_e070   TYPE e070,
      gs_sel    LIKE gt_sel,
      gs_header TYPE trwbo_request_header.
DATA: gv_title TYPE as4text,
      gv_err   TYPE xfeld.
CONSTANTS: gc_client TYPE stpa-client VALUE '100'.

AT SELECTION-SCREEN OUTPUT.
  PERFORM f_modify_screen.

AT SELECTION-SCREEN.
  PERFORM f_selection_screen.

AT SELECTION-SCREEN ON s_reque.
  PERFORM f_check_reque.

START-OF-SELECTION.
  PERFORM f_get_subtasks.
  PERFORM f_create_toc.
  PERFORM f_check_old_toc.
  PERFORM f_relase_subtasks.
  PERFORM f_release_n_transport_toc.

*&---------------------------------------------------------------------*
*&      Form  F_MODIFY_SCREEN
*&---------------------------------------------------------------------*
FORM f_modify_screen.
  %_p_new_%_app_%-text   = 'Yeni TOC oluştur'.
  %_p_old_%_app_%-text   = 'Mevcut TOC''a ekle'.
  %_s_reque_%_app_%-text = 'Request'.
  %_p_oldrq_%_app_%-text = 'Mevcut TOC'.
  %_p_uname_%_app_%-text = 'Kullanıcı'.
  %_p_relnw_%_app_%-text = 'TOC''u release et'.
  %_p_trnnw_%_app_%-text = 'TOC''u taşı'.
  %_p_relsb_%_app_%-text = 'Sub task''i release et'.

  LOOP AT SCREEN.
    IF p_relnw IS INITIAL.
      IF screen-name EQ 'P_TRNNW'.
        screen-input = 0.
      ENDIF.
    ENDIF.

    IF screen-name EQ 'S_REQUE-LOW'.
      screen-required = 2.
    ENDIF.

    IF p_old IS INITIAL.
      IF screen-name CS 'P_OLDRQ'.
        screen-active = 0.
      ENDIF.
    ELSE.
      IF screen-name EQ 'P_OLDRQ'.
        screen-required = 2.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SELECTION_SCREEN
*&---------------------------------------------------------------------*
FORM f_selection_screen.
  IF p_relnw IS INITIAL.
    CLEAR p_trnnw.
  ENDIF.

  LOOP AT s_reque.
    CONDENSE s_reque-low.
    MODIFY s_reque.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_REQUE
*&---------------------------------------------------------------------*
FORM f_check_reque.
  CLEAR gv_err.

  CHECK sy-ucomm EQ 'ONLI'.
  CHECK s_reque[] IS INITIAL.

  gv_err = 'X'.

  MESSAGE s899(fb) WITH 'Tüm zorunlu alanları doldurun'
    DISPLAY LIKE 'E'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_SUBTASKS
*&---------------------------------------------------------------------*
FORM f_get_subtasks.
  RANGES: lr_uname FOR v_e071eu-as4user.

  FREE: gt_e071eu, gt_sel.

  CHECK gv_err IS INITIAL.

  IF p_uname IS NOT INITIAL.
    lr_uname = 'IEQ'.
    lr_uname-low = p_uname.
    APPEND lr_uname.
  ENDIF.

  SELECT *
    FROM v_e071eu
    INTO TABLE gt_e071eu
    WHERE strkorr  IN s_reque
      AND as4user  IN lr_uname
      AND trstatus EQ 'D'.

  LOOP AT gt_e071eu INTO gs_e071eu.
    MOVE-CORRESPONDING gs_e071eu TO gs_sel.
    COLLECT gs_sel INTO gt_sel.
  ENDLOOP.
  LOOP AT gt_sel INTO gs_sel.
    SELECT SINGLE as4text
      FROM e07t
      INTO gs_sel-as4text
      WHERE trkorr EQ gs_sel-trkorr.
    MODIFY gt_sel FROM gs_sel.
  ENDLOOP.
  IF sy-subrc NE 0.
    gv_err = 'X'.

    RETURN.
  ENDIF.

  SELECT SINGLE as4text
    FROM e07t
    INTO gv_title
    WHERE trkorr IN s_reque.

  IF gt_sel[] IS INITIAL.
    gv_err = 'X'.

    RETURN.
  ENDIF.

  DESCRIBE TABLE gt_sel.
  IF sy-tfill EQ 1.
    LOOP AT gt_sel INTO gs_sel.
      gs_sel-selkz = 'X'.
      MODIFY gt_sel FROM gs_sel.
    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_internal_tabname     = 'GT_SEL'
      i_inclname             = sy-repid
      i_bypassing_buffer     = 'X'
    CHANGING
      ct_fieldcat            = gt_fldct[]
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_title               = gv_title
      i_screen_start_column = 10
      i_screen_start_line   = 2
      i_screen_end_column   = 110
      i_screen_end_line     = 7
      i_selection           = 'X'
      i_zebra               = 'X'
      i_tabname             = 'GT_SEL'
      it_fieldcat           = gt_fldct[]
      i_checkbox_fieldname  = 'SELKZ'
    TABLES
      t_outtab              = gt_sel
    EXCEPTIONS
      program_error         = 1
      OTHERS                = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT gt_sel INTO gs_sel WHERE selkz EQ 'X'.
    EXIT.
  ENDLOOP.
  IF sy-subrc NE 0.
    gv_err = 'X'.

    RETURN.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CREATE_TOC
*&---------------------------------------------------------------------*
FORM f_create_toc .
  DATA: lt_e071 TYPE tr_objects,
        lt_line TYPE TABLE OF tline.
  DATA: lv_like      TYPE as4text,
        lv_datum     TYPE datum,
        lv_tarsystem TYPE tr_target,
        lv_title     TYPE as4text,
        lv_tdline    TYPE tdline.

  CHECK gv_err IS INITIAL.
  CHECK p_new IS NOT INITIAL.

  CONCATENATE 'TOC' gv_title INTO gv_title SEPARATED BY space.

  CONCATENATE gv_title(57) '%' INTO lv_like.

  lv_datum = sy-datum - 300.
  SELECT COUNT( * )
    FROM e07t
    INNER JOIN e070 ON e070~trkorr EQ e07t~trkorr
    INTO @DATA(lv_count)
    WHERE as4text    LIKE @lv_like
      AND trfunction EQ 'T'
      AND as4date    GE @lv_datum.

  DATA(lv_numc2) = CONV numc2( lv_count ).
  ADD 1 TO lv_numc2.

  CONCATENATE '#' lv_numc2 INTO lv_title.
  gv_title+57(3) = lv_title.

  SELECT SINGLE tarsystem
    FROM e070
    INTO lv_tarsystem
    WHERE trkorr IN s_reque.

  CALL FUNCTION 'TR_INSERT_REQUEST_WITH_TASKS'
    EXPORTING
      iv_type           = 'T'
      iv_text           = gv_title
      iv_target         = lv_tarsystem
    IMPORTING
      es_request_header = gs_header
    EXCEPTIONS
      insert_failed     = 1
      enqueue_failed    = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    gv_err = 'X'.

    RETURN.
  ENDIF.

  DO.
    SELECT SINGLE *
      FROM e070
      INTO gs_e070
      WHERE trkorr EQ gs_header-trkorr.
    IF sy-subrc EQ 0.
      EXIT.
    ELSE.
      WAIT UP TO '0.5' SECONDS.
    ENDIF.
  ENDDO.

  PERFORM f_insert_subtasks TABLES lt_e071.

  LOOP AT gt_sel INTO gs_sel WHERE selkz EQ 'X'.
    APPEND VALUE #( tdformat = '/' tdline = gs_sel-trkorr ) TO lt_line.
  ENDLOOP.
  IF sy-subrc EQ 0.
    CONCATENATE '@' sy-uname INTO lv_tdline.

    APPEND VALUE #( tdformat = '/' tdline = space ) TO lt_line.
    APPEND VALUE #( tdformat = '/' tdline = lv_tdline ) TO lt_line.
  ENDIF.

  IF lt_line IS NOT INITIAL.
    CALL FUNCTION 'TRINT_DOCU_INTERFACE'
      EXPORTING
        iv_object = gs_header-trkorr
        iv_action = 'M'
      TABLES
        tt_line   = lt_line
      EXCEPTIONS
        error     = 1.
  ENDIF.

  MESSAGE s899(fb) WITH 'Request oluşturuldu'
    gs_header-trkorr." DISPLAY LIKE 'S'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_OLD_TOC
*&---------------------------------------------------------------------*
FORM f_check_old_toc.
  DATA: lt_e071 TYPE tr_objects.

  CHECK p_old IS NOT INITIAL.

  SELECT SINGLE *
    FROM e070
    INTO CORRESPONDING FIELDS OF @gs_header
    WHERE trkorr EQ @p_oldrq.
  IF sy-subrc NE 0.
    gv_err = 'X'.

    MESSAGE s899(fb) WITH 'Mevcut TOC hatalı' DISPLAY LIKE 'E'.

    RETURN.
  ENDIF.

  PERFORM f_insert_subtasks TABLES lt_e071.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_RELASE_SUBTASKS
*&---------------------------------------------------------------------*
FORM f_relase_subtasks .
  CHECK gv_err IS INITIAL.

  IF p_relsb EQ 'X'.
    LOOP AT gt_sel INTO gs_sel WHERE selkz EQ 'X'.
      CALL FUNCTION 'TR_RELEASE_REQUEST'
        EXPORTING
          iv_trkorr                  = gs_sel-trkorr
          iv_dialog                  = space
          iv_without_locking         = 'X'
          iv_display_export_log      = space
        EXCEPTIONS
          cts_initialization_failure = 1
          enqueue_failed             = 2
          no_authorization           = 3
          invalid_request            = 4
          request_already_released   = 5
          repeat_too_early           = 6
          error_in_export_methods    = 7
          object_check_error         = 8
          docu_missing               = 9
          db_access_error            = 10
          action_aborted_by_user     = 11
          export_failed              = 12
          OTHERS                     = 13.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_RELEASE_N_TRANSPORT_TOC
*&---------------------------------------------------------------------*
FORM f_release_n_transport_toc.
  DATA: lt_req TYPE stms_tr_requests.

  CHECK gv_err IS INITIAL.

  IF p_relnw EQ 'X'.
    CALL FUNCTION 'TR_RELEASE_REQUEST'
      EXPORTING
        iv_trkorr                  = gs_header-trkorr
        iv_dialog                  = space
        iv_without_locking         = 'X'
        iv_display_export_log      = space
      EXCEPTIONS
        cts_initialization_failure = 1
        enqueue_failed             = 2
        no_authorization           = 3
        invalid_request            = 4
        request_already_released   = 5
        repeat_too_early           = 6
        error_in_export_methods    = 7
        object_check_error         = 8
        docu_missing               = 9
        db_access_error            = 10
        action_aborted_by_user     = 11
        export_failed              = 12
        OTHERS                     = 13.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF p_trnnw EQ 'X'.
      DO 20 TIMES.
        SELECT SINGLE *
          FROM e070
          INTO gs_e070
          WHERE trkorr EQ gs_header-trkorr.
        IF gs_e070-trstatus EQ 'R'.
          SELECT  *
            FROM e070
            INTO CORRESPONDING FIELDS OF TABLE lt_req
            WHERE trkorr EQ gs_header-trkorr.

          CALL FUNCTION 'TMS_UI_TRANSMIT_TR_QUEUE'
            EXPORTING
              iv_system             = gs_e070-tarsystem
              it_requests           = lt_req
            EXCEPTIONS
              cancelled_by_user     = 1
              without_refresh       = 2
              transmit_queue_failed = 3
              OTHERS                = 4.
          IF sy-subrc <> 0.
            IF sy-subrc EQ 1.
              RETURN.
            ENDIF.

            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

          CALL FUNCTION 'TMS_MGR_IMPORT_TR_REQUEST'
            EXPORTING
              iv_system                  = gs_e070-tarsystem
              iv_request                 = gs_header-trkorr
              iv_client                  = gc_client
              iv_overtake                = 'X'
            EXCEPTIONS
              read_config_failed         = 1
              table_of_requests_is_empty = 2
              OTHERS                     = 3.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

          EXIT.
        ELSE.
          WAIT UP TO 1 SECONDS.
        ENDIF.
      ENDDO.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_INSERT_SUBTASKS
*&---------------------------------------------------------------------*
FORM f_insert_subtasks TABLES t_e071 TYPE tr_objects.
  DATA: lt_e071k TYPE tr_keys.

  LOOP AT gt_sel INTO gs_sel WHERE selkz EQ 'X'.
    LOOP AT gt_e071eu INTO gs_e071eu WHERE trkorr EQ gs_sel-trkorr.
      CLEAR gs_e071.
      MOVE-CORRESPONDING gs_e071eu TO gs_e071.
      APPEND gs_e071 TO t_e071.
    ENDLOOP.

    SELECT *
      FROM e071k
      APPENDING TABLE @lt_e071k
      WHERE trkorr EQ @gs_sel-trkorr.
  ENDLOOP.

  CALL FUNCTION 'TR_REQUEST_CHOICE'
    EXPORTING
      iv_suppress_dialog   = 'X'
      iv_request_types     = 'T'
      iv_request           = gs_header-trkorr
      it_e071              = t_e071[]
      it_e071k             = lt_e071k[]
    IMPORTING
      es_request           = gs_header
    EXCEPTIONS
      invalid_request      = 1
      invalid_request_type = 2
      user_not_owner       = 3
      no_objects_appended  = 4
      enqueue_error        = 5
      cancelled_by_user    = 6
      recursive_call       = 7
      OTHERS               = 8.
  IF sy-subrc <> 0.
    IF p_old EQ space.
      CALL FUNCTION 'TRINT_TDR_USER_COMMAND'
        EXPORTING
          iv_object  = gs_header-trkorr
          iv_type    = 'TASK'
          iv_command = 'DELE'.
    ENDIF.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    gv_err = 'X'.

    RETURN.
  ENDIF.

ENDFORM.
**********************************************************************
***TRINT_DOCU_INTERFACE
***DOKHL
