*----------------------------------------------------------------------*
*       CLASS cl_appl_log DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*

CLASS cl_appl_log_root DEFINITION.
    PUBLIC SECTION.
  
      DATA :
        gs_display_profile TYPE  bal_s_prof,
        gv_log_handle      TYPE  balloghndl,
  
        gv_no_toolbar      TYPE c VALUE '',
  
        gt_t_temp_bapiret2 TYPE bapiret2_t.
  
      METHODS :
        constructor,
  
        create_log_object
          IMPORTING
            ip_objname     TYPE c OPTIONAL
            ip_sub_objname TYPE c OPTIONAL,
  
        create_display_profile,
  
        add_message
          IMPORTING ip_msgty      TYPE c DEFAULT 'I'
                    ip_msgid      TYPE c OPTIONAL
                    ip_msgno      TYPE c OPTIONAL
                    ip_msgv1      TYPE any OPTIONAL
                    ip_msgv2      TYPE any OPTIONAL
                    ip_msgv3      TYPE any OPTIONAL
                    ip_msgv4      TYPE any OPTIONAL
  
                    ip_s_bal_msg  TYPE bal_s_msg OPTIONAL
                    ip_t_bal_msg  TYPE bal_t_msg OPTIONAL
  
                    ip_s_bapiret2 TYPE bapiret2 OPTIONAL
                    ip_t_bapiret2 TYPE bapiret2_t OPTIONAL
  
                    ip_free_text  TYPE any OPTIONAL
                    ip_free_text2 TYPE any OPTIONAL
                    ip_free_text3 TYPE any OPTIONAL
                    ip_free_text4 TYPE any OPTIONAL,
  
        clear_log,
  
        error_exists
          RETURNING VALUE(ep_return) TYPE char1,
  
        read_log EXPORTING ep_t_messages TYPE bal_t_msg,
  
        log_write_to_spool,
  
        save_to_db.
  
  
      CLASS-METHODS :
  
        get_mess_text
          IMPORTING
            ip_s_bapiret2 TYPE bapiret2 OPTIONAL
            ip_s_bal_msg  TYPE bal_s_msg OPTIONAL
          EXPORTING
            ep_text       TYPE c,
  
        log_write_to_spool_static
          IMPORTING
            ip_t_bapiret2 TYPE bapiret2_t OPTIONAL
            ip_t_bal_msg  TYPE bal_t_msg OPTIONAL
          EXPORTING
            ep_t_text     TYPE bapiret2_t,
  
        add_to_message_tab
          IMPORTING ip_msgty      TYPE c OPTIONAL
                    ip_msgid      TYPE c OPTIONAL
                    ip_msgno      TYPE any OPTIONAL
  
                    ip_msgv1      TYPE any OPTIONAL
                    ip_msgv2      TYPE any OPTIONAL
                    ip_msgv3      TYPE any OPTIONAL
                    ip_msgv4      TYPE any OPTIONAL
  
                    ip_free_text  TYPE any OPTIONAL
                    ip_free_text2 TYPE any OPTIONAL
                    ip_free_text3 TYPE any OPTIONAL
                    ip_free_text4 TYPE any OPTIONAL
  
                    ip_s_bal_msg  TYPE bal_s_msg OPTIONAL
                    ip_s_bapiret2 TYPE bapiret2 OPTIONAL
  
                    ip_anon       TYPE char1 DEFAULT ''
  
          CHANGING  cp_t_bapiret2 TYPE bapiret2_t OPTIONAL
                    cp_t_bal_msg  TYPE bal_t_msg OPTIONAL,
  
        format_message
          IMPORTING ip_msgty      TYPE c OPTIONAL
                    ip_msgid      TYPE c OPTIONAL
                    ip_msgno      TYPE any OPTIONAL
  
                    ip_msgv1      TYPE any OPTIONAL
                    ip_msgv2      TYPE any OPTIONAL
                    ip_msgv3      TYPE any OPTIONAL
                    ip_msgv4      TYPE any OPTIONAL
  
                    ip_free_text  TYPE any OPTIONAL
                    ip_free_text2 TYPE any OPTIONAL
                    ip_free_text3 TYPE any OPTIONAL
                    ip_free_text4 TYPE any OPTIONAL
  
                    ip_anon       TYPE char1 DEFAULT ''  " convert mess to anonym
  
          EXPORTING ep_s_bapiret2 TYPE bapiret2
                    ep_s_bal_msg  TYPE bal_s_msg,
  
        error_mess_exists
          IMPORTING ip_t_bapiret2    TYPE bapiret2_t OPTIONAL
                    ip_t_bal_msg     TYPE bal_t_msg OPTIONAL
          RETURNING VALUE(ep_return) TYPE char1,
  
        convert_bal_to_bapiret
          IMPORTING
            ip_s_bal_msg  TYPE bal_s_msg
          EXPORTING
            ep_s_bapiret2 TYPE bapiret2,
  
        convert_bapiret_to_bal
          IMPORTING
            ip_s_bapiret2 TYPE bapiret2
          EXPORTING
            ep_s_bal_msg  TYPE bal_s_msg,
  
  
        get_fnc_from_message
          IMPORTING
            ip_char_count TYPE i DEFAULT 50
          EXPORTING
            ep_result     TYPE c
            ep_exit       TYPE c
          CHANGING
            cp_message    TYPE c.
  
  
  
  
  
  ENDCLASS.                    "cl_appl_log_root DEFINITION
  
  *----------------------------------------------------------------------*
  *       CLASS cl_appl_log_root IMPLEMENTATION
  *----------------------------------------------------------------------*
  *
  *----------------------------------------------------------------------*
  CLASS cl_appl_log_root IMPLEMENTATION.
  
    METHOD constructor.
  
    ENDMETHOD.                    "constructor
  
    METHOD add_message.
  
      DATA : ls_msg      TYPE bal_s_msg,
             ls_bapiret2 TYPE bapiret2.
  
      IF ip_msgno IS SUPPLIED
      AND NOT ip_msgno IS INITIAL.
  * --1-- Single message with number , type and variables :
  
        CALL METHOD format_message
          EXPORTING
            ip_msgty     = ip_msgty
            ip_msgid     = ip_msgid
            ip_msgno     = ip_msgno
            ip_msgv1     = ip_msgv1
            ip_msgv2     = ip_msgv2
            ip_msgv3     = ip_msgv3
            ip_msgv4     = ip_msgv4
          IMPORTING
            ep_s_bal_msg = ls_msg.
  
        CALL FUNCTION 'BAL_LOG_MSG_ADD'
          EXPORTING
            i_log_handle = gv_log_handle
            i_s_msg      = ls_msg
          EXCEPTIONS
            OTHERS       = 1.
  
      ELSEIF ip_s_bal_msg IS SUPPLIED.
  
  * --2-- Single message with type bal_s_msg  :
        CALL METHOD format_message
          EXPORTING
            ip_msgty     = ip_s_bal_msg-msgty
            ip_msgid     = ip_s_bal_msg-msgid
            ip_msgno     = ip_s_bal_msg-msgno
            ip_msgv1     = ip_s_bal_msg-msgv1
            ip_msgv2     = ip_s_bal_msg-msgv2
            ip_msgv3     = ip_s_bal_msg-msgv3
            ip_msgv4     = ip_s_bal_msg-msgv4
          IMPORTING
            ep_s_bal_msg = ls_msg.
  
        CALL FUNCTION 'BAL_LOG_MSG_ADD'
          EXPORTING
            i_log_handle = gv_log_handle
            i_s_msg      = ls_msg
          EXCEPTIONS
            OTHERS       = 1.
  
      ELSEIF ip_t_bal_msg IS SUPPLIED.
  
  * --3-- Message table with type bal_s_msg
  
        LOOP AT ip_t_bal_msg INTO ls_msg.
  
          CALL METHOD format_message
            EXPORTING
              ip_msgty     = ls_msg-msgty
              ip_msgid     = ls_msg-msgid
              ip_msgno     = ls_msg-msgno
              ip_msgv1     = ls_msg-msgv1
              ip_msgv2     = ls_msg-msgv2
              ip_msgv3     = ls_msg-msgv3
              ip_msgv4     = ls_msg-msgv4
            IMPORTING
              ep_s_bal_msg = ls_msg.
  
          CALL FUNCTION 'BAL_LOG_MSG_ADD'
            EXPORTING
              i_log_handle = gv_log_handle
              i_s_msg      = ls_msg
            EXCEPTIONS
              OTHERS       = 1.
  
          CLEAR ls_msg.
  
        ENDLOOP.
  
      ELSEIF ip_s_bapiret2 IS SUPPLIED.
  * --5-- Single message with type BAPIRET2
  
        CALL METHOD format_message
          EXPORTING
            ip_msgty     = ip_s_bapiret2-type
            ip_msgid     = ip_s_bapiret2-id
            ip_msgno     = ip_s_bapiret2-number
            ip_msgv1     = ip_s_bapiret2-message_v1
            ip_msgv2     = ip_s_bapiret2-message_v2
            ip_msgv3     = ip_s_bapiret2-message_v3
            ip_msgv4     = ip_s_bapiret2-message_v4
          IMPORTING
            ep_s_bal_msg = ls_msg.
  
        CALL FUNCTION 'BAL_LOG_MSG_ADD'
          EXPORTING
            i_log_handle = gv_log_handle
            i_s_msg      = ls_msg
          EXCEPTIONS
            OTHERS       = 1.
  
  
      ELSEIF ip_t_bapiret2 IS SUPPLIED .
  
  * --5-- Message table with type BAPIRET2
  
        LOOP AT ip_t_bapiret2 INTO ls_bapiret2.
  
          CALL METHOD format_message
            EXPORTING
              ip_msgty     = ls_bapiret2-type
              ip_msgid     = ls_bapiret2-id
              ip_msgno     = ls_bapiret2-number
              ip_msgv1     = ls_bapiret2-message_v1
              ip_msgv2     = ls_bapiret2-message_v2
              ip_msgv3     = ls_bapiret2-message_v3
              ip_msgv4     = ls_bapiret2-message_v4
            IMPORTING
              ep_s_bal_msg = ls_msg.
  
          CALL FUNCTION 'BAL_LOG_MSG_ADD'
            EXPORTING
              i_log_handle = gv_log_handle
              i_s_msg      = ls_msg
            EXCEPTIONS
              OTHERS       = 1.
  
          CLEAR : ls_msg, ls_bapiret2.
  
        ENDLOOP.
  
      ELSEIF ip_free_text IS SUPPLIED.
  
  * --5-- Free text as message :
  
        CALL METHOD format_message
          EXPORTING
            ip_msgty      = ip_msgty
            ip_free_text  = ip_free_text
            ip_free_text2 = ip_free_text2
            ip_free_text3 = ip_free_text3
            ip_free_text4 = ip_free_text4
          IMPORTING
            ep_s_bapiret2 = ls_bapiret2.
  
        CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
          EXPORTING
            i_msgty          = ip_msgty
            i_text           = ls_bapiret2-message
  *         I_PROBCLASS      = 'I'
  *         E_S_MSG_HANDLE   = LV_HANDLE
            i_log_handle     = gv_log_handle
          EXCEPTIONS
            log_not_found    = 1
            msg_inconsistent = 2
            log_is_full      = 3
            OTHERS           = 4.
  
      ENDIF.
  
    ENDMETHOD.                    "add_message
  
    METHOD create_log_object.
  
      DATA ls_log TYPE  bal_s_log.
  
      ls_log-object     = ip_objname.
      ls_log-subobject  = ip_sub_objname.
  
      CALL FUNCTION 'BAL_LOG_CREATE'
        EXPORTING
          i_s_log                 = ls_log
        IMPORTING
          e_log_handle            = gv_log_handle
        EXCEPTIONS
          log_header_inconsistent = 1
          OTHERS                  = 2.
  
    ENDMETHOD.                    "create_log_object
  
    METHOD create_display_profile.
  
      " -- Display Profile Settings :
      "    No Tree :
      CALL FUNCTION 'BAL_DSP_PROFILE_NO_TREE_GET'
        IMPORTING
          e_s_display_profile = gs_display_profile
        EXCEPTIONS
          OTHERS              = 1.
  
      gs_display_profile-use_grid = 'X'.
      gs_display_profile-disvariant-report = sy-repid.
      gs_display_profile-disvariant-handle = 'APPL_LOG'.
  
      gs_display_profile-no_toolbar = gv_no_toolbar.
  
  
    ENDMETHOD.                    "create_display_profile
  
    METHOD save_to_db.
  
      CALL FUNCTION 'BAL_DB_SAVE'
        EXPORTING
          i_save_all       = 'X'
        EXCEPTIONS
          log_not_found    = 1
          save_not_allowed = 2
          numbering_error  = 3
          OTHERS           = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  
        EXIT.
      ENDIF.
  
      COMMIT WORK AND WAIT.
  
    ENDMETHOD.
  
    METHOD get_mess_text.
  
      CLEAR ep_text.
  
      IF NOT ip_s_bapiret2 IS INITIAL.
  
        MESSAGE ID ip_s_bapiret2-id
        TYPE ip_s_bapiret2-type
        NUMBER ip_s_bapiret2-number
        WITH ip_s_bapiret2-message_v1
             ip_s_bapiret2-message_v2
             ip_s_bapiret2-message_v3
             ip_s_bapiret2-message_v4
        INTO ep_text.
  
      ELSE.
  
        MESSAGE
          ID      ip_s_bal_msg-msgid
          TYPE    ip_s_bal_msg-msgty
          NUMBER  ip_s_bal_msg-msgno
        WITH
          ip_s_bal_msg-msgv1
          ip_s_bal_msg-msgv2
          ip_s_bal_msg-msgv3
          ip_s_bal_msg-msgv4
        INTO ep_text.
  
      ENDIF.
  
  
    ENDMETHOD.
  
    METHOD error_exists.
  
      DATA lt_messages TYPE bal_t_msg.
  
      CALL METHOD me->read_log
        IMPORTING
          ep_t_messages = lt_messages.
  
      LOOP AT lt_messages TRANSPORTING NO FIELDS
      WHERE msgty = 'E'
         OR msgty = 'A'.
        EXIT.
      ENDLOOP.
  
      IF sy-subrc EQ 0.
        ep_return = 'X'.
      ELSE.
        ep_return = ''.
      ENDIF.
  
  
    ENDMETHOD.                    "error_exists
  
    METHOD clear_log.
  
      CALL FUNCTION 'BAL_LOG_MSG_DELETE_ALL'
        EXPORTING
          i_log_handle  = gv_log_handle
        EXCEPTIONS
          log_not_found = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
  * MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
  *         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
  
    ENDMETHOD.                    "clear_log
  
    METHOD log_write_to_spool_static.
  
  *    IMPORTING
  *      ip_t_bapiret2 TYPE bapiret2_t OPTIONAL
  *      ip_t_bal_msg TYPE bal_t_msg OPTIONAL
  *    EXPORTING
  *      ep_t_text TYPE bapiret2_t,
  
      DATA ls_bapiret2 LIKE LINE OF ip_t_bapiret2.
      DATA ls_bal LIKE LINE OF ip_t_bal_msg.
      DATA lv_mess_text(200).
      DATA ls_text TYPE bapiret2.
      DATA lv_tm(12).
  
      IF NOT ip_t_bapiret2 IS  INITIAL.
  
        LOOP AT ip_t_bapiret2 INTO ls_bapiret2.
  
          CALL METHOD get_mess_text
            EXPORTING
              ip_s_bapiret2 = ls_bapiret2
            IMPORTING
              ep_text       = lv_mess_text.
  
          CASE ls_bapiret2-type.
            WHEN 'E'
              OR 'A'.
              lv_tm = 'HATA:'.
  
            WHEN 'W'.
              lv_tm = 'UYARI:'.
  
          ENDCASE.
  
          CONDENSE lv_mess_text.
  
          CONCATENATE
            lv_tm
            lv_mess_text
          INTO
            lv_mess_text
          SEPARATED BY space.
  
          WRITE :/ lv_mess_text.
  
          MOVE-CORRESPONDING ls_bapiret2 TO ls_text.
          ls_text-message = lv_mess_text.
          APPEND ls_text TO ep_t_text.
  
          CLEAR ls_bapiret2.
        ENDLOOP.
  
      ELSEIF NOT ip_t_bal_msg IS INITIAL.
  
        LOOP AT ip_t_bal_msg INTO ls_bal.
  
          CALL METHOD get_mess_text
            EXPORTING
              ip_s_bal_msg  = ls_bal
            IMPORTING
              ep_text       = lv_mess_text.
  
          CASE ls_bal-msgty.
            WHEN 'E'
              OR 'A'.
              lv_tm = 'HATA:'.
  
            WHEN 'W'.
              lv_tm = 'UYARI:'.
  
          ENDCASE.
  
          CONDENSE lv_mess_text.
  
          CONCATENATE
            lv_tm
            lv_mess_text
          INTO
            lv_mess_text
          SEPARATED BY space.
  
  
          WRITE :/ lv_mess_text.
  
          CALL METHOD convert_bal_to_bapiret
            EXPORTING
              ip_s_bal_msg  = ls_bal
            IMPORTING
              ep_s_bapiret2 = ls_text.
  
          ls_text-message = lv_mess_text.
          APPEND ls_text TO ep_t_text.
  
          CLEAR ls_bal.
        ENDLOOP.
  
      ENDIF.
  
  
    ENDMETHOD.                    "log_write_to_spool_static
  
    METHOD log_write_to_spool.
  
      DATA lt_mess TYPE bal_t_msg.
  
      CALL METHOD read_log
        IMPORTING
          ep_t_messages = lt_mess.
  
      CALL METHOD log_write_to_spool_static
        EXPORTING
          ip_t_bal_msg = lt_mess.
  
    ENDMETHOD.                    "log_write_to_spool
  
    METHOD  read_log.
  
      DATA : lt_log_handle TYPE bal_t_logh,
             lt_msg_handle TYPE  bal_t_msgh,
             lw_msg_handle LIKE LINE OF lt_msg_handle,
             ls_message    TYPE bal_s_msg.
  
      APPEND gv_log_handle TO lt_log_handle.
  
      CALL FUNCTION 'BAL_GLB_SEARCH_MSG'
        EXPORTING
  *       I_S_LOG_FILTER = I_S_LOG_FILTER
  *       I_T_LOG_CONTEXT_FILTER       = I_T_LOG_CONTEXT_FILTER
          i_t_log_handle = lt_log_handle
  *       I_S_MSG_FILTER = I_S_MSG_FILTER
  *       I_T_MSG_CONTEXT_FILTER       = I_T_MSG_CONTEXT_FILTER
  *       I_T_MSG_HANDLE = I_T_MSG_HANDLE
        IMPORTING
  *       E_T_LOG_HANDLE = E_T_LOG_HANDLE
          e_t_msg_handle = lt_msg_handle
        EXCEPTIONS
          msg_not_found  = 1
          OTHERS         = 2.
  
      IF sy-subrc <> 0.
  * MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
  *         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
  
      CLEAR lw_msg_handle.
      LOOP AT lt_msg_handle INTO lw_msg_handle.
  
        CALL FUNCTION 'BAL_LOG_MSG_READ'
          EXPORTING
            i_s_msg_handle = lw_msg_handle
          IMPORTING
            e_s_msg        = ls_message
          EXCEPTIONS
            log_not_found  = 1
                             clear
                             w_msg_handle.
  
        APPEND ls_message TO ep_t_messages.
  
      ENDLOOP.
  
    ENDMETHOD.                    "read_log
  
    METHOD convert_bapiret_to_bal.
  
      ep_s_bal_msg-msgty   = ip_s_bapiret2-type.
      ep_s_bal_msg-msgid   = ip_s_bapiret2-id.
      ep_s_bal_msg-msgno   = ip_s_bapiret2-number.
      ep_s_bal_msg-msgv1   = ip_s_bapiret2-message_v1.
      ep_s_bal_msg-msgv2   = ip_s_bapiret2-message_v2.
      ep_s_bal_msg-msgv3   = ip_s_bapiret2-message_v3.
      ep_s_bal_msg-msgv4   = ip_s_bapiret2-message_v4.
  
    ENDMETHOD.                    "convert_bapiret_to_bal
  
    METHOD convert_bal_to_bapiret.
  
      ep_s_bapiret2-type        = ip_s_bal_msg-msgty.
      ep_s_bapiret2-id          = ip_s_bal_msg-msgid.
      ep_s_bapiret2-number      = ip_s_bal_msg-msgno.
      ep_s_bapiret2-message_v1  = ip_s_bal_msg-msgv1.
      ep_s_bapiret2-message_v2  = ip_s_bal_msg-msgv2.
      ep_s_bapiret2-message_v3  = ip_s_bal_msg-msgv3.
      ep_s_bapiret2-message_v4  = ip_s_bal_msg-msgv4.
  
    ENDMETHOD.                    "convert_bal_to_bapiret
  
    METHOD error_mess_exists.
  
      IF ip_t_bapiret2 IS SUPPLIED.
  
        LOOP AT ip_t_bapiret2 TRANSPORTING NO FIELDS
        WHERE type = 'E'
           OR type = 'A'
           OR type = 'X'.
          EXIT.
        ENDLOOP.
  
        IF sy-subrc EQ 0.
          ep_return = 'X'.
        ELSE.
          ep_return = ''.
        ENDIF.
  
      ELSEIF ip_t_bal_msg IS SUPPLIED.
  
        LOOP AT ip_t_bal_msg TRANSPORTING NO FIELDS
        WHERE msgty = 'E'
           OR msgty = 'A'
           OR msgty = 'X'.
          EXIT.
        ENDLOOP.
  
        IF sy-subrc EQ 0.
          ep_return = 'X'.
        ELSE.
          ep_return = ''.
        ENDIF.
  
      ENDIF.
  
    ENDMETHOD.                    "error_mess_exists
  
    METHOD format_message.
  
      IF ip_free_text IS SUPPLIED
      AND NOT ip_free_text IS INITIAL.
  
        ep_s_bapiret2-id         = '00'.
        ep_s_bapiret2-number     = '398'.
        ep_s_bapiret2-type       = ip_msgty.
  
        WRITE :
        ip_free_text   TO ep_s_bapiret2-message_v1,
        ip_free_text2  TO ep_s_bapiret2-message_v2,
        ip_free_text3  TO ep_s_bapiret2-message_v3,
        ip_free_text4  TO ep_s_bapiret2-message_v4.
  
      ELSE.
  
        ep_s_bapiret2-id         = ip_msgid.
        ep_s_bapiret2-number     = ip_msgno.
        ep_s_bapiret2-type       = ip_msgty.
  
        WRITE :
        ip_msgv1 TO ep_s_bapiret2-message_v1,
        ip_msgv2 TO ep_s_bapiret2-message_v2,
        ip_msgv3 TO ep_s_bapiret2-message_v3,
        ip_msgv4 TO ep_s_bapiret2-message_v4.
  
      ENDIF.
  
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING :
          input         = ep_s_bapiret2-message_v1
        IMPORTING
          output        = ep_s_bapiret2-message_v1,
  
          input         = ep_s_bapiret2-message_v2
        IMPORTING
          output        = ep_s_bapiret2-message_v2,
  
          input         = ep_s_bapiret2-message_v3
        IMPORTING
          output        = ep_s_bapiret2-message_v3,
  
          input         = ep_s_bapiret2-message_v4
        IMPORTING
          output        = ep_s_bapiret2-message_v4.
  
  
      CONDENSE :
      ep_s_bapiret2-message_v1,
      ep_s_bapiret2-message_v2,
      ep_s_bapiret2-message_v3,
      ep_s_bapiret2-message_v4.
  
      CALL METHOD get_mess_text
        EXPORTING
          ip_s_bapiret2 = ep_s_bapiret2
        IMPORTING
          ep_text       = ep_s_bapiret2-message.
  
      "convert message to an anonym message
      " (in RFC calls , message is now shown if the
      "  local message class is not existing in
      "  remote client system ):
      IF ip_anon EQ 'X'.
        ep_s_bapiret2-id         = '00'.
        ep_s_bapiret2-number     = '398'.
  
        CLEAR :
          ep_s_bapiret2-message_v1,
          ep_s_bapiret2-message_v2,
          ep_s_bapiret2-message_v3,
          ep_s_bapiret2-message_v4.
  
        DATA lv_exit.
        DATA lv_numc1.
        DATA lv_fname(50).
  
        FIELD-SYMBOLS <f_mess_var> TYPE any.
  
        DO 4 TIMES.
  
          lv_numc1 = sy-index.
          CONCATENATE
            'EP_S_BAPIRET2-MESSAGE_V' lv_numc1
          INTO lv_fname.
  
          ASSIGN (lv_fname) TO <f_mess_var>.
  
          CALL METHOD get_fnc_from_message
            IMPORTING
              ep_result  = <f_mess_var>
              ep_exit    = lv_exit
            CHANGING
              cp_message = ep_s_bapiret2-message.
  
          IF lv_exit EQ 'X'.
            EXIT.
          ENDIF.
  
  
        ENDDO.
  
  
  
  
  
  
  
      ENDIF.
  
      IF ep_s_bal_msg IS REQUESTED.
  
        ep_s_bal_msg-msgid = ep_s_bapiret2-id.
        ep_s_bal_msg-msgno = ep_s_bapiret2-number.
        ep_s_bal_msg-msgty = ep_s_bapiret2-type.
        ep_s_bal_msg-msgv1 = ep_s_bapiret2-message_v1.
        ep_s_bal_msg-msgv2 = ep_s_bapiret2-message_v2.
        ep_s_bal_msg-msgv3 = ep_s_bapiret2-message_v3.
        ep_s_bal_msg-msgv4 = ep_s_bapiret2-message_v4.
  
      ENDIF.
  
    ENDMETHOD.                    "format_message
  
    METHOD get_fnc_from_message.
      "get first N characters from a message
  
  
      IF strlen( cp_message ) LE ip_char_count. "50
        ep_result  = cp_message.
        ep_exit    = 'X'.
      ELSE.
        ep_result = cp_message(ip_char_count).
        SHIFT cp_message LEFT BY ip_char_count PLACES.
        CONDENSE cp_message.
      ENDIF.
  
    ENDMETHOD.
  
    METHOD add_to_message_tab.
  
      DATA :
        lw_bapiret TYPE bapiret2,
        lw_balmsg  TYPE bal_s_msg.
  
      IF ip_s_bal_msg IS SUPPLIED.
  
        CHECK NOT ip_s_bal_msg IS INITIAL.
  
        CALL METHOD format_message
          EXPORTING
            ip_msgty      = ip_s_bal_msg-msgty
            ip_msgid      = ip_s_bal_msg-msgid
            ip_msgno      = ip_s_bal_msg-msgno
            ip_msgv1      = ip_s_bal_msg-msgv1
            ip_msgv2      = ip_s_bal_msg-msgv2
            ip_msgv3      = ip_s_bal_msg-msgv3
            ip_msgv4      = ip_s_bal_msg-msgv4
            ip_anon       = ip_anon
          IMPORTING
            ep_s_bapiret2 = lw_bapiret
            ep_s_bal_msg  = lw_balmsg.
  
      ELSEIF ip_s_bapiret2 IS SUPPLIED.
  
        CHECK NOT ip_s_bapiret2 IS INITIAL.
  
        CALL METHOD format_message
          EXPORTING
            ip_msgty      = ip_s_bapiret2-type
            ip_msgid      = ip_s_bapiret2-id
            ip_msgno      = ip_s_bapiret2-number
            ip_msgv1      = ip_s_bapiret2-message_v1
            ip_msgv2      = ip_s_bapiret2-message_v2
            ip_msgv3      = ip_s_bapiret2-message_v3
            ip_msgv4      = ip_s_bapiret2-message_v4
            ip_anon       = ip_anon
          IMPORTING
            ep_s_bapiret2 = lw_bapiret
            ep_s_bal_msg  = lw_balmsg.
  
      ELSE.
  
        CALL METHOD format_message
          EXPORTING
            ip_msgty      = ip_msgty
            ip_msgid      = ip_msgid
            ip_msgno      = ip_msgno
            ip_msgv1      = ip_msgv1
            ip_msgv2      = ip_msgv2
            ip_msgv3      = ip_msgv3
            ip_msgv4      = ip_msgv4
            ip_free_text  = ip_free_text
            ip_free_text2 = ip_free_text2
            ip_free_text3 = ip_free_text3
            ip_free_text4 = ip_free_text4
            ip_anon       = ip_anon
          IMPORTING
            ep_s_bapiret2 = lw_bapiret
            ep_s_bal_msg  = lw_balmsg.
  
      ENDIF.
  
      IF cp_t_bapiret2 IS SUPPLIED.
        COLLECT lw_bapiret INTO cp_t_bapiret2.
      ENDIF.
  
      IF cp_t_bal_msg IS SUPPLIED.
        APPEND lw_balmsg TO cp_t_bal_msg.
      ENDIF.
  
    ENDMETHOD.                    "add_to_message_tab
  
  
  ENDCLASS.                    "cl_appl_log_root IMPLEMENTATION
  
  
  *----------------------------------------------------------------------*
  *       CLASS cl_appl_log DEFINITION
  *----------------------------------------------------------------------*
  *
  *----------------------------------------------------------------------*
  CLASS cl_appl_log_dialog DEFINITION INHERITING FROM cl_appl_log_root.
    PUBLIC SECTION.
  
      DATA :
        gr_container      TYPE REF TO cl_gui_custom_container,
        gv_container_name TYPE scrfname,
  
        gv_control_handle TYPE  balcnthndl.
  
  
      METHODS :
        constructor
          IMPORTING
            ip_container_name TYPE c
            ip_objname        TYPE c OPTIONAL
            ip_sub_objname    TYPE c OPTIONAL,
  
        on_pbo
          IMPORTING ip_no_refresh TYPE checkbox DEFAULT '',
  
  
        on_pai,
  
        refresh_display.
  
  
  
  ENDCLASS.                    "cl_appl_log DEFINITION
  
  *----------------------------------------------------------------------*
  *       CLASS cl_appl_log IMPLEMENTATION
  *----------------------------------------------------------------------*
  *
  *----------------------------------------------------------------------*
  CLASS cl_appl_log_dialog IMPLEMENTATION.
  
    METHOD constructor.
  
      CALL METHOD super->constructor.
  
      " --1-- Create screen container :
      gv_container_name = ip_container_name.
      TRANSLATE gv_container_name TO UPPER CASE.
  
      " --2-- Create Log Oject  :
      CALL METHOD create_log_object
        EXPORTING
          ip_objname     = ip_objname
          ip_sub_objname = ip_sub_objname.
  
    ENDMETHOD.                    "constructor
  
  
    METHOD on_pbo.
  
      DATA lt_log_handle TYPE bal_t_logh.
  
      IF gv_control_handle IS INITIAL.
  
        CREATE OBJECT gr_container
          EXPORTING
            container_name = gv_container_name.
  
        CALL METHOD create_display_profile.
  
        APPEND gv_log_handle TO lt_log_handle.
  
        CALL FUNCTION 'BAL_CNTL_CREATE'
          EXPORTING
            i_container          = gr_container
            i_s_display_profile  = gs_display_profile
            i_t_log_handle       = lt_log_handle
          IMPORTING
            e_control_handle     = gv_control_handle
          EXCEPTIONS
            profile_inconsistent = 1
            internal_error       = 2
            OTHERS               = 3.
  
      ELSE.
  
        IF ip_no_refresh EQ ''.
  
          CALL METHOD refresh_display.
  
        ENDIF.
  
      ENDIF.
  
    ENDMETHOD.                    "on_pbo
  
    METHOD on_pai.
  
      CALL METHOD clear_log.
  
    ENDMETHOD.                    "on_pai
  
    METHOD refresh_display.
  
      DATA lt_log_handle TYPE bal_t_logh.
  
      CHECK NOT gv_control_handle IS INITIAL.
  
      APPEND gv_log_handle TO lt_log_handle.
  
      CALL FUNCTION 'BAL_CNTL_REFRESH'
        EXPORTING
          i_control_handle  = gv_control_handle
          i_t_log_handle    = lt_log_handle
        EXCEPTIONS
          control_not_found = 1
          internal_error    = 2
          OTHERS            = 3.
  
    ENDMETHOD.                    "refresh_display
  
  ENDCLASS.                    "cl_appl_log IMPLEMENTATION
  
  
  *----------------------------------------------------------------------*
  *       CLASS cl_appl_log_fullscreen DEFINITION
  *----------------------------------------------------------------------*
  *
  *----------------------------------------------------------------------*
  CLASS cl_appl_log_fullscreen DEFINITION
  INHERITING FROM cl_appl_log_root.
  
    PUBLIC SECTION.
  
      DATA gv_popup.
  
      METHODS :
        constructor
          IMPORTING
            ip_objname     TYPE c OPTIONAL
            ip_sub_objname TYPE c OPTIONAL,
  
        display.
  
  ENDCLASS.                    "cl_appl_log_fullscreen DEFINITION
  
  *----------------------------------------------------------------------*
  *       CLASS cl_appl_log_fullscreen IMPLEMENTATION
  *----------------------------------------------------------------------*
  *
  *----------------------------------------------------------------------*
  CLASS cl_appl_log_fullscreen IMPLEMENTATION.
  
    METHOD constructor.
  
      CALL METHOD super->constructor.
      " --- Create Log Oject  :
      CALL METHOD create_log_object
        EXPORTING
          ip_objname     = ip_objname
          ip_sub_objname = ip_sub_objname.
  
    ENDMETHOD.                    "constructor
  
    METHOD display.
  
      DATA  : lt_messages   TYPE bal_t_msg,
              lt_log_handle TYPE bal_t_logh,
              lv_gui.
  
      CALL FUNCTION 'GUI_IS_AVAILABLE'
        IMPORTING
          return = lv_gui.
  
      IF lv_gui IS INITIAL.
  
        CALL METHOD log_write_to_spool.
  
      ELSE.
  
        " Check if any message exists :
        CALL METHOD read_log
          IMPORTING
            ep_t_messages = lt_messages.
  
        CHECK NOT lt_messages IS INITIAL.
  
        "Get display profile
        CALL METHOD create_display_profile.
  
        IF gv_popup EQ 'X'.
  
          gs_display_profile-start_col  =  10.
          gs_display_profile-start_row  =  10.
          gs_display_profile-end_col    =  100.
          gs_display_profile-end_row    =  20.
  
        ENDIF.
  
        " Show in full screen :
        APPEND gv_log_handle TO lt_log_handle.
        CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
          EXPORTING
            i_t_log_handle      = lt_log_handle
            i_s_display_profile = gs_display_profile
          EXCEPTIONS
            OTHERS              = 1.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
  
  
      ENDIF.
  
    ENDMETHOD.                    "display
  
  ENDCLASS.                    "cl_appl_log_fullscreen IMPLEMENTATION