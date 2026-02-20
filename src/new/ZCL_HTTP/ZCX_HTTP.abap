CLASS zcx_http DEFINITION
  INHERITING FROM cx_static_check
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES if_t100_message.

    ALIASES default_textid
      FOR if_t100_message~default_textid.
    ALIASES t100key
      FOR if_t100_message~t100key.

    DATA msgty TYPE symsgty.
    DATA msgv1 TYPE symsgv.
    DATA msgv2 TYPE symsgv.
    DATA msgv3 TYPE symsgv.
    DATA msgv4 TYPE symsgv.

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !msgty    TYPE symsgty OPTIONAL
        !msgv1    TYPE symsgv OPTIONAL
        !msgv2    TYPE symsgv OPTIONAL
        !msgv3    TYPE symsgv OPTIONAL
        !msgv4    TYPE symsgv OPTIONAL.

    METHODS get_msg
      RETURNING
        VALUE(rt_msg) TYPE bapiret2_tab.

ENDCLASS.


CLASS zcx_http IMPLEMENTATION.

  METHOD constructor.
    super->constructor( previous = previous ).

    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

    me->msgty = msgty.
    me->msgv1 = msgv1.
    me->msgv2 = msgv2.
    me->msgv3 = msgv3.
    me->msgv4 = msgv4.
  ENDMETHOD.


  METHOD get_msg.

    DATA ls_msg LIKE LINE OF rt_msg.

    MESSAGE ID me->t100key-msgid TYPE me->msgty NUMBER me->t100key-msgno
      WITH me->msgv1 me->msgv2 me->msgv3 me->msgv4
      INTO ls_msg-message.

    ls_msg-type       = me->msgty.
    ls_msg-id         = me->t100key-msgid.
    ls_msg-number     = me->t100key-msgno.
    ls_msg-message_v1 = me->msgv1.
    ls_msg-message_v2 = me->msgv2.
    ls_msg-message_v3 = me->msgv3.
    ls_msg-message_v4 = me->msgv4.
    APPEND ls_msg TO rt_msg.

  ENDMETHOD.

ENDCLASS.