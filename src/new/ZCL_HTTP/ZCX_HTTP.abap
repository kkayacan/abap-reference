class ZCX_HTTP definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  aliases DEFAULT_TEXTID
    for IF_T100_MESSAGE~DEFAULT_TEXTID .
  aliases MSGTY
    for IF_T100_DYN_MSG~MSGTY .
  aliases MSGV1
    for IF_T100_DYN_MSG~MSGV1 .
  aliases MSGV2
    for IF_T100_DYN_MSG~MSGV2 .
  aliases MSGV3
    for IF_T100_DYN_MSG~MSGV3 .
  aliases MSGV4
    for IF_T100_DYN_MSG~MSGV4 .
  aliases T100KEY
    for IF_T100_MESSAGE~T100KEY .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGTY type SYMSGTY optional
      !MSGV1 type SYMSGV optional
      !MSGV2 type SYMSGV optional
      !MSGV3 type SYMSGV optional
      !MSGV4 type SYMSGV optional .
  methods GET_MSG
    returning
      value(RT_MSG) type BAPIRET2_TAB .

METHOD get_msg.

  MESSAGE ID me->t100key-msgid TYPE me->msgty NUMBER me->t100key-msgno
  WITH me->msgv1 me->msgv2 me->msgv3 me->msgv4
  INTO DATA(lv_message).

  APPEND VALUE #( type       = me->msgty
                  id         = me->t100key-msgid
                  number     = me->t100key-msgno
                  message_v1 = me->msgv1
                  message_v2 = me->msgv2
                  message_v3 = me->msgv3
                  message_v4 = me->msgv4
                  message    = lv_message ) TO rt_msg.

ENDMETHOD.