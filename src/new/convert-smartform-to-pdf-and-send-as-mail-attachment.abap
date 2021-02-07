FORM run.

  DATA: fmname          TYPE rs38l_fnam,
        ls_output       TYPE ssfcrescl,
        lt_otf          TYPE TABLE OF itcoo,
        lv_size         TYPE i,
        ls_info         TYPE zhrs_adese_card,
        lv_transfer_bin TYPE sx_boolean,
        lt_content_txt  TYPE soli_tab,
        lt_content_hex  TYPE solix_tab,
        lt_objhead      TYPE soli_tab,
        lv_len          TYPE so_obj_len,
        lv_subject      TYPE so_obj_des,
        lt_text         TYPE soli_tab,
        lv_html         TYPE string.

        CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
          EXPORTING
            formname           = 'ZHRF_ADESE_CARD'
          IMPORTING
            fm_name            = fmname
          EXCEPTIONS
            no_form            = 1
            no_function_module = 2
            OTHERS             = 3.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        CALL FUNCTION fmname
          EXPORTING
            control_parameters = VALUE ssfctrlop( no_dialog = abap_true
                                                  getotf    = abap_true )
            output_options     = VALUE ssfcompop( tddest = 'PDF1' )
            user_settings      = abap_false
            ls_info            = ls_info
          IMPORTING
            job_output_info    = ls_output
          EXCEPTIONS
            formatting_error   = 1
            internal_error     = 2
            send_error         = 3
            user_canceled      = 4
            OTHERS             = 5.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        LOOP AT ls_output-otfdata ASSIGNING FIELD-SYMBOL(<ls_otfdata>).
          APPEND INITIAL LINE TO lt_content_txt ASSIGNING FIELD-SYMBOL(<ls_content_txt>).
          <ls_content_txt>-line = <ls_otfdata>.
        ENDLOOP.

        CALL FUNCTION 'SX_OBJECT_CONVERT_OTF_PDF'
          EXPORTING
            format_src      = 'OTF'
            format_dst      = 'PDF'
          CHANGING
            transfer_bin    = lv_transfer_bin
            content_txt     = lt_content_txt
            content_bin     = lt_content_hex
            objhead         = lt_objhead
            len             = lv_len
          EXCEPTIONS
            err_conv_failed = 1
            OTHERS          = 2.

        lv_subject = TEXT-t01.
        lv_html = '<html><body>' && condense( |{ CONV string( p_pernr ) ALPHA = OUT }| )
               && ` ` && TEXT-t03 && '</body></html>'.

  lt_text = cl_document_bcs=>string_to_soli( lv_html ).

  DATA(lv_sent_to_all) = zcl_bcs=>send(
      it_recipients  = VALUE #( ( type = 'C' address = 'ZADESECARD' ) )
      iv_subject     = lv_subject
      it_body        = lt_text
      it_attach      = VALUE #( ( type = 'PDF' subject = 'TalepForm' size = lv_len hex = lt_content_hex ) ) ).

ENDFORM.



class ZCL_BCS definition
  public
  create public .

public section.

  types:
    BEGIN OF st_attach,
        type    TYPE soodk-objtp,
        subject TYPE sood-objdes,
        size    TYPE sood-objlen,
        text    TYPE soli_tab,
        hex     TYPE solix_tab,
      END OF st_attach .
  types:
    tt_attach TYPE TABLE OF st_attach .
  types:
    BEGIN OF st_sender,
        type    TYPE soos1-recesc,
        address TYPE adr6-smtp_addr,
      END OF st_sender .
  types:
    BEGIN OF st_recipient,
             type       TYPE soos1-recesc,
             address    TYPE adr6-smtp_addr,
             copy       TYPE so_snd_cp,
             blind_copy TYPE so_snd_bc,
           END OF st_recipient .
  types:
    tt_recipients TYPE TABLE OF st_recipient .

  class-methods SEND
    importing
      !IS_SENDER type ST_SENDER optional
      !IT_RECIPIENTS type TT_RECIPIENTS
      !IV_SUBJECT type SOOD-OBJDES
      !IT_BODY type BCSY_TEXT
      !IT_ATTACH type TT_ATTACH optional
      !IV_COMMIT type OS_BOOLEAN default ABAP_TRUE
    returning
      value(RV_SENT_TO_ALL) type OS_BOOLEAN .
protected section.
private section.
ENDCLASS.



CLASS ZCL_BCS IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_BCS=>SEND
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_SENDER                      TYPE        ST_SENDER(optional)
* | [--->] IT_RECIPIENTS                  TYPE        TT_RECIPIENTS
* | [--->] IV_SUBJECT                     TYPE        SOOD-OBJDES
* | [--->] IT_BODY                        TYPE        BCSY_TEXT
* | [--->] IT_ATTACH                      TYPE        TT_ATTACH(optional)
* | [--->] IV_COMMIT                      TYPE        OS_BOOLEAN (default =ABAP_TRUE)
* | [<-()] RV_SENT_TO_ALL                 TYPE        OS_BOOLEAN
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD send.

  DATA: lo_sender    TYPE REF TO if_sender_bcs,
        lo_recipient TYPE REF TO if_recipient_bcs.

* * document
  TRY.
      DATA(lo_doc) = cl_document_bcs=>create_from_text( i_text         = it_body
                                                        i_documenttype = 'HTM'
                                                        i_subject      = iv_subject ).
    CATCH cx_bcs INTO DATA(lx_bcs).
      RETURN.
  ENDTRY.

* * attachment
  LOOP AT it_attach ASSIGNING FIELD-SYMBOL(<ls_attach>).
    TRY.
        lo_doc->add_attachment( i_attachment_type    = <ls_attach>-type
                                i_attachment_subject = <ls_attach>-subject
                                i_attachment_size    = <ls_attach>-size
                                i_att_content_text   = <ls_attach>-text
                                i_att_content_hex    = <ls_attach>-hex ).
      CATCH cx_bcs INTO lx_bcs.
        RETURN.
    ENDTRY.
  ENDLOOP.

* create persistent send request
  TRY.
      DATA(lo_bcs) = cl_bcs=>create_persistent( ).
    CATCH cx_bcs INTO lx_bcs.
      RETURN.
  ENDTRY.

* add document to send request
  TRY.
      lo_bcs->send_request->setu_document( lo_doc ).
    CATCH cx_bcs INTO lx_bcs.
      RETURN.
  ENDTRY.

* set priority of send request
  TRY.
      lo_bcs->send_request->setu_priority( '9' ).
    CATCH cx_bcs INTO lx_bcs.
  ENDTRY.

* set outbox flag
  TRY.
      lo_bcs->send_request->set_link_to_outbox( abap_true ).
    CATCH cx_bcs INTO lx_bcs.
  ENDTRY.

* request error status
  TRY.
      lo_bcs->send_request->setu_requested_status( 'A' ).
    CATCH cx_bcs INTO lx_bcs.
  ENDTRY.

* mail for which status?
  TRY.
      lo_bcs->send_request->setu_status_mail( 'E' ).
    CATCH cx_bcs INTO lx_bcs.
  ENDTRY.

* set sender
* set reply recipient
* set status recipient
  CASE is_sender-type.
    WHEN 'B'.
      TRY.
          lo_sender    ?= cl_sapuser_bcs=>create( CONV uname( is_sender-address ) ).
          lo_recipient ?= cl_sapuser_bcs=>create( CONV uname( is_sender-address ) ).
        CATCH cx_bcs INTO lx_bcs.
          RETURN.
      ENDTRY.
    WHEN 'U'.
      TRY.
          lo_sender    ?= cl_cam_address_bcs=>create_internet_address( is_sender-address ).
          lo_recipient ?= cl_cam_address_bcs=>create_internet_address( is_sender-address ).
        CATCH cx_bcs INTO lx_bcs.
          RETURN.
      ENDTRY.
  ENDCASE.

  TRY.
      lo_bcs->send_request->setu_sender( lo_sender ).
      lo_bcs->send_request->setu_reply_to( lo_recipient ).
      lo_bcs->send_request->setu_status_recipient( lo_recipient ).
    CATCH cx_bcs INTO lx_bcs.
      RETURN.
  ENDTRY.

* * add recipients
  LOOP AT it_recipients ASSIGNING FIELD-SYMBOL(<ls_recipient>).
    CASE <ls_recipient>-type.
* * 1. SAP user
      WHEN 'B'.
        TRY.
            lo_recipient ?= cl_sapuser_bcs=>create( CONV uname( <ls_recipient>-address ) ).
          CATCH cx_bcs INTO lx_bcs.
            RETURN.
        ENDTRY.
* * 2. distribution list
      WHEN 'C' OR 'P'.
        TRY.
            lo_recipient ?= cl_distributionlist_bcs=>getu_persistent(
              i_dliname = CONV so_obj_nam( <ls_recipient>-address )
              i_private = SWITCH #( <ls_recipient>-type WHEN 'P' THEN abap_true ELSE abap_false ) ).
          CATCH cx_bcs INTO lx_bcs.
            RETURN.
        ENDTRY.
* * 3. direct internet address
      WHEN 'U'.
        TRY.
            lo_recipient ?= cl_cam_address_bcs=>create_internet_address( <ls_recipient>-address ).
          CATCH cx_bcs INTO lx_bcs.
            RETURN.
        ENDTRY.
    ENDCASE.
    TRY.
        lo_bcs->send_request->add_recipient( i_recipient  = lo_recipient
                                             i_express    = abap_false
                                             i_copy       = <ls_recipient>-copy
                                             i_blind_copy = <ls_recipient>-blind_copy ).
      CATCH cx_bcs INTO lx_bcs.
        RETURN.
    ENDTRY.
  ENDLOOP.

* release send request (will be submitted automatically)
  TRY.
      lo_bcs->send_without_dialog(
        IMPORTING
          e_sent_to_all              = rv_sent_to_all
          e_recipients_with_error    = DATA(lt_recipients_with_error)
          e_orig_recs_with_error     = DATA(lt_orig_recs_with_error) ).
    CATCH cx_bcs INTO lx_bcs.
      RETURN.
  ENDTRY.

  IF iv_commit = abap_true.
    COMMIT WORK.
  ENDIF.

ENDMETHOD.
ENDCLASS.