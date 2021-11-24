METHOD storeproducts_get_entityset.

    DATA(lt_filter_select_options) = io_tech_request_context->get_filter( )->get_filter_select_options( ).
    READ TABLE lt_filter_select_options INTO DATA(ls_so) WITH KEY property = 'DUMMY_STR_PROD_STOCK_INCL'.
    IF sy-subrc = 0.
      IF ls_so-select_options IS NOT INITIAL.
        zcl_rtst_op_storeproduct=>set_apptype( CONV char1( ls_so-select_options[ 1 ]-low ) ).
      ENDIF.
    ENDIF.

    CLEAR ls_so.
    READ TABLE lt_filter_select_options INTO ls_so WITH KEY property = 'DUMMY_STORE_PRODUCT_INCL'.

    IF ls_so IS NOT INITIAL.
      DATA lt_entityset TYPE zcl_zretailstore_order_mpc=>tt_storeproduct.
      zcl_rtst_op_storeproduct=>set_ignore_paging( abap_true ).
      zcl_rtst_op_storeproduct=>get_instance( )->get_entityset(
        EXPORTING
          io_tech_request_context = io_tech_request_context
          io_message_container    = /iwbep/if_mgw_conv_srv_runtime~get_message_container( )
        IMPORTING
          et_entityset            = lt_entityset
          es_response_context     = es_response_context ).

      DATA(lv_released) = abap_false.
      LOOP AT lt_entityset ASSIGNING FIELD-SYMBOL(<ls_entity>) WHERE order_document_id IS NOT INITIAL.
        SELECT SINGLE eban~frgkz, t16fs~frgc1
           FROM eban
           JOIN t16fs ON t16fs~frggr = eban~frggr
                     AND t16fs~frgsx = eban~frgst
          WHERE eban~banfn = @<ls_entity>-order_document_id
            AND eban~bnfpo = @<ls_entity>-order_document_item_id
          INTO @DATA(ls_eban).
        IF sy-subrc = 0 AND ls_eban-frgkz <> '2'.
          CALL FUNCTION 'BAPI_REQUISITION_RELEASE'
            EXPORTING
              number                 = <ls_entity>-order_document_id
              rel_code               = ls_eban-frgc1
              item                   = CONV bapi2009ob-preq_item( <ls_entity>-order_document_item_id )
            EXCEPTIONS
              authority_check_fail   = 1
              requisition_not_found  = 2
              enqueue_fail           = 3
              prerequisite_fail      = 4
              release_already_posted = 5
              responsibility_fail    = 6
              OTHERS                 = 7.
          IF sy-subrc <> 0.
            me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( )->add_message(
              iv_msg_type               = /iwbep/cl_cos_logger=>error
              iv_msg_id                 = sy-msgid
              iv_msg_number             = sy-msgno
              iv_msg_v1                 = sy-msgv1
              iv_msg_v2                 = sy-msgv2
              iv_msg_v3                 = sy-msgv3
              iv_msg_v4                 = sy-msgv4
              iv_add_to_response_header = abap_true
              iv_message_target         = CONV string( <ls_entity>-product_name ) ).
            EXIT.
          ELSEIF lv_released = abap_false.
            me->/iwbep/if_mgw_conv_srv_runtime~get_message_container( )->add_message(
              iv_msg_type               = /iwbep/cl_cos_logger=>success
              iv_msg_id                 = 'SV'
              iv_msg_number             = '018'
              iv_add_to_response_header = abap_true ).
          ENDIF.
        ENDIF.
      ENDLOOP.

      zcl_rtst_op_storeproduct=>set_ignore_paging( abap_false ).
    ENDIF.

    zcl_rtst_op_storeproduct=>get_instance( )->get_entityset(
      EXPORTING
        io_tech_request_context = io_tech_request_context
        io_message_container    = /iwbep/if_mgw_conv_srv_runtime~get_message_container( )
      IMPORTING
        et_entityset            = et_entityset
        es_response_context     = es_response_context ).

  ENDMETHOD.