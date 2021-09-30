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