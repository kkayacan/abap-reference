method define.

    super->define( ).

    data(lo_action) = model->get_action( 'EmptyProductTransferHandlingUnit' ). "#EC NOTEXT
    lo_action->set_http_method( /iwbep/if_mgw_med_odata_types=>gcs_med_http_methods-put ).

    lo_action = model->get_action( 'CloseProductTransferHandlingUnit' ). "#EC NOTEXT
    lo_action->set_http_method( /iwbep/if_mgw_med_odata_types=>gcs_med_http_methods-put ).

    lo_action = model->get_action( 'SubmitProductTransfer' ). "#EC NOTEXT
    lo_action->set_http_method( /iwbep/if_mgw_med_odata_types=>gcs_med_http_methods-put ).

    lo_action = model->get_action( 'OpenProductTransferHandlingUnit' ). "#EC NOTEXT
    lo_action->set_http_method( /iwbep/if_mgw_med_odata_types=>gcs_med_http_methods-put ).

    me->modify_product_transfer( ).
*    me->modify_settings( ).
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""$"$\SE:(1) Class CL_RTST_TRNSFR_PRODUCT_MPC_EXT, Method DEFINE, End                                                                                          A
*$*$-Start: (1)---------------------------------------------------------------------------------$*$*
ENHANCEMENT 1  ZRTST_TRNSFR_PROD_MPC_DEFINE.    "active version
  lo_action = model->get_action( iv_action_name = 'SubmitProductTransfer' ).
  DATA(lo_param) = lo_action->CREATE_INPUT_PARAMETER( IV_PARAMETER_NAME = 'AppType' IV_ABAP_FIELDNAME = 'APP_TYPE' ).
  lo_param->/IWBEP/IF_MGW_ODATA_PROPERTY~set_type_edm_string( ).
  lo_param->/IWBEP/IF_MGW_ODATA_PROPERTY~set_maxlength( iv_max_length = 10 ). "#EC NOTEXT
  lo_param->/IWBEP/IF_MGW_ODATA_PROPERTY~set_nullable( iv_nullable = abap_true ).
ENDENHANCEMENT.
*$*$-End:   (1)---------------------------------------------------------------------------------$*$*
  endmethod.