    data lo_entity_type type ref to /iwbep/if_mgw_odata_entity_typ.
    data lo_property type ref to /iwbep/if_mgw_odata_property.

*   entity PostDocumentItem
    lo_entity_type = model->get_entity_type( iv_entity_name = cl_rtst_rcv_product_mpc=>gc_postdocumentitem ).

*   add properties
    lo_property = lo_entity_type->create_property( iv_property_name = 'ExpectedQuantity' iv_abap_fieldname = 'EXPECTED_QUANTITY' ). "#EC NOTEXT
    lo_property->set_type_edm_decimal( ).
    lo_property->set_precison( iv_precision = 3 ).          "#EC NOTEXT
    lo_property->set_maxlength( iv_max_length = 13 ).       "#EC NOTEXT
    lo_property->set_creatable( abap_true ).
    lo_property->set_updatable( abap_false ).
    lo_property->set_sortable( abap_false ).
    lo_property->set_nullable( abap_false ).
    lo_property->set_filterable( abap_false ).

    lo_property = lo_entity_type->create_property( iv_property_name = 'ExpectedQuantityUnitCode' iv_abap_fieldname = 'EXPECTED_QUANTITY_UNIT_CODE' ). "#EC NOTEXT
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 3 ).        "#EC NOTEXT
    lo_property->set_conversion_exit( 'CUNIT' ).            "#EC NOTEXT
    lo_property->set_semantic( 'unit-of-measure' ).         "#EC NOTEXT
    lo_property->set_creatable( abap_true ).
    lo_property->set_updatable( abap_false ).
    lo_property->set_sortable( abap_false ).
    lo_property->set_nullable( abap_false ).
    lo_property->set_filterable( abap_false ).

    lo_property = lo_entity_type->create_property( iv_property_name = 'ExpectedQuantityUnitName' iv_abap_fieldname = 'EXPECTED_QUANTITY_UNIT_NAME' ). "#EC NOTEXT
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 10 ).       "#EC NOTEXT
    lo_property->set_creatable( abap_true ).
    lo_property->set_updatable( abap_false ).
    lo_property->set_sortable( abap_false ).
    lo_property->set_nullable( abap_false ).
    lo_property->set_filterable( abap_false ).

    lo_property = lo_entity_type->create_property( iv_property_name = 'ExpectedQuantityBase' iv_abap_fieldname = 'EXPECTED_QUANTITY_BASE' ). "#EC NOTEXT
    lo_property->set_type_edm_decimal( ).
    lo_property->set_precison( iv_precision = 3 ).          "#EC NOTEXT
    lo_property->set_maxlength( iv_max_length = 13 ).       "#EC NOTEXT
    lo_property->set_creatable( abap_false ).
    lo_property->set_updatable( abap_true ).
    lo_property->set_sortable( abap_false ).
    lo_property->set_nullable( abap_false ).
    lo_property->set_filterable( abap_false ).