DATA: lo_type TYPE REF TO cl_abap_structdescr,
      lt_comp_head TYPE abap_component_tab,
      ls_comp like LINE OF lt_comp_head.

lo_type ?= cl_abap_typedescr=>describe_by_data( ls_data ).
lt_comp_head = lo_type->get_components( ).

LOOP AT lt_comp_head INTO ls_comp.
    IF ls_comp-type IS INSTANCE OF CL_ABAP_ELEMDESCR.
        WRITE:/ ls_comp-name, '1'.
    ELSE.
        WRITE:/ ls_comp-name, '0'.
    ENDIF.
ENDLOOP.