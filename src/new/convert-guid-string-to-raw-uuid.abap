DATA: lv_offset TYPE i,
lv_guid   TYPE c LENGTH 36,
lv_sourceuuid TYPE SYSUUID_X.

lv_guid = <ls_line>-value+lv_offset(36).
cl_gdt_conversion=>guid_inbound(
  EXPORTING
    im_value  = lv_guid
  IMPORTING
    ex_guid_x = lv_sourceuuid ).
  SELECT SINGLE @abap_true FROM /oaa/src_capa_w
    WHERE sourceuuid = @lv_sourceuuid
    INTO @DATA(lv_exists).