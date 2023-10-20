TYPES: BEGIN OF lst_schema,
         column_name              TYPE string,
         data_type                TYPE string,
         character_maximum_length TYPE i,
         numeric_precision        TYPE i,
         datetime_precision       TYPE i,
         is_nullable              TYPE string,
       END OF lst_schema.
DATA: ls_schema TYPE lst_schema,
      lt_schema TYPE TABLE OF lst_schema.

DATA: lv_query TYPE string.

lv_query = `SELECT COLUMN_NAME, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH, NUMERIC_PRECISION, DATETIME_PRECISION, ` &&
           `IS_NULLABLE  FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = N'crm_customer_vw'`.

DATA(o_conn) = cl_sql_connection=>get_connection( CONV dbcon_name( 'VERIPARK' ) ).
DATA(lo_statement) = o_conn->create_statement( ).
DATA(lo_result) = lo_statement->execute_query( lv_query ).
lo_result->set_param_struct( REF #( ls_schema ) ).
WHILE lo_result->next( ) > 0.
*  WRITE:/ ls_schema-column_name.
  APPEND ls_schema TO lt_schema.
ENDWHILE.
lo_result->close( ).
cl_demo_output=>display( lt_schema ).