class ZCL_DATABASE_READER definition
  public
  final
  create private

  global friends ZCL_DEPENDENCY_FACTORY .

public section.

  interfaces ZIF_DATABASE_READER .
protected section.
private section.
ENDCLASS.



CLASS ZCL_DATABASE_READER IMPLEMENTATION.


  METHOD zif_database_reader~derive_customer_details.

* KNA1 - Primary Key = KUNNR
    SELECT SINGLE *
      FROM kna1
      INTO CORRESPONDING FIELDS OF rs_header_details
      WHERE kunnr EQ id_customer_number.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
