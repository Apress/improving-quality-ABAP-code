interface ZIF_DATABASE_READER
  public .


  methods DERIVE_CUSTOMER_DETAILS default ignore
    importing
      !ID_CUSTOMER_NUMBER type KUNAG
    returning
      value(RS_HEADER_DETAILS) type KNA1 .
endinterface.
