FUNCTION ZIQAC_TABLEKEY_EDITOR_EXIT .
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      BUFFER TYPE  RSWSOURCET
*"  EXCEPTIONS
*"      CANCELLED
*"--------------------------------------------------------------------
* Listing 5.22: Function to insert comment describing key fields of a database table
*--------------------------------------------------------------------*
* Local Variables
  DATA: lt_sval   TYPE STANDARD TABLE OF sval  WITH HEADER LINE,
        lt_dd03l  TYPE STANDARD TABLE OF dd03l WITH HEADER LINE,
        ls_dd09l  TYPE dd09l,
        ld_string TYPE string.

  lt_sval-tabname   = 'DD02L'.
  lt_sval-fieldname = 'TABNAME'.
  APPEND lt_sval.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = 'Enter Table Name'
    TABLES
      fields          = lt_sval
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  READ TABLE lt_sval INDEX 1.

  CHECK lt_sval-value IS NOT INITIAL.

  SELECT SINGLE * FROM dd09l
    INTO ls_dd09l
    WHERE tabname EQ lt_sval-value.

  CHECK sy-subrc = 0.

  CONCATENATE '*' lt_sval-value INTO buffer SEPARATED BY space.

  IF ls_dd09l-bufallow = 'X'."Buffering Switched On
    CASE ls_dd09l-pufferung.
      WHEN 'X'.
        ld_string = 'Fully Buffered'.
      WHEN 'P'.
        ld_string = 'Single Record Buffering'.
      WHEN 'G'.
        ld_string = 'Generic Buffering'.
      WHEN OTHERS.
* Should never happen
    ENDCASE.
  ENDIF.

  IF ld_string IS NOT INITIAL.
    CONCATENATE buffer ld_string INTO buffer SEPARATED BY ' - '.
  ENDIF.

  SELECT * FROM dd03l
    INTO CORRESPONDING FIELDS OF TABLE lt_dd03l
    WHERE tabname   EQ lt_sval-value
    AND   keyflag   EQ 'X'
    AND   fieldname NE 'MANDT'.

  SORT lt_dd03l BY position.

  IF lt_dd03l[] IS NOT INITIAL.
    CONCATENATE buffer 'Primary Key =' INTO buffer SEPARATED BY ' - '.
  ENDIF.

  LOOP AT lt_dd03l.
    IF sy-tabix = 1.
      CONCATENATE buffer lt_dd03l-fieldname INTO buffer SEPARATED BY space.
    ELSE.
      CONCATENATE buffer lt_dd03l-fieldname INTO buffer SEPARATED BY ' / '.
    ENDIF.
  ENDLOOP.

  APPEND buffer.

ENDFUNCTION.
