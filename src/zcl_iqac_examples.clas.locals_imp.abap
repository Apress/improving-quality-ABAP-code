*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_elephant DEFINITION.
  PUBLIC SECTION.
    METHODS: derive_trunk_size IMPORTING id_elephant_number   TYPE i
                               RETURNING VALUE(rd_trunk_size) TYPE i,
             derive_run_away   IMPORTING id_elephant_name     TYPE string
                                         id_circus_name       TYPE string
                               RETURNING VALUE(rf_result)     TYPE abap_bool.

ENDCLASS.

CLASS lcl_elephant IMPLEMENTATION.

  METHOD derive_trunk_size.

  ENDMETHOD.

  METHOD derive_run_away.

  ENDMETHOD.

ENDCLASS.

CLASS lcx_lightning_strike DEFINITION INHERITING FROM cx_no_check.

ENDCLASS.
