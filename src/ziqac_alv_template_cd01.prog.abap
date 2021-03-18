*&---------------------------------------------------------------------*
*&  Include           ZIQAC_ALV_TEMPLATE_CD01
*&---------------------------------------------------------------------*
* Class Definitions
* NB - The Clean Code guidelines recommend that all PUBLIC methods
* of classes, even local classes, be defined via interfaces
*----------------------------------------------------------------------*
INTERFACE lif_selections.

  DATA: s_bname TYPE RANGE OF usr05-bname,
        p_vari  TYPE disvariant-variant,
        p_send  TYPE char01,
        p_email TYPE ad_smtpadr.

ENDINTERFACE.

CLASS lcl_selections DEFINITION ##CLASS_FINAL.

  PUBLIC SECTION.

    INTERFACES lif_selections.

    ALIASES: s_bname      FOR lif_selections~s_bname,
             p_vari       FOR lif_selections~p_vari.

    METHODS: constructor IMPORTING
                           is_bname LIKE s_bname
                           ip_vari  LIKE p_vari.

ENDCLASS.

INTERFACE lif_persistency_layer.

  METHODS: derive_data RETURNING VALUE(rt_output_data) TYPE g_tt_alv_output.

ENDINTERFACE.
*----------------------------------------------------------------------*
*       CLASS lcl_persistency_layer DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_persistency_layer DEFINITION ##CLASS_FINAL.

  PUBLIC SECTION.
    INTERFACES lif_persistency_layer.

    ALIASES: derive_data FOR lif_persistency_layer~derive_data.

    METHODS: constructor IMPORTING io_selections TYPE REF TO lif_selections.

  PRIVATE SECTION.
    DATA: mo_selections TYPE REF TO lif_selections.

ENDCLASS.                    "lcl_persistency_layer DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_model DEFINITION
*----------------------------------------------------------------------*
INTERFACE lif_model.

  DATA: mo_selections  TYPE REF TO lif_selections,
        mo_pers_layer  TYPE REF TO lif_persistency_layer,
        mt_output_data TYPE g_tt_alv_output.

  METHODS:
    data_retrieval,
    prepare_data_for_output,
    user_command IMPORTING id_user_command TYPE sy-ucomm ##NEEDED
                           id_column       TYPE salv_de_column ##NEEDED
                           id_row          TYPE salv_de_row ##NEEDED.

  EVENTS: data_changed,
          output_message EXPORTING VALUE(user_message) TYPE string.

ENDINTERFACE.

CLASS lcl_model DEFINITION ##CLASS_FINAL.

  PUBLIC SECTION.

    INTERFACES: lif_model.

    ALIASES:
    mo_selections           FOR lif_model~mo_selections,
    mo_pers_layer           FOR lif_model~mo_pers_layer,
    mt_output_data          FOR lif_model~mt_output_data,
    data_retrieval          FOR lif_model~data_retrieval,
    prepare_data_for_output FOR lif_model~prepare_data_for_output,
    user_command            FOR lif_model~user_command,
    data_changed            FOR lif_model~data_changed,
    output_message          FOR lif_model~output_message.

    METHODS:
      constructor IMPORTING io_selections TYPE REF TO lif_selections.

  PRIVATE SECTION.

ENDCLASS.                    "lcl_model DEFINITION

*--------------------------------------------------------------------*
* VIEW definition
*--------------------------------------------------------------------*
INTERFACE lif_view.

    EVENTS user_command_received
      EXPORTING
       VALUE(ed_user_command) TYPE salv_de_function OPTIONAL
       VALUE(ed_row)          TYPE salv_de_row      OPTIONAL
       VALUE(ed_column)       TYPE salv_de_column   OPTIONAL.

    METHODS:
      initialize IMPORTING id_variant    TYPE disvariant-variant
                 CHANGING  ct_data_table TYPE ANY TABLE,
      application_specific_changes,
      display,
      output_message IMPORTING id_message TYPE string,
      "Respond to User Actions
      handle_link_click FOR EVENT link_click OF cl_salv_events_table
        IMPORTING
          row
          column,
      handle_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING
          e_salv_function.

    DATA: mo_alv_grid TYPE REF TO cl_salv_table.

ENDINTERFACE.

CLASS lcl_view DEFINITION ##CLASS_FINAL.
  PUBLIC SECTION.

    INTERFACES: lif_view.

    ALIASES:
    user_command_received        FOR lif_view~user_command_received,
    initialize                   FOR lif_view~initialize,
    application_specific_changes FOR lif_view~application_specific_changes,
    display                      FOR lif_view~display,
    output_message               FOR lif_view~output_message,
    handle_link_click            FOR lif_view~handle_link_click,
    handle_user_command          FOR lif_view~handle_user_command,
    mo_alv_grid                  FOR lif_view~mo_alv_grid.

  PRIVATE SECTION.
    METHODS:
      set_layout IMPORTING id_variant TYPE disvariant-variant,
      set_handlers.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_controller DEFINITION
*----------------------------------------------------------------------*
INTERFACE lif_controller.

    METHODS:
      on_user_command FOR EVENT user_command_received OF lcl_view
        IMPORTING ed_user_command
                  ed_row
                  ed_column,
      on_data_changed FOR EVENT data_changed OF lcl_model,
      on_message_output FOR EVENT output_message OF lcl_model
        IMPORTING user_message.

ENDINTERFACE.

CLASS lcl_controller DEFINITION ##CLASS_FINAL.

  PUBLIC SECTION.

    INTERFACES lif_controller.

    ALIASES:
    on_user_command   FOR lif_controller~on_user_command,
    on_data_changed   FOR lif_controller~on_data_changed,
    on_message_output FOR lif_controller~on_message_output.

    METHODS:
      constructor  IMPORTING io_model TYPE REF TO lcl_model
                             io_view  TYPE REF TO lcl_view.

  PRIVATE SECTION.
   DATA: mo_model TYPE REF TO lcl_model,
         mo_view  TYPE REF TO lcl_view.

ENDCLASS.                    "lcl_controller DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_application DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_application DEFINITION ##CLASS_FINAL.

  PUBLIC SECTION.
    CLASS-DATA: mo_model      TYPE REF TO lcl_model,
                mo_controller TYPE REF TO lcl_controller,
                mo_view       TYPE REF TO lcl_view.

    CLASS-METHODS:
      main       IMPORTING io_selections TYPE REF TO lif_selections,
      f4_layouts IMPORTING id_restrict TYPE salv_de_layout_restriction
                 CHANGING  cd_layout   TYPE disvariant-variant.

ENDCLASS.                    "lcl_application DEFINITION

INTERFACE lif_dependency_injector.

  METHODS inject_pers_layer
    IMPORTING
      io_pers_layer TYPE REF TO lif_persistency_layer.

ENDINTERFACE.

INTERFACE lif_dependency_factory.

  CLASS-METHODS get_instance
    RETURNING
      VALUE(ro_factory) TYPE REF TO lif_dependency_factory.
  METHODS get_pers_layer
    IMPORTING io_selections        TYPE REF TO lif_selections
    RETURNING VALUE(ro_pers_layer) TYPE REF TO lif_persistency_layer.

  DATA mo_pers_layer TYPE REF TO lif_persistency_layer.

ENDINTERFACE.

CLASS lcl_dependency_injector DEFINITION
  FINAL
  CREATE PUBLIC
  FOR TESTING.

  PUBLIC SECTION.

    INTERFACES lif_dependency_injector.

    ALIASES inject_pers_layer FOR lif_dependency_injector~inject_pers_layer.

    METHODS constructor.

  PRIVATE SECTION.

    DATA mo_factory TYPE REF TO lif_dependency_factory.

ENDCLASS.

CLASS lcl_dependency_factory DEFINITION FINAL.

  PUBLIC SECTION.

    INTERFACES lif_dependency_factory.

    ALIASES get_pers_layer FOR lif_dependency_factory~get_pers_layer.
    ALIASES get_instance   FOR lif_dependency_factory~get_instance.
    ALIASES mo_pers_layer  FOR lif_dependency_factory~mo_pers_layer.

    CLASS-DATA mo_factory TYPE REF TO lif_dependency_factory.

ENDCLASS.
