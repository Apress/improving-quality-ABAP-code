*&---------------------------------------------------------------------*
*&  Include           ZIQAC_TEMPLATE_CIO1
*&---------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.

  METHOD main.

    mo_model      = NEW #( io_selections = io_selections ).
    mo_view       = NEW #( ).
    mo_controller = NEW #( io_model = mo_model
                           io_view  = mo_view ).

    mo_model->data_retrieval( ).
    mo_model->prepare_data_for_output( ).
    mo_view->initialize( EXPORTING id_variant    = io_selections->p_vari
                         CHANGING  ct_data_table = mo_model->mt_output_data[] ).
    mo_view->application_specific_changes( ).
    mo_view->display( ).

  ENDMETHOD.                                               "main

  METHOD f4_layouts.

    DATA(ls_key)    = VALUE salv_s_layout_key( report = sy-repid ).
    DATA(ls_layout) = cl_salv_layout_service=>f4_layouts( s_key = ls_key
                                                          layout   = cd_layout
                                                          restrict = id_restrict ).

    cd_layout = ls_layout-layout.

  ENDMETHOD.

ENDCLASS.                    "lcl_application IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_selections IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_selections IMPLEMENTATION.

  METHOD constructor.

    s_bname = is_bname.
    p_vari  = ip_vari.

  ENDMETHOD.                                               "constructor

ENDCLASS."Local Selections

*----------------------------------------------------------------------*
*       CLASS lcl_view IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_view IMPLEMENTATION.

  METHOD initialize.

    TRY.

        cl_salv_table=>factory(
              IMPORTING r_salv_table = mo_alv_grid
              CHANGING  t_table      = ct_data_table[] ).

        "Display Basic Toolbar
        mo_alv_grid->get_functions( )->set_all( if_salv_c_bool_sap=>true ).

        set_layout( id_variant ).
        set_handlers( ).

      CATCH cx_salv_msg INTO DATA(lo_salv_msg).
        DATA(ls_error) = lo_salv_msg->get_message( ).
        MESSAGE ID ls_error-msgid TYPE 'E' NUMBER ls_error-msgno
                WITH ls_error-msgv1 ls_error-msgv2
                     ls_error-msgv3 ls_error-msgv4.
    ENDTRY.

  ENDMETHOD.

  METHOD application_specific_changes.
* Local Variables
    DATA: lo_column TYPE REF TO cl_salv_column_table.

    TRY.
        "Optimize Column Widths
        DATA(lo_columns) = mo_alv_grid->get_columns( ).
        lo_columns->set_optimize( if_salv_c_bool_sap=>true ).

        "Set hotspots, add long texts and other "FIELDCAT" type tasks
        lo_column ?= lo_columns->get_column( 'BNAME' ).
        lo_column->set_cell_type( value = if_salv_c_cell_type=>hotspot ).

        "Any errors here are down to spelling mistakes in the code and thus should cause a hard
        "error to ensure the program never makes it to QA until all such errors are fixed
      CATCH cx_salv_not_found INTO DATA(lo_not_found).
        DATA(ls_error) = lo_not_found->get_message( ).
        MESSAGE ID ls_error-msgid TYPE 'E' NUMBER ls_error-msgno
                WITH ls_error-msgv1 ls_error-msgv2
                     ls_error-msgv3 ls_error-msgv4.
      CATCH cx_salv_data_error INTO DATA(lo_data_error).
        ls_error = lo_data_error->get_message( ).
        MESSAGE ID ls_error-msgid TYPE 'E' NUMBER ls_error-msgno
                WITH ls_error-msgv1 ls_error-msgv2
                     ls_error-msgv3 ls_error-msgv4.
      CATCH cx_salv_msg INTO DATA(lo_error).
        ls_error = lo_error->get_message( ).
        MESSAGE ID ls_error-msgid TYPE 'E' NUMBER ls_error-msgno
                WITH ls_error-msgv1 ls_error-msgv2
                     ls_error-msgv3 ls_error-msgv4.
    ENDTRY.

  ENDMETHOD.                                               "application_specific_changes

  METHOD display.

    mo_alv_grid->display( ).

  ENDMETHOD.

  METHOD output_message.

    IF sy-batch IS INITIAL.
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          text = id_message.
    ELSE.
      MESSAGE id_message TYPE 'S'.
    ENDIF.

  ENDMETHOD.

  METHOD set_layout.
* Local Variables
    DATA: ls_key TYPE salv_s_layout_key.

    DATA(lo_layout) = mo_alv_grid->get_layout( ).

    ls_key-report = sy-cprog.

    lo_layout->set_key( ls_key ).

    lo_layout->set_default( 'X' ).

    IF id_variant IS NOT INITIAL.
      lo_layout->set_initial_layout( id_variant ).
    ENDIF.

    lo_layout->set_save_restriction( if_salv_c_layout=>restrict_user_dependant ).

  ENDMETHOD.

  METHOD set_handlers.
*--------------------------------------------------------------------*
* Here you direct any user commands (actions) that the standard
* CL_SALV_TABLE triggers to methods which will pass those commands
* from the view to the controller. It is not the job of the view to
* respond to user commands directly
*--------------------------------------------------------------------*
    DATA(lo_events) = mo_alv_grid->get_event( ).

    SET HANDLER handle_link_click   FOR lo_events.

    SET HANDLER handle_user_command FOR lo_events.

  ENDMETHOD.

  METHOD handle_link_click.

    RAISE EVENT user_command_received EXPORTING ed_user_command = gc_drill_down     "Double Click
                                                ed_row          = row
                                                ed_column       = column.

  ENDMETHOD.


  METHOD handle_user_command.

    RAISE EVENT user_command_received EXPORTING ed_user_command = e_salv_function.

  ENDMETHOD.

ENDCLASS.                    "lcl_view IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS lcl_persistency_layer IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_persistency_layer IMPLEMENTATION.

  METHOD constructor.

    mo_selections = io_selections.

  ENDMETHOD.

  METHOD derive_data.

* USR05 - Generic Buffering - Primary Key = BNAME / PARID
    SELECT *
      FROM usr05
      INTO CORRESPONDING FIELDS OF TABLE rt_output_data
      WHERE bname IN mo_selections->s_bname
      ORDER BY PRIMARY KEY."#EC CI_GENBUFF

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

  ENDMETHOD.                                               "derive_data

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_model IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_model IMPLEMENTATION.

  METHOD constructor.

    mo_selections = io_selections.

    "Based on the selction criteria e.g. country a different concrete class could be returned
    mo_pers_layer = lcl_dependency_factory=>get_instance( )->get_pers_layer( mo_selections ).

  ENDMETHOD.                                               "constructor

  METHOD data_retrieval.

    mt_output_data[] = mo_pers_layer->derive_data( ).

  ENDMETHOD.                                               "data_retrieval

*--------------------------------------------------------------------*
* METHOD prepare_data_for_output
*--------------------------------------------------------------------*
* Initially this method will be empty. After a year it will contain
* a billion lines of conditional logic. The idea is to add all that
* logic in a TDD manner via a series of methods that do one thing
* only
*--------------------------------------------------------------------*
  METHOD prepare_data_for_output ##needed.

  ENDMETHOD.                                               "prepare_data_for_output

  METHOD user_command ##NEEDED.
*--------------------------------------------------------------------*
* User commands that the model react to generally update the database
* If that happens then the DATA_CHANGED event is raised which tells
* the controller to tell the view to display the changed data
*--------------------------------------------------------------------*

  ENDMETHOD."User Command / Model

ENDCLASS.                    "lcl_model IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_controller IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_controller IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).

    mo_model = io_model.
    mo_view  = io_view.

    "Make the controller react to the views events
    SET HANDLER on_user_command FOR mo_view.
    "If the model changes the data, the controller needs to tell the view
    SET HANDLER on_data_changed FOR mo_model.
    "The model cannot output messages itself
    SET HANDLER on_message_output FOR mo_model.

  ENDMETHOD.                                               "constructor

  METHOD on_user_command.
*--------------------------------------------------------------------*
* IMPORTING ed_user_command
*           ed_row
*           ed_column.
*--------------------------------------------------------------------*
* The controller evaluates the user command first to see if any
* sort of screen navigation is required, and then when finished
* the command is passed to the model in case any sort of database
* update is needed
*--------------------------------------------------------------------*

    CASE ed_user_command.
      WHEN gc_drill_down.
        READ TABLE mo_model->mt_output_data
        ASSIGNING FIELD-SYMBOL(<ls_output>) INDEX ed_row.
        IF sy-subrc NE 0.
          RETURN.
        ENDIF.
        CASE ed_column.
          WHEN 'BNAME'.
            "Drill Down to Display User Details
            IF <ls_output>-bname IS INITIAL.
              RETURN.
            ENDIF.
            SET PARAMETER ID 'XUS' FIELD <ls_output>-bname.
            "The "skip first screen" does not work for SU01 but does for most transactios!
            CALL TRANSACTION 'SU01D' AND SKIP FIRST SCREEN."#EC CI_CALLTA
            RETURN.
          WHEN OTHERS.
            RETURN.
        ENDCASE."What column was selected for drill down?
    ENDCASE."What user command was chosen?

    mo_model->user_command(
         id_user_command = ed_user_command
         id_column       = ed_column
         id_row          = ed_row ).

  ENDMETHOD."User Command / Controller

  METHOD on_data_changed.

    mo_view->mo_alv_grid->refresh( ).

  ENDMETHOD.                                               "on_data_changed

  METHOD on_message_output.
*--------------------------------------------------------------------*
* FOR EVENT output_message OF lcl_model
* IMPORTING user_message.
*--------------------------------------------------------------------*

    mo_view->output_message( user_message ).

  ENDMETHOD.

ENDCLASS.                    "lcl_controller IMPLEMENTATION

CLASS lcl_dependency_factory IMPLEMENTATION.

  METHOD lif_dependency_factory~get_pers_layer.

    IF mo_pers_layer IS NOT BOUND.
      mo_pers_layer = NEW lcl_persistency_layer( io_selections ).
    ENDIF.

    ro_pers_layer = mo_pers_layer.

  ENDMETHOD.

  METHOD lif_dependency_factory~get_instance.

    IF mo_factory IS NOT BOUND.
      CREATE OBJECT mo_factory TYPE lcl_dependency_factory.
    ENDIF.

    ro_factory = mo_factory.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_dependency_injector IMPLEMENTATION.

  METHOD constructor.

    CLEAR lcl_dependency_factory=>mo_factory.

    mo_factory = lcl_dependency_factory=>get_instance( ).

  ENDMETHOD.

  METHOD lif_dependency_injector~inject_pers_layer.

    mo_factory->mo_pers_layer = io_pers_layer.

  ENDMETHOD.

ENDCLASS.
