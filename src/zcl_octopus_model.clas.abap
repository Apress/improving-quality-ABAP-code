class ZCL_OCTOPUS_MODEL definition
  public
  create public .

public section.
protected section.
private section.

  data CUSTOMER_ACCOUNT_GROUP type KTOKD .
  data CUSTOMER_NUMBER type KUNAG .

  methods PROCESS_MAIN_SCREEN .
ENDCLASS.



CLASS ZCL_OCTOPUS_MODEL IMPLEMENTATION.


  METHOD process_main_screen.
*--------------------------------------------------------------------*
* Listing 2.16: The Actual Production Code
* Here we are demonstrating how production code gets instances of
* specialized helper classes via the dependency factory
* In real life such helper classes would most likely be member variabkes
* and the factory would create them in the CONSTRUCTOR
*--------------------------------------------------------------------*
    DATA(lo_factory) = zcl_dependency_factory=>get_instance( ).

    DATA(lo_pers_layer) = lo_factory->get_database_reader( ).

    customer_account_group =
    lo_pers_layer->derive_customer_details( customer_number )-ktokd.

  ENDMETHOD.
ENDCLASS.
