class ZCL_DEPENDENCY_INJECTOR definition
  public
  final
  create public
  for testing .

public section.

  interfaces ZIF_DEPENDENCY_INJECTOR .

  aliases INJECT_DATABASE_READER
    for ZIF_DEPENDENCY_INJECTOR~INJECT_DATABASE_READER .

  methods CONSTRUCTOR .
protected section.
private section.

  data MO_FACTORY type ref to ZCL_DEPENDENCY_FACTORY .
ENDCLASS.



CLASS ZCL_DEPENDENCY_INJECTOR IMPLEMENTATION.


  METHOD constructor.
*--------------------------------------------------------------------*
* Listing 2.4: Constructor of the Injector Class
*--------------------------------------------------------------------*
    CLEAR zcl_dependency_factory=>mo_factory.

    mo_factory = zcl_dependency_factory=>get_instance( ).

  ENDMETHOD.


  METHOD zif_dependency_injector~inject_database_reader.
*--------------------------------------------------------------------*
* Listing 2.5: Injector Method
*--------------------------------------------------------------------*
    mo_factory->mo_database_reader = io_database_reader.

  ENDMETHOD.
ENDCLASS.
