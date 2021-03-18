class ZCL_DEPENDENCY_FACTORY definition
  public
  final
  create private

  global friends ZCL_DEPENDENCY_INJECTOR .

public section.

  interfaces ZIF_DEPENDENCY_FACTORY .

  aliases GET_DATABASE_READER
    for ZIF_DEPENDENCY_FACTORY~GET_DATABASE_READER .
  aliases GET_INSTANCE
    for ZIF_DEPENDENCY_FACTORY~GET_INSTANCE .
protected section.
private section.

  class-data MO_FACTORY type ref to ZCL_DEPENDENCY_FACTORY .
  data MO_DATABASE_READER type ref to ZIF_DATABASE_READER .
ENDCLASS.



CLASS ZCL_DEPENDENCY_FACTORY IMPLEMENTATION.


  METHOD zif_dependency_factory~get_database_reader.
*--------------------------------------------------------------------*
* Listing 2.3: Factory Method of factory class that returns an
* instance of a helper class
* In a unit test the MO_DATABASE_READER already exists because it
* has been "injected" with a test double
*--------------------------------------------------------------------*
    IF mo_database_reader IS NOT BOUND.
      CREATE OBJECT mo_database_reader TYPE zcl_database_reader.
    ENDIF.

    ro_database_reader = mo_database_reader.

  ENDMETHOD.


  METHOD zif_dependency_factory~get_instance.
*--------------------------------------------------------------------*
* Listing 2.2: Factory Method for a Factory Class!
*--------------------------------------------------------------------*
    IF mo_factory IS NOT BOUND.
      CREATE OBJECT mo_factory.
    ENDIF.

    ro_factory = mo_factory.

  ENDMETHOD.
ENDCLASS.
