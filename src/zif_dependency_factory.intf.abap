interface ZIF_DEPENDENCY_FACTORY
  public .


  class-methods GET_INSTANCE
    returning
      value(RO_FACTORY) type ref to ZCL_DEPENDENCY_FACTORY .
  methods GET_DATABASE_READER
    returning
      value(RO_DATABASE_READER) type ref to ZIF_DATABASE_READER .
endinterface.
