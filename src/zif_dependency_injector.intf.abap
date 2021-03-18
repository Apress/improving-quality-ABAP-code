interface ZIF_DEPENDENCY_INJECTOR
  public .


  methods INJECT_DATABASE_READER
    importing
      !IO_DATABASE_READER type ref to ZIF_DATABASE_READER .
endinterface.
