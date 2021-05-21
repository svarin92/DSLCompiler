unit DSLCompiler.Runnable;

interface

{$mode objfpc}{$H+}

uses

  // rtl, fcl, lcl
  SysUtils;

type

  TParameters = specialize TArray<Integer>;

  IProgram = interface 
  ['{2B93BEE7-EF20-41F4-B599-4C28131D6655}']
    function Call(
      const FunctionName: string; 
      const Params: TParameters; 
      var return: Integer
      ): Boolean;
  end; { IProgram }

implementation

end.
