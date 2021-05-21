unit DSLCompiler.Base;

interface

{$mode objfpc}{$H+}

uses
  
  // dsl compiler
  DSLCompiler.ErrorInfo;

type

  TCompilerBase = class(TInterfacedObject, IErrorInfo)
  strict private
    fLastError: string;
  protected
    function  SetError(const Error: string): Boolean;
  //
    property __LastError: string 
      read fLastError 
      write fLastError;
  public
    function  ErrorInfo: string;
  end; { TCompilerBase }

implementation

function TCompilerBase.ErrorInfo: string;
begin
  Result := fLastError;
end; { TCompilerBase.ErrorInfo }

function TCompilerBase.SetError(const Error: string): Boolean;
begin
  fLastError := Error;
  Result := False;
end; { TCompilerBase.SetError }

end.
