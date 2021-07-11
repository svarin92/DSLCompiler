unit DSLCompiler.Base;

interface

{$mode objfpc}{$H+}

uses
  
  // dsl compiler
  DSLCompiler.ErrorInfo;

type

  TVisitor = class;
  TSymbolizedObject = class;

  {: Abstract base class for all TCodegen objects. Implements IInterface and
     optional reference counting. Cloned from IndySoap's abstract base class. }
  TPragma = class(TObject, IInterface)
  private
    {$IFDEF REFERENCE_COUNTING}
    fRefCounting: boolean;
    fRefCount:    integer;
    {$ENDIF}
  protected
    {$IFDEF FPC_HAS_CONSTREF}
      function QueryInterface(
        constref IID: TGuid; 
        out obj
        ): LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
      function _AddRef: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
      function _Release: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    {$ELSE}
      function QueryInterface(
        const IID: tguid; 
        out obj
        ): longint; stdcall;
      function _AddRef: longint; stdcall;
      function _Release: longint; stdcall;
    {$ENDIF}
  public
    {$IFDEF REFERENCE_COUNTING}
    constructor CreateWithRefCounting;
  //
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    {$ENDIF}
  end; { TPragma }

  TPragmaClass = class of TPragma;

  {: Class that does the visiting. }
  TVisitor = class(TPragma)
  protected
    function AcceptVisitor(const Visited: TSymbolizedObject): Boolean; virtual; abstract;
      // Visit only if Visited is a given business object. 
  public
    procedure Execute(const Visited: TSymbolizedObject); virtual; abstract;
      // Visit business objects.
  end; { TInterfacedVisitor }

  {: A class reference for holding TInterfacedVisitor }
  TVisitorClass = class of TVisitor ;

  {: TSymbolizedObject implements the Iterate method, which will pass a
     TVisitor over every node in the graph of objects. }
  TSymbolizedObject = class(TInterfacedObject)
  public
    procedure Iterate(const Visitor: TVisitor); virtual;
      // Accept visitor.
  end; { TSymbolizedObject }

  {: A class reference for holding TSymbolizedObject }
  TVisitedClass = class of TSymbolizedObject;

  TCompilerBase = class(TInterfacedObject, IErrorInfo)
  strict private
    fLastError: string;
  protected
    function SetError(const Error: string): Boolean;
  //
    property __LastError: string 
      read fLastError 
      write fLastError;
  public
    function ErrorInfo: string;
  end; { TCompilerBase }

implementation

{$IFDEF MSWINDOWS}
uses

  Windows;
{$ENDIF}

{ TPragma }

{$IFDEF FPC_HAS_CONSTREF}
function TPragma.QueryInterface(
  constref IID: TGuid; 
  out obj
  ): LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
{$ELSE}
function TPragma.QueryInterface(
  const IID: tguid; 
  out obj
  ): longint; stdcall;
{$ENDIF}
 begin
  if
    GetInterface(IID, obj)
  then
    Result := 0
  else
    Result := E_NOINTERFACE;
end; { TPragma.QueryInterface }

{$IFDEF FPC_HAS_CONSTREF}
function TPragma._AddRef: longint; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
{$ELSE}
function TPragma._AddRef: longint; stdcall;
{$ENDIF}
begin
  {$IFDEF REFERENCE_COUNTING}
  Result := InterlockedIncrement(fRefCount);
  {$else}
  Result := 0;
  {$ENDIF}
end; { TPragma._AddRef }

{$IFDEF FPC_HAS_CONSTREF}
function TPragma._Release: longint; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
{$ELSE}
function TPragma._Release: longint; stdcall;
{$ENDIF}
begin
  {$IFDEF REFERENCE_COUNTING}
  Result := InterlockedDecrement(fRefCount);

  if
    fRefCounting
  then
    if Result = 0 then Destroy;
  {$else}
  Result := 0;
  {$ENDIF}
end; { TPragma._Release }

{$IFDEF REFERENCE_COUNTING}
constructor TPragma.CreateWithRefCounting;
begin
  Create;
  fRefCounting := True;
end; { TPragma.CreateWithRefCounting }
{$ENDIF}

{$IFDEF REFERENCE_COUNTING}
procedure TPragma.AfterConstruction;
begin
  inherited AfterConstruction;

  // Release the constructor's implicit refcount
  if fRefCounting then InterlockedDecrement(fRefCount);
end; { TPragma.AfterConstruction }
{$ENDIF}

{$IFDEF REFERENCE_COUNTING}
procedure TPragma.BeforeDestruction;
begin
  if
    fRefCounting
  then
    if fRefCount <> 0 then System.Error(reInvalidPtr);

  inherited BeforeDestruction;
end; { TPragma.BeforeDestruction }

class function TPragma.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TPragma(Result).fRefCount := 1;
end; { TPragma.NewInstance }
{$ENDIF}

{ TSymbolizedObject }

procedure TSymbolizedObject.Iterate(const Visitor: TVisitor);
begin
  Visitor.Execute(Self);
end; { TSymbolizedObject.Iterate }

{ TCompilerBase }

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
