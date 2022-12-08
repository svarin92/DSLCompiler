unit DSLCompiler.Thing;

interface

{$mode objfpc}{$H+}

type

  {: Abstract base class for all TCodegen objects. Implements IInterface
     and optional reference counting. Cloned from IndySoap's abstract
     base class. }
  TThing = class(TObject, IInterface)
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
    //
      class function NewInstance: TObject; override;
    {$ENDIF}
  end; { TThing }

  TThingClass = class of TThing;

implementation

{$IFDEF MSWINDOWS}
uses
  
  // rtl, fcl, lcl
  Windows;
{$ENDIF}

{ TThing }

{$IFDEF FPC_HAS_CONSTREF}
function TThing.QueryInterface(
  constref IID: TGuid; 
  out obj
  ): LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
{$ELSE}
function TThing.QueryInterface(
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
end; { TThing.QueryInterface }

{$IFDEF FPC_HAS_CONSTREF}
function TThing._AddRef: longint; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
{$ELSE}
function TThing._AddRef: longint; stdcall;
{$ENDIF}
begin
  {$IFDEF REFERENCE_COUNTING}
  Result := InterlockedIncrement(fRefCount);
  {$ELSE}
  Result := 0;
  {$ENDIF}
end; { TThing._AddRef }

{$IFDEF FPC_HAS_CONSTREF}
function TThing._Release: longint; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
{$ELSE}
function TThing._Release: longint; stdcall;
{$ENDIF}
begin
  {$IFDEF REFERENCE_COUNTING}
  Result := InterlockedDecrement(fRefCount);

  if
    fRefCounting
  then
    if Result = 0 then Destroy;
  {$ELSE}
  Result := 0;
  {$ENDIF}
end; { TThing._Release }

{$IFDEF REFERENCE_COUNTING}
constructor TThing.CreateWithRefCounting;
begin
  Create;
  fRefCounting := True;
end; { TThing.CreateWithRefCounting }
{$ENDIF}

{$IFDEF REFERENCE_COUNTING}
procedure TThing.AfterConstruction;
begin
  inherited AfterConstruction;

  if fRefCounting then InterlockedDecrement(fRefCount);
    // Release the constructor's implicit refcount.
end; { TThing.AfterConstruction }
{$ENDIF}

{$IFDEF REFERENCE_COUNTING}
procedure TThing.BeforeDestruction;
begin
  if
    fRefCounting
  then
    if fRefCount <> 0 then System.Error(reInvalidPtr);

  inherited BeforeDestruction;
end; { TThing.BeforeDestruction }

class function TThing.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TThing(Result).fRefCount := 1;
end; { TThing.NewInstance }
{$ENDIF}

end.
