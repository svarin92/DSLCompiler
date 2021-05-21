unit DSLCompiler.System;

interface

{$mode objfpc}{$H+}

type

  TDSLObject = class(TObject, IInterface)
  private
    {$IFDEF REFERENCE_COUNTING}
    fRefCounting: boolean;
    fRefCount:    integer;
    {$ENDIF}
  protected
    {$IFDEF FPC_HAS_CONSTREF}
      function QueryInterface(constref IID: TGuid; out obj): LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
      function _AddRef: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
      function _Release: LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    {$ELSE}
      function QueryInterface(const IID: tguid; out obj): longint; stdcall;
      function _AddRef: longint; stdcall;
      function _Release: longint; stdcall;
    {$ENDIF}
  public
    {$IFDEF REFERENCE_COUNTING}
    constructor CreateWithRefCounting;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    {$ENDIF}
  end; { TDSLObject }

  TDSLObjectClass = class of TDSLObject;

implementation

{$IFDEF MSWINDOWS}
uses
  Windows;
{$ENDIF}

{$IFDEF FPC_HAS_CONSTREF}
function TDSLObject.QueryInterface(constref IID: TGuid; out obj): LongInt; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
{$ELSE}
function TDSLObject.QueryInterface(const IID: tguid; out obj): longint; stdcall;
{$ENDIF}
 begin
  if
    GetInterface(IID, obj)
  then
    Result := 0
  else
    Result := E_NOINTERFACE;
end; { TDSLObject.QueryInterface }

{$IFDEF FPC_HAS_CONSTREF}
function TDSLObject._AddRef: longint; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
{$ELSE}
function TDSLObject._AddRef: longint; stdcall;
{$ENDIF}
begin
  {$IFDEF REFERENCE_COUNTING}
  Result := InterlockedIncrement(fRefCount);
  {$else}
  Result := 0;
  {$ENDIF}
end; { TDSLObject._AddRef }

{$IFDEF FPC_HAS_CONSTREF}
function TDSLObject._Release: longint; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
{$ELSE}
function TDSLObject._Release: longint; stdcall;
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
end; { TDSLObject._Release }

{$IFDEF REFERENCE_COUNTING}
constructor TDSLObject.CreateWithRefCounting;
begin
  Create;
  fRefCounting := True;
end; { TDSLObject.CreateWithRefCounting }
{$ENDIF}

{$IFDEF REFERENCE_COUNTING}
procedure TDSLObject.AfterConstruction;
begin
  inherited AfterConstruction;

  // Release the constructor's implicit refcount
  if fRefCounting then InterlockedDecrement(fRefCount);
end; { TDSLObject.AfterConstruction }
{$ENDIF}

{$IFDEF REFERENCE_COUNTING}
procedure TDSLObject.BeforeDestruction;
begin
  if
    fRefCounting
  then
    if fRefCount <> 0 then System.Error(reInvalidPtr);

  inherited BeforeDestruction;
end; { TDSLObject.BeforeDestruction }

class function TDSLObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TDSLObject(Result).fRefCount := 1;
end; { TDSLObject.NewInstance }
{$ENDIF}

end.
