unit DSLCompiler.Tokenizer;

interface

{$mode objfpc}{$H+}

uses

  // rtl, fcl, lcl
  Classes,
  Types;

type

  TTokenKind = (tkUnknown, tkWhitespace,
                tkIdent, tkNumber,
                tkLeftParen, tkRightParen,
                tkLeftCurly, tkRightCurly,
                tkLeftSquare, tkRightSquare,
                tkLessThan, tkPlus, tkMinus,
                tkComma, tkSemicolon,
                tkEOF);

  ITokenizer = interface 
  ['{086E9EFE-DB1E-4D81-A16A-C9F1F0F06D2B}']
    function  CurrentLocation: TPoint;
    function  GetToken(
      var kind: TTokenKind; 
      var identifier: string
      ): Boolean;
    procedure Initialize(const Code: string);
    function  IsAtEnd: Boolean;
  end; { ITokenizer }

  TTokenizerFactory = function: ITokenizer;

function CreateTokenizer: ITokenizer;

implementation

uses
  
  // rtl, fcl, lcl
  SysUtils,
  Character,

  // dsl compiler
  DSLCompiler.Base;  

type

  TTokenizer = class(TCompilerBase, ITokenizer)
  strict private
    fCurrentLine: string;
    fLastLine   : Integer;
    fLastLineLen: Integer;
    fLookahead  : Char;
    fNextChar   : Integer;
    fNextLine   : Integer;
    fProgram    : TStringList;
  strict protected
    procedure PushBack(ch: Char);
    procedure SkipWhitespace;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function  CurrentLocation: TPoint; inline;
    function  GetChar(var ch: Char): Boolean;
    function  GetIdent: string;
    function  GetNumber: string;
    function  GetToken(
      var kind: TTokenKind; 
      var identifier: string
      ): Boolean;
    procedure Initialize(const Code: string);
    function  IsAtEnd: Boolean;
  end; { TTokenizer }

{ exports }

function CreateTokenizer: ITokenizer;
begin
  Result := TTokenizer.Create;
end; { CreateTokenizer }

{ TTokenizer }

procedure TTokenizer.AfterConstruction;
begin
  inherited;
  fProgram := TStringList.Create;
end; { TTokenizer.AfterConstruction }

procedure TTokenizer.BeforeDestruction;
begin
  FreeAndNil(fProgram);
  inherited;
end; { TTokenizer.BeforeDestruction }

function TTokenizer.CurrentLocation: TPoint;
begin
  Result := Point(fNextLine + 1, fNextChar);
end; { TTokenizer.CurrentLocation }

function TTokenizer.GetChar(var ch: Char): Boolean;
begin
  if 
    fLookahead <> #0 
  then begin
    ch := fLookahead;
    fLookahead := #0;
    Result := True;
  end else begin
    Result := not IsAtEnd;
    
    if 
      Result 
    then begin
      ch := fCurrentLine[fNextChar];
      Inc(fNextChar);
      
      if 
        fNextChar > Length(fCurrentLine) 
      then begin
        Inc(fNextLine);

        if 
          fNextLine < fProgram.Count 
        then
          fCurrentLine := fProgram[fNextLine];

        fNextChar := 1;
      end;
    end;
  end;
end; { TTokenizer.GetChar }

function TTokenizer.GetIdent: string;
var
  _ch: Char;
begin
  Result := '';

  while 
    GetChar(_ch) 
  do begin
    if 
      TCharacter.IsLetter(_ch) 
        or TCharacter.IsNumber(_ch) 
        or (_ch = '_')
    then
      Result := Result + _ch
    else begin
      PushBack(_ch);
      Exit;
    end;
  end;
end; { TTokenizer.GetIdent }

function TTokenizer.GetNumber: string;
var
  _ch: Char;
begin
  Result := '';

  while 
    GetChar(_ch) 
  do begin
    if 
      CharInSet(_ch, ['0'..'9']) 
    then
      Result := Result + _ch
    else begin
      PushBack(_ch);
      Exit;
    end;
  end;
end; { TTokenizer.GetNumber }

function TTokenizer.GetToken(
  var kind: TTokenKind; 
  var identifier: string
  ): Boolean;
var
  _ch: Char;
begin
  identifier := '';
  Result := GetChar(_ch);
  
  if 
    not Result 
  then begin
    kind := tkEOF;
    Exit;
  end;
  
  case 
    _ch 
  of
    '(': kind := tkLeftParen;
    ')': kind := tkRightParen;
    '{': kind := tkLeftCurly;
    '}': kind := tkRightCurly;
    '[': kind := tkLeftSquare;
    ']': kind := tkRightSquare;
    '+': kind := tkPlus;
    '-': kind := tkMinus;
    '<': kind := tkLessThan;
    ',': kind := tkComma;
    ';': kind := tkSemicolon;
    else if
        TCharacter.IsLetter(_ch)
      then begin
        kind := tkIdent;
        identifier := _ch + GetIdent;
      end else if
          CharInSet(_ch, ['0'..'9']) 
        then begin
          kind := tkNumber;
          identifier := _ch + GetNumber;
        end else if
            TCharacter.IsWhiteSpace(_ch)
          then begin
            kind := tkWhitespace;
            SkipWhitespace;
          end else
            kind := tkUnknown;
  end;
end; { TTokenizer.GetToken }

procedure TTokenizer.Initialize(const Code: string);
begin
  fProgram.Text := Code;
  fNextLine := 0;
  fNextChar := 1;
  fLookahead := #0;
  fLastLine := fProgram.Count - 1;
  
  if 
    fLastLine >= 0 
  then begin
    fLastLineLen := Length(fProgram[fLastLine]);
    fCurrentLine := fProgram[fNextLine];
  end;
end; { TTokenizer.Initialize }

function TTokenizer.IsAtEnd: Boolean;
begin
  Result := (fNextLine > fLastLine) 
              or ((fNextLine = fLastLine) 
                    and (fNextChar > (fLastLineLen+1)));
end; { TTokenizer.IsAtEnd }

procedure TTokenizer.PushBack(ch: Char);
begin
  Assert(fLookahead = #0, 'TTokenizer: Lookahead buffer is not empty');
  fLookahead := ch;
end; { TTokenizer.PushBack }

procedure TTokenizer.SkipWhitespace;
var
  _ch: Char;
begin
  while 
    GetChar(_ch) 
  do
    if 
      not TCharacter.IsWhiteSpace(_ch)
    then begin
      PushBack(_ch);
      Exit;
    end;
end; { TTokenizer.SkipWhitespace }

end.
