unit DSLCompiler.Parser;

interface

{$mode objfpc}{$H+}

uses

  // dsl compiler
  DSLCompiler.AST,
  DSLCompiler.Tokenizer;

type

  IParser = interface 
  ['{73F3CBB3-3DEF-4573-B079-7EFB00631560}']
    function Parse(
      const Code: string; 
      const Tokenizer: ITokenizer;
      const AST: IAST
      ): Boolean;
  end; { IParser }

  TParserFactory = function: IParser;

function CreateParser: IParser;

implementation

uses

  // rtl, fcl, lcl
  Types,
  SysUtils,

  // dsl compiler
  DSLCompiler.Base;

type

  TTokenKinds = set of TTokenKind;

  TParser = class(TCompilerBase, IParser)
  strict private type
    TContext = record
      _CurrentFunc   : IASTFunction;
    end; (* TContext *)
  var
    fAST           : IAST;
    fContext       : TContext;
    fTokenizer     : ITokenizer;
    fLookaheadToken: TTokenKind;
    fLookaheadIdent: string;
  strict protected
    function FetchToken(
      allowed: TTokenKinds; 
      var ident: string; 
      var token: TTokenKind
      ): Boolean; overload;
    function FetchToken(
      allowed: TTokenKinds; 
      var ident: string
      ): Boolean; overload; inline;
    function FetchToken(allowed: TTokenKinds): Boolean; overload; inline;
    function GetToken(
      var token: TTokenKind; 
      var ident: string
      ): Boolean;
    function IsFunction(
      const Ident: string; 
      var funcIdx: Integer
      ): Boolean;
    function IsVariable(
      const Ident: string; 
      var varIdx: Integer
      ): Boolean;
    function ParseBlock(var block: IASTBlock): Boolean;
    function ParseExpression(var expression: IASTExpression): Boolean;
    function ParseExpresionList(parameters: TExpressionList): Boolean;
    function ParseFunction: Boolean;
    function ParseReturn(var statement: IASTStatement): Boolean;
    function ParseIf(var statement: IASTStatement): Boolean;
    function ParseStatement(var statement: IASTStatement): Boolean;
    function ParseTerm(var term: IASTTerm): Boolean;
    procedure PushBack(
      token: TTokenKind; 
      const Ident: string
      );
  //
    property __AST: IAST
      read fAST;
  public
    function Parse(
      const Code: string; 
      const Tokenizer: ITokenizer;
      const AST: IAST
      ): Boolean;
  end; { TParser }

{ exports }

function CreateParser: IParser;
begin
  Result := TParser.Create;
end; { CreateParser }

{ TParser }

function TParser.FetchToken(
  allowed: TTokenKinds; 
  var ident: string;
  var token: TTokenKind
  ): Boolean;
var
  _loc: TPoint;
begin
  Result := False;

  while 
    GetToken(token, ident) 
  do
    if 
      token in allowed 
    then
      Exit(True)
    else if 
        token = tkWhitespace 
      then
        // do nothing
      else begin
        _loc := fTokenizer.CurrentLocation;
        __LastError := Format(
                         'Invalid syntax in line %d, character %d', 
                         [_loc.X, _loc.Y]
                         );
        Exit;
      end;
end; { TParser.FetchToken }

function TParser.FetchToken(
  allowed: TTokenKinds; 
  var ident: string
  ): Boolean;
var
  _token: TTokenKind;
begin
  Result := FetchToken(allowed, ident, _token);
end; { TParser.FetchToken }

function TParser.FetchToken(allowed: TTokenKinds): Boolean;
var
  _ident: string;
begin
  Result := FetchToken(allowed, _ident);
end; { TParser.FetchToken }

function TParser.GetToken(
  var token: TTokenKind; 
  var ident: string
  ): Boolean;
begin
  if 
    fLookaheadIdent <> #0 
  then begin
    token := fLookaheadToken;
    ident := fLookaheadIdent;
    fLookaheadIdent := #0;
    Result := True;
  end else 
    Result := fTokenizer.GetToken(token, ident);
end; { TParser.GetToken }

function TParser.IsFunction(
  const Ident: string; 
  var funcIdx: Integer
  ): Boolean;
begin
  funcIdx := __AST.__Functions.IndexOf(Ident);
  Result := (funcIdx >= 0);
end; { TParser.IsFunction }

function TParser.IsVariable(
  const Ident: string; 
  var varIdx: Integer
  ): Boolean;
begin
  Assert(
    Assigned(fContext._CurrentFunc), 
    'TParser.IsVariable: No active function'
  );

  varIdx := fContext._CurrentFunc.__ParamNames.IndexOf(Ident);
  Result := (varIdx >= 0);
end; { TParser.IsVariable }

function TParser.Parse(
  const Code: string; 
  const Tokenizer: ITokenizer;
  const AST: IAST
  ): Boolean;
begin
  Result := False;
  fTokenizer := Tokenizer;
  fAST := AST;
  fLookaheadIdent := #0;
  Tokenizer.Initialize(Code);

  while 
    not Tokenizer.IsAtEnd 
  do
    if not ParseFunction then Exit;

  Result := True;
end; { TParser.Parse }

function TParser.ParseBlock(var block: IASTBlock): Boolean;
var
  _ident     : string;
  _statement : IASTStatement;
  _statements: TStatementList;
  _token     : TTokenKind;
begin
  Result := False;

  /// block = "{" statement {";" statement} [";"] "}"

  _statements := TStatementList.Create;

  try
    if not FetchToken([tkLeftCurly]) then Exit;

    repeat
      if not ParseStatement(_statement) then Exit;

      _statements.Add(_statement);

      if 
        not FetchToken([tkSemicolon, tkRightCurly], _ident, _token) 
      then
        Exit;

      if 
        _token = tkSemicolon 
      then begin
        // semicolon doesn't automatically force new statement; 
        // it may be followed by a curly right brace
        if 
          not FetchToken([tkRightCurly], _ident, _token) 
        then
          PushBack(_token, _ident);
      end;
    until 
      _token = tkRightCurly;

    block := __AST.CreateBlock;

    for 
      _statement in _statements 
    do
      block.__Statements.Add(_statement);

  finally 
    FreeAndNil(_statements); 
  end;
  
  Result := True;
end; { TParser.ParseBlock }

function TParser.ParseExpresionList(parameters: TExpressionList): Boolean;
var
  _expected: TTokenKinds;
  _expr    : IASTExpression;
  _ident   : string;
  _token   : TTokenKind;
begin
  Result := False;

  /// "(" [expression {"," expression}] ")"

  if not FetchToken([tkLeftParen]) then Exit;

  _expected := [tkRightParen];

  // parameter list, including ")"
  repeat
    if 
      FetchToken(_expected, _ident, _token) 
    then begin
      if 
        _token = tkRightParen 
      then 
        break; //repeat
      // else - token = tkComma, parse next expression
    end else 
      if 
        _expected = [tkRightParen] 
      then
        // only the first time around: not a ")" so try parsing it is
        // an expression
        PushBack(_token, _ident)
      else
        Exit;

    Result := ParseExpression(_expr);
    
    if not Result then Exit;

    parameters.Add(_expr);

    Include(_expected, tkComma);
  until 
    False;

  Result := True;
end; { TParser.ParseExpresionList }

function TParser.ParseExpression(var expression: IASTExpression): Boolean;
var
  _expr : IASTExpression;
  _ident: string;
  _term : IASTTerm;
  _token: TTokenKind;
begin
  Result := False;

  /// expression = term
  ///            | term operator term
  ///
  /// operator = "+" | "-" | "*" | "<"

  _expr := __AST.CreateExpression;

  if not ParseTerm(_term) then Exit;

  _expr.__Term1 := _term;

  if 
    not FetchToken([tkLessThan, tkPlus, tkMinus, tkAsterisk], _ident, _token) 
  then begin
    PushBack(_token, _ident);
    _expr.__BinaryOp := opNone;
  end else begin
    case 
      _token 
    of
      tkLessThan: _expr.__BinaryOp := opCompareLess;
      tkPlus    : _expr.__BinaryOp := opAdd;
      tkMinus   : _expr.__BinaryOp := opSubtract;
      tkAsterisk: _expr.__BinaryOp := opMult;
      else 
        raise Exception.Create('TParser.ParseExpression: Unexpected token');
    end;

    if not ParseTerm(_term) then Exit;

    _expr.__Term2 := _term;
  end;

  expression := _expr;
  Result := True;
end; { TParser.ParseExpression }

function TParser.ParseFunction: Boolean;
var
  _block     : IASTBlock;
  _expected  : TTokenKinds;
  _func      : IASTFunction;
  _funcName  : string;
  _ident     : string;
  _token     : TTokenKind;
begin
  Result := False;

  /// function = identifier "(" [identifier { "," identifier }] ")" block

  // function name
  if not FetchToken([tkIdent], _funcName, _token) then Exit(_token = tkEOF);

  _func := __AST.CreateFunction;
  _func.__Name := _funcName;

  __AST.__Functions.Add(_func); 
    // we might need this function in the global table for recursive calls
  fContext._CurrentFunc := _func;
  
  try
    // "("
    if not FetchToken([tkLeftParen]) then Exit;

    // parameter list, including ")"
    _expected := [tkIdent, tkRightParen];
    repeat
      if not FetchToken(_expected, _ident, _token) then Exit;

      if 
        _token = tkRightParen 
      then 
        Break //repeat
      else if 
          _token = tkIdent
        then begin
          _func.__ParamNames.Add(_ident);
          _expected := _expected - [tkIdent] + [tkComma, tkRightParen];
        end 
      else if
          _token = tkComma 
        then
          _expected := _expected + [tkIdent] - [tkComma, tkRightParen]
      else begin
        __LastError := 'Internal error in ParseFunction';
        Exit;
      end;
    until 
      False;

    // function body
    if not ParseBlock(_block) then Exit;
    
    _func.__Body := _block;
    Result := True;
  finally
    fContext._CurrentFunc := nil;
  end;
end; { TParser.ParseFunction }

function TParser.ParseIf(var statement: IASTStatement): Boolean;
var
  _condition: IASTExpression;
  _elseBlock: IASTBlock;
  _ident    : string;
  _loc      : TPoint;
  _stmt     : IASTIfStatement;
  _thenBlock: IASTBlock;
begin
  Result := False;

  /// if = "if" expression block "else" block
  /// ("if" was already parsed)

  if not ParseExpression(_condition) then Exit;

  if not ParseBlock(_thenBlock) then Exit;

  if not FetchToken([tkIdent], _ident) then Exit;

  if 
    not SameText(_ident, 'else') 
  then begin
    _loc := fTokenizer.CurrentLocation;
    __LastError := Format(
                     '"else" expected in line %d, column %d',
                     [_loc.X, _loc.Y]
                     );
    Exit;
  end;

  if not ParseBlock(_elseBlock) then Exit;

  _stmt := __AST.CreateStatement(stIf) as IASTIfStatement;
  _stmt.__Condition := _condition;
  _stmt.__ThenBlock := _thenBlock;
  _stmt.__ElseBlock := _elseBlock;
  statement := _stmt;
  Result := True;
end; { TParser.ParseIf }

function TParser.ParseReturn(var statement: IASTStatement): Boolean;
var
  _expression: IASTExpression;
  _stmt      : IASTReturnStatement;
begin
  Result := False;

  /// return = "return" expression

  // ("return" was already parsed)

  if not ParseExpression(_expression) then Exit;

  _stmt := __Ast.CreateStatement(stReturn) as IASTReturnStatement;
  _stmt.__Expression := _expression;

  statement := _stmt;
  Result := True;
end; { TParser.ParseReturn }

function TParser.ParseStatement(var statement: IASTStatement): Boolean;
var
  _ident: string;
  _loc  : TPoint;
begin
  Result := False;

  /// statement = if
  ///           | return

  if not FetchToken([tkIdent], _ident) then Exit;

  if 
    SameText(_ident, 'if') 
  then
    Result := ParseIf(statement)
  else if 
      SameText(_ident, 'return')   
    then
      Result := ParseReturn(statement)
    else begin
      _loc := fTokenizer.CurrentLocation;
      __LastError := Format(
                       'Invalid reserved word %s in line %d, column %d',
                       [_ident, _loc.X, _loc.Y]
                     );
  end;
end; { TParser.ParseStatement }

function TParser.ParseTerm(var term: IASTTerm): Boolean;
var
  _constant: IASTTermConstant;
  _funcCall: IASTTermFunctionCall;
  _funcIdx : Integer;
  _ident   : string;
  _loc     : TPoint;
  _token   : TTokenKind;
  _variable: IASTTermVariable;
  _varIdx  : Integer;
begin
  Result := False;

  /// term = numeric_constant
  ///      | function_call
  ///      | identifier
  ///
  /// function_call = identifier "(" [expression { "," expression } ] ")"

  if not FetchToken([tkIdent, tkNumber], _ident, _token) then Exit;

  if 
    _token = tkNumber 
  then begin
    // parse numeric constant
    _constant := __AST.CreateTerm(termConstant) as IASTTermConstant;
    _constant.__Value := StrToInt(_ident);
    term := _constant;
    Result := True;
  end else if
      IsFunction(_ident, _funcIdx)     
    then begin
      // parse function call
      _funcCall := __AST.CreateTerm(termFunctionCall) as IASTTermFunctionCall;
      _funcCall.__FunctionIdx := _funcIdx;
      Result := ParseExpresionList(_funcCall.__Parameters);
      
      if Result then term := _funcCall;
    end else if 
        IsVariable(_ident, _varIdx)
      then begin
        // parse variable
        _variable := __AST.CreateTerm(termVariable) as IASTTermVariable;
        _variable.__VariableIdx := _varIdx;
        term := _variable;
        Result := True;
      end else begin
        _loc := fTokenizer.CurrentLocation;
        __LastError := Format(
                         'Unexpected token in line %d, column %d (not a number,'
                           + 'variable, or function)',
                         [_loc.X, _loc.Y]
                       );
      end;
end; { TParser.ParseTerm }

procedure TParser.PushBack(
  token: TTokenKind; 
  const Ident: string
  );
begin
  Assert(fLookaheadIdent = #0, 'TParser: Lookahead buffer is not empty');
  fLookaheadToken := token;
  fLookaheadIdent := Ident;
end; { TParser.PushBack }

end.
