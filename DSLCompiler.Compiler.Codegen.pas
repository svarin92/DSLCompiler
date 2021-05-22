unit DSLCompiler.Compiler.Codegen;

interface

{$mode objfpc}{$H+}

uses

  // dsl compiler
  DSLCompiler.System,
  DSLCompiler.Runnable;

type

  PExecContext = ^TExecContext;

  TFunction = function (
                execContext: PExecContext;
                const Parameters: TParameters
                ): Integer of object;

  TExecContext = record
    _Functions: specialize TArray<TFunction>;
  end; (* TExecContext *)

  TContext = record
    _Exec  : PExecContext;
    _Params: TParameters;
    _Result: Integer;
  end; (* TContext *)

  TExpression = function (var context: TContext): Integer of object;
  TStatement = procedure (var context: TContext) of object;
  TStatements = specialize TArray<TStatement>;
  TFuncCallParams = specialize TArray<TExpression>;

  TCodegenAdd = class(TDSLObject)
  strict private
    _Expr1, _Expr2: TExpression;
  public
    constructor Create(expr1, expr2: TExpression);
    function Process(var context: TContext): Integer;
  //
    class function CreateDecl(const Expr1, Expr2: TExpression): TExpression; 
  end; { TCodegenAdd }

  TCodegenBlock = class(TDSLObject)
  strict private
    _Statements: TStatements;
  public
    constructor Create(statements: TStatements);
    procedure Process(var context: TContext);
  //
    class function CreateDecl(const Statements: TStatements): TStatement;
  end; { TCodegenBlock }

  TCodegenConstant = class(TDSLObject)
  strict private
    _Value: Integer;
  public
    constructor Create(value: Integer);
    function Process(var context: TContext): Integer;
  //
    class function CreateDecl(value: Integer): TExpression;
  end; { TCodegenConstant }

  TCodegenFunctionCall = class(TDSLObject)
  strict private
    _FuncIndex: Integer;
    _Params: TFuncCallParams;
  public
    constructor Create(
      funcIndex: Integer;
      params: TFuncCallParams
      );
    function Process(var context: TContext): Integer;
  //
    class function CreateDecl(
      funcIndex: Integer;
      const Params: TFuncCallParams
      ): TExpression;      
  end; { TCodegenFunctionCall }

  TCodegenIfStatement = class(TDSLObject)
  strict private
    _Condition: TExpression;
    _ThenBlock, _ElseBlock: TStatement;
  public
    constructor Create(
      condition: TExpression;
      thenBlock, elseBlock: TStatement
      );
    procedure Process(var context: TContext);
  //
    class function CreateDecl(
      const Condition: TExpression;
      const ThenBlock, ElseBlock: TStatement
      ): TStatement;
  end; { TCodegenIfStatement }

  TCodegenIsLess = class(TDSLObject)
  strict private
    _Expr1, _Expr2: TExpression;
  public
    constructor Create(expr1, expr2: TExpression);
    function Process(var context: TContext): Integer;
  //
    class function CreateDecl(const Expr1, Expr2: TExpression): TExpression;
  end; { TCodegenIsLess }

  TCodegenReturnStatement = class(TDSLObject)
  strict private
    _Expression: TExpression;
  public
    constructor Create(expression: TExpression);
    procedure Process(var context: TContext);
  //
    class function CreateDecl(const Expression: TExpression): TStatement;
  end; { TCodegenReturnStatement }

  TCodegenSubtract = class(TDSLObject)
  strict private
    _Expr1, _Expr2: TExpression;
  public
    constructor Create(expr1, expr2: TExpression);
    function Process(var context: TContext): Integer;
  //
    class function CreateDecl(const Expr1, Expr2: TExpression): TExpression;
  end; { TCodegenSubtract }

  TCodegenVariable = class(TDSLObject)
  strict private
    _VarIndex: Integer;
  public
    constructor Create(varIndex: Integer);
    function Process(var context: TContext): Integer;
  //
    class function CreateDecl(varIndex: Integer): TExpression;
  end; { TCodegenVariable }

  TCodegenFunction = class(TDSLObject)
  strict private
    _Block: TStatement;
  public
    constructor Create(block: TStatement);
    function Process(
      execContext: PExecContext;
      const params: TParameters
      ): Integer;
  //
    class function CreateDecl(const Block: TStatement): TFunction;
  end; { TCodegenFunction }

implementation

{ TCodegenBlock }

constructor TCodegenBlock.Create(statements: TStatements);
begin
  inherited Create;
  _Statements := statements;  
end; { TCodegenBlock.Create }

procedure TCodegenBlock.Process(var context: TContext);
var
  _stmt: TStatement;
begin
  for
    _stmt in _Statements
  do
    _stmt(context);
end; { TCodegenBlock.Process }

class function TCodegenBlock.CreateDecl(
  const Statements: TStatements
  ): TStatement;
begin
  Result := @Self.Create(Statements).Process;
end; { TCodegenBlock.CreateDecl }

{ TCodegenFunction }

constructor TCodegenFunction.Create(block: TStatement);
begin
  inherited Create;
  _Block := block;  
end; { TCodegenFunction.Create }

function TCodegenFunction.Process(
  execContext: PExecContext; 
  const Params: TParameters
  ): Integer;
var
  _context: TContext;
begin
  _context._Exec := execContext;
  _context._Params := Params;
  _context._Result := 0;
  _Block(_context);
  Result := _context._Result;
end; { TCodegenFunction.Process }

class function TCodegenFunction.CreateDecl(const Block: TStatement): TFunction;
begin
  Result := @Self.Create(Block).Process;
end; { TCodegenFunction.CreateDecl }

{ TCodegenReturnStatement }

constructor TCodegenReturnStatement.Create(expression: TExpression);
begin
  inherited Create;
  _Expression := expression;  
end; { TCodegenReturnStatement.Create }

procedure TCodegenReturnStatement.Process(var context: TContext);
begin
  context._Result := _Expression(context);
end; { TCodegenReturnStatement.Process }

class function TCodegenReturnStatement.CreateDecl(
  const Expression: TExpression
  ): TStatement;
begin
  Result := @Self.Create(Expression).Process;
end; { CodegenReturnStatement.CreateDecl }

{ TCodegenIfStatement }

constructor TCodegenIfStatement.Create(
  condition: TExpression; 
  thenBlock, elseBlock: TStatement
  );
begin
  inherited Create;
  _Condition := condition;
  _ThenBlock := thenBlock;
  _ElseBlock := elseBlock; 
end; { TCodegenIfStatement.Create }

procedure TCodegenIfStatement.Process(var context: TContext);
begin
  if
    _Condition(context) <> 0
  then
    _ThenBlock(context)
  else
    _ElseBlock(context);
end; { TCodegenIfStatement.Process }

class function TCodegenIfStatement.CreateDecl(
  const Condition: TExpression; 
  const ThenBlock, ElseBlock: TStatement
  ): TStatement;
begin
  Result := @Self.Create(Condition, ThenBlock, ElseBlock).Process;
end; { TCodegenIfStatement.CreateDecl }

{ TCodegenAdd }

constructor TCodegenAdd.Create(expr1, expr2: TExpression);
begin
  inherited Create;
  _Expr1 := expr1;
  _Expr2 := expr2;  
end; { TCodegenAdd.Create }

function TCodegenAdd.Process(var context: TContext): Integer;
begin
  Result := _Expr1(context) + _Expr2(context);
end; { TCodegenAdd.Process }

class function TCodegenAdd.CreateDecl(
  const Expr1, Expr2: TExpression
  ): TExpression;
begin
  Result := @Self.Create(Expr1, Expr2).Process;
end; { TCodegenAdd.CreateDecl }

{ TCodegenSubtract }

constructor TCodegenSubtract.Create(expr1, expr2: TExpression);
begin
  inherited Create;
  _Expr1 := expr1;
  _Expr2 := expr2;  
end; { TCodegenSubtract.Create }

function TCodegenSubtract.Process(var context: TContext): Integer;
begin
  Result := _Expr1(context) - _Expr2(context);
end; { TCodegenSubtract.Process }

class function TCodegenSubtract.CreateDecl(
  const Expr1, Expr2: TExpression
  ): TExpression;
begin
  Result := @Self.Create(Expr1, Expr2).Process;
end; { TCodegenSubtract.CreateDecl }

{ TCodegenIsLess }

constructor TCodegenIsLess.Create(expr1, expr2: TExpression);
begin
  inherited Create;
  _Expr1 := expr1;
  _Expr2 := expr2;  
end; { TCodegenIsLess.Create }

function TCodegenIsLess.Process(var context: TContext): Integer;
var
  _diff: Integer;
begin
  _diff := _Expr1(context) - _Expr2(context);

  if _diff < 0 then Result := 1 else Result := 0;
end; { TCodegenIsLess.Process }

class function TCodegenIsLess.CreateDecl(
  const Expr1, Expr2: TExpression
  ): TExpression;
begin
  Result := @Self.Create(Expr1, Expr2).Process;
end; { TCodegenIsLess.CreateDecl }

{ TCodegenConstant }

constructor TCodegenConstant.Create(value: Integer);
begin
  inherited Create;
  _Value := value;  
end; { TCodegenConstant.Create }

function TCodegenConstant.Process(var context: TContext): Integer;
begin
  Result := _Value;
end; { TCodegenConstant.Process }

class function TCodegenConstant.CreateDecl(value: Integer): TExpression;
begin
  Result := @Self.Create(value).Process;
end; { TCodegenConstant.CreateDecl }

{ TCodegenVariable }

constructor TCodegenVariable.Create(varIndex: Integer);
begin
  inherited Create;
  _VarIndex := varIndex;  
end; { TCodegenVariable.Create }

function TCodegenVariable.Process(var context: TContext): Integer;
begin
  Result := context._Params[_VarIndex];
end; { TCodegenVariable.Process }

class function TCodegenVariable.CreateDecl(varIndex: Integer): TExpression;
begin
  Result := @Self.Create(varIndex).Process;
end; { TCodegenVariable.CreateDecl }

{ TCodegenFunctionCall }

constructor TCodegenFunctionCall.Create(
  funcIndex: Integer; 
  params: TFuncCallParams
  );
begin
  inherited Create;
  _FuncIndex := funcIndex;
  _Params := params; 
end; { TCodegenFunctionCall.Create }

function TCodegenFunctionCall.Process(var context: TContext): Integer;
var
  _funcParams: TParameters;
  _iParam    : Integer;
begin
  SetLength(_funcParams, Length(_Params));

  for
    _iParam := Low(_Params) to High(_Params)
  do
    _funcParams[_iParam] := _Params[_iParam](context);

  Result := context._Exec^._Functions[_FuncIndex](context._Exec, _funcParams);
end; { TCodegenFunctionCall.Process }

class function TCodegenFunctionCall.CreateDecl(
  funcIndex: Integer; 
  const Params: TFuncCallParams
  ): TExpression;
begin
  Result := @Self.Create(funcIndex, Params).Process;
end; { TCodegenFunctionCall.CreateDecl }

end.
