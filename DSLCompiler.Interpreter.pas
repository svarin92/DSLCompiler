unit DSLCompiler.Interpreter;

interface

{$mode objfpc}{$H+}

uses

  // dsl compiler 
  DSLCompiler.AST,
  DSLCompiler.Runnable;

function CreateInterpreter(const AST: IAST): IProgram;

implementation

uses

  // rtl, fcl, lcl
  SysUtils,

  // dsl compiler
  DSLCompiler.Base;

type

  TContext = record
    _Params: TParameters;
    _Result: Integer;
  end; (* TContext *)

  TInterpreter = class(TCompilerBase, IProgram)
  strict private
    fAST: IAST;
  strict protected
    function CallFunction(
      const Func: IASTFunction; 
      const Params: TParameters;
      var return: Integer
      ): Boolean;
    function EvalBlock(
      var context: TContext; 
      const Block: IASTBlock
      ): Boolean;
    function EvalExpression(
      var context: TContext; 
      const Expression: IASTExpression; 
      var value: Integer): 
      Boolean;
    function EvalFunctionCall(
      var context: TContext; 
      const functionCall: IASTTermFunctionCall; 
      var value: Integer
      ): Boolean;
    function EvalIfStatement(
      var context: TContext; 
      const Statement: IASTIfStatement
      ): Boolean;
    function EvalReturnStatement(
      var context: TContext; 
      const Statement: IASTReturnStatement
      ): Boolean;
    function EvalStatement(
      var context: TContext; 
      const Statement: IASTStatement
      ): Boolean;
    function EvalTerm(
      var context: TContext; 
      const term: IASTTerm; 
      var value: Integer
      ): Boolean;
  public
    constructor Create(const AST: IAST);   
  //
    function Call(
      const FunctionName: string; 
      const Params: TParameters;
      var return: Integer
      ): Boolean;      
  end; { TInterpreter }

{ exports }

function CreateInterpreter(const AST: IAST): IProgram;
begin
  Result := TInterpreter.Create(AST);
end; { CreateInterpreter }

{ TInterpreter }

constructor TInterpreter.Create(const AST: IAST);
begin
  inherited Create;
  fAST := AST;
end; { TInterpreter.Create }

function TInterpreter.Call(
  const FunctionName: string; 
  const Params: TParameters;
  var return: Integer
  ): Boolean;
var
  _iFunc: Integer;
begin
  for 
    _iFunc := 0 to fAST.__Functions.Count - 1
  do
    if 
      SameText(FunctionName, fAST.__Functions[_iFunc].__Name)
    then
      Exit(CallFunction(fAST.__Functions[_iFunc], Params, return));

  Result := SetError('Unknown function');
end; { TInterpreter.Call }

function TInterpreter.CallFunction(
  const Func: IASTFunction; 
  const Params: TParameters; 
  var return: Integer
  ): Boolean;
var
  _context: TContext;
begin
  if 
    Length(Params) <> Func.__ParamNames.Count
  then
    Exit(SetError('Invalid number of parameters'));

  _context._Params := Params;
  _context._Result := 0;
  Result := EvalBlock(_context, Func.__Body);
  
  if   
    Result 
  then 
    return := _context._Result;
end; { TInterpreter.CallFunction }

function TInterpreter.EvalBlock(
  var context: TContext; 
  const Block: IASTBlock
  ): Boolean;
var
  _statement: IASTStatement;
begin
  Result := False;

  for 
    _statement in Block.__Statements 
  do
    if not EvalStatement(context, _statement) then Exit;

  Result := True;
end; { TInterpreter.EvalBlock }

function TInterpreter.EvalExpression(
  var context: TContext; 
  const Expression: IASTExpression; 
  var value: Integer
  ): Boolean;
var
  _term1: Integer;
  _term2: Integer;
begin
  Result := False;

  if not EvalTerm(context, Expression.__Term1, _term1) then Exit;

  if 
    Expression.__BinaryOp = opNone 
  then begin
    value := _term1;
    Result := True;
    Exit;
  end;

  if not EvalTerm(context, expression.__Term2, _term2) then Exit;

  case 
    Expression.__BinaryOp 
  of
    opAdd:
      value := _term1 + _term2;
    opSubtract:
      value := _term1 - _term2;
    opCompareLess:
      if _term1 < _term2 then  value := 1 else value := 0;
    else 
      Exit(SetError('*** Unexpected binary operation'));
  end;

  Result := True;
end; { TInterpreter.EvalExpression }

function TInterpreter.EvalFunctionCall(
  var context: TContext;
  const FunctionCall: IASTTermFunctionCall; 
  var value: Integer
  ): Boolean;
var
  _funcReturn: Integer;
  _iParam    : Integer;
  _parameters: TParameters;
  _paramValue: Integer;
begin
  Result := False;
  
  if 
    (FunctionCall.__FunctionIdx < 0)
      or (FunctionCall.__FunctionIdx >= fAST.__Functions.Count)
  then
    Exit(SetError('*** Invalid function'));

  SetLength(_parameters, FunctionCall.__Parameters.Count);
  
  for 
    _iParam := 0 to FunctionCall.__Parameters.Count - 1 
  do begin
    if 
      not EvalExpression(
            context,
            FunctionCall.__Parameters[_iParam],
            _paramValue
          )
    then
      Exit;
    
    _parameters[_iParam] := _paramValue;
  end;

  if 
    not CallFunction(
          fAST.__Functions[FunctionCall.__FunctionIdx],
          _parameters,
          _funcReturn
        )
  then
    Exit;

  value := _funcReturn;
  Result := True;
end; { TInterpreter.EvalFunctionCall }

function TInterpreter.EvalIfStatement(
  var context: TContext;
  const Statement: IASTIfStatement
  ): Boolean;
var
  _value: Integer;
begin
  Result := EvalExpression(context, Statement.__Condition, _value);
  
  if 
    Result 
  then begin
    if 
      _value <> 0 
    then
      Result := EvalBlock(context, Statement.__ThenBlock)
    else
      Result := EvalBlock(context, Statement.__ElseBlock);
  end;
end; { TInterpreter.EvalIfStatement }

function TInterpreter.EvalReturnStatement(
  var context: TContext;
  const Statement: IASTReturnStatement
  ): Boolean;
var
  _value: Integer;
begin
  Result := EvalExpression(context, Statement.__Expression, _value);
  
  if 
    Result 
  then
    context._Result := _value;
end; { TInterpreter.EvalReturnStatement }

function TInterpreter.EvalStatement(
  var context: TContext; 
  const Statement: IASTStatement
  ): Boolean;
var
  stmIf    : IASTIfStatement;
  stmReturn: IASTReturnStatement;
begin
  if 
    Supports(Statement, IASTIfStatement, stmIf) 
  then
    Result := EvalIfStatement(context, stmIf)
  else if 
      Supports(Statement, IASTReturnStatement, stmReturn) 
    then
      Result := EvalReturnStatement(context, stmReturn)
    else
      Result := SetError('*** Unknown statement');
end; { TInterpreter.EvalStatement }

function TInterpreter.EvalTerm(
  var context: TContext; 
  const Term: IASTTerm;
  var value: Integer
  ): Boolean;
var
  _funcResult  : Integer;
  _termConst   : IASTTermConstant;
  _termFuncCall: IASTTermFunctionCall;
  _termVar     : IASTTermVariable;  
begin
  Result := True;
  
  if 
    Supports(Term, IASTTermConstant, _termConst) 
  then
    value := _termConst.__Value
  else if 
      Supports(Term, IASTTermVariable, _termVar) 
    then begin
      if 
        (_termVar.__VariableIdx < Low(context._Params))
          or (_termVar.__VariableIdx > High(context._Params))
      then
        Result := SetError('*** Invalida variable')
      else
        value := context._Params[_termVar.__VariableIdx];
    end else if 
        Supports(Term, IASTTermFunctionCall, _termFuncCall) 
      then begin
        Result := EvalFunctionCall(context, _termFuncCall, _funcResult);
    
        if Result then value := _funcResult;
      end
    else
      Result := SetError('*** Unexpected term');
end; { TInterpreter.EvalTerm }

end.
