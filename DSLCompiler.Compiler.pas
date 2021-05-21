unit DSLCompiler.Compiler;

interface

{$mode objfpc}{$H+}

uses

  // dsl compiler
  DSLCompiler.AST,  
  DSLCompiler.Runnable;

type

  ICodegen = interface 
  ['{C359C174-E324-4709-86EF-EE61AFE3B1FD}']
    function Generate(
      const AST: IAST; 
      var runnable: IProgram
      ): Boolean;
  end; { ICodegen }

  TCodegenFactory = function: ICodegen;

function CreateCodegen: ICodegen;

implementation

uses
  
  // rtl, fcl, lcl
  SysUtils,

  // delphi-like
  Generics.Collections,
  
  // dsl compiler
  DSLCompiler.Base,
  DSLCompiler.Compiler.Codegen;

type

  IProgramEx = interface 
  ['{4CEF7C78-FF69-47E2-9F63-706E167AF3A9}']
    procedure DeclareFunction(
      idx: Integer; 
      const Name: string; 
      const Code: TFunction
      );
  end; { IProgramEx }

  TProgram = class(TCompilerBase, IProgram, IProgramEx)
  strict private type
    TFunctionInfo = record
      _Name: string;
      _Code: TFunction;
    end; (* TFunctionInfo *)
  var
    fFunctions: specialize TList<TFunctionInfo>;
  strict protected
    procedure SetupContext(var context: TExecContext);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function  Call(
      const functionName: string; 
      const params: TParameters; 
      var return: Integer
      ): Boolean;
    procedure DeclareFunction(
      idx: Integer; 
      const Name: string; 
      const Code: TFunction
      );
  end; { TProgram }

  TCodegen = class(TCompilerBase, ICodegen)
  strict private
    fAST: IAST;
  strict protected
    function  CompileBlock(
      const ASTBlock: IASTBlock; 
      var codeBlock: TStatement
      ): Boolean;
    function  CompileExpression(
      const ASTExpression: IASTExpression;
      var codeExpression: TExpression
      ): Boolean;
    function  CompileFunctionCall(
      const ASTFuncCall: IASTTermFunctionCall;
      var codeExpression: TExpression
      ): Boolean;
    function  CompileIfStatement(
      const ASTStatement: IASTIfStatement;
      var codeStatement: TStatement
      ): Boolean;
    function  CompileReturnStatement(
      const ASTStatement: IASTReturnStatement;
      var codeStatement: TStatement
      ): Boolean;
    function  CompileStatement(
      const ASTStatement: IASTStatement;
      var codeStatement: TStatement
      ): Boolean;
    function  CompileTerm(
      const ASTTerm: IASTTerm; 
      var codeTerm: TExpression
      ): Boolean;
  public
    function  Generate(
      const AST: IAST; 
      var runnable: IProgram
      ): Boolean;
  end; { TCodegen }

{ exports }

function CreateCodegen: ICodegen;
begin
  Result := TCodegen.Create;
end; { CreateCodegen }

{ TProgram }

procedure TProgram.AfterConstruction;
begin
  inherited;
  fFunctions := specialize TList<TFunctionInfo>.Create;
end; { TProgram.AfterConstruction }

procedure TProgram.BeforeDestruction;
begin
  FreeAndNil(fFunctions);
  inherited;
end; { TProgram.BeforeDestruction }

function TProgram.Call(
  const FunctionName: string; 
  const Params: TParameters;
  var return: Integer
  ): Boolean;
var
  _context : TExecContext;
  _funcInfo: TFunctionInfo;
begin
  Result := False;

  for 
    _funcInfo in fFunctions 
  do begin
    if 
      SameText(FunctionName, _funcInfo._Name) 
    then begin
      SetupContext(_context);
      return := _funcInfo._Code(@_context, Params);
      Exit(True);
    end;
  end;

  __LastError := 'Function not found';
end; { TProgram.Call }

procedure TProgram.DeclareFunction(
  idx: Integer; 
  const Name: string;
  const Code: TFunction
  );
var
  _funcInfo: TFunctionInfo;
begin
  Assert(idx = fFunctions.Count); 
  _funcInfo._Name := Name;
  _funcInfo._Code := Code;
  fFunctions.Add(_funcInfo);
end; { TProgram.DeclareFunction }

procedure TProgram.SetupContext(var context: TExecContext);
var
  _iFunc: Integer;
begin
  SetLength(context._Functions, fFunctions.Count);

  for 
    _iFunc := 0 to fFunctions.Count - 1 
  do
    context._Functions[_iFunc] := fFunctions[_iFunc]._Code;
end; { TProgram.SetupContext }

{ TCodegen }

function TCodegen.CompileBlock(
  const ASTBlock: IASTBlock; 
  var codeBlock: TStatement
  ): Boolean;
var
  _codeStatement: TStatement;
  _iStatement   : Integer;
  _statements   : TStatements;
begin
  Result := False;
  SetLength(_statements, ASTBlock.__Statements.Count);
  
  for 
    _iStatement := 0 to ASTBlock.__Statements.Count - 1 
  do begin
    if 
      not CompileStatement(ASTBlock.__Statements[_iStatement], _codeStatement) 
    then
      Exit;

    _statements[_iStatement] := _codeStatement;
  end;

  codeBlock := TCodegenBlock.CreateDecl(_statements);
  Result := True;
end; { TCodegen.CompileBlock }

function TCodegen.CompileExpression(
  const ASTExpression: IASTExpression;
  var codeExpression: TExpression
  ): Boolean;
var
  _term1: TExpression;
  _term2: TExpression;
begin
  Result := False;

  if not CompileTerm(ASTExpression.__Term1, _term1) then Exit;

  if 
    ASTExpression.__BinaryOp = opNone 
  then begin
    codeExpression := _term1;
    Result := True;
  end else begin
    if 
      not CompileTerm(ASTExpression.__Term2, _term2) 
    then
      Exit;

    Result := True;

    case
      ASTExpression.__BinaryOp
    of
      opAdd:         codeExpression := TCodegenAdd.CreateDecl(_term1, _term2);
      opSubtract:    codeExpression := TCodegenSubtract.CreateDecl(_term1, _term2);
      opCompareLess: codeExpression := TCodegenIsLess.CreateDecl(_term1, _term2);
      else           Result := SetError('*** Unexpected operator');
    end;
  end;
end; { TCodegen.CompileExpression }

function TCodegen.CompileFunctionCall(
  const ASTFuncCall: IASTTermFunctionCall;
  var codeExpression: TExpression
  ): Boolean;
var
  _func      : IASTFunction;
  _iParam    : Integer;
  _parameters: TFuncCallParams;
  _paramExpr : TExpression;
begin
  Result := False;

  if 
    ASTFuncCall.__FunctionIdx >= fAST.__Functions.Count
  then
    __LastError := '*** Invalid function'
  else begin
    _func := fAST.__Functions[ASTFuncCall.__FunctionIdx];

    if 
      _func.__ParamNames.Count <> ASTFuncCall.__Parameters.Count
    then
      __LastError := Format(
                       'Invalid number of parameters in %s() call',
                       [_func.__Name]
                       )
    else begin
      SetLength(_parameters, ASTFuncCall.__Parameters.Count);

      for 
        _iParam := 0 to ASTFuncCall.__Parameters.Count - 1 
      do begin
        if 
          not CompileExpression(ASTFuncCall.__Parameters[_iParam], _paramExpr) 
        then
          Exit;
        
        _parameters[_iParam] := _paramExpr;
      end;

      codeExpression := TCodegenFunctionCall.CreateDecl(
                           ASTFuncCall.__FunctionIdx,
                           _parameters
                           );
      Result := True;
    end;
  end;
end; { TCodegen.CompileFunctionCall }

function TCodegen.CompileIfStatement(
  const ASTStatement: IASTIfStatement;
  var codeStatement: TStatement
  ): Boolean;
var
  _condition: TExpression;
  _elseBlock: TStatement;
  _thenBlock: TStatement;
begin
  Result := False;

  if 
    not CompileExpression(ASTStatement.__Condition, _condition)
  then
    Exit;
  
  if 
    not CompileBlock(ASTStatement.__ThenBlock, _thenBlock) 
  then
    Exit;
  
  if 
    not CompileBlock(ASTStatement.__ElseBlock, _elseBlock) 
  then
    Exit;

  codeStatement := TCodegenIfStatement.CreateDecl(
                     _condition,
                     _thenBlock,
                     _elseBlock
                     );
  Result := True;
end; { TCodegen.CompileIfStatement }

function TCodegen.CompileReturnStatement(
  const ASTStatement: IASTReturnStatement; 
  var codeStatement: TStatement
  ): Boolean;
var
  _expression: TExpression;
begin
  Result := CompileExpression(ASTStatement.__Expression, _expression);
  
  if 
    Result 
  then
    codeStatement := TCodegenReturnStatement.CreateDecl(_expression);
end; { TCodegen.CompileReturnStatement }

function TCodegen.CompileStatement(
  const ASTStatement: IASTStatement;
  var codeStatement: TStatement
  ): Boolean;
var
  stmIf    : IASTIfStatement;
  stmReturn: IASTReturnStatement;
begin
  if 
    Supports(astStatement, IASTIfStatement, stmIf) 
  then
    Result := CompileIfStatement(stmIf, codeStatement)
  else if 
      Supports(astStatement, IASTReturnStatement, stmReturn) 
    then
      Result := CompileReturnStatement(
                  stmReturn,
                  codeStatement
                )
    else
      Result := SetError('*** Unknown statement');
end; { TCodegen.CompileStatement }

function TCodegen.CompileTerm(
  const ASTTerm: IASTTerm;
  var codeTerm: TExpression
  ): Boolean;
var
  _termConst   : IASTTermConstant;
  _termFuncCall: IASTTermFunctionCall;
  _termVar     : IASTTermVariable;
begin
  Result := True;

  if
    Supports(ASTTerm, IASTTermConstant, _termConst)
  then
    codeTerm := TCodegenConstant.CreateDecl(_termConst.__Value)
  else if
      Supports(ASTTerm, IASTTermVariable, _termVar)
    then
      codeTerm := TCodegenVariable.CreateDecl(_termVar.__VariableIdx)
    else if
        Supports(ASTTerm, IASTTermFunctionCall, _termFuncCall)
      then
        Result := CompileFunctionCall(_termFuncCall, codeTerm)
      else
        Result := SetError('*** Unexpected term');
end; { TCodegen.CompileTerm }

function TCodegen.Generate(
  const AST: IAST; 
  var runnable: IProgram
  ): Boolean;
var
  _block      : TStatement;
  _i          : Integer;
  _runnableInt: IProgramEx;
begin
  Result := False; 
    // to keep compiler happy
  fAST := AST;
  runnable := TProgram.Create;
  _runnableInt := runnable as IProgramEx;

  for 
    _i := 0 to AST.__Functions.Count - 1 
  do begin
    if 
      not CompileBlock(AST.__Functions[_i].__Body, _block)
    then
      Exit;

  _runnableInt.DeclareFunction(
                 _i,
                 AST.__Functions[_i].__Name,
                 TCodegenFunction.CreateDecl(_block)
                )
  end;

  Result := True;
end; { TCodegen.Generate }

end.
