unit DSLCompiler.Compiler.Dump;

interface

{$mode objfpc}{$H+}

uses

  // rtl, fcl, lcl
  Classes,

  // dsl compiler
  DSLCompiler.AST,
  DSLCompiler.Compiler;

function CreateCodegenDump(dump: TStringList): ICodegen;

implementation

uses

  // rtl, fcl, lcl
  SysUtils,
  
  // dsl compiler
  DSLCompiler.Base, 
  DSLCompiler.Runnable;

type

  TCodegenDump = class(TCompilerBase, ICodegen)
  strict private
    fAST        : IAST;
    fCurrentFunc: IASTFunction;
    fDump       : TStringList;
    fErrors     : Boolean;
    fText       : string;
  strict protected
    procedure DumpBlock(
      const Indent: string; 
      const Block: IASTBlock
      );
    procedure DumpExpression(const Expr: IASTExpression);
    procedure DumpFunction(const Func: IASTFunction);
    procedure DumpFunctionCall(const FuncCall: IASTTermFunctionCall);
    procedure DumpIfStatement(
      const Indent: string; 
      const Statement: IASTIfStatement
      );
    procedure DumpReturnStatement(
      const Indent: string; 
      const Statement: IASTReturnStatement
      );
    procedure DumpStatement(
      const Indent: string; 
      const Statement: IASTStatement
      );
    procedure DumpTerm(const Term: IASTTerm);
    procedure WriteText(const S: string);
    procedure WritelnText(const S: string = '');
  public
    constructor Create(dump: TStringList);
    function Generate(
      const AST: IAST; 
      var runnable: IProgram
      ): Boolean;
  end; { TCodegenDump }

{ externals }

function CreateCodegenDump(dump: TStringList): ICodegen;
begin
  Result := TCodegenDump.Create(dump);
end; { CreateCodegenDump }

{ TCodegenDump }

constructor TCodegenDump.Create(dump: TStringList);
begin
  inherited Create;
  fDump := dump;
end; { TCodegenDump.Create }

procedure TCodegenDump.DumpBlock(
  const Indent: string; 
  const Block: IASTBlock
  );
var
  _iStatement: Integer;
begin
  WriteText(Indent); WritelnText('{');

  for 
    _iStatement := 0 to Block.__Statements.Count - 1
  do begin
    DumpStatement(Indent + '  ', Block.__Statements[_iStatement]);
  
    if 
      _iStatement < (Block.__Statements.Count - 1) 
    then
      WritelnText(';')
    else
      WritelnText;
  end;
  
  WriteText(Indent); WritelnText('}');
end; { TCodegenDump.DumpBlock }

procedure TCodegenDump.DumpExpression(const Expr: IASTExpression);
begin
  DumpTerm(Expr.__Term1);

  case 
    Expr.__BinaryOp 
  of
    opNone:        Exit;
    opAdd:         WriteText(' + ');
    opSubtract:    WriteText(' - ');
    opCompareLess: WriteText(' < ');
    else begin
      WritelnText('*** Unexpected operator');
      fErrors := True;
    end;
  end;

  DumpTerm(Expr.__Term2);
end; { TCodegenDump.DumpExpression }

procedure TCodegenDump.DumpFunction(const Func: IASTFunction);
begin
  fCurrentFunc := Func;
  WritelnText(
    Format(
      '%s(%s)',
      [Func.__Name, ''.Join(',', Func.__ParamNames.ToArray)]
    )
  );
  DumpBlock('', Func.__Body);
  fCurrentFunc := nil;
end; { TCodegenDump.DumpFunction }

procedure TCodegenDump.DumpFunctionCall(const FuncCall: IASTTermFunctionCall);
var
  _func  : IASTFunction;
  _iParam: Integer;
begin
  _func := fAST.__Functions[FuncCall.__FunctionIdx];
  WriteText(_func.__Name);
  WriteText('(');

  for 
    _iParam := 0 to FuncCall.__Parameters.Count - 1
  do begin
    if _iParam > 0 then WriteText(', ');

    DumpExpression(FuncCall.__Parameters[_iParam]);
  end;

  WriteText(')');
end; { TCodegenDump.DumpFunctionCall }

procedure TCodegenDump.DumpIfStatement(
  const Indent: string; 
  const Statement: IASTIfStatement);
begin
  WriteText(Indent);
  WriteText('if (');
  DumpExpression(Statement.__Condition);
  WritelnText(')');
  DumpBlock(Indent, Statement.__ThenBlock);
  WriteText(Indent);
  WritelnText('else');
  DumpBlock(Indent, Statement.__ElseBlock);
end; { TCodegenDump.DumpIfStatement }

procedure TCodegenDump.DumpReturnStatement(
  const Indent: string;
  const Statement: IASTReturnStatement
  );
begin
  WriteText(Indent);
  WriteText('return ');
  DumpExpression(Statement.__Expression);
end; { TCodegenDump.DumpReturnStatement }

procedure TCodegenDump.DumpStatement(
  const Indent: string;
  const Statement: IASTStatement
  );
var
  _stmIf    : IASTIfStatement;
  _stmReturn: IASTReturnStatement;
begin
  if 
    Supports(statement, IASTIfStatement, _stmIf) 
  then
    DumpIfStatement(Indent, _stmIf)
  else if 
      Supports(statement, IASTReturnStatement, _stmReturn) 
    then
      DumpReturnStatement(Indent, _stmReturn)
    else begin
      WritelnText('*** Unknown statement');
      fErrors := True;
      Exit;
    end;
end; { TCodegenDump.DumpStatement }

procedure TCodegenDump.DumpTerm(const Term: IASTTerm);
var
  _termConst   : IASTTermConstant;
  _termFuncCall: IASTTermFunctionCall;
  _termVar     : IASTTermVariable;
begin
  if 
    Supports(term, IASTTermConstant, _termConst) 
  then
    WriteText(IntToStr(_termConst.__Value))
  else if 
      Supports(term, IASTTermVariable, _termVar) 
    then
      WriteText(fCurrentFunc.__ParamNames[_termVar.__VariableIdx])
    else if 
        Supports(term, IASTTermFunctionCall, _termFuncCall)
      then
        DumpFunctionCall(_termFuncCall)
      else begin
        WritelnText('*** Unexpected term');
        fErrors := True;
      end;
end; { TCodegenDump.DumpTerm }

function TCodegenDump.Generate(
  const AST: IAST;
  var runnable: IProgram
  ): Boolean;
var
  _i: Integer;
begin
  fErrors := False;
  fAST := AST;

  for 
    _i := 0 to AST.__Functions.Count - 1
  do begin
    if 
      _i > 0 
    then
      WritelnText;

    DumpFunction(AST.__Functions[_i]);
  end;

  fDump.Text := fText;
  Result := not fErrors;
end; { TCodegenDump.Generate }

procedure TCodegenDump.WritelnText(const S: string);
begin
  WriteText(S);
  WriteText(#13#10);
end; { TCodegenDump.WritelnText }

procedure TCodegenDump.WriteText(const S: string);
begin
  fText := fText + S;
end; { TCodegenDump.WriteText }

end.
