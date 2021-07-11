unit DSLCompiler.AST;

interface

{$mode objfpc}{$H+}

uses

  // delphi-like
  Generics.Collections;

type

  TASTStatementType = (stIf, stReturn);
  TASTTermType = (termConstant, termVariable, termFunctionCall);
  TBinaryOperation = (opNone, opAdd, opSubtract, opMult, opCompareLess);

  IASTBlock = interface;
  IASTExpression = interface;

  IASTTerm = interface 
  ['{74B36C0D-30A4-47E6-B359-E45C4E94580C}']
  //
  end; { IASTTerm }

  IASTTermConstant = interface(IASTTerm) 
  ['{737340F5-E605-4480-BE6E-DA56FAA34104}']
    function GetValue: Integer;
    procedure SetValue(const Value: Integer);
  //
    property __Value: Integer 
      read GetValue 
      write SetValue;
  end; { IASTTermConstant }

  IASTTermVariable = interface(IASTTerm) 
  ['{933DCB5B-6C73-44C2-BEAF-D8E16EF8134C}']
    function GetVariableIdx: Integer;
    procedure SetVariableIdx(const Value: Integer);
  //
    property __VariableIdx: Integer 
      read GetVariableIdx 
      write SetVariableIdx;
  end; { IASTTermVariable }

  TExpressionList = specialize TList<IASTExpression>;

  IASTTermFunctionCall = interface(IASTTerm) 
  ['{09F0FACF-4A6C-4E59-91C8-5104C560D36C}']
    function GetFunctionIdx: Integer;
    function GetParameters: TExpressionList;
    procedure SetFunctionIdx(const Value: Integer);
  //
    property __FunctionIdx: Integer 
      read GetFunctionIdx 
      write SetFunctionIdx;
    property __Parameters: TExpressionList 
      read GetParameters;
  end; { IASTTermFunctionCall }

  IASTExpression = interface 
  ['{086BECB3-C733-4875-ABE0-EE71DCC0011D}']
    function GetBinaryOp: TBinaryOperation;
    function GetTerm1: IASTTerm;
    function GetTerm2: IASTTerm;
    procedure SetBinaryOp(const Value: TBinaryOperation);
    procedure SetTerm1(const Value: IASTTerm);
    procedure SetTerm2(const Value: IASTTerm);
  //
    property __Term1: IASTTerm 
      read GetTerm1 
      write SetTerm1;
    property __Term2: IASTTerm 
      read GetTerm2 
      write SetTerm2;
    property __BinaryOp: TBinaryOperation 
      read GetBinaryOp 
      write SetBinaryOp;
  end; { IASTExpression }

  IASTStatement = interface 
  ['{372AF2FA-E139-4EFB-8282-57FFE0EDAEC8}']
  //
  end; { IASTStatement }  

  IASTIfStatement = interface(IASTStatement) 
  ['{A6BE8E87-39EC-4832-9F4A-D5BF0901DA17}']
    function GetCondition: IASTExpression;
    function GetElseBlock: IASTBlock;
    function GetThenBlock: IASTBlock;
    procedure SetCondition(const Value: IASTExpression);
    procedure SetElseBlock(const Value: IASTBlock);
    procedure SetThenBlock(const Value: IASTBlock);
  //
    property __Condition: IASTExpression 
      read GetCondition 
      write SetCondition;
    property __ThenBlock: IASTBlock 
      read GetThenBlock 
      write SetThenBlock;
    property __ElseBlock: IASTBlock 
      read GetElseBlock 
      write SetElseBlock;
  end; { IASTIfStatement }

  IASTReturnStatement = interface(IASTStatement) 
  ['{61F7403E-CB08-43FC-AF37-A96B05BB2F9C}']
    function GetExpression: IASTExpression;
    procedure SetExpression(const Value: IASTExpression);
  //
    property __Expression: IASTExpression 
      read GetExpression 
      write SetExpression;
  end; { IASTReturnStatement }

  TStatementList = specialize TList<IASTStatement>;

  IASTBlock = interface 
  ['{450D40D0-4866-4CD2-98E8-88387F5B9904}']
    function GetStatements: TStatementList;
  //
    property __Statements: TStatementList 
      read GetStatements;
  end; { IASTBlock }

  TParameterList = specialize TList<string>;

  IASTFunction = interface 
  ['{FA4F603A-FE89-40D4-8F96-5607E4EBE511}']
    function GetBody: IASTBlock;
    function GetName: string;
    function GetParamNames: TParameterList;
    procedure SetBody(const Value: IASTBlock);
    procedure SetName(const Value: string);
  //
    property __Name: string 
      read GetName 
      write SetName;
    property __ParamNames: TParameterList 
      read GetParamNames;
    property __Body: IASTBlock 
      read GetBody 
      write SetBody;
  end; { IASTFunction }

  IASTFunctions = interface 
  ['{95A0897F-ED13-40F5-B955-9917AC911EDB}']
    function GetItems(idxFunction: Integer): IASTFunction;
    function Add(const Func: IASTFunction): Integer;
    function Count: Integer;
    function IndexOf(const Name: string): Integer;
  //  
    property __Items[idxFunction: Integer]: IASTFunction 
      read GetItems; default;
  end; { IASTFunctions }

  IASTFactory = interface 
  ['{1284482C-CA38-4D9B-A84A-B2BAED9CC8E2}']
    function CreateBlock: IASTBlock;
    function CreateExpression: IASTExpression;
    function CreateFunction: IASTFunction;
    function CreateStatement(statementType: TASTStatementType): IASTStatement;
    function CreateTerm(termType: TASTTermType): IASTTerm;
  end; { IASTFactory }

  IAST = interface(IASTFactory) 
  ['{114E494C-8319-45F1-91C8-4102AED1809E}']
    function GetFunctions: IASTFunctions;
  //
    property __Functions: IASTFunctions 
      read GetFunctions;
  end; { IAST }

  TASTFactory = function: IAST;

function CreateAST: IAST;

implementation

uses
  
  // rtl, fcl, lcl
  SysUtils;

type

  TASTTerm = class(TInterfacedObject, IASTTerm)
  //
  end; { TASTTerm }

  TASTTermConstant = class(TASTTerm, IASTTermConstant)
  strict private
    fValue: Integer;
  strict protected
    function GetValue: Integer; inline;
    procedure SetValue(const Value: Integer); inline;
  public
    property __Value: Integer 
      read GetValue 
      write SetValue;
  end; { TASTTermConstant } 

  TASTTermVariable = class(TASTTerm, IASTTermVariable)
  strict private
    fVariableIdx: Integer;
  strict protected
    function GetVariableIdx: Integer; inline;
    procedure SetVariableIdx(const Value: Integer); inline;
  public
    property __VariableIdx: Integer 
      read GetVariableIdx 
      write SetVariableIdx;
  end; { TASTTermVariable }

  TASTTermFunctionCall = class(TASTTerm, IASTTermFunctionCall)
  strict private
    fFunctionIdx: Integer;
    fParameters : TExpressionList;
  strict protected
    function GetFunctionIdx: Integer; inline;
    function GetParameters: TExpressionList; inline;
    procedure SetFunctionIdx(const Value: Integer); inline;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  //
    property __FunctionIdx: Integer 
      read GetFunctionIdx 
      write SetFunctionIdx;
    property __Parameters: TExpressionList 
      read GetParameters;
  end; { TASTTermFunctionCall }

  TASTExpression = class(TInterfacedObject, IASTExpression)
  strict private
    fBinaryOp: TBinaryOperation;
    fTerm1   : IASTTerm;
    fTerm2   : IASTTerm;
  strict protected
    function GetBinaryOp: TBinaryOperation; inline;
    function GetTerm1: IASTTerm; inline;
    function GetTerm2: IASTTerm; inline;
    procedure SetBinaryOp(const Value: TBinaryOperation); inline;
    procedure SetTerm1(const Value: IASTTerm); inline;
    procedure SetTerm2(const Value: IASTTerm); inline;
  public
    property __BinaryOp: TBinaryOperation 
      read GetBinaryOp 
      write SetBinaryOp;
    property __Term1: IASTTerm 
      read GetTerm1 
      write SetTerm1;
    property __Term2: IASTTerm 
      read GetTerm2 
      write SetTerm2;
  end; { TASTExpression }

  TASTStatement = class(TInterfacedObject, IASTStatement)
  //
  end; { TASTStatement }

  TASTIfStatement = class(TASTStatement, IASTIfStatement)
  strict private
    fCondition: IASTExpression;
    fElseBlock: IASTBlock;
    fThenBlock: IASTBlock;
  strict protected
    function GetCondition: IASTExpression; inline;
    function GetElseBlock: IASTBlock; inline;
    function GetThenBlock: IASTBlock; inline;
    procedure SetCondition(const Value: IASTExpression); inline;
    procedure SetElseBlock(const Value: IASTBlock); inline;
    procedure SetThenBlock(const Value: IASTBlock); inline;
  public
    property __Condition: IASTExpression
      read GetCondition 
      write SetCondition;
    property __ThenBlock: IASTBlock
      read GetThenBlock 
      write SetThenBlock;
    property __ElseBlock: IASTBlock
      read GetElseBlock 
      write SetElseBlock;
  end; { TASTIfStatement }

  TASTReturnStatement = class(TASTStatement, IASTReturnStatement)
  strict private
    fExpression: IASTExpression;
  strict protected
    function GetExpression: IASTExpression; inline;
    procedure SetExpression(const Value: IASTExpression); inline;
  public
    property __Expression: IASTExpression
      read GetExpression 
      write SetExpression;
  end; { TASTReturnStatement }

  TASTBlock = class(TInterfacedObject, IASTBlock)
  strict private
    fStatements: TStatementList;
  strict protected
    function GetStatements: TStatementList; inline;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  //  
    property __Statements: TStatementList 
      read GetStatements;
  end; { TASTBlock }

  TASTFunction = class(TInterfacedObject, IASTFunction)
  strict private
    fBody      : IASTBlock;
    fName      : string;
    fParamNames: TParameterList;
  strict protected
    function GetBody: IASTBlock;
    function GetName: string; inline;
    function GetParamNames: TParameterList; inline;
    procedure SetBody(const Value: IASTBlock); inline;
    procedure SetName(const Value: string); inline;
    procedure SetParamNames(const Value: TParameterList); inline;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  //
    property __Name: string 
      read GetName 
      write SetName;     
    property __ParamNames: TParameterList 
      read GetParamNames 
      write SetParamNames;
    property __Body: IASTBlock 
      read GetBody 
      write SetBody;
  end; { TASTFunction }

  TASTFunctions = class(TInterfacedObject, IASTFunctions)
  strict private
    fFunctions: specialize TList<IASTFunction>;
  strict protected
    function GetItems(idxFunction: Integer): IASTFunction; inline;
  public
    function Add(const Func: IASTFunction): Integer;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Count: Integer; inline;
    function IndexOf(const Name: string): Integer;
  //
    property __Items[idxFunction: Integer]: IASTFunction 
      read GetItems; default;
  end; { TASTFunctions }

  TASTMaker = class(TInterfacedObject, IASTFactory)
  public
    function CreateBlock: IASTBlock;
    function CreateExpression: IASTExpression;
    function CreateFunction: IASTFunction;
    function CreateStatement(statementType: TASTStatementType): IASTStatement;
    function CreateTerm(termType: TASTTermType): IASTTerm;
  end; { TASTMaker }

  TAST = class(TASTMaker, IAST)
  strict private
    fFunctions: IASTFunctions;
  public
    procedure AfterConstruction; override;
    function GetFunctions: IASTFunctions; inline;
  //  
    property __Functions: IASTFunctions
      read GetFunctions;
  end; { TAST }

{ exports }

function CreateAST: IAST;
begin
  Result := TAST.Create;
end; { CreateAST }

{ TASTTermConstant }

function TASTTermConstant.GetValue: Integer;
begin
  Result := fValue;
end; { TASTTermConstant.GetValue }

procedure TASTTermConstant.SetValue(const Value: Integer);
begin
  fValue := Value;
end; { TASTTermConstant.SetValue }

{ TASTTermVariable }

function TASTTermVariable.GetVariableIdx: Integer;
begin
  Result := fVariableIdx;
end; { TASTTermVariable.GetVariableIdx }

procedure TASTTermVariable.SetVariableIdx(const Value: Integer);
begin
  fVariableIdx := Value;
end; { TASTTermVariable.SetVariableIdx }

{ TASTTermFunctionCall }

procedure TASTTermFunctionCall.AfterConstruction;
begin
  inherited;
  fParameters := TExpressionList.Create;
end; { TASTTermFunctionCall.AfterConstruction }

procedure TASTTermFunctionCall.BeforeDestruction;
begin
  FreeAndNil(fParameters);
  inherited;
end; { TASTTermFunctionCall.BeforeDestruction }

function TASTTermFunctionCall.GetFunctionIdx: Integer;
begin
  Result := fFunctionIdx;
end; { TASTTermFunctionCall.GetFunctionIdx }

function TASTTermFunctionCall.GetParameters: TExpressionList;
begin
  Result := fParameters;
end; { TASTTermFunctionCall.GetParameters }

procedure TASTTermFunctionCall.SetFunctionIdx(const Value: Integer);
begin
  fFunctionIdx := Value;
end; { TASTTermFunctionCall.SetFunctionIdx }

{ TASTExpression }

function TASTExpression.GetBinaryOp: TBinaryOperation;
begin
  Result := fBinaryOp;
end; { TASTExpression.GetBinaryOp }

function TASTExpression.GetTerm1: IASTTerm;
begin
  Result := fTerm1;
end; { TASTExpression.GetTerm1 }

function TASTExpression.GetTerm2: IASTTerm;
begin
  Result := fTerm2;
end; { TASTExpression.GetTerm2 }

procedure TASTExpression.SetBinaryOp(const Value: TBinaryOperation);
begin
  fBinaryOp := Value;
end; { TASTExpression.SetBinaryOp }

procedure TASTExpression.SetTerm1(const Value: IASTTerm);
begin
  fTerm1 := Value;
end; { TASTExpression.SetTerm1 }

procedure TASTExpression.SetTerm2(const Value: IASTTerm);
begin
  fTerm2 := Value;
end; { TASTExpression.SetTerm2 }

{ TASTIfStatement }

function TASTIfStatement.GetCondition: IASTExpression;
begin
  Result := fCondition;
end; { TASTIfStatement.GetCondition }

function TASTIfStatement.GetElseBlock: IASTBlock;
begin
  Result := fElseBlock;
end; { TASTIfStatement.GetElseBlock }

function TASTIfStatement.GetThenBlock: IASTBlock;
begin
  Result := fThenBlock;
end; { TASTIfStatement.GetThenBlock }

procedure TASTIfStatement.SetCondition(const Value: IASTExpression);
begin
  fCondition := Value;
end; { TASTIfStatement.SetCondition }

procedure TASTIfStatement.SetElseBlock(const Value: IASTBlock);
begin
  fElseBlock := Value;
end; { TASTIfStatement.SetElseBlock }

procedure TASTIfStatement.SetThenBlock(const Value: IASTBlock);
begin
  fThenBlock := Value;
end; { TASTIfStatement.SetThenBlock }

{ TASTReturnStatement }

function TASTReturnStatement.GetExpression: IASTExpression;
begin
  Result := fExpression;
end; { TASTReturnStatement.GetExpression }

procedure TASTReturnStatement.SetExpression(const Value: IASTExpression);
begin
  fExpression := Value;
end; { TASTReturnStatement.SetExpression }

{ TASTBlock }

procedure TASTBlock.AfterConstruction;
begin
  inherited;
  fStatements := TStatementList.Create;
end; { TASTBlock.AfterConstruction }

procedure TASTBlock.BeforeDestruction;
begin
  FreeAndNil(fStatements);
  inherited;
end; { TASTBlock.BeforeDestruction }

function TASTBlock.GetStatements: TStatementList;
begin
  Result := fStatements;
end; { TASTBlock.GetStatements }

{ TASTFunction }

procedure TASTFunction.AfterConstruction;
begin
  inherited;
  fParamNames := TParameterList.Create;
end; { TASTFunction.AfterConstruction }

procedure TASTFunction.BeforeDestruction;
begin
  FreeAndNil(fParamNames);
  inherited;
end; { TASTFunction.BeforeDestruction }

function TASTFunction.GetBody: IASTBlock;
begin
  Result := fBody;
end; { TASTFunction.GetBody }

function TASTFunction.GetName: string;
begin
  Result := fName;
end; { TASTFunction.GetName }

function TASTFunction.GetParamNames: TParameterList;
begin
  Result := fParamNames;
end; { TASTFunction.GetParamNames }

procedure TASTFunction.SetBody(const Value: IASTBlock);
begin
  fBody := Value;
end; { TASTFunction.SetBody }

procedure TASTFunction.SetName(const Value: string);
begin
  fName := Value;
end; { TASTFunction.SetName }

procedure TASTFunction.SetParamNames(const Value: TParameterList);
begin
  fParamNames := Value;
end; { TASTFunction.SetParamNames }

{ TASTFunctions }

function TASTFunctions.Add(const Func: IASTFunction): Integer;
begin
  Result := fFunctions.Add(Func);
end; { TASTFunctions.Add }

procedure TASTFunctions.AfterConstruction;
begin
  inherited;
  fFunctions := specialize TList<IASTFunction>.Create;
end; { TASTFunctions.AfterConstruction }

procedure TASTFunctions.BeforeDestruction;
begin
  FreeAndNil(fFunctions);
  inherited;
end; { TASTFunctions.BeforeDestruction }

function TASTFunctions.Count: Integer;
begin
  Result := fFunctions.Count;
end; { TASTFunctions.Count }

function TASTFunctions.GetItems(idxFunction: Integer): IASTFunction;
begin
  Result := fFunctions[idxFunction];
end; { TASTFunctions.GetItems }

function TASTFunctions.IndexOf(const Name: string): Integer;
begin
  for 
    Result := 0 to Count - 1 
  do
    if 
      SameText(__Items[Result].__Name, Name) 
    then
      Exit;

  Result := -1;
end; { TASTFunctions.IndexOf }

{ TASTMaker }

function TASTMaker.CreateBlock: IASTBlock;
begin
  Result := TASTBlock.Create;
end; { TASTMaker.CreateBlock }

function TASTMaker.CreateExpression: IASTExpression;
begin
  Result := TASTExpression.Create;
end; { TASTMaker.CreateExpression }

function TASTMaker.CreateFunction: IASTFunction;
begin
  Result := TASTFunction.Create;
end; { TASTMaker.CreateFunction }

function TASTMaker.CreateStatement(
  statementType: TASTStatementType
  ): IASTStatement;
begin
  case 
    statementType 
  of
    stIf:     Result := TASTIfStatement.Create;
    stReturn: Result := TASTReturnStatement.Create;
    else 
      raise Exception.Create('<AST Factory> CreateStatement: Unexpected statement type');
  end;
end; { TASTMaker.CreateStatement }

function TASTMaker.CreateTerm(termType: TASTTermType): IASTTerm;
begin
  case 
    termType 
  of
    termConstant:     Result := TASTTermConstant.Create;
    termVariable:     Result := TASTTermVariable.Create;
    termFunctionCall: Result := TASTTermFunctionCall.Create;
    else 
      raise Exception.Create('<AST Factory> CreateTerm: Unexpected term type');
  end;
end; { TASTMaker.CreateTerm }

{ TAST }

procedure TAST.AfterConstruction;
begin
  inherited;
  fFunctions := TASTFunctions.Create;
end; { TAST.AfterConstruction }

function TAST.GetFunctions: IASTFunctions;
begin
  Result := fFunctions;
end; { TAST.GetFunctions }

end.
