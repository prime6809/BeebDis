unit SymbolListUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

USES Types,Classes,Contnrs,SysUtils,fpexprpars,
     ConsoleUnit,UtilsUnit,BeebDisDefsUnit;

TYPE
     TSymbolType	= (stLoaded, stGenerated, stAll);

     TSymbol	= class(TObject)
       Symbol	: STRING;
       Address	: DWORD;
       SType	: TSymbolType;
       CONSTRUCTOR Create(ASymbol	: STRING;
       			          AAddress	: DWORD;
       			          ASType	: TSymbolType);
       FUNCTION FormatSymbol(Column	: INTEGER) : STRING;

     END;

     TSymbolList = Class(TObjectList)
     PROTECTED
       FVerbose	: BOOLEAN;
       FName	: STRING;
       FMaxAddr : DWORD;
       FMinAddr	: DWORD;

       FUNCTION GetAddressFromIndex(Index : INTEGER) : DWORD;
       FUNCTION GetSymbolFromIndex(Index : INTEGER) : STRING;
       FUNCTION AddAddress(Address	: DWORD;
			               ALabel	: STRING;
			               ASType	: TSymbolType = stGenerated) : STRING;
       FUNCTION IndexOfLabel(ALabel	: STRING) : INTEGER;
       FUNCTION IndexOfAddress(AAddress	: DWORD) : INTEGER;

       FUNCTION AddSymbol(ALabel	: STRING;
			              AAddress	: DWORD;
			              ASType	: TSymbolType = stGenerated) : INTEGER;

    PUBLIC
       PROPERTY Symbols[Index	: INTEGER]	: STRING READ GetSymbolFromIndex;
       PROPERTY Addresses[Index	: INTEGER] 	: DWORD READ GetAddressFromIndex;
       PROPERTY Verbose 			: BOOLEAN READ FVerbose WRITE FVerbose;
       PROPERTY Name				: STRING READ FName WRITE FName;
       PROPERTY MinAddr				: DWORD READ FMinAddr;
       PROPERTY MaxAddr				: DWORD READ FMaxAddr;

       CONSTRUCTOR Create(AName		: STRING);
       PROCEDURE SetRange(AMaxAddr	: DWORD;
			              AMinAddr	: DWORD);
       PROCEDURE DeleteAddress(Address	: DWORD);
       FUNCTION SafeAddAddress(Address	: DWORD;
			                   ALabel	: STRING) : STRING;

       FUNCTION GetSymbol(Address	: DWORD;
			              CanCreate	: BOOLEAN = TRUE) : STRING;
       FUNCTION GetSymbolValue(Address	: DWORD;
       			               CanCreate: BOOLEAN = TRUE) : STRING;

       PROCEDURE LoadLabels(FileName	: STRING);
       FUNCTION DumpList : STRING;
       PROCEDURE SortSymbols;
       FUNCTION InRange(First	: DWORD;
			            Last	: DWORD) : BOOLEAN;
       PROCEDURE RemoveBlanks;
       PROCEDURE SaveSymbolsToFile(AFileName	: STRING;
				                   ASymbolType	: TSymbolType);
     END;

implementation

FUNCTION SymbolCompare(Item1	: Pointer;
		               Item2	: Pointer): Integer;

VAR	Address1	: DWORD;
	Address2	: DWORD;

BEGIN;
  Address1:=TSymbol(Item1).Address;
  Address2:=TSymbol(Item2).Address;

  IF (Address1 < Address2) THEN
    Result:=-1
  ELSE IF (Address1 > Address2) THEN
    Result:=1
  ELSE
    Result:=0;
END;

CONSTRUCTOR TSymbol.Create(ASymbol	: STRING;
       			           AAddress	: DWORD;
       			           ASType	: TSymbolType);

BEGIN;
  INHERITED Create;
  Self.Symbol:=ASymbol;
  Self.Address:=AAddress;
  Self.SType:=ASType;
END;

FUNCTION TSymbol.FormatSymbol(Column	: INTEGER) : STRING;

BEGIN;
  Result:=Format('%s',[Symbol]);
  PadToAdd(Result,Column,Format('= $%4.4X',[Address]));
END;

CONSTRUCTOR TSymbolList.Create(AName		: STRING);

BEGIN;
  INHERITED Create;
  FName:=AName;
  FMaxAddr:=$FFFFFFFF;
  FMinAddr:=0;
  OwnsObjects:=TRUE;
END;

PROCEDURE TSymbolList.SetRange(AMaxAddr	: DWORD;
			       AMinAddr	: DWORD);

BEGIN;
  FMaxAddr:=AMaxAddr;
  FMinAddr:=AMinAddr;
END;

FUNCTION TSymbolList.IndexOfLabel(ALabel	: STRING) : INTEGER;

VAR	ItemNo	: INTEGER;

BEGIN;
  ItemNo:=Count-1;
  ALabel:=LowerCase(ALabel);

  WHILE ((ItemNo>-1) AND (ALabel<>LowerCase(TSymbol(Items[ItemNo]).Symbol))) DO
    ItemNo:=ItemNo-1;

  Result:=ItemNo;
END;

FUNCTION TSymbolList.IndexOfAddress(AAddress	: DWORD) : INTEGER;

VAR	ItemNo	: INTEGER;

BEGIN;
  ItemNo:=Count-1;

  WHILE ((ItemNo>-1) AND (AAddress<>TSymbol(Items[ItemNo]).Address)) DO
    ItemNo:=ItemNo-1;

  Result:=ItemNo;
END;

FUNCTION TSymbolList.AddSymbol(ALabel	: STRING;
			                   AAddress	: DWORD;
			                   ASType	: TSymbolType = stGenerated) : INTEGER;

VAR	ToAdd	: TSymbol;

BEGIN;
  ToAdd:=TSymbol.Create(ALabel,AAddress,ASType);
  Result:=Add(ToAdd);
END;

FUNCTION TSymbolList.AddAddress(Address	: DWORD;
			   	                ALabel	: STRING;
				                ASType	: TSymbolType = stGenerated) : STRING;

BEGIN;
  Result:='';

  IF ((Address>=FMinAddr) AND (Address<=FMaxAddr)) THEN
  BEGIN;
    IF (ALabel='') THEN
      ALabel:=Format('L%4.4X',[Address]);

    IF (IndexOfLabel(ALabel)<0) THEN
    BEGIN;
      AddSymbol(ALabel,Address,ASType);

      IF (Verbose) THEN
        WriteLnFmt('%s:Label %s Address %4.4x',[FName,ALabel,Address]);

      Result:=TSymbol(Items[IndexOfAddress(Address)]).Symbol;
    END;
  END
END;

FUNCTION TSymbolList.SafeAddAddress(Address	: DWORD;
			                        ALabel	: STRING) : STRING;

VAR	SymbolIndex	: INTEGER;

BEGIN;
  SymbolIndex:=IndexOfAddress(Address);
  IF (SymbolIndex >= 0) THEN
    Result:=Symbols[SymbolIndex]
  ELSE
    Result:=AddAddress(Address,ALabel);
END;

PROCEDURE TSymbolList.DeleteAddress(Address	: DWORD);

VAR	ItemNo	: INTEGER;

BEGIN;
  ItemNo:=IndexOfAddress(Address);

  IF (ItemNo>-1) THEN
    Delete(ItemNo);
END;

FUNCTION TSymbolList.GetSymbol(Address		: DWORD;
			                   CanCreate	: BOOLEAN = TRUE) : STRING;

VAR	SymbolIndex	: INTEGER;

BEGIN;
  SymbolIndex:=IndexOfAddress(Address);
  IF (SymbolIndex >= 0) THEN
    Result:=Symbols[SymbolIndex]
  ELSE
  BEGIN;
    IF (CanCreate) THEN
      Result:=AddAddress(Address,'')
    ELSE
      Result:='';
  END;
END;

FUNCTION TSymbolList.GetSymbolValue(Address	: DWORD;
       			            CanCreate	: BOOLEAN = TRUE) : STRING;

BEGIN;
  Result:=GetSymbol(Address,CanCreate);
  IF(Result='') THEN
    Result:=Format('$%4.4X',[Address]);
END;

PROCEDURE TSymbolList.LoadLabels(FileName	: STRING);

VAR	LabelFile	: TStringList;
	LineNo		: INTEGER;
	Line		: STRING;
	SpacePos	: INTEGER;
    ALabel		: STRING;
    AddrStr		: STRING;
    Address		: INTEGER;
    CommentPos  : INTEGER;
    FParser     : TFPExpressionParser;

BEGIN;
  LabelFile:=TStringList.Create;
  FParser := TFPExpressionParser.Create(nil);
  TRY
    IF (FileExists(FileName)) THEN
    BEGIN;
      LabelFile.LoadFromFile(FileName);

      FOR LineNo:=0 TO (LabelFile.Count-1) DO
      BEGIN;
        Line:=Trim(LabelFile.Strings[LineNo]);

        CommentPos:=Pos(';',Line);
        IF (CommentPos>0) THEN
          SetLength(Line,CommentPos-1);

        Line:=StringReplace(Line,#9,' ',[rfReplaceAll,rfIgnoreCase]);
        Line:=StringReplace(Line,'&','$',[rfReplaceAll,rfIgnoreCase]);

        SpacePos:=Pos('=',Line);
        IF (SpacePos>0) THEN
        BEGIN;
          ALabel:=Trim(Copy(Line,1,SpacePos-1));
          AddrStr:=Trim(Copy(Line,SpacePos+1,MaxInt));
          Address:=StrTointDef(AddrStr,-1);

          // If simple conversion does not work try evaluating it as an expression.
          IF (Address = -1) THEN
          BEGIN;
            FParser.Expression:=AddrStr;
            Address:=FParser.AsInteger;
          END;

          IF ((Address>-1) AND (Address<$10000)) THEN
          BEGIN
            AddAddress(Address,ALabel,stLoaded);
            IF(FParser.Identifiers.IndexOfIdentifier(ALabel) < 0) THEN
              FParser.Identifiers.AddIntegerVariable(ALabel,Address);
          END;
        END;
      END;
    END;
  FINALLY
    LabelFile.Free;
    FParser.Free;
  END;
END;

FUNCTION TSymbolList.DumpList : STRING;

VAR	SymbolNo	: INTEGER;

BEGIN;
  Result:='';

  FOR SymbolNo:=0 TO (Count-1) DO
    Result:=Result+Format('%4.4X, %s',[Addresses[SymbolNo],Symbols[SymbolNo]])+#$0d+#$0a;

END;

FUNCTION TSymbolList.GetAddressFromIndex(Index : INTEGER) : DWORD;

BEGIN;
  IF ((Index>=0) AND (Index<Count)) THEN
    Result:=TSymbol(Items[Index]).Address
  ELSE
    Result:=0;
END;

FUNCTION TSymbolList.GetSymbolFromIndex(Index : INTEGER) : STRING;

BEGIN;
  IF ((Index>=0) AND (Index<Count)) THEN
    Result:=TSymbol(Items[Index]).Symbol
  ELSE
    Result:='';
END;


PROCEDURE TSymbolList.SortSymbols;

BEGIN;
  Sort(SymbolCompare);
END;

FUNCTION TSymbolList.InRange(First	: DWORD;
			                 Last	: DWORD) : BOOLEAN;

BEGIN;
END;

PROCEDURE TSymbolList.RemoveBlanks;

VAR	ItemNo	: INTEGER;

BEGIN;
  ItemNo:=Count-1;
  WHILE (ItemNo>-1) DO
  BEGIN;
    IF (Symbols[ItemNo]='') THEN
      Delete(ItemNo);

    ItemNo:=ItemNo-1;
  END;
END;

PROCEDURE TSymbolList.SaveSymbolsToFile(AFileName	: STRING;
				                        ASymbolType	: TSymbolType);

VAR	OutList	: TStringList;
    ItemNo	: INTEGER;
//	Symbol	: TSymbol;

BEGIN;
  OutList:=TStringList.Create;

  TRY
    SortSymbols;
    FOR ItemNo:=0 TO (Count-1) DO
      WITH TSymbol(Items[ItemNo]) DO
        IF ((SType=ASymbolType) OR (ASymbolType=stAll)) THEN
          OutList.Add(FormatSymbol(FirstColumn));

    OutList.SaveToFile(AFileName);
  FINALLY
    OutList.Free;
  END;
END;

end.
