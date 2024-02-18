unit SymbolListUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

USES Types,Classes,Contnrs,SysUtils,fpexprpars,StrUtils,
     ConsoleUnit,UtilsUnit,BeebDisDefsUnit,ParameterListUnit;

CONST
    DefPrefix   = 'L';
    DefAdSpace  = $00;  { Default address space }
    AdSpaceAny  = $FF;  { Match any address space }

TYPE
     TSymbolType	= (stLoaded, stGenerated, stAll);

     TSymbol	= class(TObject)
       Symbol	: STRING;
       Address	: DWORD;
       AdSpace  : BYTE;
       SType	: TSymbolType;
       RefCount : INTEGER;

       CONSTRUCTOR Create(ASymbol	: STRING;
       			          AAddress	: DWORD;
                          AAdSpace  : BYTE;
       			          ASType	: TSymbolType;
                          ARefCount : INTEGER);
       FUNCTION FormatSymbol(Column	: INTEGER;
                             Equate : STRING) : STRING;

       PROCEDURE IncRefCount(AIncrement : INTEGER = 1);
       FUNCTION Matches(AAddress    : DWORD;
                        AAdSpace    : BYTE) : BOOLEAN;
       FUNCTION LabMatches(ALabel   : STRING;
                           AAdSpace : BYTE) : BOOLEAN;


     END;

     TSymbolList = Class(TObjectList)
     PROTECTED
       FVerbosity   : BYTE;
       FName	    : STRING;
       FMaxAddr     : DWORD;
       FMinAddr	    : DWORD;
       FZeroAddr    : DWORD;
       FParameters  : TParameterList;
       FSymbolFiles : TStringList;
       FZeroBased   : BOOLEAN;


       FUNCTION GetAddressFromIndex(Index : INTEGER) : DWORD;
       FUNCTION GetSymbolFromIndex(Index : INTEGER) : STRING;
       FUNCTION AddAddress(Address	    : DWORD;
                           AdSpace      : BYTE;
			               ALabel	    : STRING;
			               ASType	    : TSymbolType = stGenerated;
                           ARefCount    : INTEGER = 0;
                           Prefix       : STRING = DefPrefix) : STRING;
       FUNCTION IndexOfLabel(ALabel	    : STRING;
                             AAdSpace   : BYTE{ = DefAdSpace}) : INTEGER;
       FUNCTION IndexOfAddress(AAddress	: DWORD;
                               AAdSpace : BYTE{ = DefAdSpace}) : INTEGER;

       FUNCTION AddSymbol(ALabel	: STRING;
			              AAddress	: DWORD;
                          AAdSpace  : BYTE;
			              ASType	: TSymbolType = stGenerated;
                          ARefCount : INTEGER = 0) : INTEGER;
       FUNCTION ProcessComment(Line         : STRING;
                               CommentSeq   : STRING;
                               BeginOnly    : BOOLEAN = FALSE) : STRING;
    PUBLIC
       PROPERTY Symbols[Index	: INTEGER]	: STRING READ GetSymbolFromIndex;
       PROPERTY Addresses[Index	: INTEGER] 	: DWORD READ GetAddressFromIndex;
       PROPERTY Verbosity 			        : BYTE READ FVerbosity WRITE FVerbosity;
       PROPERTY Name				        : STRING READ FName WRITE FName;
       PROPERTY MinAddr				        : DWORD READ FMinAddr;
       PROPERTY MaxAddr				        : DWORD READ FMaxAddr;
       PROPERTY ZeroAddr				    : DWORD READ FZeroAddr WRITE FZeroAddr;
       PROPERTY ZeroBased                   : BOOLEAN READ FZeroBased WRITE FZeroBased;

       CONSTRUCTOR Create(AName		    : STRING;
                          Parameters    : TParameterList);
       DESTRUCTOR Destroy; OVERRIDE;
       PROCEDURE SetRange(AMaxAddr	: DWORD;
			              AMinAddr	: DWORD);
       PROCEDURE DeleteAddress(Address	: DWORD;
                               AdSpace  : BYTE);
       FUNCTION SafeAddAddress(Address	: DWORD;
                               AdSpace  : BYTE;
			                   ALabel	: STRING;
                               IsEntry  : BOOLEAN = FALSE) : STRING;

       FUNCTION AddOrRenameAddress(Address	: DWORD;
                                   AdSpace  : BYTE;
			                       ALabel	: STRING;
                                   IsEntry  : BOOLEAN = FALSE) : STRING;

       FUNCTION GetSymbol(Address	: DWORD;
                          AdSpace   : BYTE;
			              CanCreate	: BOOLEAN = TRUE;
                          ARefCount : INTEGER = 0;
                          Prefix    : STRING = DefPrefix) : STRING;

       FUNCTION GetSymbolValue(Address	    : DWORD;
                               AdSpace      : BYTE;
       			               CanCreate    : BOOLEAN = TRUE;
                               ARefCount    : INTEGER = 0) : STRING;

       FUNCTION GetSymbolRefs(Address	    : DWORD;
                              AdSpace       : BYTE) : INTEGER;

       PROCEDURE LoadLabels(FileName	: STRING);
       FUNCTION DumpList : STRING;
       PROCEDURE SortSymbols;
       FUNCTION InRange(AAddress   : DWORD) : BOOLEAN;
       PROCEDURE RemoveBlanks;
       PROCEDURE SaveSymbolsToFile(AFileName	: STRING;
				                   ASymbolType	: TSymbolType;
                                   Equate       : STRING);
       PROCEDURE AddFile(AFileName  : STRING);
       PROCEDURE ImportFiles;
       PROCEDURE ResolveFromList(OtherList  : TSymbolList);
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

CONSTRUCTOR TSymbol.Create(ASymbol	    : STRING;
       			           AAddress	    : DWORD;
                           AAdSpace     : BYTE;
       			           ASType	    : TSymbolType;
                           ARefCount    : INTEGER);

BEGIN;
  INHERITED Create;
  Symbol:=ASymbol;
  Address:=AAddress;
  AdSpace:=AAdSpace;
  SType:=ASType;
  RefCount:=ARefCount;
END;

FUNCTION TSymbol.FormatSymbol(Column    : INTEGER;
                              Equate    : STRING) : STRING;

BEGIN;
  Result:=Format('%s ',[Symbol]);
  PadToAdd(Result,Column,Format('%s $%4.4X',[Equate,Address]));
END;

PROCEDURE TSymbol.IncRefCount(AIncrement : INTEGER = 1);

BEGIN;
  RefCount:=RefCount+AIncrement;
END;

FUNCTION TSymbol.Matches(AAddress    : DWORD;
                         AAdSpace    : BYTE) : BOOLEAN;

BEGIN;
  Result:=((AAdSpace=AdSpaceAny) OR (AAdSpace = AdSpace)) AND (AAddress = Address);
END;

FUNCTION TSymbol.LabMatches(ALabel   : STRING;
                            AAdSpace : BYTE) : BOOLEAN;

BEGIN;
  Result:=((AAdSpace=AdSpaceAny) OR (AAdSpace = AdSpace)) AND
           (LowerCase(ALabel) = LowerCase(Symbol));
END;

CONSTRUCTOR TSymbolList.Create(AName		: STRING;
                               Parameters   : TParameterList);

BEGIN;
  INHERITED Create;
  FName:=AName;
  FMaxAddr:=$FFFFFFFF;
  FMinAddr:=0;
  OwnsObjects:=TRUE;
  FParameters:=Parameters;
  FSymbolFiles:=TStringList.Create;
  FZeroBased:=FALSE;
  FZeroAddr:=0;
END;

DESTRUCTOR TSymbolList.Destroy;

BEGIN;
  FSymbolFiles.Free;
  INHERITED Destroy;
END;

PROCEDURE TSymbolList.SetRange(AMaxAddr	: DWORD;
			                   AMinAddr	: DWORD);

BEGIN;
  FMaxAddr:=AMaxAddr;
  FMinAddr:=AMinAddr;
END;

FUNCTION TSymbolList.IndexOfLabel(ALabel	: STRING;
                                  AAdSpace  : BYTE{ = DefAdSpace}) : INTEGER;

VAR	ItemNo	: INTEGER;

BEGIN;
  ItemNo:=Count-1;

  WHILE ((ItemNo>-1) AND NOT TSymbol(Items[ItemNo]).LabMatches(ALabel,AAdSpace)) DO
    ItemNo:=ItemNo-1;

  Result:=ItemNo;
END;

FUNCTION TSymbolList.IndexOfAddress(AAddress	: DWORD;
                                    AAdSpace    : BYTE{ = DefAdSpace}) : INTEGER;

VAR	ItemNo	: INTEGER;

BEGIN;
  ItemNo:=Count-1;

  WHILE ((ItemNo>-1) AND NOT TSymbol(Items[ItemNo]).Matches(AAddress,AAdSpace)) DO
    ItemNo:=ItemNo-1;

  Result:=ItemNo;
END;

FUNCTION TSymbolList.AddSymbol(ALabel	    : STRING;
			                   AAddress	    : DWORD;
                               AAdSpace     : BYTE;
			                   ASType	    : TSymbolType = stGenerated;
                               ARefCount    : INTEGER = 0) : INTEGER;

VAR	ToAdd	: TSymbol;

BEGIN;
  ToAdd:=TSymbol.Create(ALabel,AAddress,AAdSpace,ASType,ARefCount);
  Result:=Add(ToAdd);
END;

FUNCTION TSymbolList.AddAddress(Address	    : DWORD;
                                AdSpace     : BYTE;
			   	                ALabel	    : STRING;
				                ASType	    : TSymbolType = stGenerated;
                                ARefCount   : INTEGER = 0;
                                Prefix      : STRING = DefPrefix) : STRING;

VAR SymAIndex   : INTEGER;
    SymLIndex   : INTEGER;

BEGIN;
  Result:='';

  IF ((Address>=FMinAddr) AND (Address<=FMaxAddr)) THEN
  BEGIN;
    IF (ALabel='') THEN
    BEGIN;
      IF ((FZeroBased) AND (Address>=ZeroAddr) AND (Prefix=DefPrefix)) THEN
        ALabel:=Format('%s%4.4X',[Trim(Prefix),Address-ZeroAddr])
      ELSE
        ALabel:=Format('%s%4.4X',[Trim(Prefix),Address]);
    END;

    SymLIndex:=IndexOfLabel(ALabel,AdSpace);

    IF (SymLIndex<0) THEN
    BEGIN;
      SymAIndex:=IndexOfAddress(Address,AdSpace);
      IF (SymAIndex<0) THEN
      BEGIN;
        SymAIndex:=AddSymbol(ALabel,Address,AdSpace,ASType,ARefCount);

        WriteLnFmtV(FVerbosity,VBDebug,'%s:Label %s Address %4.4x, space=%2.2X',[FName,ALabel,Address,AdSpace]);

        Result:=TSymbol(Items[SymAIndex]).Symbol;
      END
      ELSE
        TSymbol(Items[SymAIndex]).IncRefCount;
    END
    ELSE
      TSymbol(Items[SymLIndex]).IncRefCount;
  END
END;

FUNCTION TSymbolList.SafeAddAddress(Address	: DWORD;
                                    AdSpace : BYTE;
			                        ALabel	: STRING;
                                    IsEntry : BOOLEAN) : STRING;

VAR	SymbolIndex	: INTEGER;

BEGIN;
  SymbolIndex:=IndexOfAddress(Address,AdSpace);

  //WriteLn(SymbolIndex);
  IF (SymbolIndex < 0) THEN
    AddAddress(Address,AdSpace,ALabel);

  SymbolIndex:=IndexOfAddress(Address,AdSpace);

  IF ((IsEntry) AND (SymbolIndex>0)) THEN
    TSymbol(Items[SymbolIndex]).IncRefCount(1);

  Result:=Symbols[SymbolIndex];
END;

FUNCTION TSymbolList.AddOrRenameAddress(Address	    : DWORD;
                                        AdSpace     : BYTE;
 	                                    ALabel	    : STRING;
                                        IsEntry     : BOOLEAN = FALSE) : STRING;
VAR	SymbolIndex	: INTEGER;

BEGIN;
  SymbolIndex:=IndexOfAddress(Address,AdSpace);

  IF (SymbolIndex < 0) THEN
    SafeAddAddress(Address,AdSpace,ALabel,IsEntry)
  ELSE
    TSymbol(Items[SymbolIndex]).Symbol:=ALabel;
END;

PROCEDURE TSymbolList.DeleteAddress(Address	: DWORD;
                                    AdSpace : BYTE);

VAR	ItemNo	: INTEGER;

BEGIN;
  ItemNo:=IndexOfAddress(Address,AdSpace);

  IF (ItemNo>-1) THEN
    Delete(ItemNo);
END;

FUNCTION TSymbolList.GetSymbol(Address		: DWORD;
                               AdSpace      : BYTE;
			                   CanCreate	: BOOLEAN = TRUE;
                               ARefCount    : INTEGER = 0;
                               Prefix       : STRING = DefPrefix) : STRING;

VAR	SymbolIndex	: INTEGER;

BEGIN;
  SymbolIndex:=IndexOfAddress(Address,AdSpace);
  IF (SymbolIndex >= 0) THEN
  BEGIN
    Result:=Symbols[SymbolIndex];
    TSymbol(Items[SymbolIndex]).IncRefCount(ARefCount);
  END
  ELSE
  BEGIN;
    IF (CanCreate) THEN
      Result:=AddAddress(Address,AdSpace,'',stGenerated,ARefCount,Prefix)
    ELSE
      Result:='';
  END;
END;

FUNCTION TSymbolList.GetSymbolValue(Address	    : DWORD;
                                    AdSpace     : BYTE;
       			                    CanCreate	: BOOLEAN = TRUE;
                                    ARefCount   : INTEGER = 0) : STRING;

BEGIN;
  Result:=GetSymbol(Address,AdSpace,CanCreate);
  IF(Result='') THEN
    Result:=Format('$%4.4X',[Address]);
END;

FUNCTION TSymbolList.GetSymbolRefs(Address	    : DWORD;
                                   AdSpace      : BYTE) : INTEGER;

VAR	SymbolIndex	: INTEGER;

BEGIN;
  SymbolIndex:=IndexOfAddress(Address,AdSpace);

  IF (SymbolIndex >= 0) THEN
    Result:=TSymbol(Items[SymbolIndex]).RefCount
  ELSE
    Result:=0;
END;

FUNCTION TSymbolList.ProcessComment(Line         : STRING;
                                    CommentSeq   : STRING;
                                    BeginOnly    : BOOLEAN = FALSE) : STRING;

VAR CommentPos  : INTEGER;

BEGIN;
  Result:=Line;                     { Assume no comment }
  CommentPos:=Pos(CommentSeq,Line);
  IF (((CommentPos > 0) AND (NOT BeginOnly)) OR
      ((CommentPos = 1) AND BeginOnly)) THEN
    SetLength(Result,CommentPos-1);
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
    EquateStr   : STRING;

BEGIN;
  LabelFile:=TStringList.Create;
  FParser := TFPExpressionParser.Create(nil);
  TRY
    EquateStr:=Format(' %s ',[FParameters[mlEquate]]);
    IF (FileExists(FileName)) THEN
    BEGIN;
      LabelFile.LoadFromFile(FileName);

      FOR LineNo:=0 TO (LabelFile.Count-1) DO
      BEGIN;
        Line:=Trim(LabelFile.Strings[LineNo]);

        Line:=ProcessComment(Line,';',FALSE);
        Line:=ProcessComment(Line,'*',TRUE);

        Line:=StringReplace(Line,#9,' ',[rfReplaceAll,rfIgnoreCase]);
        Line:=StringReplace(Line,'&','$',[rfReplaceAll,rfIgnoreCase]);
        Line:=StringReplace(Line,EquateStr,'=',[rfIgnoreCase]);

        SpacePos:=Pos('=',Line);
        IF (SpacePos>0) THEN
        BEGIN;
          {Extract label and value from line}
          ALabel:=Trim(Copy(Line,1,SpacePos-1));
          AddrStr:=Trim(Copy(Line,SpacePos+1,MaxInt));

          {Discard any of the value part after a space, to discard space}
          {seperated comments}
          SpacePos:=Pos(' ',AddrStr);
          IF (SpacePos>0) THEN
            AddrStr:=Trim(Copy(AddrStr,1,SpacePos-1));

          {Convert address to numeric value}
          Address:=StrTointDef(AddrStr,-1);

          // If simple conversion does not work try evaluating it as an expression.
          IF (Address = -1) THEN
          BEGIN;
            FParser.Expression:=AddrStr;
            Address:=FParser.AsInteger;
          END;

          IF ((Address>-1) AND (Address<$10000)) THEN
          BEGIN
            AddAddress(Address,DefAdSpace,ALabel,stLoaded);
            WriteLnFmtV(FVerbosity,VBDebug,'Loaded lable %s value $%4.4X',[ALabel,Address]);
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
    Symbol      : TSymbol;

BEGIN;
  Result:='';

  FOR SymbolNo:=0 TO (Count-1) DO
  BEGIN;
    Symbol:=TSymbol(Items[SymbolNo]);
    Result:=Result+Format('%4.4X, %2.2x %s %d',[Symbol.Address,Symbol.AdSpace,Symbol.Symbol,Symbol.RefCount])+#$0d+#$0a;
  end;

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

FUNCTION TSymbolList.InRange(AAddress   : DWORD) : BOOLEAN;

BEGIN;
  Result:=((AAddress >= FMinAddr) AND (AAddress <= FMaxAddr));
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
				                        ASymbolType	: TSymbolType;
                                        Equate      : STRING);

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
          OutList.Add(FormatSymbol(FirstColumn,Equate));

    OutList.SaveToFile(AFileName);
  FINALLY
    OutList.Free;
  END;
END;

PROCEDURE TSymbolList.AddFile(AFileName  : STRING);

BEGIN;
  FSymbolFiles.Add(AFileName);
END;

PROCEDURE TSymbolList.ImportFiles;

VAR FileNo  : INTEGER;

BEGIN;
  FOR FileNo:=0 TO (FSymbolFiles.Count-1) DO
  BEGIN;
    WriteLnFmtv(FVerbosity,VBNormal,'Loading symbols from %s',[FSymbolFiles[FileNo]]);
    LoadLabels(FSymbolFiles[FileNo]);
  END;

  FSymbolFiles.Clear;
END;

PROCEDURE TSymbolList.ResolveFromList(OtherList  : TSymbolList);

VAR Address : DWORD;
    AdSpace : BYTE;
    Symbol  : STRING;
    ItemNo  : INTEGER;

BEGIN;
  FOR ItemNo:=0 TO (Count-1) DO
  BEGIN;
    Address:=TSymbol(Items[ItemNo]).Address;
    AdSpace:=TSymbol(Items[ItemNo]).AdSpace;
    Symbol:=OtherList.GetSymbol(Address,AdSpace,FALSE);
    IF (Symbol<>'') THEN
      TSymbol(Items[ItemNo]).Symbol:=Symbol;
  END;
END;


end.
