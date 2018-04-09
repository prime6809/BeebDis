unit MemoryListUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

USES Types,SysUtils,Classes,Contnrs, CPUMemoryUnit, SymbolListUnit, UtilsUnit,
     BeebDisDefsUnit;

TYPE    TItemType = (tyCode, tyDataByte, tyDataWord, tyDataDWord, tyDataString,
		             tyDataStringTerm,tyDataStringTermHi,tyDataStringTermHiZ,
                     tyDataWordEntry, tyDataWordRTSEntry);

	TLocation = Class(TObject)
    PROTECTED
      FItemType     : TItemType;    { Item type }
      FItemTypeStr  : STRING;
      PROCEDURE SetItemType(InType  : TItemType);
    PUBLIC
      Address	    : DWORD;	{ Address }
	  Length	    : INTEGER;	{ Length of data }
	  Text		    : STRING;	{ Disassembled text }
	  NewLineAfter	: BOOLEAN;	{ Newline after this one ? }

      PROPERTY ItemType     : TItemType READ FItemType WRITE SetItemType;
      PROPERTY ItemTypeStr  : STRING READ FItemTypeStr;

	  CONSTRUCTOR Create(PAddress	: DWORD;
			             PLength	: INTEGER;
			             PText	    : STRING;
			             PItemType	: TItemType;
			             PNewLineA  : BOOLEAN); overload;
      FUNCTION IsInRange(AAddress	: DWORD) : BOOLEAN;
	  FUNCTION Split(AAddress	: DWORD;
		             ALabel		: STRING) : TLocation;
      FUNCTION GetItemSize : INTEGER;
      FUNCTION AsString : STRING;
	END;

    TMemoryList = Class(TObjectList)
    PRIVATE
      FListing	    : TStringList;
	  FMemory	    : TCPUmemory;
	  FSymbols	    : TSymbolList;
      FEntryPoints	: TSymbolList;

      FUNCTION GetListing : TStringList;
      FUNCTION Add(ToAdd	: TLocation) : INTEGER;
	  FUNCTION FixupString(Item	: TLocation) : STRING;
	  FUNCTION FormatData(Item	: TLocation) : STRING;
	  FUNCTION FormatCode(Item	: TLocation) : STRING;
	  FUNCTION FormatDataOutput(Item	: TLocation) : STRING;
      FUNCTION GetLocation(Which	: INTEGER) : TLocation;
	  FUNCTION SearchInsert(SymbolNo: INTEGER) : TLocation;
      FUNCTION NewSymbolInRange(SymbolNo    : INTEGER;
                                Location    : TLocation) : TLocation;
      FUNCTION GetPC : DWORD;
      PROCEDURE SetPC(NewPC : DWORD);
    PROTECTED
      PROPERTY Locations[Which	: INTEGER] : TLocation READ GetLocation;
    PUBLIC
      OutputFileName	: STRING;
	  PROPERTY Listing : TStringList READ GetListing;
      PROPERTY PC : DWORD READ GetPC WRITE SetPC;

      CONSTRUCTOR Create(Memory	    : TCPUmemory;
		                 Symbols	: TSymbolList;
                         Entries	: TSymbolList);
	  PROCEDURE AddData(DataType	: TItemType;
	                    Location	: STRING;
		                Count	    : DWORD;
                        Terminator	: BYTE = 0);
      PROCEDURE AddCode(Location	: DWORD;
		                Count	    : DWORD;
       		            CodeLine	: STRING;
		                NewLine	: BOOLEAN);
      PROCEDURE AddEntry(Location	: STRING);
      PROCEDURE Dump;
    END;

implementation

CONST	IndentStr	= '        ';


CONSTRUCTOR TLocation.Create(PAddress	: DWORD;
			                 PLength	: INTEGER;
			                 PText	    : STRING;
			                 PItemType	: TItemType;
			                 PNewLineA  : BOOLEAN);

BEGIN;
  INHERITED Create;
  Address:=PAddress;
  Length:=PLength;
  Text:=PText;
  ItemType:=PItemType;
  NewLineAfter:=PNewLineA;
END;

PROCEDURE TLocation.SetItemType(InType  : TItemType);

BEGIN;
  FItemType:=InType;
  CASE FItemType OF
    tyCode                      : FItemTypeStr:='tyCode';
    tyDataByte                  : FItemTypeStr:='tyDataByte';
    tyDataWord                  : FItemTypeStr:='tyDataWord';
    tyDataDWord                 : FItemTypeStr:='tyDataDWord';
    tyDataString                : FItemTypeStr:='tyDataString';
    tyDataStringTerm            : FItemTypeStr:='tyDataStringTerm';
    tyDataStringTermHi          : FItemTypeStr:='tyDataStringTermHi';
    tyDataWordEntry             : FItemTypeStr:='tyDataWordEntry';
    tyDataWordRTSEntry          : FItemTypeStr:='tyDataWordRTSEntry';
  END;
END;

FUNCTION TLocation.IsInRange(AAddress	: DWORD) : BOOLEAN;

BEGIN;
  Result:=((AAddress>=Address) AND (AAddress<(Address+Length)));
END;

FUNCTION TLocation.Split(AAddress	: DWORD;
			             ALabel		: STRING) : TLocation;

VAR	NewLength	: DWORD;

BEGIN;
  IF (IsInRange(AAddress)) THEN
  BEGIN;
    NewLength:=AAddress-Address;
    Result:=TLocation.Create(AAddress,Length-NewLength,'',ItemType,NewLineAfter);
    Length:=NewLength;
  END
  ELSE
    Result:=Self;
END;

FUNCTION TLocation.GetItemSize : INTEGER;

BEGIN;
  CASE ItemType OF
    tyDataByte		    : Result:=1;
    tyDataWord,
    tyDataWordEntry,
    tyDataWordRTSEntry  : Result:=2;
    tyDataDWord		    : Result:=4;
    tyDataString,
    tyDataStringTerm,
    tyDataStringTermHi,
    tyDataStringTermHiZ : Result:=1;
  ELSE
    Result:=0;
  END;
END;

FUNCTION TLocation.AsString : STRING;

BEGIN;
  Result:=Format('Addr:$%4.4X, Size:%d, Length:%4d, Type:%s, Text:%s',
                 [Address,GetItemSize,Length,ItemTypeStr,Text]);

END;


{ Note this function inverts the values returned for items that are not equal }
{ so that the sort order is reversed as we want lowest -> highest address.    }
FUNCTION MemoryListCompare(Item1	: Pointer;
			               Item2	: Pointer) : INTEGER;

VAR	Location1	: TLocation;
	Location2	: TLocation;

BEGIN;
  Location1:=TLocation(Item1);
  Location2:=TLocation(Item2);

  IF (Location1.Address > Location2.Address) THEN
    Result:=1
  ELSE IF (Location1.Address < Location2.Address) THEN
    Result:=-1
  ELSE
    Result:=0;
END;

CONSTRUCTOR TMemoryList.Create(Memory	: TCPUmemory;
			                   Symbols	: TSymbolList;
                               Entries	: TSymbolList);

BEGIN;
  INHERITED Create;
  FListing:=TStringList.Create;
  FMemory:=Memory;
  FSymbols:=Symbols;
  FEntryPoints:=Entries;
  OutputFileName:='';
END;

FUNCTION TMemoryList.GetListing : TStringList;

VAR	ItemNo		: INTEGER;
	CodeLabel   : STRING;
	Item		: TLocation;
	Line		: STRING;

BEGIN;
  Sort(MemoryListCompare);
  Line:='';

  {Output symbols that are outside our range}
  FSymbols.SortSymbols;
  FOR ItemNo:=0 TO (FSymbols.Count-1) DO
   IF ((FSymbols.Addresses[ItemNo]<FMemory.BaseAddr) OR
       (FSymbols.Addresses[ItemNo]>FMemory.EndAddr)) THEN
   BEGIN;
     Flisting.Add(TSymbol(FSymbols.Items[ItemNo]).FormatSymbol(FirstColumn));
   END
   ELSE
   BEGIN;
     SearchInsert(ItemNo);
     Sort(MemoryListCompare);
   END;

  FSymbols.RemoveBlanks;
  FListing.Add('');
  FOR ItemNo:=0 TO (Count-1) DO
  BEGIN;
    Item:=TLocation(Items[ItemNo]);
    Line:='';
    IF (ItemNo=0) THEN
    BEGIN;
      PadToAdd(Line,FirstColumn,'org');
      PadToAdd(Line,SecondColumn,Format('$%4.4X',[Item.Address]));
      Flisting.Add(Line);
    END;
    {Get symbol for this address if any}
    CodeLabel:=FSymbols.GetSymbol(Item.Address,FALSE);

    IF (CodeLabel<>'') THEN
      Flisting.Add(Format('.%s',[CodeLabel]));

    IF (Item.ItemType=tyCode) THEN
      FListing.Add(FormatCode(Item))
    ELSE
      FListing.Add(FormatData(Item));
  END;
  FListing.Add('.'+EndAddrLable);
  IF (OutputFileName<>'') THEN
    FListing.Add(Format('SAVE "%s",%s,%s',[ChangeFileExt(OutputFileName,BinExt),StartAddrLable,EndAddrLable]));
  Result:=FListing;
END;

FUNCTION TMemoryList.Add(ToAdd	: TLocation) : INTEGER;

BEGIN;
  Result:=INHERITED Add(ToAdd);
END;

PROCEDURE TMemoryList.AddData(DataType		: TItemType;
		                      Location		: STRING;
			                  Count		    : DWORD;
                              Terminator	: BYTE = 0);

VAR ToAdd	    : TLocation;
    DataSize	: INTEGER;
    ItemSize	: DWORD;
    Current	    : CHAR;
    ItemNo	    : INTEGER;

BEGIN;
  IF (LowerCase(Location)=TokenPC) THEN
    ToAdd:=TLocation.Create(FMemory.PC,Count,'',DataType,FALSE)
  ELSE
    ToAdd:=TLocation.Create(StrTointDef(Location,0),Count,'',DataType,FALSE);

  //ItemSize:=GetItemSize(ToAdd);
  ItemSize:=ToAdd.GetItemSize;

  IF ((DataType IN [tyDataString, tyDataStringTerm, tyDataStringTermHi,
                    tyDataStringTermHiZ]) AND (Count=0)) THEN
  BEGIN;
    FMemory.PC:=ToAdd.Address;

    {Force terminator = 0, if terminating on Hi or Z}
    IF (DataType=tyDataStringTermHiZ) THEN
      Terminator:=0;

    Current:=FMemory.ReadChar;
    WHILE (((DataType=tyDataString) 	  AND (IsASCII(Current))) OR
           ((DataType=tyDataStringTerm)   AND (ORD(Current)<>Terminator)) OR
           ((DataType=tyDataStringTermHi) AND (ORD(Current)<$80)) OR
           ((DataType=tyDataStringTermHiZ) AND (ORD(Current)<>Terminator) AND (ORD(Current)<$80))) DO
    BEGIN;
      Count:=Count+1;
      Current:=FMemory.ReadChar;
    END;
    FMemory.PC:=FMemory.PC-1;
  END;

  {Include the terminator}
  IF (DataType=tyDataStringTerm) THEN
    Count:=Count+1;

  IF ((DataType in[tyDataWordEntry,tyDataWordRTSEntry]) AND (Count>0)) THEN
  BEGIN;
    FMemory.PC:=ToAdd.Address;
    FOR ItemNo:=0 TO (Count-1) DO
      IF (DataType=tyDataWordRTSEntry) THEN
        FEntryPoints.GetSymbol(FMemory.ReadWord+1,TRUE)
      ELSE
        FEntryPoints.GetSymbol(FMemory.ReadWord,TRUE);
  END;

  DataSize:=Count*ItemSize;
  ToAdd.Length:=DataSize;

  FMemory.FlagData(ToAdd.Address,DataSize);
  FMemory.PC:=ToAdd.Address+DataSize;
  Add(ToAdd);
  FSymbols.GetSymbol(ToAdd.Address);
END;

PROCEDURE TMemoryList.AddCode(Location	: DWORD;
			                  Count	    : DWORD;
          		              CodeLine	: STRING;
			                  NewLine	: BOOLEAN);

VAR ToAdd	: TLocation;

BEGIN;
  ToAdd:=TLocation.Create(Location,Count,CodeLine,tyCode,NewLine);
  Add(ToAdd);
END;

PROCEDURE TMemoryList.AddEntry(Location	: STRING);

BEGIN;
  IF (LowerCase(Location)=TokenPC) THEN
    FEntryPoints.SafeAddAddress(FMemory.PC,'')
  ELSE
    FEntryPoints.SafeAddAddress(StrToIntDef(Location,0),'');
END;

FUNCTION TMemoryList.FixupString(Item	: TLocation) : STRING;

VAR	CurrentChar	: CHAR;
        CharNo		: INTEGER;
        AsciiLast	: BOOLEAN;
        AsciiCurrent	: BOOLEAN;

BEGIN;
  Result:='';
  AsciiCurrent:=FALSE;

  FOR CharNo:=1 TO Length(Item.Text) DO
  BEGIN;
    CurrentChar:=Item.Text[CharNo];
    AsciiLast:=AsciiCurrent;
    AsciiCurrent:=IsASCII(CurrentChar);

    IF (NOT AsciiLast AND AsciiCurrent) THEN
    BEGIN;
      IF (CharNo<>1) THEN
        Result:=Result+',';

      Result:=Result+'"'+CurrentChar
    END
    ELSE IF (AsciiLast AND AsciiCurrent) THEN
      Result:=Result+CurrentChar
    ELSE IF (AsciiLast AND NOT AsciiCurrent) THEN
      Result:=Result+Format('",$%2.2X',[ORD(CurrentChar)])
    ELSE {NOT AsciiLast AND NOT AsciiCurrent}
    BEGIN;
      IF (CharNo<>1) THEN
        Result:=Result+Format(',$%2.2X',[ORD(CurrentChar)])
      ELSE
        Result:=Result+Format('$%2.2X',[ORD(CurrentChar)])
    END;
  END;

  IF (AsciiCurrent) THEN
    Result:=Result+'"';
    
  Item.Text:=Result;
END;

FUNCTION TMemoryList.FormatData(Item	: TLocation) : STRING;

CONST	DefBytesPerLine	= 8;
	    StrBytesPerLine = 64;

VAR ItemSize	: INTEGER;
    ItemNo	    : INTEGER;
    ItemCount	: INTEGER;
    ByteNo	    : INTEGER;
    BytesPerLine: INTEGER;

BEGIN;
  Result:='';
  //ItemSize:=GetItemSize(Item);
  ItemSize:=Item.GetItemSize;

  IF (Item.Length>0) THEN
  BEGIN;
    CASE Item.ItemType OF
      tyDataString,
      tyDataStringTerm,
      tyDataStringTermHi,
      tyDataStringTermHiZ   : BytesPerLine:=StrBytesPerLine;
      tyDataWordEntry,
      tyDataWordRTSEntry    : BytesPerLine:=2;
    ELSE
      BytesPerLine:=DefBytesPerLine;
    END;

    ItemNo:=0;
    ByteNo:=BytesPerLine;
    ItemCount:=Item.Length DIV ItemSize;

    FMemory.PC:=Item.Address;

    WHILE (ItemNo<ItemCount) DO
    BEGIN;
      IF (ByteNo=BytesPerLine) THEN
      BEGIN;
        Result:=Result+FormatDataOutput(Item);
        Item.Text:='';
        ByteNo:=0;
      END;

      CASE Item.ItemType OF
        tyDataByte 		        : Item.Text:=Item.Text+Format('$%2.2X,',[FMemory.ReadByte]);
        tyDataWord              : Item.Text:=Item.Text+Format('$%4.4X,',[FMemory.ReadWord]);
        tyDataWordEntry         : Item.Text:=Item.Text+FSymbols.GetSymbolValue(FMemory.ReadWord,FALSE)+',';
        tyDataWordRTSEntry      : Item.Text:=Item.Text+FSymbols.GetSymbolValue(FMemory.ReadWord+1,FALSE)+'-1'+',';
        tyDataDWord		        : Item.Text:=Item.Text+Format('$%8.8X,',[FMemory.ReadDWord]);
        tyDataString,
        tyDataStringTerm,
        tyDataStringTermHi,
        tyDataStringTermHiZ     : Item.Text:=Item.Text+FMemory.ReadChar;
      END;

      ByteNo:=ByteNo+ItemSize;
      ItemNo:=ItemNo+1;
    END;
    IF (Item.Text<>'') THEN
      Result:=Result+FormatDataOutput(Item);

  END;
END;

FUNCTION TMemoryList.FormatCode(Item	: TLocation) : STRING;

VAR	SpacePos	: INTEGER;
	OpCode		: STRING;
	Oprands		: STRING;

BEGIN;
  Result:='';
  SpacePos:=Pos(' ',Item.Text);

  IF (Item.Text[1]<>';') THEN
  BEGIN;
    IF (SpacePos>0) THEN
    BEGIN;
      OpCode:=Copy(Item.Text,1,SpacePos-1);
      Oprands:=Copy(Item.Text,SpacePos+1,MaxInt);
    END
    ELSE
    BEGIN;
      OpCode:=Item.Text;
      Oprands:='';
    END;

    PadTo(Result,FirstColumn);
    Result:=Result+OpCode;

    IF (Oprands<>'') THEN
      PadToAdd(Result,SecondColumn,Oprands);

    IF (Item.NewLineAfter) THEN
      Result:=Result+EOL;
  END
  ELSE
    Result:=Item.Text;
END;



FUNCTION TMemoryList.FormatDataOutput(Item	: TLocation) : STRING;

BEGIN;
  Result:='';
  IF (Item.Text<>'') THEN
  BEGIN;
    IF(Item.ItemType IN[tyDataString,tyDataStringTerm,
                        tyDataStringTermHi,tyDataStringTermHiZ]) THEN
      FixupString(Item);

    CASE Item.ItemType OF
      tyDataByte	        : Result:=Format('%sEQUB    %s',[IndentStr,Item.Text]);
      tyDataWord,
      tyDataWordEntry	    : Result:=Format('%sEQUW    %s',[IndentStr,Item.Text]);
      tyDataWordRTSEntry    : Result:=Format('%sEQUW    %s',[IndentStr,Item.Text]);
      tyDataDWord	        : Result:=Format('%sEQUD    %s',[IndentStr,Item.Text]);
      tyDataString,
      tyDataStringTerm,
      tyDataStringTermHi,
      tyDataStringTermHiZ   : Result:=Format('%sEQUS    %s',[IndentStr,Item.Text]);
    END;
    IF(Result[Length(Result)]=',') THEN
      SetLength(Result,Length(Result)-1);

    Result:=Result+Eol;
  END;
END;

FUNCTION TMemoryList.GetLocation(Which	: INTEGER) : TLocation;

BEGIN;
  IF ((Which>=0) AND (Which<Count)) THEN
    Result:=TLocation(Items[Which])
  ELSE
    Result:=NIL;
END;

FUNCTION TMemoryList.SearchInsert(SymbolNo	: INTEGER) : TLocation;

VAR	Found		    : BOOLEAN;
	ItemNo		    : INTEGER;
	Location        : TLocation;
	NewLocation	    : TLocation;
	SymbolAddress	: DWORD;

BEGIN;
  ItemNo:=0;
  Found:=FALSE;
  SymbolAddress:=FSymbols.Addresses[SymbolNo];
  NewLocation:=NIL;

  WHILE (NOT Found AND (ItemNo<Count)) DO
  BEGIN;
    Location:=GetLocation(ItemNo);
    IF (Location.Address=SymbolAddress) THEN
      Found:=TRUE
    ELSE IF (Location.IsInRange(SymbolAddress)) THEN
    BEGIN;
      IF(Location.ItemType=tyCode) THEN
      BEGIN;
        NewLocation:=NewSymbolInRange(SymbolNo,Location);
      END
      ELSE
      BEGIN;
        // Only actually split a range if the address is a multiple of
        // the element size into the range, otherwise make the new range
        // just old start+offset, as for code.
        IF (((SymbolAddress-Location.Address) MOD Location.GetItemSize) = 0) THEN
          NewLocation:=Location.Split(SymbolAddress,FSymbols.Symbols[SymbolNo])
        ELSE
          NewLocation:=NewSymbolInRange(SymbolNo,Location);
      END;

      IF(NewLocation.Address=SymbolAddress) THEN
      BEGIN;
        Add(NewLocation);
        Found:=TRUE;
      END;
    END;
    ItemNo:=ItemNo+1;
  END;
  Result:=NewLocation;
END;

FUNCTION TMemoryList.NewSymbolInRange(SymbolNo	: INTEGER;
                                      Location  : TLocation)  : TLocation;

VAR     SymbolAddress	: DWORD;

BEGIN;
  SymbolAddress:=FSymbols.Addresses[SymbolNo];

  WITH FSymbols DO
  BEGIN;
    GetSymbol(Location.Address);
    Result:=TLocation.Create(SymbolAddress,0,'',tyCode,FALSE);
    Result.Text:=Format('%s = %s+%d',[GetSymbol(SymbolAddress),GetSymbol(Location.Address),SymbolAddress-Location.Address]);
    TSymbol(FSymbols.Items[SymbolNo]).Symbol:='';
  END;
END;

PROCEDURE TMemoryList.Dump;

VAR Idx         : INTEGER;
    Item	: TLocation;

BEGIN;
  FOR Idx:=0 TO (Count-1) DO
  BEGIN;
    Item:=TLocation(Items[Idx]);
    WriteLn(Item.AsString);
  END;
END;

FUNCTION TMemoryList.GetPC : DWORD;

BEGIN;
  Result:=FMemory.PC;
END;

PROCEDURE TMemoryList.SetPC(NewPC : DWORD);

BEGIN
  FMemory.PC:=NewPC;
END;

end.
