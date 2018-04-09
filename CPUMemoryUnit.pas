unit CPUMemoryUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

USES Types,Classes,SysUtils,UtilsUnit,RamothStringListUnit;

CONST	IsData		= 'D';
	    IsCode		= 'C';
	    IsUnused	= 'U';
	    IsDone		= 'O';
        NoChange	= 'N';

        OpCodeJMP   = $20;  { 6502 absolute JMP }

TYPE TCPUmemory = Class(TObject)
     PROTECTED
       FMemory		    : ARRAY[0..$FFFF]OF BYTE;
       FMemoryFlags	    : ARRAY[0..$FFFF]OF CHAR;
       FBaseAddr	    : DWORD;
       FEndAddr		    : DWORD;
       FPC		        : DWORD;
       FLoaded		    : BOOLEAN;

       FHexDump		    : TStringList;
       FFoundStrings	: TRamothStringList;

       PROCEDURE SetPC(NewPC	: DWORD);
       PROCEDURE FlagArea(Location	: WORD;
			              Count		: WORD;
			              Flag		: CHAR);
     PUBLIC
       PROPERTY BaseAddr	: DWORD READ FBaseAddr;
       PROPERTY EndAddr		: DWORD READ FEndAddr;
       PROPERTY PC		    : DWORD READ FPC WRITE SetPC;
       PROPERTY Loaded		: BOOLEAN READ FLoaded;
       PROPERTY HexDump		: TStringList READ FHexDump;
       PROPERTY FoundStrings: TRamothStringList READ FFoundStrings;

       CONSTRUCTOR Create;
       DESTRUCTOR Destroy; override;
       PROCEDURE LoadFromFile(FileName	: STRING;
			                  PBaseAddr	: DWORD);
       FUNCTION ReadByte(Flag : CHAR = NoChange) : BYTE;
       FUNCTION ReadWord(Flag : CHAR = NoChange) : WORD;
       FUNCTION ReadDWord(Flag : CHAR = NoChange) : LongWord;
       FUNCTION ReadChar(Flag : CHAR = NoChange) : CHAR;
       PROCEDURE FlagData(Location	: DWORD;
			              Count		: DWORD);
       PROCEDURE FlagCode(Location	: DWORD;
			              Count		: DWORD);
       FUNCTION GetFlag : CHAR;
       FUNCTION FindCode : BOOLEAN;
       FUNCTION AreaLength(AType	    : CHAR) : DWORD;
       PROCEDURE HexDumpArea(First	    : DWORD;
			                 Last	    : DWORD;
			                 DoFlags	: BOOLEAN = FALSE);
       PROCEDURE FindStrings;
       PROCEDURE FindInlineStrings(InlineAddr   : WORD);
     END;

implementation

CONSTRUCTOR TCPUmemory.Create;

BEGIN;
  INHERITED Create;
  FLoaded:=FALSE;
  FBaseAddr:=0;
  FEndAddr:=0;
  FPC:=0;
  FHexDump:=TStringList.Create;
  FFoundStrings:=TRamothStringList.Create;
END;

DESTRUCTOR TCPUmemory.Destroy;

BEGIN;
  FHexDump.Free;
  FFoundStrings.Free;
  INHERITED Destroy;
END;

PROCEDURE TCPUmemory.SetPC(NewPC	: DWORD);

BEGIN;
  FPC:=NewPC;
END;

PROCEDURE TCPUmemory.LoadFromFile(FileName	: STRING;
			                      PBaseAddr	: DWORD);

VAR	TempStream	: TMemoryStream;

BEGIN;
  FBaseAddr:=PBaseAddr;
  FPC:=FBaseAddr;
  FlagArea(0,$FFFF,IsUnused);

  TempStream:=TMemoryStream.Create;
  TRY
    TempStream.LoadFromFile(FileName);
    TempStream.Position:=0;

    IF ((FBaseAddr+TempStream.Size)<=$10000) THEN
    BEGIN;
      TempStream.Read(FMemory[FBaseAddr],TempStream.Size);
      FlagCode(FBaseAddr,TempStream.Size);
    END;

    FEndAddr:=FBaseAddr+TempStream.Size;
    HexDumpArea(FBaseAddr,FEndAddr,True);

    //WriteLnFmt('Loaded %s at %4.4X',[FileName,FBaseAddr]);
    FLoaded:=TRUE;
  FINALLY
    TempStream.Free;
  END;
END;

FUNCTION TCPUmemory.ReadByte(Flag : CHAR = NoChange) : BYTE;

BEGIN;
  IF (FLag<>NoChange) THEN
    FMemoryFlags[FPC]:=Flag;

  Result:=FMemory[FPC];
  FPC:=FPC+1;
END;

FUNCTION TCPUmemory.ReadWord(Flag : CHAR = NoChange) : WORD;

BEGIN;
  Result:=ReadByte(Flag)+(ReadByte(Flag)*$100);
END;

FUNCTION TCPUmemory.ReadDWord(Flag : CHAR = NoChange) : LongWord;
BEGIN;
  Result:=ReadWord(Flag)+(ReadWord(Flag)*$10000);
END;

FUNCTION TCPUmemory.ReadChar(Flag : CHAR = NoChange) : CHAR;

BEGIN;
  Result:=Char(ReadByte(Flag));
END;

PROCEDURE TCPUmemory.FlagArea(Location	: WORD;
			                  Count	    : WORD;
			                  Flag	    : CHAR);
VAR Loc	: WORD;

BEGIN;
  FOR Loc:=Location TO (Location+Count-1) DO
    FMemoryFlags[Loc]:=Flag;
END;

PROCEDURE TCPUmemory.FlagData(Location	: DWORD;
			                  Count	    : DWORD);

BEGIN;
  FlagArea(Location,Count,IsData);
END;

PROCEDURE TCPUmemory.FlagCode(Location	: DWORD;
			                  Count	    : DWORD);

BEGIN;
  FlagArea(Location,Count,IsCode);
END;

FUNCTION TCPUmemory.GetFlag : CHAR;

BEGIN;
  Result:=FMemoryFlags[FPC];
END;

FUNCTION TCPUmemory.FindCode : BOOLEAN;

VAR	Location	: WORD;

BEGIN;
  Location:=0;

  WHILE ((Location<>$FFFF) AND (FMemoryFlags[Location]<>IsCode)) DO
    Location:=Location+1;

  PC:=Location;
  Result:=(FMemoryFlags[Location]=IsCode);
END;

FUNCTION TCPUmemory.AreaLength(AType	: CHAR) : DWORD;

VAR	LocalPC		: DWORD;

BEGIN;
  LocalPC:=FPC;

  WHILE ((LocalPC<>$FFFF) AND (FMemoryFlags[LocalPC]=AType)) DO
    LocalPC:=LocalPC+1;

  Result:=LocalPC-FPC;
END;

PROCEDURE TCPUmemory.HexDumpArea(First		: DWORD;
			                     Last		: DWORD;
			                     DoFlags	: BOOLEAN = FALSE);

CONST	LineLen	= 16;

VAR    	Line	: INTEGER;
        LByte	: INTEGER;
	    NoLines	: INTEGER;
    	OutBuff	: STRING;
        OutBuff2: STRING;
        OutBuff3: STRING;
        Current	: Byte;
        NoBytes	: WORD;
	    LocalPC	: DWORD;

BEGIN;
  NoBytes:=Last-First;
  NoLines:=NoBytes DIV LineLen;
  FHexDump.Clear;
  FHexDump.Add('');

  LocalPC:=First;

  FOR Line:=0 TO NoLines DO
  BEGIN;
    OutBuff:=Format('; %4.4X ',[LocalPC]);
    OutBuff2:='';
    OutBuff3:='';
    FOR LByte:=(Line*LineLen) TO ((Line+1)*LineLen)-1 DO
    BEGIN;
      Current:=FMemory[LocalPC];
      OutBuff3:=OutBuff3+FMemoryFlags[LocalPC];

      LocalPC:=LocalPC+1;
      OutBuff:=OutBuff+Format('%2.2X ',[Current]);
      IF ((Current>31) AND (Current<127)) THEN
        OutBuff2:=OutBuff2+CHR(Current)
      ELSE
        OutBuff2:=OutBuff2+'-';
    END;

    IF (DoFlags) THEN
      FHexDump.Add(Format('%s%s %s',[OutBuff,OutBuff2,Outbuff3]))
    ELSE
      FHexDump.Add(Format('%s%s',[OutBuff,OutBuff2]));
  END;
END;

PROCEDURE TCPUmemory.FindStrings;

VAR	Found		    : STRING;
   	Current		    : CHAR;
    FirstCharPos    : WORD;

BEGIN;
  FPC:=FBaseAddr;
  Found:='';
  FFoundStrings.Clear;
  FFoundStrings.Add('if(0)');
  FFoundStrings.Add('Begin:Strings discovered');

  WHILE (FPC<FEndAddr) DO
  BEGIN;
    Current:=ReadChar;
    IF (IsASCII(Current)) THEN
    BEGIN;
      IF (Found='') THEN
        FirstCharPos:=FPC-1;

      Found:=Found+Current;
    END
    ELSE
    BEGIN;
      IF (Length(Found)>2) THEN
        FFoundStrings.Add(Format('string $%4.4X %d ; "%s"',[FirstCharPos,Length(Found),Found]));

      Found:='';
    END;
  END;
  FFoundStrings.Add('End:Strings discovered');
  FFoundStrings.Add('endif');
END;

PROCEDURE TCPUmemory.FindInlineStrings(InlineAddr   : WORD);

VAR	Found		    : STRING;
   	Current		    : BYTE;
    FirstCharPos    : WORD;
    TmpPC           : DWORD;
    JmpDest         : WORD;

BEGIN;
  FPC:=FBaseAddr;
  Found:='';

  FFoundStrings.Add('if(0)');
  FFoundStrings.Add('Begin:Inline Strings discovered');

  WHILE (FPC<FEndAddr) DO
  BEGIN;
    Current:=ReadByte;
    IF (Current=OpCodeJMP) THEN
    BEGIN;
      TmpPC:=FPC;
      JmpDest:=ReadWord;
      IF (JmpDest=InlineAddr) THEN
      BEGIN;
        FFoundStrings.AddFormat('stringhiz $%4.4X',[FPC]);
        FFoundStrings.Add('entry pc');
      END
      ELSE
        FPC:=TmpPC;
    END;
  END;
  FFoundStrings.Add('End:Inline Strings discovered');
  FFoundStrings.Add('endif');
END;

end.
