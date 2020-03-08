unit Disassembler6809Unit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

USES Types,SysUtils,Classes,CPUMemoryUnit, SymbolListUnit,UtilsUnit,
     MemoryListUnit,ConsoleUnit,AbstractDisassemblerUnit,
     ParameterListUnit,StrUtils;

CONST
    PreByte10       = $10;
    PreByte11       = $11;

    TFRRegNames : ARRAY[0..15] OF STRING =
      ('D','X','Y','U','S','PC','Invalid','Invalid',
       'A','B','CC','DP','Invalid','Invalid','Invalid','Invalid');

    OpEXG           = $1E;
    OpTFR           = $1F;
    OpPSHS          = $34;
    OpPULS          = $35;
    OpPSHU          = $36;
    OpPULU          = $37;
    OpCWAI          = $3C;

    {Executable format specifiers}
    exeOS9Mod       = 'os9';            { OS9 module format}
    exeDDos         = 'dragondos';      { DragonDos / DragonMMC executable file }
    exeFlex         = 'flex';           { Flex segmented CMD / SYS file }

    OS9Sync         = $87CD;            { OS9's module header identifier }
    OS9HeadCLength  = $08;              { Number of bytes in OS9 header that are parity checked }

    DDHeadStart     = $55;              { Dragon dos / DragonMMC executable header start marker }
    DDHeadEnd       = $AA;              { Dragon dos / DragonMMC executable header end marker }
    DDHeadLength    = $09;              { Dragon dos / DragonMMC executable header length }

    { Flex binary files are made up of multiple records, which need not be }
    { contiguous in memory, but have a maximum size of 255 bytes }
    FlexSOR         = $02;              { Flex Start of binary record }
    FlexTransfer    = $16;              { Flex transfer (exec) address record }


TYPE
    TAddressMode    = (amImmediate,     { oprand follows opcode in memory }
                       amDirect,        { Direct page, low byte of address follows }
                       amIndexed,       { Indexed by X,Y,U,S }
                       amExtended,      { 16 bit address of oprand follows }
                       amImplied,       { Oprand implied by opcode e.g. COMA }
                       amRelative,      { PC relative offset e.g. BRA, LBRA }
                       amOS9            { OS9 function call decoding }
                       );

    TOpCode6809 = Class(TOpCode)
      OpStr 	: STRING;               { To be inserted into disassembily }
      OpBytes   : INTEGER;              { Number of bytes for instruction, note for indexed may be variable }
      OpMode    : TAddressMode;         { Addressing mode of operation }
      CPU	    : TCPUSet;              { CPUs that this instruction is valid for }
      IsBranch  : BOOLEAN;              { Is this a branching instruction? }
    END;

    TDisassembler6809 = Class(TADisassembler)
    PROTECTED
      Op		    : TOpCode6809;
      TargetLable	: STRING;
      TargetAddr    : WORD;
      IndexByte     : BYTE;
      CommentStr    : STRING;

      FUNCTION GetTargetLable(ATargetAddr   : WORD;
                              ATargetIsCode : BOOLEAN) : STRING;

      FUNCTION DecodeImmediate  : STRING;
      FUNCTION DecodeDirect     : STRING;
      FUNCTION DecodeIndexed    : STRING;
      FUNCTION DecodeExtended   : STRING;
      FUNCTION DecodePushPull   : STRING;
      FUNCTION DecodeImplied    : STRING;
      FUNCTION DecodeRelative   : STRING;
      FUNCTION DecodeOS9Call    : STRING;

      FUNCTION CheckOS9Header   : BOOLEAN;
      FUNCTION CheckDDosHeader  : BOOLEAN;

      PROCEDURE DecodeInstruction;  override;
	  PROCEDURE InitOpcodes;        override;
      PROCEDURE InitDirectives;     override;
	  PROCEDURE MakeOpCode(OpNo		    : INTEGER;
			     	       InOpStr		: STRING;
			     	       InOpBytes	: INTEGER;
                           InOpMode     : TAddressMode;
			     	       InCPU		: TCPUSet = [tc6809];
                           InBranch		: BOOLEAN = FALSE);

    PUBLIC
      CONSTRUCTOR Create;
      DESTRUCTOR Destroy; override;
      PROCEDURE Go;       override;
    END;

implementation

CONSTRUCTOR TDisassembler6809.Create;

BEGIN;
  INHERITED Create;
  Verbosity:=VBNormal;
  FCPU:=tc6809;
  FMinCPU:=tc6809;
  FMaxCPU:=tc6809;
  FSwapWords:=TRUE;
END;

DESTRUCTOR TDisassembler6809.Destroy;

BEGIN;
  INHERITED Destroy;
END;
PROCEDURE TDisassembler6809.Go;

VAR	EntryPoint	: DWORD;

BEGIN;
  INHERITED Go;
  Memory.SwapWords:=TRUE;

  {OS9 uses SWI2 followed by a byte code to do it's function calls}
  {If we are disassembling an OS9 module, then select this behavior}
  IF (LowerCase(Parameters[parExecFormat])=exeOS9Mod) THEN
  BEGIN;
    MakeOpCode($103F,'OS9 %s',    1,amOS9);
    CheckOS9Header;
  END;

  {Check for DragonDOS / DragonMMC executable}
  IF (LowerCase(Parameters[parExecFormat])=exeDDos) THEN
    CheckDDosHeader;

  {If no entry point is speccified, assume the base address}
  IF (EntryPoints.Count<1) THEN
    EntryPoints.GetSymbol(Memory.BaseAddr);

  WHILE (EntryPoints.Count>0) DO
  BEGIN;
    EntryPoint:=EntryPoints.Addresses[0];
    SymbolList.SafeAddAddress(EntryPoint,EntryPoints.Symbols[0],TRUE);
    WriteLnFmtV(FVerbosity,VBVerbose,'Disassembling %4.4X',[EntryPoint]);

    Memory.PC:=EntryPoint;

    WHILE (Memory.GetFlag=IsCode) DO
    BEGIN;
      DecodeInstruction;
    END;
    EntryPoints.DeleteAddress(EntryPoint);

    {Check to see if there is any more un-disassembled code ?}
  END;

  { Find any remaining un-processed bytes and mark them as byte data}
  WHILE (Memory.FindCode) DO
    MemoryList.AddData(tyDataByte,'pc',Memory.AreaLength(IsCode));
END;

FUNCTION TDisassembler6809.GetTargetLable(ATargetAddr   : WORD;
                                          ATargetIsCode : BOOLEAN) : STRING;

BEGIN
  Result:=EntryPoints.GetSymbol(ATargetAddr,FALSE);
  IF (Result='') THEN
  BEGIN;
    Result:=SymbolList.GetSymbol(ATargetAddr,True,1);

    IF (ATargetIsCode AND EntryPoints.InRange(ATargetAddr)) THEN
    begin;
      EntryPoints.SafeAddAddress(ATargetAddr,Result,TRUE);
      WriteLnFmt('%4.4X, %s',[ATargetAddr,Result]);
    end;
  END;
END;

FUNCTION TDisassembler6809.DecodeImmediate  : STRING;

VAR IByte   : BYTE;
    IWord   : WORD;

BEGIN;
  IF (Op.OpBytes = 1) THEN
  BEGIN
    IByte:=Memory.ReadByte(IsDone);
    TargetLable:=Format('$%2.2X',[Ibyte]);
  END
  ELSE
  BEGIN
    IWord:=Memory.ReadWord(IsDone);
    TargetLable:=SymbolList.GetSymbol(IWord,FALSE,1);
    IF (TargetLable='') THEN
      TargetLable:=Format('$%4.4X',[Iword]);
  END;
  Result:=Format(Op.OpStr,[TargetLable]);
END;

FUNCTION TDisassembler6809.DecodeDirect     : STRING;

VAR DPByte  : BYTE;

BEGIN;
  {GetDP ref}
  DPByte:=Memory.ReadByte(IsDone);
  //TargetLable:=SymbolList.GetSymbol(DPByte,TRUE,1);
  TargetLable:=GetTargetLable(DPByte,Op.IsBranch);
  Result:=Format(Op.OpStr,[TargetLable]);
END;

FUNCTION TDisassembler6809.DecodeIndexed    : STRING;

CONST
    RegMask         = $60;
    IndirectMask    = $10;
    IndexModeMask   = $0F;
    FiveBitMask     = $80;

VAR RegSelect       : BYTE;
    ShortOffs       : INTEGER;
    EffectiveStr    : STRING;
    RegName         : STRING;
    IndexMode       : BYTE;
    ByteOffs        : Shortint;
    WordOffs        : SmallInt;

BEGIN;
  IndexByte:=Memory.ReadByte(IsDone);

  RegSelect:=IndexByte AND RegMask;
  CASE RegSelect OF
    $00     : RegName:='X';
    $20     : RegName:='Y';
    $40     : RegName:='U';
    $60     : RegName:='S';
  END;

  IF ((IndexByte AND FiveBitMask)=0) THEN
  BEGIN;
    ShortOffs:=IndexByte AND $1F;
    IF (ShortOffs > $0F) THEN
      ShortOffs:=-16+(ShortOffs AND $0F);

    EffectiveStr:=Format('%d,%s',[ShortOffs,RegName]);
  END
  ELSE
  BEGIN;
    IndexMode:=IndexByte AND IndexModeMask;

    CASE IndexMode OF
      $00   : EffectiveStr:=Format(',%s+',[RegName]);
      $01   : EffectiveStr:=Format(',%s++',[RegName]);
      $02   : EffectiveStr:=Format(',-%s',[RegName]);
      $03   : EffectiveStr:=Format(',--%s',[RegName]);
      $04   : EffectiveStr:=Format(',%s',[RegName]);
      $05   : EffectiveStr:=Format('B,%s',[RegName]);
      $06   : EffectiveStr:=Format('A,%s',[RegName]);
      $08   : BEGIN;
                ByteOffs:=Memory.ReadByte(IsDone);
                EffectiveStr:=Format('%d,%s',[ByteOffs,RegName]);
              END;
      $09   : BEGIN;
                WordOffs:=Memory.ReadWord(IsDone);
                EffectiveStr:=Format('%d,%s',[WordOffs,RegName]);
              END;
      $0B   : EffectiveStr:=Format('D,%s',[RegName]);
      $0C   : BEGIN;
                ByteOffs:=Memory.ReadByte(IsDone);
                EffectiveStr:=Format('%s,PCR',[GetTargetLable(CalcRelative8(ByteOffs),FALSE)]);
              END;
      $0D   : BEGIN;
                WordOffs:=Memory.ReadWord(IsDone);
                EffectiveStr:=Format('%s,PCR',[GetTargetLable(CalcRelative16(WordOffs),FALSE)]);
              END;
      $0F   : BEGIN;
                WordOffs:=Memory.ReadWord(IsDone);
                EffectiveStr:=Format('%s',[GetTargetLable(WordOffs,FALSE)]);
              END;
      ELSE
        EffectiveStr:=Format('Invalid indexed byte : $%2.2X',[IndexByte]);
    END;

    {Indirect mode only applies here}
    IF (((IndexByte AND IndirectMask) = IndirectMask) AND NOT (IndexMode IN [0,2])) THEN
      EffectiveStr:='['+EffectiveStr+']';
  END;

  Result:=Format(Op.OpStr,[EffectiveStr]);
END;

FUNCTION TDisassembler6809.DecodeExtended   : STRING;

VAR DestWord    : WORD;

BEGIN;
  DestWord:=Memory.ReadWord(IsDone);
  TargetLable:=GetTargetLable(DestWord,Op.IsBranch);
  Result:=Format(Op.OpStr,[TargetLable]);
END;

FUNCTION TDisassembler6809.DecodePushPull : STRING;

VAR PostByte    : BYTE;
    RegStr      : STRING;
    RegList     : TStringList;

BEGIN;
  PostByte:=Memory.ReadByte(IsDone);
  RegList:=TStringList.Create;

  TRY
    {Can only push/pul U onto S stack and S onto U stack }
    IF (Op.OpCode IN [OpPSHS, OpPULS]) THEN
      RegStr:='U'
    ELSE
      RegStr:='S';

    IF ((PostByte AND $80)=$80) THEN RegList.Add('PC');
    IF ((PostByte AND $40)=$40) THEN RegList.Add(RegStr);
    IF ((PostByte AND $20)=$20) THEN RegList.Add('Y');
    IF ((PostByte AND $10)=$10) THEN RegList.Add('X');
    IF ((PostByte AND $08)=$08) THEN RegList.Add('DP');
    IF ((PostByte AND $04)=$04) THEN RegList.Add('B');
    IF ((PostByte AND $02)=$02) THEN RegList.Add('A');
    IF ((PostByte AND $01)=$01) THEN RegList.Add('CC');

    IF ((PostByte AND $80)=$80) THEN
      CommentStr:='Pull of PC, effective RTS';

    Result:=Format(Op.OpStr,[RegList.CommaText]);
  FINALLY
    RegList.Free;
  END;
END;

FUNCTION TDisassembler6809.DecodeImplied  : STRING;

VAR PostByte    : BYTE;
    Src         : BYTE;
    Dest        : BYTE;

BEGIN;
  IF (Op.OpCode IN [OpEXG,OpTFR]) THEN
  BEGIN;
    PostByte:=Memory.ReadByte(IsDone);
    Src:=(PostByte AND $F0) SHR 4;
    Dest:=PostByte AND $0F;
    Result:=Format(Op.OpStr,[TFRRegNames[Src]+','+TFRRegNames[Dest]]);
  END
  ELSE IF (Op.OpCode IN [OpPSHS,OpPULS,OpPSHU,OpPULU]) THEN
    Result:=DecodePushPull
  ELSE IF (Op.OpCode = OpCWAI) THEN
    Result:=DecodeImmediate
  ELSE
    Result:=Op.OpStr;
END;

FUNCTION TDisassembler6809.DecodeRelative   : STRING;

VAR BByte   : BYTE;
    BWord   : WORD;
    RelDest : WORD;

BEGIN;
  IF (Op.OpBytes = 1) THEN
  BEGIN
    BByte:=Memory.ReadByte(IsDone);
    RelDest:=CalcRelative8(BByte);
  END
  ELSE
  BEGIN
    BWord:=Memory.ReadWord(IsDone);
    RelDest:=CalcRelative16(BWord);
  END;

  {Try getting target from entry list first, in case it has an explicitly}
  {defined symbol                                                        }
  TargetLable:=GetTargetLable(RelDest,Op.IsBranch);
  Result:=Format(Op.OpStr,[TargetLable]);
END;

FUNCTION TDisassembler6809.DecodeOS9Call    : STRING;

VAR CallNo      : BYTE;

BEGIN;
  CallNo:=Memory.ReadByte(IsDone);

  CASE CallNo OF
    $00 : TargetLable:='F$Link';          { Link to Module }
    $01 : TargetLable:='F$Load';          { Load Module from File }
    $02 : TargetLable:='F$UnLink';        { Unlink Module }
    $03 : TargetLable:='F$Fork';          { Start New Process }
    $04 : TargetLable:='F$Wait';          { Wait for Child Process to Die }
    $05 : TargetLable:='F$Chain';         { Chain Process to New Module }
    $06 : TargetLable:='F$Exit';          { Terminate Process }
    $07 : TargetLable:='F$Mem';           { Set Memory Size }
    $08 : TargetLable:='F$Send';          { Send Signal to Process }
    $09 : TargetLable:='F$Icpt';          { Set Signal Intercept }
    $0A : TargetLable:='F$Sleep';         { Suspend Process }
    $0B : TargetLable:='F$SSpd';          { Suspend Process }
    $0C : TargetLable:='F$ID';            { Return Process ID }
    $0D : TargetLable:='F$SPrior';        { Set Process Priority }
    $0E : TargetLable:='F$SSWI';          { Set Software Interrupt }
    $0F : TargetLable:='F$PErr';          { Print Error }
    $10 : TargetLable:='F$PrsNam';        { Parse Pathlist Name }
    $11 : TargetLable:='F$CmpNam';        { Compare Two Names }
    $12 : TargetLable:='F$SchBit';        { Search Bit Map }
    $13 : TargetLable:='F$AllBit';        { Allocate in Bit Map }
    $14 : TargetLable:='F$DelBit';        { Deallocate in Bit Map }
    $15 : TargetLable:='F$Time';          { Get Current Time }
    $16 : TargetLable:='F$STime';         { Set Current Time }
    $17 : TargetLable:='F$CRC';           { Generate CRC }
    $18 : TargetLable:='F$GPrDsc';        { get Process Descriptor copy }
    $19 : TargetLable:='F$GBlkMp';        { get System Block Map copy }
    $1A : TargetLable:='F$GModDr';        { get Module Directory copy }
    $1B : TargetLable:='F$CpyMem';        { Copy External Memory }
    $1C : TargetLable:='F$SUser';         { Set User ID number }
    $1D : TargetLable:='F$UnLoad';        { Unlink Module by name }
    $1E : TargetLable:='F$Alarm';         { Color Computer Alarm Call (system wide) }
    $1F : TargetLable:='invalid';
    $20 : TargetLable:='invalid';
    $21 : TargetLable:='F$NMLink';        { Color Computer NonMapping Link }
    $22 : TargetLable:='F$NMLoad';        { Color Computer NonMapping Load }
    $23 : TargetLable:='invalid';
    $24 : TargetLable:='invalid';
    $25 : TargetLable:='F$TPS';           { Return System's Ticks Per Second }
    $26 : TargetLable:='F$TimAlm';        { COCO individual process alarm call }
    $27 : TargetLable:='F$VIRQ';          { Install/Delete Virtual IRQ }
    $28 : TargetLable:='F$SRqMem';        { System Memory Request }
    $29 : TargetLable:='F$SRtMem';        { System Memory Return }
    $2A : TargetLable:='F$IRQ';           { Enter IRQ Polling Table }
    $2B : TargetLable:='F$IOQu';          { Enter I/O Queue }
    $2C : TargetLable:='F$AProc';         { Enter Active Process Queue }
    $2D : TargetLable:='F$NProc';         { Start Next Process }
    $2E : TargetLable:='F$VModul';        { Validate Module }
    $2F : TargetLable:='F$Find64';        { Find Process/Path Descriptor }
    $30 : TargetLable:='F$All64';         { Allocate Process/Path Descriptor }
    $31 : TargetLable:='F$Ret64';         { Return Process/Path Descriptor }
    $32 : TargetLable:='F$SSvc';          { Service Request Table Initialization }
    $33 : TargetLable:='F$IODel';         { Delete I/O Module }
    $34 : TargetLable:='F$SLink';         { System Link }
    $35 : TargetLable:='F$Boot';          { Bootstrap System }
    $36 : TargetLable:='F$BtMem';         { Bootstrap Memory Request }
    $37 : TargetLable:='F$GProcP';        { Get Process ptr }
    $38 : TargetLable:='F$Move';          { Move Data (low bound first) }
    $39 : TargetLable:='F$AllRAM';        { Allocate RAM blocks }
    $3A : TargetLable:='F$AllImg';        { Allocate Image RAM blocks }
    $3B : TargetLable:='F$DelImg';        { Deallocate Image RAM blocks }
    $3C : TargetLable:='F$SetImg';        { Set Process DAT Image }
    $3D : TargetLable:='F$FreeLB';        { Get Free Low Block }
    $3E : TargetLable:='F$FreeHB';        { Get Free High Block }
    $3F : TargetLable:='F$AllTsk';        { Allocate Process Task number }
    $40 : TargetLable:='F$DelTsk';        { Deallocate Process Task number }
    $41 : TargetLable:='F$SetTsk';        { Set Process Task DAT registers }
    $42 : TargetLable:='F$ResTsk';        { Reserve Task number }
    $43 : TargetLable:='F$RelTsk';        { Release Task number }
    $44 : TargetLable:='F$DATLog';        { Convert DAT Block/Offset to Logical }
    $45 : TargetLable:='F$DATTmp';        { Make temporary DAT image (Obsolete) }
    $46 : TargetLable:='F$LDAXY';         { Load A [X;[Y]] }
    $47 : TargetLable:='F$LDAXYP';        { Load A [X+;[Y]] }
    $48 : TargetLable:='F$LDDDXY';        { Load D [D+X;[Y]] }
    $49 : TargetLable:='F$LDABX';         { Load A from 0;X in task B }
    $4A : TargetLable:='F$STABX';         { Store A at 0;X in task B }
    $4B : TargetLable:='F$AllPrc';        { Allocate Process Descriptor }
    $4C : TargetLable:='F$DelPrc';        { Deallocate Process Descriptor }
    $4D : TargetLable:='F$ELink';         { Link using Module Directory Entry }
    $4E : TargetLable:='F$FModul';        { Find Module Directory Entry }
    $4F : TargetLable:='F$MapBlk';        { Map Specific Block }
    $50 : TargetLable:='F$ClrBlk';        { Clear Specific Block }
    $51 : TargetLable:='F$DelRAM';        { Deallocate RAM blocks }
    $52 : TargetLable:='F$GCMDir';        { Pack module directory }
    $53 : TargetLable:='F$AlHRam';        { Allocate HIGH RAM Blocks }

    $70 : TargetLable:='F$RegDmp';        { Ron Lammardo's debugging register dump call }
    $71 : TargetLable:='F$NVRAM';         { Non Volatile RAM (RTC battery backed static) read/write }

    $80 : TargetLable:='I$Attach';        { Attach I/O Device }
    $81 : TargetLable:='I$Detach';        { Detach I/O Device }
    $82 : TargetLable:='I$Dup';           { Duplicate Path }
    $83 : TargetLable:='I$Create';        { Create New File }
    $84 : TargetLable:='I$Open';          { Open Existing File }
    $85 : TargetLable:='I$MakDir';        { Make Directory File }
    $86 : TargetLable:='I$ChgDir';        { Change Default Directory }
    $87 : TargetLable:='I$Delete';        { Delete File }
    $88 : TargetLable:='I$Seek';          { Change Current Position }
    $89 : TargetLable:='I$Read';          { Read Data }
    $8A : TargetLable:='I$Write';         { Write Data }
    $8B : TargetLable:='I$ReadLn';        { Read Line of ASCII Data }
    $8C : TargetLable:='I$WritLn';        { Write Line of ASCII Data }
    $8D : TargetLable:='I$GetStt';        { Get Path Status }
    $8E : TargetLable:='I$SetStt';        { Set Path Status }
    $8F : TargetLable:='I$Close';         { Close Path }
    $90 : TargetLable:='I$DeletX'         { Delete from current exec dir }
  ELSE
    TargetLable:='invalid';
  END;
  Result:=Format(Op.OpStr,[TargetLable]);
END;

{
    Check for an OS9 module header in the format :
    Offset  Size    Purpose
    $00     $02     Sync bytes, should be $87CD
    $02     $02     Module size in bytes
    $04     $02     Module name offset
    $06     $01     Module type (MSN) and language (LSN)
    $07     $01     Attributes (MSN) and Revision (LSN)
    $08     $01     Header parity check.
    $09     $02     Execution offset
    $0B     $02     Permanent storage size
}

FUNCTION TDisassembler6809.CheckOS9Header   : BOOLEAN;

VAR MSync       : WORD;
    MName       : WORD;
    MParity     : BYTE;
    MExec       : WORD;
    PCount      : WORD;
    Parity      : BYTE;
    PComment    : STRING;

BEGIN;
  Memory.PC:=Memory.BaseAddr;

  Parity:=$FF;
  FOR PCount:=0 TO (OS9HeadCLength-1) DO
    Parity:=Parity XOR Memory.ReadByte(NoChange);

  Memory.PC:=Memory.BaseAddr;
  MSync:=Memory.ReadWord(NoChange);

  Result:=(MSync=OS9Sync);
  IF (Result) THEN
  BEGIN;
           Memory.ReadWord(NoChange);
    MName:=Memory.ReadWord(NoChange);
           Memory.ReadByte(NoChange);
           Memory.ReadByte(NoChange);
    MParity:=Memory.ReadByte(NoChange);
    MExec:=Memory.ReadWord(NoChange);
           Memory.ReadWord(NoChange);

    IF (Parity=MParity) THEN
      PComment:='OS9 header parity check, valid'
    ELSE
      PComment:=Format('OS9 header parity check, Invalid! should be %2.2X',[Parity]);

    Memory.PC:=Memory.BaseAddr;
    MemoryList.AddData(tyDataWord,'pc',1,0,'OS9 module identifier',1);
    MemoryList.AddData(tyDataWord,'pc',1,0,'OS9 module size',0);
    MemoryList.AddData(tyDataWord,'pc',1,0,'OS9 name offset',0);
    MemoryList.AddData(tyDataByte,'pc',1,0,'OS9 Type & Language',0);
    MemoryList.AddData(tyDataByte,'pc',1,0,'OS9 Attributes and revision',0);
    MemoryList.AddData(tyDataByte,'pc',1,0,PComment,0);
    MemoryList.AddData(tyDataWord,'pc',1,0,'OS9 exec offset',0);
    MemoryList.AddData(tyDataWord,'pc',1,0,'OS9 permanent storage size',0);

    MemoryList.AddData(tyDataStringTermHi,IntToStr(Memory.BaseAddr+MName),0,0,'OS9 Module name',1);

    EntryPoints.GetSymbol(Memory.BaseAddr+MExec);

    Result:=TRUE;
  END;
END;

{
    Check for a DragonDos / DragonMMC header of the format :
    Offset  Size    Purpose
    $00     $01     Start marker, always $55
    $01     $01     Filetype
    $02     $02     Load address
    $04     $02     Length
    $06     $02     Exec address
    $08     $01     End marker always $AA
}

FUNCTION TDisassembler6809.CheckDDosHeader  : BOOLEAN;

VAR DDMarks     : BYTE;
    DDMarke     : BYTE;
    DDLoad      : WORD;
    DDExec      : WORD;
    CodeAddr    : DWORD;
    LoadComment : STRING;

BEGIN;
  Memory.PC:=Memory.BaseAddr;

  DDMarks:=     Memory.ReadByte(NoChange);
                Memory.ReadByte(NoChange);
  DDLoad:=      Memory.ReadWord(NoChange);
                Memory.ReadWord(NoChange);
  DDExec:=      Memory.ReadWord(NoChange);
  DDMarke:=     Memory.ReadByte(NoChange);
  CodeAddr:=    Memory.PC;

  Result:=((DDMarks=DDHeadStart) AND (DDMarke=DDHeadEnd));

  IF (Result) THEN
  BEGIN;
    Memory.PC:=Memory.BaseAddr;

    IF (DDLoad = CodeAddr) THEN
      LoadComment:='Load address'
    ELSE
      LoadComment:=Format('Error! loaded at wrong address, set load address to %4.4X in the control file',[DDLoad-DDHeadLength]);

    MemoryList.AddData(tyDataByte,'pc',1,0,'DragonDos / Dragon MMC Executable header begins',1);
    MemoryList.AddData(tyDataByte,'pc',1,0,'Filetype',0);
    MemoryList.AddData(tyDataWord,'pc',1,0,LoadComment,0);
    MemoryList.AddData(tyDataWord,'pc',1,0,'File length',0);
    MemoryList.AddData(tyDataWord,'pc',1,0,'Execution address',0);
    MemoryList.AddData(tyDataByte,'pc',1,0,'DragonDos / Dragon MMC Executable header ends',0);

    IF (DDLoad = CodeAddr) THEN
      EntryPoints.GetSymbol(DDExec)
    ELSE
      EntryPoints.GetSymbol(CodeAddr);

  END;
END;

PROCEDURE TDisassembler6809.DecodeInstruction;

VAR	OpCode		: WORD;
    OpPost      : BYTE;

    Instruction	: STRING;
    HexBytes    : STRING;
    DebugStr    : STRING;
    Location	: WORD;

BEGIN;
  {Get the current PC, we need this later}
  Location:=Memory.PC;

  if(Location=$c1c3) then
    writeln('break!');

  {Fetch the opcode, and look it up}
  OpCode:=Memory.ReadByte(IsDone);
  OpPost:=0;

  {Instruction is prefixed by a pre-byte, deal with it}
  IF (OpCode IN [PreByte10, PreByte11]) THEN
  BEGIN;
    OpPost:=Memory.ReadByte(IsDone);
    OpCode:=(OpCode SHL 8) OR OpPost;
  END;

  CommentStr:='';

  Op:=TOpCode6809(FOpCodes[OpCode]);
  IF (Op<>NIL) THEN
  BEGIN;
    CASE Op.OpMode OF
      amDirect      : Instruction:=DecodeDirect;
      amExtended    : Instruction:=DecodeExtended;
      amImmediate   : Instruction:=DecodeImmediate;
      amIndexed     : Instruction:=DecodeIndexed;
      amRelative    : Instruction:=DecodeRelative;
      amImplied     : Instruction:=DecodeImplied;
      amOS9         : Instruction:=DecodeOS9Call;
    ELSE
      IF (OpPost<>0) THEN
        Instruction:=FormatInvalid(Location,OpCode,2)
      ELSE
        Instruction:=FormatInvalid(Location,OpCode,1);
    END;

//  IF (Op.Branch<>brRtsRti) THEN
    BEGIN;
      IF (NOT Op.IsBranch) THEN
        MemoryList.AddCode(Location,Op.OpBytes+1,Instruction,FALSE,CommentStr)
      ELSE
        MemoryList.AddCode(Location,Op.OpBytes+1,Instruction,TRUE,CommentStr);
    END;
  END
  ELSE
  BEGIN
    IF (OpPost<>0) THEN
      Instruction:=FormatInvalid(Location,OpCode,2)
    ELSE
      Instruction:=FormatInvalid(Location,OpCode,1);

    MemoryList.AddCode(Location,1,Instruction,TRUE);
  END;

  IF (FVerbosity=VBVerbose) THEN
  BEGIN
    HexBytes:=Memory.HexDumpBytes(Location,Memory.PC-1);
    DebugStr:=Format('%4.4X %s ',[Location,HexBytes]);
    PadToAdd(DebugStr,30,Instruction);
    PadToAdd(DebugStr,50,Format('%4.4X',[Memory.PC]));
    WriteLn(DebugStr);
  END;
END;

PROCEDURE TDisassembler6809.MakeOpCode(OpNo		    : INTEGER;
			     	                   InOpStr		: STRING;
			     	                   InOpBytes	: INTEGER;
                                       InOpMode     : TAddressMode;
			     	                   InCPU		: TCPUSet = [tc6809];
                                       InBranch		: BOOLEAN = FALSE);

VAR OpCode  : TOpCode6809;

BEGIN;
  OpCode:=TOpCode6809.Create;
  WITH OpCode DO
  BEGIN;
    OpCode:=OpNo;
    OpStr:=InOpStr;
    OpBytes:=InOpBytes;
    OpMode:=InOpMode;
    CPU:=InCPU;
    IsBranch:=InBranch;
  END;
  FOpCodes[OpNo]:=OpCode;
END;

PROCEDURE TDisassembler6809.InitOpcodes;

BEGIN;
  MakeOpCode($00,'NEG <%s',      1,amDirect);
  MakeOpCode($03,'COM <%s',      1,amDirect);
  MakeOpCode($04,'LSR <%s',      1,amDirect);
  MakeOpCode($06,'ROR <%s',      1,amDirect);
  MakeOpCode($07,'ASR <%s',      1,amDirect);
  MakeOpCode($08,'LSL <%s',      1,amDirect);
  MakeOpCode($09,'ROL <%s',      1,amDirect);
  MakeOpCode($0A,'DEC <%s',      1,amDirect);
  MakeOpCode($0C,'INC <%s',      1,amDirect);
  MakeOpCode($0D,'TST <%s',      1,amDirect);
  MakeOpCode($0E,'JMP <%s',      1,amDirect,[tc6809],TRUE);
  MakeOpCode($0F,'CLR <%s',      1,amDirect);

//  MakeOpCode($10,'',            0,amImplied,[tc6809]);
//  MakeOpCode($11,'',            0,amImplied,[tc6809]);
  MakeOpCode($12,'NOP',         0,amImplied);
  MakeOpCode($13,'SYNC',        0,amImplied);
  MakeOpCode($16,'LBRA %s',     2,amRelative,[tc6809],TRUE);
  MakeOpCode($17,'LBSR %s',     2,amRelative,[tc6809],TRUE);
  MakeOpCode($19,'DAA',         0,amImplied);
  MakeOpCode($1A,'ORCC #%s',    1,amImmediate);
  MakeOpCode($1C,'ANDCC #%s',   1,amImmediate);
  MakeOpCode($1D,'SEX',         0,amImplied);
  MakeOpCode($1E,'EXG %s',      1,amImplied);
  MakeOpCode($1F,'TFR %s',      1,amImplied);

  MakeOpCode($20,'BRA %s',      1,amRelative,[tc6809],TRUE);
  MakeOpCode($21,'BRN %s',      1,amRelative,[tc6809],TRUE);
  MakeOpCode($22,'BHI %s',      1,amRelative,[tc6809],TRUE);
  MakeOpCode($23,'BLS %s',      1,amRelative,[tc6809],TRUE);
  MakeOpCode($24,'BHS %s',      1,amRelative,[tc6809],TRUE);
  MakeOpCode($25,'BLO %s',      1,amRelative,[tc6809],TRUE);
  MakeOpCode($26,'BNE %s',      1,amRelative,[tc6809],TRUE);
  MakeOpCode($27,'BEQ %s',      1,amRelative,[tc6809],TRUE);
  MakeOpCode($28,'BVC %s',      1,amRelative,[tc6809],TRUE);
  MakeOpCode($29,'BVS %s',      1,amRelative,[tc6809],TRUE);
  MakeOpCode($2A,'BPL %s',      1,amRelative,[tc6809],TRUE);
  MakeOpCode($2B,'BMI %s',      1,amRelative,[tc6809],TRUE);
  MakeOpCode($2C,'BGE %s',      1,amRelative,[tc6809],TRUE);
  MakeOpCode($2D,'BLT %s',      1,amRelative,[tc6809],TRUE);
  MakeOpCode($2E,'BGT %s',      1,amRelative,[tc6809],TRUE);
  MakeOpCode($2F,'BLE %s',      1,amRelative,[tc6809],TRUE);

  MakeOpCode($30,'LEAX %s',     1,amIndexed);
  MakeOpCode($31,'LEAY %s',     1,amIndexed);
  MakeOpCode($32,'LEAS %s',     1,amIndexed);
  MakeOpCode($33,'LEAU %s',     1,amIndexed);
  MakeOpCode($34,'PSHS %s',     1,amImplied);
  MakeOpCode($35,'PULS %s',     1,amImplied);
  MakeOpCode($36,'PSHU %s',     1,amImplied);
  MakeOpCode($37,'PULU %s',     1,amImplied);
  MakeOpCode($39,'RTS',         0,amImplied);
  MakeOpCode($3A,'ABX',         0,amImplied);
  MakeOpCode($3B,'RTI',         0,amImplied);
  MakeOpCode($3C,'CWAI #$%s',   1,amImplied);
  MakeOpCode($3D,'MUL',         0,amImplied);
  MakeOpCode($3F,'SWI',         0,amImplied);

  MakeOpCode($40,'NEGA',        0,amImplied);
  MakeOpCode($43,'COMA',        0,amImplied);
  MakeOpCode($44,'LSRA',        0,amImplied);
  MakeOpCode($46,'RORA',        0,amImplied);
  MakeOpCode($47,'ASRA',        0,amImplied);
  MakeOpCode($48,'LSLA',        0,amImplied);
  MakeOpCode($49,'ROLA',        0,amImplied);
  MakeOpCode($4A,'DECA',        0,amImplied);
  MakeOpCode($4C,'INCA',        0,amImplied);
  MakeOpCode($4D,'TSTA',        0,amImplied);
  MakeOpCode($4F,'CLRA',        0,amImplied);

  MakeOpCode($50,'NEGB',        0,amImplied);
  MakeOpCode($53,'COMB',        0,amImplied);
  MakeOpCode($54,'LSRB',        0,amImplied);
  MakeOpCode($56,'RORB',        0,amImplied);
  MakeOpCode($57,'ASRB',        0,amImplied);
  MakeOpCode($58,'LSLB',        0,amImplied);
  MakeOpCode($59,'ROLB',        0,amImplied);
  MakeOpCode($5A,'DECB',        0,amImplied);
  MakeOpCode($5C,'INCB',        0,amImplied);
  MakeOpCode($5D,'TSTB',        0,amImplied);
  MakeOpCode($5F,'CLRB',        0,amImplied);

  MakeOpCode($60,'NEG %s',      1,amIndexed);
  MakeOpCode($63,'COM %s',      1,amIndexed);
  MakeOpCode($64,'LSR %s',      1,amIndexed);
  MakeOpCode($66,'ROR %s',      1,amIndexed);
  MakeOpCode($67,'ASR %s',      1,amIndexed);
  MakeOpCode($68,'LSL %s',      1,amIndexed);
  MakeOpCode($69,'ROL %s',      1,amIndexed);
  MakeOpCode($6A,'DEC %s',      1,amIndexed);
  MakeOpCode($6C,'INC %s',      1,amIndexed);
  MakeOpCode($6D,'TST %s',      1,amIndexed);
  MakeOpCode($6E,'JMP %s',      1,amIndexed,[tc6809],TRUE);
  MakeOpCode($6F,'CLR %s',      1,amIndexed);

  MakeOpCode($70,'NEG %s',      2,amExtended);
  MakeOpCode($73,'COM %s',      2,amExtended);
  MakeOpCode($74,'LSR %s',      2,amExtended);
  MakeOpCode($76,'ROR %s',      2,amExtended);
  MakeOpCode($77,'ASR %s',      2,amExtended);
  MakeOpCode($78,'LSL %s',      2,amExtended);
  MakeOpCode($79,'ROL %s',      2,amExtended);
  MakeOpCode($7A,'DEC %s',      2,amExtended);
  MakeOpCode($7C,'INC %s',      2,amExtended);
  MakeOpCode($7D,'TST %s',      2,amExtended);
  MakeOpCode($7E,'JMP %s',      2,amExtended,[tc6809],TRUE);
  MakeOpCode($7F,'CLR %s',      2,amExtended);

  MakeOpCode($80,'SUBA #%s',    1,amImmediate);
  MakeOpCode($81,'CMPA #%s',    1,amImmediate);
  MakeOpCode($82,'SBCA #%s',    1,amImmediate);
  MakeOpCode($83,'SUBD #%s',    2,amImmediate);
  MakeOpCode($84,'ANDA #%s',    1,amImmediate);
  MakeOpCode($85,'BITA #%s',    1,amImmediate);
  MakeOpCode($86,'LDA #%s',     1,amImmediate);
  MakeOpCode($88,'EORA #%s',    1,amImmediate);
  MakeOpCode($89,'ADCA #%s',    1,amImmediate);
  MakeOpCode($8A,'ORA #%s',     1,amImmediate);
  MakeOpCode($8B,'ADDA #%s',    1,amImmediate);
  MakeOpCode($8C,'CMPX #%s',    2,amImmediate);
  MakeOpCode($8D,'BSR %s',      1,amRelative,[tc6809],TRUE);
  MakeOpCode($8E,'LDX #%s',     2,amImmediate);

  MakeOpCode($90,'SUBA <%s',     1,amDirect);
  MakeOpCode($91,'CMPA <%s',     1,amDirect);
  MakeOpCode($92,'SBCA <%s',     1,amDirect);
  MakeOpCode($93,'SUBD <%s',     1,amDirect);
  MakeOpCode($94,'ANDA <%s',     1,amDirect);
  MakeOpCode($95,'BITA <%s',     1,amDirect);
  MakeOpCode($96,'LDA <%s',      1,amDirect);
  MakeOpCode($97,'STA <%s',      1,amDirect);
  MakeOpCode($98,'EORA <%s',     1,amDirect);
  MakeOpCode($99,'ADCA <%s',     1,amDirect);
  MakeOpCode($9A,'ORA <%s',      1,amDirect);
  MakeOpCode($9B,'ADDA <%s',     1,amDirect);
  MakeOpCode($9C,'CMPX <%s',     1,amDirect);
  MakeOpCode($9D,'JSR <%s',      1,amDirect,[tc6809],TRUE);
  MakeOpCode($9E,'LDX <%s',      1,amDirect);
  MakeOpCode($9F,'STX <%s',      1,amDirect);

  MakeOpCode($A0,'SUBA %s',     2,amIndexed);
  MakeOpCode($A1,'CMPA %s',     2,amIndexed);
  MakeOpCode($A2,'SBCA %s',     2,amIndexed);
  MakeOpCode($A3,'SUBD %s',     2,amIndexed);
  MakeOpCode($A4,'ANDA %s',     2,amIndexed);
  MakeOpCode($A5,'BITA %s',     2,amIndexed);
  MakeOpCode($A6,'LDA %s',      2,amIndexed);
  MakeOpCode($A7,'STA %s',      2,amIndexed);
  MakeOpCode($A8,'EORA %s',     2,amIndexed);
  MakeOpCode($A9,'ADCA %s',     2,amIndexed);
  MakeOpCode($AA,'ORA %s',      2,amIndexed);
  MakeOpCode($AB,'ADDA %s',     2,amIndexed);
  MakeOpCode($AC,'CMPX %s',     2,amIndexed);
  MakeOpCode($AD,'JSR %s',      2,amIndexed,[tc6809],TRUE);
  MakeOpCode($AE,'LDX %s',      2,amIndexed);
  MakeOpCode($AF,'STX %s',      2,amIndexed);

  MakeOpCode($B0,'SUBA %s',     3,amExtended);
  MakeOpCode($B1,'CMPA %s',     3,amExtended);
  MakeOpCode($B2,'SBCA %s',     3,amExtended);
  MakeOpCode($B3,'SUBD %s',     3,amExtended);
  MakeOpCode($B4,'ANDA %s',     3,amExtended);
  MakeOpCode($B5,'BITA %s',     3,amExtended);
  MakeOpCode($B6,'LDA %s',      3,amExtended);
  MakeOpCode($B7,'STA %s',      3,amExtended);
  MakeOpCode($B8,'EORA %s',     3,amExtended);
  MakeOpCode($B9,'ADCA %s',     3,amExtended);
  MakeOpCode($BA,'ORA %s',      3,amExtended);
  MakeOpCode($BB,'ADDA %s',     3,amExtended);
  MakeOpCode($BC,'CMPX %s',     3,amExtended);
  MakeOpCode($BD,'JSR %s',      3,amExtended,[tc6809],TRUE);
  MakeOpCode($BE,'LDX %s',      3,amExtended);
  MakeOpCode($BF,'STX %s',      3,amExtended);

  MakeOpCode($C0,'SUBB #%s',    1,amImmediate);
  MakeOpCode($C1,'CMPB #%s',    1,amImmediate);
  MakeOpCode($C2,'SBCB #%s',    1,amImmediate);
  MakeOpCode($C3,'ADDD #%s',    2,amImmediate);
  MakeOpCode($C4,'ANDB #%s',    1,amImmediate);
  MakeOpCode($C5,'BITB #%s',    1,amImmediate);
  MakeOpCode($C6,'LDB #%s',     1,amImmediate);
  MakeOpCode($C8,'EORB #%s',    1,amImmediate);
  MakeOpCode($C9,'ADCB #%s',    1,amImmediate);
  MakeOpCode($CA,'ORB #%s',     1,amImmediate);
  MakeOpCode($CB,'ADDB #%s',    1,amImmediate);
  MakeOpCode($CC,'LDD #%s',     2,amImmediate);
  MakeOpCode($CE,'LDU #%s',     2,amImmediate);

  MakeOpCode($D0,'SUBB <%s',     1,amDirect);
  MakeOpCode($D1,'CMPB <%s',     1,amDirect);
  MakeOpCode($D2,'SBCB <%s',     1,amDirect);
  MakeOpCode($D3,'ADDD <%s',     1,amDirect);
  MakeOpCode($D4,'ANDB <%s',     1,amDirect);
  MakeOpCode($D5,'BITB <%s',     1,amDirect);
  MakeOpCode($D6,'LDB <%s',      1,amDirect);
  MakeOpCode($D7,'STB <%s',      1,amDirect);
  MakeOpCode($D8,'EORB <%s',     1,amDirect);
  MakeOpCode($D9,'ADCB <%s',     1,amDirect);
  MakeOpCode($DA,'ORB <%s',      1,amDirect);
  MakeOpCode($DB,'ADDB <%s',     1,amDirect);
  MakeOpCode($DC,'LDD <%s',      1,amDirect);
  MakeOpCode($DD,'STD <%s',      1,amDirect);
  MakeOpCode($DE,'LDU <%s',      1,amDirect);
  MakeOpCode($DF,'STU <%s',      1,amDirect);

  MakeOpCode($E0,'SUBB %s',     2,amIndexed);
  MakeOpCode($E1,'CMPB %s',     2,amIndexed);
  MakeOpCode($E2,'SBCB %s',     2,amIndexed);
  MakeOpCode($E3,'ADDD %s',     2,amIndexed);
  MakeOpCode($E4,'ANDB %s',     2,amIndexed);
  MakeOpCode($E5,'BITB %s',     2,amIndexed);
  MakeOpCode($E6,'LDB %s',      2,amIndexed);
  MakeOpCode($E7,'STB %s',      2,amIndexed);
  MakeOpCode($E8,'EORB %s',     2,amIndexed);
  MakeOpCode($E9,'ADCB %s',     2,amIndexed);
  MakeOpCode($EA,'ORB %s',      2,amIndexed);
  MakeOpCode($EB,'ADDB %s',     2,amIndexed);
  MakeOpCode($EC,'LDD %s',      2,amIndexed);
  MakeOpCode($ED,'STD %s',      2,amIndexed);
  MakeOpCode($EE,'LDU %s',      2,amIndexed);
  MakeOpCode($EF,'STU %s',      2,amIndexed);

  MakeOpCode($F0,'SUBB %s',     3,amExtended);
  MakeOpCode($F1,'CMPB %s',     3,amExtended);
  MakeOpCode($F2,'SBCB %s',     3,amExtended);
  MakeOpCode($F3,'ADDD %s',     3,amExtended);
  MakeOpCode($F4,'ANDB %s',     3,amExtended);
  MakeOpCode($F5,'BITB %s',     3,amExtended);
  MakeOpCode($F6,'LDB %s',      3,amExtended);
  MakeOpCode($F7,'STB %s',      3,amExtended);
  MakeOpCode($F8,'EORB %s',     3,amExtended);
  MakeOpCode($F9,'ADCB %s',     3,amExtended);
  MakeOpCode($FA,'ORB %s',      3,amExtended);
  MakeOpCode($FB,'ADDB %s',     3,amExtended);
  MakeOpCode($FC,'LDD %s',      3,amExtended);
  MakeOpCode($FD,'STD %s',      3,amExtended);
  MakeOpCode($FE,'LDU %s',      3,amExtended);
  MakeOpCode($FF,'STU %s',      3,amExtended);

  MakeOpCode($1021,'LBRN %s',   2,amRelative,[tc6809],TRUE);
  MakeOpCode($1022,'LBHI %s',   2,amRelative,[tc6809],TRUE);
  MakeOpCode($1023,'LBLS %s',   2,amRelative,[tc6809],TRUE);
  MakeOpCode($1024,'LBHS %s',   2,amRelative,[tc6809],TRUE);
  MakeOpCode($1025,'LBLO %s',   2,amRelative,[tc6809],TRUE);
  MakeOpCode($1026,'LBNE %s',   2,amRelative,[tc6809],TRUE);
  MakeOpCode($1027,'LBEQ %s',   2,amRelative,[tc6809],TRUE);
  MakeOpCode($1028,'LBVC %s',   2,amRelative,[tc6809],TRUE);
  MakeOpCode($1029,'LBVS %s',   2,amRelative,[tc6809],TRUE);
  MakeOpCode($102A,'LBPL %s',   2,amRelative,[tc6809],TRUE);
  MakeOpCode($102B,'LBMI %s',   2,amRelative,[tc6809],TRUE);
  MakeOpCode($102C,'LBGE %s',   2,amRelative,[tc6809],TRUE);
  MakeOpCode($102D,'LBLT %s',   2,amRelative,[tc6809],TRUE);
  MakeOpCode($102E,'LBGT %s',   2,amRelative,[tc6809],TRUE);
  MakeOpCode($102F,'LBLE %s',   2,amRelative,[tc6809],TRUE);

  MakeOpCode($103F,'SWI2',      0,amImplied);

  MakeOpCode($1083,'CMPD #%s',  2,amImmediate);
  MakeOpCode($108C,'CMPY #%s',  2,amImmediate);
  MakeOpCode($108E,'LDY #%s',   2,amImmediate);

  MakeOpCode($1093,'CMPD <%s',   1,amDirect);
  MakeOpCode($109C,'CMPY <%s',   1,amDirect);
  MakeOpCode($109E,'LDY <%s',    1,amDirect);
  MakeOpCode($109F,'STY <%s',    1,amDirect);

  MakeOpCode($10A3,'CMPD %s',   2,amIndexed);
  MakeOpCode($10AC,'CMPY %s',   2,amIndexed);
  MakeOpCode($10AE,'LDY %s',    2,amIndexed);
  MakeOpCode($10AF,'STY %s',    2,amIndexed);

  MakeOpCode($10B3,'CMPD %s',   2,amExtended);
  MakeOpCode($10BC,'CMPY %s',   2,amExtended);
  MakeOpCode($10BE,'LDY %s',    2,amExtended);
  MakeOpCode($10BF,'STY %s',    2,amExtended);

  MakeOpCode($10CE,'LDS #%s',   2,amImmediate);
  MakeOpCode($10DE,'LDS <%s',    1,amDirect);
  MakeOpCode($10DF,'STS <%s',    1,amDirect);
  MakeOpCode($10EE,'LDS %s',    2,amIndexed);
  MakeOpCode($10EF,'STS %s',    2,amIndexed);
  MakeOpCode($10FE,'LDS %s',    2,amExtended);
  MakeOpCode($10FF,'STS %s',    2,amExtended);

  MakeOpCode($113F,'SWI3',      0,amImplied);
  MakeOpCode($1183,'CMPU #%s',  2,amImmediate);
  MakeOpCode($118C,'CMPS #%s',  2,amImmediate);
  MakeOpCode($1193,'CMPU <%s',   1,amDirect);
  MakeOpCode($119C,'CMPS <%s',   1,amDirect);
  MakeOpCode($11A3,'CMPU %s',   2,amIndexed);
  MakeOpCode($11AC,'CMPS %s',   2,amIndexed);
  MakeOpCode($11B3,'CMPU %s',   2,amExtended);
  MakeOpCode($11BC,'CMPS %s',   2,amExtended);
END;

PROCEDURE TDisassembler6809.InitDirectives;

BEGIN
  Parameters.Overwrite:=FALSE;

  Parameters[mlLabelPrefix]:=   '';
  Parameters[mlLabelSuffix]:=   '';
  Parameters[mlDefineByte]:=    'FCB';
  Parameters[mlDefineWord]:=    'FDB';
  Parameters[mlDefineDWord]:=   'FQB';
  Parameters[mlDefineString]:=  'FCC';
  Parameters[mlOrigin]:=        'org';
  Parameters[mlBeginIgnore]:=   ' ifeq 1';
  Parameters[mlEndIgnore]:=     ' endc';
  Parameters[mlSaveCmd]:=       '; ';
  Parameters[mlEquate]:=        'EQU';
  Parameters[mlCommentChar]:=   ';';
  Parameters[mlCommentCol]:=    '40';

  Parameters[numBinPrefix]:=    '%';
  Parameters[numOctPrefix]:=    '@';
  Parameters[numHexPrefix]:=    '$';

  Parameters.Overwrite:=TRUE;
END;

end.
