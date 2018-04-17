unit Disassembler6809Unit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

USES Types,SysUtils,Classes,CPUMemoryUnit, SymbolListUnit,UtilsUnit,
     MemoryListUnit,BeebDisDefsUnit,ConsoleUnit,AbstractDisassemblerUnit;

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

TYPE
    TAddressMode    = (amImmediate,     { oprand follows opcode in memory }
                       amDirect,        { Direct page, low byte of address follows }
                       amIndexed,       { Indexed by X,Y,U,S }
                       amExtended,      { 16 bit address of oprand follows }
                       amImplied,       { Oprand implied by opcode e.g. COMA }
                       amRelative       { PC relative offset e.g. BRA, LBRA }
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

      FUNCTION GetTargetLable(ATargetAddr   : WORD) : STRING;

      FUNCTION DecodeImmediate  : STRING;
      FUNCTION DecodeDirect     : STRING;
      FUNCTION DecodeIndexed    : STRING;
      FUNCTION DecodeExtended   : STRING;
      FUNCTION DecodePushPull   : STRING;
      FUNCTION DecodeImplied    : STRING;
      FUNCTION DecodeRelative   : STRING;

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
  InitOpcodes;
  Verbose:=FALSE;
  FCPU:=tc6809;
  FMinCPU:=tc6809;
  FMaxCPU:=tc6809;
END;

DESTRUCTOR TDisassembler6809.Destroy;

BEGIN;
  INHERITED Destroy;
END;

PROCEDURE TDisassembler6809.Go;

VAR	EntryPoint	: DWORD;

BEGIN;
  InitDirectives;
  SymbolList.SafeAddAddress(Memory.BaseAddr,StartAddrLable);

  {If no entry point is speccified, assume the base address}
  IF (EntryPoints.Count<1) THEN
    EntryPoints.GetSymbol(Memory.BaseAddr);

  WHILE (EntryPoints.Count>0) DO
  BEGIN;
    EntryPoint:=EntryPoints.Addresses[0];
    SymbolList.SafeAddAddress(EntryPoint,'');
WriteLn(Format('Disassembling %4.4X',[EntryPoint]));
    Memory.PC:=EntryPoint;
    //Memory.FlagCode(Memory.PC,1);

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

FUNCTION TDisassembler6809.GetTargetLable(ATargetAddr   : WORD) : STRING;

BEGIN
  Result:=SymbolList.GetSymbol(ATargetAddr);
END;

FUNCTION TDisassembler6809.DecodeImmediate  : STRING;

VAR IByte   : BYTE;
    IWord   : WORD;

BEGIN;
  IF (Op.OpBytes = 1) THEN
  BEGIN
    IByte:=Memory.ReadByte(IsDone);
    TargetLable:=Format('%2.2X',[Ibyte]);
  END
  ELSE
  BEGIN
    IWord:=Memory.ReadWord(IsDone);
    TargetLable:=Format('%4.4X',[SwapWord(Iword)]);
  END;
  Result:=Format(Op.OpStr,[TargetLable]);
END;

FUNCTION TDisassembler6809.DecodeDirect     : STRING;

VAR DPByte  : BYTE;

BEGIN;
  {GetDP ref}
  DPByte:=Memory.ReadByte(IsDone);
  TargetLable:=SymbolList.GetSymbol(DPByte);
  Result:=Format(Op.OpStr,[TargetLable]);
  IF (Op.IsBranch) THEN
    EntryPoints.GetSymbol(DPByte);
END;

FUNCTION TDisassembler6809.DecodeIndexed    : STRING;

CONST
    RegMask         = $60;
    IndeirectMask   = $10;
    IndexModeMask   = $0F;
    FiveBitMask     = $80;

VAR RegSelect       : BYTE;
    ShortOffs       : INTEGER;
    EffectiveStr    : STRING;
    RegName         : STRING;
    IndexMode       : BYTE;
    ByteOffs        : BYTE;
    WordOffs        : WORD;

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
                EffectiveStr:=Format('$%2.2X,%s',[ByteOffs,RegName]);
              END;
      $09   : BEGIN;
                WordOffs:=SwapWord(Memory.ReadWord(IsDone));
                EffectiveStr:=Format('$%4.4X,%s',[WordOffs,RegName]);
              END;
      $0B   : EffectiveStr:=Format('D,%s',[RegName]);
      $0C   : BEGIN;
                ByteOffs:=Memory.ReadByte(IsDone);
                EffectiveStr:=Format('%s,PC',[GetTargetLable(Memory.PC+ByteOffs)]);
              END;
      $0D   : BEGIN;
                WordOffs:=SwapWord(Memory.ReadWord(IsDone));
                EffectiveStr:=Format('%s,PC',[GetTargetLable(Memory.PC+WordOffs)]);
              END;
      $0F   : BEGIN;
                WordOffs:=SwapWord(Memory.ReadWord(IsDone));
                EffectiveStr:=Format('$%4.4X',[WordOffs]);
              END;
      ELSE
        EffectiveStr:=Format('Invalid indexed byte : $%2.2X',[IndexByte]);
    END;

    {Indirect mode only applies here}
    IF (((IndexByte AND IndeirectMask) = IndeirectMask) AND NOT (IndexMode IN [0,2])) THEN
      EffectiveStr:='['+EffectiveStr+']';
  END;

  Result:=Format(Op.OpStr,[EffectiveStr]);
END;

FUNCTION TDisassembler6809.DecodeExtended   : STRING;

VAR DestWord    : WORD;

BEGIN;
  DestWord:=SwapWord(Memory.ReadWord(IsDone));
  TargetLable:=SymbolList.GetSymbol(DestWord);
  Result:=Format(Op.OpStr,[TargetLable]);
  IF (Op.IsBranch) THEN
    EntryPoints.GetSymbol(DestWord);
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
    BWord:=SwapWord(Memory.ReadWord(IsDone));
    RelDest:=CalcRelative16(BWord);
  END;
  TargetLable:=SymbolList.GetSymbol(RelDest);
  EntryPoints.GetSymbol(RelDest);
  Result:=Format(Op.OpStr,[TargetLable]);
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

  {Fetch the opcode, and look it up}
  OpCode:=Memory.ReadByte(IsDone);

  {Instruction is prefixed by a pre-byte, deal with it}
  IF (OpCode IN [PreByte10, PreByte11]) THEN
  BEGIN;
    OpPost:=Memory.ReadByte(IsDone);
    OpCode:=(OpCode SHL 8) OR OpPost;
  END;

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
    ELSE
      Instruction:=Format('; PC=%4.4X INVALID opcode %2.2x',[Location,OpCode]);
    END;

//  IF (Op.Branch<>brRtsRti) THEN
    BEGIN;
      IF (NOT Op.IsBranch) THEN
        MemoryList.AddCode(Location,Op.OpBytes+1,Instruction,FALSE)
      ELSE
        MemoryList.AddCode(Location,Op.OpBytes+1,Instruction,TRUE);
    END;
  END
  ELSE
  BEGIN
    Instruction:=Format('; PC=%4.4X INVALID opcode %2.2x',[Location,OpCode]);
    MemoryList.AddCode(Location,1,Instruction,TRUE);
  END;
  IF (Verbose) THEN
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
  MakeOpCode($00,'NEG %s',      1,amDirect);
  MakeOpCode($03,'COM %s',      1,amDirect);
  MakeOpCode($04,'LSR %s',      1,amDirect);
  MakeOpCode($06,'ROR %s',      1,amDirect);
  MakeOpCode($07,'ASR %s',      1,amDirect);
  MakeOpCode($08,'LSL %s',      1,amDirect);
  MakeOpCode($09,'ROL %s',      1,amDirect);
  MakeOpCode($0A,'DEC %s',      1,amDirect);
  MakeOpCode($0C,'INC %s',      1,amDirect);
  MakeOpCode($0D,'TST %s',      1,amDirect);
  MakeOpCode($0E,'JMP %s',      1,amDirect,[tc6809],TRUE);
  MakeOpCode($0F,'CLR %s',      1,amDirect);

//  MakeOpCode($10,'',            0,amImplied,[tc6809]);
//  MakeOpCode($11,'',            0,amImplied,[tc6809]);
  MakeOpCode($12,'NOP',         0,amImplied);
  MakeOpCode($13,'SYNC',        0,amImplied);
  MakeOpCode($16,'LBRA %s',     2,amRelative,[tc6809],TRUE);
  MakeOpCode($17,'LBSR %s',     2,amRelative,[tc6809],TRUE);
  MakeOpCode($19,'DAA',         0,amImplied);
  MakeOpCode($1A,'ORCC #$%s',   1,amImmediate);
  MakeOpCode($1C,'ANDCC #$%s',  1,amImmediate);
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
  MakeOpCode($3C,'CWAI %s',     1,amImplied);
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

  MakeOpCode($80,'SUBA #$%s',   1,amImmediate);
  MakeOpCode($81,'CMPA #$%s',   1,amImmediate);
  MakeOpCode($82,'SBCA #$%s',   1,amImmediate);
  MakeOpCode($83,'SUBD #$%s',   2,amImmediate);
  MakeOpCode($84,'ANDA #$%s',   1,amImmediate);
  MakeOpCode($85,'BITA #$%s',   1,amImmediate);
  MakeOpCode($86,'LDA #$%s',    1,amImmediate);
  MakeOpCode($88,'EORA #$%s',   1,amImmediate);
  MakeOpCode($89,'ADCA #$%s',   1,amImmediate);
  MakeOpCode($8A,'ORA #$%s',    1,amImmediate);
  MakeOpCode($8B,'ADDA #$%s',   1,amImmediate);
  MakeOpCode($8C,'CMPX #$%s',   2,amImmediate);
  MakeOpCode($8D,'BSR %s',      1,amRelative,[tc6809],TRUE);
  MakeOpCode($8E,'LDX #$%s',    2,amImmediate);

  MakeOpCode($90,'SUBA %s',     1,amDirect);
  MakeOpCode($91,'CMPA %s',     1,amDirect);
  MakeOpCode($92,'SBCA %s',     1,amDirect);
  MakeOpCode($93,'SUBD %s',     1,amDirect);
  MakeOpCode($94,'ANDA %s',     1,amDirect);
  MakeOpCode($95,'BITA %s',     1,amDirect);
  MakeOpCode($96,'LDA %s',      1,amDirect);
  MakeOpCode($97,'STA %s',      1,amDirect);
  MakeOpCode($98,'EORA %s',     1,amDirect);
  MakeOpCode($99,'ADCA %s',     1,amDirect);
  MakeOpCode($9A,'ORA %s',      1,amDirect);
  MakeOpCode($9B,'ADDA %s',     1,amDirect);
  MakeOpCode($9C,'CMPX %s',     1,amDirect);
  MakeOpCode($9D,'JSR %s',      1,amDirect,[tc6809],TRUE);
  MakeOpCode($9E,'LDX %s',      1,amDirect);
  MakeOpCode($9F,'STX %s',      1,amDirect);

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

  MakeOpCode($C0,'SUBB #$%s',   1,amImmediate);
  MakeOpCode($C1,'CMPB #$%s',   1,amImmediate);
  MakeOpCode($C2,'SBCB #$%s',   1,amImmediate);
  MakeOpCode($C3,'ADDD #$%s',   2,amImmediate);
  MakeOpCode($C4,'ANDB #$%s',   1,amImmediate);
  MakeOpCode($C5,'BITB #$%s',   1,amImmediate);
  MakeOpCode($C6,'LDB #$%s',    1,amImmediate);
  MakeOpCode($C8,'EORB #$%s',   1,amImmediate);
  MakeOpCode($C9,'ADCB #$%s',   1,amImmediate);
  MakeOpCode($CA,'ORB #$%s',    1,amImmediate);
  MakeOpCode($CB,'ADDB #$%s',   1,amImmediate);
  MakeOpCode($CC,'LDD #$%s',    2,amImmediate);
  MakeOpCode($CE,'LDU #$%s',    2,amImmediate);

  MakeOpCode($D0,'SUBB %s',     1,amDirect);
  MakeOpCode($D1,'CMPB %s',     1,amDirect);
  MakeOpCode($D2,'SBCB %s',     1,amDirect);
  MakeOpCode($D3,'ADDD %s',     1,amDirect);
  MakeOpCode($D4,'ANDB %s',     1,amDirect);
  MakeOpCode($D5,'BITB %s',     1,amDirect);
  MakeOpCode($D6,'LDB %s',      1,amDirect);
  MakeOpCode($D7,'STB %s',      1,amDirect);
  MakeOpCode($D8,'EORB %s',     1,amDirect);
  MakeOpCode($D9,'ADCB %s',     1,amDirect);
  MakeOpCode($DA,'ORB %s',      1,amDirect);
  MakeOpCode($DB,'ADDB %s',     1,amDirect);
  MakeOpCode($DC,'LDD %s',     1,amDirect);
  MakeOpCode($DD,'STD %s',      1,amDirect);
  MakeOpCode($DE,'LDU %s',      1,amDirect);
  MakeOpCode($DF,'STU %s',      1,amDirect);

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
  MakeOpCode($EC,'LDD %s',     2,amIndexed);
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
  MakeOpCode($FC,'LDD %s',     3,amExtended);
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

  MakeOpCode($1083,'CMPD #$%s', 2,amImmediate);
  MakeOpCode($108C,'CMPY #$%s', 2,amImmediate);
  MakeOpCode($108E,'LDY #$%s',  2,amImmediate);

  MakeOpCode($1093,'CMPD %s',   1,amDirect);
  MakeOpCode($109C,'CMPY %s',   1,amDirect);
  MakeOpCode($109E,'LDY %s',    1,amDirect);
  MakeOpCode($109F,'STY %s',    1,amDirect);

  MakeOpCode($10A3,'CMPD %s',   2,amIndexed);
  MakeOpCode($10AC,'CMPY %s',   2,amIndexed);
  MakeOpCode($10AE,'LDY %s',    2,amIndexed);
  MakeOpCode($10AF,'STY %s',    2,amIndexed);

  MakeOpCode($10B3,'CMPD %s',   2,amExtended);
  MakeOpCode($10BC,'CMPY %s',   2,amExtended);
  MakeOpCode($10BE,'LDY %s',    2,amExtended);
  MakeOpCode($10BF,'STY %s',    2,amExtended);

  MakeOpCode($10CE,'LDS #$%s',  2,amImmediate);
  MakeOpCode($10DE,'LDS %s',    1,amDirect);
  MakeOpCode($10DF,'STS %s',    1,amDirect);
  MakeOpCode($10EE,'LDS %s',    2,amIndexed);
  MakeOpCode($10EF,'STS %s',    2,amIndexed);
  MakeOpCode($10FE,'LDS %s',    2,amExtended);
  MakeOpCode($10FF,'STS %s',    2,amExtended);

  MakeOpCode($113F,'SWI3',      0,amImplied);
  MakeOpCode($1183,'CMPU #$%s', 2,amImmediate);
  MakeOpCode($118C,'CMPS #$%s', 2,amImmediate);
  MakeOpCode($1193,'CMPU %s',   1,amDirect);
  MakeOpCode($119C,'CMPS %s',   1,amDirect);
  MakeOpCode($11A3,'CMPU %s',   2,amIndexed);
  MakeOpCode($11AC,'CMPS %s',   2,amIndexed);
  MakeOpCode($11B3,'CMPU %s',   2,amExtended);
  MakeOpCode($11BC,'CMPS %s',   2,amExtended);
END;

PROCEDURE TDisassembler6809.InitDirectives;

BEGIN
  WITH MemoryList DO
  BEGIN
    Parameters[mlLabelPrefix]:= '';
    Parameters[mlLabelSuffix]:= '';
    Parameters[mlDefineByte]:=  'FCB';
    Parameters[mlDefineWord]:=  'FDB';
    Parameters[mlDefineDWord]:= 'FQB';
    Parameters[mlDefineString]:='FCC';
    Parameters[mlOrigin]:=      'org';
    Parameters[mlBeginIgnore]:= ' ifeq 1';
    Parameters[mlEndIgnore]:=   ' endc';
    Parameters[mlSaveCmd]:=     '; ';
    Parameters[mlEquate]:=      'EQU';
  END;
END;

end.
