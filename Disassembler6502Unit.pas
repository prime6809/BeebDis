unit Disassembler6502Unit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

USES Types,SysUtils,Classes,CPUMemoryUnit, SymbolListUnit,
     MemoryListUnit,AbstractDisassemblerUnit,
     ParameterListUnit,ConsoleUnit;

TYPE
    TBranch = (brNone,brRelative,brAbsolute,brRtsRti,brZPRelative);

    TOpCode6502 = Class(TOpCode)
      OpStr 	: STRING;
      OpBytes   : INTEGER;
      CPU	    : TCPUSet;
      Branch	: TBranch;
    END;

    TDisassembler6502 = Class(TADisassembler)
    PROTECTED
      PROCEDURE DecodeInstruction;  override;
	  PROCEDURE InitOpcodes;        override;
      PROCEDURE InitDirectives;     override;

	  PROCEDURE MakeOpCode(OpNo	        : INTEGER;
	    	               InOpStr	    : STRING;
			               InOpBytes	: INTEGER;
			               InCPU	    : TCPUSet;
			               InBranch	    : TBranch = brNone);
    PUBLIC
      CONSTRUCTOR Create;
      DESTRUCTOR Destroy; override;
      PROCEDURE Go;  override;
    END;

implementation

CONST	OpCodeBRK	= 0;

CONSTRUCTOR TDisassembler6502.Create;

BEGIN;
  INHERITED Create;
  FCPU:=tc6502;
  FMinCPU:=tc6502;
  FMaxCPU:=tc6512;
END;

DESTRUCTOR TDisassembler6502.Destroy;

VAR	OpNo	: INTEGER;

BEGIN;
  FOR OpNo:=$00 TO $FF DO
    FOpCodes[OpNo].Free;

  INHERITED Destroy;
END;

PROCEDURE TDisassembler6502.Go;

VAR	EntryPoint	: DWORD;

BEGIN;
  INHERITED Go;

  {If no entry point is speccified, assume the base address}
  IF (EntryPoints.Count<1) THEN
    EntryPoints.GetSymbol(Memory.BaseAddr);

  WHILE (EntryPoints.Count>0) DO
  BEGIN;
    EntryPoint:=EntryPoints.Addresses[0];
    SymbolList.SafeAddAddress(EntryPoint,'',TRUE);
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

PROCEDURE TDisassembler6502.DecodeInstruction;

VAR	OpCode		: BYTE;
	Op		    : TOpCode6502;
    ByteParam	: BYTE;
    ByteParam2	: BYTE;
    WordParam	: WORD;
	TargetAddr	: WORD;
    IsImmediate	: BOOLEAN;
    Instruction	: STRING;
    TargetLable	: STRING;
    ZPLabel     : STRING;
    Location	: WORD;

BEGIN;
  {Get the current PC, we need this later}
  Location:=Memory.PC;

  {Fetch the opcode, and look it up}
  OpCode:=Memory.ReadByte(IsDone);
  Op:=TOpCode6502(FOpCodes[OpCode]);

  { Init them to something sensible, and silence compiler warning! }
  ByteParam:=0;
  WordParam:=0;

  if (Opcode=$60) THEN
    WriteLnFmtV(FVerbosity,VBVerbose,'RTS at $%4.4X',[Location]);

  IF  ((Op<>NIL) AND (FCPU IN Op.CPU)) THEN
  BEGIN;
    IsImmediate:=(Pos('#',Op.OpStr)>0);

    IF (Op.OpBytes>0) THEN
    BEGIN;
      IF (Op.OpBytes=1) THEN
        ByteParam:=Memory.ReadByte(IsDone);

      IF (Op.OpBytes=2) THEN
      BEGIN;
        IF (Op.Branch=brZPRelative) THEN    { ZP relative we need to fetch bytes seperately}
        BEGIN
          ByteParam:=Memory.ReadByte(IsDone);
          ByteParam2:=Memory.ReadByte(IsDone);
        END
        ELSE
          WordParam:=Memory.ReadWord(IsDone);
      END;

      IF (Op.Branch=brRelative) THEN        { Relative address, Bcc etc }
      BEGIN;
        TargetAddr:=CalcRelative8(ByteParam);
      END
      ELSE IF (Op.Branch=brZPRelative) THEN { ZP then relative, wd65c02 BRRx etc }
      BEGIN
        TargetAddr:=CalcRelative8(ByteParam2);
      END
      ELSE
      BEGIN;
        IF (Op.OpBytes=2) THEN
          TargetAddr:=WordParam	    { Absolute address }
        ELSE
          TargetAddr:=ByteParam;	{ Zero Page }
      END;

      IF (Op.Branch=brZPRelative) THEN
      BEGIN
        TargetLable:=SymbolList.GetSymbol(TargetAddr,TRUE,1);
        ZPLabel:=SymbolList.GetSymbol(ByteParam,TRUE,1);
      END
      ELSE IF (NOT IsImmediate) THEN
        TargetLable:=SymbolList.GetSymbol(TargetAddr,TRUE,1)
      ELSE
        TargetLable:=Format('%2.2X',[ByteParam]);

      {If this was a branch / jump or subroutine call add to Entry points}
      IF (Op.Branch<>brNone) THEN
        EntryPoints.GetSymbol(TargetAddr);
    END;
    {Generate the line of code}
    IF(Op.Branch=brZPRelative) THEN
      Instruction:=Format(Op.OpStr,[ZPLabel, TargetLable])
    ELSE
      Instruction:=Format(Op.OpStr,[TargetLable]);

    {Byte following a BRK instruction is always ignored by the CPU so }
    {mark as data so it does not generate an invalid opcode}
    IF ((OpCode=OpCodeBRK) AND (Memory.GetFlag=IsCode)) THEN
      MemoryList.AddData(tyDataByte,'pc',1);

    IF (Op.Branch=brNone) THEN
      MemoryList.AddCode(Location,Op.OpBytes+1,Instruction,FALSE)
    ELSE
      MemoryList.AddCode(Location,Op.OpBytes+1,Instruction,TRUE);
  END
  ELSE
  BEGIN;
    Instruction:=Format('; PC=%4.4X INVALID opcode %2.2x',[Location,OpCode]);
  END;
END;

PROCEDURE TDisassembler6502.MakeOpCode(OpNo		    : INTEGER;
			     	                   InOpStr		: STRING;
			     	                   InOpBytes	: INTEGER;
			     	                   InCPU		: TCPUSet;
			     	                   InBranch		: TBranch = brNone);

VAR OpCode  : TOpCode6502;

BEGIN;
  OpCode:=TOpCode6502.Create;
  WITH OpCode DO
  BEGIN;
    OpStr:=InOpStr;
    OpBytes:=InOpBytes;
    CPU:=InCPU;
    Branch:=InBranch;
  END;
  FOpCodes[OpNo]:=OpCode;
END;

PROCEDURE TDisassembler6502.InitOpcodes;

BEGIN;
  MakeOpCode($00,'BRK',		    0,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($01,'ORA (%s,X)',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($04,'TSB %s',      1,[tc65c02,tc65c02wd]);
  MakeOpCode($05,'ORA %s', 	    1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($06,'ASL %s', 	    1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($07,'RMB0 %s',     1,[tc65c02wd]);
  MakeOpCode($08,'PHP',		    0,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($09,'ORA #$%s',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($0A,'ASL A',	    0,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($0C,'TSB %s',      2,[tc65c02,tc65c02wd]);
  MakeOpCode($0D,'ORA %s',	    2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($0E,'ASL %s',	    2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($0F,'BBR0 %s,%s',  2,[tc65c02wd],brZPRelative);

  MakeOpCode($10,'BPL %s',	    1,[tc6502,tc65c02,tc65c02wd],brRelative);
  MakeOpCode($11,'ORA (%s),Y',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($12,'ORA (%s)',    1,[tc65c02,tc65c02wd]);
  MakeOpCode($14,'TRB %s',      1,[tc65c02,tc65c02wd]);
  MakeOpCode($15,'ORA %s,X',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($16,'ASL %s,X',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($17,'RMB1 %s',     1,[tc65c02wd]);
  MakeOpCode($18,'CLC',		    0,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($19,'ORA %s,Y',	2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($1A,'INC A',       0,[tc65c02,tc65c02wd]);
  MakeOpCode($1C,'TRB %s',      2,[tc65c02,tc65c02wd]);
  MakeOpCode($1D,'ORA %s,X',	2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($1E,'ASL %s,X',	2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($1F,'BBR1 %s,%s',  2,[tc65c02wd],brZPRelative);

  MakeOpCode($20,'JSR %s',	    2,[tc6502,tc65c02,tc65c02wd],brAbsolute);
  MakeOpCode($21,'AND (%s,X)',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($24,'BIT %s',	    1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($25,'AND %s',	    1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($26,'ROL %s',	    1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($27,'RMB2 %s',     1,[tc65c02wd]);
  MakeOpCode($28,'PLP',		    0,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($29,'AND #$%s',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($2A,'ROL A',	    0,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($2C,'BIT %s',	    2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($2D,'AND %s',	    2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($2E,'ROL %s',	    2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($2F,'BBR2 %s,%s',  2,[tc65c02wd],brZPRelative);

  MakeOpCode($30,'BMI %s',	    1,[tc6502,tc65c02,tc65c02wd],brRelative);
  MakeOpCode($31,'AND (%s),Y',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($32,'AND (%s)',	1,[tc65c02,tc65c02wd]);
  MakeOpCode($34,'BIT %s,X',    1,[tc65c02,tc65c02wd]);
  MakeOpCode($35,'AND %s,X',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($36,'ROL %s,X',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($37,'RMB3 %s',     1,[tc65c02wd]);
  MakeOpCode($38,'SEC',		    0,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($39,'AND %s,Y',	2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($3A,'DEC A',       0,[tc65c02,tc65c02wd]);
  MakeOpCode($3C,'BIT %s,X',    2,[tc65c02,tc65c02wd]);
  MakeOpCode($3D,'AND %s,X',	2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($3E,'ROL %s,X',	2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($3F,'BBR3 %s,%s',  2,[tc65c02wd],brZPRelative);

  MakeOpCode($40,'RTI',		    0,[tc6502,tc65c02,tc65c02wd],brRtsRti);
  MakeOpCode($41,'EOR (%s,X)',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($45,'EOR %s',	    1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($46,'LSR %s',	    1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($47,'RMB4 %s',     1,[tc65c02wd]);
  MakeOpCode($48,'PHA',		    0,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($49,'EOR #$%s',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($4A,'LSR A',	    0,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($4C,'JMP %s',	    2,[tc6502,tc65c02,tc65c02wd],brAbsolute);
  MakeOpCode($4D,'EOR %s',	    2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($4E,'LSR %s',	    2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($4F,'BBR4 %s,%s',  2,[tc65c02wd],brZPRelative);

  MakeOpCode($50,'BVC %s',	    1,[tc6502,tc65c02,tc65c02wd],brRelative);
  MakeOpCode($51,'EOR (%s),Y',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($52,'EOR (%s)',	1,[tc65c02,tc65c02wd]);
  MakeOpCode($55,'EOR %s,X',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($56,'LSR %s,X',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($57,'RMB5 %s',     1,[tc65c02wd]);
  MakeOpCode($58,'CLI',		    0,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($59,'EOR %s,Y',	2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($5A,'PHY',         0,[tc65c02,tc65c02wd]);
  MakeOpCode($5D,'EOR %s,X',	2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($5E,'LSR %s,X',	2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($5F,'BBR5 %s,%s',  2,[tc65c02wd],brZPRelative);

  MakeOpCode($60,'RTS',		    0,[tc6502,tc65c02,tc65c02wd],brRtsRti);
  MakeOpCode($61,'ADC (%s,X)',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($64,'STZ %s',      1,[tc65c02,tc65c02wd]);
  MakeOpCode($65,'ADC %s',	    1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($66,'ROR %s',	    1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($67,'RMB6 %s',     1,[tc65c02wd]);
  MakeOpCode($68,'PLA',		    0,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($69,'ADC #$%s',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($6A,'ROR A',	    0,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($6C,'JMP (%s)',	2,[tc6502,tc65c02,tc65c02wd],brAbsolute);
  MakeOpCode($6D,'ADC %s',	    2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($6E,'ROR %s',	    2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($6F,'BBR6 %s,%s',  2,[tc65c02wd],brZPRelative);

  MakeOpCode($70,'BVS %s',	    1,[tc6502,tc65c02,tc65c02wd],brRelative);
  MakeOpCode($71,'ADC (%s),Y',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($72,'ADC (%s)',    1,[tc65c02,tc65c02wd]);
  MakeOpCode($74,'STZ %s,X',    1,[tc65c02,tc65c02wd]);
  MakeOpCode($75,'ADC %s,X',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($76,'ROR %s,X',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($77,'RMB7 %s',     1,[tc65c02wd]);
  MakeOpCode($78,'SEI',		    0,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($79,'ADC %s,Y',	2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($7A,'PLY',         0,[tc65c02,tc65c02wd]);
  MakeOpCode($7C,'JMP (%s,X)',  2,[tc65c02,tc65c02wd]);
  MakeOpCode($7D,'ADC %s,X',	2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($7E,'ROR %s,X',	2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($7F,'BBR7 %s,%s',  2,[tc65c02wd],brZPRelative);

  MakeOpCode($80,'BRA %s',      1,[tc65c02,tc65c02wd],brRelative);
  MakeOpCode($81,'STA (%s,X)',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($84,'STY %s',	    1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($85,'STA %s',	    1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($86,'STX %s',	    1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($87,'SMB0 %s',     1,[tc65c02wd]);
  MakeOpCode($88,'DEY',		    0,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($89,'BIT #%s',     1,[tc65c02,tc65c02wd]);
  MakeOpCode($8A,'TXA',		    0,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($8C,'STY %s',	    2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($8D,'STA %s',	    2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($8E,'STX %s',	    2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($8F,'BBS0 %s,%s',  2,[tc65c02wd],brZPRelative);

  MakeOpCode($90,'BCC %s',	    1,[tc6502,tc65c02,tc65c02wd],brRelative);
  MakeOpCode($91,'STA (%s),Y',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($92,'STA (%s)',    1,[tc65c02,tc65c02wd]);
  MakeOpCode($94,'STY %s,X',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($95,'STA %s,X',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($96,'STX %s,Y',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($97,'SMB1 %s',     1,[tc65c02wd]);
  MakeOpCode($98,'TYA',		    0,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($99,'STA %s,Y',	2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($9A,'TXS',		    0,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($9C,'STZ %s',      2,[tc65c02,tc65c02wd]);
  MakeOpCode($9D,'STA %s,X',	2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($9E,'STZ %s,X',    2,[tc65c02,tc65c02wd]);
  MakeOpCode($9F,'BBS1 %s,%s',  2,[tc65c02wd],brZPRelative);

  MakeOpCode($A0,'LDY #$%s',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($A1,'LDA (%s,X)',  1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($A2,'LDX #$%s',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($A4,'LDY %s',	    1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($A5,'LDA %s',	    1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($A6,'LDX %s',	    1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($A7,'SMB2 %s',     1,[tc65c02wd]);
  MakeOpCode($A8,'TAY',		    0,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($A9,'LDA #$%s',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($AA,'TAX',		    0,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($AC,'LDY %s',	    2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($AD,'LDA %s',	    2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($AE,'LDX %s',	    2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($AF,'BBS2 %s,%s',  2,[tc65c02wd],brZPRelative);

  MakeOpCode($B0,'BCS %s',	    1,[tc6502,tc65c02,tc65c02wd],brRelative);
  MakeOpCode($B1,'LDA (%s),Y',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($B2,'LDA (%s)',	1,[tc65c02,tc65c02wd]);
  MakeOpCode($B4,'LDY %s,X',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($B5,'LDA %s,X',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($B6,'LDX %s,Y',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($B7,'SMB3 %s',     1,[tc65c02wd]);
  MakeOpCode($B8,'CLV',		    0,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($B9,'LDA %s,Y',	2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($BA,'TSX',		    0,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($BC,'LDY %s,X',	2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($BD,'LDA %s,X',	2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($BE,'LDX %s,Y',	2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($BF,'BBS3 %s,%s',  2,[tc65c02wd],brZPRelative);

  MakeOpCode($C0,'CPY #$%s',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($C1,'CMP (%s,X)',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($C4,'CPY %s',	    1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($C5,'CMP %s',	    1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($C6,'DEC %s',	    1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($C7,'SMB4 %s',     1,[tc65c02wd]);
  MakeOpCode($C8,'INY',		    0,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($C9,'CMP #$%s',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($CA,'DEX',		    0,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($CB,'WAI',         0,[tc65c02wd]);
  MakeOpCode($CC,'CPY %s',	    2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($CD,'CMP %s',	    2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($CE,'DEC %s',	    2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($CF,'BBS4 %s,%s',  2,[tc65c02wd],brZPRelative);

  MakeOpCode($D0,'BNE %s',	    1,[tc6502,tc65c02,tc65c02wd],brRelative);
  MakeOpCode($D1,'CMP (%s),Y',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($D2,'CMP (%s)',    1,[tc65c02,tc65c02wd]);
  MakeOpCode($D5,'CMP %s,X',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($D6,'DEC %s,X',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($D7,'SMB5 %s',     1,[tc65c02wd]);
  MakeOpCode($D8,'CLD',		    0,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($D9,'CMP %s,Y',	2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($DA,'PHX',         0,[tc65c02,tc65c02wd]);
  MakeOpCode($DB,'STP',         0,[tc65c02wd]);
  MakeOpCode($DD,'CMP %s,X',	2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($DE,'DEC %s,X',	2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($DF,'BBS5 %s,%s',  2,[tc65c02wd],brZPRelative);

  MakeOpCode($E0,'CPX #$%s',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($E1,'SBC (%s,X)',  1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($E4,'CPX %s',	    1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($E5,'SBC %s',     	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($E6,'INC %s',     	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($E7,'SMB6 %s',     1,[tc65c02wd]);
  MakeOpCode($E8,'INX',	       	0,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($E9,'SBC #$%s',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($EA,'NOP',	       	0,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($EC,'CPX %s',     	2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($ED,'SBC %s',     	2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($EE,'INC %s',     	2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($EF,'BBS6 %s,%s',  2,[tc65c02wd],brZPRelative);

  MakeOpCode($F0,'BEQ %s',     	1,[tc6502,tc65c02,tc65c02wd],brRelative);
  MakeOpCode($F1,'SBC (%s),Y',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($F2,'SBC (%s)',	1,[tc65c02,tc65c02wd]);
  MakeOpCode($F5,'SBC %s,X',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($F6,'INC %s,X',	1,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($F7,'SMB7 %s',     1,[tc65c02wd]);
  MakeOpCode($F8,'SED',	       	0,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($F9,'SBC %s,Y',	2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($FA,'PLX',         0,[tc65c02,tc65c02wd]);
  MakeOpCode($FD,'SBC %s,X',	2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($FE,'INC %s,X',	2,[tc6502,tc65c02,tc65c02wd]);
  MakeOpCode($FF,'BBS7 %s,%s',  2,[tc65c02wd],brZPRelative);
END;

PROCEDURE TDisassembler6502.InitDirectives;

BEGIN
  Parameters.Overwrite:=FALSE;

  Parameters[mlLabelPrefix]:=   '.';
  Parameters[mlLabelSuffix]:=   '';
  Parameters[mlDefineByte]:=    'EQUB';
  Parameters[mlDefineWord]:=    'EQUW';
  Parameters[mlDefineDWord]:=   'EQUD';
  Parameters[mlDefineString]:=  'EQUS';
  Parameters[mlOrigin]:=        'org';
  Parameters[mlBeginIgnore]:=   'if(0)';
  Parameters[mlEndIgnore]:=     'endif';
  Parameters[mlSaveCmd]:=       'SAVE';
  Parameters[mlEquate]:=        '=';
  Parameters[mlCommentChar]:=   ';';
  Parameters[mlCommentCol]:=    '40';

  Parameters[numBinPrefix]:=    '%';
  Parameters[numOctPrefix]:=    '@';
  Parameters[numHexPrefix]:=    '$';

  Parameters.Overwrite:=TRUE;
END;

end.
