unit DisassemblerUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

USES Types,SysUtils,Classes,CPUMemoryUnit, SymbolListUnit,
     MemoryListUnit,BeebDisDefsUnit,ConsoleUnit;

TYPE
    TCPU = (tc6502, tc65c02, tc6512);

    TBranch = (brNone,brRelative,brAbsolute,brRtsRti);

    TOpCode = RECORD
      OpStr 	: STRING;
      OpBytes   : INTEGER;
      CPU	    : TCPU;
      Branch	: TBranch;
      Valid	    : BOOLEAN;
    END;

    TDisassemblerUnit = Class(TObject)
    PROTECTED
 	  FOpCodes	: ARRAY[$00..$FF]OF TOpCode;
	  FVerbose	: BOOLEAN;

      PROCEDURE DecodeInstruction;
	  PROCEDURE InitOpcodes;
	  PROCEDURE MakeOpCode(OpNo	    : INTEGER;
	    	               InOpStr	: STRING;
			               InOpBytes	: INTEGER;
			               InCPU	    : TCPU;
			               InBranch	: TBranch = brNone);
	  PROCEDURE SetVerbose(NewValue	: BOOLEAN);
    PUBLIC
      SymbolList		: TSymbolList;
	  EntryPoints		: TSymbolList;
	  Memory			: TCPUmemory;
	  MemoryList		: TMemoryList;
	  PROPERTY Verbose 	: BOOLEAN READ FVerbose WRITE SetVerbose;
        
      CONSTRUCTOR Create;
      DESTRUCTOR Destroy; override;
      PROCEDURE Go;
	  PROCEDURE LoadFromFile(FileName	: STRING;
			                 PBaseAddr	: DWORD);
     END;

implementation

CONST	OpCodeBRK	= 0;

CONSTRUCTOR TDisassemblerUnit.Create;

BEGIN;
  INHERITED Create;
  Memory:=TCPUmemory.Create;
  SymbolList:=TSymbolList.Create('SymbolList');
  EntryPoints:=TSymbolList.Create('EntryPoints');
  MemoryList:=TMemoryList.Create(Memory,SymbolList,EntryPoints);
  InitOpcodes;
  Verbose:=FALSE;
END;

DESTRUCTOR TDisassemblerUnit.Destroy;

BEGIN;
  MemoryList.Free;
  EntryPoints.Free;
  SymbolList.Free;
  Memory.Free;
  INHERITED Destroy;
END;

PROCEDURE TDisassemblerUnit.SetVerbose(NewValue	: BOOLEAN);

BEGIN;
  FVerbose:=NewValue;
  EntryPoints.Verbose:=FVerbose;
  SymbolList.Verbose:=FALSE;
  SymbolList.Verbose:=FVerbose;
END;

PROCEDURE TDisassemblerUnit.LoadFromFile(FileName	: STRING;
			       		 PBaseAddr	: DWORD);

BEGIN;
  Memory.LoadFromFile(FileName,PBaseAddr);
  EntryPoints.SetRange(Memory.EndAddr,Memory.BaseAddr);
  IF (FVerbose) THEN
    WriteLnFmt('Loaded %s at $%4.4X - $%4.4X',[FileName,Memory.BaseAddr,Memory.EndAddr]);
END;

PROCEDURE TDisassemblerUnit.Go;

VAR	EntryPoint	: DWORD;

BEGIN;
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

PROCEDURE TDisassemblerUnit.DecodeInstruction;

VAR	OpCode		: BYTE;
	Op		    : TOpCode;
    ByteParam	: BYTE;
    WordParam	: WORD;
	TargetAddr	: WORD;
    IsImmediate	: BOOLEAN;
    Instruction	: STRING;
    TargetLable	: STRING;
    Location	: WORD;

BEGIN;
  {Get the current PC, we need this later}
  Location:=Memory.PC;

  {Fetch the opcode, and look it up}
  OpCode:=Memory.ReadByte(IsDone);
  Op:=FOpCodes[OpCode];

  { Init them to something sensible, and silence compiler warning! }
  ByteParam:=0;
  WordParam:=0;

  if (Opcode=$60) THEN
    WriteLn(Format('RTS at $%4.4X',[Location]));

  IF (Op.Valid) THEN
  BEGIN;
    IsImmediate:=(Pos('#',Op.OpStr)>0);

    IF (Op.OpBytes>0) THEN
    BEGIN;
      IF (Op.OpBytes=1) THEN
        ByteParam:=Memory.ReadByte(IsDone);

      IF (Op.OpBytes=2) THEN
        WordParam:=Memory.ReadWord(IsDone);

      IF (Op.Branch=brRelative) THEN
      BEGIN;
        { Relative branch calculate offset from PC }
        IF (ByteParam < $80) THEN
          TargetAddr:=Memory.PC+(ByteParam)
        ELSE
          TargetAddr:=Memory.PC-256+ByteParam;
      END
      ELSE
      BEGIN;
        IF (Op.OpBytes=2) THEN
          TargetAddr:=WordParam	{ Absolute address }
        ELSE
          TargetAddr:=ByteParam;	{ Zero Page }
      END;

      IF (NOT IsImmediate) THEN
        TargetLable:=SymbolList.GetSymbol(TargetAddr)
      ELSE
        TargetLable:=Format('%2.2X',[ByteParam]);

      {If this was a branch / jump or subroutine call add to Entry points}
      IF (Op.Branch<>brNone) THEN
        EntryPoints.GetSymbol(TargetAddr);
    END;
    {Generate the line of code}
    Instruction:=Format(Op.OpStr,[TargetLable]);

    {Byte following a BRK instruction is always ignored by the CPU so }
    {mark as data so it does not generate an invalid opcode}
    IF ((OpCode=OpCodeBRK) AND (Memory.GetFlag=IsCode)) THEN
      MemoryList.AddData(tyDataByte,'pc',1);
  END
  ELSE
  BEGIN;
    Instruction:=Format('; PC=%4.4X INVALID opcode %2.2x',[Location,OpCode]);
  END;

//  IF (Op.Branch<>brRtsRti) THEN
  BEGIN;
    IF (Op.Branch=brNone) THEN
      MemoryList.AddCode(Location,Op.OpBytes+1,Instruction,FALSE)
    ELSE
      MemoryList.AddCode(Location,Op.OpBytes+1,Instruction,TRUE);
  END;
END;

PROCEDURE TDisassemblerUnit.MakeOpCode(OpNo		    : INTEGER;
			     	                   InOpStr		: STRING;
			     	                   InOpBytes	: INTEGER;
			     	                   InCPU		: TCPU;
			     	                   InBranch		: TBranch = brNone);

BEGIN;
  WITH FOpCodes[OpNo] DO
  BEGIN;
    OpStr:=InOpStr;
    OpBytes:=InOpBytes;
    CPU:=InCPU;
    Branch:=InBranch;
    Valid:=TRUE;
  END;
END;

PROCEDURE TDisassemblerUnit.InitOpcodes;

VAR	OpNo	: INTEGER;

BEGIN;
  FOR OpNo:=$00 TO $FF DO
    FOpCodes[OpNo].Valid:=FALSE;

  MakeOpCode($00,'BRK',		    0,tc6502);
  MakeOpCode($01,'ORA (%s,X)',	1,tc6502);
  MakeOpCode($05,'ORA %s', 	    1,tc6502);
  MakeOpCode($06,'ASL %s', 	    1,tc6502);
  MakeOpCode($08,'PHP',		    0,tc6502);
  MakeOpCode($09,'ORA #$%s',	1,tc6502);
  MakeOpCode($0A,'ASL A',	    0,tc6502);
  MakeOpCode($0D,'ORA %s',	    2,tc6502);
  MakeOpCode($0E,'ASL %s',	    2,tc6502);

  MakeOpCode($10,'BPL %s',	    1,tc6502,brRelative);
  MakeOpCode($11,'ORA (%s),Y',	1,tc6502);
  MakeOpCode($15,'ORA %s,X',	1,tc6502);
  MakeOpCode($16,'ASL %s,X',	1,tc6502);
  MakeOpCode($18,'CLC',		    0,tc6502);
  MakeOpCode($19,'ORA %s,Y',	2,tc6502);
  MakeOpCode($1D,'ORA %s,X',	2,tc6502);
  MakeOpCode($1E,'ASL %s,X',	2,tc6502);

  MakeOpCode($20,'JSR %s',	    2,tc6502,brAbsolute);
  MakeOpCode($21,'AND (%s,X)',	1,tc6502);
  MakeOpCode($24,'BIT %s',	    1,tc6502);
  MakeOpCode($25,'AND %s',	    1,tc6502);
  MakeOpCode($26,'ROL %s',	    1,tc6502);
  MakeOpCode($28,'PLP',		    0,tc6502);
  MakeOpCode($29,'AND #$%s',	1,tc6502);
  MakeOpCode($2A,'ROL A',	    0,tc6502);
  MakeOpCode($2C,'BIT %s',	    2,tc6502);
  MakeOpCode($2D,'AND %s',	    2,tc6502);
  MakeOpCode($2E,'ROL %s',	    2,tc6502);

  MakeOpCode($30,'BMI %s',	    1,tc6502,brRelative);
  MakeOpCode($31,'AND (%s),Y',	1,tc6502);
  MakeOpCode($35,'AND %s,X',	1,tc6502);
  MakeOpCode($36,'ROL %s,X',	1,tc6502);
  MakeOpCode($38,'SEC',		    0,tc6502);
  MakeOpCode($39,'AND %s,Y',	2,tc6502);
  MakeOpCode($3D,'AND %s,X',	2,tc6502);
  MakeOpCode($3E,'ROL %s,X',	2,tc6502);

  MakeOpCode($40,'RTI',		    0,tc6502,brRtsRti);
  MakeOpCode($41,'EOR (%s,X)',	1,tc6502);
  MakeOpCode($45,'EOR %s',	    1,tc6502);
  MakeOpCode($46,'LSR %s',	    1,tc6502);
  MakeOpCode($48,'PHA',		    0,tc6502);
  MakeOpCode($49,'EOR #$%s',	1,tc6502);
  MakeOpCode($4A,'LSR A',	    0,tc6502);
  MakeOpCode($4C,'JMP %s',	    2,tc6502,brAbsolute);
  MakeOpCode($4D,'EOR %s',	    2,tc6502);
  MakeOpCode($4E,'LSR %s',	    2,tc6502);

  MakeOpCode($50,'BVC %s',	    1,tc6502,brRelative);
  MakeOpCode($51,'EOR (%s),Y',	1,tc6502);
  MakeOpCode($55,'EOR %s,X',	1,tc6502);
  MakeOpCode($56,'LSR %s,X',	1,tc6502);
  MakeOpCode($58,'CLI',		    0,tc6502);
  MakeOpCode($59,'EOR %s,Y',	2,tc6502);
  MakeOpCode($5D,'EOR %s,X',	2,tc6502);
  MakeOpCode($5E,'LSR %s,X',	2,tc6502);

  MakeOpCode($60,'RTS',		    0,tc6502,brRtsRti);
  MakeOpCode($61,'ADC (%s,X)',	1,tc6502);
  MakeOpCode($65,'ADC %s',	    1,tc6502);
  MakeOpCode($66,'ROR %s',	    1,tc6502);
  MakeOpCode($68,'PLA',		    0,tc6502);
  MakeOpCode($69,'ADC #$%s',	1,tc6502);
  MakeOpCode($6A,'ROR A',	    0,tc6502);
  MakeOpCode($6C,'JMP (%s)',	2,tc6502,brAbsolute);
  MakeOpCode($6D,'ADC %s',	    2,tc6502);
  MakeOpCode($6E,'ROR %s',	    2,tc6502);

  MakeOpCode($70,'BVS %s',	    1,tc6502,brRelative);
  MakeOpCode($71,'ADC (%s),Y',	1,tc6502);
  MakeOpCode($75,'ADC %s,X',	1,tc6502);
  MakeOpCode($76,'ROR %s,X',	1,tc6502);
  MakeOpCode($78,'SEI',		    0,tc6502);
  MakeOpCode($79,'ADC %s,Y',	2,tc6502);
  MakeOpCode($7D,'ADC %s,X',	2,tc6502);
  MakeOpCode($7E,'ROR %s,X',	2,tc6502);

  MakeOpCode($81,'STA (%s,X)',	1,tc6502);
  MakeOpCode($84,'STY %s',	    1,tc6502);
  MakeOpCode($85,'STA %s',	    1,tc6502);
  MakeOpCode($86,'STX %s',	    1,tc6502);
  MakeOpCode($88,'DEY',		    0,tc6502);
  MakeOpCode($8A,'TXA',		    0,tc6502);
  MakeOpCode($8C,'STY %s',	    2,tc6502);
  MakeOpCode($8D,'STA %s',	    2,tc6502);
  MakeOpCode($8E,'STX %s',	    2,tc6502);

  MakeOpCode($90,'BCC %s',	    1,tc6502,brRelative);
  MakeOpCode($91,'STA (%s),Y',	1,tc6502);
  MakeOpCode($94,'STY %s,X',	1,tc6502);
  MakeOpCode($95,'STA %s,X',	1,tc6502);
  MakeOpCode($96,'STX %s,Y',	1,tc6502);
  MakeOpCode($98,'TYA',		    0,tc6502);
  MakeOpCode($99,'STA %s,Y',	2,tc6502);
  MakeOpCode($9A,'TXS',		    0,tc6502);
  MakeOpCode($9D,'STA %s,X',	2,tc6502);

  MakeOpCode($A0,'LDY #$%s',	1,tc6502);
  MakeOpCode($A1,'LDA (%s,X)',  1,tc6502);
  MakeOpCode($A2,'LDX #$%s',	1,tc6502);
  MakeOpCode($A4,'LDY %s',	    1,tc6502);
  MakeOpCode($A5,'LDA %s',	    1,tc6502);
  MakeOpCode($A6,'LDX %s',	    1,tc6502);
  MakeOpCode($A8,'TAY',		    0,tc6502);
  MakeOpCode($A9,'LDA #$%s',	1,tc6502);
  MakeOpCode($AA,'TAX',		    0,tc6502);
  MakeOpCode($AC,'LDY %s',	    2,tc6502);
  MakeOpCode($AD,'LDA %s',	    2,tc6502);
  MakeOpCode($AE,'LDX %s',	    2,tc6502);

  MakeOpCode($B0,'BCS %s',	    1,tc6502,brRelative);
  MakeOpCode($B1,'LDA (%s),Y',	1,tc6502);
  MakeOpCode($B4,'LDY %s,X',	1,tc6502);
  MakeOpCode($B5,'LDA %s,X',	1,tc6502);
  MakeOpCode($B6,'LDX %s,Y',	1,tc6502);
  MakeOpCode($B8,'CLV',		    0,tc6502);
  MakeOpCode($B9,'LDA %s,Y',	2,tc6502);
  MakeOpCode($BA,'TSX',		    0,tc6502);
  MakeOpCode($BC,'LDY %s,X',	2,tc6502);
  MakeOpCode($BD,'LDA %s,X',	2,tc6502);
  MakeOpCode($BE,'LDX %s,Y',	2,tc6502);

  MakeOpCode($C0,'CPY #$%s',	1,tc6502);
  MakeOpCode($C1,'CMP (%s,X)',	1,tc6502);
  MakeOpCode($C4,'CPY %s',	    1,tc6502);
  MakeOpCode($C5,'CMP %s',	    1,tc6502);
  MakeOpCode($C6,'DEC %s',	    1,tc6502);
  MakeOpCode($C8,'INY',		    0,tc6502);
  MakeOpCode($C9,'CMP #$%s',	1,tc6502);
  MakeOpCode($CA,'DEX',		    0,tc6502);
  MakeOpCode($CC,'CPY %s',	    2,tc6502);
  MakeOpCode($CD,'CMP %s',	    2,tc6502);
  MakeOpCode($CE,'DEC %s',	    2,tc6502);

  MakeOpCode($D0,'BNE %s',	    1,tc6502,brRelative);
  MakeOpCode($D1,'CMP (%s),Y',	1,tc6502);
  MakeOpCode($D5,'CMP %s,X',	1,tc6502);
  MakeOpCode($D6,'DEC %s,X',	1,tc6502);
  MakeOpCode($D8,'CLD',		    0,tc6502);
  MakeOpCode($D9,'CMP %s,Y',	2,tc6502);
  MakeOpCode($DD,'CMP %s,X',	2,tc6502);
  MakeOpCode($DE,'DEC %s,X',	2,tc6502);

  MakeOpCode($E0,'CPX #$%s',	1,tc6502);
  MakeOpCode($E1,'SBC (%s,X)',  1,tc6502);
  MakeOpCode($E4,'CPX %s',	    1,tc6502);
  MakeOpCode($E5,'SBC %s',     	1,tc6502);
  MakeOpCode($E6,'INC %s',     	1,tc6502);
  MakeOpCode($E8,'INX',	       	0,tc6502);
  MakeOpCode($E9,'SBC #$%s',	1,tc6502);
  MakeOpCode($EA,'NOP',	       	0,tc6502);
  MakeOpCode($EC,'CPX %s',     	2,tc6502);
  MakeOpCode($ED,'SBC %s',     	2,tc6502);
  MakeOpCode($EE,'INC %s',     	2,tc6502);

  MakeOpCode($F0,'BEQ %s',     	1,tc6502,brRelative);
  MakeOpCode($F1,'SBC (%s),Y',	1,tc6502);
  MakeOpCode($F5,'SBC %s,X',	1,tc6502);
  MakeOpCode($F6,'INC %s,X',	1,tc6502);
  MakeOpCode($F8,'SED',	       	0,tc6502);
  MakeOpCode($F9,'SBC %s,Y',	2,tc6502);
  MakeOpCode($FD,'SBC %s,X',	2,tc6502);
  MakeOpCode($FE,'INC %s,X',	2,tc6502);
END;

end.
