UNIT AbstractDisassemblerUnit;

{$mode delphi}

INTERFACE

USES
  Classes, SysUtils,CPUMemoryUnit, SymbolListUnit,
  MemoryListUnit,ConsoleUnit;

CONST
    MinOpCode   = 0;
    MaxOpCode   = $FFFF;


TYPE
    TCPU    = (tcInvalid, tc6502, tc65c02, tc65c02wd, tc6512, tc6809);
    TCPUSet = SET OF TCPU;

    TOpCode = Class(TObject)
      OpCode    : WORD;
      OpStr 	: STRING;
      OpBytes   : INTEGER;
      CPU	    : TCPUSet;
      CONSTRUCTOR Create;
    END;

    TADisassembler = Class(TObject)
    PROTECTED
      FOpCodes	: ARRAY[MinOpCode..MaxOpCode]OF TOpCode;
      FVerbose	: BOOLEAN;
      FCPU      : TCPU;
      FMaxCPU   : TCPU;
      FMinCPU   : TCPU;

      FUNCTION CalcRelative8(RelOffset     : Shortint) : WORD;
      FUNCTION CalcRelative16(RelOffset     : Smallint) : WORD;
      PROCEDURE DecodeInstruction;   virtual; abstract;
      PROCEDURE InitOpcodes;         virtual;
      PROCEDURE InitDirectives;      virtual; abstract;
      PROCEDURE SetVerbose(NewValue	: BOOLEAN);
      PROCEDURE SetCPU(CPUType  : TCPU);
    PUBLIC
      SymbolList		: TSymbolList;
      EntryPoints		: TSymbolList;
      Memory			: TCPUmemory;
      MemoryList		: TMemoryList;
      PROPERTY Verbose 	: BOOLEAN READ FVerbose WRITE SetVerbose;
      PROPERTY CPU      : TCPU READ FCPU WRITE SetCPU;

      CONSTRUCTOR Create;
      DESTRUCTOR Destroy; override;
      PROCEDURE Go; virtual; abstract;
      FUNCTION ValidCPU(CPUType : TCPU) : BOOLEAN;
    END;

implementation

CONSTRUCTOR TOpCode.Create;

BEGIN;
  INHERITED Create;
  OpStr:='';
  CPU:=[];
END;

CONSTRUCTOR TADisassembler.Create;

BEGIN;
  INHERITED Create;
  FVerbose:=FALSE;
  FCPU:=tcInvalid;
END;

DESTRUCTOR TADisassembler.Destroy;

BEGIN;
  INHERITED Destroy;
END;

PROCEDURE TADisassembler.SetVerbose(NewValue	: BOOLEAN);

BEGIN;
  FVerbose:=NewValue;
END;

FUNCTION TADisassembler.CalcRelative8(RelOffset     : ShortInt) : WORD;

BEGIN;
  { Relative branch calculate offset from PC }
//  IF (RelOffset < $80) THEN
    Result:=Memory.PC+(RelOffset)
//  ELSE
//    Result:=Memory.PC-256+RelOffset;
END;

FUNCTION TADisassembler.CalcRelative16(RelOffset     : SmallInt) : WORD;

BEGIN;
  { Relative branch calculate offset from PC }
//  IF (RelOffset < $8000) THEN
    Result:=Memory.PC+(RelOffset)
//  ELSE
//    Result:=Memory.PC-32768+RelOffset;
END;

PROCEDURE TADisassembler.InitOpcodes;

VAR OpIdx   : INTEGER;

BEGIN;
  FOR OpIdx:=MinOpCode TO MaxOpCode DO
    FOpCodes[OpIdx]:=NIL;
END;

FUNCTION TADisassembler.ValidCPU(CPUType : TCPU) : BOOLEAN;

BEGIN;
  Result:=((CPUType >= FMinCPU) AND (CPUType <= FMaxCPU));
END;

PROCEDURE TADisassembler.SetCPU(CPUType  : TCPU);
BEGIN;
  IF (ValidCPU(CPUType)) THEN
    FCPU:=CPUType;
END;

end.
