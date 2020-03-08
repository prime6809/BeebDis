UNIT AbstractDisassemblerUnit;

{$mode delphi}

INTERFACE

USES
  Classes, SysUtils,CPUMemoryUnit, SymbolListUnit, StrUtils,
  MemoryListUnit,ConsoleUnit,ParameterListUnit,BeebDisDefsUnit;

CONST
    MinOpCode       = 0;
    MaxOpCode       = $FFFF;
    DefRadix        = 10;
    DefVerbosity    = 1;


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
      FOpCodes	    : ARRAY[MinOpCode..MaxOpCode]OF TOpCode;
      FVerbosity    : BYTE;
      FCPU          : TCPU;
      FMaxCPU       : TCPU;
      FMinCPU       : TCPU;
      FRadix        : BYTE;
      FSwapWords    : BOOLEAN;

      FUNCTION CalcRelative8(RelOffset     : Shortint) : WORD;
      FUNCTION CalcRelative16(RelOffset     : Smallint) : WORD;
      PROCEDURE DecodeInstruction;   virtual; abstract;
      PROCEDURE InitOpcodes;         virtual;
      PROCEDURE InitDirectives;      virtual; abstract;
      PROCEDURE SetVerbosity(NewValue	: BYTE);
      PROCEDURE SetCPU(CPUType  : TCPU);
      PROCEDURE SetRadix(ARadix : BYTE);
      FUNCTION FormatInvalid(ALocation  : WORD;
                             AOpCode    : WORD;
                             ABytes     : BYTE = 1) : STRING;
      FUNCTION FormatNum(ANumber        : CARDINAL;
                         APlaces        : INTEGER) : STRING;
    PUBLIC
      SymbolList		: TSymbolList;
      EntryPoints		: TSymbolList;
      Memory			: TCPUmemory;
      MemoryList		: TMemoryList;
      Parameters        : TParameterList;

      PROPERTY Verbosity: BYTE READ FVerbosity WRITE SetVerbosity;
      PROPERTY CPU      : TCPU READ FCPU WRITE SetCPU;
      PROPERTY Radix    : BYTE READ FRadix WRITE SetRadix;
      PROPERTY SwapWords: BOOLEAN READ FSwapWords;

      CONSTRUCTOR Create;
      DESTRUCTOR Destroy; override;
      PROCEDURE Go; virtual;

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
  FVerbosity:=VBNormal;
  FCPU:=tcInvalid;
  FRadix:=DefRadix;
  FSwapWords:=FALSE;
END;

DESTRUCTOR TADisassembler.Destroy;

BEGIN;
  INHERITED Destroy;
END;

PROCEDURE TADisassembler.SetVerbosity(NewValue	: BYTE);

BEGIN;
  FVerbosity:=NewValue;
END;

FUNCTION TADisassembler.CalcRelative8(RelOffset     : ShortInt) : WORD;

BEGIN;
  { Relative branch calculate offset from PC }
  Result:=Memory.PC+(RelOffset)
END;

FUNCTION TADisassembler.CalcRelative16(RelOffset     : SmallInt) : WORD;

BEGIN;
  { Relative branch calculate offset from PC }
    Result:=Memory.PC+(RelOffset)
END;

PROCEDURE TADisassembler.InitOpcodes;

VAR OpIdx   : INTEGER;

BEGIN;
  FOR OpIdx:=MinOpCode TO MaxOpCode DO
    FOpCodes[OpIdx]:=NIL;
END;

PROCEDURE TADisassembler.Go;

VAR ItemNo  : INTEGER;

BEGIN;
  InitOpcodes;
  InitDirectives;
  SymbolList.ImportFiles;
  SymbolList.SafeAddAddress(Memory.BaseAddr,StartAddrLable,FALSE);

  FOR ItemNo:=0 TO (EntryPoints.Count-1) DO
  BEGIN;

  END;
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

PROCEDURE TADisassembler.SetRadix(ARadix : BYTE);

BEGIN;
  IF (ARadix IN [2,8,10,16]) THEN
    FRadix:=ARadix;
END;

FUNCTION TADisassembler.FormatInvalid(ALocation  : WORD;
                                      AOpCode    : WORD;
                                      ABytes     : BYTE = 1) : STRING;

BEGIN;
  IF (ABytes=1) THEN
    Result:=Format('%s    $%2.2X    %s PC=%4.4X INVALID opcode %2.2x',
                  [Parameters[mlDefineByte],(AOpCode AND $0FF),
                   Parameters[mlCommentChar],ALocation,AOpCode])
  ELSE
      Result:=Format('%s    $%4.4X    %s PC=%4.4X INVALID opcode %2.2x',
                  [Parameters[mlDefineWord],AOpCode,
                   Parameters[mlCommentChar],ALocation,AOpCode])
END;

FUNCTION TADisassembler.FormatNum(ANumber        : CARDINAL;
                                  APlaces        : INTEGER) : STRING;

BEGIN;
  Result:=Dec2Numb(ANumber,APlaces,FRadix);
  CASE FRadix OF
    2   : Result:=Parameters[numBinPrefix]+Result+Parameters[numBinSuffix];
    8   : Result:=Parameters[numOctPrefix]+Result+Parameters[numOctSuffix];
    10  : Result:=Parameters[numDecPrefix]+Result+Parameters[numDecSuffix];
    16  : Result:=Parameters[numHexPrefix]+Result+Parameters[numHexSuffix];
  END;
END;


end.
