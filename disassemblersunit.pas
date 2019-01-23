UNIT DisassemblersUnit;

{$mode delphi}

INTERFACE

USES
    Types,SysUtils,Classes,Contnrs, CPUMemoryUnit, SymbolListUnit,
    MemoryListUnit,AbstractDisassemblerUnit,
    Disassembler6809Unit, ConsoleUnit, ParameterListUnit;

CONST
    Invalid = -1;

TYPE
    TMetaDisassembler = class(TObjectList)
    PROTECTED
      FSelected         : INTEGER;
      FMax              : INTEGER;
      FVerbosity        : BYTE;
      FCPU              : TCPU;
      FRadix            : BYTE;

      PROCEDURE SetVerbosity(NewValue	: BYTE);
      FUNCTION CPUNameToType(CPUName     : STRING) : TCPU;
      PROCEDURE SetRadix(ARadix : BYTE);
    PUBLIC
      SymbolList		: TSymbolList;
      EntryPoints		: TSymbolList;
      Memory			: TCPUmemory;
      MemoryList		: TMemoryList;
      Parameters        : TParameterList;

      PROPERTY Verbosity: BYTE READ FVerbosity WRITE SetVerbosity;
      PROPERTY CPU      : TCPU READ FCPU;
      PROPERTY Radix    : BYTE READ FRadix WRITE SetRadix;

      CONSTRUCTOR Create;
      DESTRUCTOR Destroy; override;
      PROCEDURE Add(ADisassembler   : TADisassembler);
      PROCEDURE Go;
      PROCEDURE LoadFromFile(FileName	: STRING;
		                     PBaseAddr	: DWORD);
      FUNCTION SetCPUFromName(CPUName     : STRING) : BOOLEAN;

    END;

implementation

CONSTRUCTOR TMetaDisassembler.Create;

BEGIN;
  INHERITED Create;
  Parameters:=TParameterList.Create;
  Memory:=TCPUmemory.Create;
  SymbolList:=TSymbolList.Create('SymbolList',Parameters);
  EntryPoints:=TSymbolList.Create('EntryPoints',Parameters);
  MemoryList:=TMemoryList.Create(Memory,SymbolList,EntryPoints,Parameters);
  FSelected:=Invalid;
  FRadix:=DefRadix;
END;

DESTRUCTOR TMetaDisassembler.Destroy;

BEGIN;
  MemoryList.Free;
  EntryPoints.Free;
  SymbolList.Free;
  Memory.Free;
  Parameters.Free;
  INHERITED Destroy;
END;

PROCEDURE TMetaDisassembler.Add(ADisassembler   : TADisassembler);

BEGIN;
  WITH ADisassembler DO
  BEGIN;
    Memory:=Self.Memory;
    SymbolList:=Self.SymbolList;
    EntryPoints:=Self.EntryPoints;
    MemoryList:=Self.MemoryList;
    Parameters:=Self.Parameters;
  END;
  INHERITED Add(ADisassembler);
END;

{PROCEDURE TMetaDisassembler.SetVerbose(NewValue	: BOOLEAN);

VAR Idx : INTEGER;

BEGIN;
  FVerbose:=NewValue;
  EntryPoints.Verbose:=NewValue;
  SymbolList.Verbose:=NewValue;

  FOR Idx:=0 TO (Count-1) DO
    TADisassembler(Items[Idx]).Verbose:=NewValue;
END; }
PROCEDURE TMetaDisassembler.SetVerbosity(NewValue	: BYTE);

VAR Idx : INTEGER;

BEGIN;
  FVerbosity:=NewValue;
  EntryPoints.Verbosity:=NewValue;
  SymbolList.Verbosity:=NewValue;

  FOR Idx:=0 TO (Count-1) DO
    TADisassembler(Items[Idx]).Verbosity:=NewValue;
END;

FUNCTION TMetaDisassembler.CPUNameToType(CPUName     : STRING) : TCPU;

BEGIN;
  CPUName:=LowerCase(Trim(CPUName));

  IF (CPUName='6502') THEN
    Result:=tc6502
  ELSE IF (CPUName='65c02') THEN
    Result:=tc65c02
  ELSE IF (CPUName='wd65c02') THEN
    Result:=tc65c02wd
  ELSE IF (CPUName='6512') THEN
    Result:=tc6512
  ELSE IF (CPUName='6809') THEN
    Result:=tc6809
  ELSE
    Result:=tcInvalid;
END;

FUNCTION TMetaDisassembler.SetCPUFromName(CPUName     : STRING) : BOOLEAN;

VAR CPUType : TCPU;
    Idx     : INTEGER;
    Valid   : BOOLEAN;

BEGIN;
  CPUType:=CPUNameToType(CPUName);

  Valid:=FALSE;
  Idx:=-1;
  WHILE ((Idx < (Count-1)) AND NOT Valid) DO
  BEGIN;
    Idx:=Idx+1;
    Valid:=TADisassembler(Items[Idx]).ValidCPU(CPUType);
  END;

  IF (Valid) THEN
  BEGIN;
    FSelected:=Idx;
    TADisassembler(Items[FSelected]).CPU:=CPUType;
    Memory.SwapWords:=TADisassembler(Items[FSelected]).SwapWords;
    FCPU:=CPUType;
  END;

  Result:=Valid;
END;

PROCEDURE TMetaDisassembler.SetRadix(ARadix : BYTE);

VAR Idx : INTEGER;

BEGIN;
  FRadix:=ARadix;

  FOR Idx:=0 TO (Count-1) DO
    TADisassembler(Items[Idx]).Radix:=FRadix;
END;

PROCEDURE TMetaDisassembler.Go;

BEGIN;
  IF (FSelected<>Invalid) THEN
    TADisassembler(Self.Items[FSelected]).Go;
END;

PROCEDURE TMetaDisassembler.LoadFromFile(FileName	: STRING;
                                         PBaseAddr	: DWORD);

BEGIN;
  Memory.LoadFromFile(FileName,PBaseAddr);
  EntryPoints.SetRange(Memory.EndAddr,Memory.BaseAddr);
  WriteLnFmtV(FVerbosity,VBNormal,'Loaded %s at $%4.4X - $%4.4X',[FileName,Memory.BaseAddr,Memory.EndAddr]);
END;

END.

