program BeebDis;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$APPTYPE CONSOLE}

{ 6502 Disassembler meant as a complement to BeebAsm                          }
{ 2013-03-26 Phill Harvey-Smith                                               }
{                                                                             }
{ 2017-05-20 Fixed a bug where splitting a range of data addresses could lead }
{       to data not turning up in output file. Address ranges will now only   }
{       be split if the split point is a multiple of the data size for non    }
{       byte length ranges.                                                   }
{                                                                             }
{ 2018-02-23 merged in and modified fpexprpars from the main freepascal       }
{       library. This is so that we can support expressions in the label file }
{       such as those used in beebasm e.g. VAR2 = VAR1 + $100; etc            }
{                                                                             }
{ 2018-04-10 Added options to scan for Acorn JSR inline strings. Also added   }
{       ability to have repeated blocks of data in defined in the control     }
{       file for defining data tables to be disassembled.                     }
{       Added ability to set CPU type and disasseble 65c02/wd65c02 code.      }
{                                                                             }
{ 2018-04-17 Broke up dissassembler into CPU specific and generic types,      }
{       where CPU specific type is a descendent of the abstract generic type. }
{       Implemented a container for multiple disassembler types so that the   }
{       CPU family to disassemble may be selected at runtime.                 }
{       Implemented Motorola 6809 (and compatible) disassembly.               }
{                                                                             }
{ 2019-01-23 Fixed a bug where symbol files not being read.                   }
{       Added verbose keyword to control file.                                }

uses
  SysUtils,
  Classes,
  MemoryListUnit in 'MemoryListUnit.pas',
  SymbolListUnit in 'SymbolListUnit.pas',
  ConsoleUnit in 'ConsoleUnit.pas',
  CPUMemoryUnit in 'CPUMemoryUnit.pas',
  RamothStringListUnit in 'RamothStringListUnit.pas',
  AbstractDisassemblerUnit in 'AbstractDisassemblerUnit.pas',
  Disassembler6502Unit in 'Disassembler6502Unit.pas',
  Disassembler6809Unit in 'Disassembler6809Unit.pas',
  DisassemblersUnit in 'DisassemblersUnit.pas',
  UtilsUnit in 'UtilsUnit.pas',
  BeebDisDefsUnit in 'BeebDisDefsUnit.pas',
  ParameterListUnit in 'ParameterListUnit.pas';

VAR Disassember	    : TMetaDisassembler;
	ControlFile	    : TStringList;
	CFileName	    : STRING;
    CFilePath       : STRING;
    Abort		    : BOOLEAN;
	OutputBuffer	: TStringList;
	Options		    : TRamothStringList;
    InlineAddr      : DWORD;


PROCEDURE Initialize;

BEGIN;
  Disassember:=TMetaDisassembler.Create;
  Disassember.Add(TDisassembler6502.Create);
  Disassember.Add(TDisassembler6809.Create);

  ControlFile:=TStringList.Create;
  OutputBuffer:=TStringList.Create;
  CFileName:=ParamStr(1);
  CFilePath:=ExtractFilePath(CFileName);
  Abort:=FALSE;
  Disassember.Verbosity:=VBNormal;
  Options:=TRamothStringList.Create;
  InlineAddr:=0;;
END;

PROCEDURE SignOn;

BEGIN;
  WriteLnFmtV(Disassember.Verbosity,VBNormal,'BeebDis V%d.%2.2d 2019-01, Phill Harvey-Smith.',[Major,Minor]);
END;

PROCEDURE Finalize;

BEGIN;
  //WriteLn(Disassember.Parameters.AsString);
  Disassember.Free;
  ControlFile.Free;
  OutputBuffer.Free;
  Options.Free;
END;

{ So that BeebDis can be run from it's build directory but keep the control}
{ files in a seperate path, we search first in the current dir, and then in}
{ the same directory as the control file on the command line }

FUNCTION GetFileName(InFileName    : STRING) : STRING;

BEGIN
  Result:='';
  InFileName:=Trim(InFileName);
  IF (FileExists(InFileName)) THEN
    Result:=InFileName
  ELSE
    IF (FileExists(CFilePath+InFileName)) THEN
      Result:=CFilePath+InFileName;
END;

{ Control file syntax

Parameters in [] are optional.
All optional counts default to 1

LOAD addr,filename	    ; Load filename at CPUaddress addr
SAVE filename		    ; set output file name rather than stdout
SYMBOLS filename	    ; load symbols from filename
NEWSYM filename		    ; save newly generated symbols to filename
CPU num                 ; Set CPU type :
                          6502      : Standard MOS 6502
                          65c02     : CMOS 65c02
                          wd65c02   : WDC / Rockwell 65c02
                          6512      : 6512
                          6809      : Motorola 6809 / 6809E

BYTE addr [count]	    ; Byte data for count bytes at address addr
WORD addr [count]      	; Word data for count bytes at address addr
DWORD addr [count]      ; DWord data for count bytes at address addr
STRING addr count	    ; String data for count bytes at address addr
STRINGZ addr		    ; String data until a $00 byte is reached
STRINGTERM addr term	; String data until specified terminator is reached
STRINGHI addr		    ; String data until a byte with high bit set is reached
STRINGHIZ addr          ; String data until either a zero byte or a byte with
                        ; high bit set.
ENTRY addr        	    ; Code entry point
WORDENTRY addr count	; Use words at addr as entrypoints.
WORDRTS addr count      ; Like wordentry but word pushed onto stack and jumped
                        ; to by rts, point to lable-1.
HEXDUMP			        ; Add a hexdump to the end of the output
STRINGSCAN		        ; Scan for strings output them in the listing
INLINESCAN addr         ; Scan for Acorn type inline print strinsg that are
                        ; a JSR addr followed immediately by the string data.
                        ; if found a STRINGHIZ will be added for the address
                        ; immediately after the JSR & addr.
ACTIVE 1 | 0            ; Turn on and off interpretation of keywords, can be
                        ; used to comment a block of directives in a control
                        ; file without having to comment each individually
NEWPC addr              ; Set a new PC for subsequent operations.
REPEAT count            ; Repeat the following lines (up until endrepeat) the
                        ; specified number of times, this is usefull for
                        ; extracting data structures like strings etc.
                        ; NOTE repeats cannot (currently) be nested.
ENDREPEAT               ; Terminate a previous repeat.
OPTION name value       ; Set parameter name = value both string, boolean values
                        ; may be set with the values 'true' or 'false' or '1'
                        ; or '0'
RADIX 2 | 8 | 10 | 16   ; Set the default radix for number output.
VERBOSE level           ; Set verbosity level :
                        ;       0=quiet, (no output unless error)
                        ;       1=normal, signon + file loads / saves
                        ;       2=verbose, additional information
                        ;       3+=debug, internal debug info.
ToDo :
}

PROCEDURE ReadControlFile(InFilename	: STRING);

VAR	LineNo	    : INTEGER;
	Split	    : TRamothStringList;
	Keyword	    : STRING;
    Active      : BOOLEAN = TRUE;

    NewControl  : TStringList;
    RepeatBuf   : TStringList;
    RepeatNo    : INTEGER;
    RepeatCount : INTEGER = 0;
    InRepeat    : BOOLEAN = FALSE;
    ControlStr  : STRING;

BEGIN;
  NewControl:=TStringList.Create;
  RepeatBuf:=TStringList.Create;
  Split:=TRamothStringList.Create;

  TRY;
    Split.Delimiter:=' ';
    Split.QuoteChar:='"';
    Split.GetBlank:=TRUE;

    ControlFile.LoadFromFile(InFilename);

    { First parse the control file and expand any repeat directives, also set }
    { Verbosity level }

    FOR LineNo:=0 TO (ControlFile.Count-1) DO
    BEGIN;
      Split.Split(Trim(ControlFile.Strings[LineNo]));
      Split.PurgeBlank;
      Keyword:=LowerCase(Split[0]);

      IF (Keyword=KWVerbose) THEN
        Disassember.Verbosity:=StrToIntDef(Split[1],DefVerbosity);

      IF (Keyword=KWActive) THEN
          TryStrToBool(Split[1],Active);

      IF (Active) THEN
      BEGIN;

        IF ((Keyword=KWRepeat) OR (Keyword=KWEndRepeat)) THEN
        BEGIN;
          IF ((NOT InRepeat) AND (Keyword='repeat')) THEN
          BEGIN
            RepeatCount:=StrToIntDef(Split[1],1);
            InRepeat:=TRUE;
          END;

          IF ((InRepeat) AND (Keyword='endrepeat')) THEN
          BEGIN;
            InRepeat:=FALSE;
            FOR RepeatNo:=1 TO RepeatCount DO
              NewControl.AddStrings(RepeatBuf);

            RepeatBuf.Clear;
          END;
        END
        ELSE
        BEGIN
          IF (InRepeat) THEN
            RepeatBuf.Add(ControlFile.Strings[LineNo])
          ELSE
            NewControl.Add(ControlFile.Strings[LineNo]);
        END;
      END;
    END;

    IF (InRepeat) THEN
      raise Exception.CreateFmt('Error: reached end of control file looking for %s',[KWEndRepeat]);

    ControlFile.Clear;
    ControlFile.AddStrings(NewControl);

    SignOn;

//    Writeln(ControlFile.Text);

    LineNo:=0;
    WHILE ((NOT Abort) AND (LineNo<ControlFile.Count)) DO
    BEGIN;
      ControlStr:=Trim(StringReplace(ControlFile.Strings[LineNo],#9,' ',[rfReplaceAll,rfIgnoreCase]));
      Split.Split(ControlStr);
      Split.PurgeBlank;
      Keyword:=LowerCase(Split[0]);

      WITH Disassember DO
      BEGIN;
        IF (Keyword=KWActive) THEN
          TryStrToBool(Split[1],Active);

        IF ((Active) AND (Length(Keyword)>0)) THEN
        BEGIN;
          IF (Keyword=KWLoad) THEN
            LoadFromFile(GetFileName(Split[2]),StrToIntDef(Split[1],0));

          IF (Keyword=KWSymbols) THEN
            SymbolList.AddFile(GetFileName(Split[1]));

          IF (Keyword=KWSave) THEN
            MemoryList.OutputFilename:=CFilePath+Trim(Split[1]);

          IF (Keyword=KWCPU) THEN
            SetCPUFromName(Split[1]);

          IF (Keyword=KWByte) THEN
            MemoryList.AddData(tyDataByte,Split[1],StrToIntDef(Split[2],1));

          IF (Keyword=KWWord) THEN
            MemoryList.AddData(tyDataWord,Split[1],StrToIntDef(Split[2],1));

          IF (Keyword=KWDWord) THEN
            MemoryList.AddData(tyDataDWord,Split[1],StrToIntDef(Split[2],1));

          IF (Keyword=KWString) THEN
            MemoryList.AddData(tyDataString,Split[1],StrToIntDef(Split[2],0));

          IF (Keyword=KWStringz) THEN
            MemoryList.AddData(tyDataStringTerm,Split[1],0,0);

          IF (Keyword=KWStringTerm) THEN
            MemoryList.AddData(tyDataStringTerm,Split[1],0,StrToIntDef(Split[2],0));

          IF (Keyword=KWStringHi) THEN
            MemoryList.AddData(tyDataStringTermHi,Split[1],0,StrToIntDef(Split[2],0));

          IF (Keyword=KWStringHiZ) THEN
            MemoryList.AddData(tyDataStringTermHiZ,Split[1],0,0);

          IF (Keyword=KWEntry) THEN
              MemoryList.AddEntry(Split[1],Split[2]);

          IF (Keyword=KWWordEntry) THEN
            MemoryList.AddData(tyDataWordEntry,Split[1],StrToIntDef(Split[2],1));

          IF (Keyword=KWWordRTS) THEN
            MemoryList.AddData(tyDataWordRTSEntry,Split[1],StrToIntDef(Split[2],1));

	      IF (Keyword=KWHexDump) THEN
          BEGIN;
            Options.BoolValues[OptHexDump]:=TRUE;
            Options.Values[OptHexDumpFile]:=Split[1];
          END;

	      IF (Keyword=KWStringScan) THEN
            Options.BoolValues[OptStringScan]:=TRUE;

	      IF (Keyword=KWInlineScan) THEN
          BEGIN;
            InlineAddr:=StrToIntDef(Split[1],0);
            Options.BoolValues[OptInlineScan]:=TRUE;
          END;

          IF (Keyword=KWNewSym) THEN
          BEGIN;
            Options.BoolValues[OptNewSym]:=TRUE;
            Options.Values[OptNewSymFile]:=Split[1];
          END;

          IF (Keyword=KWNewPC) THEN
            MemoryList.PC:=StrToIntDef(Split[1],MemoryList.PC);

          IF (Keyword=KWOption) THEN
              Disassember.Parameters[Split[1]]:=Split[2];

          IF (Keyword=KWRadix) THEN
            Disassember.Radix:=StrToIntDef(Split[1],DefRadix);

        END;
      END;

      LineNo:=LineNo+1;
    END;
  FINALLY
    NewControl.Free;
    RepeatBuf.Free;
    Split.Free;
    IF (NOT Disassember.Memory.Loaded) THEN
      raise Exception.Create('Error: nothing loaded, nothing to do');
  END;
  IF (Options.BoolValues[OptStringScan]) THEN
    Disassember.Memory.FindStrings;

  IF (Options.BoolValues[OptInlineScan]) THEN
    Disassember.Memory.FindInlineStrings(InlineAddr);

END;

begin
  TRY
    Initialize;
    TRY
      IF (FileExists(CFileName)) THEN
        ReadControlFile(CFileName)
      ELSE
        raise Exception.Create('Error: no control file');

      Disassember.Go;
      OutputBuffer.Add(Disassember.MemoryList.Listing.Text);
      with Disassember.Memory do
        HexDumpArea(BaseAddr,EndAddr,True);

      IF (Options.BoolValues[OpthexDump]) THEN
        IF (Options.Values[OptHexDumpFile]<>'') THEN
          Disassember.Memory.HexDump.SaveToFile(Options.Values[OptHexDumpFile])
        ELSE
          OutputBuffer.Add(Disassember.Memory.HexDump.Text);

      IF (Options.BoolValues[OptStringScan]) THEN
      BEGIN
        OutputBuffer.Add(Disassember.Parameters[mlBeginIgnore]);
        OutputBuffer.Add(Disassember.Memory.FoundStrings.Text);
        OutputBuffer.Add(Disassember.Parameters[mlEndIgnore]);
      END;

      IF (Options.BoolValues[OptNewSym]) THEN
        IF (Options.Values[OptNewSymFile]<>'') THEN
        BEGIN;
          WriteLnFmtV(Disassember.Verbosity,VBNormal,'writing new symbol file : %s',[Options.Values[OptNewSymFile]]);
          Disassember.SymbolList.SaveSymbolsToFile(Options.Values[OptNewSymFile],stGenerated,
                                                   Disassember.Parameters[mlEquate]);
        END;

      IF (Disassember.MemoryList.OutputFilename<>'') THEN
      BEGIN;
        WriteLnFmtV(Disassember.Verbosity,VBNormal,'Writing output to : %s',[Disassember.MemoryList.OutputFilename]);
        OutputBuffer.SaveToFile(Disassember.MemoryList.OutputFilename);
      END
      ELSE
        WriteLn(OutputBuffer.Text);
    FINALLY
      Finalize;
    END;
  EXCEPT
    ON E: Exception DO WriteLn(E.Message);
  END;
end.
