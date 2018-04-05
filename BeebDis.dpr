program BeebDis;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$APPTYPE CONSOLE}

{ 6502 Disassembler meant as a complement to BeebAsm }
{ 2013-03-26 Phill Harvey-Smith }
{ 2017-05-20 Fixed a bug where splitting a range of data addresses could lead }
{       to data not turning up in output file. Address ranges will now only   }
{       be split if the split point is a multiple of the data size for non    }
{       byte length ranges.                                                   }
{ 2018-02-23 merged in and modified fpexprpars from the main freepascal       }
{       library. This is so that we can support expressions in the label file }
{       such as those used in beebasm e.g. VAR2 = VAR1 + $100; etc            }

uses
  SysUtils,
  Classes,
  MemoryListUnit in 'MemoryListUnit.pas',
  SymbolListUnit in 'SymbolListUnit.pas',
  ConsoleUnit in 'ConsoleUnit.pas',
  CPUMemoryUnit in 'CPUMemoryUnit.pas',
  RamothStringListUnit in 'RamothStringListUnit.pas',
  DisassemblerUnit in 'DisassemblerUnit.pas',
  UtilsUnit in 'UtilsUnit.pas',
//  EvalUnit in 'EvalUnit.pas',
  BeebDisDefsUnit in 'BeebDisDefsUnit.pas';

VAR Disassember	    : TDisassemblerUnit;
	ControlFile	    : TStringList;
	CFileName	    : STRING;
	Abort		    : BOOLEAN;
	OutputBuffer	: TStringList;
	Options		    : TRamothStringList;

PROCEDURE Initialize;

BEGIN;
  Disassember:=TDisassemblerUnit.Create;
  ControlFile:=TStringList.Create;
  OutputBuffer:=TStringList.Create;
  CFileName:=ParamStr(1);
  Abort:=FALSE;
  Disassember.Verbose:=TRUE;
  Options:=TRamothStringList.Create;
END;

PROCEDURE SignOn;

BEGIN;
  WriteLn(Format('BeebDis V%d.%2.2d 2018-02, Phill Harvey-Smith.',[Major,Minor]));
END;

PROCEDURE Finalize;

BEGIN;
  Disassember.Free;
  ControlFile.Free;
  OutputBuffer.Free;
  Options.Free;
END;

{ Control file syntax

Parameters in [] are optional.
All optional counts default to 1

LOAD addr,filename	    ; Load filename at CPUaddress addr
SAVE filename		    ; set output file name rather than stdout
SYMBOLS filename	    ; load symbols from filename
NEWSYM filename		    ; save newly generated symbols to filename

BYTE addr [count]	    ; Byte data for count bytes at address addr
WORD addr [count]      	; Word data for count bytes at address addr
DWORD addr [count]      ; DWord data for count bytes at address addr
STRING addr count	    ; String data for count bytes at address addr
STRINGZ addr		    ; String data until a $00 byte is reached
STRINGTERM addr term	; String data until specified terminator is reached
STRINGHI addr		    ; String data until a byte with high bit set is reached
ENTRY addr        	    ; Code entry point
WORDENTRY addr count	; Use words at addr as entrypoints.
WORDRTS addr count      ; Like wordentry but word pussed onto stack and jumped
                        ; to by rts, point to lable-1.
HEXDUMP			        ; Add a hexdump to the end of the output
STRINGSCAN		        ; Scan for strings output them in the listing
ACTIVE 1 | 0            ; Turn on and off interpretation of keywords, can be
                        ; used to comment a block of directives in a control
                        ; file without having to comment each individually
ToDo :
}

PROCEDURE ReadControlFile(InFilename	: STRING);

VAR	LineNo	: INTEGER;
	Split	: TRamothStringList;
	Keyword	: STRING;
        Active  : BOOLEAN = TRUE;

BEGIN;
  Split:=TRamothStringList.Create;

  TRY;
    Split.Delimiter:=' ';
    Split.QuoteChar:='"';
    Split.GetBlank:=TRUE;

    ControlFile.LoadFromFile(InFilename);
    LineNo:=0;
    WHILE ((NOT Abort) AND (LineNo<ControlFile.Count)) DO
    BEGIN;
      Split.Split(Trim(ControlFile.Strings[LineNo]));
      Keyword:=LowerCase(Split[0]);

      WITH Disassember DO
      BEGIN;
        IF (Keyword='active') THEN
          TryStrToBool(Split[1],Active);

        IF ((Active) AND (Length(Keyword)>0)) THEN
        BEGIN;
          IF (Keyword='load') THEN
            LoadFromFile(Trim(Split[2]),StrToIntDef(Split[1],0));

          IF (Keyword='symbols') THEN
            SymbolList.LoadLabels(Split[1]);

          IF (Keyword='save') THEN
            MemoryList.OutputFilename:=Split[1];

          IF (Keyword='byte') THEN
            MemoryList.AddData(tyDataByte,Split[1],StrToIntDef(Split[2],1));

          IF (Keyword='word') THEN
            MemoryList.AddData(tyDataWord,Split[1],StrToIntDef(Split[2],1));

          IF (Keyword='dword') THEN
            MemoryList.AddData(tyDataDWord,Split[1],StrToIntDef(Split[2],1));

          IF (Keyword='string') THEN
            MemoryList.AddData(tyDataString,Split[1],StrToIntDef(Split[2],0));

          IF (Keyword='stringz') THEN
            MemoryList.AddData(tyDataStringTerm,Split[1],0,0);

          IF (Keyword='stringterm') THEN
            MemoryList.AddData(tyDataStringTerm,Split[1],0,StrToIntDef(Split[2],0));

          IF (Keyword='stringhi') THEN
            MemoryList.AddData(tyDataStringTermHi,Split[1],0,StrToIntDef(Split[2],0));

          IF (Keyword='entry') THEN
            MemoryList.AddEntry(Split[1]);

          IF (Keyword='wordentry') THEN
            MemoryList.AddData(tyDataWordEntry,Split[1],StrToIntDef(Split[2],1));

          IF (Keyword='wordrts') THEN
            MemoryList.AddData(tyDataWordRTSEntry,Split[1],StrToIntDef(Split[2],1));

	      IF (Keyword='hexdump') THEN
          BEGIN;
            Options.BoolValues[OptHexDump]:=TRUE;
            Options.Values[OptHexDumpFile]:=Split[1];
          END;

	      IF (Keyword='stringscan') THEN
            Options.BoolValues[OptStringScan]:=TRUE;

          IF (Keyword='newsym') THEN
          BEGIN;
            Options.BoolValues[OptNewSym]:=TRUE;
            Options.Values[OptNewSymFile]:=Split[1];
          END;
        END;
      END;

      LineNo:=LineNo+1;
    END;
  FINALLY
    Split.Free;
    IF (NOT Disassember.Memory.Loaded) THEN
      raise Exception.Create('Error: nothing loaded, nothing to do');
  END;
  IF (Options.BoolValues[OptStringScan]) THEN
    Disassember.Memory.FindStrings;
END;

begin
  TRY
    Initialize;
    SignOn;
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
        OutputBuffer.Add(Disassember.Memory.FoundStrings.Text);

      IF (Options.BoolValues[OptNewSym]) THEN
        IF (Options.Values[OptNewSymFile]<>'') THEN
	  Disassember.SymbolList.SaveSymbolsToFile(Options.Values[OptNewSymFile],stGenerated);

      IF (Disassember.MemoryList.OutputFilename<>'') THEN
        OutputBuffer.SaveToFile(Disassember.MemoryList.OutputFilename)
      ELSE
        WriteLn(OutputBuffer.Text);
    FINALLY
      Finalize;
    END;
  EXCEPT
    ON E: Exception DO WriteLn(E.Message);
  END;
end.
