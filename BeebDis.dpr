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
  BeebDisDefsUnit in 'BeebDisDefsUnit.pas';




VAR Disassember	    : TDisassemblerUnit;
	ControlFile	    : TStringList;
	CFileName	    : STRING;
    CFilePath       : STRING;
    Abort		    : BOOLEAN;
	OutputBuffer	: TStringList;
	Options		    : TRamothStringList;


PROCEDURE Initialize;

BEGIN;
  Disassember:=TDisassemblerUnit.Create;
  ControlFile:=TStringList.Create;
  OutputBuffer:=TStringList.Create;
  CFileName:=ParamStr(1);
  CFilePath:=ExtractFilePath(CFileName);
  Abort:=FALSE;
  Disassember.Verbose:=TRUE;
  Options:=TRamothStringList.Create;
END;

PROCEDURE SignOn;

BEGIN;
  WriteLn(Format('BeebDis V%d.%2.2d 2018-04, Phill Harvey-Smith.',[Major,Minor]));
END;

PROCEDURE Finalize;

BEGIN;
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
NEWPC addr              ; Set a new PC for subsequent operations.
REPEAT count            ; Repeat the following lines (up until endrepeat) the
                        ; specified number of times, this is usefull for
                        ; extracting data structures like strings etc.
                        ; NOTE repeats cannot (currently) be nested.
ENDREPEAT               ; Terminate a previous repeat.
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

BEGIN;
  NewControl:=TStringList.Create;
  RepeatBuf:=TStringList.Create;
  Split:=TRamothStringList.Create;

  TRY;
    Split.Delimiter:=' ';
    Split.QuoteChar:='"';
    Split.GetBlank:=TRUE;

    ControlFile.LoadFromFile(InFilename);

    { First parse the control file and expand any repeat directives }

    FOR LineNo:=0 TO (ControlFile.Count-1) DO
    BEGIN;
      Split.Split(Trim(ControlFile.Strings[LineNo]));
      Split.PurgeBlank;
      Keyword:=LowerCase(Split[0]);

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

    NewControl.SaveToFile('c:\tmp\newcontrol.txt');
    ControlFile.Clear;
    ControlFile.AddStrings(NewControl);

    LineNo:=0;
    WHILE ((NOT Abort) AND (LineNo<ControlFile.Count)) DO
    BEGIN;
      Split.Split(Trim(ControlFile.Strings[LineNo]));
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
            SymbolList.LoadLabels(GetFileName(Split[1]));

          IF (Keyword=KWSave) THEN
            MemoryList.OutputFilename:=CFilePath+Trim(Split[1]);

          IF (Keyword=KWByte) THEN
            MemoryList.AddData(tyDataByte,Split[1],StrToIntDef(Split[2],1));

          IF (Keyword=KWWord) THEN
            MemoryList.AddData(tyDataWord,Split[1],StrToIntDef(Split[2],1));

          IF (Keyword=KWDWord) THEN
            MemoryList.AddData(tyDataDWord,Split[1],StrToIntDef(Split[2],1));

          IF (Keyword=KWString) THEN
          begin
            WriteLnFmt('Split[1]:%s Split[2]:%s',[Split[1],Split[2]]);
            MemoryList.AddData(tyDataString,Split[1],StrToIntDef(Split[2],0));
          END;

          IF (Keyword=KWStringz) THEN
            MemoryList.AddData(tyDataStringTerm,Split[1],0,0);

          IF (Keyword=KWStringTerm) THEN
            MemoryList.AddData(tyDataStringTerm,Split[1],0,StrToIntDef(Split[2],0));

          IF (Keyword=KWStringHi) THEN
            MemoryList.AddData(tyDataStringTermHi,Split[1],0,StrToIntDef(Split[2],0));

          IF (Keyword=KWEntry) THEN
            MemoryList.AddEntry(Split[1]);

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

          IF (Keyword=KWNewSym) THEN
          BEGIN;
            Options.BoolValues[OptNewSym]:=TRUE;
            Options.Values[OptNewSymFile]:=Split[1];
          END;

          IF (Keyword=KWNewPC) THEN
            MemoryList.PC:=StrToIntDef(Split[1],MemoryList.PC);
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
