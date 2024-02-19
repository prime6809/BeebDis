unit UtilsUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

USES Classes,SysUtils;

FUNCTION IsASCII(ToTest	: CHAR) : BOOLEAN;
FUNCTION PadTo(VAR ToPad	: STRING;
	           NewLen	    : INTEGER) : STRING;

FUNCTION PadToAdd(VAR ToPad	: STRING;
	              NewLen	: INTEGER;
		          ToAdd	    : STRING) : STRING;

FUNCTION PadToAddFmt(VAR ToPad	: STRING;
	                 NewLen	    : INTEGER;
		             FormatStr  : STRING;
                     ToAdd      : ARRAY OF CONST) : STRING;

FUNCTION FormatLine(Params  : ARRAY OF STRING;
                    Cols    : ARRAY OF INTEGER) : STRING;

FUNCTION SwapWord(AWord : WORD) : WORD;
FUNCTION MinWidth(ARadix    : BYTE;
                  NoBytes   : BYTE) : BYTE;

implementation

FUNCTION IsASCII(ToTest	: CHAR) : BOOLEAN;

BEGIN;
  Result:=(ToTest IN [#$20..#$7F]);
END;

FUNCTION PadTo(VAR ToPad	: STRING;
	           NewLen	    : INTEGER) : STRING;

BEGIN;
  WHILE (Length(ToPad)<NewLen) DO
    ToPad:=ToPad+' ';

  Result:=ToPad;
END;

FUNCTION PadToAdd(VAR ToPad	: STRING;
	              NewLen	: INTEGER;
		          ToAdd	    : STRING) : STRING;

BEGIN;
  PadTo(ToPad,NewLen);
  ToPad:=ToPad+ToAdd;

  Result:=ToPad;
END;

FUNCTION PadToAddFmt(VAR ToPad	: STRING;
	                 NewLen	    : INTEGER;
		             FormatStr  : STRING;
                     ToAdd      : ARRAY OF CONST) : STRING;

BEGIN;
  Result:=PadToAdd(ToPad,NewLen,Format(FormatStr,ToAdd));
END;

FUNCTION SwapWord(AWord : WORD) : WORD;

VAR MSB : BYTE;
    LSB : BYTE;

BEGIN;
  MSB := (AWord AND $FF00) SHR 8;
  LSB := (AWord AND $00FF);
  Result:=(LSB SHL 8)+MSB;
END;

FUNCTION FormatLine(Params  : ARRAY OF STRING;
                    Cols    : ARRAY OF INTEGER) : STRING;

VAR Idx : INTEGER;

BEGIN;
  IF (Length(Params) <> Length(Cols)) THEN
    Raise Exception.CreateFmt('Error in Format line length of Params (%d) <> length of Cols (%d)',[Length(Params),Length(Cols)]);

  Result:='';
  FOR Idx:=0 TO (Length(Params)-1) DO
  BEGIN;
    PadToAdd(Result,Cols[Idx],Params[Idx]);
  END;
END;

FUNCTION MinWidth(ARadix    : BYTE;
                  NoBytes   : BYTE) : BYTE;

BEGIN;
  IF (ARadix=2) THEN
  BEGIN;
    CASE NoBytes OF
      1 : Result:=8;
      2 : Result:=16;
    ELSE
      Result:=32;
    END;
  END
  ELSE IF (ARadix=8) THEN
  BEGIN;
    CASE NoBytes OF
      1 : Result:=3;
      2 : Result:=6;
    ELSE
      Result:=11;
    END;
  END
  ELSE IF (ARadix=10) THEN
  BEGIN;
    CASE NoBytes OF
      1 : Result:=3;
      2 : Result:=5;
    ELSE
      Result:=10;
    END;
  END
  ELSE IF (ARadix=16) THEN
  BEGIN;
    CASE NoBytes OF
      1 : Result:=2;
      2 : Result:=4;
    ELSE
      Result:=8;
    END;
  END;
END;

end.
