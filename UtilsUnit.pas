unit UtilsUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

FUNCTION IsASCII(ToTest	: CHAR) : BOOLEAN;
FUNCTION PadTo(VAR ToPad	: STRING;
	           NewLen	    : INTEGER) : STRING;

FUNCTION PadToAdd(VAR ToPad	: STRING;
	              NewLen	: INTEGER;
		          ToAdd	    : STRING) : STRING;

FUNCTION SwapWord(AWord : WORD) : WORD;

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

FUNCTION SwapWord(AWord : WORD) : WORD;

VAR MSB : BYTE;
    LSB : BYTE;

BEGIN;
  MSB := (AWord AND $FF00) SHR 8;
  LSB := (AWord AND $00FF);
  Result:=(LSB SHL 8)+MSB;
END;

end.
