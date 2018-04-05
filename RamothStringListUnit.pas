unit RamothStringListUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

USES Classes,SysUtils;

TYPE    PStrings	= ^TStrings;

	TRamothStringList = class(TStringList)
	PRIVATE
          FStrIndex	: INTEGER;
	PROTECTED
	  FGetBlank	: BOOLEAN;
          FUNCTION Get(Index: Integer): string; override;
	  FUNCTION GetBoolValue(AName	: STRING) : BOOLEAN;
	  PROCEDURE SetBoolValue(AName	: STRING;
				 AValue	: BOOLEAN);
        PUBLIC
          PROPERTY StrIndex	: INTEGER READ FStrIndex WRITE FStrIndex;
	  PROPERTY GetBlank	: BOOLEAN READ FGetBlank WRITE FGetBlank;
	  PROPERTY BoolValues[AName	: STRING] : BOOLEAN READ GetBoolValue WRITE SetBoolValue;

	  FUNCTION AddFormat(FormatStr	: STRING;
          		     Params	: ARRAY OF CONST) : INTEGER;

          PROCEDURE First;
          FUNCTION Next : STRING;
          FUNCTION Eof : BOOLEAN;
          PROCEDURE CopyNToList(Dest		: PStrings;
          			NoToCopy	: INTEGER);
          PROCEDURE Skip(NoToSkip	: INTEGER);
          PROCEDURE AddMultiple(Params	: ARRAY OF STRING);
          PROCEDURE ClearAddMultiple(Params	: ARRAY OF STRING);
          PROCEDURE Split(ToSplit       : STRING;
                          ClearFirst    : BOOLEAN = TRUE);
          PROCEDURE ClearAndSplit(ToSplit       : STRING);
	END;

implementation

FUNCTION TRamothStringList.Get(Index: Integer): string;

BEGIN;
  IF ((GetBlank) AND (Index>=Count)) THEN
      Result:=''
  ELSE
    Result:=INHERITED Get(Index);
END;

FUNCTION TRamothStringList.AddFormat(FormatStr	: STRING;
          		             Params	: ARRAY OF CONST) : INTEGER;

BEGIN;
  Result:=Add(Format(FormatStr,Params));
END;

PROCEDURE TRamothStringList.First;

BEGIN;
  FStrIndex:=0;
END;

FUNCTION TRamothStringList.Next : STRING;

BEGIN;
  Result:='';
  IF (FStrIndex<Count) THEN
  BEGIN;
    Result:=Strings[FStrIndex];
    FStrIndex:=FStrIndex+1;
  END;
END;

FUNCTION TRamothStringList.Eof : BOOLEAN;

BEGIN;
  IF (FStrIndex>=Count) THEN
    Result:=TRUE
  ELSE
    Result:=FALSE;
END;

PROCEDURE TRamothStringList.CopyNToList(Dest		: PStrings;
  			                NoToCopy	: INTEGER);

VAR	Idx	: INTEGER;

BEGIN;
  FOR Idx:=0 TO (NoToCopy-1) DO
    Dest^.Add(Next);
END;

PROCEDURE TRamothStringList.Skip(NoToSkip	: INTEGER);

BEGIN;
  FStrIndex:=FStrIndex+NoToSkip;
END;

PROCEDURE TRamothStringList.AddMultiple(Params	: ARRAY OF STRING);

VAR	StringNo	: INTEGER;

BEGIN;
  FOR StringNo:=0 TO High(Params) DO
    Add(Params[StringNo]);
END;

PROCEDURE TRamothStringList.ClearAddMultiple(Params	: ARRAY OF STRING);

BEGIN;
  Clear;
  AddMultiple(Params);
END;

{ Split a string into components using *ONLY* the specified delimiter    }
{ note this is not the same as setting DelimitedText, as doing so always }
{ also uses space, which is not what is required in most cases !         }

PROCEDURE TRamothStringList.Split(ToSplit       : STRING;
                                  ClearFirst    : BOOLEAN = TRUE);

VAR     SrcIdx  : INTEGER;
        Dest    : STRING;
        InQuote : BOOLEAN;
        Current : CHAR;

BEGIN;
  Dest:='';
  InQuote:=FALSE;

  IF (ClearFirst) THEN
    Clear;

  FOR SrcIdx:=1 TO Length(ToSplit) DO
  BEGIN;
    Current:=ToSplit[SrcIdx];

    IF (Current=QuoteChar) THEN
      InQuote:=NOT InQuote;

    IF ((Current=Delimiter) AND (NOT InQuote)) THEN
    BEGIN;
      Add(Dest);
      Dest:='';
    END;

    IF ((InQuote AND (Current<>QuoteChar)) OR
        (NOT InQuote AND (Current<>Delimiter) AND (Current<>QuoteChar))) THEN
      Dest:=Dest+Current;
  END;
  IF (Length(Dest)>0) THEN
    Add(Dest);
END;

PROCEDURE TRamothStringList.ClearAndSplit(ToSplit       : STRING);

BEGIN;
  Clear;
  Split(ToSplit);
END;

FUNCTION TRamothStringList.GetBoolValue(AName	: STRING) : BOOLEAN;

BEGIN;
  Result:=StrToBoolDef(Values[AName],False);
END;

PROCEDURE TRamothStringList.SetBoolValue(AName	: STRING;
				 	 AValue	: BOOLEAN);

BEGIN;
  Values[AName]:=BoolToStr(AValue,TRUE);
END;

end.
