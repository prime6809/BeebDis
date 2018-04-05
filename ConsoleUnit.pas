unit ConsoleUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

USES Classes,SysUtils;

PROCEDURE WriteFmt(Fmt		: STRING;
		           Params	: ARRAY OF CONST);

PROCEDURE WriteLnFmt(Fmt	: STRING;
		             Params	: ARRAY OF CONST);

implementation

PROCEDURE WriteFmt(Fmt		: STRING;
		           Params	: ARRAY OF CONST);

BEGIN;
  Write(Format(Fmt,Params));
END;

PROCEDURE WriteLnFmt(Fmt	: STRING;
		             Params	: ARRAY OF CONST);

BEGIN;
  WriteLn(Format(Fmt,Params));
END;

end.
