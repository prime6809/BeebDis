unit ConsoleUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

USES Classes,SysUtils;

CONST   VBSilent    = 0;
        VBNormal    = 1;
        VBVerbose   = 2;
        VBDebug     = 3;

{ Formatted versions of Write / WriteLn                                        }
PROCEDURE WriteFmt(Fmt		: STRING;
		           Params	: ARRAY OF CONST);

PROCEDURE WriteLnFmt(Fmt	: STRING;
		             Params	: ARRAY OF CONST);

{ Conditional formatted versions of Write / WriteLn, only outputs if condition }
{ is true.                                                                     }
PROCEDURE WriteCondFmt(Condition    : BOOLEAN;
                       Fmt		    : STRING;
		               Params	    : ARRAY OF CONST);

PROCEDURE WriteLnCondFmt(Condition  : BOOLEAN;
                         Fmt        : STRING;
		                 Params     : ARRAY OF CONST);

{ Verbosity testing versions of Write / WriteLn, only produces output if       }
{ VLevel >= OPLevel.                                                           }
PROCEDURE WriteFmtV(VLevel  : BYTE;
                    OPLevel : BYTE;
                    Fmt		: STRING;
		            Params	: ARRAY OF CONST);

PROCEDURE WriteLnFmtV(VLevel    : BYTE;
                      OPLevel   : BYTE;
                      Fmt	    : STRING;
		              Params    : ARRAY OF CONST);


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

PROCEDURE WriteCondFmt(Condition    : BOOLEAN;
                       Fmt		    : STRING;
		               Params	    : ARRAY OF CONST);

BEGIN;
  IF (Condition) THEN
    WriteFmt(Fmt,Params);
END;

PROCEDURE WritelNCondFmt(Condition  : BOOLEAN;
                         Fmt        : STRING;
		                 Params     : ARRAY OF CONST);

BEGIN;
  IF (Condition) THEN
    WriteLnFmt(Fmt,Params);
END;
PROCEDURE WriteFmtV(VLevel  : BYTE;
                    OPLevel : BYTE;
                    Fmt		: STRING;
		            Params	: ARRAY OF CONST);

BEGIN;
  IF (VLevel >= OPLevel) THEN
    WriteFmt(Fmt,Params);
END;


PROCEDURE WriteLnFmtV(VLevel    : BYTE;
                      OPLevel   : BYTE;
                      Fmt	    : STRING;
		              Params	: ARRAY OF CONST);

BEGIN;
  IF (VLevel >= OPLevel) THEN
    WriteLnFmt(Fmt,Params);
END;

end.
