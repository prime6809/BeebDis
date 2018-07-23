UNIT ParameterListUnit;

{$mode delphi}

INTERFACE

uses
  Classes, SysUtils;

{ Constants for setting assembler output directives, these should be set by   }
{ setting TMemoryList's Parameters.                                           }
{ The defaults are useful for BeebASM, the Disassembler6809Unit changes these }
{ to be the standard 6809 equivilents as used by the mamou assembler.         }
{ In the future I may allow for these to be defined in the control file       }
CONST
    {MemoryList related}
    mlLabelPrefix   = 'LabelPrefix';    // e.g. the preiod before beebasm lables
    mlLabelSuffix   = 'LabelSuffix';    // e.g. a colon after the lable
                                        // token used to define :
    mlDefineByte    = 'DefineByte';     // bytes : EQUB, FCB etc
    mlDefineWord    = 'DefineWord';     // words : EQUW, FDB etc
    mlDefineDWord   = 'DefineDWord';    // dwords : EQUD, FCD? etc
    mlDefineString  = 'DefineString';   // strings : EQUS, FCC etc
    mlOrigin        = 'Origin';         // the origin : ORG etc
    mlEquate        = 'Equate';         // a symbol, =, EQU etc.
    mlBeginIgnore   = 'BeginIgnore';    // beginning of an ignored block
    mlEndIgnore     = 'EndIgnore';      // end of an ignored block
    mlSaveCmd       = 'SaveCommand';    // the save command (just for beebasm currently)
    mlCommentChar   = 'CommentChar';    // Character used to start a comment
    mlCommentCol    = 'CommentCol';     // (minimum) Column to start comments

    numBinPrefix    = 'BinPrefix';      // Binary prefix (if any).
    numBinSuffix    = 'BinSuffix';      // Binary prefix (if any).
    numOctPrefix    = 'OctPrefix';      // Octal prefix (if any).
    numOctSuffix    = 'OctSuffix';      // Octal  prefix (if any).
    numDecPrefix    = 'DecPrefix';      // Decimal prefix (if any).
    numDecSuffix    = 'DecSuffix';      // Decimal prefix (if any).
    numHexPrefix    = 'HexPrefix';      // Hexidecimal prefix (if any).
    numHexSuffix    = 'HexSuffix';      // Hexidecimal prefix (if any).

    parExecFormat   = 'ExecFormat';     // Executable format to load
    parRadix        = 'Radix';          // Radix for output numbers

TYPE
    TParameterList = class(TObject)
    PROTECTED
      FParameters   : TStringList;
      FOverWrite    : BOOLEAN;

      FUNCTION GetAsString : STRING;
      FUNCTION GetParam(Index   : STRING) : STRING;
      PROCEDURE SetParam(Index  : STRING;
                         Value  : STRING);
      FUNCTION GetParamBool(Index   : STRING) : BOOLEAN;
      PROCEDURE SetParamBool(Index  : STRING;
                             Value  : BOOLEAN);
      FUNCTION GetParamInteger(Index   : STRING) : Integer;
      PROCEDURE SetParamInteger(Index  : STRING;
                                Value  : Integer);

    PUBLIC
      PROPERTY Parameters[Index : STRING] : STRING READ GetParam WRITE SetParam; Default;
      PROPERTY BoolParameters[Index : STRING] : BOOLEAN READ GetParamBool WRITE SetParamBool;
      PROPERTY IntegerParameters[Index : STRING] : INTEGER READ GetParamInteger WRITE SetParamInteger;
      PROPERTY AsString : STRING READ GetAsString;
      PROPERTY Overwrite : BOOLEAN READ FOverWrite WRITE FOverWrite;

      CONSTRUCTOR Create;
      DESTRUCTOR Destroy; OVERRIDE;
    END;

implementation

CONSTRUCTOR TParameterList.Create;

BEGIN;
  INHERITED Create;
  Overwrite:=TRUE;
  FParameters:=TStringList.Create;
END;

DESTRUCTOR TParameterList.Destroy;

BEGIN;
  FParameters.Free;

  INHERITED Destroy;
END;

FUNCTION TParameterList.GetAsString : STRING;

BEGIN;
  Result:=FParameters.Text;;
END;

FUNCTION TParameterList.GetParam(Index   : STRING) : STRING;

BEGIN;
  Result:=FParameters.Values[LowerCase(Index)];
END;

PROCEDURE TParameterList.SetParam(Index  : STRING;
                                  Value  : STRING);

BEGIN;
  IF (Overwrite OR (FParameters.Values[LowerCase(Index)]='')) THEN
    FParameters.Values[LowerCase(Index)]:=Value;
END;

FUNCTION TParameterList.GetParamBool(Index   : STRING) : BOOLEAN;

BEGIN;
  Result:=StrToBoolDef(FParameters.Values[LowerCase(Index)],FALSE);
END;

PROCEDURE TParameterList.SetParamBool(Index  : STRING;
                                      Value  : BOOLEAN);

BEGIN;
  SetParam(Index, BoolToStr(Value));
END;

FUNCTION TParameterList.GetParamInteger(Index   : STRING) : Integer;

BEGIN;
  Result:=StrToIntDef(FParameters.Values[LowerCase(Index)],0);
END;

PROCEDURE TParameterList.SetParamInteger(Index  : STRING;
                                         Value  : Integer);

BEGIN;
  SetParam(Index, IntToStr(Value));
END;

END.

