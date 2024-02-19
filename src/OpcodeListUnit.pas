UNIT OpcodeListUnit;

{$mode delphi}

INTERFACE

USES
  Classes, SysUtils,Contnrs,;



TYPE
    TOpcodeList = Class(TObjectList)
      FUNCTION GetOpcode(Index      : WORD;
                         VAR OpCode : TOpCode) : BOOLEAN;
    END;

IMPLEMENTATION


END.

