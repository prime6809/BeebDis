unit BeebDisDefsUnit;

interface

CONST
	Major	= 1;
	Minor	= 25;

{$IFDEF UNIX}
	Eol		= #$0A;
{$ELSE}
	Eol		= #$0D+#$0A;
{$ENDIF}
    FirstColumn	    = 8;
	SecondColumn	= 16;

	StartAddrLable  = 'BeebDisStartAddr';
    EndAddrLable  	= 'BeebDisEndAddr';
	BinExt		    = '.bin';
    TokenPC		    = 'pc';

	OptHexDump	    = 'HexDump';
	OptHexDumpFile	= 'HexDumpFile';
	OptStringScan	= 'StringScan';
    OptInlineScan   = 'InlineScan';
	OptNewSym	    = 'NewSym';
	OptNewSymFile	= 'NewSymFile';

    { Keywords accepted by control file parser, defined here as symbolic names }
    { to make future code mainmaintenance easier!   }

    KWRepeat        = 'repeat';
    KWEndRepeat     = 'endrepeat';
    KWActive        = 'active';
    KWLoad          = 'load';
    KWSymbols       = 'symbols';
    KWSave          = 'save';
    KWByte          = 'byte';
    KWWord          = 'word';
    KWDWord         = 'dword';
    KWString        = 'string';
    KWStringz       = 'stringz';
    KWStringTerm    = 'stringterm';
    KWStringHi      = 'stringhi';
    KWStringHiZ     = 'stringhiz';
    KWEntry         = 'entry';
    KWWordEntry     = 'wordentry';
    KWWordRTS       = 'wordrts';
    KWHexDump       = 'hexdump';
    KWStringScan    = 'stringscan';
    KWInlineScan    = 'inlinescan';
    KWNewSym        = 'newsym';
    KWNewPC         = 'newpc';
    KWCPU           = 'cpu';
    KWOption        = 'option';
    KWRadix         = 'radix';
    KWVerbose       = 'verbose';

implementation

end.
