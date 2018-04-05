unit BeebDisDefsUnit;

interface

CONST
	Major	= 1;
	Minor	= 00;

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
	OptNewSym	    = 'NewSym';
	OptNewSymFile	= 'NewSymFile';

implementation

end.
