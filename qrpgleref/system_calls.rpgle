**FREE

DCL-PR system INT(10) EXTPROC('system');
    Command POINTER VALUE OPTIONS(*STRING);
END-PR;

DCL-C SYSTEM_SUCCESS 0;


DCL-PR errorifs POINTER EXTPROC('__errno');
END-PR;

DCL-PR strerror POINTER EXTPROC('strerror');
    error_num INT(10) VALUE;
END-PR;

DCL-PR open INT(10) EXTPROC('open');
    ifspath  POINTER VALUE OPTIONS(*STRING);
    oflag    INT(10) VALUE;
    mode     UNS(10) VALUE OPTIONS(*NOPASS);
    codepage UNS(10) VALUE OPTIONS(*NOPASS);
END-PR;

DCL-PR close INT(10) EXTPROC('close');
    fileds INT(10) VALUE;
END-PR;

DCL-PR read INT(10) EXTPROC('read');
    fileds    INT(10) VALUE;
    buffer    POINTER VALUE;
    noofbytes UNS(10) VALUE;
END-PR;

DCL-PR write INT(10) EXTPROC('write');
    fileds    INT(10) VALUE;
    buffer    POINTER VALUE;
    noofbytes UNS(10) VALUE;
END-PR;

// oflag
DCL-C O_readonly              1;
DCL-C O_writeonly             2;
DCL-C O_readwrite             4;
DCL-C O_createfileifnotexist  8;
DCL-C O_exclusivecreate       16;
DCL-C O_truncateto0bytes      64;
DCL-C O_appendtofile          256;
DCL-C O_converttextbycodepage 8388608;
DCL-C O_openintextmode        16777216;

// mode
// owner authority
DCL-C M_readowner    256;
DCL-C M_writeowner   128;
DCL-C M_executeowner 64;
// group authority
DCL-C M_readgroup    32;
DCL-C M_writegroup   16;
DCL-C M_executegroup 8;
// other people
DCL-C M_readother    4;
DCL-C M_writeother   2;
DCL-C M_executeother 1;






DCL-C NO  'N';
DCL-C YES 'Y';

DCL-C FALSE '0';
DCL-C TRUE  '1';

DCL-C NULL X'00';

DCL-C LOWER 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
DCL-C UPPER 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';

DCL-C NUMBER '0123456789';
DCL-C SIGNED '0123456789-';

DCL-C CR   x'0D';
DCL-C LF   x'25';


