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
    fileds     INT(10) VALUE;
    buffer     POINTER VALUE;
    buffersize UNS(10) VALUE;
END-PR;

DCL-PR write INT(10) EXTPROC('write');
    fileds     INT(10) VALUE;
    buffer     POINTER VALUE;
    buffersize UNS(10) VALUE;
END-PR;

// oflag
DCL-C O_RDONLY   1;
DCL-C O_WRONLY   2;
DCL-C O_RDWR     4;
DCL-C O_CREAT    8;
DCL-C O_EXCL     16;
DCL-C O_TRUNC    64;
DCL-C O_APPEND   256;
DCL-C O_CODEPAGE 8388608;
DCL-C O_TEXTDATA 16777216;

// mode
// owner authority
DCL-C S_IRUSR  256;
DCL-C S_IWUSR  128;
DCL-C S_IXUSR  64;

// group authority
DCL-C S_IRGRP  32;
DCL-C S_IWGRP  16;
DCL-C S_IXGRP  8;

// other people
DCL-C S_IROTH  4;
DCL-C S_IWOTH  2;
DCL-C S_IXOTH  1;







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


