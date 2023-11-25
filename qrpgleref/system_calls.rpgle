**FREE

DCL-PR system INT(10) EXTPROC('system');
    Command POINTER VALUE OPTIONS(*STRING);
END-PR;

DCL-C SYSTEM_SUCCESS 0;



DCL-S pFile  POINTER TEMPLATE;
DCL-S stdin  LIKE(pFile) IMPORT('_C_IFS_stdin');
DCL-S stdout LIKE(pFile) IMPORT('_C_IFS_stdout');
DCL-S stderr LIKE(pFile) IMPORT('_C_IFS_stderr');



DCL-PR fopen LIKE(pFile) EXTPROC('_C_IFS_fopen');
    filename POINTER VALUE OPTIONS(*STRING);
    mode     POINTER VALUE OPTIONS(*STRING);
END-PR;

DCL-PR fclose INT(10) EXTPROC('_C_IFS_fclose');
    stream LIKE(pFile) VALUE;
END-PR;



DCL-PR fgets POINTER EXTPROC('_C_IFS_fgets');
    string POINTER VALUE;
    size   INT(10) VALUE;
    stream LIKE(pFile) VALUE;
END-PR;

DCL-PR fputs INT(10) EXTPROC('_C_IFS_fputs');
    string POINTER VALUE OPTIONS(*STRING);
    stream LIKE(pFile) VALUE;
END-PR;



DCL-PR fread UNS(10) EXTPROC('_C_IFS_fread');
    data   POINTER VALUE;
    size   UNS(10) VALUE;
    count  UNS(10) VALUE;
    stream LIKE(pFile) VALUE;
END-PR;

DCL-PR fwrite UNS(10) EXTPROC('_C_IFS_fwrite');
    data   POINTER VALUE;
    size   UNS(10) VALUE;
    count  UNS(10) VALUE;
    stream LIKE(pFile) VALUE;
END-PR;



DCL-C NO  'N';
DCL-C YES 'Y';

DCL-C FALSE '0';
DCL-C TRUE  '1';

DCL-C NULL X'00';

DCL-C LOWER 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
DCL-C UPPER 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';

DCL-C NUMBER '0123456789';
DCL-C SIGNED '0123456789-';


