const
   ERROR_AVI_FILE                  = 10000;
   ERROR_UNKNOWN_FILE_KEY          = 10001;
   ERROR_CHECKSUM_ERROR            = 10002;
   ERROR_INTERNAL_FILE             = 10003;
   ERROR_BASE_FILE_MISSING         = 10004;
   ERROR_MARKED_FOR_DELETE         = 10005;

// SFileCreateArchive
   HASH_TABLE_SIZE_MIN        = $00000004;
   HASH_TABLE_SIZE_DEFAULT    = $00001000;
   HASH_TABLE_SIZE_MAX        = $00080000;

   HASH_ENTRY_DELETED         = $FFFFFFFE;
   HASH_ENTRY_FREE            = $FFFFFFFF;

   HET_ENTRY_DELETED                = $80;
   HET_ENTRY_FREE                   = $00;

   HASH_STATE_SIZE                  = $60;

   MPQ_PATCH_PREFIX_LEN             = $20;

// Values for SFileOpenArchive
   SFILE_OPEN_HARD_DISK_FILE           = 2;
   SFILE_OPEN_CDROM_FILE               = 3;

// Values for SFileOpenFile
   SFILE_OPEN_FROM_MPQ        = $00000000;
   SFILE_OPEN_BASE_FILE       = $FFFFFFFD;
   SFILE_OPEN_ANY_LOCALE      = $FFFFFFFE;
   SFILE_OPEN_LOCAL_FILE      = $FFFFFFFF;

// Flags for TMPQArchive::dwFlags
   MPQ_FLAG_READ_ONLY         = $00000001;
   MPQ_FLAG_CHANGED           = $00000002;
   MPQ_FLAG_PROTECTED         = $00000004;
   MPQ_FLAG_CHECK_SECTOR_CRC  = $00000008;
   MPQ_FLAG_NEED_FIX_SIZE     = $00000010;
   MPQ_FLAG_INV_LISTFILE      = $00000020;
   MPQ_FLAG_INV_ATTRIBUTES    = $00000040;

// Return value for SFileGetFileSize and SFileSetFilePointer
   SFILE_INVALID_SIZE         = $FFFFFFFF;
   SFILE_INVALID_POS          = $FFFFFFFF;
   SFILE_INVALID_ATTRIBUTES   = $FFFFFFFF;

// Flags for SFileAddFile
   MPQ_FILE_IMPLODE           = $00000100;
   MPQ_FILE_COMPRESS          = $00000200;
   MPQ_FILE_COMPRESSED        = $0000FF00;
   MPQ_FILE_ENCRYPTED         = $00010000;
   MPQ_FILE_FIX_KEY           = $00020000;
   MPQ_FILE_PATCH_FILE        = $00100000;
   MPQ_FILE_SINGLE_UNIT       = $01000000;
   MPQ_FILE_DELETE_MARKER     = $02000000;
   MPQ_FILE_SECTOR_CRC        = $04000000;

   MPQ_FILE_EXISTS            = $80000000;
   MPQ_FILE_REPLACEEXISTING   = $80000000;

   MPQ_FILE_VALID_FLAGS       = (MPQ_FILE_IMPLODE or
                                  MPQ_FILE_COMPRESS or
                                  MPQ_FILE_ENCRYPTED or
                                  MPQ_FILE_FIX_KEY or
                                  MPQ_FILE_PATCH_FILE or
                                  MPQ_FILE_SINGLE_UNIT or
                                  MPQ_FILE_DELETE_MARKER or
                                  MPQ_FILE_SECTOR_CRC or
                                  MPQ_FILE_EXISTS);

// Compression types for multiple compressions
   MPQ_COMPRESSION_HUFFMANN         = $01;
   MPQ_COMPRESSION_ZLIB             = $02;
   MPQ_COMPRESSION_PKWARE           = $08;
   MPQ_COMPRESSION_BZIP2            = $10;
   MPQ_COMPRESSION_SPARSE           = $20;
   MPQ_COMPRESSION_ADPCM_MONO       = $40;
   MPQ_COMPRESSION_ADPCM_STEREO     = $80;
   MPQ_COMPRESSION_LZMA             = $12;
   MPQ_COMPRESSION_NEXT_SAME  = $FFFFFFFF;

// Constants for SFileAddWave
   MPQ_WAVE_QUALITY_HIGH               = 0;
   MPQ_WAVE_QUALITY_MEDIUM             = 1;
   MPQ_WAVE_QUALITY_LOW                = 2;

// Signatures for HET and BET table
   HET_TABLE_SIGNATURE        = $1A544548;
   BET_TABLE_SIGNATURE        = $1A544542;

// Decryption keys for MPQ tables
   MPQ_KEY_HASH_TABLE         = $C3AF3770;
   MPQ_KEY_BLOCK_TABLE        = $EC83B3A3;

// Block map defines
   MPQ_DATA_BITMAP_SIGNATURE  = $33767470;

// Constants for SFileGetFileInfo
   SFILE_INFO_ARCHIVE_NAME            =  1;
   SFILE_INFO_ARCHIVE_SIZE            =  2;
   SFILE_INFO_MAX_FILE_COUNT          =  3;
   SFILE_INFO_HASH_TABLE_SIZE         =  4;
   SFILE_INFO_BLOCK_TABLE_SIZE        =  5;
   SFILE_INFO_SECTOR_SIZE             =  6;
   SFILE_INFO_HASH_TABLE              =  7;
   SFILE_INFO_BLOCK_TABLE             =  8;
   SFILE_INFO_NUM_FILES               =  9;
   SFILE_INFO_STREAM_FLAGS            = 10;
   SFILE_INFO_IS_READ_ONLY            = 11;
//------
   SFILE_INFO_HASH_INDEX             = 100;
   SFILE_INFO_CODENAME1              = 101;
   SFILE_INFO_CODENAME2              = 102;
   SFILE_INFO_LOCALEID               = 103;
   SFILE_INFO_BLOCKINDEX             = 104;
   SFILE_INFO_FILE_SIZE              = 105;
   SFILE_INFO_COMPRESSED_SIZE        = 106;
   SFILE_INFO_FLAGS                  = 107;
   SFILE_INFO_POSITION               = 108;
   SFILE_INFO_KEY                    = 109;
   SFILE_INFO_KEY_UNFIXED            = 110;
   SFILE_INFO_FILETIME               = 111;
   SFILE_INFO_PATCH_CHAIN            = 112;

   LISTFILE_NAME       = '(listfile)';
   SIGNATURE_NAME      = '(signature)';
   ATTRIBUTES_NAME     = '(attributes)';
   PATCH_METADATA_NAME = '(patch_metadata)';

   STORMLIB_VERSION        = $0814;
   STORMLIB_VERSION_STRING = '8.20';

   MPQ_FORMAT_VERSION_1                = 0;
   MPQ_FORMAT_VERSION_2                = 1;
   MPQ_FORMAT_VERSION_3                = 2;
   MPQ_FORMAT_VERSION_4                = 3;

// Flags for MPQ attributes
   MPQ_ATTRIBUTE_CRC32        = $00000001;
   MPQ_ATTRIBUTE_FILETIME     = $00000002;
   MPQ_ATTRIBUTE_MD5          = $00000004;
   MPQ_ATTRIBUTE_PATCH_BIT    = $00000008;
   MPQ_ATTRIBUTE_ALL          = $0000000F;

   MPQ_ATTRIBUTES_V1          =        100;

// Flags for SFileOpenArchive
   BASE_PROVIDER_FILE         = $00000000;
   BASE_PROVIDER_MAP          = $00000001;
   BASE_PROVIDER_HTTP         = $00000002;
   BASE_PROVIDER_MASK         = $0000000F;

   STREAM_PROVIDER_LINEAR     = $00000000;
   STREAM_PROVIDER_PARTIAL    = $00000010;
   STREAM_PROVIDER_ENCRYPTED  = $00000020;
   STREAM_PROVIDER_MASK       = $000000F0;

   STREAM_FLAG_READ_ONLY      = $00000100;
   STREAM_FLAG_WRITE_SHARE    = $00000200;
   STREAM_FLAG_MASK           = $0000FF00;
   STREAM_OPTIONS_MASK        = $0000FFFF;

   MPQ_OPEN_NO_LISTFILE       = $00010000;
   MPQ_OPEN_NO_ATTRIBUTES     = $00020000;
   MPQ_OPEN_FORCE_MPQ_V1      = $00040000;
   MPQ_OPEN_CHECK_SECTOR_CRC  = $00080000;

// SFileCreateArchive
   MPQ_CREATE_ATTRIBUTES      = $00100000;
   MPQ_CREATE_ARCHIVE_V1      = $00000000;
   MPQ_CREATE_ARCHIVE_V2      = $01000000;
   MPQ_CREATE_ARCHIVE_V3      = $02000000;
   MPQ_CREATE_ARCHIVE_V4      = $03000000;
   MPQ_CREATE_ARCHIVE_VMASK   = $0F000000;

   FLAGS_TO_FORMAT_SHIFT              = 24;

// SFileVerifyFile
   SFILE_VERIFY_SECTOR_CRC    = $00000001;
   SFILE_VERIFY_FILE_CRC      = $00000002;
   SFILE_VERIFY_FILE_MD5      = $00000004;
   SFILE_VERIFY_RAW_MD5       = $00000008;
   SFILE_VERIFY_ALL           = $0000000F;

// SFileVerifyFile
   VERIFY_OPEN_ERROR              = $0001;
   VERIFY_READ_ERROR              = $0002;
   VERIFY_FILE_HAS_SECTOR_CRC     = $0004;
   VERIFY_FILE_SECTOR_CRC_ERROR   = $0008;
   VERIFY_FILE_HAS_CHECKSUM       = $0010;
   VERIFY_FILE_CHECKSUM_ERROR     = $0020;
   VERIFY_FILE_HAS_MD5            = $0040;
   VERIFY_FILE_MD5_ERROR          = $0080;
   VERIFY_FILE_HAS_RAW_MD5        = $0100;
   VERIFY_FILE_RAW_MD5_ERROR      = $0200;
   VERIFY_FILE_ERROR_MASK     = (VERIFY_OPEN_ERROR or VERIFY_READ_ERROR or VERIFY_FILE_SECTOR_CRC_ERROR or VERIFY_FILE_CHECKSUM_ERROR or VERIFY_FILE_MD5_ERROR or VERIFY_FILE_RAW_MD5_ERROR);

// SFileVerifyRawData
   SFILE_VERIFY_MPQ_HEADER        = $0001;
   SFILE_VERIFY_HET_TABLE         = $0002;
   SFILE_VERIFY_BET_TABLE         = $0003;
   SFILE_VERIFY_HASH_TABLE        = $0004;
   SFILE_VERIFY_BLOCK_TABLE       = $0005;
   SFILE_VERIFY_HIBLOCK_TABLE     = $0006;
   SFILE_VERIFY_FILE              = $0007;

// SFileVerifyArchive
   ERROR_NO_SIGNATURE                  = 0;
   ERROR_VERIFY_FAILED                 = 1;
   ERROR_WEAK_SIGNATURE_OK             = 2;
   ERROR_WEAK_SIGNATURE_ERROR          = 3;
   ERROR_STRONG_SIGNATURE_OK           = 4;
   ERROR_STRONG_SIGNATURE_ERROR        = 5;

