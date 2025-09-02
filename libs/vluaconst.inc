const
  LUA_VERSION_        = 'Lua 5.2.4';
  LUA_VERSION_MAJOR   = '5';
  LUA_VERSION_MINOR   = '2';
  LUA_VERSION_NUM     = 502;
  LUA_VERSION_RELEASE = '4';
  LUA_SIGNATURE       = #27'Lua';

  LUA_MULTRET         = -1;
  LUAI_FIRSTPSEUDOIDX = -1001000;
  LUA_REGISTRYINDEX   = LUAI_FIRSTPSEUDOIDX;

  LUA_RIDX_MAINTHREAD = 1;
  LUA_RIDX_GLOBALS    = 2;
  LUA_RIDX_LAST       = LUA_RIDX_GLOBALS;

const
  LUA_OK       = 0;
  LUA_YIELD_   = 1;
  LUA_ERRRUN   = 2;
  LUA_ERRSYNTAX= 3;
  LUA_ERRMEM   = 4;
  LUA_ERRGCMM  = 5;
  LUA_ERRERR   = 6;

const
  LUA_TNONE          = -1;
  LUA_TNIL           = 0;
  LUA_TBOOLEAN       = 1;
  LUA_TLIGHTUSERDATA = 2;
  LUA_TNUMBER        = 3;
  LUA_TSTRING        = 4;
  LUA_TTABLE         = 5;
  LUA_TFUNCTION      = 6;
  LUA_TUSERDATA      = 7;
  LUA_TTHREAD        = 8;

const
  LUA_OPEQ = 0;
  LUA_OPLT = 1;
  LUA_OPLE = 2;

const
  LUA_GCSTOP       = 0;
  LUA_GCRESTART    = 1;
  LUA_GCCOLLECT    = 2;
  LUA_GCCOUNT      = 3;
  LUA_GCCOUNTB     = 4;
  LUA_GCSTEP       = 5;
  LUA_GCSETPAUSE   = 6;
  LUA_GCSETSTEPMUL = 7;
  LUA_GCSETMAJORINC= 8;
  LUA_GCISRUNNING  = 9;
  LUA_GCGEN        = 10;
  LUA_GCINC        = 11;

const
  LUA_HOOKCALL     = 0;
  LUA_HOOKRET      = 1;
  LUA_HOOKLINE     = 2;
  LUA_HOOKCOUNT    = 3;
  LUA_HOOKTAILCALL = 4;

  LUA_MASKCALL  = 1 shl LUA_HOOKCALL;
  LUA_MASKRET   = 1 shl LUA_HOOKRET;
  LUA_MASKLINE  = 1 shl LUA_HOOKLINE;
  LUA_MASKCOUNT = 1 shl LUA_HOOKCOUNT;

const
  LUA_COLIBNAME   = 'coroutine';
  LUA_TABLIBNAME  = 'table';
  LUA_IOLIBNAME   = 'io';
  LUA_OSLIBNAME   = 'os';
  LUA_STRLIBNAME  = 'string';
  LUA_MATHLIBNAME = 'math';
  LUA_DBLIBNAME   = 'debug';
  LUA_LOADLIBNAME = 'package';


const
  LUA_ERRFILE = LUA_ERRERR + 1;

const
  LUA_NOREF  = -2;
  LUA_REFNIL = -1;

const
  LUA_IDSIZE       = 60;
  LUAL_BUFFERSIZE  = 8192;

