type
  Mix_Chunk = record
    allocated: Integer;
    abuf: PUInt8;
    alen: Uint32;
    volume: Uint8;
  end;
  PMix_Chunk = ^Mix_Chunk;
  Mix_Music  = record;
  PMix_Music = ^Mix_Music;

// Function types
type
  Mix_EffectFunc_t = procedure(chan: Integer; stream: Pointer; len: Integer; udata: Pointer); cdecl;
  Mix_EffectDone_t = procedure(chan: Integer; udata: Pointer); cdecl;
  Mix_MusicType    = Integer;
  Mix_Fading       = Integer;


