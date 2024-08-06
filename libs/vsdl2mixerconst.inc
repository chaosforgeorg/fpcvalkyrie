const
  MIX_CHANNELS          = 8;
  MIX_DEFAULT_FREQUENCY = 22050;
  MIX_DEFAULT_FORMAT    = AUDIO_S16SYS;
  MIX_DEFAULT_CHANNELS  = 2;
  MIX_MAX_VOLUME        = 128;

  MIX_CHANNEL_POST      = -2;
  MIX_EFFECTSMAXSPEED   = 'MIX_EFFECTSMAXSPEED';

const
  MIX_INIT_FLAC    = $00000001;
  MIX_INIT_MOD     = $00000002;
  MIX_INIT_MP3     = $00000008;
  MIX_INIT_OGG     = $00000010;
  MIX_INIT_MID     = $00000020;
  MIX_INIT_OPUS    = $00000040;
  MIX_INIT_WAVPACK = $00000080;

// Mix_Fading constants
const
  MIX_NO_FADING    = 0;
  MIX_FADING_OUT   = 1;
  MIX_FADING_IN    = 2;

// Mix_MusicType constants
const
  MUS_NONE             = 0;
  MUS_CMD              = 1;
  MUS_WAV              = 2;
  MUS_MOD              = 3;
  MUS_MID              = 4;
  MUS_OGG              = 5;
  MUS_MP3              = 6;
  MUS_MP3_MAD_UNUSED   = 7;
  MUS_FLAC             = 8;
  MUS_MODPLUG_UNUSED   = 9;
  MUS_OPUS             = 10;
  MUS_WAVPACK          = 11;
  MUS_GME              = 12;

