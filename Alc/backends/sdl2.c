/**
 * OpenAL cross platform audio library
 * Copyright (C) 2018 by authors.
 * This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Library General Public
 *  License as published by the Free Software Foundation; either
 *  version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 *  License along with this library; if not, write to the
 *  Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 * Or go to http://www.gnu.org/copyleft/lgpl.html
 */


#include <switch.h>

#include "config.h"

#include <stdlib.h>
#include <malloc.h>

#include "alMain.h"
#include "alu.h"
#include "threads.h"
#include "compat.h"

#include "base.h"


 //------------------------------------------------------------------------------
 // String IDs
 //------------------------------------------------------------------------------
#undef          __NAME__
#undef          __VER__
#define         __NAME__        "Nintendo Switch Backend for OpenAL Soft"
#define         __VER__         "1.00"
#if __SZID__
static char     szID[256] = "$Id: " __FILE__ " " __VER__ " " __DATE__ " " __TIME__ " Hedges Exp $\n";
#endif  /* __SZID__ */


//------------------------------------------------------------------------------
// Constants & Defs
//------------------------------------------------------------------------------
typedef uint8_t Uint8;
typedef uint16_t Uint16;
typedef uint32_t Uint32;


typedef Uint16 SDL_AudioFormat;


#define SDL_AUDIO_MASK_BITSIZE       (0xFF)
#define SDL_AUDIO_MASK_DATATYPE      (1<<8)
#define SDL_AUDIO_MASK_ENDIAN        (1<<12)
#define SDL_AUDIO_MASK_SIGNED        (1<<15)
#define SDL_AUDIO_BITSIZE(x)         (x & SDL_AUDIO_MASK_BITSIZE)
#define SDL_AUDIO_ISFLOAT(x)         (x & SDL_AUDIO_MASK_DATATYPE)
#define SDL_AUDIO_ISBIGENDIAN(x)     (x & SDL_AUDIO_MASK_ENDIAN)
#define SDL_AUDIO_ISSIGNED(x)        (x & SDL_AUDIO_MASK_SIGNED)
#define SDL_AUDIO_ISINT(x)           (!SDL_AUDIO_ISFLOAT(x))
#define SDL_AUDIO_ISLITTLEENDIAN(x)  (!SDL_AUDIO_ISBIGENDIAN(x))
#define SDL_AUDIO_ISUNSIGNED(x)      (!SDL_AUDIO_ISSIGNED(x))


#define AUDIO_U8        0x0008  /**< Unsigned 8-bit samples */
#define AUDIO_S8        0x8008  /**< Signed 8-bit samples */
#define AUDIO_U16LSB    0x0010  /**< Unsigned 16-bit samples */
#define AUDIO_S16LSB    0x8010  /**< Signed 16-bit samples */
#define AUDIO_U16MSB    0x1010  /**< As above, but big-endian byte order */
#define AUDIO_S16MSB    0x9010  /**< As above, but big-endian byte order */
#define AUDIO_U16       AUDIO_U16LSB
#define AUDIO_S16       AUDIO_S16LSB


#define AUDIO_S32LSB    0x8020  /**< 32-bit integer samples */
#define AUDIO_S32MSB    0x9020  /**< As above, but big-endian byte order */
#define AUDIO_S32       AUDIO_S32LSB


#define AUDIO_F32LSB    0x8120  /**< 32-bit floating point samples */
#define AUDIO_F32MSB    0x9120  /**< As above, but big-endian byte order */
#define AUDIO_F32       AUDIO_F32LSB


#define AUDIO_U16SYS    AUDIO_U16LSB
#define AUDIO_S16SYS    AUDIO_S16LSB
#define AUDIO_S32SYS    AUDIO_S32LSB
#define AUDIO_F32SYS    AUDIO_F32LSB


#define DEVNAME_PREFIX "OpenAL Soft on "


struct SDL_PrivateAudioData
{
	void *buffer[2];
	AudioOutBuffer source_buffer[2];
	AudioOutBuffer *released_buffer;
	u32 released_count;
};


typedef struct SDL_AudioSpec
{
	int freq;                   /**< DSP frequency -- samples per second */
	SDL_AudioFormat format;     /**< Audio data format */
	Uint8 channels;             /**< Number of channels: 1 mono, 2 stereo */
	Uint8 silence;              /**< Audio buffer silence value (calculated) */
	Uint16 samples;             /**< Audio buffer size in sample FRAMES (total samples divided by channel count) */
	Uint16 padding;             /**< Necessary for some compile environments */
	Uint32 size;                /**< Audio buffer size in bytes (calculated) */
} SDL_AudioSpec;


typedef struct ALCsdl2Backend {
    DERIVE_FROM_TYPE(ALCbackend);

    ALsizei frameSize;
    ALuint Frequency;
    enum DevFmtChannels FmtChans;
    enum DevFmtType     FmtType;
    ALuint UpdateSize;

	SDL_AudioSpec spec;

	struct SDL_PrivateAudioData *hidden;

	ATOMIC(int) killNow;
	althrd_t thread;
} ALCsdl2Backend;


static void ALCsdl2Backend_Construct(ALCsdl2Backend *self, ALCdevice *device);
static void ALCsdl2Backend_Destruct(ALCsdl2Backend *self);
static ALCenum ALCsdl2Backend_open(ALCsdl2Backend *self, const ALCchar *name);
static ALCboolean ALCsdl2Backend_reset(ALCsdl2Backend *self);
static ALCboolean ALCsdl2Backend_start(ALCsdl2Backend *self);
static void ALCsdl2Backend_stop(ALCsdl2Backend *self);
static DECLARE_FORWARD2(ALCsdl2Backend, ALCbackend, ALCenum, captureSamples, void*, ALCuint)
static DECLARE_FORWARD(ALCsdl2Backend, ALCbackend, ALCuint, availableSamples)
static DECLARE_FORWARD(ALCsdl2Backend, ALCbackend, ClockLatency, getClockLatency)
static void ALCsdl2Backend_lock(ALCsdl2Backend *self);
static void ALCsdl2Backend_unlock(ALCsdl2Backend *self);
DECLARE_DEFAULT_ALLOCATORS(ALCsdl2Backend)

DEFINE_ALCBACKEND_VTABLE(ALCsdl2Backend);


//------------------------------------------------------------------------------
// Global Data
//------------------------------------------------------------------------------
static const ALCchar defaultDeviceName[] = DEVNAME_PREFIX "Nintendo Switch";


//------------------------------------------------------------------------------
// Helper Code
//------------------------------------------------------------------------------
static Result nxOpenDevice(ALCsdl2Backend *this, SDL_AudioSpec *want)
{
	Result res;

	res = audoutInitialize();
	if(res != 0) {
		return res;
	}

	res = audoutStartAudioOut();
	if(res != 0) {
		audoutExit();
		return res;
	}

	this->hidden = (struct SDL_PrivateAudioData *) malloc(sizeof(*this->hidden));
	if(this->hidden == NULL) {
		return -1;
	}
	memset(this->hidden, 0, sizeof(*this->hidden));

	this->spec.format = AUDIO_S16LSB;
	this->spec.freq = 48000;
	this->spec.channels = 2;
	this->spec.samples = want->samples;

	this->spec.silence = 0x00;
	this->spec.size = SDL_AUDIO_BITSIZE(this->spec.format) / 8;
	this->spec.size *= this->spec.channels;
	this->spec.size *= this->spec.samples;

	for(int i = 0; i < 2; i++) {
		u32 size = (u32)(this->spec.size + 0xfff) & ~0xfff;
		this->hidden->buffer[i] = memalign(0x1000, size);
		memset(this->hidden->buffer[i], 0, size);
		this->hidden->source_buffer[i].next = NULL;
		this->hidden->source_buffer[i].buffer = this->hidden->buffer[i];
		this->hidden->source_buffer[i].buffer_size =
			(u64) this->spec.size / this->spec.channels / 4;
		this->hidden->source_buffer[i].data_size = (u64) this->spec.size;
		this->hidden->source_buffer[i].data_offset = (u64)0;
		audoutAppendAudioOutBuffer(&this->hidden->source_buffer[i]);
	}

	return 0;
}


static void nxCloseDevice(ALCsdl2Backend *this)
{
	if(this->hidden->buffer[0]) {
		free(this->hidden->buffer[0]);
	}
	if(this->hidden->buffer[1]) {
		free(this->hidden->buffer[1]);
	}

	audoutStopAudioOut();
	audoutExit();

	free(this->hidden);
}


static void nxPlayDevice(ALCsdl2Backend *this)
{
	audoutAppendAudioOutBuffer(this->hidden->released_buffer);
}


static Uint8 *nxGetDeviceBuf(ALCsdl2Backend *this)
{
	audoutWaitPlayFinish(&this->hidden->released_buffer,
						 &this->hidden->released_count, U64_MAX);

	return this->hidden->released_buffer->buffer;
}


static int ALCsdl2Backend_mixerProc(void *ptr)
{
	ALCsdl2Backend *self = (ALCsdl2Backend *)ptr;
	ALCdevice *device = STATIC_CAST(ALCbackend, self)->mDevice;
	struct timespec now, start;
	ALuint64 avail, done;
	const long restTime = (long)((ALuint64)device->UpdateSize * 1000000000 /
								 device->Frequency / 2);

	svcSetThreadPriority(CUR_THREAD_HANDLE, 0x1c);
	althrd_setname(althrd_current(), MIXER_THREAD_NAME);

	done = 0;
	if(altimespec_get(&start, AL_TIME_UTC) != AL_TIME_UTC)
	{
		ERR("Failed to get starting time\n");
		return 1;
	}
	while(!ATOMIC_LOAD(&self->killNow, almemory_order_acquire)/* &&
		  ATOMIC_LOAD(&device->Connected, almemory_order_acquire)*/)
	{
		if(altimespec_get(&now, AL_TIME_UTC) != AL_TIME_UTC)
		{
			ERR("Failed to get current time\n");
			return 1;
		}

		avail = (now.tv_sec - start.tv_sec) * device->Frequency;
		avail += (ALint64)(now.tv_nsec - start.tv_nsec) * device->Frequency / 1000000000;
		if(avail < done)
		{
			/* Oops, time skipped backwards. Reset the number of samples done
			* with one update available since we (likely) just came back from
			* sleeping. */
			done = avail - device->UpdateSize;
		}

		if(avail-done < device->UpdateSize)
			al_nssleep(restTime);
		else while(avail-done >= device->UpdateSize)
		{
			ALCsdl2Backend_lock(self);
			aluMixData(device, nxGetDeviceBuf(self), self->spec.size/self->frameSize);
			nxPlayDevice(self);
			ALCsdl2Backend_unlock(self);
			done += device->UpdateSize;
		}
	}

	return 0;
}


//------------------------------------------------------------------------------
// Code
//------------------------------------------------------------------------------
static void ALCsdl2Backend_Construct(ALCsdl2Backend *self, ALCdevice *device)
{
    ALCbackend_Construct(STATIC_CAST(ALCbackend, self), device);
    SET_VTABLE2(ALCsdl2Backend, ALCbackend, self);

	device->Frequency = 48000;
	device->FmtType = DevFmtShort;

    self->frameSize = FrameSizeFromDevFmt(device->FmtChans, device->FmtType, device->AmbiOrder);
    self->Frequency = device->Frequency;
    self->FmtChans = device->FmtChans;
    self->FmtType = device->FmtType;
    self->UpdateSize = device->UpdateSize;

	ATOMIC_INIT(&self->killNow, AL_TRUE);
}

static void ALCsdl2Backend_Destruct(ALCsdl2Backend *self)
{
    nxCloseDevice(self);
	
    ALCbackend_Destruct(STATIC_CAST(ALCbackend, self));
}

ALCenum ALCsdl2Backend_open(ALCsdl2Backend *self, const ALCchar *name)
{
    ALCdevice *device = STATIC_CAST(ALCbackend, self)->mDevice;
    SDL_AudioSpec want, have;

    memset(&want, 0, sizeof(want));
	memset(&have, 0, sizeof(have));

    want.freq = device->Frequency;
    switch(device->FmtType)
    {
        case DevFmtUByte: want.format = AUDIO_U8; break;
        case DevFmtByte: want.format = AUDIO_S8; break;
        case DevFmtUShort: want.format = AUDIO_U16SYS; break;
        case DevFmtShort: want.format = AUDIO_S16SYS; break;
        case DevFmtUInt: /* fall-through */
        case DevFmtInt: want.format = AUDIO_S32SYS; break;
        case DevFmtFloat: want.format = AUDIO_F32; break;
    }
    want.channels = (device->FmtChans == DevFmtMono) ? 1 : 2;
    want.samples = device->UpdateSize;

    nxOpenDevice(self, &want);

	have = want;

    device->Frequency = have.freq;
    if(have.channels == 1)
        device->FmtChans = DevFmtMono;
    else if(have.channels == 2)
        device->FmtChans = DevFmtStereo;
    else
    {
        ERR("Got unhandled SDL channel count: %d\n", (int)have.channels);
        return ALC_INVALID_VALUE;
    }
    switch(have.format)
    {
        case AUDIO_U8:     device->FmtType = DevFmtUByte;  break;
        case AUDIO_S8:     device->FmtType = DevFmtByte;   break;
        case AUDIO_U16SYS: device->FmtType = DevFmtUShort; break;
        case AUDIO_S16SYS: device->FmtType = DevFmtShort;  break;
        case AUDIO_S32SYS: device->FmtType = DevFmtInt;    break;
        case AUDIO_F32SYS: device->FmtType = DevFmtFloat;  break;
        default:
            ERR("Got unsupported SDL format: 0x%04x\n", have.format);
            return ALC_INVALID_VALUE;
    }
    device->UpdateSize = have.samples;
    device->NumUpdates = 3;

    self->frameSize = FrameSizeFromDevFmt(device->FmtChans, device->FmtType, device->AmbiOrder);
    self->Frequency = device->Frequency;
    self->FmtChans = device->FmtChans;
    self->FmtType = device->FmtType;
    self->UpdateSize = device->UpdateSize;

    alstr_copy_cstr(&device->DeviceName, name ? name : defaultDeviceName);

    return ALC_NO_ERROR;
}

static ALCboolean ALCsdl2Backend_reset(ALCsdl2Backend *self)
{
    ALCdevice *device = STATIC_CAST(ALCbackend, self)->mDevice;
    device->Frequency = self->Frequency;
    device->FmtChans = self->FmtChans;
    device->FmtType = self->FmtType;
    device->UpdateSize = self->UpdateSize;
    device->NumUpdates = 2;
    SetDefaultWFXChannelOrder(device);
    return ALC_TRUE;
}

static ALCboolean ALCsdl2Backend_start(ALCsdl2Backend *self)
{
	ATOMIC_STORE(&self->killNow, AL_FALSE, almemory_order_release);
	if(althrd_create(&self->thread, ALCsdl2Backend_mixerProc, self) != althrd_success)
		return ALC_FALSE;
	return ALC_TRUE;
}

extern bool g_emu;

static void ALCsdl2Backend_stop(ALCsdl2Backend *self)
{
	int res;

	if(ATOMIC_EXCHANGE(&self->killNow, AL_TRUE, almemory_order_acq_rel))
		return;
    if(g_emu)
    {
        res = 0;
    }
    else
    {
	    althrd_join(self->thread, &res);
    }
}

static void ALCsdl2Backend_lock(ALCsdl2Backend *self)
{
}

static void ALCsdl2Backend_unlock(ALCsdl2Backend *self)
{
}


typedef struct ALCsdl2BackendFactory {
    DERIVE_FROM_TYPE(ALCbackendFactory);
} ALCsdl2BackendFactory;
#define ALCsdl2BACKENDFACTORY_INITIALIZER { { GET_VTABLE2(ALCsdl2BackendFactory, ALCbackendFactory) } }

ALCbackendFactory *ALCsdl2BackendFactory_getFactory(void);

static ALCboolean ALCsdl2BackendFactory_init(ALCsdl2BackendFactory *self);
static void ALCsdl2BackendFactory_deinit(ALCsdl2BackendFactory *self);
static ALCboolean ALCsdl2BackendFactory_querySupport(ALCsdl2BackendFactory *self, ALCbackend_Type type);
static void ALCsdl2BackendFactory_probe(ALCsdl2BackendFactory *self, enum DevProbe type, al_string *outnames);
static ALCbackend* ALCsdl2BackendFactory_createBackend(ALCsdl2BackendFactory *self, ALCdevice *device, ALCbackend_Type type);
DEFINE_ALCBACKENDFACTORY_VTABLE(ALCsdl2BackendFactory);


ALCbackendFactory *ALCsdl2BackendFactory_getFactory(void)
{
    static ALCsdl2BackendFactory factory = ALCsdl2BACKENDFACTORY_INITIALIZER;
    return STATIC_CAST(ALCbackendFactory, &factory);
}


static ALCboolean ALCsdl2BackendFactory_init(ALCsdl2BackendFactory* UNUSED(self))
{
    return AL_TRUE;
}

static void ALCsdl2BackendFactory_deinit(ALCsdl2BackendFactory* UNUSED(self))
{
}

static ALCboolean ALCsdl2BackendFactory_querySupport(ALCsdl2BackendFactory* UNUSED(self), ALCbackend_Type type)
{
    if(type == ALCbackend_Playback)
        return ALC_TRUE;
    return ALC_FALSE;
}

static void ALCsdl2BackendFactory_probe(ALCsdl2BackendFactory* UNUSED(self), enum DevProbe type, al_string *outnames)
{
    int num_devices, i;
    al_string name;

    if(type != ALL_DEVICE_PROBE)
        return;

    AL_STRING_INIT(name);
    num_devices = 1;

    alstr_append_range(outnames, defaultDeviceName, defaultDeviceName+sizeof(defaultDeviceName));
    for(i = 0;i < num_devices;++i)
    {
        alstr_copy_cstr(&name, DEVNAME_PREFIX);
        alstr_append_cstr(&name, defaultDeviceName);
        if(!alstr_empty(name))
            alstr_append_range(outnames, VECTOR_BEGIN(name), VECTOR_END(name)+1);
    }
    alstr_reset(&name);
}

static ALCbackend* ALCsdl2BackendFactory_createBackend(ALCsdl2BackendFactory* UNUSED(self), ALCdevice *device, ALCbackend_Type type)
{
    if(type == ALCbackend_Playback)
    {
        ALCsdl2Backend *backend;
        NEW_OBJ(backend, ALCsdl2Backend)(device);
        if(!backend) return NULL;
        return STATIC_CAST(ALCbackend, backend);
    }

    return NULL;
}
