

#define INITGUID
#include "Alloc.h"
#include "ComTry.h"
#include "MyCom.h"
#include "IArchive.h"
#include "MyVector.h"
#include "CopyCoder.h"
#include "LimitedStreams.h"
#include "PropVariant.h"
#include "ProgressUtils.h"
#include "RegisterArc.h"
#include "StringConvert.h"
#include "libvpk++.h"
#include <cwchar>


class CHandler :
	public IInArchive, public IInArchiveGetStream, // reading
	public IOutArchive, public ISetProperties, // writing
	public CMyUnknownImp
{
	MY_UNKNOWN_IMP4( IInArchive, IInArchiveGetStream, IOutArchive, ISetProperties )
	INTERFACE_IInArchive( override )
	INTERFACE_IOutArchive( override )

	STDMETHOD( GetStream )( UInt32 index, ISequentialInStream** stream ) override;
	STDMETHOD( SetProperties )( const wchar_t* const* names, const PROPVARIANT* values, UInt32 numProps ) override;

private:
	libvpk::VPKSet vpk;
	CMyComPtr<IInStream> basePak;
	CObjectVector<CMyComPtr<IInStream>> paks;
	UInt64 size = 0;
	bool missingPak = false;
};

STDMETHODIMP CHandler::Open( IInStream* inStream, const UInt64* maxCheckStartPosition, IArchiveOpenCallback* callback ) MY_NO_THROW_DECL_ONLY
{
	try
	{
		vpk.open( inStream, callback );
		const auto& f = vpk.files().container();
		int largestId = -1;
		int preloadSize = 0;
		UInt64 s = 0;
		for ( size_t i = 0; i < f.size(); i++ )
		{
			const auto& file = f[i].second;
			if ( const auto arc = file.archiveIdx; arc != 0x7FFF && arc > largestId )
				largestId = arc;
			if ( const auto preload = file.preloadLength; preload > preloadSize )
				preloadSize = preload;
			s += file.fileLength;
		}

		size = s;

		if ( largestId != -1 )
		{
			CMyComPtr<IArchiveOpenVolumeCallback> volCallback;
			callback->QueryInterface( IID_IArchiveOpenVolumeCallback, (void**)&volCallback );
			if ( !volCallback )
				return S_OK;

			UString name;
			{
				NWindows::NCOM::CPropVariant prop;
				RINOK( volCallback->GetProperty( kpidName, &prop ) );
				if ( prop.vt != VT_BSTR )
					return S_OK;
				name = prop.bstrVal;
			}

			paks.ClearAndReserve( largestId + 1 );

			wchar_t base[MAX_PATH * 4];
			auto end = name.ReverseFind_Dot();
			if ( end > 3 && name[end - 1] == L'r' && name[end - 2] == L'i' && name[end - 3] == L'd' )
				name.DeleteFrom( end - 3 );

			for ( int i = 0; i < largestId + 1; i++ )
			{
				swprintf_s( base, L"%s%03d.vpk", name.GetBuf(), i );

				IInStream* s;
				if ( volCallback->GetStream( base, &s ) == S_FALSE || s == nullptr )
					missingPak = true;
				paks.Add( s );
			}
		}

		if ( preloadSize > 0 || largestId == -1 )
			basePak = inStream;

		return S_OK;
	}
	catch ( ... )
	{
		Close();
		return E_OUTOFMEMORY;
	}
}

STDMETHODIMP CHandler::Close() MY_NO_THROW_DECL_ONLY
{
	basePak.Release();
	paks.Clear();
	vpk.clear();
	size = 0;
	return S_OK;
}

STDMETHODIMP CHandler::GetNumberOfItems( UInt32* numItems ) MY_NO_THROW_DECL_ONLY
{
	*numItems = static_cast<UInt32>( vpk.files().size() );
	return S_OK;
}


static constexpr const Byte kProps[] =
{
	kpidPath,
	kpidSize,
	kpidOffset,
	kpidVolume
};

static constexpr const Byte kArcProps[] =
{
	kpidTotalPhySize,
	kpidNumVolumes
};

IMP_IInArchive_Props
IMP_IInArchive_ArcProps


STDMETHODIMP CHandler::GetArchiveProperty( PROPID propID, PROPVARIANT* value ) MY_NO_THROW_DECL_ONLY
{
	COM_TRY_BEGIN
		NWindows::NCOM::CPropVariant prop;
		switch ( propID )
		{
		case kpidTotalPhySize:
			{
				const auto& h = vpk.header();
				prop = size +
					( h.version == 1 ? sizeof( libvpk::meta::VPKHeader1 ) : sizeof( libvpk::meta::VPKHeader2 ) )
					+ h.treeSize + h.archiveMD5SectionSize + h.otherMD5SectionSize + h.signatureSectionSize;
				break;
			}

		case kpidNumVolumes:
			prop = static_cast<UInt32>( paks.Size() );
			break;
		}
		prop.Detach( value );
		return S_OK;
	COM_TRY_END
}

STDMETHODIMP CHandler::GetProperty( UInt32 index, PROPID propID, PROPVARIANT* value ) MY_NO_THROW_DECL_ONLY
{
	COM_TRY_BEGIN
		const auto& i = vpk.files().container().at( index );
		const auto& item = i.second;
		NWindows::NCOM::CPropVariant prop;
		switch ( propID )
		{
		case kpidPath:
			prop = MultiByteToUnicodeString( i.first.c_str(), CP_ACP );
			break;
		//case kpidIsDir:
		case kpidSize:
		case kpidPackSize:
			prop = static_cast<UInt64>( item.fileLength );
			break;

		case kpidCRC:
			prop = item.crc;
			break;

		case kpidVolume:
			if ( item.archiveIdx != 0x7FFF )
				prop = static_cast<UInt32>( item.archiveIdx );
			break;

		case kpidOffset:
			prop = static_cast<UInt32>( item.fileOffset );
			break;

		default:
			break;
		}
		prop.Detach( value );
		return S_OK;
	COM_TRY_END
}

STDMETHODIMP CHandler::Extract( const UInt32* indices, UInt32 numItems, Int32 testMode, IArchiveExtractCallback* extractCallback ) MY_NO_THROW_DECL_ONLY
{
	COM_TRY_BEGIN
		const auto& f = vpk.files().container();
		const bool allFilesMode = numItems == (UInt32)(Int32)-1;
		if ( allFilesMode )
			numItems = static_cast<UInt32>( f.size() );

		UInt64 totalSize = allFilesMode ? size : 0;
		if ( !allFilesMode )
		{
			for ( UInt32 i = 0; i < numItems; i++ )
				totalSize += f[indices[i]].second.fileLength;
		}

		extractCallback->SetTotal( totalSize );

		CLocalProgress* lps = new CLocalProgress;
		CMyComPtr<ICompressProgressInfo> progress = lps;
		lps->Init( extractCallback, false );

		NCompress::CCopyCoder* copyCoderSpec = new NCompress::CCopyCoder();
		CMyComPtr<ICompressCoder> copyCoder = copyCoderSpec;

		CLimitedSequentialOutStream* outStreamSpec = new CLimitedSequentialOutStream;
		CMyComPtr<ISequentialOutStream> outStream( outStreamSpec );

		UInt64 currentTotalSize = 0;
		for ( UInt32 i = 0; i < numItems; i++ )
		{
			lps->InSize = lps->OutSize = currentTotalSize;
			RINOK( lps->SetCur() );
			CMyComPtr<ISequentialOutStream> realOutStream;
			Int32 askMode = testMode ? NArchive::NExtract::NAskMode::kTest : NArchive::NExtract::NAskMode::kExtract;
			UInt32 index = allFilesMode ? i : indices[i];

			RINOK( extractCallback->GetStream( index, &realOutStream, askMode ) );

			const auto& item = f[index].second;

			currentTotalSize += item.fileLength;

			if ( !testMode && !realOutStream )
				continue;

			RINOK( extractCallback->PrepareOperation( askMode ) );
			outStreamSpec->SetStream( realOutStream );
			realOutStream.Release();
			outStreamSpec->Init( item.fileLength );
			Int32 opRes;
			CMyComPtr<ISequentialInStream> inStream;
			HRESULT res = GetStream( index, &inStream );
			if ( res == E_NOTIMPL )
				opRes = NArchive::NExtract::NOperationResult::kUnsupportedMethod;
			else if ( res != S_OK )
				opRes = NArchive::NExtract::NOperationResult::kDataError;
			else
			{
				RINOK( copyCoder->Code( inStream, outStream, NULL, NULL, progress ) );
				opRes = outStreamSpec->IsFinishedOK() ? NArchive::NExtract::NOperationResult::kOK : NArchive::NExtract::NOperationResult::kDataError;
			}
			outStreamSpec->ReleaseStream();
			RINOK( extractCallback->SetOperationResult( opRes ) );
		}
		return S_OK;
	COM_TRY_END
}

STDMETHODIMP CHandler::GetStream( UInt32 index, ISequentialInStream** stream )
{
	*stream = 0;
	const auto& i = vpk.files().container().at( index ).second;

	const auto& h = vpk.header();
	const auto offset = i.fileOffset + ( i.archiveIdx != 0x7FFF ? 0 : h.treeSize + ( h.version == 1 ? sizeof( libvpk::meta::VPKHeader1 ) : sizeof( libvpk::meta::VPKHeader ) ) );
	if ( i.preloadLength == 0 )
		return CreateLimitedInStream( i.archiveIdx == 0x7FFF ? basePak : paks[i.archiveIdx], offset, i.fileLength, stream );

	CLimitedCachedInStream* limitedStreamSpec = new CLimitedCachedInStream;
	CMyComPtr<IInStream> limitedStream = limitedStreamSpec;
	limitedStreamSpec->SetStream( i.archiveIdx == 0x7FFF ? basePak : paks[i.archiveIdx], offset );

	auto& preload = limitedStreamSpec->Buffer;
	preload.Alloc( i.preloadLength );

	size_t size = i.preloadLength;
	basePak->Seek( i.preloadOffset, STREAM_SEEK_SET, nullptr );
	ReadStream( basePak, preload, &size );

	limitedStreamSpec->SetCache( i.preloadLength, 0 );
	limitedStreamSpec->InitAndSeek( 0, i.fileLength );
	*stream = limitedStream.Detach();
	return S_OK;
}


STDMETHODIMP CHandler::GetFileTimeType( UInt32* type )
{
	*type = NFileTimeType::kUnix; // technically VPK doesn't support time information
	return S_OK;
}


#define CRC32_INIT_VALUE 0xFFFFFFFFUL
#define CRC32_XOR_VALUE  0xFFFFFFFFUL

typedef unsigned int CRC32_t;

#define NUM_BYTES 256
static constexpr const CRC32_t pulCRCTable[NUM_BYTES] =
{
	0x00000000, 0x77073096, 0xee0e612c, 0x990951ba,
	0x076dc419, 0x706af48f, 0xe963a535, 0x9e6495a3,
	0x0edb8832, 0x79dcb8a4, 0xe0d5e91e, 0x97d2d988,
	0x09b64c2b, 0x7eb17cbd, 0xe7b82d07, 0x90bf1d91,
	0x1db71064, 0x6ab020f2, 0xf3b97148, 0x84be41de,
	0x1adad47d, 0x6ddde4eb, 0xf4d4b551, 0x83d385c7,
	0x136c9856, 0x646ba8c0, 0xfd62f97a, 0x8a65c9ec,
	0x14015c4f, 0x63066cd9, 0xfa0f3d63, 0x8d080df5,
	0x3b6e20c8, 0x4c69105e, 0xd56041e4, 0xa2677172,
	0x3c03e4d1, 0x4b04d447, 0xd20d85fd, 0xa50ab56b,
	0x35b5a8fa, 0x42b2986c, 0xdbbbc9d6, 0xacbcf940,
	0x32d86ce3, 0x45df5c75, 0xdcd60dcf, 0xabd13d59,
	0x26d930ac, 0x51de003a, 0xc8d75180, 0xbfd06116,
	0x21b4f4b5, 0x56b3c423, 0xcfba9599, 0xb8bda50f,
	0x2802b89e, 0x5f058808, 0xc60cd9b2, 0xb10be924,
	0x2f6f7c87, 0x58684c11, 0xc1611dab, 0xb6662d3d,
	0x76dc4190, 0x01db7106, 0x98d220bc, 0xefd5102a,
	0x71b18589, 0x06b6b51f, 0x9fbfe4a5, 0xe8b8d433,
	0x7807c9a2, 0x0f00f934, 0x9609a88e, 0xe10e9818,
	0x7f6a0dbb, 0x086d3d2d, 0x91646c97, 0xe6635c01,
	0x6b6b51f4, 0x1c6c6162, 0x856530d8, 0xf262004e,
	0x6c0695ed, 0x1b01a57b, 0x8208f4c1, 0xf50fc457,
	0x65b0d9c6, 0x12b7e950, 0x8bbeb8ea, 0xfcb9887c,
	0x62dd1ddf, 0x15da2d49, 0x8cd37cf3, 0xfbd44c65,
	0x4db26158, 0x3ab551ce, 0xa3bc0074, 0xd4bb30e2,
	0x4adfa541, 0x3dd895d7, 0xa4d1c46d, 0xd3d6f4fb,
	0x4369e96a, 0x346ed9fc, 0xad678846, 0xda60b8d0,
	0x44042d73, 0x33031de5, 0xaa0a4c5f, 0xdd0d7cc9,
	0x5005713c, 0x270241aa, 0xbe0b1010, 0xc90c2086,
	0x5768b525, 0x206f85b3, 0xb966d409, 0xce61e49f,
	0x5edef90e, 0x29d9c998, 0xb0d09822, 0xc7d7a8b4,
	0x59b33d17, 0x2eb40d81, 0xb7bd5c3b, 0xc0ba6cad,
	0xedb88320, 0x9abfb3b6, 0x03b6e20c, 0x74b1d29a,
	0xead54739, 0x9dd277af, 0x04db2615, 0x73dc1683,
	0xe3630b12, 0x94643b84, 0x0d6d6a3e, 0x7a6a5aa8,
	0xe40ecf0b, 0x9309ff9d, 0x0a00ae27, 0x7d079eb1,
	0xf00f9344, 0x8708a3d2, 0x1e01f268, 0x6906c2fe,
	0xf762575d, 0x806567cb, 0x196c3671, 0x6e6b06e7,
	0xfed41b76, 0x89d32be0, 0x10da7a5a, 0x67dd4acc,
	0xf9b9df6f, 0x8ebeeff9, 0x17b7be43, 0x60b08ed5,
	0xd6d6a3e8, 0xa1d1937e, 0x38d8c2c4, 0x4fdff252,
	0xd1bb67f1, 0xa6bc5767, 0x3fb506dd, 0x48b2364b,
	0xd80d2bda, 0xaf0a1b4c, 0x36034af6, 0x41047a60,
	0xdf60efc3, 0xa867df55, 0x316e8eef, 0x4669be79,
	0xcb61b38c, 0xbc66831a, 0x256fd2a0, 0x5268e236,
	0xcc0c7795, 0xbb0b4703, 0x220216b9, 0x5505262f,
	0xc5ba3bbe, 0xb2bd0b28, 0x2bb45a92, 0x5cb36a04,
	0xc2d7ffa7, 0xb5d0cf31, 0x2cd99e8b, 0x5bdeae1d,
	0x9b64c2b0, 0xec63f226, 0x756aa39c, 0x026d930a,
	0x9c0906a9, 0xeb0e363f, 0x72076785, 0x05005713,
	0x95bf4a82, 0xe2b87a14, 0x7bb12bae, 0x0cb61b38,
	0x92d28e9b, 0xe5d5be0d, 0x7cdcefb7, 0x0bdbdf21,
	0x86d3d2d4, 0xf1d4e242, 0x68ddb3f8, 0x1fda836e,
	0x81be16cd, 0xf6b9265b, 0x6fb077e1, 0x18b74777,
	0x88085ae6, 0xff0f6a70, 0x66063bca, 0x11010b5c,
	0x8f659eff, 0xf862ae69, 0x616bffd3, 0x166ccf45,
	0xa00ae278, 0xd70dd2ee, 0x4e048354, 0x3903b3c2,
	0xa7672661, 0xd06016f7, 0x4969474d, 0x3e6e77db,
	0xaed16a4a, 0xd9d65adc, 0x40df0b66, 0x37d83bf0,
	0xa9bcae53, 0xdebb9ec5, 0x47b2cf7f, 0x30b5ffe9,
	0xbdbdf21c, 0xcabac28a, 0x53b39330, 0x24b4a3a6,
	0xbad03605, 0xcdd70693, 0x54de5729, 0x23d967bf,
	0xb3667a2e, 0xc4614ab8, 0x5d681b02, 0x2a6f2b94,
	0xb40bbe37, 0xc30c8ea1, 0x5a05df1b, 0x2d02ef8d
};

void CRC32_Init( CRC32_t& pulCRC )
{
	pulCRC = CRC32_INIT_VALUE;
}

void CRC32_Final( CRC32_t& pulCRC )
{
	pulCRC ^= CRC32_XOR_VALUE;
}

void CRC32_ProcessBuffer( CRC32_t& pulCRC, const void* pBuffer, int nBuffer )
{
	CRC32_t ulCrc = pulCRC;
	unsigned char* pb = (unsigned char*)pBuffer;
	unsigned int nFront;
	int nMain;

JustAfew:

	switch ( nBuffer )
	{
	case 7:
		ulCrc = pulCRCTable[*pb++ ^ (unsigned char)ulCrc] ^ ( ulCrc >> 8 );
		[[fallthrough]];

	case 6:
		ulCrc = pulCRCTable[*pb++ ^ (unsigned char)ulCrc] ^ ( ulCrc >> 8 );
		[[fallthrough]];

	case 5:
		ulCrc = pulCRCTable[*pb++ ^ (unsigned char)ulCrc] ^ ( ulCrc >> 8 );
		[[fallthrough]];

	case 4:
		ulCrc ^= *(CRC32_t*)pb;
		ulCrc = pulCRCTable[(unsigned char)ulCrc] ^ ( ulCrc >> 8 );
		ulCrc = pulCRCTable[(unsigned char)ulCrc] ^ ( ulCrc >> 8 );
		ulCrc = pulCRCTable[(unsigned char)ulCrc] ^ ( ulCrc >> 8 );
		ulCrc = pulCRCTable[(unsigned char)ulCrc] ^ ( ulCrc >> 8 );
		pulCRC = ulCrc;
		return;

	case 3:
		ulCrc = pulCRCTable[*pb++ ^ (unsigned char)ulCrc] ^ ( ulCrc >> 8 );
		[[fallthrough]];

	case 2:
		ulCrc = pulCRCTable[*pb++ ^ (unsigned char)ulCrc] ^ ( ulCrc >> 8 );
		[[fallthrough]];

	case 1:
		ulCrc = pulCRCTable[*pb++ ^ (unsigned char)ulCrc] ^ ( ulCrc >> 8 );
		[[fallthrough]];

	case 0:
		pulCRC = ulCrc;
		return;
	}

	// We may need to do some alignment work up front, and at the end, so that
	// the main loop is aligned and only has to worry about 8 byte at a time.
	//
	// The low-order two bits of pb and nBuffer in total control the
	// upfront work.
	//
	nFront = ( (uintptr_t)pb ) & 3;
	nBuffer -= nFront;
	switch ( nFront )
	{
	case 3:
		ulCrc = pulCRCTable[*pb++ ^ (unsigned char)ulCrc] ^ ( ulCrc >> 8 );
		[[fallthrough]];
	case 2:
		ulCrc = pulCRCTable[*pb++ ^ (unsigned char)ulCrc] ^ ( ulCrc >> 8 );
		[[fallthrough]];
	case 1:
		ulCrc = pulCRCTable[*pb++ ^ (unsigned char)ulCrc] ^ ( ulCrc >> 8 );
	}

	nMain = nBuffer >> 3;
	while ( nMain-- )
	{
		ulCrc ^= *(CRC32_t*)pb;
		ulCrc = pulCRCTable[(unsigned char)ulCrc] ^ ( ulCrc >> 8 );
		ulCrc = pulCRCTable[(unsigned char)ulCrc] ^ ( ulCrc >> 8 );
		ulCrc = pulCRCTable[(unsigned char)ulCrc] ^ ( ulCrc >> 8 );
		ulCrc = pulCRCTable[(unsigned char)ulCrc] ^ ( ulCrc >> 8 );
		ulCrc ^= *(CRC32_t*)( pb + 4 );
		ulCrc = pulCRCTable[(unsigned char)ulCrc] ^ ( ulCrc >> 8 );
		ulCrc = pulCRCTable[(unsigned char)ulCrc] ^ ( ulCrc >> 8 );
		ulCrc = pulCRCTable[(unsigned char)ulCrc] ^ ( ulCrc >> 8 );
		ulCrc = pulCRCTable[(unsigned char)ulCrc] ^ ( ulCrc >> 8 );
		pb += 8;
	}

	nBuffer &= 7;
	goto JustAfew;
}

static constexpr const size_t kCacheBlockSize = (1 << 20);
static constexpr const size_t kCacheSize = (kCacheBlockSize << 2);
static constexpr const size_t kCacheMask = (kCacheSize - 1);

class CCacheOutStream : public IOutStream, public CMyUnknownImp
{
	CMyComPtr<IOutStream> _stream;
	CMyComPtr<ISequentialOutStream> _seqStream;
	Byte* _cache;
	UInt64 _virtPos;
	UInt64 _virtSize;
	UInt64 _phyPos;
	UInt64 _phySize; // <= _virtSize
	UInt64 _cachedPos; // (_cachedPos + _cachedSize) <= _virtSize
	size_t _cachedSize;

	HRESULT MyWrite( size_t size );
	HRESULT MyWriteBlock()
	{
		return MyWrite( kCacheBlockSize - ( (size_t)_cachedPos & ( kCacheBlockSize - 1 ) ) );
	}
public:
	CCacheOutStream() : _cache( NULL ) {}
	~CCacheOutStream();
	bool Allocate();
	HRESULT Init( ISequentialOutStream* seqStream, IOutStream* stream );

	HRESULT FlushCache();
	MY_UNKNOWN_IMP

	STDMETHOD( Write )( const void* data, UInt32 size, UInt32* processedSize );
	STDMETHOD( Seek )( Int64 offset, UInt32 seekOrigin, UInt64* newPosition );
	STDMETHOD( SetSize )( UInt64 newSize );
};

bool CCacheOutStream::Allocate()
{
	if ( !_cache )
		_cache = ( Byte* )::MidAlloc( kCacheSize );
	return _cache != NULL;
}

HRESULT CCacheOutStream::Init( ISequentialOutStream* seqStream, IOutStream* stream )
{
	_virtPos = 0;
	_phyPos = 0;
	_virtSize = 0;
	_seqStream = seqStream;
	_stream = stream;
	if ( _stream )
	{
		RINOK( _stream->Seek( 0, STREAM_SEEK_CUR, &_virtPos ) );
		RINOK( _stream->Seek( 0, STREAM_SEEK_END, &_virtSize ) );
		RINOK( _stream->Seek( _virtPos, STREAM_SEEK_SET, &_virtPos ) );
	}
	_phyPos = _virtPos;
	_phySize = _virtSize;
	_cachedPos = 0;
	_cachedSize = 0;
	return S_OK;
}

HRESULT CCacheOutStream::MyWrite( size_t size )
{
	while ( size != 0 && _cachedSize != 0 )
	{
		if ( _phyPos != _cachedPos )
		{
			if ( !_stream )
				return E_FAIL;
			RINOK( _stream->Seek( _cachedPos, STREAM_SEEK_SET, &_phyPos ) );
		}
		size_t pos = (size_t)_cachedPos & kCacheMask;
		size_t curSize = MyMin( kCacheSize - pos, _cachedSize );
		curSize = MyMin( curSize, size );
		RINOK( WriteStream( _seqStream, _cache + pos, curSize ) );
		_phyPos += curSize;
		if ( _phySize < _phyPos )
			_phySize = _phyPos;
		_cachedPos += curSize;
		_cachedSize -= curSize;
		size -= curSize;
	}
	return S_OK;
}

HRESULT CCacheOutStream::FlushCache()
{
	return MyWrite( _cachedSize );
}

CCacheOutStream::~CCacheOutStream()
{
	FlushCache();
	if ( _stream )
	{
		if ( _virtSize != _phySize )
			_stream->SetSize( _virtSize );
		if ( _virtPos != _phyPos )
			_stream->Seek( _virtPos, STREAM_SEEK_SET, NULL );
	}
	::MidFree( _cache );
}

STDMETHODIMP CCacheOutStream::Write( const void* data, UInt32 size, UInt32* processedSize )
{
	if ( processedSize )
		*processedSize = 0;
	if ( size == 0 )
		return S_OK;

	UInt64 zerosStart = _virtPos;
	if ( _cachedSize != 0 )
	{
		if ( _virtPos < _cachedPos )
		{
			RINOK( FlushCache() );
		}
		else
		{
			UInt64 cachedEnd = _cachedPos + _cachedSize;
			if ( cachedEnd < _virtPos )
			{
				if ( cachedEnd < _phySize )
				{
					RINOK( FlushCache() );
				}
				else
					zerosStart = cachedEnd;
			}
		}
	}

	if ( _cachedSize == 0 && _phySize < _virtPos )
		_cachedPos = zerosStart = _phySize;

	if ( zerosStart != _virtPos )
	{
		// write zeros to [cachedEnd ... _virtPos)

		for ( ;;)
		{
			UInt64 cachedEnd = _cachedPos + _cachedSize;
			size_t endPos = (size_t)cachedEnd & kCacheMask;
			size_t curSize = kCacheSize - endPos;
			if ( curSize > _virtPos - cachedEnd )
				curSize = (size_t)( _virtPos - cachedEnd );
			if ( curSize == 0 )
				break;
			while ( curSize > ( kCacheSize - _cachedSize ) )
			{
				RINOK( MyWriteBlock() );
			}
			memset( _cache + endPos, 0, curSize );
			_cachedSize += curSize;
		}
	}

	if ( _cachedSize == 0 )
		_cachedPos = _virtPos;

	size_t pos = (size_t)_virtPos & kCacheMask;
	size = (UInt32)MyMin( (size_t)size, kCacheSize - pos );
	UInt64 cachedEnd = _cachedPos + _cachedSize;
	if ( _virtPos != cachedEnd ) // _virtPos < cachedEnd
		size = (UInt32)MyMin( (size_t)size, (size_t)( cachedEnd - _virtPos ) );
	else
	{
		// _virtPos == cachedEnd
		if ( _cachedSize == kCacheSize )
		{
			RINOK( MyWriteBlock() );
		}
		size_t startPos = (size_t)_cachedPos & kCacheMask;
		if ( startPos > pos )
			size = (UInt32)MyMin( (size_t)size, (size_t)( startPos - pos ) );
		_cachedSize += size;
	}
	memcpy( _cache + pos, data, size );
	if ( processedSize )
		*processedSize = size;
	_virtPos += size;
	if ( _virtSize < _virtPos )
		_virtSize = _virtPos;
	return S_OK;
}

STDMETHODIMP CCacheOutStream::Seek( Int64 offset, UInt32 seekOrigin, UInt64* newPosition )
{
	switch ( seekOrigin )
	{
	case STREAM_SEEK_SET: break;
	case STREAM_SEEK_CUR: offset += _virtPos; break;
	case STREAM_SEEK_END: offset += _virtSize; break;
	default: return STG_E_INVALIDFUNCTION;
	}
	if ( offset < 0 )
		return HRESULT_WIN32_ERROR_NEGATIVE_SEEK;
	_virtPos = offset;
	if ( newPosition )
		*newPosition = offset;
	return S_OK;
}

STDMETHODIMP CCacheOutStream::SetSize( UInt64 newSize )
{
	_virtSize = newSize;
	if ( newSize < _phySize )
	{
		if ( !_stream )
			return E_NOTIMPL;
		RINOK( _stream->SetSize( newSize ) );
		_phySize = newSize;
	}
	if ( newSize <= _cachedPos )
	{
		_cachedSize = 0;
		_cachedPos = newSize;
	}
	if ( newSize < _cachedPos + _cachedSize )
		_cachedSize = (size_t)( newSize - _cachedPos );
	return S_OK;
}

static constexpr std::string_view bannedExts[] =
{
	".bat", ".cmd", ".com", ".dll", ".exe", ".msi", ".rar", ".reg", ".zip", ".so"
};

class VpkWriter
{
public:
	VpkWriter() = default;

	HRESULT addItem( AString internalPath, UInt32 size, IInStream* stream )
	{
		internalPath.MakeLower_Ascii();
		const std::string path{ internalPath.Ptr(), internalPath.Len() };
		const auto spl = path.rfind( '/' );
		for ( size_t i = 0; i < ARRAYSIZE( bannedExts ); i++ )
		{
			if ( path.find( bannedExts[i] ) != std::string::npos )
				return E_FAIL;
		}

		auto& dir = resolvePath( root, spl == std::string::npos ? std::string{} : path.substr( 0, spl ), {} );
		const auto& name = path.substr( spl + 1 );

		dir.files.emplace( name, Dir::File{ size, stream } );
		return S_OK;
	}

	void write_pak( ISequentialOutStream* outStream, IArchiveUpdateCallback* callback )
	{
		CLocalProgress* lps = new CLocalProgress;
		CMyComPtr<ICompressProgressInfo> progress = lps;
		lps->Init( callback, true );

		NCompress::CCopyCoder* copyCoderSpec = new NCompress::CCopyCoder();
		CMyComPtr<ICompressCoder> copyCoder = copyCoderSpec;

		CMyComPtr<IOutStream> stream_;
		outStream->QueryInterface( IID_IOutStream, (void**)&stream_ );

		CCacheOutStream* stream = new CCacheOutStream();
		stream->Allocate();
		stream->Init( outStream, stream_ );

		UInt32 size = 0, treeSize = 0;
		FilesByExt sorted_files;
		sort_files( sorted_files, root, size, treeSize );

		for ( auto& [ext, dirs] : sorted_files.files )
			treeSize += static_cast<UInt32>( dirs.files.size() + 1 );
		++treeSize;

		write_header( stream, size, treeSize );

		UInt32 currentOffset = 0;
		for ( auto& [ext, dirs] : sorted_files.files )
		{
			write( stream, ext );
			for ( auto& [dir, files] : dirs.files )
			{
				write( stream, dir );
				for ( auto& [file, data] : files )
				{
					UInt64 pos;
					stream->Seek( 0, STREAM_SEEK_CUR, &pos );
					lps->InSize = lps->OutSize = pos;
					lps->SetCur();
					write( stream, file );
					write<UInt32>( stream, calc_crc( *data ) );
					write<UInt16>( stream, 0 );
					write<UInt16>( stream, 0x7FFF );
					write<UInt32>( stream, currentOffset );
					write<UInt32>( stream, data->size );
					write<UInt16>( stream, 0xFFFF );
					currentOffset += data->size;

				}
				write<std::string>( stream, {} );
			}
			write<std::string>( stream, {} );
		}
		write<std::string>( stream, {} );

		stream->Seek( treeSize + sizeof( libvpk::meta::VPKHeader ), STREAM_SEEK_SET, nullptr );

		for ( auto& [ext, dirs] : sorted_files.files )
		{
			for ( auto& [dir, files] : dirs.files )
			{
				for ( auto& [file, data] : files )
				{
					UInt64 pos;
					stream->Seek( 0, STREAM_SEEK_CUR, &pos );
					lps->InSize = lps->OutSize = pos;
					lps->SetCur();
					copyCoder->Code( data->stream, stream, NULL, NULL, progress );
					data->stream->Release();
				}
			}
		}

		stream->FlushCache();
		stream->Release();
	}

private:
	template <typename T>
	void write( IOutStream* stream, const T& data )
	{
		stream->Write( &data, sizeof( T ), nullptr );
	}

	template <>
	void write<std::string>( IOutStream* stream, const std::string& string )
	{
		stream->Write( string.c_str(), static_cast<UInt32>( string.size() + 1 ), nullptr );
	}

	void write_header( IOutStream* stream, UInt32 size, UInt32 treeSize )
	{
		libvpk::meta::VPKHeader header;
		header.signature = libvpk::meta::VPKHeader::ValidSignature;
		header.version = 2;
		header.treeSize = static_cast<Int32>( treeSize );
		header.fileDataSectionSize = static_cast<Int32>( size );

		stream->Write( &header, sizeof( header ), nullptr );
	}

	struct Dir
	{
		struct File
		{
			UInt32 size = 0;
			CMyComPtr<IInStream> stream;
		};

		std::string name;
		chobo::flat_map<std::string, Dir> folders;
		chobo::flat_map<std::string, File> files;
	};
	Dir root;

	struct FilesByExt
	{
		struct FilesByFolder
		{
			chobo::flat_map<std::string, std::vector<std::pair<std::string, Dir::File*>>> files;
		};

		chobo::flat_map<std::string, FilesByFolder> files;
	};

	static Dir& resolvePath( Dir& root, const std::string& path, const std::string& name )
	{
		if ( path.empty() )
			return root;

		const auto sep = path.find( '/' );
		auto res = root.folders.emplace( path.substr( 0, sep ), Dir{} );
		if ( res.second )
			res.first->second.name = name.empty() ? res.first->first : name + '/' + res.first->first;
		return resolvePath( res.first->second, sep == std::string::npos ? std::string{} : path.substr( sep + 1 ), res.first->second.name );
	}

	static void sort_files( FilesByExt& files, Dir& root, UInt32& size, UInt32& treeSize )
	{
		constexpr const auto vpkMetaSize = 3 * sizeof( Int32 ) + 3 * sizeof( Int16 );

		using namespace std::string_view_literals;
		for ( auto& [name, file] : root.files )
		{
			const auto sep = name.rfind( '.' );
			auto r = files.files.emplace( sep != std::string::npos ? name.substr( sep + 1 ) : " "sv, FilesByExt::FilesByFolder{} );
			auto r2 = r.first->second.files.emplace( root.name.empty() ? " "sv : root.name, std::vector<std::pair<std::string, Dir::File*>>{} );
			r2.first->second.emplace_back( name.substr( 0, sep ), &file );
			size += file.size;
			if ( r.second )
				treeSize += static_cast<UInt32>( r.first->first.size() + 1 );
			if ( r2.second )
				treeSize += static_cast<UInt32>( r2.first->first.size() + 1 );
			treeSize += static_cast<UInt32>( ( sep == std::string::npos ? name.size() : sep ) + 1 + vpkMetaSize );
		}

		for ( auto& folder : root.folders )
			sort_files( files, folder.second, size, treeSize );
	}

	CRC32_t calc_crc( Dir::File& file )
	{
		CRC32_t crc;
		CRC32_Init( crc );
		constexpr UInt32 bufSize = 512 * 1024;
		auto buf = new byte[bufSize];

		IInStream* stream = file.stream;
		UInt32 read;
		while ( stream->Read( buf, bufSize, &read ) == S_OK && read > 0 )
			CRC32_ProcessBuffer( crc, buf, read );
		CRC32_Final( crc );

		delete[] buf;
		stream->Seek( 0, STREAM_SEEK_SET, nullptr );

		return crc;
	}

};


static const wchar_t kOsPathSepar = WCHAR_PATH_SEPARATOR;
static const wchar_t kUnixPathSepar = L'/';

void ReplaceSlashes_OsToUnix( UString& name )
{
	name.Replace( kOsPathSepar, kUnixPathSepar );
}


STDMETHODIMP CHandler::UpdateItems( ISequentialOutStream* outStream, UInt32 numItems, IArchiveUpdateCallback* callback )
{
	COM_TRY_BEGIN
		if ( size != 0 || !vpk.files().empty() )
			return E_NOTIMPL;

		if ( !callback )
			return E_FAIL;

		VpkWriter writer;
		for ( UInt32 i = 0; i < numItems; i++ )
		{
			NWindows::NCOM::CPropVariant prop;
			RINOK( callback->GetProperty( i, kpidIsDir, &prop ) );
			if ( prop.vt != VT_BOOL )
				return E_INVALIDARG;
			if ( prop.boolVal != VARIANT_FALSE )
				continue; // skip folders
			prop.Clear();

			RINOK( callback->GetProperty( i, kpidPath, &prop ) );
			if ( prop.vt != VT_BSTR )
				return E_INVALIDARG;
			UString name = prop.bstrVal;
			ReplaceSlashes_OsToUnix( name );
			prop.Clear();

			RINOK( callback->GetProperty( i, kpidSize, &prop ) );
			if ( prop.vt != VT_UI8 )
				return E_INVALIDARG;
			UInt64 size = prop.uhVal.QuadPart;

			CMyComPtr<ISequentialInStream> fileInStream;
			callback->GetStream( i, &fileInStream );
			if ( !fileInStream )
				return E_FAIL;

			IInStream* inStream;
			fileInStream->QueryInterface( IID_IInStream, (void**)&inStream );
			if ( !inStream )
				return E_FAIL;

			RINOK( writer.addItem( UnicodeStringToMultiByte( name ), static_cast<UInt32>( size ), inStream ) );
		}

		writer.write_pak( outStream, callback );

		RINOK( callback->SetOperationResult( NArchive::NExtract::NOperationResult::kOK ) );

		return S_OK;
	COM_TRY_END
}

STDMETHODIMP CHandler::SetProperties( const wchar_t* const* names, const PROPVARIANT* values, UInt32 numProps )
{
	return S_OK;
}



static constexpr const Byte k_Signature[] = {
	8, 0x34, 0x12, 0xaa, 0x55, 0x1, 0x0, 0x0, 0x0,	// V1
	8, 0x34, 0x12, 0xaa, 0x55, 0x2, 0x0, 0x0, 0x0	// V2
};

API_FUNC_IsArc IsArc_Vpk( const Byte* p, size_t size )
{
	if ( size < 8 )
		return k_IsArc_Res_NEED_MORE;

	const auto ver = *reinterpret_cast<const UInt32*>( p + 4 );
	if ( ver == 0 || ver > 2 )
		return k_IsArc_Res_NO;

	return k_IsArc_Res_YES;
}

REGISTER_ARC_IO(
	"VPK", "vpk", 0, 1,
	k_Signature,
	0,
	NArcInfoFlags::kMultiSignature | NArcInfoFlags::kUseGlobalOffset,
	IsArc_Vpk
)