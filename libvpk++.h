#pragma once

#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

#include "tuple.hpp"
#include "IStream.h"
#include "StreamUtils.h"

namespace libvpk
{
	namespace helpers
	{
		template <typename T>
		T read( IInStream* stream )
		{
			T value;
			return ReadStream_FALSE( stream, &value, sizeof( T ) ) == S_OK ? value : T();
		}

		template <>
		std::string read<std::string>( IInStream* stream )
		{
			std::vector<char> value;
			char ch;
			UInt32 s;
			while ( stream->Read( &ch, sizeof( char ), &s ) == S_OK && s == sizeof( char ) && ch != '\0' )
				value.emplace_back( ch );
			return { value.cbegin(), value.cend() };
		}

		class NonCopyable
		{
		protected:
			NonCopyable() = default;
			~NonCopyable() = default;

			NonCopyable( const NonCopyable& ) = delete;
			NonCopyable& operator=( const NonCopyable& ) = delete;
		};
	}

	namespace meta
	{
		struct VPKHeader1
		{
			static constexpr uint32_t ValidSignature = 0x55aa1234;

			int32_t signature = 0;
			int32_t version	  = 0;
			int32_t treeSize  = 0;
		};

		struct VPKHeader2 : public VPKHeader1
		{
			VPKHeader2() = default;
			VPKHeader2( const VPKHeader1& header ) : VPKHeader1( header ) {}

			int32_t fileDataSectionSize	  = 0;
			int32_t archiveMD5SectionSize = 0;
			int32_t otherMD5SectionSize	  = 0;
			int32_t signatureSectionSize  = 0;
		};

		using VPKHeader = VPKHeader2;
	}

	struct VPKFileDesc
	{
		uint16_t archiveIdx;

		uint16_t preloadLength;
		uint32_t preloadOffset;

		uint32_t fileOffset;
		uint32_t fileLength;

		uint32_t crc;
	};

	using VPKFileList = std::vector<tuplet::pair<std::string, VPKFileDesc>>;

	class VPKSet : private helpers::NonCopyable
	{
	public:
		VPKSet() = default;

		HRESULT open( IInStream* stream, IArchiveOpenCallback* callback )
		{
			UInt64 pos;
			RINOK( stream->Seek( 0, STREAM_SEEK_CUR, &pos ) );
			auto initialHeader = helpers::read<meta::VPKHeader1>( stream );
			if ( initialHeader.signature != meta::VPKHeader1::ValidSignature )
				return S_FALSE;

			if ( initialHeader.version == 1 )
				m_header = initialHeader;
			else if ( initialHeader.version == 2 )
			{
				// Return to the beginning and read a full VPK 2 ptr.
				RINOK( stream->Seek( pos, STREAM_SEEK_SET, nullptr ) );
				m_header = helpers::read<meta::VPKHeader2>( stream );
				if ( m_header.signature != meta::VPKHeader1::ValidSignature )
					return S_FALSE;
			}
			else
				return S_FALSE;

			RINOK( stream->Seek( pos, STREAM_SEEK_SET, nullptr ) );
			CMyComPtr<CLimitedCachedInStream> bufStream = new CLimitedCachedInStream();
			const auto size = m_header.treeSize + ( initialHeader.version == 2 ? sizeof( meta::VPKHeader2 ) : sizeof( meta::VPKHeader1 ) );
			bufStream->Buffer.Alloc( size );
			RINOK( ReadStream_FALSE( stream, bufStream->Buffer, size ) );
			bufStream->SetStream( stream, 0 );
			bufStream->SetCache( size, 0 );
			bufStream->InitAndSeek( 0, size );
			RINOK( bufStream->Seek( initialHeader.version == 2 ? sizeof( meta::VPKHeader2 ) : sizeof( meta::VPKHeader1 ), STREAM_SEEK_SET, nullptr ) );

			const UInt64 total = m_header.treeSize;
			callback->SetTotal( nullptr, &total );
			return parseDirectory( bufStream, callback );
		}

		const meta::VPKHeader& header() const
		{
			return m_header;
		}

		const VPKFileList& files() const
		{
			return m_files;
		}

		void clear()
		{
			m_header = {};
			m_files.clear();
		}

	private:

		HRESULT parseDirectory( IInStream* stream, IArchiveOpenCallback* callback )
		{
			for ( ;;)
			{
				auto extension = helpers::read<std::string>( stream );
				if ( extension.empty() )
					break;

				for ( ;;)
				{
					auto path = helpers::read<std::string>( stream );
					if ( path.empty() )
						break;

					for ( ;;)
					{
						{
							UInt64 pos;
							const UInt64 files = m_files.size();
							RINOK( stream->Seek( 0, STREAM_SEEK_CUR, &pos ) );
							RINOK( callback->SetCompleted( &files, &pos ) );
						}

						auto name = helpers::read<std::string>( stream );
						if ( name.empty() )
							break;

						using namespace std::string_view_literals;

						std::string fullPath;
						if ( path != " "sv )
							fullPath = path + '/';
						fullPath += name;
						if ( extension != " "sv )
							fullPath += '.' + extension;

						RINOK( parseFile( stream, std::move( fullPath ) ) );
					}
				}
			}

			return S_OK;
		}

		HRESULT parseFile( IInStream* stream, std::string vpkFilePath )
		{
			uint32_t crc = helpers::read<uint32_t>( stream );
			uint16_t preloadBytes = helpers::read<uint16_t>( stream );
			uint16_t archiveIndex = helpers::read<uint16_t>( stream );

			uint32_t offset = helpers::read<uint32_t>( stream );
			uint32_t length = helpers::read<uint32_t>( stream );

			// Read the terminator for the file info.
			if ( helpers::read<uint16_t>( stream ) != 0xFFFF )
				return S_FALSE;

			UInt64 pos;
			RINOK( stream->Seek( 0, STREAM_SEEK_CUR, &pos ) );

			VPKFileDesc desc{ archiveIndex, preloadBytes, static_cast<uint32_t>( pos ), offset, length, crc };

			// Skip over the preload section
			if ( desc.preloadLength != 0 )
			{
				RINOK( stream->Seek( desc.preloadLength, STREAM_SEEK_CUR, nullptr ) );
				desc.fileLength += desc.preloadLength;
			}

			m_files.emplace_back().assign( std::move( vpkFilePath ), desc );
			return S_OK;
		}

		meta::VPKHeader m_header;
		VPKFileList m_files;
	};
}