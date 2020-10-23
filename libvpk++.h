#pragma once

#include <string>
#include <string_view>
#include <optional>
#include <cstdint>
#include <vector>
#include <memory>
#include <optional>
#include <algorithm>

#include "flat_map.hpp"
#include "IStream.h"
#include "StreamUtils.h"

namespace libvpk
{
    namespace helpers
    {
        std::string_view removeExtension( std::string_view string, std::string_view ending )
        {
            const size_t position = string.find( ending );

            return position != std::string_view::npos ? string.substr( 0, position ) : string;
        }

        std::string_view normalizePath( std::string_view path )
        {
            // Remove .vpk and _dir
            path = helpers::removeExtension( path, ".vpk" );
            path = helpers::removeExtension( path, "_dir" );

            return path;
        }

        template <typename T>
        T read( IInStream* stream )
        {
            T value;
            return ReadStream_FALSE( stream, &value, sizeof( T ) ) == S_OK ? value : T();
        }

        template <>
        std::string read<std::string>( IInStream* stream )
        {
            std::string value;
            char ch;
            UInt32 s;
            while ( stream->Read( &ch, sizeof( char ), &s ) == S_OK && s == sizeof( char ) && ch != '\0' )
                value += ch;
            return value;
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
            int32_t version   = 0;
            int32_t treeSize  = 0;
        };

        struct VPKHeader2 : public VPKHeader1
        {
            VPKHeader2() = default;
            VPKHeader2( const VPKHeader1& header ) : VPKHeader1( header ) {}

            int32_t fileDataSectionSize   = 0;
            int32_t archiveMD5SectionSize = 0;
            int32_t otherMD5SectionSize   = 0;
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

    using VPKFileMap = chobo::flat_map<std::string, VPKFileDesc>;

    class VPKSet : private helpers::NonCopyable
    {
    public:
        VPKSet() = default;

        void open( IInStream* stream, IArchiveOpenCallback* callback )
        {
            auto initialHeader = helpers::read<meta::VPKHeader1>( stream );
            if ( initialHeader.signature != meta::VPKHeader1::ValidSignature )
                throw std::exception( "Invalid VPK directory signature." );

            if ( initialHeader.version == 1 )
                m_header = initialHeader;
            else if ( initialHeader.version == 2 )
            {
                // Return to the beginning and read a full VPK 2 ptr.
                stream->Seek( 0, STREAM_SEEK_SET, nullptr );
                m_header = helpers::read<meta::VPKHeader2>( stream );
            }
            else
                throw std::exception( "Unknown VPK version." );

            const UInt64 total = m_header.treeSize;
            callback->SetTotal( nullptr, &total );
            parseDirectory( stream, callback );
        }

        const meta::VPKHeader& header() const
        {
            return m_header;
        }

        std::optional<VPKFileDesc> file( const std::string& path ) const
        {
            auto iter = m_files.find( path );
            if ( iter == m_files.end() )
                return std::nullopt;

            return iter->second;
        }

        const VPKFileMap& files() const
        {
            return m_files;
        }

        void clear()
        {
            m_header = {};
            m_files.clear();
        }

    private:

        void parseDirectory( IInStream* stream, IArchiveOpenCallback* callback )
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
                            stream->Seek( 0, STREAM_SEEK_CUR, &pos );
                            callback->SetCompleted( &files, &pos );
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

                        parseFile( stream, fullPath );
                    }
                }
            }
        }

        void parseFile( IInStream* stream, const std::string& vpkFilePath )
        {
            uint32_t crc = helpers::read<uint32_t>( stream );
            uint16_t preloadBytes = helpers::read<uint16_t>( stream );
            uint16_t archiveIndex = helpers::read<uint16_t>( stream );

            uint32_t offset = helpers::read<uint32_t>( stream );
            uint32_t length = helpers::read<uint32_t>( stream );

            // Read the terminator for the file info.
            helpers::read<int16_t>( stream );

            UInt64 pos;
            stream->Seek( 0, STREAM_SEEK_CUR, &pos );
            VPKFileDesc desc{ archiveIndex, preloadBytes, static_cast<uint32_t>( pos ), offset, length, crc };

            // Skip over the preload section
            if ( desc.preloadLength != 0 )
                stream->Seek( desc.preloadLength, STREAM_SEEK_CUR, nullptr );

            m_files.emplace( vpkFilePath, desc );
        }

        meta::VPKHeader m_header;
        VPKFileMap m_files;
    };
}