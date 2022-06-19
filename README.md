# VPK format provider for 7-Zip
Provides ability for 7-Zip to fully read and write (with some limitations) V1 and V2 VPK files
 
## Limitations
Creation of VPK files is limited to single VPK mode, because of 7-Zip's own multi-pack handling. If you want to write multi-pack VPKs use [my fork of 7-Zip](https://github.com/SCell555/7-Zip-zstd) with necessary functionality exposed.
 
## Screenshots

![Vpk Read](./screenshots/vpk_handler.png)

![Vpk Create](./screenshots/vpk_create.png)

![Vpk Info](./screenshots/vpk_info.png)