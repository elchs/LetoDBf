About LZ4 compression:

this is used alternatively to else used ZLIB compression.
Very! recommended, ultra-fast compression algorithm best suited for the task
to compress network traffic.

Not needed, but theoretically you can refresh LZ4 distro, 
by cloning an (when !) updated repository into this location with:
git clone https://github.com/Cyan4973/lz4 lz4

Note, that a very slighly patched version is used, see lz4.c.dif and lz4.h.dif,
but it shell work without that for LetoDBf.
Also note, that only the pure 'lib' directory of the distro is distributed,
without the bunch of test applications etc.
If you are additional interested in that, checkout given above link.