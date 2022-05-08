boot.bin: pcpi_bootrom.asm
	z80asm -o boot.rom pcpi_bootrom.asm
	srec_cat boot.rom -binary -fill 0xFF 0x0 0x800 -crop 0x0 0x7ff -checksum_positive_big_endian 0x7ff 1 1 -o boot.bin -binary
	cp boot.bin applicard-v9.bin
