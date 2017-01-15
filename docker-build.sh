bin/bash
set -x
rm -f i*.bin *.elf *.img *.o *.ppu
docker run --rm -v $(pwd):/workdir markfirmware/ufpc \
 -dTARGET_QEMUARM7A -B -Tultibo -O2 -Parm -CpARMV7A -WpQEMUVPB @/root/ultibo/core/fpc/bin/qemuvpb.cfg \
 ptemperature.lpr
docker run --rm -v $(pwd):/workdir markfirmware/ufpc \
 -dTARGET_RPI3 -B -Tultibo -O2 -Parm -CpARMV7A -WpRPI3B @/root/ultibo/core/fpc/bin/rpi3.cfg \
 ptemperature.lpr
