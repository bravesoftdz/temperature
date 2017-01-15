#!/bin/bash

./docker-build.sh && scripts/linux-test-with-qemu.sh
