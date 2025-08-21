function nvrun --description 'run a program on the NVidia GPU'
    env __NV_PRIME_RENDER_OFFLOAD=1 __GLX_VENDOR_LIBRARY_NAME=nvidia $argv
end
