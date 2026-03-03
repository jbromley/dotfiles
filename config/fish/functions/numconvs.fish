function htod --description "Convert hexadecimal number to decimal"
    if test (count $argv) -ne 1
        echo "Usage: htod HEX_VALUE" >&2
        return 1
    end

    set -l input (string upper $argv[1])

    if not string match -rq '^[0-9A-F]+$' -- $input
        echo "htod: invalid hexadecimal value: $argv[1]" >&2
        return 1
    end

    echo "ibase=16; $input" | bc
end

function dtoh --description "Convert decimal number to hexadecimal"
    if test (count $argv) -ne 1
        echo "Usage: dtoh DECIMAL_VALUE" >&2
        return 1
    end

    if not string match -rq '^[0-9]+$' -- $argv[1]
        echo "dtoh: invalid decimal value: $argv[1]" >&2
        return 1
    end

    echo "obase=16; $argv[1]" | bc
end

function btod --description "Convert binary number to decimal"
    if test (count $argv) -ne 1
        echo "Usage: btod BINARY_VALUE" >&2
        return 1
    end

    set -l input (string upper $argv[1])

    if not string match -rq '^[01]+$' -- $input
        echo "btod: invalid binary value: $argv[1]" >&2
        return 1
    end

    echo "ibase=2; $input" | bc
end

function dtob --description "Convert decimal number to binary"
    if test (count $argv) -ne 1
        echo "Usage: dtob DECIMAL_VALUE" >&2
        return 1
    end

    if not string match -rq '^[0-9]+$' -- $argv[1]
        echo "dtob: invalid decimal value: $argv[1]" >&2
        return 1
    end

    echo "obase=2; $argv[1]" | bc
end

function htob --description "Convert hexadecimal number to binary"
    if test (count $argv) -ne 1
        echo "Usage: htob HEX_VALUE" >&2
        return 1
    end

    set -l input (string upper $argv[1])

    if not string match -rq '^[0-9A-F]+$' -- $input
        echo "htob: invalid hexadecimal value: $argv[1]" >&2
        return 1
    end

    echo "obase=2; ibase=16; $input" | bc
end

function btoh --description "Convert binary number to hexadecimal"
    if test (count $argv) -ne 1
        echo "Usage: btoh BINARY_VALUE" >&2
        return 1
    end

    if not string match -rq '^[01]+$' -- $argv[1]
        echo "btoh: invalid decimal value: $argv[1]" >&2
        return 1
    end

    echo "obase=16; ibase=2; $argv[1]" | bc
end
