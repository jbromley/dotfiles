#!/usr/bin/env bash
set -euo pipefail

# qemu-vm.sh — minimal wrapper for qemu-system-x86_64
# Requirements: qemu-system-x86_64, qemu-img

usage() {
  cat <<'USAGE'
Usage:
  qemu-vm.sh -i ISO -d DISK_IMG [-s SIZE] [-c CPUS] [-m MEM] [--name NAME] [--uefi] [--no-kvm] [--extra ARGS]

Required:
  -i, --iso ISO            Path to installer ISO (e.g., overbot-custom.iso)
  -d, --disk DISK_IMG      Path to qcow2 disk image (created if missing)

Optional:
  -s, --size SIZE          Disk size (e.g., 20G). Used only when creating DISK_IMG.
  -c, --cpus CPUS          vCPU count (default: 4)
  -m, --mem MEM            Memory (default: 4G) e.g., 8192M, 4G
      --name NAME          VM name (QEMU window title)
      --uefi               Boot with OVMF UEFI firmware if available
      --no-kvm             Disable KVM (fallback to TCG)
      --extra ARGS         Extra args passed verbatim to QEMU (quote as one string)
  -h, --help               Show this help

Examples:
  qemu-vm.sh -i overbot-custom.iso -d vm-disk.qcow2 -s 20G -c 4 -m 4G --uefi
  qemu-vm.sh -i overbot-custom.iso -d vm-disk.qcow2 --extra "-nic user,hostfwd=tcp::2222-:22"
USAGE
}

# Defaults
CPUS=4
MEM=4G
DISK_SIZE=""
ISO=""
DISK_IMG=""
NAME=""
USE_UEFI=0
USE_KVM=1
EXTRA_ARGS=""

# Parse args
while [[ $# -gt 0 ]]; do
  case "$1" in
    -i|--iso) ISO="$2"; shift 2 ;;
    -d|--disk) DISK_IMG="$2"; shift 2 ;;
    -s|--size) DISK_SIZE="$2"; shift 2 ;;
    -c|--cpus) CPUS="$2"; shift 2 ;;
    -m|--mem) MEM="$2"; shift 2 ;;
    --name) NAME="$2"; shift 2 ;;
    --uefi) USE_UEFI=1; shift ;;
    --no-kvm) USE_KVM=0; shift ;;
    --extra) EXTRA_ARGS="$2"; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown arg: $1"; usage; exit 1 ;;
  esac
done

# Validate deps
command -v qemu-system-x86_64 >/dev/null || { echo "qemu-system-x86_64 not found"; exit 1; }
command -v qemu-img >/dev/null || { echo "qemu-img not found"; exit 1; }

# Validate required args
[[ -n "$ISO" ]] || { echo "Missing --iso"; usage; exit 1; }
[[ -f "$ISO" ]] || { echo "ISO not found: $ISO"; exit 1; }
[[ -n "$DISK_IMG" ]] || { echo "Missing --disk"; usage; exit 1; }

# Create disk if missing
if [[ ! -f "$DISK_IMG" ]]; then
  [[ -n "$DISK_SIZE" ]] || { echo "Disk $DISK_IMG does not exist. Provide --size (e.g., 20G) to create it."; exit 1; }
  echo "Creating disk $DISK_IMG ($DISK_SIZE)..."
  qemu-img create -f qcow2 "$DISK_IMG" "$DISK_SIZE" >/dev/null
fi

# Resolve UEFI firmware if requested
OVMF_CODE=""
if [[ $USE_UEFI -eq 1 ]]; then
  for p in \
    /usr/share/OVMF/OVMF_CODE.fd \
    /usr/share/ovmf/x64/OVMF_CODE.fd \
    /usr/share/edk2/ovmf/OVMF_CODE.fd \
    /usr/share/edk2/x64/OVMF_CODE.fd \
    /usr/share/qemu/OVMF_CODE.fd
  do
    [[ -f "$p" ]] && { OVMF_CODE="$p"; break; }
  done
  [[ -n "$OVMF_CODE" ]] || { echo "UEFI requested but OVMF_CODE.fd not found in common paths."; exit 1; }
fi

# KVM availability
KVM_ARGS=()
if [[ $USE_KVM -eq 1 ]] && [[ -e /dev/kvm ]]; then
  KVM_ARGS+=( -enable-kvm -cpu host )
else
  KVM_ARGS+=( -accel tcg )
fi

# Base QEMU args
ARGS=(
  -machine type=q35,accel=kvm:tcg
  -smp "$CPUS"
  -m "$MEM"
  -name "${NAME:-qemu-vm}"
  -display default,show-cursor=on
  -boot d
  -cdrom "$ISO"
  -drive file="$DISK_IMG",if=virtio,format=qcow2,cache=none,discard=unmap
  -device virtio-net-pci,netdev=n0
  -netdev user,id=n0
)

# UEFI
if [[ -n "$OVMF_CODE" ]]; then
  ARGS+=( -drive if=pflash,format=raw,readonly=on,file="$OVMF_CODE" )
fi

# Add KVM/CPU accel settings
ARGS+=( "${KVM_ARGS[@]}" )

# Append extra args if provided
if [[ -n "$EXTRA_ARGS" ]]; then
  # shellcheck disable=SC2206
  EXTRA_SPLIT=($EXTRA_ARGS)
  ARGS+=( "${EXTRA_SPLIT[@]}" )
fi

echo "Launching QEMU..."
exec qemu-system-x86_64 "${ARGS[@]}"

