#!/bin/bash

#===============================================================================
# Title:         GCP Disk Mount Script
# Description:   This script automates the process of formatting and mounting
#                Google Cloud Platform disks, and provides functionality to
#                list available disks.
#
# Usage:         sudo ./script.sh <command> [arguments]
#                Commands:
#                  mount pd <disk_device> <mount_point> [--format] [--save] - Format and mount a persistent disk
#                  mount ssd <disk_device> <mount_point> [--format] [--save] - Format and mount a local SSD
#                  disks - List available disks that can be mounted
#
# Examples:      sudo ./script.sh mount pd /dev/sdb /mnt/data --format --save
#                sudo ./script.sh mount ssd /dev/nvme0n1 /mnt/disks/ssd --format
#                sudo ./script.sh disks
#
# Notes:         - Requires root privileges (sudo)
#                - Creates timestamped backup of /etc/fstab when mounting
#                - Uses UUID for persistent mounting
#                - Includes discard option for SSD optimization
#                - Checks if disk is already mounted before attempting to mount
#===============================================================================

# Exit immediately if a command exits with a non-zero status
set -e

# Function to display usage
usage() {
    echo "Usage: $0 <command> [arguments]"
    echo ""
    echo "Commands:"
    echo "  mount pd <disk_device> <mount_point> [--format] [--save] - Format and mount a persistent disk"
    echo "  mount ssd <disk_device> <mount_point> [--format] [--save] - Format and mount a local SSD"
    echo "  disks                                 - List available disks"
    echo ""
    echo "Examples:"
    echo "  $0 mount pd /dev/sdb /mnt/data --format --save"
    echo "  $0 mount ssd /dev/nvme0n1 /mnt/disks/ssd --format"
    echo "  $0 disks"
    exit 1
}

# Function to check if a disk is already mounted
check_if_mounted() {
    local disk="$1"
    local real_disk=$(readlink -f "$disk")
    
    # Check if disk is mounted
    if mount | grep -q "^$real_disk "; then
        return 0  # Disk is mounted
    fi
    
    # Check if any partition of the disk is mounted
    if lsblk -no NAME,MOUNTPOINT "$real_disk" | grep -v "^$(basename "$real_disk") " | grep -q "[^ ]\+"; then
        return 0  # A partition of the disk is mounted
    fi
    
    # Check if disk is in fstab
    local disk_uuid=$(sudo blkid -s UUID -o value "$real_disk" 2>/dev/null)
    if [ -n "$disk_uuid" ] && grep -q "UUID=$disk_uuid" /etc/fstab; then
        return 0  # Disk UUID is in fstab
    fi
    
    return 1  # Disk is not mounted
}

# Function to provide unmount instructions
provide_unmount_instructions() {
    local disk="$1"
    local real_disk=$(readlink -f "$disk")
    
    echo "Error: Disk $disk is already mounted or has mounted partitions."
    echo ""
    echo "Current mount information:"
    echo "-------------------------"
    mount | grep "^$real_disk" || true
    lsblk -no NAME,MOUNTPOINT "$real_disk" | grep -v "^$(basename "$real_disk") $" | grep "[^ ]\+" || true
    echo ""
    
    echo "To unmount this disk, you can use the following commands:"
    echo "--------------------------------------------------------"
    
    # Get mount points for the disk and its partitions
    local mount_points=$(mount | grep "^$real_disk" | awk '{print $3}')
    mount_points+=" $(lsblk -no MOUNTPOINT "$real_disk" | grep -v "^$")"
    
    # Print unmount commands for each mount point
    for mp in $mount_points; do
        if [ -n "$mp" ]; then
            echo "sudo umount $mp"
        fi
    done
    
    # Check if disk is in fstab
    local disk_uuid=$(sudo blkid -s UUID -o value "$real_disk" 2>/dev/null)
    if [ -n "$disk_uuid" ] && grep -q "UUID=$disk_uuid" /etc/fstab; then
        echo ""
        echo "The disk is also in /etc/fstab. To remove it:"
        echo "-------------------------------------------"
        echo "1. Make a backup of your fstab:"
        echo "   sudo cp /etc/fstab /etc/fstab.backup.$(date +%Y%m%d_%H%M%S)"
        echo ""
        echo "2. Remove the entry from fstab (this will show the line to remove):"
        echo "   grep \"UUID=$disk_uuid\" /etc/fstab"
        echo ""
        echo "3. Edit the file to remove the entry:"
        echo "   sudo nano /etc/fstab"
        echo "   or"
        echo "   sudo sed -i '/UUID=$disk_uuid/d' /etc/fstab"
    fi
    
    exit 1
}

# Function to mount a persistent disk
mount_persistent_disk() {
    local format_disk=false
    local save_fstab=false

    # Parse optional flags
    while [[ "$#" -gt 0 ]]; do
        case "$1" in
            --format)
                format_disk=true
                shift
                ;;
            --save)
                save_fstab=true
                shift
                ;;
            -*)
                echo "Unknown option: $1"
                usage
                ;;
            *)
                break # End of options
                ;;
        esac
    done

    # Check if correct number of arguments provided after flags
    if [ "$#" -ne 2 ]; then
        echo "Usage: $0 mount pd <disk_device> <mount_point> [--format] [--save]"
        echo "Example: $0 mount pd /dev/sdb /mnt/data --format --save"
        exit 1
    fi

    # Assign command line arguments to variables
    DISK="$1"
    MOUNT_POINT="$2"
    FSTAB="/etc/fstab"
    FSTAB_BACKUP="/etc/fstab.backup.$(date +%Y%m%d_%H%M%S)"

    # Check if disk exists
    if [ ! -b "$DISK" ]; then
        echo "Error: Disk $DISK not found"
        exit 1
    fi
    
    # Check if disk is already mounted
    if check_if_mounted "$DISK"; then
        provide_unmount_instructions "$DISK"
    fi

    # Check if mount point exists, if not create it
    if [ ! -d "$MOUNT_POINT" ]; then
        echo "Creating mount point directory $MOUNT_POINT"
        sudo mkdir -p "$MOUNT_POINT"
    fi
    
    # Check if mount point is already in use
    if mountpoint -q "$MOUNT_POINT"; then
        echo "Error: Mount point $MOUNT_POINT is already in use"
        echo ""
        echo "Current mount information:"
        mount | grep "$MOUNT_POINT"
        echo ""
        echo "To unmount this mount point, use:"
        echo "sudo umount $MOUNT_POINT"
        exit 1
    fi

    # Format the disk with ext4 filesystem if --format is provided
    if [ "$format_disk" = true ]; then
        echo "Formatting $DISK with ext4 filesystem..."
        sudo mkfs.ext4 -m 0 -E lazy_itable_init=0,lazy_journal_init=0,discard "$DISK"
    else
        echo "Skipping disk formatting as --format flag was not provided."
    fi

    # Get disk UUID
    UUID=$(sudo blkid -s UUID -o value "$DISK")

    # Add entry to fstab for persistent mounting if --save is provided
    if [ "$save_fstab" = true ]; then
        # Backup fstab
        echo "Creating backup of $FSTAB at $FSTAB_BACKUP..."
        sudo cp "$FSTAB" "$FSTAB_BACKUP"

        # Verify backup was created
        if [ ! -f "$FSTAB_BACKUP" ]; then
            echo "Error: Failed to create fstab backup"
            exit 1
        fi

        echo "Adding entry to $FSTAB..."
        echo "UUID=$UUID $MOUNT_POINT ext4 discard,defaults,nofail 0 2" | sudo tee -a "$FSTAB"
    else
        echo "Skipping fstab entry creation as --save flag was not provided."
    fi

    # Mount the disk
    echo "Mounting the disk..."
    sudo mount -o discard,defaults "$DISK" "$MOUNT_POINT"

    # Verify mount
    mount_result=$(`mount | grep -q "$MOUNT_POINT"`)
    if [[ -n "$mount_result" ]]; then
        echo "Successfully mounted $DISK to $MOUNT_POINT"
        if [ "$save_fstab" = true ]; then
            echo "A backup of your original fstab was created at $FSTAB_BACKUP"
        fi
        df -h "$MOUNT_POINT"
    else
        echo "Error: Failed to mount disk"
        # Restore fstab backup if mount fails and it was modified
        if [ "$save_fstab" = true ] && [ -f "$FSTAB_BACKUP" ]; then
            sudo cp "$FSTAB_BACKUP" "$FSTAB"
            echo "Restored original fstab from backup due to mount failure"
        fi
        exit 1
    fi
}

# Function to mount a local SSD
mount_local_ssd() {
    local format_disk=false
    local save_fstab=false

    # Parse optional flags
    while [[ "$#" -gt 0 ]]; do
        case "$1" in
            --format)
                format_disk=true
                shift
                ;;
            --save)
                save_fstab=true
                shift
                ;;
            -*)
                echo "Unknown option: $1"
                usage
                ;;
            *)
                break # End of options
                ;;
        esac
    done

    # Check if correct number of arguments provided after flags
    if [ "$#" -ne 2 ]; then
        echo "Usage: $0 mount ssd <disk_device> <mount_point> [--format] [--save]"
        echo "Example: $0 mount ssd /dev/nvme0n1 /mnt/disks/ssd --format --save"
        exit 1
    fi

    # Assign command line arguments to variables
    DISK="$1"
    MOUNT_POINT="$2"
    FSTAB="/etc/fstab"
    FSTAB_BACKUP="/etc/fstab.backup.$(date +%Y%m%d_%H%M%S)"

    # Check if disk exists
    if [ ! -b "$DISK" ]; then
        echo "Error: Disk $DISK not found"
        exit 1
    fi
    
    # Check if disk is already mounted
    if check_if_mounted "$DISK"; then
        provide_unmount_instructions "$DISK"
    fi

    # Check if mount point exists, if not create it
    if [ ! -d "$MOUNT_POINT" ]; then
        echo "Creating mount point directory $MOUNT_POINT"
        sudo mkdir -p "$MOUNT_POINT"
    fi
    
    # Check if mount point is already in use
    if mountpoint -q "$MOUNT_POINT"; then
        echo "Error: Mount point $MOUNT_POINT is already in use"
        echo ""
        echo "Current mount information:"
        mount | grep "$MOUNT_POINT"
        echo ""
        echo "To unmount this mount point, use:"
        echo "sudo umount $MOUNT_POINT"
        exit 1
    fi

    # Format the disk with ext4 filesystem (optimized for SSD) if --format is provided
    if [ "$format_disk" = true ]; then
        echo "Formatting $DISK with ext4 filesystem optimized for SSD..."
        sudo mkfs.ext4 -F "$DISK"
    else
        echo "Skipping disk formatting as --format flag was not provided."
    fi

    # Get disk UUID
    UUID=$(sudo blkid -s UUID -o value "$DISK")

    # Add entry to fstab for persistent mounting with SSD-specific options if --save is provided
    if [ "$save_fstab" = true ]; then
        # Backup fstab
        echo "Creating backup of $FSTAB at $FSTAB_BACKUP..."
        sudo cp "$FSTAB" "$FSTAB_BACKUP"

        # Verify backup was created
        if [ ! -f "$FSTAB_BACKUP" ]; then
            echo "Error: Failed to create fstab backup"
            exit 1
        fi

        echo "Adding entry to $FSTAB..."
        echo "UUID=$UUID $MOUNT_POINT ext4 discard,defaults,nofail 0 2" | sudo tee -a "$FSTAB"
    else
        echo "Skipping fstab entry creation as --save flag was not provided."
    fi

    # Mount the SSD
    echo "Mounting the SSD..."
    sudo mount "$DISK" "$MOUNT_POINT"

    # Configure read and write access to the device
    echo "Setting permissions..."
    sudo chmod a+w "$MOUNT_POINT"

    # Verify mount
    mount_result=$(`mount | grep -q "$MOUNT_POINT"`)
    if [[ -n  "$mount_result" ]]; then
        echo "Successfully mounted SSD $DISK to $MOUNT_POINT"
        if [ "$save_fstab" = true ]; then
            echo "A backup of your original fstab was created at $FSTAB_BACKUP"
        fi
        df -h "$MOUNT_POINT"
    else
        echo "Error: Failed to mount SSD"
        # Restore fstab backup if mount fails and it was modified
        if [ "$save_fstab" = true ] && [ -f "$FSTAB_BACKUP" ]; then
            sudo cp "$FSTAB_BACKUP" "$FSTAB"
            echo "Restored original fstab from backup due to mount failure"
        fi
        exit 1
    fi
}

# Function to list available disks
list_disks() {
    echo "=== Available Disks for Mounting ==="
    echo ""
    
    # Get list of all block devices
    echo "Block Devices (lsblk):"
    echo "----------------------"
    lsblk -o NAME,SIZE,TYPE,MOUNTPOINT,FSTYPE | grep -v "loop" | grep -v "rom"
    echo ""
    
    # Show unmounted disks (potential candidates for mounting)
    echo "Unmounted Disks (potential candidates for mounting):"
    echo "---------------------------------------------------"
    lsblk -o NAME,SIZE,TYPE,MOUNTPOINT | grep -v "loop" | grep -v "rom" | grep -v "/$" | grep -v "[[:space:]]/$" | grep -v "[[:space:]]/boot" | grep -v "[[:space:]]/home" | grep disk
    echo ""
    
    # Show detailed disk information using lsblk instead of fdisk
    echo "Detailed Disk Information:"
    echo "-------------------------"
    lsblk -o NAME,SIZE,TYPE,MOUNTPOINT,FSTYPE,MODEL,SERIAL,UUID -p | grep -v "loop" | grep -v "rom"
    echo ""
    
    # Check for NVMe devices using simple file checks (no nvme-cli required)
    echo "NVMe Devices (if any):"
    echo "---------------------"
    if [ -d "/dev" ] && ls /dev/nvme* &>/dev/null; then
        echo "Found NVMe devices:"
        for nvme_dev in /dev/nvme*; do
            if [[ -b "$nvme_dev" && ! "$nvme_dev" =~ "p[0-9]+" ]]; then
                echo "$nvme_dev ($(lsblk -dno SIZE "$nvme_dev" 2>/dev/null || echo "size unknown"))"
            fi
        done
    else
        echo "No NVMe devices found"
    fi
    echo ""
    
    echo "To mount a persistent disk, use: $0 mount pd <disk_device> <mount_point> [--format] [--save]"
    echo "Example: $0 mount pd /dev/sdb /mnt/data --format --save"
    echo ""
    echo "To mount a local SSD, use: $0 mount ssd <disk_device> <mount_point> [--format] [--save]"
    echo "Example: $0 mount ssd /dev/nvme0n1 /mnt/disks/ssd --format --save"
}

# Main script execution
if [ "$#" -lt 1 ]; then
    usage
fi

# Process commands
case "$1" in
    mount)
        if [ "$#" -lt 2 ]; then
            usage
        fi
        case "$2" in
            pd)
                shift 2
                mount_persistent_disk "$@"
                ;;
            ssd)
                shift 2
                mount_local_ssd "$@"
                ;;
            *)
                echo "Unknown mount type: $2"
                usage
                ;;
        esac
        ;;
    disks)
        list_disks
        ;;
    *)
        echo "Unknown command: $1"
        usage
        ;;
esac
