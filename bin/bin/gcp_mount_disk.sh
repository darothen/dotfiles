#!/bin/bash

#===============================================================================
# Title:         GCP Persistent Disk Mount Script
# Description:   This script automates the process of formatting and mounting
#                a new Google Cloud Platform Persistent Disk. It performs the
#                following operations:
#                - Formats the specified disk with ext4 filesystem
#                - Creates the mount point directory if it doesn't exist
#                - Creates a backup of /etc/fstab before modification
#                - Adds a UUID-based mount entry to /etc/fstab
#                - Mounts the disk and verifies the mount
#                - Restores fstab backup if mount fails
#
# Usage:         sudo ./script.sh <disk_device> <mount_point>
# Example:       sudo ./script.sh /dev/sdb /mnt/data
#
# Arguments:     disk_device - The device path of the disk (e.g., /dev/sdb)
#                mount_point - The directory where the disk should be mounted
#
# Finding Disk:  To find the correct disk_device value, you can use any of these
#                commands:
#                - 'lsblk' - Lists all block devices with their paths and sizes
#                - 'sudo fdisk -l' - Shows detailed disk information
#                - 'df -h' - Shows currently mounted disks
#                The new disk will typically be the unmounted device without
#                any partitions. Look for a disk with the expected size that
#                isn't mounted.
#
# Notes:         - Requires root privileges (sudo)
#                - Creates timestamped backup of /etc/fstab
#                - Uses UUID for persistent mounting
#                - Includes discard option for SSD optimization
#===============================================================================

# Exit immediately if a command exits with a non-zero status
set -e

# Function to display usage
usage() {
    echo "Usage: $0 <disk_device> <mount_point>"
    echo "Example: $0 /dev/sdb /mnt/data"
    exit 1
}

# Check if correct number of arguments provided
if [ "$#" -ne 2 ]; then
    usage
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

# Check if mount point exists, if not create it
if [ ! -d "$MOUNT_POINT" ]; then
    echo "Creating mount point directory $MOUNT_POINT"
    sudo mkdir -p "$MOUNT_POINT"
fi

# Format the disk with ext4 filesystem
echo "Formatting $DISK with ext4 filesystem..."
sudo mkfs.ext4 -m 0 -E lazy_itable_init=0,lazy_journal_init=0 "$DISK"

# Get disk UUID
UUID=$(sudo blkid -s UUID -o value "$DISK")

# Backup fstab
echo "Creating backup of $FSTAB at $FSTAB_BACKUP..."
sudo cp "$FSTAB" "$FSTAB_BACKUP"

# Verify backup was created
if [ ! -f "$FSTAB_BACKUP" ]; then
    echo "Error: Failed to create fstab backup"
    exit 1
fi

# Add entry to fstab for persistent mounting
echo "Adding entry to $FSTAB..."
echo "UUID=$UUID $MOUNT_POINT ext4 discard,defaults,nofail 0 2" | sudo tee -a "$FSTAB"

# Mount the disk
echo "Mounting the disk..."
sudo mount "$MOUNT_POINT"

# Verify mount
if mount | grep -q "$MOUNT_POINT"; then
    echo "Successfully mounted $DISK to $MOUNT_POINT"
    echo "A backup of your original fstab was created at $FSTAB_BACKUP"
    df -h "$MOUNT_POINT"
else
    echo "Error: Failed to mount disk"
    # Restore fstab backup if mount fails
    sudo cp "$FSTAB_BACKUP" "$FSTAB"
    echo "Restored original fstab from backup due to mount failure"
    exit 1
fi
