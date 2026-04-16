#!/usr/bin/env bash
set -euo pipefail

############################################
# paths
############################################
EXTRACT_ROOT=".../temp_abcd_data/_extracted"

DEST_BL=".../abcd_nback_bl_data/uncleaned"      
DEST_2Y=".../abcd_nback_2y_data/uncleaned"      
DEST_4Y=".../abcd_nback_4y_data/uncleaned"      
DEST_6Y=".../abcd_nback_6y_data/uncleaned"      

############################################
# parse args
############################################
DRY_RUN=false
if [[ "${1:-}" == "--dry-run" ]]; then
    DRY_RUN=true
    echo "🧪 Dry-run mode ON — no files will be moved."
fi

############################################
# make sure dests exist
############################################
mkdir -p "$DEST_BL" "$DEST_2Y" "$DEST_4Y" "$DEST_6Y"

run_cmd() {
    if $DRY_RUN; then
        echo "[DRY-RUN] $*"
    else
        eval "$@"
    fi
}

############################################
# find and move txt files
############################################
echo "==> Scanning extracted folders in $EXTRACT_ROOT ..."

find "$EXTRACT_ROOT" -type f -name "*EventRelatedInformation*.txt" | while read -r txtfile; do
    # extract subject ID and session from the path
    # path pattern example: .../sub-XXXX/ses-00A/func/file.txt
    subname=$(echo "$txtfile" | grep -oE "sub-[^/]+" | head -n 1)
    sesname=$(echo "$txtfile" | grep -oE "ses-[^/]+" | head -n 1)

    if [[ -z "$subname" || -z "$sesname" ]]; then
        echo "[WARN] Could not extract subject/session from path: $txtfile"
        continue
    fi

    case "$sesname" in
        ses-00A) dest="$DEST_BL" ;;
        ses-02A) dest="$DEST_2Y" ;;
        ses-04A) dest="$DEST_4Y" ;;
        ses-06A) dest="$DEST_6Y" ;;
        *)
            echo "[WARN] Unknown session ($sesname) in $txtfile, skipping"
            continue
            ;;
    esac

    mkdir -p "$dest"

    newname="${subname}_${sesname}_nback.csv"
    destpath="$dest/$newname"

    echo "Moving $txtfile -> $destpath"
    run_cmd "cp \"$txtfile\" \"$destpath\""
done

echo "✅ Done. (Dry-run: $DRY_RUN)"
