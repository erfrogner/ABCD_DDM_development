#!/usr/bin/env bash
set -euo pipefail

############################################
# paths
############################################
BASE_DIR=".../temp_abcd_data"         
EXTRACT_ROOT="$BASE_DIR/_extracted"          # temp extraction directory

DEST_BL=".../abcd_nback_bl_data/uncleaned"      # ses-00A
DEST_2Y=".../abcd_nback_2y_data/uncleaned"      # ses-02A
DEST_4Y=".../abcd_nback_4y_data/uncleaned"      # ses-04A
DEST_6Y=".../abcd_nback_6y_data/uncleaned"      # ses-06A


############################################
# parse args
############################################
DRY_RUN=false
if [[ "${1:-}" == "--dry-run" ]]; then
    DRY_RUN=true
    echo "🧪 Dry-run mode ON — no files will be extracted or copied."
fi

############################################
# check dests exist
############################################
mkdir -p "$EXTRACT_ROOT" \
         "$DEST_BL" "$DEST_2Y" "$DEST_4Y" "$DEST_6Y"
         

############################################
# run or print
############################################
run_cmd() {
    if $DRY_RUN; then
        echo "[DRY-RUN] $*"
    else
        eval "$@"
    fi
}

############################################
# loop over all .7z archives in BASE_DIR
############################################
for archive in "$BASE_DIR"/*.7z; do
    [ -e "$archive" ] || continue

    arch_name=$(basename "$archive")
    arch_noext="${arch_name%.7z}"
    extract_to="$EXTRACT_ROOT/$arch_noext"
    mkdir -p "$extract_to"

    echo "==> Processing $arch_name"

    if $DRY_RUN; then
        echo "[DRY-RUN] Would extract $archive to $extract_to"
    else
        7z x -y -o"$extract_to" "$archive"
    fi

    ########################################
    # walk extracted content
    ########################################
    find "$extract_to" -maxdepth 1 -type d -name "sub-*" | while read -r subdir; do
        subname=$(basename "$subdir")  

        find "$subdir" -maxdepth 1 -type d -name "ses-*" | while read -r sesdir; do
            sesname=$(basename "$sesdir")  
            funcdir="$sesdir/func"
            [ -d "$funcdir" ] || continue

            txtfile=$(ls "$funcdir"/*EventRelatedInformation*.txt 2>/dev/null | sort | head -n 1 || true)
            if [ -z "${txtfile:-}" ]; then
                echo "[WARN] No EventRelatedInformation txt in $funcdir"
                continue
            fi

            # determine destination
            case "$sesname" in
                ses-00A) dest="$DEST_BL" ;;
                ses-02A) dest="$DEST_2Y" ;;
                ses-04A) dest="$DEST_4Y" ;;
                ses-06A) dest="$DEST_6Y" ;;
                *)
                    echo "[WARN] Session $sesname not recognized, skipping"
                    continue
                    ;;
            esac

            mkdir -p "$dest"

            newname="${subname}_${sesname}_nback.csv"
            destpath="$dest/$newname"

            echo "Would move $txtfile -> $destpath"
            run_cmd "cp \"$txtfile\" \"$destpath\""
        done
    done
done

echo "Done. (Dry-run: $DRY_RUN)"
