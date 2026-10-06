#!/usr/bin/env bash

# Help message
show_help() {
echo "Usage: $(basename "$0") <directory>"
echo ""
echo "Description:"
echo "  Updates Bracken .report files in the specified directory by setting field 4 to D"
echo "  when field 5 is one of these kingdom/domain taxonomy IDs: 2157, 2, 2759, 10239."
echo "  Run this before kraken-biom to restore compatibility with newer Kraken/Bracken"
echo "  reports, where the expected domain rank code may no longer be present."
echo ""
echo "Options:"
echo "  -h, --help    Show this help message"
}

# Handle help
if [[ "$1" == "-h" || "$1" == "--help" ]]; then
show_help
exit 0
fi

# Input validation
if [[ -z "$1" || ! -d "$1" ]]; then
echo "Error: you must specify a valid directory"
echo "Use --help for more information"
exit 1
fi

# Process all report files in the target directory
for f in "$1"/*.report; do
awk 'BEGIN {OFS="\t"} {
if ($5=="2157" || $5=="2" || $5=="2759" || $5=="10239") $4="D";
print
}' "$f" > "$f.tmp" && mv "$f.tmp" "$f"
done