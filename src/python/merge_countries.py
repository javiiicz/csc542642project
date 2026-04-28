import csv
from pathlib import Path

base = Path("/Users/somakshivlani/Documents/CSC642/Project")

map_path = base / "player_country_map.csv"
understat_path = base / "wc2026_outputs/understat_big5_player_season_2122_2526_full.csv"
out_path = base / "wc2026_outputs/understat_big5_player_season_2122_2526_full_merged_nations.csv"

# Build player -> nation lookup from your country map
name_to_nation = {}
with map_path.open(newline="", encoding="utf-8") as f:
    for row in csv.DictReader(f):
        player = (row.get("player") or "").strip()
        nation = (row.get("nation") or "").strip()
        if player and nation:
            name_to_nation[player] = nation

# Merge nation into the big understat file by exact player name
rows = []
matched = 0
unmatched = 0

with understat_path.open(newline="", encoding="utf-8") as f:
    reader = csv.DictReader(f)
    fieldnames = reader.fieldnames

    for row in reader:
        player = (row.get("player") or "").strip()
        mapped_nation = name_to_nation.get(player, "")

        if mapped_nation:
            row["nation"] = mapped_nation
            matched += 1
        else:
            # keep blank if not found
            row["nation"] = ""
            unmatched += 1

        rows.append(row)

with out_path.open("w", newline="", encoding="utf-8") as f:
    writer = csv.DictWriter(f, fieldnames=fieldnames)
    writer.writeheader()
    writer.writerows(rows)

print(f"output: {out_path}")
print(f"total_rows: {len(rows)}")
print(f"matched_from_map: {matched}")
print(f"still_blank: {unmatched}")