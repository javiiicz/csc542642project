#!/usr/bin/env python3
"""CLI: merge Understat merged-intl CSV with Transfermarkt active players.

Matched rows get `nation` from TM `country_of_citizenship`; originals are in `nation_understat`.
"""

from pathlib import Path

import pandas as pd

from transfermarkt_match import merge_understat_with_transfermarkt, unmatched_pairs

BASE = Path(__file__).resolve().parent
DEFAULT_U = BASE / "wc2026_outputs/understat_big5_player_season_2122_2526_full_merged_intl.csv"
DEFAULT_TM = BASE / "transfermarkt_data/active_players_2025.csv"
OUT = BASE / "wc2026_outputs/understat_big5_player_season_2122_2526_full_merged_intl_tm.csv"
UNMATCHED = BASE / "wc2026_outputs/understat_tm_unmatched_pairs.csv"


def main() -> None:
    u = pd.read_csv(DEFAULT_U)
    tm = pd.read_csv(DEFAULT_TM)
    merged, report = merge_understat_with_transfermarkt(u, tm)
    merged.to_csv(OUT, index=False)
    print("Saved:", OUT)
    print(report)
    um = unmatched_pairs(merged)
    um.to_csv(UNMATCHED, index=False)
    print("Unmatched pairs:", len(um), "->", UNMATCHED)


if __name__ == "__main__":
    main()
