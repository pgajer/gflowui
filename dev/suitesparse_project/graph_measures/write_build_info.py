"""Refresh report metadata immediately before a PDF build."""
from datetime import datetime
from pathlib import Path
from zoneinfo import ZoneInfo

stamp = datetime.now(ZoneInfo("America/New_York")).strftime("%Y-%m-%d %H:%M:%S %Z")
Path(__file__).with_name("graph_measures_build_info.tex").write_text(
    "\\renewcommand{\\reportbuilddatetime}{" + stamp + "}\n"
)
