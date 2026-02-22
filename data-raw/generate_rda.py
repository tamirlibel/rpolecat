"""Generate .rda files for rpolecat package data using rdata package."""
import numpy as np
import pandas as pd
import rdata
import os

data_dir = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "data")

# --- quad_categories ---
quad_categories = pd.DataFrame({
    "event_type": [
        "agree", "consult", "support", "concede",
        "cooperate", "aid", "retreat",
        "request", "accuse", "reject", "threaten",
        "protest", "sanction", "mobilize", "coerce", "assault"
    ],
    "quad_category": (
        ["Verbal Cooperation"] * 4 +
        ["Material Cooperation"] * 3 +
        ["Verbal Conflict"] * 4 +
        ["Material Conflict"] * 5
    ),
    "goldstein": [
        3.0, 2.0, 2.5, 1.0,
        5.0, 7.0, 4.0,
        -1.0, -2.0, -3.0, -5.0,
        -6.0, -5.5, -7.0, -8.0, -10.0
    ]
})

rdata.write_rda(
    os.path.join(data_dir, "quad_categories.rda"),
    {"quad_categories": quad_categories},
    compression="gzip"
)
print(f"Wrote quad_categories.rda ({len(quad_categories)} rows)")

# --- polecat_sample ---
np.random.seed(42)
n = 500

event_types = [
    "agree", "consult", "support", "concede", "cooperate", "aid",
    "retreat", "request", "accuse", "reject", "threaten",
    "protest", "sanction", "mobilize", "coerce", "assault"
]
type_weights = np.array([
    0.05, 0.08, 0.06, 0.03, 0.07, 0.05,
    0.02, 0.08, 0.10, 0.06, 0.08,
    0.10, 0.04, 0.05, 0.06, 0.07
])
type_weights = type_weights / type_weights.sum()

countries = [
    "USA", "GBR", "FRA", "DEU", "RUS", "CHN", "IND", "BRA",
    "JPN", "KOR", "IRN", "ISR", "SAU", "UKR", "TUR", "EGY",
    "NGA", "ZAF", "MEX", "AUS"
]
actors = [
    "Government", "Military", "Police", "Citizen", "Media",
    "Opposition", "Rebel", "NGO", "IGO", "Business"
]
contexts_list = [
    "military", "intelligence", "executive", "legislative",
    "election", "economic", "legal", "human_rights", "diplomatic",
    "territory", "health", "migration", "environment",
    "terrorism", "cyber", "crime", "corruption"
]

mode_map = {
    "consult": ["visit", "third-party", "multilateral", "phone"],
    "retreat": ["withdraw", "release", "return", "ceasefire"],
    "request": ["assist", "change", "yield", "meet"],
    "accuse": ["disapprove", "investigate", "allege"],
    "reject": ["assist", "change", "yield", "meet"],
    "threaten": ["restrict", "ban", "arrest", "violence"],
    "protest": ["demo", "riot", "strike", "boycott"],
    "sanction": ["convict", "expel", "withdraw", "discontinue"],
    "mobilize": ["troops", "weapons", "police", "militia"],
    "coerce": ["seize", "restrict", "ban", "arrest", "censor"],
    "assault": ["beat", "firearms", "explosives", "aerial", "drone"],
}

country_coords = {
    "USA": (38.0, -97.0), "GBR": (51.5, -0.1), "FRA": (46.2, 2.2),
    "DEU": (51.2, 10.4), "RUS": (55.8, 37.6), "CHN": (35.9, 104.2),
    "IND": (20.6, 79.0), "BRA": (-14.2, -51.9), "JPN": (36.2, 138.3),
    "KOR": (35.9, 127.8), "IRN": (32.4, 53.7), "ISR": (31.0, 34.8),
    "SAU": (23.9, 45.1), "UKR": (48.4, 31.2), "TUR": (39.9, 32.9),
    "EGY": (26.8, 30.8), "NGA": (9.1, 8.7), "ZAF": (-30.6, 22.9),
    "MEX": (23.6, -102.6), "AUS": (-25.3, 133.8),
}

sampled_types = list(np.random.choice(event_types, n, p=type_weights))

start_date = pd.Timestamp("2023-01-01")
end_date = pd.Timestamp("2023-12-31")
days_range = (end_date - start_date).days
dates = sorted([
    start_date + pd.Timedelta(days=int(d))
    for d in np.random.choice(days_range + 1, n, replace=True)
])

src_countries = list(np.random.choice(countries, n))
tgt_countries = list(np.random.choice(countries, n))
intra = np.random.choice([True, False], n, p=[0.3, 0.7])
for i in range(n):
    if intra[i]:
        tgt_countries[i] = src_countries[i]

src_actors = list(np.random.choice(actors, n))
tgt_actors = list(np.random.choice(actors, n))

sampled_modes = []
for et in sampled_types:
    if et in mode_map:
        sampled_modes.append(str(np.random.choice(mode_map[et])))
    else:
        sampled_modes.append("")

sampled_contexts = []
for _ in range(n):
    nc = int(np.random.choice([1, 2]))
    sampled_contexts.append(";".join(np.random.choice(contexts_list, nc, replace=False)))

has_geo = np.random.choice([True, False], n, p=[0.8, 0.2])
lats = [np.nan] * n
lons = [np.nan] * n
for i in range(n):
    if has_geo[i]:
        cc = country_coords[src_countries[i]]
        lats[i] = float(cc[0] + np.random.normal(0, 2))
        lons[i] = float(cc[1] + np.random.normal(0, 2))

urls = [f"https://example.com/article/{i+1:06d}" for i in range(n)]

polecat_sample = pd.DataFrame({
    "event_id": [f"EVT-2023-{i+1:06d}" for i in range(n)],
    "story_date": [d.strftime("%Y-%m-%d") for d in dates],
    "source": [f"{sc}/{sa}" for sc, sa in zip(src_countries, src_actors)],
    "source_actor": src_actors,
    "source_agent": [""] * n,
    "source_country": src_countries,
    "target": [f"{tc}/{ta}" for tc, ta in zip(tgt_countries, tgt_actors)],
    "target_actor": tgt_actors,
    "target_agent": [""] * n,
    "target_country": tgt_countries,
    "event_type": sampled_types,
    "mode": sampled_modes,
    "context": sampled_contexts,
    "latitude": lats,
    "longitude": lons,
    "goldstein": [round(float(x), 1) for x in np.random.uniform(-10, 10, n)],
    "url": urls,
})

# Add 20 duplicates
dup_idx = np.random.choice(n, 20, replace=False)
dups = polecat_sample.iloc[dup_idx].copy()
dups = dups.reset_index(drop=True)
dups["event_id"] = [f"EVT-2023-D{i+1:05d}" for i in range(20)]
dups["url"] = [f"https://example.com/article/dup-{i+1:05d}" for i in range(20)]
polecat_sample = pd.concat([polecat_sample, dups], ignore_index=True)

rdata.write_rda(
    os.path.join(data_dir, "polecat_sample.rda"),
    {"polecat_sample": polecat_sample},
    compression="gzip"
)
print(f"Wrote polecat_sample.rda ({len(polecat_sample)} rows)")

# Clean up temp files
for f in ["quad_categories_tmp.rds", "polecat_sample_tmp.rds"]:
    p = os.path.join(data_dir, f)
    if os.path.exists(p):
        os.remove(p)
        print(f"Removed {f}")
