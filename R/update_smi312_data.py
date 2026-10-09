"""
Replace the SMI312 SSC and SMI312 cingulate gyrus rows in
HU_IR_Sex_LongFormat_FROM_PRISM.xlsx with re-quantified values supplied
2026-07-20, and rebuild the n_per_cell_summary tab.

Notes on the incoming data:
  * It carries per-animal sample IDs, which the original Prism export did not.
    A Sample_ID column is added to Long_Format_Data (populated for these two
    outcomes, blank for every other outcome).
  * It contains an explicit NL group. Per the convention already established
    for this dataset (see the Instructions tab), NL is treated as equivalent
    to Sham, i.e. HU = "Sham", IR = 0. The pre-existing Sham cell counts
    (Females 8 / Males 9) equal sham + NL in the new data, confirming the
    original Prism tables were built the same way.
  * "#DIV/0!" marks an unmeasurable/missing image and is dropped entirely.
"""

import re
import shutil
from pathlib import Path

import pandas as pd

BASE = Path(__file__).resolve().parent.parent
XLSX = BASE / "HU_IR_Sex_LongFormat_FROM_PRISM.xlsx"

RAW = {
    "SMI312 SSC": """
353 F sham 1.2873
354 F sham #DIV/0!
351 F 50cGy 14.7397
352 F 50cGy 6.453
357 F 50cGy 13.57555
358 F 50cGy #DIV/0!
349 F 100cGy 4.5107
350 F 100cGy 7.9908
355 F 100cGy 11.3666
356 F 100cGy 6.8975
363 M sham 2.07745
364 M sham #DIV/0!
361 M 50cGy 13.937725
362 M 50cGy 8.5119
365 M 50cGy 1.672433333
366 M 50cGy 5.3325
359 M 100cGy 8.9553
360 M 100cGy 6.7176
367 M 100cGy 3.5234
368 M 100cGy 11.0462
369 F NL #DIV/0!
370 F NL 3.0765
371 F NL 1.5272
372 F NL 9.4812
373 F HU 6.0924
374 F HU 7.264325
375 F HU 5.9791
376 F HU 7.02275
377 F HU 13.0118
378 F HU #DIV/0!
379 M NL 8.85045
380 M NL 9.94745
381 M NL 1.558833333
382 M NL 3.0878
383 M HU 6.569266667
384 M HU 15.0887
385 M HU 3.2062
386 M HU 2.608725
387 M HU 1.378
388 M HU #DIV/0!
459 F sham 6.094033333
460 F sham 0.001
461 F sham 5.08325
462 F sham 3.637133333
463 F 50cGy+HU 11.5864
464 F 50cGy+HU 14.1619
465 F 50cGy+HU 9.94975
466 F 50cGy+HU 13.44505
467 F 100cGy+HU 3.346125
468 F 100cGy+HU 7.403566667
469 F 100cGy+HU 9.362133333
470 F 100cGy+HU 4.7179
471 M sham 3.971366667
472 M sham 3.373566667
473 M sham 3.96375
474 M sham 3.5432
475 M 50cGy+HU 8.7785
476 M 50cGy+HU 5.6757
477 M 50cGy+HU 15.5526
478 M 50cGy+HU 15.1947
479 M 100cGy+HU 8.5385
480 M 100cGy+HU 14.12903333
481 M 100cGy+HU 5.5977
482 M 100cGy+HU 11.95836667
""",
    "SMI312 cingulate gyrus": """
353 F sham #DIV/0!
354 F sham #DIV/0!
351 F 50cGy 14.1194
352 F 50cGy 22.9693
357 F 50cGy 16.6084
358 F 50cGy #DIV/0!
349 F 100cGy 10.6635
350 F 100cGy 34.6336
355 F 100cGy 15.2942
356 F 100cGy 28.3517
363 M sham 5.1414
364 M sham #DIV/0!
361 M 50cGy 11.4131
362 M 50cGy 13.3421
365 M 50cGy 8.4861
366 M 50cGy 9.907
359 M 100cGy 10.1671
360 M 100cGy 16.8774
367 M 100cGy #DIV/0!
368 M 100cGy 7.1906
369 F NL 0.9844
370 F NL 5.2931
371 F NL 4.9176
372 F NL 8.6719
373 F HU 5.3008
374 F HU 6.1889
375 F HU 6.7669
376 F HU 14.6782
377 F HU 21.3276
378 F HU #DIV/0!
379 M NL 0.966
380 M NL 2.4486
381 M NL 1.1672
382 M NL 6.2065
383 M HU 16.933
384 M HU 10.0294
385 M HU 11.9328
386 M HU 9.3799
387 M HU 9.0802
388 M HU #DIV/0!
459 F sham 15.3291
460 F sham 0.202
461 F sham 7.0443
462 F sham 8.3967
463 F 50cGy+HU 13.83555
464 F 50cGy+HU 20.0114
465 F 50cGy+HU 10.2461
466 F 50cGy+HU 22.0495
467 F 100cGy+HU 6.3485
468 F 100cGy+HU 15.6363
469 F 100cGy+HU 24.4856
470 F 100cGy+HU 22.8379
471 M sham 5.43525
472 M sham 8.0096
473 M sham 7.0454
474 M sham 5.6441
475 M 50cGy+HU 7.0282
476 M 50cGy+HU 6.3623
477 M 50cGy+HU #DIV/0!
478 M 50cGy+HU 11.33165
479 M 100cGy+HU 13.311
480 M 100cGy+HU 25.4737
481 M 100cGy+HU 12.6527
482 M 100cGy+HU 36.2799
""",
}

# treatment string -> (HU, IR, canonical Prism-style group label)
TREATMENT = {
    "sham":       ("Sham", 0,   "Sham"),
    "NL":         ("Sham", 0,   "NL"),      # NL treated as Sham per project convention
    "50cGy":      ("Sham", 50,  "50cGy"),
    "100cGy":     ("Sham", 100, "100cGy"),
    "HU":         ("HU",   0,   "HU"),
    "50cGy+HU":   ("HU",   50,  "HU+50cGy"),
    "100cGy+HU":  ("HU",   100, "HU+100cGy"),
}
SEX = {"F": "Females", "M": "Males"}

TITLES = {
    "SMI312 SSC": "*SMI312 SSC",
    "SMI312 cingulate gyrus": "*SMI312 cingulate gyrus",
}


def parse(outcome, block):
    rows, dropped = [], 0
    for line in block.strip().splitlines():
        m = re.match(r"^(\d+)\s+([FM])\s+(.+?)\s+(\S+)$", line.strip())
        if not m:
            raise ValueError(f"unparsed line in {outcome}: {line!r}")
        sample, sex, treat, val = m.groups()
        treat = treat.replace(" ", "")
        if treat not in TREATMENT:
            raise ValueError(f"unknown treatment {treat!r} in {outcome}")
        if val.startswith("#DIV"):
            dropped += 1
            continue
        hu, ir, label = TREATMENT[treat]
        rows.append({
            "Outcome": outcome, "Sex": SEX[sex], "HU": hu, "IR": ir,
            "Value": float(val), "Block": 1,
            "Original_Prism_Group_Label": label,
            "Prism_Table_Title": TITLES[outcome],
            "Sample_ID": int(sample),
        })
    return rows, dropped


def main():
    backup = XLSX.with_name(XLSX.stem + "_BACKUP_pre_SMI312_update.xlsx")
    if not backup.exists():
        shutil.copy2(XLSX, backup)
        print(f"backup written: {backup.name}")

    sheets = pd.read_excel(XLSX, sheet_name=None)
    long = sheets["Long_Format_Data"].copy()
    if "Sample_ID" not in long.columns:
        long["Sample_ID"] = pd.NA

    new_rows = []
    for outcome, block in RAW.items():
        rows, dropped = parse(outcome, block)
        old_n = int((long.Outcome == outcome).sum())
        print(f"{outcome}: old n={old_n} -> new n={len(rows)} ({dropped} #DIV/0! dropped)")
        new_rows.extend(rows)

    long = long[~long.Outcome.isin(RAW.keys())]
    long = pd.concat([long, pd.DataFrame(new_rows)], ignore_index=True)
    long = long.sort_values(["Outcome", "Block", "Sex", "HU", "IR"], kind="stable").reset_index(drop=True)

    ncell = (long.groupby(["Outcome", "HU", "IR", "Sex"], as_index=False)
                  .size().rename(columns={"size": "n"})
                  .sort_values(["Outcome", "HU", "IR", "Sex"], kind="stable")
                  .reset_index(drop=True))

    instr = sheets["Instructions"].copy()
    col = instr.columns[0]
    note = [
        "",
        "UPDATE 2026-07-20 - SMI312 SSC and SMI312 cingulate gyrus re-quantified",
        ("These two outcomes were replaced wholesale with re-quantified values supplied by the user "
         "(more refined measurements plus images that were previously missing). Values marked #DIV/0! "
         "in the source were unmeasurable images and were dropped. The incoming data carried per-animal "
         "sample IDs, which are recorded in the new Sample_ID column (blank for all other outcomes, whose "
         "Prism export did not retain IDs). The incoming data also had an explicit NL group; consistent "
         "with the convention above, NL was merged into Sham (HU=Sham, IR=0) - the pre-existing Sham cell "
         "counts (Females 8 / Males 9) equal sham+NL in the new data, confirming the original Prism tables "
         "were built the same way. Note this update fixes the Females/Sham/50cGy cell in SMI312 cingulate "
         "gyrus, which previously contained only n=1."),
    ]
    instr = pd.concat([instr, pd.DataFrame({col: note})], ignore_index=True)

    sheets["Instructions"] = instr
    sheets["Long_Format_Data"] = long
    sheets["n_per_cell_summary"] = ncell

    with pd.ExcelWriter(XLSX, engine="openpyxl") as w:
        for name in ["Instructions", "Long_Format_Data", "n_per_cell_summary", "Outcomes_List"]:
            sheets[name].to_excel(w, sheet_name=name, index=False)

    print(f"\nwrote {XLSX.name}: {len(long)} total rows, {long.Outcome.nunique()} outcomes")
    for outcome in RAW:
        print(f"\n== {outcome} ==")
        print(long[long.Outcome == outcome].groupby(["Sex", "HU", "IR"]).size().to_string())


if __name__ == "__main__":
    main()
