# Definitions — Significant Changes

Significant Change (SC) — A material change to an school that requires formal approval.
Tracker — Core SQL table containing every Significant Change record.
URN — Unique Reference Number for a school; used as the primary join key.
GIAS — “Get Information About Schools”, the authoritative national school‑level dataset.
00_core — Schema containing cleaned and standardised core education datasets.
01_sigchange — Schema containing Significant Change application data and lookup tables.
01_AIDT — Schema for shared app metadata, user roles, audit logs, and quality outputs.
Quality Pipeline — Daily R process evaluating data consistency and storing issues.
quality_list — Table storing quality issues identified by the pipeline (SC uses app_id = 3).
app_id — Identifier used in AIDT to distinguish applications (Significant Change = 3).
SLIC — Our team level reporting tool consuming SC metrics.
SSSR — Our team :)  responsible for Significant Change tooling and quality processes.
POSIT — Posit environment running scheduled QMD-based jobs and hosting our r based portals.
ETL — Extract > Transform > Load processing across data systems.
SLA — Service Level Agreement defining operational deadlines or expectations.