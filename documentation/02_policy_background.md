# Data Flow — RISE Universal - Hubs

This document describes the end‑to‑end data flow, schemas, ERDs, quality pipelines, and output products that support the **RISE Hubs** process.

RISE Hubs capture the relationship between schools and their assigned Regional Improvement / Support Hub over time, providing a consistent, auditable source of truth for internal tools, monitoring, and policy analysis.

---

## High‑Level Flow

Source inputs (hub allocation lists, internal trackers, managed updates)  
SQL table creation and storage (`01_RISE`)  
Join to authoritative school reference data (`00_core`)  
Validation and quality pipelines (R using shared DAU tooling)  
Analysis and monitoring  
Reporting and internal tools  
Downstream reuse (MI, extracts, policy analysis)

---

## System Architecture (Mermaid)

```mermaid
flowchart LR
    A[Source Inputs<br>(Hub allocations, updates)] --> B[SQL Tables<br>(01_RISE)]
    B --> C[Join to 00_core<br>(School reference via URN)]
    C --> D[R ETL / Validation<br>(dauPortalTools)]
    D --> E[01_AIDT Outputs<br>(quality_list, logs)]
    E --> F[Dashboards / Internal Tools]
    E --> G[Extracts / Analysis]
```

# Data Model
## RISE Hubs ERD

    ```erDiagram

    ruh_list {
        INT ruhl_id PK
        NVARCHAR ruhl_name
    }

    ruh_schools {
        INT ruhs_id PK
        INT ruhl_id FK
        INT ruhs_urn
        DATE ruhs_dateactive
        DATE ruhs_dateended
        NVARCHAR ruhs_comment
    }

    edubase {
        INT URN PK
        NVARCHAR school_name
        BIT open
    }

    ruh_list ||--o{ ruh_schools : "ruhl_id"
    edubase ||--o{ ruh_schools : "URN"
    ```

# Source Data
## RISE Schema — 01_RISE

[01_RISE].[ruh_list]
Canonical reference list of recognised RISE hubs.

[01_RISE].[ruh_schools]
Time‑bounded mapping between schools (URN) and hubs.
Supports historical tracking via ruhs_dateactive / ruhs_dateended.


## Core School Data — 00_core

[00_core].[Edubase]
Authoritative school reference data from GIAS, joined via URN.


Supporting Reference Data

[02_Single].[allschoolscompletefromfeb24]
Latest snapshot of open schools used for validation and completeness checks.


ETL & Quality Processes
RISE Hubs Quality Pipeline (R)
Purpose
Surface data quality issues in the RISE Hubs dataset and maintain a reliable, intervention‑ready view of school‑to‑hub relationships.

Inputs

[01_RISE].[ruh_schools]
[01_RISE].[ruh_list]
[00_core].[Edubase] (via URN)


Outputs

[01_AIDT].[quality_list]

Dedicated app_id for RISE Hubs
record_id = ruhs_id
Supports monitoring, triage, and MI




Example Quality Rules


Single Active Hub per School

A school must not have more than one record where:
ruhs_dateended IS NULL





Valid Date Ranges

ruhs_dateactive must be on or before ruhs_dateended (where present)



School Open Status

Schools with an active hub assignment must be open in 00_core



Valid Hub Reference

ruhl_id must exist in [01_RISE].[ruh_list]




Quality Issue Process Flow