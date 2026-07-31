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

    ```mermaid
erDiagram

    EventStatus {
        INT s_id PK
        NVARCHAR s_name
        NVARCHAR s_created_by
        DATETIME s_created_on
        NVARCHAR s_modified_by
        DATETIME s_modified_on
    }

    EventTypes {
        INT et_id PK
        NVARCHAR et_name
        NVARCHAR et_created_by
        DATETIME et_created_on
        NVARCHAR et_modified_by
        DATETIME et_modified_on
    }

    AttendeeTypes {
        INT at_id PK
        NVARCHAR at_name
        NVARCHAR at_link
        NVARCHAR at_created_by
        DATETIME at_created_on
        NVARCHAR at_modified_by
        DATETIME at_modified_on
    }

    AgendaOptions {
        INT ao_id PK
        NVARCHAR ao_name
        NVARCHAR ao_created_by
        DATETIME ao_created_on
        NVARCHAR ao_modified_by
        DATETIME ao_modified_on
    }

    Events {
        INT ev_id PK
        NVARCHAR ev_name
        DATE ev_date
        NVARCHAR ev_location
        NVARCHAR ev_chair
        NVARCHAR ev_project_lead
        NVARCHAR ev_created_by
        DATETIME ev_created_on
        NVARCHAR ev_modified_by
        DATETIME ev_modified_on
        INT ev_et_id FK
        INT ev_s_id FK
    }

    AgendaItems {
        INT ai_id PK
        INT ai_ev_id FK
        INT ai_ao_id FK
        NVARCHAR ai_notes
        NVARCHAR ai_created_by
        DATETIME ai_created_on
        NVARCHAR ai_modified_by
        DATETIME ai_modified_on
    }

    Attendees {
        INT a_id PK
        INT a_ev_id FK
        INT a_at_type FK
        NVARCHAR a_notes
        NVARCHAR a_created_by
        DATETIME a_created_on
        NVARCHAR a_modified_by
        DATETIME a_modified_on
    }

    EventTypes ||--o{ Events : "ev_et_id"
    EventStatus ||--o{ Events : "ev_s_id"
    Events ||--o{ AgendaItems : "ai_ev_id"
    AgendaOptions ||--o{ AgendaItems : "ai_ao_id"
    Events ||--o{ Attendees : "a_ev_id"
    AttendeeTypes ||--o{ Attendees : "a_at_type"
    ```

## Source Data
### RISE Schema — 01_RISE

[01_RISE].[ruh_list]
Canonical reference list of recognised RISE hubs.

[01_RISE].[ruh_schools]
Time‑bounded mapping between schools (URN) and hubs.
Supports historical tracking via ruhs_dateactive / ruhs_dateended.



### AIDT Core Tables — 01_AIDT
[01_AIDT].[app_list] List of apps
[01_AIDT].[users] List of app users
[01_AIDT].[roles] List of available app roles
[01_AIDT].[user_roles] Links a user to a role
[01_AIDT].[transaction_table] List of all transactions across apps
[01_AIDT].[tools_analytics] Access and key action stamp from app
[01_AIDT].[quality_list] Quality List for the quality issues discovered by our pipeline
[01_AIDT].[quality_check] Quality Check for the list of checks against this tool
[01_AIDT].[quality_check_log] Summary of quality run by rule
[01_AIDT].[quality_summary] Summary of quality issues

### Core Data — 00_core
[00_Core].[Edubase]  High level school data from GIAS

### Others
[02_Single].[allschoolscompletefromfeb24] Latest school level data for open schools

---

## ETL Processes

### Quality Pipeline (R) — Overview

**Purpose.** Surface data quality issues in the RISE Hubs dataset and maintain a reliable, intervention‑ready view of school‑to‑hub relationships.

**Scope.**

- Inputs: [01_RISE].[ruh_schools], [01_RISE].[ruh_list], [00_core].[Edubase] (via URN)
- Output: [01_AIDT].[quality_list]

**Run cadence.** Daily QMD based R pipeline ran on our POSIT server.
**Owner.** SSSR
