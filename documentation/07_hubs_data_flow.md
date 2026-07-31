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
    ruh_hubs {
        INT ruhb_id PK
        NVARCHAR ruhb_name
        DATETIME2 date_created
        NVARCHAR user_id_created
        DATETIME2 date_edited
        NVARCHAR user_id_edited
    }

    ruh_support_types {
        INT ruht_id PK
        INT ruhb_id FK
        NVARCHAR ruht_name
        NVARCHAR ruht_description
        DATETIME2 date_created
        NVARCHAR user_id_created
        DATETIME2 date_edited
        NVARCHAR user_id_edited
    }

    ruh_lead_schools {
        INT ruhl_id PK
        INT ruhb_id FK
        INT ruhl_urn
        DATE ruhl_dateactive
        DATE ruhl_dateended
        BIT ruhl_active
        NVARCHAR ruhl_comment
        DATETIME2 date_created
        NVARCHAR user_id_created
        DATETIME2 date_edited
        NVARCHAR user_id_edited
    }

    ruh_actions {
        INT ruha_id PK
        INT ruhb_id FK
        INT ruht_id FK
        NVARCHAR ruha_name
        NVARCHAR ruha_description
        DATETIME2 date_created
        NVARCHAR user_id_created
        DATETIME2 date_edited
        NVARCHAR user_id_edited
    }

    ruh_support_schools {
        INT ruhs_id PK
        INT ruhb_id FK
        INT ruhl_id FK "Nullable"
        INT ruht_id FK
        INT ruhs_urn
        DATE ruhs_dateactive
        DATE ruhs_dateended
        BIT ruhs_active
        NVARCHAR ruhs_comment
        DATETIME2 date_created
        NVARCHAR user_id_created
        DATETIME2 date_edited
        NVARCHAR user_id_edited
    }

    ruh_support_school_actions {
        INT ruhsa_id PK
        INT ruhs_id FK
        INT ruha_id FK
        DATE ruhsa_date
        NVARCHAR ruhsa_comment
        DATETIME2 date_created
        NVARCHAR user_id_created
        DATETIME2 date_edited
        NVARCHAR user_id_edited
    }

    ruh_hubs ||--o{ ruh_support_types : "defines"
    ruh_hubs ||--o{ ruh_lead_schools : "contains"
    ruh_hubs ||--o{ ruh_actions : "owns"
    ruh_hubs ||--o{ ruh_support_schools : "oversees"
    
    ruh_support_types ||--o{ ruh_actions : "categorises"
    ruh_support_types ||--o{ ruh_support_schools : "classifies"
    
    ruh_lead_schools ||--o{ ruh_support_schools : "manages (optional)"
    
    ruh_support_schools ||--o{ ruh_support_school_actions : "receives"
    ruh_actions ||--o{ ruh_support_school_actions : "logged_in"
    ```

Source Data
RISE Schema — 01_RISE
[01_RISE].[ruh_hubs] Canonical lookup table identifying all recognized RISE Regional Improvement / Support Hubs.

[01_RISE].[ruh_support_types] Defines the categories and frameworks of support interventions available across the hubs.

[01_RISE].[ruh_lead_schools] Tracks designated lead institutions responsible for coordinating or delivering hub activities, mapped via unique reference numbers (URNs).

[01_RISE].[ruh_actions] A catalog of explicit action templates or interventions tied back to hubs and support streams.

[01_RISE].[ruh_support_schools] Time-bounded dataset mapping participating support-recipient schools (URN) to specific hubs and lead schools. Tracks operational status via active flags and open/close dates.

[01_RISE].[ruh_support_school_actions] The transactional junction mapping real-world actions taken for specific supported schools, documenting precise delivery dates and commentary.

AIDT Core Tables — 01_AIDT
[01_AIDT].[app_list]: List of apps

[01_AIDT].[users]: List of app users

[01_AIDT].[roles]: List of available app roles

[01_AIDT].[user_roles]: Links a user to a role

[01_AIDT].[transaction_table]: List of all transactions across apps

[01_AIDT].[tools_analytics]: Access and key action stamp from app

[01_AIDT].[quality_list]: Quality List for the quality issues discovered by our pipeline

[01_AIDT].[quality_check]: Quality Check for the list of checks against this tool

[01_AIDT].[quality_check_log]: Summary of quality run by rule

[01_AIDT].[quality_summary]: Summary of quality issues

Core Data — 00_core
[00_Core].[Edubase]: High level school data from Get Information About Schools (GIAS).

Others
[02_Single].[allschoolscompletefromfeb24]: Latest school-level data for currently open schools.

ETL Processes
Quality Pipeline (R) — Overview
Purpose. Surface data quality issues in the RISE Hubs dataset and maintain a reliable, intervention‑ready view of school‑to‑hub relationships.

Scope.

Inputs: All 01_RISE operational tables, 00_core.Edubase (validated via URN checking)

Output: 01_AIDT.quality_list

Run cadence. Daily QMD-based R pipeline executed on our POSIT server.

Owner. SSSR


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

Test Environment Setup (01_RISE_b)
To support development, prototyping, and validation of staging datasets without risking production integrity, an identical testing zone exists under the schema 01_RISE_b.

Isolation Strategy
Data Containment: All relationships, constraint logic, and foreign key boundaries are declared completely within 01_RISE_b. Tables in this testing zone never point back to tables in 01_RISE.

Testing Scope: Developers should clone subset or dummy validation data here to iterate on new custom rules or test app transactions before deploying modifications to production pipelines.

Test Architecture Blueprint
```mermaid
flowchart TD
    subgraph Staging_Isolation_Boundary [Schema: 01_RISE_b]
        b_hubs[ruh_hubs] --> b_st[ruh_support_types]
        b_hubs --> b_ls[ruh_lead_schools]
        b_hubs --> b_act[ruh_actions]
        b_hubs --> b_ss[ruh_support_schools]
        
        b_st --> b_act
        b_st --> b_ss
        b_ls -.-> b_ss
        
        b_ss --> b_ssa[ruh_support_school_actions]
        b_act --> b_ssa
    end
    
    subgraph Core_References [Schema: 00_core / 02_Single]
        core_edu[Edubase] --- b_ls
        core_edu --- b_ss
    end
    ```
    