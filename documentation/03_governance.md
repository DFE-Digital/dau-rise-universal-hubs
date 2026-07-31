# Governance — RISE Hubs

## Purpose

This document defines the governance arrangements for the **RISE Hubs** data and tooling, ensuring the dataset remains accurate, auditable, and fit for purpose.

Governance covers ownership, change control, decision rights, and operational responsibilities.

---

## Scope

This governance applies to:

- RISE hub reference data
- School ↔ hub relationships
- Historical records
- Validation and quality processes
- Downstream use in tools, dashboards, and analysis

---

## Roles & Responsibilities

### Data Owners
- Own the policy intent and business rules behind hub allocations
- Approve material changes to logic or interpretation
- Resolve escalated data quality issues

---

### Data Stewards (DAU)
- Maintain the data model and documentation  
- Implement approved changes  
- Monitor quality outputs  
- Support users and policy colleagues  

---

### Contributors
- Propose updates or corrections
- Raise issues via GitHub
- Provide supporting context for changes

---

## Change Management

### In‑Scope Changes
- New hubs
- Hub renaming or reclassification
- School re‑assignment
- Back‑dated corrections
- Structural changes to data models or validation rules

---

### Change Process
1. Change proposed via GitHub issue  
2. Impact and validation assessed  
3. Approval obtained (where required)  
4. Change implemented and logged  
5. Documentation updated  

---

## Data Quality & Validation

- Automated validation rules are applied regularly via R pipelines
- Issues are logged to `[01_AIDT].[quality_list]`
- Open issues are monitored and triaged
- Resolution actions are auditable and timestamped

---

## Audit & History

- All hub assignments are time‑bounded
- No records are overwritten or deleted without approval
- Historical links are preserved via end‑dating

---

## Access & Security

- Write access restricted to approved roles
- Read access provided via views or tools where possible
- Credentials managed through standard DAU processes

---

## Review Cadence

- Governance reviewed annually or following:
  - Major policy change
  - Structural data model update
  - Tooling expansion

---

## Open Items / To Be Defined

- Named policy owner
- SLA expectations
- Retention period confirmation
- External data sharing rules