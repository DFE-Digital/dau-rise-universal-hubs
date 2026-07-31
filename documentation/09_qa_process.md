# Quality Assurance Process — RISE Universal

## 1. Automated QA

The Significant Change dataset is validated daily through an automated R‑based Quality Pipeline. Automated checks can be read by running:
```sql
SELECT [quality_check_id]
      ,[quality_name]
      ,[quality_description]
      ,[quality_check_steps]
      ,[quality_check_justification]
      ,[app_id]
      ,[check_active]
      ,[github_file]
  FROM [Data_Insight_Team].[01_AIDT].[quality_check]
  WHERE app_id = 3;
```
All identified issues are written to `[01_AIDT].[quality_list]` using `app_id = 3`.

---

## 2. Manual QA

Where automated checks cannot capture contextual or policy‑dependent logic, the following manual QA processes apply:

- **Monthly validation with policy teams**
  - Review of edge cases, exceptions or unusual SC scenarios.

- **Spot checking extreme or unexpected values**
  - Examples: unusually early/late dates.

- **Review of logs and QA outputs**
  - Examine entries in `quality_check_log`, `quality_summary` and `quality_list`.

- **Ad‑hoc investigation of portal‑reported issues**
  - Issues escalated through the Significant Change portal or via support channels.

---

## 3. Regression Tests

Regression testing ensures that changes to the pipeline, schemas, or business rules do not break existing functionality.

- **R unit tests (testthat)**
  - Test individual rule functions.
  - Confirm consistent behaviour for known inputs.

- **SQL validation scripts**
  - Validate PK/FK constraints, row counts, and expected joins.
  - Check integrity between `tracker`, `sigchangetype`, `giaschangetype`, and `00_core`.

- **Baseline comparison**
  - Compare quality counts and issue types to historical baselines using summary quality data.
  - Validate that expected numbers of issues are produced after code changes.

---

## 4. Error Handling

When QA fails or produces inconsistencies:

- **Notifications**
  - Email alerts are issued automatically for pipeline errors.

- **Pipeline behaviour**
  - Non‑critical rule failures produce warnings but continue execution.
  - Critical failures halt the pipeline.

- **Investigation**
  - We review logs, error messages, and failed steps.
  - Issues escalated to policy teams where required.

- **Issue logging**
  - Errors logged in:
    - .\logs\
    - Console

---

## 5. Documentation of QA

Quality outputs and evidence are stored centrally:

- **QA Tables**
  - `[01_AIDT].[quality_list]` — individual issues  
  - `[01_AIDT].[quality_check]` — defined checks  
  - `[01_AIDT].[quality_check_log]` — run logs  
  - `[01_AIDT].[quality_summary]` — aggregated issue counts  

- **Reports & Dashboards**
  - Dashboards displaying open/closed issues and trend summaries. (planned)

- **Log Files**
  - Posit Connect logs for the daily R job.

- **Documentation**
  - Quality rules captured in the Significant Change documentation repository.

    ```mermaid
  flowchart LR
    A[Extract 01_RISE.ruh_schools] --> B[Join 00_core by URN]
    B --> C["Apply quality rules (R)"]
    C --> D{Issues found?}
    D -- Yes --> E[Upsert to 01_AIDT.quality_list]
    D -- No --> F[No action required]
    E --> G[Dashboards & Internal Tools]
  ```
