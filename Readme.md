📘 README: DfE Sig Change Dashboard

Overview
This Shiny dashboard is developed by the Data Insight Team at the Department for Education to manage and monitor significant change records for schools. It connects to SQL Server and provides a user-friendly interface for viewing, editing, and reporting on school-level changes.

Purpose
The dashboard supports strategic conversations and policy decisions by enabling analysts and policy colleagues to:

Search for schools and trusts.
View and edit significant change records.
Track application progress and decisions.
Generate reports by region and change type.
Maintain lookup tables and metadata.
Structure
global.R:

Loads configuration from config.yml.
Connects to SQL Server using odbc.
Loads lookup tables (sigchangetype, giaschangetype).
Defines helper functions for rendering validated inputs.
Maps friendly field names for UI display.
app.R:

Defines the UI using page_navbar with five panels:
Intro: GOR bar chart of incomplete sigchange records.
Search: Filters by URN, school name, trust ID, and trust name.
School Details: Displays school info and associated sigchange records.
Sig Change: Full editable form for a selected sigchange record.
Support: Maintainers, policy links, and contact info.
Implements reactive logic for record selection, creation, and editing.
Uses reactiveVal to manage selected sigchange ID and trigger updates.
Features
Dynamic filtering and search.
Record creation and editing with validation.
Friendly field labels and conditional input rendering.
SQL-backed updates with safe handling of NULLs and types.
GOR-based visual reporting.
Support page with links and contact info.
Setup Instructions
Install R and RStudio.
Install required packages:

Create a config.yml file with your SQL Server credentials:

Place global.R and app.R in the same directory.
Run the app using:

🛠️ To Do / Suggested Improvements
Add user authentication and role-based access.
Implement audit logging for record edits.
Add export options (CSV, Excel, PDF).
Improve error handling and user feedback.
Add pagination to large tables.
Enable bulk record creation via upload.
Add tooltips and help icons for form fields.
Include historical change tracking.
Add dashboard usage analytics.
Add bookmarking or deep linking for schools.
Enable trust-level summary views.
Add filters for date ranges and change types.
Add spellcheck and formatting to comment fields.
Include visual indicators for required fields.
Add undo/redo functionality for edits.
Enable notifications for pending actions.
Add versioning to the dashboard and changelog.
Refactor code into modules for maintainability.
