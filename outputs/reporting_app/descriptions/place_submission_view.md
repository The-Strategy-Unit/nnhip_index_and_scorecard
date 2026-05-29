## Submission view

This view provides a **reconstruction of the original submitted dataset** for the selected Place and month, formatted to mirror the layout of the NNHIP submission template. It shows the values exactly as they were provided (after suppression rules are applied), making it easier to understand how the dashboard's metrics are derived.

### What the table shows

- **Metric grouping** Each metric is displayed in the same block structure used in the submission template, including counts, denominators and calculated rates.

- **Values types (N / D / R)** The rows represent the different value types submitted for each metric:

  - **N** = numerator

  - **D** = denominator

  - **R** = rate per 1,000 patients

- **Demographic breakdowns** Columns are organised into the same demographic groups as the submission template: **Total**, **Age Group**, **Ethnic Group** and **Deprivation Quintile**. Each demographic category expands into its individual values (e.g., age bands, ethnic categories, IMD quintiles).

- **Suppressed values** Any values suppressed in the original submission are shown as `"*"`, matching the behaviour applied during data processing.

### Why this view is useful

-   Provides a transparent, in-app way to inspect the underlying submitted data without opening external files.

-   Helps diagnose unexpected or anomalous dashboard values by showing the raw inputs used in calculations.

-   Mirrors the structure of the submission template, making it easier to cross-reference and validate data.

-   Reduces the need for manual file-hunting and comparison, speeding up debugging and quality-assurance workflows.

-   Supports clearer conversations with data providers by showing precisely what the data looks like following processing.