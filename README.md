# atutils

Reusable R utilities for acoustic telemetry data processing, with a focus on data from the European Tracking Network (ETN) and KALTOA.

The package brings together functions used across acoustic telemetry projects to avoid duplicating helper code in individual analysis repositories.

## Contents

The package currently contains utilities for:

- **ETN data:** retrieving and preparing ETN deployments, detections and receiver information.
- **Receiver logs:** reading and parsing receiver log files, including Thelma and Innovasea formats.
- **KALTOA:** preparing detection lists, managing run information and supporting clock synchronisation workflows.
- **General utilities:** reading and saving data.

## Installation

Install a specific release from GitHub using `remotes`:

```r
remotes::install_github("lottepohl/atutils@v0.1.0")
```

Replace the version tag with the release you want to use.

For projects managed with `renv`, install and record the dependency with:

```r
renv::install("lottepohl/atutils@v0.1.0")
renv::snapshot()
```

## Usage

Load the package:

```r
library(atutils)
```

Alternatively, call individual functions using the package namespace:

```r
atutils::etn_get_or_download(...)
atutils::kaltoa_auto_sync_order(...)
```

For reproducible analyses, use a specific release tag rather than installing directly from the development branch.

## Development

The package is developed using standard R package conventions.

Source files are stored directly in `R/`, with filenames grouped by functionality:

```text
R/
├── etn_data.R
├── etn_data_cleaning.R
├── etn_detections.R
├── etn_receiver_logs.R
├── general_data_input_output.R
├── general_file_reading.R
├── general_plotting.R
├── kaltoa_clock_synchronisation.R
├── kaltoa_detections.R
└── kaltoa_track_run.R
```

### Adding a function

When adding a new function:

1. **Choose the appropriate source file.** Add the function to an existing file if it belongs to that functionality. Otherwise, create a new `.R` file directly inside `R/`.

2. **Follow the naming conventions.**
   - General utilities: descriptive function names, e.g. `load_data()`.
   - ETN functions: use the `etn_` prefix, e.g. `etn_get_or_download()`.
   - KALTOA functions: use the `kaltoa_` prefix, e.g. `kaltoa_auto_sync_order()`.

3. **Document the function using roxygen2.** Include a title, description, parameter documentation, return value and, where useful, examples.

   For example:

   ```r
   #' Short description of the function
   #'
   #' More detailed explanation of what the function does.
   #'
   #' @param x Description of the input.
   #' @returns Description of the returned object.
   #' @export
   my_function <- function(x) {
     ...
   }
   ```

   Only add `@export` if the function is intended to be part of the public package API. Internal helper functions should remain unexported.

4. **Declare dependencies.** If the function uses a package that is not yet listed in `DESCRIPTION`, add it using `usethis::use_package()`.

5. **Generate documentation.**

   ```r
   devtools::document()
   ```

   This updates the generated help files in `man/` and the `NAMESPACE` file. Do not edit these generated files manually.

6. **Try the function locally.**

   ```r
   devtools::load_all()
   ```

   Run the function with representative inputs and check that it behaves as expected.

7. **Check the package.**

   ```r
   devtools::check()
   ```

   Resolve errors and warnings before committing. Review any notes and assess whether they are expected.

8. **Commit your changes.**

   ```bash
   git add .
   git commit -m "Add description of new function"
   git push origin main
   ```

### Releasing a new version

When a set of changes is ready to be shared:

1. Update the `Version` field in `DESCRIPTION`.
2. Run `devtools::document()` and `devtools::check()`.
3. Commit and push the release changes.
4. Create an annotated Git tag matching the package version:

   ```bash
   git tag -a v0.1.1 -m "atutils version 0.1.1"
   git push origin v0.1.1
   ```

5. Install that specific tag in projects that need the new release.

## General development principles

- Keep functions reusable and independent of individual analysis projects.
- Avoid hard-coded project paths and project-specific assumptions.
- Use explicit inputs and return values rather than relying on hidden global objects.
- Keep internal helper functions unexported unless they are intended for external use.
- Document side effects, particularly when functions write files or assign objects to the global environment.
- Keep package dependencies to those actually needed.

## Licence

See the `LICENSE` file for licensing information.
