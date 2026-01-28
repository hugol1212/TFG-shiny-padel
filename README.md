# template_shiny_app

Project template for a shiny app.

## How to use the template

-   Choose a new package name. In order to comply with standards, remember that **underscores are not allowed**.
-   Change the package name in the following files:
    -   `DESCRIPTION` file, line 1.
    -   `R/zzz.R`, line 2.
    -   Rename `template.shiny.app-package.R` to match `<PACKAGE_NAME>-package.R`.
-   You need, at least, the following packages for developing an app with this template (not to run the app):
    -   roxygen2
    -   testthat
    -   pkgdown
    -   usethis

The template is configured to use {renv}. You can `renv::restore()`, and then (optionally) update the packages to the last version, then `renv::snaphot()`. Or you can `renv::deactivate()`.

In `DESCRIPTION`, update the LICENSE info: `use_mit_license()`, `use_gpl3_license()`, `use_proprietary_licence("MY COMPANY")` or friends to pick a license.

## Customise your app

-   In `app.ui`:
    -   Change title and window_title
    -   Change title of sections and sidebars
-   In the `inst/app/www folder`, include your own resources.

## Template structure

-   Typical R package structure (`DESCRIPTION` file, `R/` directory).

-   R functions

-   `inst/app/www` folder for additional resources (JS, CSS, images, ...)

-   Translation folder (if multilanguage)

## My development workflow

-   Use functions for rendering outputs and even for producing ui elements.

-   Go to the terminal tab and run: `Rscript -e 'devtools::load_all();app_run()'`

-   Stop and rerun in the terminal.

## Using .Renviron for environment variables

If your package requires environment variables (e.g., API keys, database credentials, or custom settings),
you can use an `.Renviron` file to store them securely. Create or edit the `.Renviron` file in your home directory or package root by running:

```r
usethis::edit_r_environ()
```

Then, add your variables in the following format:

```r
MY_PACKAGE_API_KEY=your_api_key_here
MY_PACKAGE_CONFIG_PATH=/path/to/config
```
To use these variables within your package, retrieve them with:

```r
api_key <- Sys.getenv("MY_PACKAGE_API_KEY")
```

`.Renviron` is not included in version control (add it to `.gitignore`).
Make sure it's not modified and commited, in order to keep sensitive data secure.
