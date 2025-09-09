# ApexRMS Coding Standards Template Repository

## Table of Contents

- [Overview](#overview)
- [Setup for R Development](#setup-for-r-development)
  - [RStudio](#rstudio)
  - [VSCode](#vscode)
- [Setup for Python Development](#setup-for-python-development)
  - [VSCode](#vscode-1)
- [Adding these standards to a new project repository](#adding-these-standards-to-a-new-project-repository)
- [Adding these standards to an existing project repository](#adding-these-standards-to-an-existing-project-repository)
  - [Required files for R projects](#required-files-for-r-projects)
  - [Required files for Python projects](#required-files-for-python-projects)
  - [Required files for mixed R/Python projects](#required-files-for-mixed-rpython-projects)
- [Usage](#usage)

## Overview

The goal of this repository is to:

1. Provide guidance on how to follow the coding standards for ApexRMS
2. Provide a common set of configuration files for linters and auto-formatters that we use at Apex. These tools will help to remove some of the effort in following our coding standards, and should only need to be configured once in order for them to work with all of our projects

This README and all other files in this repository are intended to be a working set of coding standards, and are open for improvement. If there is something that you think has been missed, feel free to create a pull request to this repository. If added, any new projects created with this template will automatically have your changes.

## Setup for R Development

For R, [Air](https://posit-dev.github.io/air/) is used for auto-formatting, and [lintr](https://lintr.r-lib.org/) is used for linting. Air and lintr are both derived from the same style guidelines, so some of the issues that lintr can catch will automatically be handled by the formatting that Air will do for us!

For a list of configuration options, see:

- Air: https://posit-dev.github.io/air/configuration.html.
- lintr: https://lintr.r-lib.org/reference/index.html#individual-linters.

### RStudio

**Air**

1.  Install the Air command-line tool by running the following command in a powershell window:

        powershell -ExecutionPolicy Bypass -c "irm https://github.com/posit-dev/air/releases/latest/download/air-installer.ps1 | iex"

2.  Next, you'll need to tell RStudio to use Air as an external formatter:

    1. Open "Tools -> Global Options -> Code".
    2. Choose the Formatting tab at the top.
    3. Change the Code formatter option to External.
    4. Replace the path under "Reformat command" with the absolute path to the Air executable followed by the word "format" (ex:`C:\Users\{windows user name}\.local\bin\air.exe format`).
    5. Apply the changes and exit the settings.

3.  Finally, configure RStudio to apply the formatting on save:
    1. Open "Tools -> Global Options -> Code -> Saving" .
    2. Check "Reformat documents on save".
    3. Apply the changes and exit the settings.

**lintr**

1. Install the `lintr` package in RStudio:

   1. In the RStudio console, run the following command to install `lintr`:

      `install.packages(c("lintr", "languageserver"))`

   2. When prompted to build the package from source, select "Yes".

### VSCode

**Air**

1. Open the Extensions tab in VSCode (4th icon from the top in the left-hand sidebar).
2. In the searchbar of the Extensions tab, type "Posit.air-vscode".
3. Click on the first (and only) extension that comes up ("Air - R Language Support") and install it.

**lintr**

1.  Install the R extension for VSCode:
    1. In the Extensions tab, search for "REditorSupport.r".
    2. Click on the first (and only) extension that comes up ("R") and install it.
    3. Close VSCode.
2.  Open VSCode as an administrator:
    1. Search for VSCode in the windows search bar.
    2. Right click on VSCode.
    3. Select "Run as administrator" from the context menu.
3.  After opening, you may receive the following error notification in the bottom right corner:

        Cannot find R to use for help, package installation etc. Change setting ...

    If so, then you need to add the path to the R executable by editing your VSCode user settings:

    1. Click on the gear icon in the bottom left-hand corner of VSCode.
    2. Select "Settings" from the menu that appears.
    3. In the search bar at the top of the Settings panel, type "Rpath".
    4. Find the setting for "R > Rpath: Windows" and set it to the path of your R executable (e.g., `C:\Program Files\R\R-4.4.0\bin\R.exe`).

4.  You may also receive another error notification asking to install the "languageserver" R package. Confirm this installation and VSCode will install this into your R installations package registry.
5.  Close VSCode, and reopen normally (without admin privileges).
6.  To view the lintr in the "Problems" window at the bottom of VS Code. Right click the panel with tabs for Output, Terminal, Ports, etc, and click Problems to pin to bar.

## Setup for Python Development

For python development, [autopep8](https://pypi.org/project/autopep8/) and [pylint](https://pypi.org/project/pylint/) are used for our auto-formatting and linting, respectively. Again, these two tools are based on the same styling guidelines, so they work well together.

For a list of configuration options, see:

- pylint: https://pylint.readthedocs.io/en/stable/user_guide/configuration/all-options.html.
- pep8: https://peps.python.org/pep-0008/.

### VSCode

**pylint**

1. Install the pylint extension for VSCode:
   1. In the Extensions tab, search for "ms-python.pylint".
   2. Click on the first (and only) extension that comes up ("Pylint") and install it.

**autopep8**

1. Install the autopep8 extension for VSCode:
   1. In the Extensions tab, search for "ms-python.autopep8".
   2. Click on the first (and only) extension that comes up ("autopep8") and install it.

## Adding these standards to a new project repository

When creating a new repository through the standard flow on GitHub, you'll be provided the option to "Start with a template". Select `ApexRMS/apexCodingStandardsTemplate` to use this repository as a starting point. Once created, feel free to replace this README with one that applies to your repository's use case.

## Adding these standards to an existing project repository

If you want to start enforcing coding standards on an existing repository, simply clone this repository to your local machine, copy and **commit** the appropriate files/folders as-is into the root of your existing project repository based on your project's language requirements.

### Required files for R projects

For R-only projects, copy these files:

- `.vscode/` (entire folder)
- `.air.toml` (R code formatting configuration)
- `.lintr` (R linting configuration)
- `.gitignore`
- `.coderabbit.yml`
- `PULL_REQUEST_TEMPLATE.md`

### Required files for Python projects

For Python-only projects, copy these files:

- `.vscode/` (entire folder)
- `.pylintrc` (Python linting configuration)
- `.gitignore`
- `.coderabbit.yml`
- `PULL_REQUEST_TEMPLATE.md`

### Required files for mixed R/Python projects

For projects containing both R and Python code, copy all files:

- `.vscode/` (entire folder)
- `.air.toml` (R code formatting)
- `.lintr` (R linting)
- `.pylintrc` (Python linting)
- `.gitignore`
- `.coderabbit.yml`
- `PULL_REQUEST_TEMPLATE.md`

---

If your project already has some of the following configuration files, then use the table below to decide how to handle conflicts:

| File Path                         | Action on Conflict    |
| --------------------------------- | ------------------    |
| .vscode/copilot-instructions.md   | Overwrite             |
| .vscode/extensions.json           | Merge contents        |
| .vscode/settings.json             | Merge contents        |
| .air.toml                         | Overwrite             |
| .coderabbit.yml                   | Overwrite             |
| .gitignore                        | Merge contents        |
| .lintr                            | Overwrite             |
| .pylintrc                         | Overwrite             |
| PULL_REQUEST_TEMPLATE.md          | Merge contents        |

"Merging contents" simply means to concatenate the contents of the existing file and the file coming from this template repository together.

## Usage

Your respective IDE should now automatically be reformatting your code whenever you save your changes. For VSCode, linting suggestions should automatically be provided to you visually in the current file you are editing by underlining problems found. To view a list of all problems in the currently opened R or python file, from the top navigation bar, select "View > Problems", and a window should appear along the bottom of your screen.

In order to show linting suggestions within RStudio, a manual process must be used.

1. Along the second level menu bar at the top of the screen, open the "Addins" dropdown.
2. Under "lintr", select "Lint current file".
3. A new panel labeled "Markers" should appear in your IDE with lintr's suggestions.
