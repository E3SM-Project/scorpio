---
name: release-software
description: Release Management. Used to release a new version of SCORPIO by updating version number, generating docs and tagging a release. (Create release branch using Git, test the branch, update version in CMake, generate documentation using Doxygen, check in and tag release using Git)
version: 1.0.0
license: see LICENSE file in the main directory
compatibility: claude-code >= 1.0.0
allowed-tools:
  - Read, Edit(CMakeLists.txt), Bash, Git, CMake, Doxygen
metadata:
  category: utility
---

# Release Software (Create release branch using Git, test the branch, update version in CMake, generate documentation using Doxygen, check in and tag release using Git)

## Overview
Automate SCORPIO release management by testing, updating the version, generating documentation, and tagging the repository.

## Prerequisites
* **Git** installed and accessible via system PATH.
* **CMake** (v3.0 or higher) managing the project.
* **Doxygen** installed and accessible via system PATH.
* **PnetCDF** library installed

## When to Use
* Trigger this skill when user explicitly requests to create a new release

## Git Configuration
* The git settings to use is available at resources/git\_settings.txt
* Never push changes to the remote repository
* Always add a "Co-authored-by: <github-username> <github-username@://github.com>" section for all commits. The Co-author is the current github user.

## Instructions

### Step 1 : Pre-release Verification
* **Check Git Status** Ensure that the current working directory is clean
```bash
git status
```

* **Check current branch** Ensure that the current branch is master. All release branches are created from the master branch.
```bash
git branch --show-current
```

* **Test** Build and run tests on local node using PnetCDF.
    * On ANL compute nodes (hostname ```compute-*.cels.anl.gov```) see the script scripts/build_scorpio_anl_gce_compute_node.sh on how to build the library and run tests
    * On other machines see .github/workflows directory for an example workflow

### Step 2 : Create a release branch and update version number and docs

The release branches are named using the pattern : ```<github username>/scorpio_v<MAJOR_VERSION_NUMBER>.<MINOR_VERSION_NUMBER>.<PATCH_VERSION_NUMBER>```

* Create and checkout release branch
* Update version number in <PROJECT_SOURCE_DIR>/CMakeLists.txt . The script below shows an example of how to change the current version to 1.2.3
```bash
NEW_MAJOR_VERSION=1
NEW_MINOR_VERSION=2
NEW_PATCH_VERSION=3
sed -i "s/\(set(VERSION_MAJOR\s\+\)[0-9]\+\(\s\+\.*\)/\1\$NEW_MAJOR_VERSION\2/g" CMakeLists.txt
sed -i "s/\(set(VERSION_MINOR\s\+\)[0-9]\+\(\s\+\.*\)/\1\$NEW_MINOR_VERSION\2/g" CMakeLists.txt
sed -i "s/\(set(VERSION_PATCH\s\+\)[0-9]\+\(\s\+\.*\)/\1\$NEW_PATCH_VERSION\2/g" CMakeLists.txt
```
* Update documentation in source directory
```bash
make update_docs
```
* Check-in CMakeLists.txt and documentation (docs/html)
```bash
git add CMakeLists.txt
git add -A -f docs/html
git commit -m "Updating version number and adding generated docs"
```

### Step 3 : Tag release branch
* Create a summary of the changes since last release
* Tag release tag and include the summary of the changes since last release (above) in the commit message

### Step 4 : Post-relase sanity testing
* Build the source and documentation to ensure that the code builds

## Examples
### Input
"Create a SCORPIO release for version 1.2.3"

### Output
* A release branch created locally named ```ai-agent-bot/scorpio_v1.2.3``` with the updated version number and updated documentation
* The latest commit is tagged "scorpio-v1.2.3" (and the tag commit message includes the changes since last release)

### Input
"Create a new patch release for SCORPIO"

### Output
* If the current release is 1.2.3, release "scorpio-v1.2.4" is tagged in branch ```ai-agent-bot/scorpio_v1.2.4```

### Input
"Create a new minor release for SCORPIO"

### Output
* New minor release releases start from patch version 0
* If the current release is 1.2.3, release "scorpio-v1.3.0" is tagged in branch ```ai-agent-bot/scorpio_v1.3.0```

### Input
"Create a new major release for SCORPIO"

### Output
* New major release releases start from minor version 0 and patch version 0
* If the current release is 1.2.3, release "scorpio-v2.0.0" is tagged in branch ```ai-agent-bot/scorpio_v2.0.0```

## Notes
* All changes should be confined to the local repository. Never push the changes to the remote repository
