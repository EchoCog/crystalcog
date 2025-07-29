# GitHub Actions Workflows Documentation

This directory contains GitHub Actions workflows for building, testing, and deploying the OpenCog project components.

## Workflow Overview

### Main Workflows

1. **build.yml** - Simple build workflow for the main project
   - Builds, tests, and packages the project
   - Uses artifacts to share build outputs between jobs
   - Runs on push to main and pull requests

2. **ci.yml** - Full CI pipeline with all OpenCog components
   - Builds components in dependency order: cogutil → atomspace → (ure, asmoses, cogserver, attention) → opencog
   - Each component is built in isolation with proper dependency management
   - Includes packaging step for main branch builds

3. **ci-improved.yml** - Improved CI using reusable workflows
   - Cleaner implementation using the reusable build component
   - Better parallelization of independent components
   - Includes integration testing phase

4. **multi-platform-build.yml** - Cross-platform build matrix
   - Tests on Ubuntu 20.04 and 22.04
   - Builds in both Release and Debug modes
   - Includes sanitizer builds (AddressSanitizer, ThreadSanitizer, UBSan)
   - Code quality checks with clang-format and cppcheck

### Reusable Workflows

- **reusable/build-component.yml** - Reusable workflow for building OpenCog components
  - Standardizes the build process across all components
  - Handles dependency downloads and artifact uploads
  - Configurable for different repositories and build options

## Build Sequence

The OpenCog project has the following dependency hierarchy:

```
cogutil
   └── atomspace
          ├── ure
          │    ├── miner
          │    └── unify
          ├── asmoses
          ├── cogserver
          │    └── opencog
          └── attention
                └── opencog
```

## Workflow Features

### Artifact Management
- Build artifacts are shared between jobs using GitHub Actions artifacts
- Artifacts include libraries and headers needed by dependent components
- Artifacts are retained for 1 day by default (7 days for releases)

### Caching
- Multi-platform builds use caching for faster builds
- ccache is used to cache compilation results
- pip cache is preserved for Python dependencies

### Testing
- Each component runs its test suite independently
- Test failures don't block the pipeline (continue-on-error)
- Test results are uploaded as artifacts for debugging
- Integration tests run after all components are built

### Container Usage
- All workflows use the `opencog/opencog-deps` Docker image
- This ensures consistent build environment across all runners
- Different tags are used for different Ubuntu versions

## Running Workflows Locally

You can test these workflows locally using [act](https://github.com/nektos/act):

```bash
# Run the build workflow
act -W .github/workflows/build.yml

# Run a specific job
act -j build-and-test -W .github/workflows/build.yml

# Run with specific event
act pull_request -W .github/workflows/ci.yml
```

## Customization

### Adding a New Component

To add a new component to the build sequence:

1. Add a job in `ci-improved.yml`:
```yaml
  my-component:
    needs: atomspace  # or other dependencies
    uses: ./.github/workflows/reusable/build-component.yml
    with:
      component-name: my-component
      repository: opencog/my-component
      dependencies: cogutil-artifacts,atomspace-artifacts
```

2. Update dependent jobs to include the new component if needed

### Modifying Build Options

You can pass additional CMake arguments through the reusable workflow:

```yaml
  my-component:
    uses: ./.github/workflows/reusable/build-component.yml
    with:
      component-name: my-component
      repository: opencog/my-component
      cmake-args: "-DENABLE_FEATURE=ON -DBUILD_SHARED_LIBS=OFF"
```

## Troubleshooting

### Common Issues

1. **Build failures due to missing dependencies**
   - Check that all required artifacts are downloaded
   - Verify the dependency order is correct

2. **Test failures**
   - Tests are set to continue on error to prevent blocking
   - Check test artifacts for detailed logs

3. **Artifact upload failures**
   - Ensure paths exist before uploading
   - Use `if-no-files-found: ignore` for optional artifacts

### Debugging Tips

- Use `actions/upload-artifact` to save build logs
- Add `set -x` to shell scripts for verbose output
- Check container logs if builds fail mysteriously
- Use matrix builds to isolate platform-specific issues

## Best Practices

1. **Keep workflows DRY** - Use reusable workflows for common patterns
2. **Fail fast** - Use `fail-fast: false` in matrices only when needed
3. **Cache wisely** - Cache dependencies but not build outputs
4. **Test in parallel** - Run independent tests concurrently
5. **Document changes** - Update this README when modifying workflows