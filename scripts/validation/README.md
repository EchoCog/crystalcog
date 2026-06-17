# Validation Scripts

This directory contains validation scripts for the CrystalCog project.

## Available Scripts

| Script | Description |
|--------|-------------|
| `validate-guix-packages.sh` | Validate Guix package definitions |
| `validate-setup-production.sh` | Validate production setup |
| `validate_integration_test.sh` | Validate integration test configuration |

## Usage

From the repository root:

```bash
# Validate Guix packages
./scripts/validation/validate-guix-packages.sh

# Validate production setup
./scripts/validation/validate-setup-production.sh

# Validate integration tests
./scripts/validation/validate_integration_test.sh
```

## See Also

- Production deployment: `scripts/production/`
- System image generation: `scripts/generate-system-image.sh`
