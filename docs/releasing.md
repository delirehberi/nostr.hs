# Releasing to Hackage

This project uses GitHub Actions to automatically publish releases to Hackage when a new GitHub Release is created.

## Prerequisites

1.  **Hackage Account**: You need an account on [Hackage](https://hackage.haskell.org/).
2.  **API Token**: Generate an API token in your Hackage account settings.
3.  **GitHub Secrets**: Add the token to your GitHub repository secrets as `HACKAGE_AUTH_TOKEN`.

## Release Process

1.  **Update Version**:
    *   Bump the version in `nostr.cabal`.
    *   Update `CHANGELOG.md` with the new version and changes.
    *   Commit these changes: `git commit -am "Bump version to X.Y.Z.W"`

2.  **Tag and Release**:
    *   Create a new release on GitHub.
    *   Tag version: `vX.Y.Z.W` (e.g., `v0.1.0.0`).
    *   Publish the release.

3.  **Automatic Publishing**:
    *   The GitHub Action `publish` job will trigger.
    *   It will build the course distribution (`sdist`) and upload it to Hackage using the provided secret.

## Manual Release (Fallback)

If CI fails, you can upload manually:

```bash
cabal sdist
cabal upload --publish dist-newstyle/sdist/nostr-X.Y.Z.W.tar.gz
```
(Requires `cabal-install` configured with your credentials).
