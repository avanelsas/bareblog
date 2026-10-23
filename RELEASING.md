# Releasing BareBlog

A release is a pull request of its own.

## A release

1. On a branch named `release-X.Y.Z`, set the version:

   ```
   npm version --no-git-tag-version X.Y.Z
   ```

   This writes it in `package.json` and `package-lock.json`, the two places that hold it.

2. Add a `## [X.Y.Z] - YYYY-MM-DD` section at the top of `CHANGELOG.md`. Say what changed for an
   author, and which version of BareDOM BareBlog needs.

3. Open a pull request named `Release X.Y.Z`, and merge it once CI passes.

4. Tag the merge commit on `main`, and push the tag:

   ```
   git checkout main && git pull
   git tag vX.Y.Z && git push origin vX.Y.Z
   ```

**Tag after the merge, not before.** A squash merge makes a new commit on `main`. A tag on the
commit of the branch would point at a commit that is on no branch.

## What the tag does

`.github/workflows/release.yml` runs on a tag `vX.Y.Z`. It refuses a tag that is not the
version in `package.json`, and a version that has no section in `CHANGELOG.md`. It runs the
tests, the linter and the release build. It then makes the GitHub release `BareBlog X.Y.Z`,
with that section of the changelog as its notes and a link to the changes since the previous tag.

BareBlog is installed from its repository, so a release publishes no package.

## A release that failed

When the workflow fails, nothing is released. When nothing needs fixing, run the job again.
Otherwise fix the cause in a pull request, and move the tag to its merge commit:

```
git checkout main && git pull
git tag -f vX.Y.Z && git push -f origin vX.Y.Z
```
