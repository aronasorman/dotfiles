# Confirmed frontend-platform build 66372 diagnosis

- Repository root: `/Users/aron/src/forthbridge/frontend-platform`.
- User-visible symptom: the build fails with `Path does not exist: /home/vsts/work/1/s/apps/$(service)/deployment.azure.yaml`.
- Expected behavior: `$(service)` is replaced with a concrete service such as `video`, producing `apps/video/deployment.azure.yaml`.
- Confirmed diagnosis: automatic changed-service discovery queried Azure Repos after the repository moved to GitHub. `getPullRequests` returned `null`; calling `.at(0)` on that null result threw before the script emitted its final `#JSON` matrix. The wrapper published an empty matrix value, and the downstream job retained the literal `$(service)` token.

## Required problem bridges

Place these three cards between Expected behavior and Diagnosis:

1. `What the literal token tells us`: `$(service)` was never replaced, so the Docker task received a placeholder instead of a real service name.
2. `Where the service name comes from`: the prepare job should emit a service matrix, and `strategy.matrix` supplies one concrete `service` value to each build job.
3. `Where that process failed`: automatic discovery threw before its final `#JSON` line, and the wrapper published an empty matrix value instead of failing the prepare job.

## Main causal source anchors, ordered backward from the symptom

1. `deployment/deployment.pipeline.yml`, lines 129-137, SHA-256 `ab52f4d2b3b106f228e4049d08c308f077e0134ca94395639251d6922302d5d3`: the build job consumes the matrix and can run without a matrix-provided service variable.
2. `deployment/deployment.pipeline.yml`, lines 94-117, same SHA-256: the auto wrapper accepts only a final `#JSON` line and otherwise publishes an empty output; an explicitly named service builds its matrix directly.
3. `deployment/scripts/build-matrix/src/build-matrix-changed-services.ts`, lines 32-53, SHA-256 `c708ba3a580795ad82939a4716c57a1d883242a0fe8e5061c723a57a6d962bbe`: `getPullRequests` returns `null`, `.at(0)` is called on that null value, and a later Azure-only `getChanges` dependency remains.
4. `deployment/scripts/build-matrix/src/build-matrix-changed-services.ts`, lines 9-25, same SHA-256: discovery constructs an Azure DevOps Git client and queries `BUILD_REPOSITORY_ID`, which identifies the GitHub repository in this build rather than an Azure Repos repository.

## Supporting scope evidence

- `deployment/scripts/build-matrix/src/build-matrix-changed-services.ts`, lines 80-94, same SHA-256: the all-app path also uses Azure Repos `getItems`. This is not another step in build 66372's causal chain. If included after the root cause, label the repair-scope conclusion `Inference:` because no candidate patch was supplied.

## External verification

- Slack incident thread `https://union-studio.slack.com/archives/C0A7SSSAGJU/p1783945481725859`, Azure DevOps build 66372 metadata, and log 9 establish repository type `GitHub`, `paramServices=auto`, and the `.at(0)` failure.
- Azure DevOps build 66372 log 10 and task timeline establish the empty prepare output and unresolved `$(service)` token downstream.
- The incident thread and explicit-service pipeline branch establish selecting `video` as the discovery-bypassing workaround.
- Proposed fix: none supplied. Use `proposed_fix: null`; do not invent candidate code.
