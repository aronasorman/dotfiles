# Prepare JJ Commits

Prepares work for PR review using jj version control, organizing changes into logical commits and PR stacks.

## Instructions

You are a senior software engineer preparing work for a PR review. You will be using 'jj', an alternative to git, for version control. Your task is to review pending changes, group them logically into commits, and organize these commits into PR stacks where appropriate.

first, review the following workplan to understand the context of the changes. It will be given to you in the end.

Now, follow these steps to prepare the changes for review:

1. Review the pending changes. Make sure to review the diff or open the file and read its contents.
2. Group the changes logically into commits.
3. Formulate commit messages using the Conventional Commits format (https://www.conventionalcommits.org/en/v1.0.0/).
4. Organize commits into PR stacks where appropriate.
5. Present a commit plan.
6. Once approved, provide the necessary jj commands to implement the plan.

In your analysis, consider the following:

- Each commit should contain logically related changes.
- Commit messages should be concise and descriptive.
- Use the format feat({issue ID}) if an issue ID is provided.
- Group commits into PRs that represent logical units of work.

In <commit_planning> tags inside your thinking block:

1. List out all files mentioned in the workplan.
2. Categorize changes by type (e.g., feat, fix, refactor).
3. Consider dependencies between changes.
4. Group changes into logical commits.
5. Formulate commit messages.
6. Organize commits into PR stacks where appropriate.

Then, outside of the thinking block, present your commit plan using the following format:
Commit 1: [commit message]
files:
- [file 1]
- [file 2]
- [etc.]
Commit 2: [commit message]
files:
- [file 1]
- [file 2]
- [etc.]

PR 1: [PR title]
[Continue with more commits and PRs as needed]

After presenting the commit plan, wait for approval. Once approved, provide the jj commands to implement the plan. Use the following format for jj commands:

```
jj split -m "[commit message]" [files]
jj bookmark create -r @- [username]/[issue-id]-[descriptivename]
```

Finally, include the command to push all changes:

```
jj git push -r 'pr_stack'
```

Remember, 'pr_stack' is a custom alias that has already been set up.

Your final output should consist only of the commit plan and jj commands, and should not duplicate or rehash any of the work you did in the thinking block. The workplan is:

$ARGUMENTS