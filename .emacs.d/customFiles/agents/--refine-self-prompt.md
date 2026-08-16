---
name: amsha/--refine-self-prompt
as-prmopt: true
---
Review the conversation above and update two things:

**Memory**: who the user is. Did the user reveal persona, desires, preferences, personal details, or expectations about how you should behave? Save facts about the user and durable preferences with the `Memory` tool.

**Skills**: how to do this class of task. Be ACTIVE—most sessions produce at least one skill update. A pass that does nothing is a missed learning opportunity, not a neutral outcome.

Target shape of the skill library: **class-level skills** with a rich `SKILL.md` and a `references/` directory for session-specific detail. Do not create a long flat list of narrow, one-session/one-skill entries.

Skills may be **project-local** or **global**. The system prompt may list skills from both scopes, but they must remain independent.

- **Project-local skills** describe specifics of the current repository, codebase, project, or domain: conventions, architecture, local workflows, scripts, APIs, terminology, and validated implementation details. They must be self-contained and suitable for version control so another person working on the same project can use them. Do not make them depend on global skills, personal memories, gptel, this agent, or local agent/tooling skills.
- **Global skills** describe reusable workflows across projects. Put durable user preferences and cross-project working practices in global skills, or save them as memory when they describe who the user is. Global skills must not depend on project-local skills, repository-specific assumptions, gptel, this agent, or local agent/tooling skills.
- When a lesson is specific to the current project, prefer project-local storage. When it describes a user preference or reusable workflow, use a global skill or memory—not a project-local skill.
- A skill that mentions repository files, project conventions, local scripts, architecture, deployment procedures, or domain-specific terminology is project-local unless it has clearly been generalized.
- Project-local skills should be stored in the project’s skill directory and be suitable for version control - it will be in the `<project-root>/.agents/skills/<skill-name>/SKILL.md`.
  - {{PROJECT_ROOT_LINE}}
  - {{GLOBAL_SKILL_LINE}}

The later **curating agent reviews only the global skill collection**; it will not inspect or consolidate project-local skills. Therefore, make project-local updates complete and coherent now. Do not rely on a future curator to extract project details, repair missing context, or move material between scopes.

Signals that warrant a skill update (any one is enough):

- The user corrected your style, tone, format, legibility, verbosity, or approach. Frustration is a FIRST-CLASS skill signal, not just a memory signal. “Stop doing X,” “don’t format like this,” or “I hate when you Y” means embed the lesson in the skill that governs that task so the next session starts fixed.
- A non-trivial technique, fix, workaround, or debugging path emerged.
- A skill that was loaded or consulted turned out to be wrong, missing, or outdated—patch it now.
- The conversation revealed a reusable project convention or workflow that future agents should follow.

**Scope selection and update order**:

1. **Update a currently loaded project-local skill**, if one covers the learning. Check which skills were loaded in the conversation. If the lesson concerns the current project, patch the project-local skill first. Keep the result self-contained and specific to the project; do not add user preferences or references to global skills, gptel, this agent, or local agent/tooling skills.
2. **Update another existing project-local umbrella skill** that covers the project-specific task.
3. **Add a project-local support file** under an existing umbrella:

    - `references/<topic>.md` for session-specific detail, condensed research, API notes, or domain knowledge.
    - `templates/<name>.<ext>` for starter files meant to be copied and modified.
    - `scripts/<name>.<ext>` for statically re-runnable actions such as verification, fixture generation, or probes.

    Add a one-line pointer in `SKILL.md` so future agents can find the file.
4. **Update a currently loaded global skill** when the lesson is reusable across projects or concerns the user’s durable working preferences. Keep it independent of project-local skills and local agent/tooling context.
5. **Update an existing global umbrella skill** that covers the reusable workflow or user preference.
6. **Add a global support file** under an existing global umbrella.
7. **Create a new project-local class-level umbrella** when nothing existing covers the project-specific learning. Make the complete package independently usable by project collaborators.
8. **Create a new global class-level umbrella** only when the learning is broadly reusable and no suitable global umbrella exists.

For new skills, name them at the \*class level\*—not after a PR number, error string, codename, library alone, or one-off task. Do not create names such as `fix-X`, `debug-Y`, or a skill that only describes today’s incident.

If a project-local skill later proves broadly reusable, leave it project-local for now unless the generalization is obvious. The curating agent can promote or consolidate it later.

**User-preference embedding**: when the user complains about how you handled a task, update the skill that governs that task—memory alone is not enough. Memory says “who the user is” and records durable personal context; skills say “how to do this class of task for this user.” Put durable, cross-project preferences in a global skill and/or memory. Do not put user preferences in project-local skills; those should focus on project facts and procedures. Do not encode preferences through references to gptel, this agent, or another personal/local agent configuration.

If you notice overlapping existing skills, prefer updating the best existing umbrella. Mark related skills for later consolidation rather than creating another narrow duplicate.

**Do NOT capture as skills**:

- Environment-dependent failures: missing binaries, fresh-install errors, post-migration path mismatches, “command not found,” unconfigured credentials, or uninstalled packages. The user can fix these; they are not durable rules.
- Negative claims about tools or features, such as “the browser does not work,” “X is broken,” or “Y cannot be used from `execute_code`.” These harden into refusals after the underlying issue may be fixed.
- Session-specific transient errors that resolved before the conversation ended. If retrying worked, save the retry pattern—not the original failure.
- One-off task narratives. A request such as “summarize today’s market” or “analyze this PR” is not automatically a class of work that warrants a skill.
- Unresolved failures. If the session ended without finding a working method—several attempts failed and the user was told to check manually—do not write those attempts up as a reliable workflow. Either say “Nothing to save,” or capture only an independently validated alternative.

If a tool failed because of setup state, capture the fix—install command, configuration step, or environment variable—under an existing setup or troubleshooting skill. Never create “this tool does not work” as a standalone constraint.

Act on whichever dimension—memory or skills—has real signal. For project-specific learning, update project-local scope directly and leave the result self-contained; no later curator will review it. For reusable workflows or user preferences, update or create a global skill, and save durable personal context to memory. Do not create cross-scope dependencies. The later curating agent is responsible only for consolidation and cleanup within the global skill collection.

If genuinely nothing stands out on either dimension, say **“Nothing to save.”** and stop—but do not reach for that conclusion as a default.
