---
name: amsha/--refine-self-prompt
as-prmopt: true
---
Review the conversation above and update the appropriate persistent knowledge.

Memory: Save durable information in the appropriate memory target. The types of memory are:

- AGENT_MEMORY: durable instructions about how the agent should behave, recurring response or formatting requirements, and reliable lessons about agent workflows.
- USER: who the user is, including revealed persona, goals, preferences, personal details, working style, and expectations about how the assistant should behave.
- PROJECT: stable facts about the current project, repository, codebase, domain, architecture, conventions, terminology, APIs, and validated workflows.

Use memory for facts that are likely to remain useful beyond this conversation. Do not save transient task details, unresolved hypotheses, one-off requests, or environment-specific failures. When a fact belongs to the project, save it as project memory rather than user memory. When it describes the user's identity or durable cross-project preferences, save it as user memory. When it describes reusable agent behavior or workflow guidance, save it as agent memory.

Respect the memory target's character limit. Do not expose or reproduce full filesystem paths unless necessary. If there is no suitable existing text to replace, do not invent an ambiguous replacement; report that nothing can be safely saved.

Skills: Update the skill library when the conversation provides a reusable lesson about how to perform this class of task. Be ACTIVE—most sessions should produce at least one skill update when there is meaningful signal, either in the skill.md file itself or in the references related to a skill. A pass that does nothing is a missed learning opportunity, not a neutral outcome.

Here are the current skills that need curating. You do not need to search for anything beyond this list. Do not edit any skill that is not in this list under any circumstance:
{{SKILLS}}

Target shape of the skill library: class-level skills with a rich `SKILL.md` and a `references/` directory for session-specific detail. Do not create a long flat list of narrow, one-session/one-skill entries.

Skills may be project-local or global. The system prompt may list skills from both scopes, but they must remain independent.

- Project-local skills describe specifics of the current repository, codebase, project, or domain: conventions, architecture, local workflows, scripts, APIs, terminology, and validated implementation details. They must be self-contained and suitable for version control so another person working on the same project can use them. Do not make them depend on global skills, personal memories, gptel, this agent, or local agent/tooling skills.
- Global skills describe reusable workflows across projects. Put durable user preferences and cross-project working practices in global skills, or save them as memory when they describe who the user is. Global skills must not depend on project-local skills, gptel, this agent, or local agent/tooling context or skills.
- When a lesson is specific to the current project, prefer project-local storage. When it describes a user preference or reusable workflow, use a global skill or memory—not a project-local skill.
- A skill that mentions repository files, project conventions, local scripts, architecture, deployment procedures, or domain-specific terminology is project-local unless it has clearly been generalized.
- Project-local skills should be stored in the project’s skill directory and be suitable for version control: `<project-root>/.agents/skills/<skill-name>/SKILL.md`.
  - {{PROJECT_ROOT_LINE}}
  - {{GLOBAL_SKILL_LINE}}

The later curating agent reviews only the global skill collection; it will not inspect or consolidate project-local skills. Therefore, make project-local updates complete and coherent now. Do not rely on a future curator to extract project details, repair missing context, or move material between scopes.

Signals that warrant a skill update include:

- The user corrected your style, tone, format, legibility, verbosity, or approach. Frustration is a first-class skill signal, not just a memory signal. “Stop doing X,” “do not format like this,” or “I hate when you Y” means embed the lesson in the skill governing that task so the next session starts fixed.
- A non-trivial technique, fix, workaround, or debugging path emerged.
- A loaded or consulted skill turned out to be wrong, missing, or outdated - this must be fixed and updated.
- The conversation revealed a reusable project convention or workflow.
- A durable lesson about when or how to use memory emerged.

Scope selection and update order:

1. Update a currently loaded project-local skill if it covers the learning. If the lesson concerns the current project, patch the project-local skill first. Keep it self-contained and specific to the project; do not add user preferences or references to global skills, gptel, this agent, or local agent/tooling skills.
2. Update another existing project-local umbrella skill that covers the project-specific task.
3. Add a project-local support file under an existing umbrella:
   - `references/<topic>.md` for session-specific detail, condensed research, API notes, or domain knowledge.
   - `templates/<name>.<ext>` for starter files meant to be copied and modified.
   - `scripts/<name>.<ext>` for statically rerunnable actions such as verification, fixture generation, or probes.
   Add a one-line pointer in `SKILL.md` so future agents can find the file.
4. Update a currently loaded global skill when the lesson is reusable across projects or concerns the user's durable working preferences.
5. Update an existing global umbrella skill covering the reusable workflow or preference.
6. Add a global support file under an existing global umbrella.
7. Create a new project-local class-level umbrella when nothing existing covers the project-specific learning. Make the complete package independently usable by project collaborators.
8. Create a new global class-level umbrella only when the learning is broadly reusable and no suitable global umbrella exists.

For new skills, use class-level names rather than names based on a PR number, error string, codename, library alone, or one-off task. Do not create names such as `fix-X`, `debug-Y`, or a skill describing only today's incident.

If a project-local skill later proves broadly reusable, leave it project-local for now unless the generalization is obvious. The curating agent can promote or consolidate it later.

User-preference embedding: when the user complains about how you handled a task, update the skill governing that task; memory alone is not enough. Memory records durable facts about the user, project, or agent behavior. Skills record reusable procedures and class-level lessons. Put durable cross-project preferences in a global skill and/or `USER` memory. Put project facts and procedures in project-local skills and/or `PROJECT` memory. Do not put user preferences in project-local skills, and do not encode preferences through references to gptel, this agent, or another personal/local agent configuration.

Do not capture as skills:

- Environment-dependent failures such as missing binaries, fresh-install errors, post-migration path mismatches, command-not-found errors, unconfigured credentials, or uninstalled packages. Capture a durable setup fix under an existing setup or troubleshooting skill if one was validated; never create a skill saying that a tool does not work.
- Negative claims about tools or features, such as “the browser does not work,” “X is broken,” or “Y cannot be used from `execute_code`.”
- Session-specific transient errors that resolved before the conversation ended. Save the successful retry pattern instead.
- One-off task narratives. A request such as “summarize today's market” or “analyze this PR” is not automatically a class of work that warrants a skill.
- Unresolved failures. If no working method was found, do not write the failed attempts up as a reliable workflow.

If you notice overlapping existing skills, update the best existing umbrella and mark related skills for later consolidation rather than creating a narrow duplicate.

Act on whichever dimension—memory or skills—has real signal. For project-specific learning, update project-local scope directly and leave the result self-contained; no later curator will review it. For reusable workflows or durable preferences, update or create a global skill and save durable facts to the appropriate memory target. Do not create cross-scope dependencies.

If genuinely nothing stands out on either dimension, say exactly:

“Nothing to save.”

Then stop. Do not reach that conclusion by default.
