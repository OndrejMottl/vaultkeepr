# Task Routing

Every agent or tool working in this repository reads `AGENTS.md` and the relevant `.ai/` files directly. Do not paste or duplicate canonical policy into delegated prompts.

Use `.ai/agents/changes-reviewer.agent.md` for read-only reviews and `.ai/agents/plan-large-changes.agent.md` for non-mutating implementation plans. Task prompts should add only the concrete scope, constraints, and requested output.

Delegation is optional. Do not assume another agent lacks repository access, and do not use delegation to bypass user-controlled Git, release, data, or schema safeguards.
