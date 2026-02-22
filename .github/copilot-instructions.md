# Swift Project Instructions

You are an expert Swift developer working on a command-line tool managed by Swift Package Manager.

## Critical Verification Workflow

**You MUST verify your changes by running the project before and after modifications.**

1. **Baseline Check:**
   Before making any changes, run `swift run` in the terminal to see the current output and ensure the project builds successfully.
2. **Verification Check:**
   After applying your changes, run `swift run` again immediately.
3. **Comparison:**
   Compare the pre-change and post-change outputs to confirm:

   - The new functionality works as expected.
   - No existing functionality is broken.
   - The output format is correct.

## Project Context

- **Type:** Swift Package Manager (SPM) Command-line Tool.
- **Entry Point:** `Sources/ENDBattery/main.swift`
- **Package Manifest:** `Package.swift`

## Coding Standards

- Write clean, idiomatic Swift code.
- Prefer `struct` over `class` where appropriate.
- Ensure error handling is robust (e.g., using `do-catch` blocks).
- **No Redundant Comments:** Do NOT add meaningless comments (e.g., `// New property`, `// Update`, `// Added function`) when introducing changes. Only guide the user if the logic is complex.
- **NO THINKING IN COMMENTS (CRITICAL):** NEVER write your internal reasoning, trial-and-error process, or conversational filler in code comments. 
  - ❌ **FORBIDDEN:** `// Wait, the logic is...`, `// Actually, let's try...`, `// I should probably...`
  - ✅ **ALLOWED:** Only concise, technical explanations of the *final* decided logic (the "Why" and "What").
  - If you need to think step-by-step, do it in your chat response text OUTSIDE of the code blocks.
- **Language Requirement:** All comments in code located **BELOW** the separator line `/* ————————————————————————————— 下面不用看 ————————————————————————————— */` MUST be written in **English**.
