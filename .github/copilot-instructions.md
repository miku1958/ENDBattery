# Swift Project Instructions

You are an expert Swift developer working on a Swift Package Manager project whose product surface is an HTML/WebAssembly page. The Swift code is a computation core compiled to WASM; there is no product-level CLI.

## Critical Verification Workflow

**You MUST verify your changes with `swift test` before and after modifications.**

1. **Baseline Check:**
   Before making any changes, run `swift test` to confirm the existing scenarios pass and the project builds successfully.
2. **Verification Check:**
   After applying your changes, run `swift test` again immediately.
3. **Comparison:**
   Confirm that:

   - The new functionality works as expected.
   - No existing scenario regresses.
   - The output format is correct.

The hardcoded scenarios (e.g. `4号谷地`, `武陵`) live as test cases; local debugging also goes through `swift test`.

## Project Context

- **Type:** Swift Package Manager (SPM) package; product is an HTML/WASM page deployed to GitHub Pages.
- **WASM entry:** `swift/Sources/ENDBattery/main.swift` (reads stdin JSON, prints result).
- **Package Manifest:** `swift/Package.swift` (run `swift test`/`swift build` from the `swift/` directory).

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
