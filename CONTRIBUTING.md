# Contributing to nostr.hs

Thank you for your interest in contributing! We welcome contributions from the community.

## AI/LLM Usage Policy

*   **Allowed**: You are encouraged to use AI/LLMs (like ChatGPT, Claude, GitHub Copilot) to assist with coding, debugging, and generating documentation.
*   **Not Allowed ("Vibe Coding")**: Do not blindly copy-paste AI-generated code without understanding it. You are responsible for the code you submit. Ensure it compiles, runs, and follows the project's style.

## Pull Request Guidelines

*   **Size Limit**: Keep PRs under **500 lines** of changes. If your change is larger, please break it down into smaller, logical PRs.
*   **Scope**: Each PR should address a single concern (one feature, one bug fix, one refactor).
*   **Testing**: Ensure the project builds and examples run correctly before submitting.

## Commit Messages

*   Use the **imperative mood** (e.g., "Add feature" not "Added feature").
*   Be clear and concise.
*   Example: `Implement NIP-09 event deletion logic`

## Haskell Best Practices

To maintain a high-quality codebase, please adhere to the following:

1.  **Type Safety**:
    *   Use `newtype` wrappers for primitives (e.g., `EventId`, `PubKey`) to prevent mixing up data types.
    *   Avoid stringly-typed programming.

2.  **Strings**:
    *   Use `Data.Text` for all text processing. Avoid `String` except for legacy interactions.

3.  **Partial Functions**:
    *   **Strictly Avoid** partial functions like `head`, `tail`, `last`, `!!`.
    *   Use safe alternatives (e.g., pattern matching, `Data.List.NonEmpty`, or safe libraries).

4.  **Error Handling**:
    *   Use `Maybe` or `Either` for return types where operations can fail.
    *   Avoid throwing exceptions in pure code.

5.  **Formatting**:
    *   Code should be clean and readable.
    *   We generally follow standard Haskell formatting practices (similar to `fourmolu`).

6.  **Imports**:
    *   Use explicit imports declarations or qualified imports to avoid namespace pollution.

## Getting Started

1.  Fork the repository.
2.  Clone your fork: `git clone ...`
3.  Enter the environment: `nix develop`
4.  Build: `cabal build`

Happy coding!
